(** The HTTP/2 server engine. See h2_server.mli.

    The words of the comments:
    - R, W, the stream fibers and the watchdog: see h2_server.mli.
    - "Fail the body": the read of the request body raises [Body.Truncated] (or
      [Body.Too_large]), and its waiters wake.
    - "Cancel the fiber": R cancels the stream fiber with [Promise.cancel].
    - An "engine head": a head that the engine makes itself (408, 413, 417, 421,
      431, 500), with no content and with END_STREAM. *)

open Weft
open Caravan
module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer
module Cancel = Caravan.Private.Cancel
module Date = Caravan.Private.Date
module Deadline = Caravan.Private.Deadline
module Message_rules = Caravan.Private.Message_rules

let default_local (config : Config.t) : Conn_state.local =
  { max_concurrent_streams= 100
  ; initial_window= 65535
  ; header_table_size= 4096
  ; max_header_list_size= config.max_header_bytes }

(* The largest frame that the engine reads and writes. The engine announces no
   SETTINGS_MAX_FRAME_SIZE, and each peer accepts this size. *)
let max_frame = 16384

(* The DATA parts of a round of W: 64 KiB, the size of its buffer. A round
   copies them under the mutex, thus the limit keeps the time under the mutex
   and the scratch memory small. *)
let max_parts = 4

(* {1 The state}

   Each mutable field of [stream] and of [conn] is under the mutex [m] of the
   connection, except [fiber] and [scratch]: only R uses [fiber], and only W
   uses [scratch]. *)

(* A DATA loan of [write]: W copies the bytes [off + pos] to [off + len - 1] of
   [buf] in parts. [last]: the loan has the last bytes of a body with a known
   length. [dropped]: a reset, the end of the body or the end of the connection
   removed the loan from the queue, and it will not go. [stalled]: W found no
   send window for the loan, and woke its writer one time, so that the writer
   starts its timer. *)
type loan =
  { buf: bytes
  ; off: int
  ; len: int
  ; mutable pos: int
  ; last: bool
  ; mutable dropped: bool
  ; mutable stalled: bool }

(* An item of the queue of a stream. [Block] is a header block, with all of its
   frames. [End] is an empty DATA frame with END_STREAM. [Rst No_error] goes
   after our END_STREAM: W calls [Conn_state.reset_local] when it takes it, and
   drops it if the state gives no frame (the client ended its side meanwhile).
   Each other [Rst] is already in the closed set of the state. *)
type item =
  | Block of {block: string; end_stream: bool}
  | Loan of loan
  | End
  | Rst of Frame.code

(* The response side of a stream, in the order of its steps. [No_head]: no final
   head is in the queue. [Head]: the final head is in the queue, and END_STREAM
   is not. [Ended]: END_STREAM is in the queue, and the queue takes only a RST.
   [Closed]: a RST of ours is in the queue, or the client reset the stream: the
   queue takes nothing. *)
type response = No_head | Head | Ended | Closed

(* The state of the body of a request. *)
type inbound = Receiving | Complete | Failed of exn

(* The unread DATA of a request are the [data_len] bytes of [data] from
   [data_off]. The state of the connection permits at most the receive window of
   the stream there, thus [data] has at most that size. *)
type stream =
  { id: int
  ; input: Fiber.Condition.t (* the body reader waits for DATA *)
  ; output: Fiber.Condition.t (* the loans and the flushes wait *)
  ; items: item Queue.t
  ; mutable response: response
  ; mutable sending: bool (* the stream is in [conn.sending] *)
  ; mutable taken: int (* the number of the last item that W took *)
  ; mutable data: bytes
  ; mutable data_off: int
  ; mutable data_len: int
  ; mutable received: int (* the bytes of the body, for max_body *)
  ; mutable inbound: inbound
  ; mutable trailers: Headers.t
  ; mutable continue: bool (* a 100 can go: no input came *)
  ; mutable running: bool (* the fiber of the stream did not end *)
  ; mutable fiber: unit Promise.t option }

(* The phases of the connection. [Closing `Error] is an end with an error code,
   of ours or of the client, or a write failure: W writes only the stream-0
   queue. After [Closing `Other], W also writes the items of the streams, except
   DATA. *)
type phase = Serving | Closing of [`Error | `Other]

(* The start of the connection: the preface of the client, then its first
   SETTINGS frame. *)
type start = Before_preface | Before_settings | Started

(* The input of R. [Draining]: no new stream, after our GOAWAY NO_ERROR.
   [Ended]: the input ended. In these two, the connection ends when no stream
   fiber is live. *)
type input = Reading | Draining | Ended

type conn =
  { m: Fiber.Mutex.t
  ; work: Fiber.Condition.t (* W waits for work *)
  ; wake: int Var.t (* R waits for a change *)
  ; cs: Conn_state.t
  ; config: Config.t
  ; initial_window: int (* our SETTINGS_INITIAL_WINDOW_SIZE *)
  ; max_list: int (* our SETTINGS_MAX_HEADER_LIST_SIZE *)
  ; decoder: Hpack.Decoder.t (* only R uses it *)
  ; streams: (int, stream) Hashtbl.t
        (* the streams with a live fiber or with items for W *)
  ; zero: Frame.t Queue.t (* the frames of stream 0, and RSTs of no record *)
  ; mutable goaway: Frame.t option (* it goes after the stream-0 queue *)
  ; updates: (int * int) Queue.t
        (* the WINDOW_UPDATE frames of the streams: the stream, the increment *)
  ; sending: stream Queue.t (* the streams with items, in round robin *)
  ; mutable phase: phase
  ; mutable broken: bool (* a write of W failed *)
  ; mutable live: int (* the stream fibers that did not end *)
  ; mutable idle_since: Clock.mono
        (* the start, or the end of the last stream fiber: the start of the idle
           time *)
  ; started: Clock.mono (* the start of the engine *)
  ; exact: bool
        (* [idle_timeout] below 100 ms: the last end of a stream fiber kicks the
           watchdog *)
  ; kick: int Var.t (* the watchdog waits for a change *)
  ; mutable start: start
  ; mutable input: input
  ; mutable block_since: Clock.mono option
        (* the HEADERS frame of a header block that did not end *)
  ; mutable stopping: bool
        (* the watchdog saw the stop of the server, and R did not handle it *)
  ; mutable due: bool
        (* the watchdog saw a deadline that passed: R reads the state again *)
  ; mutable seq: int (* the number of the last item that W took *)
  ; mutable flushed: int (* the items up to this number went to the flow *)
  ; mutable flush_waiters: int
  ; mutable yield_hint: bool
        (* a round completed the last loan of a body with a known length: its
           writer queues the end at once *)
  ; mutable scratch: bytes array }

let locked c f =
  Fiber.Mutex.use_rw
    ~protect:
      (false
      [@mutate
        off
          "a critical section has no cancellation point but the wait on a \
           condition, which sees the cancels also with ~protect:true"] )
    c.m f

(* Under [m]: R looks again. Only a change of the value counts. *)
let wake_r c =
  Var.set c.wake
    (( Var.get c.wake + 1 )
     [@mutate off "each new value is a change: R compares the values"] )

let kick c =
  Var.set c.kick
    (( Var.get c.kick + 1 )
     [@mutate
       off "each new value is a change: the watchdog compares the values"] )

let closing c = match c.phase with Closing _ -> true | Serving -> false

let encode fields =
  let b = Buffer.create 128 in
  Hpack.encode b fields ; Buffer.contents b

(* Under [m]: waits on [cond] until [ready ()] gives [Some r]. A cancel gives
   the exception with its backtrace: the caller raises it after the critical
   section. [wait_timed] has the time [span] for the wait. *)
let wait_loop c cond ready =
  let rec loop () =
    match ready () with
    | Some r ->
        r
    | None ->
        Fiber.Condition.wait cond c.m ;
        loop ()
  in
  loop ()

let wait_locked c cond ready =
  match wait_loop c cond ready with
  | r ->
      `Done r
  | exception (Fiber.Cancelled _ as e) ->
      `Cancelled (e, Printexc.get_raw_backtrace ())

let wait_timed c cond span ready =
  match Fiber.with_timeout Mono span (fun () -> wait_loop c cond ready) with
  | r ->
      `Done r
  | exception Fiber.Timeout ->
      `Timeout
  | exception (Fiber.Cancelled _ as e) ->
      `Cancelled (e, Printexc.get_raw_backtrace ())

(* {1 The queues of W} *)

let push c s item =
  Queue.push item s.items ;
  if not s.sending then begin
    s.sending <- true ;
    Queue.push s c.sending
  end ;
  Fiber.Condition.broadcast c.work

let push_zero c f =
  Queue.push f c.zero ;
  Fiber.Condition.broadcast c.work

(* A frame that the state gives: the WINDOW_UPDATE frame of a stream goes in the
   update queue, each other frame in the stream-0 queue. *)
let queue_frame c (f : Frame.t) =
  match f with
  | Window_update {stream; increment} when stream <> 0 ->
      Queue.push (stream, increment) c.updates ;
      Fiber.Condition.broadcast c.work
  | _ ->
      push_zero c f

let queue_frames c frames = List.iter (queue_frame c) frames

(* Keeps the items of [s] that [keep] accepts, in their order. *)
let remove_items s keep =
  let kept = Queue.create () in
  Queue.iter (fun i -> if keep i then Queue.push i kept) s.items ;
  Queue.clear s.items ;
  Queue.transfer kept s.items

(* Removes the loans of [s] from its queue. Their writers wake and raise
   [Flow.Closed]. The result is [true] if the queue had a loan. *)
let drop_loans s =
  let found = ref false in
  remove_items s (function
    | Loan l ->
        l.dropped <- true ;
        found := true ;
        false
        [@mutate
          off
            "after each drop the queue is cleared by a reset, or no writer is \
             live: a loan that stays is never taken"]
    | _ ->
        true ) ;
  Fiber.Condition.broadcast s.output ;
  !found

(* Removes all items of [s]. *)
let drop_items s =
  ignore (drop_loans s) ;
  Queue.clear s.items

(* Our RST_STREAM NO_ERROR after our END_STREAM: W decides when it takes it. *)
let queue_end_reset c s =
  push c s (Rst No_error) ;
  s.response <- Closed

(* The record of [s] goes when its fiber ended and W took its items. *)
let forget c s =
  if (not s.running) && Queue.is_empty s.items then
    Hashtbl.remove c.streams s.id

(* Fails the body of the request of [s] with [e], if it did not end. *)
let truncate s e =
  if s.inbound = Receiving then s.inbound <- Failed e ;
  Fiber.Condition.broadcast s.input

(* The two ways to reset a stream. Each RST of ours fails the request body, so
   that a reader in another fiber of the handler does not wait for DATA that
   cannot come.

   [reset_recorded]: the state already has the stream in its closed set (an
   error that it found, or a refused stream). The RST replaces the items that W
   did not take. A stream with no record gets its RST in the stream-0 queue. *)
let reset_recorded c ~stream code =
  match Hashtbl.find_opt c.streams stream with
  | Some s ->
      truncate s Body.Truncated ;
      drop_items s ;
      push c s (Rst code) ;
      s.response <- Closed
  | None ->
      push_zero c (Rst_stream {stream; code})

(* [reset_new]: the engine starts the reset. It goes only if the state gives a
   frame: never on an idle stream, and never two times. *)
let reset_new c ~stream code =
  match Conn_state.reset_local c.cs ~stream code with
  | Some _ ->
      reset_recorded c ~stream code
  | None ->
      ()

(* Under [m]: a RST_STREAM frame of the client. The body fails, and no frame of
   the stream follows. The state can give no [Peer_reset] event for it: our
   END_STREAM is in the queue, so the state has the stream as closed, but W did
   not write all of its items yet. The result is the fiber to cancel. *)
let peer_reset c stream =
  match Hashtbl.find_opt c.streams stream with
  | Some s ->
      truncate s Body.Truncated ;
      drop_items s ;
      s.response <- Closed ;
      s.fiber
  | None ->
      None

let fiber_of c stream =
  Option.bind (Hashtbl.find_opt c.streams stream) (fun s -> s.fiber)

(* {1 W, the writer} *)

(* What W writes in a round. *)
type out =
  | Out_frame of Frame.t
  | Out_block of {stream: int; end_stream: bool; block: string}
  | Out_data of {stream: int; end_stream: bool; buf: bytes; len: int}

(* W writes the WINDOW_UPDATE frame of a stream only if the stream is not closed
   for us: no frame after a RST. *)
let update_goes c stream =
  Option.exists
    (fun s -> s.response <> Closed)
    (Hashtbl.find_opt c.streams stream)

(* The [k]-th scratch buffer of a round. *)
let scratch c k =
  if k >= Array.length c.scratch then
    c.scratch <-
      Array.append c.scratch
        (Array.init
           (( k + 1 - Array.length c.scratch )
            [@mutate off "more scratch buffers change only the memory"] )
           (fun _ -> Bytes.create max_frame) ) ;
  c.scratch.(k)

(* Under [m]: the items of [s] that go in this round. These are all items at the
   head of its queue that are not loans, then at most one DATA part of a loan,
   within the send window. The round visits a stream only while it has less than
   [max_parts] parts. The result is in the reverse order. *)
let take_items c s ~parts acc =
  let next () =
    ignore (Queue.pop s.items) ;
    c.seq <- c.seq + 1 ;
    s.taken <- c.seq
  in
  let rec loop acc =
    match Queue.peek_opt s.items with
    | None ->
        acc
    | Some (Block {block; end_stream}) ->
        next () ;
        loop (Out_block {stream= s.id; end_stream; block} :: acc)
    | Some End ->
        next () ;
        loop
          ( Out_data {stream= s.id; end_stream= true; buf= Bytes.empty; len= 0}
          :: acc )
    | Some (Rst Error.No_error) -> (
        next () ;
        match Conn_state.reset_local c.cs ~stream:s.id No_error with
        | Some f ->
            loop (Out_frame f :: acc)
        | None ->
            loop acc )
    | Some (Rst code) ->
        next () ;
        loop (Out_frame (Rst_stream {stream= s.id; code}) :: acc)
    | Some (Loan l) ->
        let n =
          min
            (Conn_state.send_window c.cs ~stream:s.id)
            (min max_frame (l.len - l.pos))
        in
        if n <= 0 then begin
          (* The writer waits for window: it starts its timer. *)
          if not l.stalled then begin
            l.stalled <- true ;
            Fiber.Condition.broadcast s.output
          end ;
          acc
        end
        else begin
          let buf = scratch c !parts in
          incr parts ;
          Bytes.blit l.buf (l.off + l.pos) buf 0 n ;
          Conn_state.sent c.cs ~stream:s.id n ;
          l.pos <- l.pos + n ;
          if l.pos = l.len then begin
            next () ;
            (* W does not read [buf] again: its writer can continue. *)
            Fiber.Condition.broadcast s.output ;
            if l.last then c.yield_hint <- true
          end ;
          Out_data {stream= s.id; end_stream= false; buf; len= n} :: acc
        end
  in
  loop acc

(* Under [m]: the frames of a round. The result is the frames, in order, and
   [true] if the round has frames of stream 0. The GOAWAY goes after the frames
   of the stream-0 queue: the SETTINGS frame of our preface is the first frame
   of the connection. *)
let round c =
  let outs = ref [] and zero = ref false in
  let control f =
    zero := true ;
    outs := Out_frame f :: !outs
  in
  Queue.iter control c.zero ;
  Queue.clear c.zero ;
  Option.iter control c.goaway ;
  c.goaway <- None ;
  ( match c.phase with
  | Serving ->
      Queue.iter
        (fun (stream, increment) ->
          if update_goes c stream then
            outs := Out_frame (Window_update {stream; increment}) :: !outs )
        c.updates
  | Closing _ ->
      () ) ;
  Queue.clear c.updates ;
  if c.phase <> Closing `Error then begin
    let parts =
      ref (0 [@mutate off "only the index of the scratch buffers changes"])
    in
    (* The streams that a full round does not visit stay at the head. *)
    let rec visit k =
      if k > 0 && !parts < max_parts then begin
        let s = Queue.pop c.sending in
        outs := take_items c s ~parts !outs ;
        if Queue.is_empty s.items then begin
          s.sending <- false ;
          forget c s
        end
        else Queue.push s c.sending ;
        visit (k - 1)
      end
    in
    visit (Queue.length c.sending)
  end ;
  (List.rev !outs, !zero)

let write_out w = function
  | Out_frame f ->
      Frame.write w f
  | Out_block {stream; end_stream; block} ->
      Frame.write_header_block w ~stream ~end_stream ~max_size:max_frame block
  | Out_data {stream; end_stream; buf; len} ->
      Frame.write_data w ~stream ~end_stream buf 0 len

(* After a flush: the waiters of a flush look again. *)
let flushed c seq =
  c.flushed <- seq ;
  if
    ( c.flush_waiters > 0 )
    [@mutate
      off "a broadcast with no waiter has no effect: the test saves the walk"]
  then Hashtbl.iter (fun _ s -> Fiber.Condition.broadcast s.output) c.streams

(* What W does next. *)
type step =
  | Write of {outs: out list; flush: bool; seq: int}
  | Yield
  | Flush of int
  | Stop of int

let writer c flow =
  let w = Writer.create ~size:65536 flow in
  (* [dirty]: W wrote frames that it did not flush. W flushes at the end of a
     round with frames of stream 0 or with a waiter of a flush, and before it
     waits. Before that flush, W yields one time if other fibers can give it
     more frames now: then one flush sends them all. *)
  let rec next ~dirty ~yielded =
    match round c with
    | [], _ when closing c ->
        Stop c.seq
    | [], _ when dirty ->
        if (not yielded) && (c.live > 1 || c.yield_hint) then begin
          begin[@mutate
                 off
                   "a hint that stays costs one yield more before an idle \
                    flush: the frames do not change"]
            c.yield_hint <- false
          end ;
          Yield
        end
        else Flush c.seq
    | [], _ ->
        Fiber.Condition.wait c.work c.m ;
        next ~dirty ~yielded
    | outs, zero ->
        Write {outs; flush= zero || c.flush_waiters > 0; seq= c.seq}
  in
  let flush seq =
    Writer.flush w ;
    locked c (fun () -> flushed c seq)
  in
  let rec loop ~dirty ~yielded =
    match locked c (fun () -> next ~dirty ~yielded) with
    | Stop seq ->
        flush seq
    | Yield ->
        Fiber.yield () ; loop ~dirty ~yielded:true
    | Flush seq ->
        flush seq ; loop ~dirty:false ~yielded
    | Write {outs; flush= now; seq} ->
        List.iter (write_out w) outs ;
        if now then flush seq ;
        loop ~dirty:(not now) ~yielded:false
  in
  match
    loop ~dirty:(false [@mutate off "a flush with no byte writes nothing"])
      ~yielded:(false [@mutate off "the first round is not dirty"])
  with
  | () ->
      ()
  | exception _ ->
      (* A write failed: R ends the connection. After a cancel of W, the lock
         raises the cancel again: it is a cancellation point. *)
      locked c (fun () ->
          c.broken <- true ;
          wake_r c )

(* {1 The stream fibers} *)

let report (config : Config.t) e bt = config.on_error e bt

let date (config : Config.t) =
  if config.date_header then Some (Date.of_posix (Time.now Posix)) else None

let engine_head config status =
  let date = match date config with Some d -> [("date", d)] | None -> [] in
  encode
    (((":status", string_of_int status) :: date) @ [("content-length", "0")])

(* Under [m]: an engine head, if no final head is in the queue. Then
   [Conn_state.end_local], in the same critical section, so that a later frame
   of the client sees our end. *)
let queue_engine_head c s block =
  if s.response = No_head then begin
    push c s (Block {block; end_stream= true}) ;
    s.response <- Ended ;
    Conn_state.end_local c.cs ~stream:s.id
  end

(* A failure of the response: a 500 if no head is in the queue, else RST_STREAM
   INTERNAL_ERROR. *)
let fail_500 c s =
  let block = engine_head c.config 500 in
  locked c (fun () ->
      queue_engine_head c s block ;
      if s.response = Head then reset_new c ~stream:s.id Internal_error )

(* Reports [e], except a failure of the request body (a fault of the client). In
   a shield: the report does not depend on a cancel that comes at the lock. *)
let report_unless_client_fault c s e bt =
  Fiber.shield (fun () ->
      let client_fault =
        match e with
        | Body.Truncated | Body.Too_large ->
            locked c (fun () ->
                match s.inbound with Failed _ -> true | _ -> false )
        | _ ->
            false
      in
      if not client_fault then report c.config e bt )

let continue_block = encode (Message.interim 100)

(* The read of the request body. A read that waits has [read_timeout]. *)
let read c s buf off len =
  let ready () =
    if s.data_len > 0 then begin
      let n = min len s.data_len in
      Bytes.blit s.data s.data_off buf off n ;
      s.data_off <- s.data_off + n ;
      s.data_len <- s.data_len - n ;
      queue_frames c (Conn_state.consumed c.cs ~stream:s.id n) ;
      Some (Ok n)
    end
    else
      match s.inbound with
      | Complete ->
          Some (Ok 0)
      | Failed e ->
          Some (Error e)
      | Receiving ->
          None
  in
  (* The read timeout: a 408 if no head is in the queue, else RST_STREAM CANCEL.
     The exit path puts RST_STREAM NO_ERROR after the 408. *)
  let timed_out () =
    queue_engine_head c s (engine_head c.config 408) ;
    if s.response = Head then reset_new c ~stream:s.id Cancel ;
    s.inbound <- Failed Body.Truncated ;
    Error Body.Truncated
  in
  let result =
    locked c (fun () ->
        match ready () with
        | Some r ->
            `Done r
        | None -> (
            (* The 100 goes at the first wait, if no input came and no final
               head is in the queue. *)
            if s.continue && s.response = No_head then
              push c s (Block {block= continue_block; end_stream= false}) ;
            match wait_timed c s.input c.config.read_timeout ready with
            | `Done r ->
                `Done r
            | `Timeout ->
                `Done (timed_out ())
            | `Cancelled _ as r ->
                r ) )
  in
  match result with
  | `Done (Ok n) ->
      n
  | `Done (Error e) ->
      raise e
  | `Cancelled (e, bt) ->
      Printexc.raise_with_backtrace e bt

(* Under [m]: the final head goes in the queue before the first output. *)
let commit c s block =
  if s.response = No_head then begin
    push c s (Block {block; end_stream= false}) ;
    s.response <- Head
  end

(* The response of the stream cannot take output: a reset, an engine head, the
   end of the connection. *)
let refuses c s =
  match s.response with Ended | Closed -> true | No_head | Head -> closing c

(* Removes the queue item [item] of a writer that leaves [write]: W never reads
   its buffer after it. *)
let remove_loan c s item =
  remove_items s (fun i -> i != item) ;
  begin[@mutate
         off
           "after a loan only loans of the same stream wait, and the send \
            window holds them too"]
    Fiber.Condition.broadcast c.work
  end

(* The sink of the response body. [refused] tells that the sink raised
   [Flow.Closed] because the stream cannot take output: not an error of the
   server. *)
let sink c s ~block ~length =
  let count = ref 0 and refused = ref false in
  let closed () =
    refused := true ;
    raise Flow.Closed
  in
  let complete () = Option.equal Int.equal length (Some !count) in
  let write buf off len =
    ( match length with
    | Some n when len > n - !count ->
        raise Caravan.Private.H1_write.Length_mismatch
    | _ ->
        () ) ;
    count := !count + len ;
    let last = complete () in
    let result =
      locked c (fun () ->
          if refuses c s then `Closed
          else begin
            commit c s block ;
            let l =
              {buf; off; len; pos= 0; last; dropped= false; stalled= false}
            in
            let item = Loan l in
            push c s item ;
            let progress () =
              if l.pos = l.len then Some `Done
              else if l.dropped then Some `Closed
              else None
            in
            (* The wait has [write_timeout] only when the loan waits for send
               window: W tells it with [stalled]. A write to the flow that waits
               has its own timeout. *)
            let untimed () =
              match progress () with
              | Some r ->
                  Some r
              | None ->
                  if l.stalled then Some `Window else None
            in
            let waited =
              match wait_locked c s.output untimed with
              | `Done `Window ->
                  wait_timed c s.output c.config.write_timeout progress
              | (`Done (`Done | `Closed) | `Cancelled _) as r ->
                  r
            in
            match waited with
            | `Done r ->
                r
            | (`Timeout | `Cancelled _) as r -> (
                remove_loan c s item ;
                match r with
                | `Timeout ->
                    reset_new c ~stream:s.id Cancel ;
                    `Timeout
                | `Cancelled _ as r ->
                    r )
          end )
    in
    match result with
    | `Done ->
        ()
    | `Closed ->
        closed ()
    | `Timeout ->
        raise Flow.Timeout
    | `Cancelled (e, bt) ->
        Printexc.raise_with_backtrace e bt
  in
  (* A flush of a complete body with a known length returns at once: the end of
     the body follows, and W flushes before it waits. *)
  let flush () =
    if not (complete ()) then
      let result =
        locked c (fun () ->
            if refuses c s then `Closed
            else begin
              commit c s block ;
              c.flush_waiters <- c.flush_waiters + 1 ;
              let r =
                wait_locked c s.output (fun () ->
                    if Queue.is_empty s.items && c.flushed >= s.taken then
                      Some `Done
                    else if refuses c s then Some `Closed
                    else None )
              in
              c.flush_waiters <- c.flush_waiters - 1 ;
              r
            end )
      in
      match result with
      | `Done `Done ->
          ()
      | `Closed | `Done `Closed ->
          closed ()
      | `Cancelled (e, bt) ->
          Printexc.raise_with_backtrace e bt
  in
  (Body.Writer.make ~write ~flush, count, refused)

(* The fields of response trailers: lowercase names, with no field of the
   connection. *)
let trailer_fields h =
  List.rev
    (Headers.fold
       (fun name value acc ->
         let name = String.lowercase_ascii name in
         if Message_rules.is_connection_field name then acc
         else (name, value) :: acc )
       h [] )

(* After the body: the trailers with END_STREAM, or the END marker. A head that
   did not go yet takes END_STREAM itself. A loan of another fiber of the
   producer that still waits is dropped: then the body is not complete, and the
   stream gets RST_STREAM INTERNAL_ERROR in place of END_STREAM. *)
let end_body c s ~block ~trailers =
  let trailers =
    match trailer_fields trailers with [] -> None | l -> Some (encode l)
  in
  locked c (fun () ->
      if refuses c s then ()
      else if drop_loans s then reset_new c ~stream:s.id Internal_error
      else begin
        ( match (s.response, trailers) with
        | No_head, None ->
            push c s (Block {block; end_stream= true})
        | No_head, Some t ->
            push c s (Block {block; end_stream= false}) ;
            push c s (Block {block= t; end_stream= true})
        | _, None ->
            push c s End
        | _, Some t ->
            push c s (Block {block= t; end_stream= true}) ) ;
        s.response <- Ended ;
        Conn_state.end_local c.cs ~stream:s.id
      end )

(* A response body raised [e]. A refusal of the sink, a reset stream and a
   failure of the request body are not errors of the server. The decision and
   the report are in a shield: a cancel at the lock does not lose them. *)
let body_failed c s ~refused e bt =
  let quiet =
    Fiber.shield (fun () ->
        let quiet =
          !refused || locked c (fun () -> s.response = Closed || closing c)
        in
        if not quiet then report_unless_client_fault c s e bt ;
        quiet )
  in
  if not quiet then fail_500 c s

(* Closes [b] and reports an exception of its close. The hook of the body
   already ran. *)
let close_body c b =
  match Body.close b with
  | () ->
      ()
  | exception e ->
      report c.config e (Printexc.get_raw_backtrace ())

(* Writes the body [b]. The body closes in each case, before the end of the
   response: the close of a source (an upstream, for example) runs also if a
   cancel comes at the next lock. *)
let send_body c s ~block ~length b =
  let sink, count, refused = sink c s ~block ~length in
  match Body.write_to b sink with
  | () -> (
      close_body c b ;
      match length with
      | Some n when !count <> n ->
          body_failed c s ~refused Caravan.Private.H1_write.Length_mismatch
            (Printexc.get_raw_backtrace ())
      | _ ->
          end_body c s ~block ~trailers:(Body.Writer.trailers sink) )
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      (try Body.close b with _ -> ()) ;
      if Cancel.is_own e then Printexc.raise_with_backtrace e bt
      else body_failed c s ~refused e bt

(* Abandons a response that the stream did not send. *)
let abandon c r =
  try Response.abandon r
  with e -> report c.config e (Printexc.get_raw_backtrace ())

let respond c s ~meth response =
  match Message.response ~meth ~date:(date c.config) response with
  | Fail_500 text ->
      abandon c response ;
      report c.config (Failure text) (Printexc.get_raw_backtrace ()) ;
      fail_500 c s
  | Send (fields, plan) -> (
      let block = encode fields in
      let b =
        match Response.kind response with
        | Body b ->
            b
        | Upgrade _ ->
            Body.empty
      in
      match plan with
      | No_body ->
          (* The hook of the body runs. *)
          close_body c b ;
          locked c (fun () -> queue_engine_head c s block)
      | Length n ->
          send_body c s ~block ~length:(Some n) b
      | Unknown_length ->
          send_body c s ~block ~length:None b )

(* The exit path of a stream fiber, in a shield. It never waits. *)
let exit_path c s pending =
  Option.iter (abandon c) pending ;
  locked c (fun () ->
      s.running <- false ;
      c.live <- c.live - 1 ;
      ( match s.response with
      | Head ->
          reset_new c ~stream:s.id Cancel
      | Ended ->
          queue_end_reset c s
      | No_head | Closed ->
          () ) ;
      queue_frames c (Conn_state.discard c.cs ~stream:s.id) ;
      forget c s ;
      if c.live = 0 then begin
        c.idle_since <- Time.now Mono ;
        if c.input <> Reading then wake_r c ;
        if c.exact then kick c
      end )

let stream_fiber c s ~meth handler req () =
  let pending = ref None in
  let main () =
    match handler req with
    | response ->
        (* The exit path abandons a response that the stream cannot take. A
           cancel in the lock leaves it there too. *)
        pending := Some response ;
        if locked c (fun () -> s.response = No_head && not (closing c)) then begin
          begin[@mutate
                 off
                   "an abandon after the send has no effect: the close of a \
                    body is idempotent"]
            pending := None
          end ;
          respond c s ~meth response
        end
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        if Cancel.is_own e then Printexc.raise_with_backtrace e bt ;
        report_unless_client_fault c s e bt ;
        fail_500 c s
  in
  match main () with
  | () ->
      Fiber.shield (fun () -> exit_path c s !pending)
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Fiber.shield (fun () -> exit_path c s !pending) ;
      Printexc.raise_with_backtrace e bt

(* {1 R, the reader and coordinator} *)

let new_stream id ~end_stream ~continue =
  { id
  ; input= Fiber.Condition.create ()
  ; output= Fiber.Condition.create ()
  ; items= Queue.create ()
  ; response= No_head
  ; sending= false
  ; taken= (0 [@mutate off "flush pushes the head first, thus W sets it"])
  ; data= Bytes.empty
  ; data_off= 0
  ; data_len= 0
  ; received= 0
  ; inbound= (if end_stream then Complete else Receiving)
  ; trailers= Headers.empty
  ; continue
  ; running= false
  ; fiber= None }

(* Under [m]: appends [data] to the unread DATA of [s]. The buffer grows to at
   most the receive window, or to the bytes that it must hold. *)
let append c s data =
  let n = String.length data in
  let need = s.data_len + n in
  if
    ( s.data_off + need > Bytes.length s.data )
    [@mutate
      off "at the boundary, the bytes move to the start: the same content"]
  then begin
    let size =
      if need <= Bytes.length s.data then Bytes.length s.data
      else max need (min c.initial_window (2 * Bytes.length s.data))
    in
    let fresh =
      if size = Bytes.length s.data then s.data else Bytes.create size
    in
    Bytes.blit s.data s.data_off fresh 0 s.data_len ;
    s.data <- fresh ;
    s.data_off <- 0
  end ;
  Bytes.blit_string data 0 s.data (s.data_off + s.data_len) n ;
  s.data_len <- need

(* Under [m]: the DATA of a request body. A stream fiber that does not take the
   bytes (its body failed) gives them back to the receive windows at once. A
   stream with no fiber drops them: the state already gave them back. *)
let on_data c ~stream ~data ~end_stream =
  match Hashtbl.find_opt c.streams stream with
  | Some ({running= true; _} as s) ->
      let n = String.length data in
      let kept =
        match s.inbound with
        | Receiving ->
            s.received <- s.received + n ;
            s.continue <- false ;
            let over =
              match c.config.max_body with
              | Some max ->
                  s.received > max
              | None ->
                  false
            in
            if over then begin
              (* A body with no content-length over max_body: RST_STREAM CANCEL,
                 and the read raises Body.Too_large. A body with a declared
                 length over max_body got its 413 before. *)
              truncate s Body.Too_large ; reset_new c ~stream Cancel ; false
            end
            else begin
              append c s data ;
              if end_stream then s.inbound <- Complete ;
              Fiber.Condition.broadcast s.input ;
              true
            end
        | Complete | Failed _ ->
            false
      in
      if not kept then queue_frames c (Conn_state.consumed c.cs ~stream n)
  | _ ->
      ()

(* Under [m]: the events of a frame, except the header blocks. The result is the
   header blocks, and the fibers to cancel. An error of the request body does
   not cancel the fiber: a read of the body raises [Body.Truncated], as in
   HTTP/1.1, and the queue refuses the response. The fiber stays live until its
   handler ends. *)
let on_events c events =
  List.fold_left
    (fun (blocks, kills, goaway) (e : Conn_state.event) ->
      match e with
      | Send f ->
          queue_frame c f ; (blocks, kills, goaway)
      | Header_block {stream; block; end_stream; use} ->
          ((stream, block, end_stream, use) :: blocks, kills, goaway)
      | Data {stream; data; end_stream} ->
          on_data c ~stream ~data ~end_stream ;
          (blocks, kills, goaway)
      | Body_error {stream} ->
          reset_recorded c ~stream Protocol_error ;
          (blocks, kills, goaway)
      | Stream_error {stream; code} ->
          reset_recorded c ~stream code ;
          (blocks, Option.to_list (fiber_of c stream) @ kills, goaway)
      | Peer_reset _ ->
          (* R handles the RST_STREAM frame itself (see [peer_reset]). *)
          (blocks, kills, goaway)
      | Credit ->
          Fiber.Condition.broadcast c.work ;
          (blocks, kills, goaway)
      | Peer_goaway {code; _} ->
          (* R applies it after the frame, in [step]. *)
          (blocks, kills, Some code) )
    ([], [], None) events

let cancel_all kills = List.iter Promise.cancel kills

(* The fibers of the streams. A cancel of a fiber that ended has no effect. *)
let all_fibers c =
  Hashtbl.fold (fun _ s acc -> Option.to_list s.fiber @ acc) c.streams []

(* The timers of the connection. [Preface]: the preface and the first SETTINGS
   frame of the client, from the start. [Block]: a header block that did not
   end, from its HEADERS frame. [Idle]: no live stream fiber, from the end of
   the last one. [Check]: a stream fiber is live. At the check, the watchdog
   reads the state again. Each fiber that was live at that read ends later, thus
   the idle deadline is not earlier than the check. Thus the end of a stream
   fiber needs no kick of the watchdog. *)
type timer = Preface | Block | Idle | Check

(* An [idle_timeout] below 100 ms has no [Check]: a check so short makes the
   watchdog wake too often, and 0 makes it spin. Then the last end of a stream
   fiber kicks the watchdog, and the idle timer is exact. *)
let exact_idle (config : Config.t) =
  Span.compare config.idle_timeout
    (Span.ms (100 [@mutate off "the floor needs no exact value"]))
  < (0 [@mutate off "an idle_timeout of exactly 100 ms can use the two ways"])

(* Under [m]: the deadline of the timers, at the time [now]. The first that
   applies of the preface, the header block, idle and the check. When R waits
   for the end of the last stream fiber, that end wakes R: no idle timer and no
   check. *)
let deadline c ~now =
  let config = c.config in
  let at timer since span =
    Option.map (fun at -> (timer, at)) (Time.add Mono since span)
  in
  match (c.start, c.block_since) with
  | (Before_preface | Before_settings), _ ->
      at Preface c.started config.header_timeout
  | Started, Some since ->
      at Block since config.header_timeout
  | Started, None when c.input <> Reading ->
      None
  | Started, None when c.live = 0 ->
      at Idle c.idle_since config.idle_timeout
  | Started, None when c.exact ->
      None
  | Started, None ->
      at Check now config.idle_timeout

let passed ~now at = Span.compare (Time.diff Mono now at) Span.zero >= 0

(* Under [m]: the timer whose deadline passed at the time [now]. The check
   starts at [now] and has no exact idle timer, thus it never passed. *)
let due_timer c ~now =
  match deadline c ~now with
  | Some (timer, at) when passed ~now at ->
      Some timer
  | _ ->
      None

(** The watchdog of the connection: a fiber that owns the timers and the stop of
    the server. Thus a wait of R has no timer and no stop event: each wait with
    a timer puts an entry in the timer heap of weft, and the heap drops it only
    when it is at the head.

    The watchdog waits for the deadline of [deadline], for a kick, and for the
    stop until it saw it one time. Its deadlines are lazy: a deadline that moves
    later needs no kick, because at the old deadline the watchdog reads the
    state again. A deadline that moves earlier needs a kick: the first SETTINGS
    frame, the start and the end of a header block (a block that opens no stream
    fiber gives the idle deadline), and the last end of a stream fiber with an
    exact idle timer. At a deadline that passed, the watchdog sets [due] and
    wakes R, then it waits for a kick: R reads the state again. At the stop, it
    sets [stopping] and wakes R. It is a daemon of the scope: the scope cancels
    it when W and the stream fibers ended, thus it never keeps the scope alive.

    The writers of the fields that it reads or sets, each under [m]:
    - [start], [input], [block_since]: R.
    - [live], [idle_since]: the stream fibers (and R, which starts them).
    - [due], [stopping]: the watchdog sets them, and R clears them.
    - [kick] (a Var): R (a frame, a stale [due]) and the stream fibers (the last
      end with an exact idle timer). *)
let rec watchdog c stop ~stop_open =
  let seen = Var.view c.kick in
  let now = Time.now Mono in
  let d =
    locked c (fun () ->
        match due_timer c ~now with
        | Some _ ->
            c.due <- true ;
            wake_r c ;
            None
        | None ->
            deadline c ~now )
  in
  match
    sync
      (choose
         ( wrap (Var.changed seen) (fun _ -> `Kick)
           ::
           ( match d with
           | Some (_, at) ->
               [wrap (Time.at Mono at) (fun () -> `Kick)]
           | None ->
               [] )
         @ if stop_open then [wrap stop (fun () -> `Stop)] else [] ) )
  with
  | `Kick ->
      watchdog c stop ~stop_open
  | `Stop ->
      locked c (fun () ->
          c.stopping <- true ;
          wake_r c ) ;
      watchdog c stop ~stop_open:false

(* What the waits of R watch. Only R uses it. [seen] is the view of [wake] at
   the start of the step. [on_yield], [on_wait] and [on_step] are the hooks of
   the tests. *)
type watch =
  { mutable seen: int Var.view
  ; on_yield: unit -> unit
  ; on_wait: unit -> unit
  ; on_step: unit -> unit }

(* The state of R. *)
type reader =
  { c: conn
  ; ctx: Engine.ctx
  ; flow: Flow.t
  ; handler: Handler.t
  ; scope: Fiber.scope
  ; frames: Reader.t
  ; w: watch
  ; writer_fiber: unit Promise.t
  ; scheme: string }

(* A wait of R, other than for its input: a change of [wake]. The two waits of R
   (the guarded read, and the wait after the end of the input) use it. *)
let wake_event w = wrap (Var.changed w.seen) (fun _ -> `Wake)

(* The guarded read of the input. Before each read, a change of [wake] raises
   [Flow.Timeout]: R then looks at the change. When no byte is ready, R waits
   for the input or for a change of [wake]. *)
let rec guarded c w flow buf off len =
  w.on_yield () ;
  (* The simulator runs the client only at a switch point, thus there the choose
     sees the change. The check and the yield are for a client that keeps the
     socket full. *)
  begin[@mutate off "the choose sees the change in the simulator"] if
    Var.get c.wake <> Var.value w.seen
  then raise Flow.Timeout
  end ;
  begin[@mutate off "the simulator switches at read_now"]
    Fiber.yield ()
  end ;
  match Flow.read_now flow buf off len with
  | Some n ->
      n
  | None -> (
      w.on_wait () ;
      match
        sync
          (choose [wrap (Flow.readable flow) (fun () -> `Ready); wake_event w])
      with
      | `Ready ->
          guarded c w flow buf off len
      | `Wake ->
          raise Flow.Timeout )

(* The end sequence. R stopped to read. In [Closing], the loans of the writers
   end with an error. An end with an error cancels each stream fiber: after the
   other ends, no fiber is live. With [linger], R reads the input after the last
   frames if W did not break: after the end of the input, the linger ends at
   once. *)
let finish ?goaway ?(linger = true) r kind =
  let c = r.c in
  let kills =
    locked c (fun () ->
        c.goaway <-
          Option.map
            (fun (code, reason) -> Conn_state.goaway c.cs code reason)
            goaway ;
        c.phase <- Closing kind ;
        Hashtbl.iter (fun _ s -> ignore (drop_loans s)) c.streams ;
        Fiber.Condition.broadcast c.work ;
        if kind = `Error then all_fibers c else [] )
  in
  cancel_all kills ;
  Flow.set_write_deadline r.flow (Deadline.from_now c.config.linger_timeout) ;
  Promise.await r.writer_fiber ;
  if linger && not (locked c (fun () -> c.broken)) then begin
    Flow.set_read_deadline r.flow (Deadline.from_now c.config.linger_timeout) ;
    Flow.linger r.flow ~max:c.config.max_drain
  end

(* An engine response with no fiber: 413, 417, 421 and 431. Then RST_STREAM
   NO_ERROR, which W drops if the client ended its side. *)
let early r ~stream ~end_stream status =
  let c = r.c in
  let block = engine_head c.config status in
  locked c (fun () ->
      let s =
        new_stream stream ~end_stream
          ~continue:(false [@mutate off "no fiber reads this body"])
      in
      Hashtbl.replace c.streams stream s ;
      queue_engine_head c s block ;
      queue_end_reset c s ;
      queue_frames c (Conn_state.discard c.cs ~stream) )

(* A request: its record and its fiber. A declared length that the DATA of the
   HEADERS frame already broke gets RST_STREAM PROTOCOL_ERROR, with no fiber. *)
let start r ~stream ~end_stream (head : Message.request_head) =
  let c = r.c in
  let s =
    locked c (fun () ->
        let broken =
          match head.content_length with
          | Some n ->
              Conn_state.set_content_length c.cs ~stream n
          | None ->
              []
        in
        if broken <> [] then begin
          ignore (on_events c broken : _ * _ * _) ;
          queue_frames c (Conn_state.discard c.cs ~stream) ;
          None
        end
        else begin
          let s =
            new_stream stream ~end_stream
              ~continue:(head.expect = `Continue && not end_stream)
          in
          s.running <- true ;
          c.live <- c.live + 1 ;
          Hashtbl.replace c.streams stream s ;
          Some s
        end )
  in
  Option.iter
    (fun s ->
      let body =
        if end_stream then Body.empty
        else
          Body.of_source ?length:head.content_length
            ~trailers:(fun () -> s.trailers)
            (read c s)
      in
      let req =
        Request.make ~version:Http_2 ~scheme:r.scheme ?authority:head.authority
          ~headers:head.headers ~body ~conn:r.ctx.conn head.meth head.path
      in
      match
        Fiber.fork_promise ~name:"caravan.h2.stream" r.scope
          (stream_fiber c s ~meth:head.meth r.handler req)
      with
      | p ->
          s.fiber <- Some p
      | exception Fiber.Scope_closed ->
          Cancel.scope_closed () )
    s

(* The trailers of a request. Malformed trailers fail the body and get
   RST_STREAM PROTOCOL_ERROR. After our END_STREAM, the trailers close the
   stream in the state: then no RST goes, and no reader waits (the producer
   ended). *)
let on_trailers c ~stream decoded =
  let trailers =
    match decoded with
    | `Fields l ->
        Message.trailers l
    | `Too_large ->
        Error "trailers over the limits"
  in
  locked c (fun () ->
      match (trailers, Hashtbl.find_opt c.streams stream) with
      | Ok h, Some s ->
          if s.inbound = Receiving then begin
            s.trailers <- h ;
            s.inbound <- Complete
          end ;
          Fiber.Condition.broadcast s.input
      | Error _, _ ->
          reset_new c ~stream Protocol_error
      | Ok _, None ->
          () )

let on_request r ~stream ~end_stream decoded =
  let c = r.c in
  match decoded with
  | `Too_large ->
      early r ~stream ~end_stream 431
  | `Fields l -> (
    match Message.request l with
    | Error (Malformed _) ->
        locked c (fun () ->
            reset_new c ~stream Protocol_error ;
            queue_frames c (Conn_state.discard c.cs ~stream) )
    | Error Misdirected ->
        early r ~stream ~end_stream 421
    | Ok head ->
        let too_large =
          match (c.config.max_body, head.content_length) with
          | Some max, Some n ->
              n > max
          | _ ->
              false
        in
        if head.expect = `Unknown then early r ~stream ~end_stream 417
        else if too_large then early r ~stream ~end_stream 413
        else start r ~stream ~end_stream head )

(* A header block. Each block is decoded, whatever its use: the dynamic table
   needs each one. *)
let on_block r (stream, block, end_stream, use) =
  let c = r.c in
  match
    Hpack.Decoder.decode c.decoder ~max_list_size:c.max_list
      ~max_fields:c.config.max_header_count block
  with
  | exception Error.Hpack reason ->
      Error (Error.Compression_error, reason)
  | decoded ->
      ( match (use : Conn_state.use) with
      | Ignored ->
          ()
      | Refused ->
          locked c (fun () -> reset_recorded c ~stream Refused_stream)
      | Reset code ->
          (* A header block on an open stream is an error of the body. *)
          locked c (fun () -> reset_recorded c ~stream code)
      | Trailers ->
          on_trailers c ~stream decoded
      | Open ->
          on_request r ~stream ~end_stream decoded ) ;
      Ok ()

let on_frame r frame =
  let c = r.c in
  match
    locked c (fun () ->
        match Conn_state.on_frame c.cs ~live:c.live frame with
        | Error e ->
            Error e
        | Ok events ->
            let blocks, kills, goaway = on_events c events in
            let kills =
              match frame with
              | Rst_stream {stream; _} ->
                  Option.to_list (peer_reset c stream) @ kills
              | _ ->
                  kills
            in
            (* The timers. The state gives an error for a first frame other than
               SETTINGS. The first SETTINGS frame, and the start and the end of
               a header block, can move the deadline earlier: they kick the
               watchdog. *)
            let block_open = Conn_state.block_open c.cs in
            if c.start = Before_settings then kick c ;
            if block_open <> (c.block_since <> None) then kick c ;
            c.start <- Started ;
            c.block_since <-
              ( if not block_open then None
                else if c.block_since = None then Some (Time.now Mono)
                else c.block_since ) ;
            Ok (blocks, kills, goaway) )
  with
  | Error e ->
      Error e
  | Ok (blocks, kills, goaway) -> (
      cancel_all kills ;
      match
        List.fold_left
          (fun res b -> match res with Ok () -> on_block r b | Error _ -> res)
          (Ok ()) (List.rev blocks)
      with
      | Error e ->
          Error e
      | Ok () ->
          Ok goaway )

(* The end of the input: each request body fails, and a header block that did
   not end has no timer. With [cancel_on_disconnect], each fiber is cancelled.
   After this, R does not wait for input. *)
let input_ended r =
  let c = r.c in
  let kills =
    locked c (fun () ->
        c.input <- Ended ;
        c.block_since <- None ;
        Hashtbl.iter (fun _ s -> truncate s Body.Truncated) c.streams ;
        if c.config.cancel_on_disconnect then all_fibers c else [] )
  in
  cancel_all kills

let connection_error r code reason = finish ~goaway:(code, reason) r `Error

(* The stop of the server, or a GOAWAY NO_ERROR of the client: no new stream.
   Our GOAWAY NO_ERROR tells the client the last stream that the engine serves
   (RFC 9113 section 6.8). After it, the state ignores the new streams. *)
let drain r reason =
  locked r.c (fun () ->
      push_zero r.c (Conn_state.goaway r.c.cs No_error reason) ;
      if r.c.input = Reading then r.c.input <- Draining )

(* A deadline that passed. The preface: the connection closes, with no GOAWAY. A
   header block: GOAWAY ENHANCE_YOUR_CALM, each fiber is cancelled. Idle: GOAWAY
   NO_ERROR. *)
let rec expired r = function
  | Preface ->
      finish ~linger:false r `Error
  | Block ->
      connection_error r Enhance_your_calm
        "a header block that did not end in time"
  | Idle ->
      finish ~goaway:(Error.No_error, "idle") r `Other
  | Check ->
      (* [due_timer] never gives a check (see there). A check only reads the
         state again. *)
      step r

(* A step of R. It takes the view of [wake] before it reads the state, so that
   no change after the read is lost. *)
and step r =
  let c = r.c and w = r.w in
  w.on_step () ;
  w.seen <- Var.view c.wake ;
  let live, broken, stopping, due, start, input =
    locked c (fun () ->
        let stopping = c.stopping in
        c.stopping <- false ;
        (c.live, c.broken, stopping, c.due, c.start, c.input) )
  in
  if broken then finish r `Error
  else if stopping then
    (* The stop: a connection with no first SETTINGS frame closes at once. *)
    if start = Started then begin
      drain r "the server stops" ; step r
    end
    else finish ~linger:false r `Error
  else if input <> Reading && live = 0 then finish r `Other
  else if due then (
    match
      locked c (fun () ->
          c.due <- false ;
          due_timer c ~now:(Time.now Mono) )
    with
    | Some timer ->
        expired r timer
    | None ->
        (* The state changed after the read of the watchdog. *)
        kick c ; step r )
  else
    match (start, input) with
    | Before_preface, _ ->
        preface r
    | _, Ended ->
        w.on_wait () ;
        ignore (sync (wake_event w)) ;
        step r
    | _, (Reading | Draining) -> (
      match Frame.read r.frames ~max_size:max_frame with
      | frame -> (
        match on_frame r frame with
        | Ok None ->
            step r
        | Ok (Some Error.No_error) ->
            drain r "the client sent GOAWAY" ;
            step r
        | Ok (Some _) ->
            (* A GOAWAY of the client with an error: each fiber is cancelled. *)
            finish r `Error
        | Error (code, reason) ->
            connection_error r code reason )
      | exception Flow.Timeout ->
          step r
      | exception Frame.Connection_error {code; reason} ->
          connection_error r code reason
      | exception (End_of_file | Flow.Closed) ->
          input_ended r ; step r )

(* The connection preface. A bad preface closes the connection at once, with no
   GOAWAY. *)
and preface r =
  match Reader.ensure r.frames (String.length Frame.preface) with
  | () when Reader.sub_string r.frames 0 24 = Frame.preface ->
      Reader.consume r.frames 24 ;
      locked r.c (fun () ->
          r.c.start <- Before_settings ;
          List.iter (push_zero r.c) (Conn_state.start_frames r.c.cs) ) ;
      step r
  | () | (exception (End_of_file | Flow.Closed)) ->
      finish ~linger:false r `Error
  | exception Flow.Timeout ->
      step r

type probe =
  { state: Conn_state.t
  ; records: unit -> int
  ; live: unit -> int
  ; inbound: unit -> int
  ; scratch: unit -> int
  ; table_size: unit -> int }

type hooks =
  { inspect: probe -> unit
  ; on_yield: unit -> unit
  ; on_wait: unit -> unit
  ; on_step: unit -> unit }

let no_hooks =
  {inspect= ignore; on_yield= ignore; on_wait= ignore; on_step= ignore}

let serve ?local ?(hooks = no_hooks) : Engine.t =
 fun ctx flow handler ->
  let config = ctx.config in
  let local = Option.value local ~default:(default_local config) in
  let now = Time.now Mono in
  let c =
    { m= Fiber.Mutex.create ()
    ; work= Fiber.Condition.create ()
    ; wake= Var.create 0
    ; cs= Conn_state.create local
    ; config
    ; initial_window= local.initial_window
    ; max_list= local.max_header_list_size
    ; decoder= Hpack.Decoder.create ~max_table_size:local.header_table_size
    ; streams= Hashtbl.create 16
    ; zero= Queue.create ()
    ; goaway= None
    ; updates= Queue.create ()
    ; sending= Queue.create ()
    ; phase= Serving
    ; broken= false
    ; live= 0
    ; idle_since= now
    ; started= now
    ; exact= exact_idle config
    ; kick= Var.create 0
    ; start= Before_preface
    ; input= Reading
    ; block_since= None
    ; stopping= false
    ; due= false [@mutate off "a due at the start costs one read of the timers"]
    ; seq= (0 [@mutate off "the item numbers only compare with each other"])
    ; flushed=
        (0
        [@mutate
          off
            "a waiter of a flush wakes only after a flush of W, after a loan \
             of its stream (number 2 or more), or after a reset"] )
    ; flush_waiters= 0
    ; yield_hint= false
    ; scratch= [||] }
  in
  let sum f = Hashtbl.fold (fun _ s n -> n + f s) c.streams 0 in
  hooks.inspect
    { state= c.cs
    ; records= (fun () -> Hashtbl.length c.streams)
    ; live= (fun () -> c.live)
    ; inbound= (fun () -> sum (fun s -> Bytes.length s.data))
    ; scratch= (fun () -> Array.length c.scratch)
    ; table_size= (fun () -> Hpack.Decoder.table_size c.decoder) } ;
  Flow.set_write_timeout flow (Some config.write_timeout) ;
  let w =
    { seen= Var.view c.wake
    ; on_yield= hooks.on_yield
    ; on_wait= hooks.on_wait
    ; on_step= hooks.on_step }
  in
  let input =
    Flow.make ~read:(guarded c w flow) ~read_now:(Flow.read_now flow)
      ~write:(Flow.write flow) ~readable:(Flow.readable flow)
      ~shutdown_send:(fun () -> Flow.shutdown_send flow)
      ~close:(fun () -> Flow.close flow)
      ~set_read_deadline:(Flow.set_read_deadline flow)
      ~set_write_deadline:(Flow.set_write_deadline flow)
      ()
  in
  Fiber.scope
  @@ fun scope ->
  Fiber.fork_daemon ~name:"caravan.h2.watchdog" scope (fun () ->
      watchdog c (Conn_info.stopping ctx.conn) ~stop_open:true ) ;
  let r =
    { c
    ; ctx
    ; flow
    ; handler
    ; scope
    ; frames= Reader.create ~size:65536 input
    ; w
    ; writer_fiber=
        Fiber.fork_promise ~name:"caravan.h2.writer" scope (fun () ->
            writer c flow )
    ; scheme=
        ( match Conn_info.scheme ctx.conn with
        | `Http ->
            "http"
        | `Https ->
            "https" ) }
  in
  step r
