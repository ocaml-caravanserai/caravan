(* The state of an HTTP/2 connection on the server side, with no I/O. *)

type local =
  { max_concurrent_streams: int
  ; initial_window: int
  ; header_table_size: int
  ; max_header_list_size: int }

type use = Open | Trailers | Refused | Reset of Frame.code | Ignored

type event =
  | Send of Frame.t
  | Header_block of {stream: int; block: string; end_stream: bool; use: use}
  | Data of {stream: int; data: string; end_stream: bool}
  | Body_error of {stream: int}
  | Peer_reset of {stream: int}
  | Credit
  | Peer_goaway of {last_stream: int; code: Frame.code}
  | Stream_error of {stream: int; code: Frame.code}

(* The maximum size of a flow-control window (RFC 9113 section 6.9.1). *)
let max_window = 0x7fff_ffff

(* The window of the RFC before a change by SETTINGS or WINDOW_UPDATE. *)
let default_window = 65535

let max_setting = 0xffff_ffff

(* The maximum size of the closed set, whatever max_concurrent_streams is. *)
let max_closed = 4096

(* The minimum of the header block limit. A block over max_header_list_size, but
   under this limit, gets the 431 response of the engine, not a GOAWAY. *)
let min_block = 65536

(* The kinds of the closed set. [Ended]: the stream ended in the two directions.
   [Reset_by_peer]: the client reset it. *)
type kind = Reset_by_us | Ignored_id | Ended | Reset_by_peer

(* A stream that [Open] opened. [closed] is [None] while the stream is active
   (open or half-closed). The record stays until the stream is closed and the
   engine discarded it.

   The receive window of a stream: [recv_window + recv_unsent + held =
   initial_window]. [held] is the bytes that came and that the handler did not
   read. [recv_unsent] is the credit that no WINDOW_UPDATE frame sent yet. *)
type stream =
  { id: int
  ; mutable closed: kind option
  ; mutable remote_ended: bool
  ; mutable local_ended: bool
  ; mutable discarded: bool
  ; mutable recv_window: int
  ; mutable recv_unsent: int
  ; mutable held: int
  ; mutable send_window: int
  ; mutable length: int option (* the declared content-length *)
  ; mutable received: int (* the sum of the DATA of the body *) }

(* A header block that waits for its END_HEADERS. *)
type block = {b_stream: int; use: use; b_end_stream: bool; buf: Buffer.t}

(* The connection receive window: [conn_recv + conn_recv_unsent + held bytes of
   all streams = conn_start]. *)
type t =
  { local: local
  ; conn_start: int
  ; max_block: int (* the maximum size of a header block *)
  ; capacity: int (* the maximum size of the closed set *)
  ; streams: (int, stream) Hashtbl.t
  ; closed_set: (int, kind) Hashtbl.t
  ; order: int Queue.t (* the ids of [closed_set], the oldest first *)
  ; mutable active: int
  ; mutable settings_seen: bool
  ; mutable last_client: int
  ; mutable goaway_last: int option (* the last stream of our first GOAWAY *)
  ; mutable block: block option
  ; mutable conn_recv: int
  ; mutable conn_recv_unsent: int
  ; mutable conn_send: int
  ; mutable peer_initial: int
  ; mutable peer_max_frame: int }

let validate l =
  let check name v lo hi =
    if v < lo || v > hi then
      invalid_arg (Printf.sprintf "Conn_state.validate: %s %d" name v)
  in
  check "max_concurrent_streams" l.max_concurrent_streams 1 max_setting ;
  check "initial_window" l.initial_window default_window max_window ;
  check "header_table_size" l.header_table_size 0 max_setting ;
  check "max_header_list_size" l.max_header_list_size 0 max_setting

(* [min max_window (max_concurrent_streams * initial_window)], with no overflow
   of the product. *)
let conn_start l =
  if l.max_concurrent_streams > max_window / l.initial_window then max_window
  else l.max_concurrent_streams * l.initial_window

let create local =
  let conn_start = conn_start local in
  { local
  ; conn_start
  ; max_block= max (4 * local.max_header_list_size) min_block
  ; capacity= min (4 * local.max_concurrent_streams) max_closed
  ; streams= Hashtbl.create 16
  ; closed_set= Hashtbl.create 64
  ; order= Queue.create ()
  ; active= 0
  ; settings_seen= false
  ; last_client= 0
  ; goaway_last= None
  ; block= None
  ; conn_recv= conn_start
  ; conn_recv_unsent= 0
  ; conn_send= default_window
  ; peer_initial= default_window
  ; peer_max_frame= 16384 }

let start_frames t =
  let l = t.local in
  let settings =
    Frame.Settings
      { ack= false
      ; settings=
          [ (0x1, l.header_table_size)
          ; (0x3, l.max_concurrent_streams)
          ; (0x4, l.initial_window)
          ; (0x6, l.max_header_list_size) ] }
  in
  if t.conn_start > default_window then
    [ settings
    ; Frame.Window_update {stream= 0; increment= t.conn_start - default_window}
    ]
  else [settings]

(* {1 Streams and the closed set} *)

type status =
  | Active of stream  (** open or half-closed *)
  | Closed of kind  (** closed, with a record or in the closed set *)
  | Closed_unknown
      (** closed, with no kind: it left the closed set, or it never opened *)
  | Idle

let status t id =
  match Hashtbl.find_opt t.streams id with
  | Some ({closed= None; _} as s) ->
      Active s
  | Some {closed= Some k; _} ->
      Closed k
  | None -> (
    match Hashtbl.find_opt t.closed_set id with
    | Some k ->
        Closed k
    | None ->
        if id land 1 = 1 && id <= t.last_client then Closed_unknown else Idle )

let remember t id kind =
  Hashtbl.replace t.closed_set id kind ;
  Queue.push id t.order ;
  if Queue.length t.order > t.capacity then
    Hashtbl.remove t.closed_set (Queue.pop t.order)

(* [rekind t id kind]: the closed stream [id] changes its kind. *)
let rekind t id kind =
  Option.iter (fun s -> s.closed <- Some kind) (Hashtbl.find_opt t.streams id) ;
  if Hashtbl.mem t.closed_set id then Hashtbl.replace t.closed_set id kind

(* [close t s kind] closes the active stream [s]. *)
let close t s kind =
  t.active <- t.active - 1 ;
  s.closed <- Some kind ;
  remember t s.id kind ;
  if s.discarded then Hashtbl.remove t.streams s.id

let open_stream t id ~end_stream =
  let s =
    { id
    ; closed= None
    ; remote_ended= end_stream
    ; local_ended= false
    ; discarded= false
    ; recv_window= t.local.initial_window
    ; recv_unsent= 0
    ; held= 0
    ; send_window= t.peer_initial
    ; length= None
    ; received= 0 }
  in
  Hashtbl.replace t.streams id s ;
  t.active <- t.active + 1

(* {1 Receive windows} *)

(* [credit t n] gives back [n] bytes to the connection receive window. The
   result has the WINDOW_UPDATE frame of stream 0 if the unsent credit is half
   of the window or more. *)
let credit t n =
  t.conn_recv_unsent <- t.conn_recv_unsent + n ;
  if 2 * t.conn_recv_unsent >= t.conn_start then begin
    let increment = t.conn_recv_unsent in
    t.conn_recv <- t.conn_recv + increment ;
    t.conn_recv_unsent <- 0 ;
    [Frame.Window_update {stream= 0; increment}]
  end
  else []

(* [stream_credit t s n] is as [credit], for the receive window of [s]. A stream
   that is closed or that the client ended gets no WINDOW_UPDATE frame: no DATA
   can come on it. *)
let stream_credit t s n =
  s.recv_unsent <- s.recv_unsent + n ;
  if
    s.closed = None && (not s.remote_ended)
    && 2 * s.recv_unsent >= t.local.initial_window
  then begin
    let increment = s.recv_unsent in
    s.recv_window <- s.recv_window + increment ;
    s.recv_unsent <- 0 ;
    [Frame.Window_update {stream= s.id; increment}]
  end
  else []

(* [give_back t s n] gives back [n] bytes of [s] to the two receive windows. *)
let give_back t s n = stream_credit t s n @ credit t n

let sends frames = List.map (fun f -> Send f) frames

let consumed t ~stream n =
  match Hashtbl.find_opt t.streams stream with
  | None ->
      []
  | Some s ->
      let n = max 0 (min n s.held) in
      s.held <- s.held - n ;
      give_back t s n

let discard t ~stream =
  match Hashtbl.find_opt t.streams stream with
  | None ->
      []
  | Some s ->
      let frames = consumed t ~stream s.held in
      s.discarded <- true ;
      if s.closed <> None then Hashtbl.remove t.streams stream ;
      frames

(* {1 Frames} *)

let error (code : Frame.code) reason = Error (code, reason)

let breaks_length s =
  match s.length with
  | Some l ->
      s.received > l || (s.remote_ended && s.received < l)
  | None ->
      false

let set_content_length t ~stream length =
  match Hashtbl.find_opt t.streams stream with
  | Some ({closed= None; _} as s) ->
      s.length <- Some length ;
      if breaks_length s then begin
        close t s Reset_by_us ;
        [Body_error {stream}]
      end
      else []
  | _ ->
      []

(* The use of a HEADERS frame, or a connection error. The order of the checks is
   the order of plan section 6. *)
let headers_use t ~live ~stream ~end_stream ~self_dependent =
  match status t stream with
  | Active s ->
      if s.remote_ended then begin
        (* h2spec http2/5.1/6 *)
        close t s Reset_by_us ; Ok (Reset Stream_closed)
      end
      else begin
        (* h2spec http2/8.1/1: the trailers must end the stream. *)
        s.remote_ended <- true ;
        if (not end_stream) || self_dependent || breaks_length s then begin
          close t s Reset_by_us ; Ok (Reset Protocol_error)
        end
        else begin
          if s.local_ended then close t s Ended ;
          Ok Trailers
        end
      end
  | Closed (Reset_by_us | Ignored_id) ->
      Ok Ignored
  | Closed (Ended | Reset_by_peer) ->
      error Stream_closed "HEADERS on a closed stream"
  | Closed_unknown ->
      (* Step 2. The plan row tells STREAM_CLOSED for an id that left the closed
         set. But such an id is the same as a lower id that was never used, and
         h2spec http2/5.1.1/2 needs PROTOCOL_ERROR for it. *)
      error Protocol_error "HEADERS on a stream identifier that is not new"
  | Idle when stream land 1 = 0 ->
      (* h2spec http2/5.1.1/1 *)
      error Protocol_error "HEADERS on an even stream identifier"
  | Idle ->
      t.last_client <- stream ;
      let mcs = t.local.max_concurrent_streams in
      if Option.is_some t.goaway_last then begin
        remember t stream Ignored_id ;
        Ok Ignored
      end
      else if t.active >= mcs || live >= mcs then begin
        remember t stream Reset_by_us ;
        Ok Refused
      end
      else if self_dependent then begin
        (* h2spec http2/5.3.1/1 *)
        remember t stream Reset_by_us ;
        Ok (Reset Protocol_error)
      end
      else begin
        open_stream t stream ~end_stream ;
        Ok Open
      end

let too_large = error Enhance_your_calm "header block too large"

let on_headers t ~live ~stream ~end_stream ~end_headers ~priority ~block =
  let self_dependent =
    match priority with
    | Some (p : Frame.priority) ->
        p.dependency = stream
    | None ->
        false
  in
  if String.length block > t.max_block then too_large
  else
    match headers_use t ~live ~stream ~end_stream ~self_dependent with
    | Error _ as e ->
        e
    | Ok use ->
        if end_headers then Ok [Header_block {stream; block; end_stream; use}]
        else begin
          let buf = Buffer.create (String.length block) in
          Buffer.add_string buf block ;
          t.block <- Some {b_stream= stream; use; b_end_stream= end_stream; buf} ;
          Ok []
        end

let in_block t b (frame : Frame.t) =
  match frame with
  | Continuation {stream; end_headers; block} when stream = b.b_stream ->
      if Buffer.length b.buf + String.length block > t.max_block then too_large
      else begin
        Buffer.add_string b.buf block ;
        if end_headers then begin
          t.block <- None ;
          Ok
            [ Header_block
                { stream
                ; block= Buffer.contents b.buf
                ; end_stream= b.b_end_stream
                ; use= b.use } ]
        end
        else Ok []
      end
  | _ ->
      (* h2spec http2/4.3/2, http2/4.3/3, http2/5.5/2, http2/6.2/1, http2/6.2/2,
         http2/6.10/2 *)
      error Protocol_error "a frame other than CONTINUATION in a header block"

let on_data t ~stream ~end_stream ~data ~flow_length =
  match status t stream with
  | Idle ->
      (* h2spec http2/5.1/1 *)
      error Protocol_error "DATA on an idle stream"
  | Closed (Ended | Reset_by_peer) ->
      (* h2spec http2/5.1/8, http2/5.1/11, http2/6.1/2 *)
      error Stream_closed "DATA on a closed stream"
  | _ when flow_length > t.conn_recv ->
      error Flow_control_error "DATA over the connection window"
  | Active s when (not s.remote_ended) && flow_length > s.recv_window ->
      error Flow_control_error "DATA over the stream window"
  | status -> (
      t.conn_recv <- t.conn_recv - flow_length ;
      match status with
      | Active s when not s.remote_ended ->
          s.recv_window <- s.recv_window - flow_length ;
          let n = String.length data in
          s.received <- s.received + n ;
          if end_stream then s.remote_ended <- true ;
          if breaks_length s then begin
            (* h2spec http2/8.1.2.6/1, http2/8.1.2.6/2 *)
            close t s Reset_by_us ;
            Ok (sends (credit t flow_length) @ [Body_error {stream}])
          end
          else begin
            let held = if s.discarded then 0 else n in
            s.held <- s.held + held ;
            let frames = give_back t s (flow_length - held) in
            if end_stream && s.local_ended then close t s Ended ;
            Ok (sends frames @ [Data {stream; data; end_stream}])
          end
      | Active s ->
          (* h2spec http2/5.1/5: a half-closed (remote) stream. *)
          close t s Reset_by_us ;
          Ok
            ( sends (credit t flow_length)
            @ [Stream_error {stream; code= Stream_closed}] )
      | _ ->
          (* An id that we reset or ignored: DATA that the client sent before it
             got our RST_STREAM or GOAWAY. An id that left the closed set gets
             the same rule: RFC 9113 section 5.1 only permits an error. *)
          Ok (sends (credit t flow_length)) )

let on_rst_stream t ~stream =
  match status t stream with
  | Idle ->
      (* h2spec http2/5.1/2, http2/6.4/2 *)
      error Protocol_error "RST_STREAM on an idle stream"
  | Active s ->
      close t s Reset_by_peer ;
      Ok [Peer_reset {stream}]
  | Closed _ | Closed_unknown ->
      Ok []

let on_window_update t ~stream ~increment =
  if stream = 0 then
    if increment = 0 then
      (* h2spec http2/6.9/1 *)
      error Protocol_error "WINDOW_UPDATE of 0 on the connection"
    else if t.conn_send + increment > max_window then
      (* h2spec http2/6.9.1/2 *)
      error Flow_control_error "connection send window over 2^31-1"
    else begin
      t.conn_send <- t.conn_send + increment ;
      Ok [Credit]
    end
  else
    match status t stream with
    | Idle ->
        (* h2spec http2/5.1/3 *)
        error Protocol_error "WINDOW_UPDATE on an idle stream"
    | Closed _ | Closed_unknown ->
        Ok []
    | Active s ->
        if increment = 0 then begin
          (* h2spec http2/6.9/2 *)
          close t s Reset_by_us ;
          Ok [Stream_error {stream; code= Protocol_error}]
        end
        else if s.send_window + increment > max_window then begin
          (* h2spec http2/6.9.1/3 *)
          close t s Reset_by_us ;
          Ok [Stream_error {stream; code= Flow_control_error}]
        end
        else begin
          s.send_window <- s.send_window + increment ;
          Ok [Credit]
        end

(* The values of a SETTINGS frame that the state uses: the last
   SETTINGS_INITIAL_WINDOW_SIZE and the last SETTINGS_MAX_FRAME_SIZE, if any. A
   value out of its range is a connection error. *)
let rec read_settings ((window, max_frame) as last) = function
  | [] ->
      Ok last
  | (0x2, v) :: _ when v > 1 ->
      (* h2spec http2/6.5.2/1 *)
      error Protocol_error "SETTINGS_ENABLE_PUSH not 0 or 1"
  | (0x4, v) :: _ when v > max_window ->
      (* h2spec http2/6.5.2/2, http2/6.9.2/3 *)
      error Flow_control_error "SETTINGS_INITIAL_WINDOW_SIZE over 2^31-1"
  | (0x4, v) :: rest ->
      read_settings (Some v, max_frame) rest
  | (0x5, v) :: _ when v < 16384 || v > 0xff_ffff ->
      (* h2spec http2/6.5.2/3, http2/6.5.2/4 *)
      error Protocol_error "SETTINGS_MAX_FRAME_SIZE out of range"
  | (0x5, v) :: rest ->
      read_settings (window, Some v) rest
  | _ :: rest ->
      read_settings last rest

let on_settings t pairs =
  match read_settings (None, None) pairs with
  | Error _ as e ->
      e
  | Ok (window, max_frame) ->
      (* One delta for each frame, with its last value (plan section 15). *)
      let delta =
        match window with Some w -> w - t.peer_initial | None -> 0
      in
      let over =
        Hashtbl.fold
          (fun _ s over ->
            over || (s.closed = None && s.send_window + delta > max_window) )
          t.streams false
      in
      if over then
        (* h2spec http2/6.9.2/3 *)
        error Flow_control_error "a stream send window over 2^31-1"
      else begin
        t.settings_seen <- true ;
        Option.iter (fun v -> t.peer_max_frame <- v) max_frame ;
        t.peer_initial <- t.peer_initial + delta ;
        let ack = Send (Frame.Settings {ack= true; settings= []}) in
        if delta = 0 then Ok [ack]
        else begin
          Hashtbl.iter
            (fun _ s -> s.send_window <- s.send_window + delta)
            t.streams ;
          Ok [ack; Credit]
        end
      end

(* A frame when no header block is open. *)
let on_other t ~live (frame : Frame.t) =
  match frame with
  | Headers {stream; end_stream; end_headers; priority; block} ->
      on_headers t ~live ~stream ~end_stream ~end_headers ~priority ~block
  | Continuation _ ->
      (* h2spec http2/5.1/4, http2/6.10/4, http2/6.10/5, http2/6.10/6. Also on
         an id that left the closed set: a CONTINUATION frame has no stream
         state of its own. *)
      error Protocol_error "CONTINUATION with no open header block"
  | Data {stream; end_stream; data; flow_length} ->
      on_data t ~stream ~end_stream ~data ~flow_length
  | Rst_stream {stream; code= _} ->
      on_rst_stream t ~stream
  | Window_update {stream; increment} ->
      on_window_update t ~stream ~increment
  | Settings {ack= false; settings= pairs} ->
      on_settings t pairs
  | Settings {ack= true; _} ->
      Ok []
  | Ping {ack= false; data} ->
      (* h2spec http2/6.7/1 *)
      Ok [Send (Frame.Ping {ack= true; data})]
  | Ping {ack= true; _} ->
      (* h2spec http2/6.7/2 *)
      Ok []
  | Goaway {last_stream; code; _} ->
      Ok [Peer_goaway {last_stream; code}]
  | Push_promise _ ->
      (* h2spec http2/8.2/1 *)
      error Protocol_error "PUSH_PROMISE from a client"
  | Priority _ | Unknown _ ->
      (* h2spec http2/5.5/1 *)
      Ok []
  | Malformed {stream; code; reason} -> (
    match status t stream with
    | Active s ->
        close t s Reset_by_us ;
        Ok [Stream_error {stream; code}]
    | Idle ->
        (* h2spec http2/5.3.1/2 *)
        Error (code, reason)
    | Closed Ended ->
        (* h2spec http2/6.3/2: our END_STREAM can wait in the queue of the
           engine, thus for the client the stream is open. *)
        rekind t stream Reset_by_us ;
        Ok [Stream_error {stream; code}]
    | Closed (Reset_by_us | Ignored_id | Reset_by_peer) | Closed_unknown ->
        Ok [] )

let on_frame t ~live (frame : Frame.t) =
  if not t.settings_seen then
    match frame with
    | Settings {ack= false; settings= pairs} ->
        on_settings t pairs
    | _ ->
        (* h2spec http2/3.5/2 *)
        error Protocol_error "the first frame is not SETTINGS"
  else
    match t.block with
    | Some b ->
        in_block t b frame
    | None ->
        on_other t ~live frame

let send_window t ~stream =
  match Hashtbl.find_opt t.streams stream with
  | Some s ->
      min t.conn_send s.send_window
  | None ->
      0

let stream_window t ~stream =
  match Hashtbl.find_opt t.streams stream with
  | Some s ->
      s.send_window
  | None ->
      0

let sent t ~stream n =
  t.conn_send <- t.conn_send - n ;
  match Hashtbl.find_opt t.streams stream with
  | Some s ->
      s.send_window <- s.send_window - n
  | None ->
      ()

let end_local t ~stream =
  match Hashtbl.find_opt t.streams stream with
  | Some ({closed= None; _} as s) ->
      s.local_ended <- true ;
      if s.remote_ended then close t s Ended
  | _ ->
      ()

let reset_local t ~stream code =
  match Hashtbl.find_opt t.streams stream with
  | Some ({closed= None; _} as s) ->
      close t s Reset_by_us ;
      Some (Frame.Rst_stream {stream; code})
  | _ ->
      None

let goaway t code debug =
  let last = Option.value t.goaway_last ~default:t.last_client in
  t.goaway_last <- Some last ;
  Frame.Goaway {last_stream= last; code; debug}

let peer_max_frame_size t = t.peer_max_frame

let active_streams t = t.active

let block_open t = Option.is_some t.block

let known_streams t = Hashtbl.length t.streams

let receive_window t = t.conn_recv + t.conn_recv_unsent
