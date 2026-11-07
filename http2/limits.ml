(* The limits against floods. See limits.mli. *)

let second = 1_000_000_000

let max_tokens = 1_000_000

module Bucket = struct
  (* [at] is the time of the last add. An add moves [at] only by the time of the
     tokens that it added, thus no part of a token is lost. *)
  type t = {size: int; refill: int; mutable count: int; mutable at: int}

  let create ~tokens ~refill = {size= tokens; refill; count= tokens; at= 0}

  (* Only an empty bucket gets the tokens of the time. The time to fill it comes
     first: then the product of the elapsed time and the refill is less than
     [size * second], at most 10^15, and the tokens are less than [size]. *)
  let add b now =
    if b.refill > 0 then begin
      let elapsed = now - b.at in
      if
        ( elapsed
        >= b.size * second / b.refill )
        [@mutate off "at the bound, the two branches give a full bucket"]
      then begin
        b.count <- b.size ;
        b.at <- now
      end
      else begin
        let n = elapsed * b.refill / second in
        b.count <- n ;
        b.at <- b.at + (n * second / b.refill)
      end
    end

  let take b ~now =
    if b.count = 0 then add b (now ()) ;
    if b.count > 0 then begin
      b.count <- b.count - 1 ;
      true
    end
    else false

  let give b n = b.count <- min b.size (b.count + n)

  let count b = b.count
end

type frame_class =
  | Settings_frame
  | Ping_frame
  | Empty_frame
  | Priority_frame
  | Window_update_frame
  | Unknown_frame
  | Goaway_frame
  | Closed_reset

let class_of : Frame.t -> frame_class option = function
  | Settings _ ->
      Some Settings_frame
  | Ping _ ->
      Some Ping_frame
  | Data {data= ""; end_stream= false; _} | Continuation {block= ""; _} ->
      Some Empty_frame
  | Priority _ | Malformed _ ->
      (* Only a PRIORITY frame can be malformed (Frame.read). *)
      Some Priority_frame
  | Window_update _ ->
      Some Window_update_frame
  | Unknown _ ->
      Some Unknown_frame
  | Goaway _ ->
      Some Goaway_frame
  | Data _ | Headers _ | Continuation _ | Rst_stream _ | Push_promise _ ->
      None

let name = function
  | Settings_frame ->
      "SETTINGS"
  | Ping_frame ->
      "PING"
  | Empty_frame ->
      "empty DATA or CONTINUATION"
  | Priority_frame ->
      "PRIORITY"
  | Window_update_frame ->
      "WINDOW_UPDATE"
  | Unknown_frame ->
      "unknown"
  | Goaway_frame ->
      "GOAWAY"
  | Closed_reset ->
      "late RST_STREAM"

type t =
  { settings: Bucket.t
  ; ping: Bucket.t
  ; empty: Bucket.t
  ; priority: Bucket.t
  ; window_update: Bucket.t
  ; unknown: Bucket.t
  ; goaway: Bucket.t
  ; closed_reset: Bucket.t }

let create ~tokens ~refill =
  let b () = Bucket.create ~tokens ~refill in
  { settings= b ()
  ; ping= b ()
  ; empty= b ()
  ; priority= b ()
  ; window_update= b ()
  ; unknown= b ()
  ; goaway= b ()
  ; closed_reset= b () }

let bucket t = function
  | Settings_frame ->
      t.settings
  | Ping_frame ->
      t.ping
  | Empty_frame ->
      t.empty
  | Priority_frame ->
      t.priority
  | Window_update_frame ->
      t.window_update
  | Unknown_frame ->
      t.unknown
  | Goaway_frame ->
      t.goaway
  | Closed_reset ->
      t.closed_reset

let take t k ~now = Bucket.take (bucket t k) ~now

let give t k n = Bucket.give (bucket t k) n

let count t k = Bucket.count (bucket t k)
