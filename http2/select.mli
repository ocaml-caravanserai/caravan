(** The selection of the engine of a connection: HTTP/2 or HTTP/1.1. The public
    functions are [Caravan_http2.engine] and [Caravan_http2.auto]. *)

val engine : ?settings:Settings.t -> unit -> Caravan.Engine.t
(** [engine ~settings ()] is the HTTP/2 engine with [settings]. It calls
    [Settings.validate] one time. *)

val auto :
  ?settings:Settings.t -> ?http1:Caravan.Engine.t -> unit -> Caravan.Engine.t
(** [auto ~settings ~http1 ()] selects the engine of each connection from the
    ALPN protocol, or, with no TLS, from the first bytes of the input. *)
