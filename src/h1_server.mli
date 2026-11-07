(* Internal. The HTTP/1.1 engine (RFC 9112). *)

val limits : Config.t -> H1_parse.limits
(** [limits c] are the parser limits of [c]. *)

val serve : Engine.t
(** [serve ctx flow handler] serves HTTP/1.1 requests on [flow]: one at a time,
    kept alive and pipelined. Each handler runs in a fiber of its own. It
    returns when the connection must close. Examples: the client closed it or
    asked for a close, a request was malformed, the engine could not complete a
    response, or the server stops. With the ALPN protocol ["h2"], it reports a
    [Failure] and returns at once. *)
