(** HTTP servers.

    {[
    let () = Server.run (module Weft_os) (fun _req -> Response.string "hello\n")
    ]}

    {!run} serves until Ctrl-C. [Weft_os] (library [weft.os]) is the world of
    the OS that the build runs on. For more control, {!start} a server in a
    [Weft.Fiber.scope] and stop it with {!shutdown}. *)

(** The type for running servers. *)
type t

val http1 : Engine.t
(** [http1] is the HTTP/1.1 engine, the default engine of {!start}. An engine
    that wraps another engine, for example the TLS engine of [caravan.tls], can
    wrap it.

    A connection with the ALPN protocol ["h2"] ({!Conn_info.alpn}) carries
    HTTP/2. [http1] reports a [Failure] to [on_error] of the configuration and
    closes the connection. To serve HTTP/2 also, use the engine
    [Caravan_http2.auto] of the library [caravan.http2]. *)

val start :
  ?config:Config.t -> ?engine:Engine.t -> Weft.Fiber.scope -> Handler.t -> t
(** [start ~config ~engine sw handler] listens on [config.addr] with
    {!Weft_flow.Tcp.Listener} (default {!Config.default}) and serves connections
    with [engine] (default HTTP/1.1). It calls [handler] for each request. At
    most [config.max_connections] connections are served at the same time.

    The server runs in a new fiber of [sw]. Thus [Fiber.scope] of [sw] returns
    only after the server stopped: call {!shutdown} or {!abort} before its body
    returns. A cancel of [sw] stops the server with no grace: the connections
    get the cancel at once, and {!Conn_info.stopping} becomes ready.

    @raise Weft.Net.Error
      if the address cannot be bound, for example with [In_use].
    @raise Weft.Fiber.Scope_closed if [sw] ended or is cancelled.
    @raise Invalid_argument if [config.max_connections] is less than 1. *)

val local_addr : t -> Weft.Net.addr
(** [local_addr s] is the address that [s] listens on, with its port when
    [config.addr] has the port [0]. *)

val shutdown : t -> unit
(** [shutdown s] stops [s]. It closes the listening socket, and idle connections
    close. The requests in progress get [config.shutdown_grace] to finish, and
    their responses end their connections. Then the server cancels them.

    After the close of the listening socket, the OS refuses new clients. On more
    than one domain, the close can occur a short time after [shutdown] returns.
    A client that connects in that time gets a reset after its connect.

    A handler can see the stop with {!Conn_info.stopping}. A response that
    switches protocols (an upgrade or a tunnel) gets a [503] in place of the
    switch (see {!Response.upgrade}). [shutdown] does not wait (see {!wait}). A
    second call does nothing. Call it from fiber code, also from a handler of
    [s].

    @raise Weft.Fiber.Not_in_fiber outside fiber code. *)

val abort : t -> unit
(** [abort s] is {!shutdown} with no grace: the server cancels the requests in
    progress at once. *)

val wait : t -> unit
(** [wait s] waits until [s] stopped and all its connections closed. It is a
    cancellation point. A cancel of the scope of the server does not make it
    raise. Call it from fiber code.

    @raise Weft.Net.Error
      if the accept of connections failed. This failure also stopped [s]. If no
      fiber calls [wait], weft reports the failure as a promise that no fiber
      awaited. An exception of [config.on_error] also goes out of [wait]. Two
      exceptions from it at the same time go out of [wait] as
      [Weft.Fiber.Multiple]. *)

val open_connections : t -> int
(** [open_connections s] is the number of connections that [s] serves at this
    time. *)

val stop_on_signals : t -> unit
(** [stop_on_signals s] waits for SIGINT and SIGTERM. The first signal calls
    {!shutdown}. The second signal calls {!abort} and [Weft.Signal.unwatch] for
    the two signals, then [stop_on_signals] returns. Then a third signal gets
    its default action, usually the end of the process. This ends a server with
    a handler that ignores the cancel.

    Two signals in one commit count as the second signal. A third signal in that
    commit is lost.

    Call it in a daemon fiber, one time for each world. It stops one server. It
    returns only after the second signal. Thus in a fiber that is not a daemon,
    the scope stays open after a stop by the first signal. With two calls in one
    world, each signal goes to one call only. Then the [unwatch] of one call
    makes the wait of the other call raise [Invalid_argument].

    The world must watch SIGINT and SIGTERM, for example [Weft.run ~signals] or
    [Weft_sim.run ~signals]. If not, the wait raises [Invalid_argument]. {!run}
    does this for you.

    @raise Weft.Net.Error
      if the OS gives an error for a signal, for example [ENOMEM] of [signalfd].
      An error of a wait comes before the next stop of [s]. An error of
      [Weft.Signal.unwatch] comes after the abort. If the unwatch of SIGINT
      fails, the world continues to watch the two signals. If the unwatch of
      SIGTERM fails, it continues to watch SIGTERM. A signal that the world
      watches does not get its default action. *)

(** {1:services Services} *)

(** A background part of the application that starts and stops with the server
    of {!run}, for example a queue of jobs (library [caravan.jobs]). *)
type service

val service :
     start:(Weft.Fiber.scope -> unit)
  -> stop:(grace:Weft.Span.t -> unit)
  -> service
(** [service ~start ~stop] is a service.
    - [start sw] must not block: it forks the fibers of the service in [sw].
    - [stop ~grace] blocks until the service ended. The work of the service gets
      at most [grace] (it can be zero), then the service cancels it. A second
      call does nothing. *)

val run :
     ?config:Config.t
  -> ?engine:Engine.t
  -> ?domains:int
  -> ?on_start:(t -> unit)
  -> ?services:service list
  -> (module Weft.OS)
  -> Handler.t
  -> unit
(** [run ~config ~engine ~domains ~on_start ~services (module O) handler] serves
    [handler] in the world [O] until the process gets SIGINT (Ctrl-C) or
    SIGTERM:
    + It calls [Weft.run ~domains ~signals:[SIGINT; SIGTERM] (module O)]. The
      world gives the default of [domains]: for the OS worlds of weft,
      [min 4 (Domain.recommended_domain_count ())].
    + It binds the address. Then it starts the [services] in list order.
    + It {!start}s the server and calls [on_start] with it, for example to print
      its address.
    + It calls {!stop_on_signals} in a daemon fiber: the first signal stops the
      server with a grace, the second signal aborts it, and a third signal gets
      its default action.
    + When the server stopped, it stops the [services] in reverse list order.
      Thus put a service that gives work to another service after it in the
      list.

    The services and the requests share one grace, [config.shutdown_grace]. It
    starts at the first {!shutdown}. The services run as usual while the
    requests finish: a handler can give them work in its grace. Then each stop
    gets the grace that remains at its start. The second signal also cancels the
    services.

    If a start raises, the services that started stop with no grace, and [run]
    raises the exception. If a service fails (a fiber of it raises), the server
    stops with its grace, the other services get a cancel, and [run] raises the
    failure. Each stop runs, also after a stop that raised.

    [O] is usually [Weft_os] (library [weft.os]), the world of the OS that the
    build runs on, or a world of one OS, for example [Weft_linux]. The Windows
    world of weft gives Ctrl-C as SIGINT, but it never gives SIGTERM.

    It returns when the server and the services stopped. Then the world closes,
    and SIGINT and SIGTERM get the action that they had before [run] (see the
    [close] of the world). Call it outside fiber code.

    @raise Failure if [O] is [Weft_os] on a system that has no OS world of weft.
    @raise Weft.Net.Error
      if the address cannot be bound, if the accept of connections failed, or
      for an error of the OS for SIGINT or SIGTERM (see {!stop_on_signals}). An
      error for a signal cancels the scope of the server: the requests in
      progress get no grace.
    @raise Weft.Fiber.Multiple
      if more than one of these raised: the wait of the server and the stops. *)

(**/**)

(** {1:seam Listener seam}

    For the tests, not part of the public API. A test gives a fake listener that
    makes [accept] errors that the simulator cannot make, for example
    [Other "EPROTO"]. {!Weft_flow.Tcp.Listener} has this signature. *)

(** The listening socket of a server. *)
module type LISTENER = sig
  type t

  val addr : t -> Weft.Net.addr

  val accept : t -> Weft.Net.t * Weft.Net.addr
  (** As {!Weft_flow.Tcp.Listener.accept}: it waits, it is a cancellation point,
      and it raises [Weft.Net.Error]. After [close], also for a call that waits
      at that time, it raises [Weft.Net.Error (Bad_fd, _)]. *)

  val close : t -> unit
  (** It never blocks, and a second call does nothing. *)
end

val start_with :
     ?config:Config.t
  -> ?engine:Engine.t
  -> Weft.Fiber.scope
  -> (module L : LISTENER) -> L.t -> Handler.t -> t
(** [start_with ~config ~engine sw (module L) listener handler] is {!start}, but
    it serves the connections of [listener]. It ignores [config.addr] and
    [config.backlog]. It closes [listener] when the server stops, or before it
    raises if the server cannot start.

    [(module L : LISTENER)] is a module-dependent argument (OCaml 5.5): the type
    of [listener] is the type [L.t] of the module. Production code gives
    [(module Weft_flow.Tcp.Listener)]. *)

val run_with :
     ?config:Config.t
  -> ?engine:Engine.t
  -> ?domains:int
  -> ?on_start:(t -> unit)
  -> ?services:service list
  -> (module Weft.OS)
  -> (module L : LISTENER) -> (unit -> L.t) -> Handler.t -> unit
(** [run_with ~config ~engine ~domains ~on_start ~services (module O) (module L)
     listen handler] is {!run}, but it serves the listener of [listen ()]. It
    calls [listen] in the world, before the services start. *)
