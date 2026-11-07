(** Server configuration.

    Start from {!default} and change what you need:
    {[
    let config = {Config.default with max_body= Some (64 * 1024 * 1024)}
    ]} *)

(** The type for server configurations. *)
type t =
  { addr: Weft.Net.addr
        (** The address to listen on. Port [0] selects a free port. *)
  ; backlog: int
        (** The length of the queue of pending connections. When the queue is
            full, the OS drops new connections, and each client waits for a
            retransmit of its SYN (1 s on Linux). The system can cap the value
            with no error: Linux uses at most [net.core.somaxconn] (4096 by
            default since Linux 5.4, 128 before), and macOS at most
            [kern.ipc.somaxconn] (128 by default). *)
  ; max_connections: int
        (** The connections that the server serves at the same time, 1 or more.
            More connections wait in the queue. [Server.start] raises
            [Invalid_argument] for a value less than 1. *)
  ; buffer_size: int
        (** The size of the read buffer and of the write buffer of each
            connection, in bytes. The server makes each buffer at its first use.
            The read buffer grows for a long request line or header section. *)
  ; max_request_line: int  (** A longer request line gets 414. *)
  ; max_header_bytes: int  (** A larger header section gets 431. *)
  ; max_header_count: int  (** More header fields get 431. *)
  ; max_body: int option
        (** A larger request body gets 413, or ends the connection after the
            response started. A chunked body counts as sent, with its framing.
            [None] is no limit: then a client can fill the memory of a handler
            that reads whole bodies. *)
  ; max_drain: int
        (** The server discards at most this number of bytes of a request body
            that the handler did not read, to keep the connection. The count
            includes the chunk framing. A larger body closes the connection. *)
  ; header_timeout: Weft.Span.t
        (** The time to receive a request head, and for a new connection to
            start its first head. A late head gets 408. *)
  ; idle_timeout: Weft.Span.t
        (** The time that a kept-alive connection waits for its next request. *)
  ; read_timeout: Weft.Span.t
        (** The time for each read of a request body, and for all of the discard
            of a body that the handler did not read. A late read gets 408 if the
            response did not start. *)
  ; write_timeout: Weft.Span.t
        (** The time for each write of a response that waits, from the start of
            its first wait. *)
  ; linger_timeout: Weft.Span.t
        (** The time that a connection that the server ends reads input, at most
            [max_drain] bytes, so that the client closes first (RFC 9112 section
            9.6). *)
  ; shutdown_grace: Weft.Span.t
        (** The time that requests in progress get to finish when the server
            stops. Then the server cancels them. A handler sees the stop with
            {!Conn_info.stopping}. If the end of the grace is after the end of
            the [Clock.mono] range (about 292 years), the grace never ends. *)
  ; date_header: bool  (** Responses get a [Date] field. *)
  ; cancel_on_disconnect: bool
        (** The server cancels a handler when its client closes the connection,
            or when a read of the connection fails. This applies after the
            server read the request body and before the response. A client that
            only half-closes looks the same as a close: set it to [false] to
            serve such clients. *)
  ; on_error: exn -> Printexc.raw_backtrace -> unit
        (** The server calls it with the exceptions of handlers and response
            bodies, and with the errors of accept. It also gets the error of a
            read of a connection that fails while its handler runs. At the fd
            limit, the server reports the first error of accept. Then it tries
            again each 100 ms, with no more reports, until an accept gives a
            connection. Two domains can call it at the same time.

            It must not raise. An exception from it at a handler, a body or a
            read cancels all connections of the server. An exception from it at
            an error of accept stops the server as a failure of the listener. In
            the two cases, [Server.wait] raises it. Two exceptions from it at
            the same time go out of [Server.wait] as [Weft.Fiber.Multiple].

            The default writes the exception to [stderr] with stdlib I/O, so a
            slow [stderr] blocks the domain. It ignores an error of [stderr],
            for example a broken pipe. *) }

val default : t
(** [default] listens on [127.0.0.1:8080] with a backlog of 4096 (up to 4096
    connections wait in the queue, if the system limit permits it) and at most
    1024 connections. It has 4 KiB buffers, the default limits (8 KiB request
    line, 16 KiB and 100 header fields), 8 MiB bodies and 64 KiB drains. Its
    timeouts are 10 s (head) and 30 s (idle, read, write), with 2 s of linger
    and a 10 s shutdown grace. It sends a [Date] field, cancels a handler when
    its client leaves, and writes errors on [stderr]. The time and the date come
    from the world. *)
