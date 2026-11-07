(** The HTTP/2 server engine (RFC 9113).

    Four kinds of fibers serve a connection:
    - R, the fiber of the connection, reads the frames and coordinates.
    - W, the writer, is the only fiber that writes the flow.
    - A stream fiber, one for each request, runs the handler and gives the
      frames of the response to W.
    - The watchdog owns the timers and the stop of the server. It wakes R when a
      deadline passed or when the server stops. Thus a wait of R for the next
      frame has no timer.

    A mutex protects the state that the fibers share. Each critical section
    gives a value and never raises.

    The connection ends:
    - At a stop of the server ([Conn_info.stopping]): GOAWAY NO_ERROR with the
      last stream, then the end when no stream fiber is live. Before the first
      SETTINGS frame of the client, a close at once.
    - At a GOAWAY NO_ERROR of the client: our GOAWAY NO_ERROR with the last
      stream, then the end when no stream fiber is live. A GOAWAY with another
      code cancels each fiber.
    - At a timer: the preface and the first SETTINGS frame in [header_timeout]
      (else a close with no GOAWAY), a header block in [header_timeout] (else
      GOAWAY ENHANCE_YOUR_CALM), and [idle_timeout] with no live stream fiber
      (GOAWAY NO_ERROR).
    - At the end of the input, at a connection error, at a write failure. An
      abort of the server cancels the connection: then the engine does no end
      sequence.

    This version does not have the limits of the flood protections.

    A request body error (a content-length rule, malformed trailers) resets the
    stream, and a read of the body raises [Body.Truncated], but the engine does
    not cancel the handler: its fiber stays live, and counts for
    SETTINGS_MAX_CONCURRENT_STREAMS, until the handler ends. *)

val default_local : Caravan.Config.t -> Conn_state.local
(** [default_local config] is the default settings of the server: 100 concurrent
    streams, an initial window of 65535, a header table of 4096 bytes, and
    [config.max_header_bytes] for the header list. *)

(** What the tests can examine after the end of the engine: the connection
    state, the number of the stream records of the engine, the number of the
    live stream fibers, the bytes of the buffers of the unread request DATA of
    all streams, the number of the scratch buffers of W, and the size of the
    dynamic table of the HPACK decoder. *)
type probe =
  { state: Conn_state.t
  ; records: unit -> int
  ; live: unit -> int
  ; inbound: unit -> int
  ; scratch: unit -> int
  ; table_size: unit -> int }

(** The hooks of the tests. The engine calls [inspect] one time, at its start,
    with the probe of the connection. R calls [on_yield] at each entry of its
    guarded read: after it read the state of the step, and before it reads the
    input. R calls [on_wait] just before each of its waits, and [on_step] at the
    start of each of its steps. A hook can raise: the engine then raises the
    exception. *)
type hooks =
  { inspect: probe -> unit
  ; on_yield: unit -> unit
  ; on_wait: unit -> unit
  ; on_step: unit -> unit }

val no_hooks : hooks
(** Hooks that do nothing. *)

val serve : ?local:Conn_state.local -> ?hooks:hooks -> Caravan.Engine.t
(** [serve ~local ~hooks ctx flow handler] serves the HTTP/2 connection [flow]
    with [handler], and returns when the connection ends. The client must start
    with the connection preface (prior knowledge, or ALPN [h2]). [local] is
    [default_local ctx.config] by default. [Conn_state.validate] must accept it.
    [hooks] is [no_hooks] by default. *)
