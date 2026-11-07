(** The state of an HTTP/2 connection on the server side, with no I/O
    ({{:https://www.rfc-editor.org/rfc/rfc9113#section-5.1}RFC 9113 section 5.1},
    {{:https://www.rfc-editor.org/rfc/rfc9113#section-6.9}section 6.9}).

    This module is a pure state machine. It decides the result of each frame
    from the client: the stream states, the closed set, the flow-control
    windows, the settings of the peer and the content-length rule. It does not
    decode HPACK, it does not run handlers and it does not write frames.

    The engine calls it under its mutex. No function raises, except {!validate}:
    each function gives a value.

    {b The closed set.} The state keeps the last
    [min (4 * max_concurrent_streams) 4096] closed identifiers, each with its
    kind: "reset by us", "ignored", "ended" (ended in the two directions) or
    "reset by the client". A frame on such an identifier gets the rule of its
    kind. A closed identifier that left the set is closed with no kind: DATA on
    it gives connection credit and is ignored, but HEADERS and CONTINUATION on
    it give [PROTOCOL_ERROR] (see {!on_frame}).

    {b The RST rule.} The engine sends RST_STREAM at most one time for each
    stream, and never for an idle stream. Each event that tells the engine to
    reset a stream ({!event.Stream_error}, {!event.Body_error}, a
    {!event.Header_block} with [Refused] or [Reset]) comes after this module put
    the stream in the closed set as "reset by us". Thus {!reset_local} then
    gives [None] for it. A RST_STREAM that the engine starts itself (with
    {!reset_local}) never goes on a stream that ended in the two directions. But
    some resets can come with the END_STREAM flag of the client after our
    END_STREAM, and the engine then sends its RST_STREAM:
    - A {!event.Body_error} (h2spec http2/8.1.2.6).
    - A {!event.Header_block} with [Reset PROTOCOL_ERROR] for trailers that end
      a body that is too short.
    - A {!event.Header_block} with [Reset PROTOCOL_ERROR] for trailers that
      depend on their own stream.
    - A {!event.Stream_error} for a malformed frame (for example a PRIORITY
      frame with a bad length) on a stream that ended in the two directions.
      Then the stream becomes "reset by us" (h2spec http2/6.3/2). A malformed
      frame on a stream of another closed kind is ignored.

    {b The time rules of the engine.}
    - The engine calls {!end_local} in the critical section that queues the
      frame with END_STREAM. Thus a later frame of the client sees the stream
      closed (h2spec http2/5.1/12).
    - For a RST_STREAM NO_ERROR after our END_STREAM, the writer calls
      {!reset_local} when it takes the RST_STREAM from the queue of the stream,
      and drops the RST_STREAM if the result is [None]: the client ended the
      stream meanwhile.
    - The engine calls {!discard} one time for each stream that [Open] opened:
      when the fiber of the stream ends, or at once when the engine starts no
      fiber for it (a malformed request, an early 4xx response). *)

(** The settings of the server. *)
type local =
  { max_concurrent_streams: int
        (** SETTINGS_MAX_CONCURRENT_STREAMS, 1 to [2^32 - 1]. *)
  ; initial_window: int
        (** SETTINGS_INITIAL_WINDOW_SIZE, 65535 to [2^31 - 1]. It is the receive
            window of each stream. The minimum is 65535, so that DATA that the
            client sends before it received these settings is legal. *)
  ; header_table_size: int  (** SETTINGS_HEADER_TABLE_SIZE, 0 to [2^32 - 1]. *)
  ; max_header_list_size: int
        (** SETTINGS_MAX_HEADER_LIST_SIZE, 0 to [2^32 - 1]. *) }

(** The type for connection states. *)
type t

val validate : local -> unit
(** [validate l] makes sure that each value of [l] is in its range.
    [Caravan_http2.engine] calls it when it builds the engine.

    @raise Invalid_argument for a value out of its range. *)

val create : local -> t
(** [create l] is the state of a new connection, before the client preface.
    [create] never raises: the caller validated [l] with {!validate}.

    The connection receive window is
    [min (2^31 - 1) (max_concurrent_streams * initial_window)]: the window that
    {!start_frames} announces. *)

val start_frames : t -> Frame.t list
(** [start_frames t] is the first frames of the server: its SETTINGS frame with
    the four values of {!local}, then a WINDOW_UPDATE frame on stream 0 that
    raises the connection receive window from 65535 to the window of {!create}.
    If that window is 65535, the WINDOW_UPDATE frame is not there: an increment
    of 0 is not legal. *)

(** What the engine does with a header block. The engine decodes each block with
    its HPACK decoder, whatever its use: the dynamic table needs each block. *)
type use =
  | Open  (** The block opens a new stream: a request. *)
  | Trailers  (** The block holds the trailers of an open stream. *)
  | Refused
      (** The engine decodes the block, then sends RST_STREAM REFUSED_STREAM. *)
  | Reset of Frame.code
      (** The engine decodes the block, then sends RST_STREAM with the code. *)
  | Ignored  (** The engine decodes the block, and does nothing more. *)

(** The type for the events that a frame of the client gives. *)
type event =
  | Send of Frame.t
      (** A frame to send: a SETTINGS ACK, a PING ACK, or a WINDOW_UPDATE frame
          on stream 0 or on the stream of a DATA frame. *)
  | Header_block of {stream: int; block: string; end_stream: bool; use: use}
      (** A complete header block: the fragments of a HEADERS frame and of its
          CONTINUATION frames, joined. The event comes at END_HEADERS.
          [end_stream] is the END_STREAM flag of the HEADERS frame. *)
  | Data of {stream: int; data: string; end_stream: bool}
      (** Request body bytes. The state holds [String.length data] bytes of the
          receive windows until {!consumed} or {!discard}. The engine drops the
          data of a stream that it discarded. *)
  | Body_error of {stream: int}
      (** A DATA frame broke the content-length rule (see
          {!set_content_length}). The engine sends RST_STREAM PROTOCOL_ERROR.
          The DATA frame of the error gives no [Data] event. Trailers that end a
          body that is too short give a [Header_block] with
          [Reset PROTOCOL_ERROR]. *)
  | Peer_reset of {stream: int}  (** The client reset the stream. *)
  | Credit
      (** The send windows changed: a WINDOW_UPDATE frame, or a SETTINGS frame
          that changed SETTINGS_INITIAL_WINDOW_SIZE. *)
  | Peer_goaway of {last_stream: int; code: Frame.code}
      (** The client sent GOAWAY with [code]. *)
  | Stream_error of {stream: int; code: Frame.code}
      (** A stream error (RFC 9113 section 5.4.2). The engine sends RST_STREAM
          with [code]. *)

val on_frame :
  t -> live:int -> Frame.t -> (event list, Frame.code * string) result
(** [on_frame t ~live f] applies the frame [f] of the client to [t], and gives
    its events in order. [live] is the count of the stream fibers of the engine
    that did not end. A new stream is refused when [active_streams t] or [live]
    is [max_concurrent_streams] or more.

    [Error (code, reason)] is a connection error (RFC 9113 section 5.4.1): the
    engine sends GOAWAY with [code] and ends the connection. The text tells the
    cause. After an error, [t] is not specified.

    The checks of a HEADERS frame on a new identifier, in this order:
    + An identifier of an open stream or of the closed set: the rules of its
      state.
    + An even identifier, or an identifier that is not more than the last
      identifier of the client: [PROTOCOL_ERROR].
    + An identifier after the last identifier of our GOAWAY: [Ignored], and the
      identifier goes into the closed set as "ignored".
    + [max_concurrent_streams] or more active streams or live fibers: [Refused],
      and the identifier goes into the closed set as "reset by us".
    + A priority that depends on its own stream: [Reset PROTOCOL_ERROR].
    + Else: [Open].

    Step 2 also applies to an identifier that left the closed set: it is the
    same as a lower identifier that was never used (h2spec http2/5.1.1/2).

    A header block of more than [max (4 * max_header_list_size) 65536] bytes,
    with all of its fragments, gives [ENHANCE_YOUR_CALM]. A smaller block over
    [max_header_list_size] gets the 431 response of the engine. *)

val set_content_length : t -> stream:int -> int -> event list
(** [set_content_length t ~stream n] sets the declared length of the request
    body of an active [stream]. The engine calls it after it decoded a head with
    a content-length. Then a DATA frame that makes the sum of the body more than
    [n] gives {!event.Body_error} at once. An END_STREAM flag with a sum less
    than [n] gives {!event.Body_error}. The result is [[Body_error]] if the body
    already broke the rule, for example after an END_STREAM flag on the HEADERS
    frame. *)

val consumed : t -> stream:int -> int -> Frame.t list
(** [consumed t ~stream n] gives back [n] bytes of [stream] to the receive
    windows: the handler read them. The result is the WINDOW_UPDATE frames to
    send. A window gets a WINDOW_UPDATE frame when its unsent credit is half of
    its size or more. A stream that the client ended gets no WINDOW_UPDATE
    frame. [n] is limited to the held bytes of [stream]. *)

val discard : t -> stream:int -> Frame.t list
(** [discard t ~stream] gives back all held bytes of [stream] to the receive
    windows, as {!consumed} does, and gives the WINDOW_UPDATE frames to send.
    The engine calls it one time for each stream that [Open] opened (see the
    time rules). After it, the state holds no byte of [stream]: the bytes of a
    later DATA frame go back at once. When the stream is closed and discarded,
    the state forgets its record. *)

val send_window : t -> stream:int -> int
(** [send_window t ~stream] is the number of bytes that the server can send on
    [stream] at this time: the minimum of the connection send window and the
    stream send window. It is negative if a SETTINGS frame made the stream
    window negative. It is [0] for a stream that the state does not know. *)

val sent : t -> stream:int -> int -> unit
(** [sent t ~stream n] records that the server sent [n] bytes of DATA on
    [stream]. *)

val end_local : t -> stream:int -> unit
(** [end_local t ~stream] records that the server sent END_STREAM on [stream].
    If the client also ended the stream, the stream goes into the closed set as
    "ended". *)

val reset_local : t -> stream:int -> Frame.code -> Frame.t option
(** [reset_local t ~stream code] resets [stream] with [code]. If [stream] is
    active, the stream goes into the closed set as "reset by us", and the result
    is the RST_STREAM frame. Else the result is [None]: the stream is idle,
    closed, or already reset. *)

val goaway : t -> Frame.code -> string -> Frame.t
(** [goaway t code debug] is our GOAWAY frame. Its last stream is the last
    identifier of the client. After it, a HEADERS frame on a new identifier
    after that last stream gives [Ignored]. A second GOAWAY frame keeps the last
    stream of the first one. *)

val peer_max_frame_size : t -> int
(** [peer_max_frame_size t] is the SETTINGS_MAX_FRAME_SIZE of the client: 16384
    before the client changes it. *)

val active_streams : t -> int
(** [active_streams t] is the count of the open and half-closed streams. *)

val block_open : t -> bool
(** [block_open t] is [true] while a header block waits for its END_HEADERS. *)

val known_streams : t -> int
(** [known_streams t] is the count of the stream records: the active streams,
    and the closed streams that the engine did not discard. It goes back to 0
    when all streams ended and were discarded. For the tests. *)

val receive_window : t -> int
(** [receive_window t] is the connection receive window with its credit that no
    WINDOW_UPDATE frame sent yet. With no held byte, it is the window of
    {!create}. For the tests. *)
