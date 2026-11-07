(** The settings of the HTTP/2 engine: the values of our SETTINGS frame, and the
    limits against floods.

    Each limit that a client passes ends the connection with GOAWAY
    ENHANCE_YOUR_CALM. The limits:
    - The rapid reset budget (CVE-2023-44487). Each stream that the client
      resets before the final head of the response costs 1, and so does each
      stream that the engine refuses or that has a malformed request. Each
      response that ends gives 1 back. The budget also gets
      [max_concurrent_streams] back in each second.
    - A token bucket for each frame class that a client can send in a flood:
      SETTINGS, PING, an empty DATA frame with no END_STREAM or an empty
      CONTINUATION frame, PRIORITY, WINDOW_UPDATE, a frame of an unknown type,
      GOAWAY, and RST_STREAM on a closed stream. Each frame takes a token.
    - The frames for stream 0 that wait while the engine cannot write them.

    Some tokens come back because of the output of the engine. Each DATA frame
    that the engine sends gives 2 tokens back to the WINDOW_UPDATE bucket (the
    client can answer with a WINDOW_UPDATE frame of the stream and one of the
    connection) and 1 token to the PING bucket (a client can measure the link
    with a PING for its data). A DATA frame that is small because of a small
    send window gives none. Each stream that starts a handler gives 1 token back
    to the PRIORITY bucket and to the bucket of unknown frames (a
    PRIORITY_UPDATE frame of RFC 9218 has an unknown type).

    A bucket that was full can take at most two times its tokens in a burst: the
    bucket adds the tokens of the time only when it is empty. *)

type t =
  { max_concurrent_streams: int
        (** SETTINGS_MAX_CONCURRENT_STREAMS, 1 to [2^32 - 1]. 100 by default:
            RFC 9113 section 6.5.2 recommends at least 100. *)
  ; initial_window: int
        (** SETTINGS_INITIAL_WINDOW_SIZE, 65535 to [2^31 - 1]: the receive
            window of each stream. 65535 by default. *)
  ; header_table_size: int
        (** SETTINGS_HEADER_TABLE_SIZE, 0 to [2^32 - 1]. 4096 by default. *)
  ; max_header_list_size: int option
        (** SETTINGS_MAX_HEADER_LIST_SIZE, 0 to [2^32 - 1]. [None] (the default)
            is [Config.max_header_bytes], the limit of HTTP/1.1. A header list
            over it gets a 431. *)
  ; reset_budget: int option
        (** The rapid reset budget, 0 to 1_000_000. [None] (the default) is
            [2 * max_concurrent_streams], at most 1_000_000. *)
  ; frame_tokens: int
        (** The tokens of the bucket of each frame class, 1 to 1_000_000. 1000
            by default. *)
  ; frame_refill: int
        (** The tokens that each bucket of a frame class gets back in each
            second, 0 to 1_000_000. 100 by default. *)
  ; control_queue: int
        (** The frames for stream 0 (for example the ACKs of PING frames) that
            the engine keeps while it cannot write them, 1 to 1_000_000. 1000 by
            default. *) }

val default : t
(** The defaults of the fields. *)

val validate : t -> unit
(** [validate s] checks the range of each field of [s].

    @raise Invalid_argument
      with a text that names the field, for example
      ["Caravan_http2.Settings: frame_tokens 0"]. *)
