(** HTTP/2 ({{:https://www.rfc-editor.org/rfc/rfc9113}RFC 9113}) for caravan
    servers.

    {[
    let () =
      Caravan.Server.run ~engine:(Caravan_http2.auto ())
        (module Weft_os)
        handler
    ]}

    The engine gives each request to the handler as the HTTP/1.1 engine does.
    The request has the version [Http_2]. A handler that works with HTTP/1.1
    works with HTTP/2, with these differences:
    - A response to a request of HTTP/2 has no connection fields (for example
      [connection], [keep-alive], [transfer-encoding]). The engine drops them.
    - The engine sends no upgrade response ({!Caravan.Response.upgrade}), and no
      2xx response to CONNECT (no tunnels). For each one, it calls
      {!Caravan.Response.abandon}, reports an error to [on_error], and sends
      500.
    - The engine sends no server push (PUSH_PROMISE).

    {1:tls TLS}

    Over TLS, the client and the server agree on the protocol with ALPN (RFC
    9113 section 3.3). Put ["h2"] and ["http/1.1"] in the ALPN list of the TLS
    configuration, and wrap {!auto}:

    {[
    let tls =
      match
        Tls.Config.server ~certificates ~alpn_protocols:["h2"; "http/1.1"] ()
      with
      | Ok c ->
          c
      | Error (`Msg m) ->
          failwith m

    let engine = Caravan_tls.engine tls (Caravan_http2.auto ())
    ]} *)

(** The settings of the engine: the values of our SETTINGS frame, and the limits
    against floods. *)
module Settings = Settings

val engine : ?settings:Settings.t -> unit -> Caravan.Engine.t
(** [engine ~settings ()] is the HTTP/2 engine. Each connection must start with
    the connection preface of HTTP/2: the client knows that the server has
    HTTP/2 (prior knowledge, for example [curl --http2-prior-knowledge]), or TLS
    selected the ALPN protocol ["h2"]. A connection that starts with other bytes
    closes at once. To serve HTTP/1.1 on the same port, use {!auto}.

    [settings] is {!Settings.default} by default.

    @raise Invalid_argument if {!Settings.validate} refuses [settings]. *)

val auto :
  ?settings:Settings.t -> ?http1:Caravan.Engine.t -> unit -> Caravan.Engine.t
(** [auto ~settings ~http1 ()] serves HTTP/2 and HTTP/1.1. [http1] is the engine
    of HTTP/1.1 ({!Caravan.Server.http1} by default). For each connection,
    [auto] selects the engine:
    - The ALPN protocol ["h2"] ({!Caravan.Conn_info.alpn}): {!engine} with
      [settings].
    - Another ALPN protocol, for example ["http/1.1"]: [http1].
    - TLS with no ALPN protocol (the client offered none): [http1]. HTTP/2 over
      TLS needs the ALPN protocol ["h2"] (RFC 9113 section 3.3).
    - No TLS: [auto] compares the first bytes of the input with the connection
      preface of HTTP/2 (24 bytes), as the bytes come. At the first byte that
      differs, [http1] gets the connection with the bytes. When the 24 bytes are
      equal, {!engine} gets the connection with the bytes. If the input ends,
      the server stops, or [header_timeout] passes before a decision, the
      connection closes.

    The comparison usually needs one read: an HTTP/1.1 request differs in its
    first 3 bytes (only the method [PRI] starts as the preface), and a client of
    HTTP/2 sends the preface in one write.

    [auto] does not upgrade a connection of HTTP/1.1 to HTTP/2 ([Upgrade: h2c],
    which RFC 9113 removed).

    @raise Invalid_argument if {!Settings.validate} refuses [settings]. *)

(**/**)

(** Internal modules, exposed for tests. Not part of the public API. *)
module Private : sig
  module Error = Error
  module Frame = Frame
  module Huffman = Huffman
  module Hpack = Hpack
  module Conn_state = Conn_state
  module Message = Message
  module Limits = Limits
  module H2_server = H2_server
end
