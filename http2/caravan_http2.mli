(** HTTP/2 ({{:https://www.rfc-editor.org/rfc/rfc9113}RFC 9113}) for caravan
    servers.

    The library is in progress: it has no public engine yet. The modules of
    {!Private} are for the tests. *)

(** The settings of the engine: the values of our SETTINGS frame, and the limits
    against floods. *)
module Settings = Settings

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
