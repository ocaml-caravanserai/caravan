(** The limits against floods: token buckets, and the classes of the frames that
    a client can send in a flood. No I/O: the caller gives the time, in
    nanoseconds (for example since the start of the connection). *)

val max_tokens : int
(** The largest number of tokens and the largest refill of a bucket: 1_000_000.
*)

(** A token bucket: a number of tokens, and a refill in time. *)
module Bucket : sig
  type t

  val create : tokens:int -> refill:int -> t
  (** [create ~tokens ~refill] is a full bucket of [tokens] tokens (0 to
      {!max_tokens}) that gets [refill] tokens back in each second (0 to
      {!max_tokens}), at the time 0. *)

  val take : t -> now:(unit -> int) -> bool
  (** [take b ~now] takes one token. It gives [false] if no token is there. Only
      when the bucket is empty, it calls [now] and adds the tokens of the time
      since the last add, up to the size of the bucket. Thus a burst can take at
      most two times the size of the bucket. *)

  val give : t -> int -> unit
  (** [give b n] adds [n] tokens, up to the size of the bucket. *)

  val count : t -> int
  (** [count b] is the tokens of [b], for the tests. *)
end

(** The frame classes that have a bucket. *)
type frame_class =
  | Settings_frame  (** SETTINGS, also an ACK *)
  | Ping_frame  (** PING, also an ACK *)
  | Empty_frame
      (** DATA with no data and no END_STREAM, or CONTINUATION with no fragment
      *)
  | Priority_frame  (** PRIORITY, also a malformed one *)
  | Window_update_frame  (** WINDOW_UPDATE *)
  | Unknown_frame  (** a frame of an unknown type *)
  | Goaway_frame  (** GOAWAY *)
  | Closed_reset  (** a late RST_STREAM: on a stream that is closed *)

val class_of : Frame.t -> frame_class option
(** [class_of f] is the class of [f], or [None] for a frame with no bucket. A
    RST_STREAM frame has no class here: its class depends on the state of its
    stream. *)

val name : frame_class -> string
(** [name k] is the name of [k] in a text, for example ["PING"]. *)

(** The buckets of the frame classes. *)
type t

val create : tokens:int -> refill:int -> t
(** [create ~tokens ~refill] is a full bucket for each class, as
    {!Bucket.create}. *)

val take : t -> frame_class -> now:(unit -> int) -> bool
(** [take t k ~now] takes one token from the bucket of [k] ({!Bucket.take}). *)

val give : t -> frame_class -> int -> unit
(** [give t k n] adds [n] tokens to the bucket of [k]. *)

val count : t -> frame_class -> int
(** [count t k] is the tokens of the bucket of [k], for the tests. *)
