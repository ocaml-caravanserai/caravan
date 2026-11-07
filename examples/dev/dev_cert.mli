(** A certificate authority (CA) and a server certificate in memory, for HTTPS
    during development. Never use them in production.

    {!make} makes a new CA key and a new server key at each call. The keys stay
    in memory, and no file gets them. Thus after the end of the program, nobody
    can sign a certificate with the CA. Give the CA to a client for one run, for
    example with [curl --cacert] (see {!write_ca}). Do not add it to the trust
    store of the system.

    The keys are P-256 keys. {!make} takes its random bytes from
    [Mirage_crypto_rng.default_generator]: set it first. *)

(** The type for a CA and its server certificate. *)
type t

val make : time:Ptime.t -> names:string list -> t
(** [make ~time ~names] is a new CA, and a server certificate that the CA signs.
    The server certificate is for each item of [names]: a DNS name, for example
    ["localhost"], or an IP address, for example ["127.0.0.1"]. The two
    certificates are valid from one day before [time] to 30 days after [time].
    The basic constraints of the CA have the path length 0: a CA under it is not
    valid (RFC 5280, section 4.2.1.9). The CA has a subject key identifier, and
    the server certificate has the same value in its authority key identifier.

    @raise Invalid_argument if [names] is empty. *)

val ca : t -> X509.Certificate.t
(** [ca t] is the certificate of the CA of [t]. A client that trusts it trusts
    the server certificate of [t]. *)

val write_ca : t -> string -> unit
(** [write_ca t path] writes the certificate of the CA of [t] to the file
    [path], in PEM. It replaces the file. It writes no key. *)

val server_config : t -> Tls.Config.server
(** [server_config t] is a TLS configuration of a server with the server
    certificate of [t], its key, and the ALPN protocol ["http/1.1"]. *)
