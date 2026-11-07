open Weft
module Flow = Caravan.Flow

(* The TLS flow is that of weft-tls. *)
exception Handshake_error = Weft_tls.Handshake_error

let server = Weft_tls.server

let client = Weft_tls.client

(* [refuse name m] raises [Invalid_argument] for the function [name]. *)
let refuse name m = invalid_arg (Printf.sprintf "Caravan_tls.%s: %s" name m)

(* [h2_cipher cs] is [true] if HTTP/2 permits the cipher suite [cs] with TLS
   1.2: an ephemeral key exchange (ECDHE or DHE) and an AEAD cipher. These are
   the cipher suites that are not in the block list of RFC 9113 appendix A (also
   [Tls.Config.Ciphers.http2]). Each TLS 1.3 cipher suite is permitted. *)
let h2_cipher cs =
  let aead =
    match Tls.Ciphersuite.ciphersuite_privprot cs with
    | `AEAD _ ->
        true
    | `Block _ ->
        false
  in
  Tls.Ciphersuite.ciphersuite_tls13 cs
  || (Tls.Ciphersuite.ciphersuite_fs cs && aead)

(* [check name ~server c] refuses the configurations that caravan cannot use.
   [name] is the name of the function, for the message. [server] is [true] for
   the configuration of a server. *)
let check name ~server (c : Tls.Config.config) =
  let refuse = refuse name in
  (* A server can have HTTP/2: a TLS engine wraps Caravan_http2.auto. The client
     of caravan has only HTTP/1.1, and a server that gets "h2" sends HTTP/2. *)
  let permitted p = p = "http/1.1" || (server && p = "h2") in
  if not (List.for_all permitted c.alpn_protocols) then
    refuse
      ( if server then "the ALPN list has a protocol other than h2 and http/1.1"
        else "the ALPN list has a protocol other than http/1.1" ) ;
  (* With TLS 1.0 and a CBC cipher, ocaml-tls puts an empty record before each
     write: each one costs in the budget of the peer. *)
  ( match fst c.protocol_versions with
  | `TLS_1_0 | `TLS_1_1 ->
      refuse "the lowest version is before TLS 1.2"
  | `TLS_1_2 | `TLS_1_3 ->
      () ) ;
  (* RFC 9113 section 9.2.2: HTTP/2 over TLS 1.2 uses no cipher suite of the
     block list. With TLS 1.3 only, ocaml-tls keeps only the cipher suites of
     TLS 1.3 in [c.ciphers], and each one obeys the rule. *)
  if List.mem "h2" c.alpn_protocols && not (List.for_all h2_cipher c.ciphers)
  then
    refuse
      "with h2 and TLS 1.2, each cipher suite must have ECDHE or DHE and AEAD \
       (Tls.Config.Ciphers.http2)" ;
  (* RFC 9113 section 9.2.1: HTTP/2 over TLS 1.2 has no renegotiation and no TLS
     compression. ocaml-tls 2.1.3 refuses each renegotiation when [use_reneg] is
     false (the default): a ClientHello after the handshake gets the alert
     no_renegotiation and the session does not change, and the server sends no
     HelloRequest. Thus [check] refuses [use_reneg] with h2. ocaml-tls has no
     TLS compression: each ServerHello selects the method NULL, also when the
     client offers another method. *)
  if c.use_reneg && List.mem "h2" c.alpn_protocols then
    refuse "with h2, the configuration permits renegotiation" ;
  (* An attacker can replay the early data of TLS 1.3. Caravan does not do the
     rules of RFC 8470 (the field Early-Data, the status 425), thus it accepts
     no early data, with HTTP/1.1 and with HTTP/2. *)
  if Int32.compare c.zero_rtt 0l > 0 then
    refuse "the configuration permits early data (zero_rtt)" ;
  match Mirage_crypto_rng.default_generator () with
  | _ ->
      ()
  | exception Mirage_crypto_rng.No_default_generator ->
      refuse
        "no default generator: call Mirage_crypto_rng_unix.use_default () first"

(* [peer config host] is [config] for the host of a URL: an IPv6 address in
   brackets, an IPv4 address, or a name. [None] if [host] is not one of
   these. *)
let peer config host =
  let n = String.length host in
  let ip a = Some (Tls.Config.ip config a) in
  if
    ( n >= 2
    && host.[0] = '['
    && host.[n - 1] = ']' )
    [@mutate
      off
        "\"[]\" and a host with one bracket are not names either: the two \
         branches give None"]
  then
    match Ipaddr.V6.of_string (String.sub host 1 (n - 2)) with
    | Ok a ->
        ip (Ipaddr.V6 a)
    | Error _ ->
        None
  else
    match Ipaddr.V4.of_string host with
    | Ok a ->
        ip (Ipaddr.V4 a)
    | Error _ -> (
      match Result.bind (Domain_name.of_string host) Domain_name.host with
      | Ok name ->
          Some (Tls.Config.peer config name)
      | Error _ ->
          None )

(* [selected p] accepts the ALPN protocol [p] of a server. ocaml-tls 2.1.3 does
   not compare the protocol of a TLS 1.3 server with the list of the client. No
   server of ocaml-tls selects another protocol, thus a test gives an epoch of
   its own to [https_with]. *)
let selected = function
  | None | Some "http/1.1" ->
      Ok ()
  | Some p ->
      Error ("the server selected the protocol " ^ p)

(* [https_with ~client config] is [https config], with the handshake [client]. A
   test gives a [client] that returns an epoch of its own. *)
let https_with ~client config =
  let c = Tls.Config.of_client config in
  let refuse = refuse "https" in
  if Option.is_some c.peer_name then
    refuse "the configuration has a peer name: the URL gives it" ;
  if Option.is_some c.ip then
    refuse "the configuration has an IP address: the URL gives it" ;
  (* ocaml-tls does not compare the peer name of a cached session with the host,
     and a resumed session skips the check of the certificate. *)
  if Option.is_some c.cached_session || Option.is_some c.cached_ticket then
    refuse "the configuration has a cached session" ;
  check "https" ~server:false c ;
  fun ~host tcp ->
    match peer config host with
    | None ->
        Error ("not a host name or an address: " ^ host)
    | Some config -> (
      match client config tcp with
      | flow, (epoch : Tls.Core.epoch_data) ->
          Result.map (fun () -> flow) (selected epoch.alpn_protocol)
      | exception Handshake_error m ->
          Error m
      | exception Flow.Closed ->
          Error "the peer closed the connection" )

let https config = https_with ~client config

(* [guard tcp ~handshake read] is [tcp], with the reads of [read] while
   [!handshake]. Each other function goes to [tcp]. *)
let guard tcp ~handshake read =
  Flow.make
    ~read:(fun b off len ->
      if !handshake then read b off len else Flow.read tcp b off len )
    ~read_now:(Flow.read_now tcp) ~write:(Flow.write tcp)
    ~readable:(Flow.readable tcp)
    ~shutdown_send:(fun () -> Flow.shutdown_send tcp)
    ~close:(fun () -> Flow.close tcp)
    ~reset:(fun () -> Flow.reset tcp)
    ~set_read_deadline:(Flow.set_read_deadline tcp)
    ~set_write_deadline:(Flow.set_write_deadline tcp)
    ~set_read_timeout:(Flow.set_read_timeout tcp)
    ~set_write_timeout:(Flow.set_write_timeout tcp)
    ()

let engine config inner =
  let c = Tls.Config.of_server config in
  check "engine" ~server:true c ;
  (* A client that gets h2 sends HTTP/2, and Server.http1 refuses each such
     connection. The check finds only Server.http1 itself, not an engine that
     wraps it. *)
  if List.mem "h2" c.alpn_protocols && inner == Caravan.Server.http1 then
    refuse "engine"
      "the ALPN list has h2, but the inner engine is Caravan.Server.http1: use \
       Caravan_http2.auto" ;
  fun (ctx : Caravan.Engine.ctx) tcp handler ->
    let c = ctx.config in
    let deadline = Time.add Mono (Time.now Mono) c.header_timeout in
    let at_deadline =
      match deadline with
      | Some d ->
          Time.at Mono d
      | None ->
          never [@mutate off "no overflow for a timeout"]
    in
    let late () =
      match deadline with
      | Some d ->
          Span.compare (Time.diff Mono (Time.now Mono) d) Span.zero >= 0
      | None ->
          false [@mutate off "no overflow for a timeout"]
    in
    let stopping = Caravan.Conn_info.stopping ctx.conn in
    (* The handshake reads with read_now, in a choose with the stop and the
       deadline. A client that keeps the socket full never makes read_now wait:
       thus each read examines the stop and the deadline, and yields. The yield
       comes before read_now, so that a cancel in it takes no byte (the rule of
       Flow.make). *)
    let rec guarded b off len =
      (* The simulator runs the client only at a switch point, thus there the
         choose sees the stop and the deadline when the buffer is empty. The
         check and the yield are for a client that keeps the socket full. *)
      if
        ( ctx.is_stopping () || late () )
        [@mutate off "the choose sees the stop in the simulator"]
      then raise Flow.Timeout ;
      Fiber.yield () [@mutate off "the simulator switches at read_now"] ;
      match Flow.read_now tcp b off len with
      | Some n ->
          n
      | None -> (
        match
          sync
            (choose
               [ wrap (Flow.readable tcp) (fun () -> `Ready)
               ; wrap stopping (fun () -> `Stop)
               ; wrap at_deadline (fun () -> `Stop) ] )
        with
        | `Ready ->
            guarded b off len
        | `Stop ->
            raise Flow.Timeout )
    in
    (* Weft_tls reads the handshake with Flow.read of its base flow. Thus the
       base flow is [tcp] with the guarded read until the end of the handshake.
       This fiber sets [handshake] to [false] before [inner] starts the other
       fibers that read. *)
    let handshake = ref true in
    let base = guard tcp ~handshake guarded in
    (* The writes of a handshake are small: they fit the buffer of a socket. The
       timeout is a guard. The inner engine sets its own timeouts. *)
    Flow.set_write_timeout tcp (Some c.write_timeout)
    [@mutate off "no test fills the socket buffer in a handshake"] ;
    match server config base with
    | exception (Handshake_error _ | Flow.Closed | Flow.Timeout) ->
        ()
    | flow, epoch -> (
        handshake := false ;
        let conn =
          Caravan.Conn_info.with_tls ?alpn:epoch.alpn_protocol ctx.conn
        in
        inner {ctx with conn} flow handler ;
        (* The close_notify, if the inner engine did not send it, then the end
           of the TCP stream: best effort, with a short deadline. *)
        Flow.set_write_deadline tcp
          (Time.add Mono (Time.now Mono) c.linger_timeout)
        [@mutate off "no test fills the socket buffer after a response"] ;
        match Flow.shutdown_send flow with
        | () ->
            ()
        | exception (Flow.Closed | Flow.Timeout) ->
            () )

module Private = struct
  let https_with = https_with

  let guard = guard
end
