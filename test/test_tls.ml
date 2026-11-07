(* The tests of caravan.tls, in the simulator. Each test sets a new Fortuna
   generator with a fixed seed first, thus a run is the same for a seed. The
   simulator runs on one OS thread, thus Fortuna is correct here. *)

open Windtrap
open Caravan
open Weft
module T = Caravan_testing
module S = Caravan_tls

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan tls tests"
       (module Mirage_crypto_rng.Fortuna) )

(* The time of the simulator in the tests: 2025-06-15. The certificates are
   valid from 2020 to 2040. *)
let t0 = 1_750_000_000.

let posix_now () = Ptime.of_float_s (Time.posix_to_unix_s (Time.now Posix))

(* [setup ?server_alpn ?client_alpn ?peer_name ()] seeds the generator and gives
   a server and a client configuration. *)
let setup ?server_alpn ?client_alpn ?peer_name () =
  seed () ;
  Tls_support.configs ?server_alpn ?client_alpn ?peer_name ~time:posix_now ()

let server_addr = Net.Tcp ("127.0.0.1", 80)

(* [run ?config ?server_alpn ?client_alpn handler f] serves [handler] over TLS
   in the simulator at [t0], and gives [f] the client configuration. *)
let run ?config ?server_alpn ?client_alpn handler f =
  let scfg, ccfg = setup ?server_alpn ?client_alpn () in
  T.run ?config ~engine:(S.engine scfg Server.http1) handler (fun t ->
      Weft_sim.set_posix (T.sim t) (Time.posix_of_unix_s t0) ;
      f t ccfg )

(* [read_all ?strict flow] reads [flow] to its end. With [~strict:false]
   (default), [Flow.Closed] is the end: for a TCP flow that the server resets.
   With [~strict:true], a TLS flow must end with close_notify. *)
let read_all ?(strict = false) flow =
  let b = Buffer.create 4096 and buf = Bytes.create 4096 in
  let rec go () =
    match Flow.read flow buf 0 4096 with
    | 0 ->
        Buffer.contents b
    | n ->
        Buffer.add_subbytes b buf 0 n ;
        go ()
    | exception Flow.Closed when not strict ->
        Buffer.contents b
  in
  go ()

(* [exchange ccfg text] sends [text] on a new TLS connection and reads the
   answer to the end. *)
let exchange ccfg text =
  let flow, _ = S.client ccfg (Tcp.connect server_addr) in
  Fun.protect ~finally:(fun () -> Flow.close flow)
  @@ fun () ->
  Flow.write_string flow text ;
  read_all ~strict:true flow

let get path =
  Printf.sprintf
    "GET %s HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" path

(* [body_of response] is the text after the head. *)
let body_of r =
  let rec find i =
    if i + 4 > String.length r then fail "no end of the head"
    else if String.sub r i 4 = "\r\n\r\n" then i + 4
    else find (i + 1)
  in
  let i = find 0 in
  String.sub r i (String.length r - i)

(* A handler that tells the scheme and the ALPN of its request. *)
let info req =
  let alpn =
    Option.value ~default:"none" (Option.bind (Request.conn req) Conn_info.alpn)
  in
  Response.string (Request.scheme req ^ " " ^ alpn)

let count sub s =
  let n = String.length sub in
  let rec go i acc =
    if i + n > String.length s then acc
    else if String.sub s i n = sub then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

(* [read_until flow sub] reads [flow] until the bytes that it read contain
   [sub], and gives them. *)
let read_until flow sub =
  let b = Buffer.create 4096 and buf = Bytes.create 4096 in
  let rec go () =
    if count sub (Buffer.contents b) > 0 then Buffer.contents b
    else
      match Flow.read flow buf 0 4096 with
      | 0 ->
          fail ("the end of the stream before " ^ sub)
      | n ->
          Buffer.add_subbytes b buf 0 n ;
          go ()
  in
  go ()

let no_errors t =
  equal (list string) []
    (List.map (fun (e, _) -> Printexc.to_string e) (T.take_errors t))

let elapsed start = Span.to_ns (Time.diff Mono (Time.now Mono) start)

(* {1 A raw client of ocaml-tls} *)

(* [raw_handshake ?with_finished tcp ccfg] does the handshake of a client with
   Tls.Engine on [tcp], and gives the state. [with_finished tls] gives the state
   and the records to send in the same TCP write as the Finished message. *)
let raw_handshake ?with_finished tcp ccfg =
  let buf = Bytes.create 65536 in
  let tls, hello = Tls.Engine.client ccfg in
  Flow.write_string tcp hello ;
  let rec go tls =
    if not (Tls.Engine.handshake_in_progress tls) then tls
    else
      let n = Flow.read tcp buf 0 65536 in
      match Tls.Engine.handle_tls tls (Bytes.sub_string buf 0 n) with
      | Ok (tls, _, `Response r, _) -> (
        match (r, with_finished) with
        | Some fin, Some f when not (Tls.Engine.handshake_in_progress tls) ->
            let tls, more = f tls in
            Flow.write_string tcp (fin ^ more) ;
            tls
        | _ ->
            Option.iter (Flow.write_string tcp) r ;
            go tls )
      | Error (f, _) ->
          fail (Tls.Engine.string_of_failure f)
  in
  go tls

(* [raw_read ?on_data tcp tls] reads [tcp] with Tls.Engine to the end, and gives
   the plaintext. [on_data tls n] runs after each data, with the count of the
   bytes so far, and gives the next state. *)
let raw_read ?(on_data = fun tls _ -> tls) tcp tls =
  let buf = Bytes.create 65536 and out = Buffer.create 4096 in
  let rec go tls =
    match Flow.read tcp buf 0 65536 with
    | 0 ->
        ()
    | n -> (
      match Tls.Engine.handle_tls tls (Bytes.sub_string buf 0 n) with
      | Ok (tls, eof, `Response r, `Data d) ->
          Option.iter (Flow.write_string tcp) r ;
          Option.iter (Buffer.add_string out) d ;
          let tls = on_data tls (Buffer.length out) in
          if eof = None then go tls
      | Error (f, _) ->
          fail (Tls.Engine.string_of_failure f) )
  in
  go tls ; Buffer.contents out

let engine_tests =
  group "The engine"
    [ test "a GET over TLS, with the scheme https" (fun () ->
          run info (fun t ccfg ->
              let r = exchange ccfg (get "/") in
              is_true ~msg:r (String.starts_with ~prefix:"HTTP/1.1 200" r) ;
              equal string "https none" (body_of r) ;
              no_errors t ) )
    ; test "ALPN: the protocol of the session goes to Conn_info" (fun () ->
          run ~client_alpn:["h2"; "http/1.1"] info (fun _ ccfg ->
              equal string "https http/1.1" (body_of (exchange ccfg (get "/"))) ) )
    ; test "bodies of 1 MiB in the two directions" (fun () ->
          let data =
            String.init (1024 * 1024) (fun i -> Char.chr (i mod 251))
          in
          let echo req = Response.string (Body.to_string (Request.body req)) in
          run echo (fun _ ccfg ->
              let r =
                exchange ccfg
                  (Printf.sprintf
                     "POST / HTTP/1.1\r\n\
                      Host: localhost\r\n\
                      Connection: close\r\n\
                      Content-Length: %d\r\n\
                      \r\n\
                      %s"
                     (String.length data) data )
              in
              is_true ~msg:"the bytes" (String.equal data (body_of r)) ) )
    ; test "three requests on one connection" (fun () ->
          run info (fun _ ccfg ->
              let one = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" in
              equal int 3
                (count "HTTP/1.1 200" (exchange ccfg (one ^ one ^ get "/"))) ) )
    ; test "the request in the same TCP write as the Finished message"
        (fun () ->
          run info (fun _ ccfg ->
              let tcp = Tcp.connect server_addr in
              Fun.protect ~finally:(fun () -> Flow.close tcp)
              @@ fun () ->
              let with_finished tls =
                Option.get (Tls.Engine.send_application_data tls [get "/"])
              in
              let tls = raw_handshake ~with_finished tcp ccfg in
              equal string "https none" (body_of (raw_read tcp tls)) ) )
    ; test "a handshake that fails or ends early is not reported" (fun () ->
          run info (fun t _ ->
              (* A handshake record with a bad body ("hello" declares a long
                 message): the server ends the connection. *)
              let tcp = Tcp.connect server_addr in
              Flow.write_string tcp "\x16\x03\x01\x00\x05hello" ;
              ignore (read_all tcp) ;
              Flow.close tcp ;
              (* A client that closes at once. *)
              Flow.close (Tcp.connect server_addr) ;
              (* The header of a long record: the deadline of the head ends
                 it. *)
              let tcp = Tcp.connect server_addr in
              Flow.write_string tcp "GET / HTTP/1.1\r\n\r\n" ;
              let start = Time.now Mono in
              ignore (read_all tcp) ;
              Flow.close tcp ;
              is_true ~msg:"the deadline of the head"
                (elapsed start >= Span.to_ns Config.default.header_timeout) ;
              T.settle t ;
              no_errors t ) )
    ; test "a handshake of more than 64 KiB ends" (fun () ->
          run info (fun t ccfg ->
              (* A ClientHello, then ChangeCipherSpec records: ocaml-tls accepts
                 each one, thus only the limit ends the handshake. *)
              let tcp = Tcp.connect server_addr in
              let _, hello = Tls.Engine.client ccfg in
              Flow.write_string tcp hello ;
              let ccs =
                String.concat ""
                  (List.init 2000 (fun _ -> "\x14\x03\x03\x00\x01\x01"))
              in
              let start = Time.now Mono in
              ( match
                  for _ = 1 to 10 do
                    Flow.write_string tcp ccs
                  done
                with
              | () ->
                  ()
              | exception Flow.Closed ->
                  () ) ;
              ignore (read_all tcp) ;
              Flow.close tcp ;
              is_true ~msg:"before the deadline"
                (elapsed start < Span.to_ns (Span.s 1)) ;
              T.settle t ;
              no_errors t ) )
    ; test "an idle connection ends with close_notify" (fun () ->
          run info (fun _ ccfg ->
              let flow, _ = S.client ccfg (Tcp.connect server_addr) in
              Fun.protect ~finally:(fun () -> Flow.close flow)
              @@ fun () ->
              Flow.write_string flow "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" ;
              (* The server closes after the idle timeout: the read gives 0, not
                 Closed. *)
              let r = read_all ~strict:true flow in
              equal string "https none" (body_of r) ) )
    ; test "a client that leaves with no close_notify is not reported"
        (fun () ->
          run info (fun t ccfg ->
              let tcp = Tcp.connect server_addr in
              let flow, _ = S.client ccfg tcp in
              Flow.write_string flow "GET / HTTP/1.1\r\nHost: localhost\r\n" ;
              Flow.close tcp ;
              T.settle t ;
              no_errors t ) )
    ; test "a stop of the server ends a slow handshake" (fun () ->
          run info (fun t _ ->
              let tcp = Tcp.connect server_addr in
              let start = Time.now Mono in
              let reader = T.background t (fun () -> ignore (read_all tcp)) in
              Time.sleep Mono (Span.ms 100) ;
              Server.shutdown (T.server t) ;
              Promise.await reader ;
              Flow.close tcp ;
              is_true ~msg:"less than 1 s"
                (elapsed start < Span.to_ns (Span.s 1)) ) )
    ; test "a stop or the deadline before the handshake: no read" (fun () ->
          (* The ClientHello waits in the socket. The engine examines the stop
             and the deadline before each read. A header_timeout of 0 puts the
             deadline at the time of the first read. *)
          let first_read ~stopping ~header_timeout =
            let scfg, ccfg = setup () in
            Sim_support.run (fun sim ->
                Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
                Sim_support.with_pair sim (fun a b ->
                    let client = Tcp.flow a and tcp = Tcp.flow b in
                    let _, hello = Tls.Engine.client ccfg in
                    Flow.write_string client hello ;
                    let ran = ref false in
                    let ctx =
                      { Engine.config= {Config.default with header_timeout}
                      ; conn=
                          Conn_info.make ~id:0 ~peer:server_addr
                            ~local:server_addr
                            ~stopping:(if stopping then always () else never)
                            ()
                      ; is_stopping= (fun () -> stopping) }
                    in
                    S.engine scfg (fun _ _ _ -> ran := true) ctx tcp info ;
                    let wrote =
                      Option.is_some (Flow.read_now client (Bytes.create 1) 0 1)
                    in
                    (!ran, wrote) ) )
          in
          let none = (false, false) in
          equal (pair bool bool) ~msg:"a stop" none
            (first_read ~stopping:true
               ~header_timeout:Config.default.header_timeout ) ;
          equal (pair bool bool) ~msg:"the deadline" none
            (first_read ~stopping:false ~header_timeout:Span.zero) )
    ; (* The engine reads the handshake with the deadline header_timeout. After
         the handshake, a read that waits has no such deadline. The head of the
         second request comes in two TLS records, after that deadline: the read
         of its second record waits. *)
      test "a read that waits after the deadline of the handshake" (fun () ->
          run info (fun _ ccfg ->
              let flow, _ = S.client ccfg (Tcp.connect server_addr) in
              Fun.protect ~finally:(fun () -> Flow.close flow)
              @@ fun () ->
              Flow.write_string flow "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" ;
              ignore (read_until flow "https none") ;
              (* The default header_timeout is 10 s. *)
              Time.sleep Mono (Span.s 15) ;
              Flow.write_string flow "GET / HTTP/1.1\r\nHost: loc" ;
              Time.sleep Mono (Span.s 1) ;
              Flow.write_string flow "alhost\r\nConnection: close\r\n\r\n" ;
              equal string "https none" (body_of (read_all ~strict:true flow)) ) )
    ; (* The TLS flow gives the read timeout of the engine to the TCP flow. A
         body that stops in the middle ends at the read timeout. *)
      test "a body that stops ends at the read timeout" (fun () ->
          let config = {Config.default with read_timeout= Span.s 2} in
          run ~config
            (fun req -> Response.string (Body.to_string (Request.body req)))
            (fun t ccfg ->
              let flow, _ = S.client ccfg (Tcp.connect server_addr) in
              Fun.protect ~finally:(fun () -> Flow.close flow)
              @@ fun () ->
              Flow.set_read_deadline flow
                (Time.add Mono (Time.now Mono) (Span.s 20)) ;
              Flow.write_string flow
                "POST / HTTP/1.1\r\n\
                 Host: localhost\r\n\
                 Content-Length: 10\r\n\
                 \r\n\
                 abc" ;
              let start = Time.now Mono in
              ignore (read_all flow) ;
              let e = elapsed start in
              is_true
                ~msg:(Printf.sprintf "%Ld ns" e)
                (e >= Span.to_ns (Span.s 2) && e < Span.to_ns (Span.s 3)) ;
              ignore (T.take_errors t) ) )
    ; (* The TLS flow gives the read deadline of the head to the TCP flow. *)
      test "a head that stops ends at header_timeout" (fun () ->
          let config = {Config.default with header_timeout= Span.s 3} in
          run ~config info (fun _ ccfg ->
              let flow, _ = S.client ccfg (Tcp.connect server_addr) in
              Fun.protect ~finally:(fun () -> Flow.close flow)
              @@ fun () ->
              Flow.set_read_deadline flow
                (Time.add Mono (Time.now Mono) (Span.s 60)) ;
              Flow.write_string flow "GET / HTTP/1.1\r\nHost: lo" ;
              let start = Time.now Mono in
              ignore (read_all flow) ;
              let e = elapsed start in
              is_true
                ~msg:(Printf.sprintf "%Ld ns" e)
                (e >= Span.to_ns (Span.s 3) && e < Span.to_ns (Span.s 4)) ) )
    ; test "the guard flow of the handshake gives each function to the TCP flow"
        (fun () ->
          let log = ref [] in
          let note s = log := s :: !log in
          let fake () =
            Flow.make
              ~read:(fun _ _ _ -> note "read" ; 1)
              ~read_now:(fun _ _ _ -> note "read_now" ; Some 1)
              ~write:(fun _ _ _ -> note "write")
              ~readable:(guard (fun () -> note "readable" ; always ()))
              ~shutdown_send:(fun () -> note "shutdown_send")
              ~close:(fun () -> note "close")
              ~reset:(fun () -> note "reset")
              ~set_read_deadline:(fun _ -> note "read deadline")
              ~set_write_deadline:(fun _ -> note "write deadline")
              ~set_read_timeout:(fun _ -> note "read timeout")
              ~set_write_timeout:(fun _ -> note "write timeout")
              ()
          in
          Sim_support.run (fun _ ->
              let handshake = ref true in
              let f =
                S.Private.guard (fake ()) ~handshake (fun _ _ _ ->
                    note "guarded" ; 1 )
              in
              let b = Bytes.create 1 in
              ignore (Flow.read f b 0 1) ;
              handshake := false ;
              ignore (Flow.read f b 0 1) ;
              ignore (Flow.read_now f b 0 1) ;
              Flow.write f b 0 1 ;
              sync (Flow.readable f) ;
              Flow.set_read_deadline f None ;
              Flow.set_write_deadline f None ;
              Flow.set_read_timeout f None ;
              Flow.set_write_timeout f None ;
              Flow.shutdown_send f ;
              Flow.reset f ;
              Flow.close (S.Private.guard (fake ()) ~handshake (fun _ _ _ -> 1)) ) ;
          equal (list string)
            [ "guarded"
            ; "read"
            ; "read_now"
            ; "write"
            ; "readable"
            ; "read deadline"
            ; "write deadline"
            ; "read timeout"
            ; "write timeout"
            ; "shutdown_send"
            ; "reset"
            ; "close" ]
            (List.rev !log) )
    ; test "the client refuses a certificate for another name" (fun () ->
          let peer_name =
            Domain_name.(host_exn (of_string_exn "example.org"))
          in
          let scfg, _ = setup () in
          let _, ccfg = setup ~peer_name () in
          T.run ~engine:(S.engine scfg Server.http1) info (fun t ->
              Weft_sim.set_posix (T.sim t) (Time.posix_of_unix_s t0) ;
              raises_match
                (function S.Handshake_error _ -> true | _ -> false)
                (fun () -> ignore (exchange ccfg (get "/"))) ) )
    ; test "the fds after 20 connections" (fun () ->
          run info (fun t ccfg ->
              ignore (exchange ccfg (get "/")) ;
              T.settle t ;
              let n = Weft_sim.open_fds (T.sim t) in
              for _ = 1 to 20 do
                ignore (exchange ccfg (get "/"))
              done ;
              T.settle t ;
              equal int n (Weft_sim.open_fds (T.sim t)) ) )
    ; test "engine refuses h2c, TLS 1.0 and a missing generator" (fun () ->
          let is_invalid = function Invalid_argument _ -> true | _ -> false in
          let scfg, _ = setup ~server_alpn:["h2c"; "http/1.1"] () in
          raises_match is_invalid (fun () ->
              ignore (S.engine scfg Server.http1 : Engine.t) ) ;
          let old_tls =
            Result.get_ok
              (Tls.Config.server ~version:(`TLS_1_0, `TLS_1_3)
                 ~certificates:
                   (let _, leaf, key = Tls_support.pki () in
                    `Single ([leaf], key) )
                 () )
          in
          raises_match is_invalid (fun () ->
              ignore (S.engine old_tls Server.http1 : Engine.t) ) ;
          let scfg, _ = setup () in
          Mirage_crypto_rng.unset_default_generator () ;
          Fun.protect ~finally:seed
          @@ fun () ->
          raises_match is_invalid (fun () ->
              ignore (S.engine scfg Server.http1 : Engine.t) ) ) ]

(* A listener and a connection for the handshakes with no HTTP. *)
let listen () = Net.listen (Net.Tcp ("server", 443))

let connect () = Tcp.connect (Net.Tcp ("server", 443))

(* {1 The client: Caravan_tls.https} *)

module C = Caravan_client

(* GET gives "hello", POST gives its body. *)
let echo req =
  if Method.equal (Request.meth req) Method.POST then
    Response.string (Body.to_string (Request.body req))
  else Response.string "hello"

(* [run_https ?ips ?config ?count handler f] serves [handler] over TLS, as
   [run], with [localhost] at 127.0.0.1. The test host listens on port 80, thus
   the URLs are [https://localhost:80/]. It gives [f] the hook of a client
   configuration with no peer name. [count] counts the connections. *)
let run_https ?ips ?config ?(count = Atomic.make 0) handler f =
  seed () ;
  let scfg, ccfg =
    Tls_support.configs ~client_peer:false ?ips ~time:posix_now ()
  in
  let engine ctx tcp h =
    Atomic.incr count ;
    S.engine scfg Server.http1 ctx tcp h
  in
  T.run ?config ~engine handler (fun t ->
      Weft_sim.set_posix (T.sim t) (Time.posix_of_unix_s t0) ;
      Weft_sim.set_dns (T.sim t) "localhost" (Addresses ["127.0.0.1"]) ;
      f t (S.https ccfg) )

(* The text of an error of the client: ["Tls"] for [Tls _], whose text comes
   from ocaml-tls. *)
let client_error f =
  match f () with
  | _ ->
      "no error"
  | exception C.Error (Tls _) ->
      "Tls"
  | exception e ->
      Printexc.to_string e

let get_text c url = Body.to_string (C.body (C.get c url))

let post_text c url body =
  let scheme, authority, path = C.Private.parse_url url in
  Body.to_string
    (C.body
       (C.request c
          (Request.make ~scheme ~authority ~body:(Body.of_string body)
             Method.POST path ) ) )

let localhost_ip = Ipaddr.of_string_exn "127.0.0.1"

(* [epoch ()] is the data of a TLS session, for a cached session. *)
let epoch () =
  let scfg, ccfg = setup () in
  Sim_support.run (fun sim ->
      Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
      let l = listen () in
      Fiber.scope (fun s ->
          let server =
            Fiber.fork_promise s (fun () ->
                let c, _ = Net.accept l in
                S.server scfg (Tcp.flow c) )
          in
          let flow, epoch = S.client ccfg (connect ()) in
          let sflow, _ = Promise.await server in
          Flow.close flow ; Flow.close sflow ; Net.close l ; epoch ) )

(* A flow with no peer: the fake handshakes do not use it. *)
let connect_nothing () =
  Flow.make
    ~read:(fun _ _ _ -> 0)
    ~read_now:(fun _ _ _ -> None)
    ~write:(fun _ _ _ -> ())
    ~readable:(always ()) ~shutdown_send:ignore ~close:ignore
    ~set_read_deadline:ignore ~set_write_deadline:ignore ()

let client_tests =
  group "The client"
    [ test "a GET and a POST over https" (fun () ->
          run_https echo (fun _ tls ->
              C.run ~tls (fun c ->
                  equal string "hello" (get_text c "https://localhost:80/") ;
                  equal string "data"
                    (post_text c "https://localhost:80/" "data") ) ) )
    ; test "requests in sequence use one TLS connection" (fun () ->
          let count = Atomic.make 0 in
          run_https ~count echo (fun _ tls ->
              C.run ~tls (fun c ->
                  for _ = 1 to 3 do
                    equal string "hello" (get_text c "https://localhost:80/")
                  done ) ) ;
          equal int 1 (Atomic.get count) )
    ; test "a certificate of a different CA gives Error (Tls _)" (fun () ->
          run_https echo (fun _ _ ->
              (* The generator continues: a new CA. *)
              let _, other =
                Tls_support.configs ~client_peer:false ~time:posix_now ()
              in
              C.run ~tls:(S.https other) (fun c ->
                  equal string "Tls"
                    (client_error (fun () -> C.get c "https://localhost:80/")) ) ) )
    ; test "an IP host checks the address of the certificate" (fun () ->
          run_https ~ips:[localhost_ip] echo (fun _ tls ->
              C.run ~tls (fun c ->
                  equal string "hello" (get_text c "https://127.0.0.1:80/") ) ) ;
          run_https echo (fun _ tls ->
              C.run ~tls (fun c ->
                  equal string "Tls"
                    (client_error (fun () -> C.get c "https://127.0.0.1:80/")) ) ) )
    ; test "a host that is not a name gives Error (Tls _)" (fun () ->
          run_https echo (fun t tls ->
              Weft_sim.set_dns (T.sim t) "a_b" (Addresses ["127.0.0.1"]) ;
              C.run ~tls (fun c ->
                  equal string "Tls"
                    (client_error (fun () -> C.get c "https://a_b:80/")) ) ) )
    ; test "a name goes in SNI, an address does not" (fun () ->
          seed () ;
          let scfg, ccfg =
            Tls_support.configs ~client_peer:false
              ~ips:[localhost_ip; Ipaddr.of_string_exn "::1"]
              ~time:posix_now ()
          in
          let names = ref [] in
          Sim_support.run (fun sim ->
              Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
              Weft_sim.set_dns sim "localhost" (Addresses ["127.0.0.1"]) ;
              let l4 = Net.listen (Net.Tcp ("127.0.0.1", 443))
              and l6 = Net.listen (Net.Tcp ("::1", 443)) in
              Fiber.scope (fun s ->
                  let server =
                    Fiber.fork_promise s (fun () ->
                        List.iter
                          (fun l ->
                            let c, _ = Net.accept l in
                            let tcp = Tcp.flow c in
                            ( match S.server scfg tcp with
                            | _, e ->
                                names :=
                                  Option.fold ~none:"no SNI"
                                    ~some:Domain_name.to_string
                                    e.Tls.Core.own_name
                                  :: !names
                            | exception _ ->
                                names := "failed" :: !names ) ;
                            Flow.close tcp )
                          [l4; l6; l4] )
                  in
                  C.run ~tls:(S.https ccfg) (fun c ->
                      List.iter
                        (fun url ->
                          ignore (client_error (fun () -> C.get c url)) )
                        [ "https://127.0.0.1/"
                        ; "https://[::1]/"
                        ; "https://localhost/" ] ) ;
                  Promise.await server ;
                  Net.close l4 ;
                  Net.close l6 ) ) ;
          equal (list string)
            ["no SNI"; "no SNI"; "localhost"]
            (List.rev !names) )
    ; test "an idle connection that the server ended is not used" (fun () ->
          (* The server ends an idle connection after 1 s, with close_notify.
             The client keeps idle connections for 5 s. A POST gets no retry: a
             reuse of the ended connection fails. *)
          let count = Atomic.make 0 in
          let config = {Config.default with idle_timeout= Span.s 1} in
          run_https ~config ~count echo (fun _ tls ->
              C.run ~tls (fun c ->
                  equal string "hello" (get_text c "https://localhost:80/") ;
                  Time.sleep Mono (Span.s 2) ;
                  equal string "data"
                    (post_text c "https://localhost:80/" "data") ) ) ;
          equal int 2 (Atomic.get count) )
    ; test "a body to the end of the connection needs close_notify" (fun () ->
          let serve ~notify =
            seed () ;
            let scfg, ccfg =
              Tls_support.configs ~client_peer:false ~time:posix_now ()
            in
            Sim_support.run (fun sim ->
                Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
                Weft_sim.set_dns sim "localhost" (Addresses ["127.0.0.1"]) ;
                let l = Net.listen (Net.Tcp ("127.0.0.1", 443)) in
                Fiber.scope (fun s ->
                    let server =
                      Fiber.fork_promise s (fun () ->
                          let c, _ = Net.accept l in
                          let tcp = Tcp.flow c in
                          let flow, _ = S.server scfg tcp in
                          let b = Buffer.create 256
                          and buf = Bytes.create 256 in
                          let rec head () =
                            let n = Flow.read flow buf 0 256 in
                            Buffer.add_subbytes b buf 0 n ;
                            let h = Buffer.contents b in
                            let len = String.length h in
                            if
                              n > 0
                              && not
                                   ( len >= 4
                                   && String.sub h (len - 4) 4 = "\r\n\r\n" )
                            then head ()
                          in
                          head () ;
                          Flow.write_string flow "HTTP/1.1 200 OK\r\n\r\nhello" ;
                          if notify then Flow.shutdown_send flow ;
                          Flow.close tcp )
                    in
                    let r =
                      C.run ~tls:(S.https ccfg) (fun c ->
                          match get_text c "https://localhost/" with
                          | text ->
                              text
                          | exception e ->
                              client_error (fun () -> raise e) )
                    in
                    Promise.await server ; Net.close l ; r ) )
          in
          equal string ~msg:"close_notify" "hello" (serve ~notify:true) ;
          equal string ~msg:"no close_notify"
            "Caravan_client.Error: the response body is truncated"
            (serve ~notify:false) )
    ; test "https refuses a configuration that it cannot use" (fun () ->
          let is_invalid = function Invalid_argument _ -> true | _ -> false in
          let refused msg cfg =
            raises_match ~msg is_invalid (fun () ->
                ignore
                  ( S.https cfg
                    : host:string -> Flow.t -> (Flow.t, string) result ) )
          in
          let epoch = epoch () in
          seed () ;
          let authenticator =
            X509.Authenticator.chain_of_trust ~time:posix_now []
          in
          let client f = Result.get_ok (f ()) in
          refused "a peer name" (snd (setup ())) ;
          refused "an IP address"
            (client (Tls.Config.client ~authenticator ~ip:localhost_ip)) ;
          refused "a cached session"
            (client (Tls.Config.client ~authenticator ~cached_session:epoch)) ;
          refused "a cached ticket"
            (client
               (Tls.Config.client ~authenticator
                  ~cached_ticket:
                    ( { Tls.Core.identifier= "t"
                      ; obfuscation= 0l
                      ; secret= String.make 32 'k'
                      ; lifetime= 0l
                      ; early_data= 0l
                      ; issued_at= Ptime.epoch }
                    , epoch ) ) ) ;
          refused "h2"
            (client (Tls.Config.client ~authenticator ~alpn_protocols:["h2"])) ;
          refused "TLS 1.0"
            (client
               (Tls.Config.client ~authenticator ~version:(`TLS_1_0, `TLS_1_3)) ) ;
          let ok = client (Tls.Config.client ~authenticator) in
          ignore (S.https ok : host:string -> Flow.t -> (Flow.t, string) result) ;
          Mirage_crypto_rng.unset_default_generator () ;
          Fun.protect ~finally:seed @@ fun () -> refused "no generator" ok )
    ; test "the client accepts no ALPN protocol or http/1.1" (fun () ->
          (* No server of ocaml-tls selects a protocol that the client did not
             offer: the handshake here gives an epoch with the protocol. *)
          let e = epoch () in
          seed () ;
          let _, ccfg =
            Tls_support.configs ~client_peer:false ~time:posix_now ()
          in
          let show = function Ok _ -> "ok" | Error m -> m in
          equal (list string)
            ["ok"; "ok"; "the server selected the protocol h2"]
            (List.map
               (fun p ->
                 let client _ tcp = (tcp, {e with Tls.Core.alpn_protocol= p}) in
                 show
                   (S.Private.https_with ~client ccfg ~host:"localhost"
                      (connect_nothing ()) ) )
               [None; Some "http/1.1"; Some "h2"] ) ) ]

let () = exit (Windtrap.run "tls" [engine_tests; client_tests])
