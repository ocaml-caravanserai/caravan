(* The tests of the http2 example. Each test serves the application over TLS
   with a development certificate, with Caravan_http2.auto, in the simulator of
   weft. The requests of HTTP/1.1 use the client of caravan. caravan has no
   client of HTTP/2: the requests of HTTP/2 use the fake client of the tests of
   caravan (Fake_h2_client), which checks each frame of the server. Each test
   sets a seeded generator first, and runs on one domain: the simulator runs on
   one OS thread, thus the Fortuna generator is correct here. The test server
   listens on port 80, thus the URLs have the port 80 in place of 8443. *)

open Windtrap
open Weft
open Caravan
open Example_dev
module T = Caravan_testing
module C = Caravan_client
module Fake = Fake_h2_client
module App = Example_http2.App

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan http2 example"
       (module Mirage_crypto_rng.Fortuna) )

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

(* The time of the simulator and of the certificate: 2025-06-15. *)
let t0 = 1_750_000_000.

let time = Option.get (Ptime.of_float_s t0)

(* [run f] serves the application as main does: TLS with the ALPN list h2 and
   http/1.1, and auto. It calls [f] with the certificate. *)
let run f =
  seed () ;
  let cert = Dev_cert.make ~time ~names:["localhost"] in
  let engine =
    Caravan_tls.engine
      (Dev_cert.server_config ~alpn:["h2"; "http/1.1"] cert)
      (Caravan_http2.auto ())
  in
  T.run ~engine App.handler (fun t ->
      let sim = T.sim t in
      Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
      Weft_sim.set_dns sim "localhost" (Addresses ["127.0.0.1"]) ;
      f cert )

(* The client configuration of curl --cacert: it trusts the CA of [cert], and
   offers the ALPN protocols [alpn]. *)
let client_config ?peer alpn cert =
  ok
    (Tls.Config.client
       ~authenticator:(Trust.of_cas [Dev_cert.ca cert])
       ?peer_name:peer ~alpn_protocols:alpn () )

(* [get1 alpn cert path] is the body of a GET of [path] with the client of
   caravan: HTTP/1.1. *)
let get1 alpn cert path =
  C.run
    ~tls:(Caravan_tls.https (client_config alpn cert))
    (fun c -> Body.to_string (C.body (C.get c ("https://localhost:80" ^ path))))

(* [with_h2 alpn cert f] opens a TLS connection with the ALPN offer [alpn],
   connects the fake client of HTTP/2 on it, and gives [f] the client. *)
let with_h2 alpn cert f =
  let localhost = Domain_name.(host_exn (of_string_exn "localhost")) in
  let flow, _ =
    Caravan_tls.client
      (client_config ~peer:localhost alpn cert)
      (Tcp.connect (Net.Tcp ("127.0.0.1", 80)))
  in
  Fun.protect ~finally:(fun () -> Flow.close flow)
  @@ fun () ->
  Fiber.scope (fun s ->
      let c = Fake.connect s flow in
      let v = f c in
      Fake.close c ; Fake.finish c ; v )

let get_fields path =
  [ (":method", "GET")
  ; (":scheme", "https")
  ; (":path", path)
  ; (":authority", "localhost") ]

(* [get2 alpn cert path] is the body of a GET of [path] with HTTP/2. *)
let get2 alpn cert path =
  with_h2 alpn cert (fun c ->
      Fake.request c ~stream:1 (get_fields path) ;
      (Fake.response c ~stream:1).body )

let page version alpn =
  Printf.sprintf "version: %s\nscheme: https\nalpn: %s\n" version alpn

let () =
  exit
  @@ Windtrap.run "http2"
       [ (* curl -i --cacert $CA https://localhost:8443/ *)
         test "ALPN h2 and http/1.1: HTTP/2" (fun () ->
             run (fun cert ->
                 equal string (page "HTTP/2" "h2")
                   (get2 ["h2"; "http/1.1"] cert "/") ) )
       ; (* curl -i --http1.1 --cacert $CA https://localhost:8443/ *)
         test "ALPN http/1.1: HTTP/1.1" (fun () ->
             run (fun cert ->
                 equal string
                   (page "HTTP/1.1" "http/1.1")
                   (get1 ["http/1.1"] cert "/") ) )
       ; (* curl -i --no-alpn --cacert $CA https://localhost:8443/ *)
         test "no ALPN: HTTP/1.1, alpn none" (fun () ->
             run (fun cert ->
                 equal string (page "HTTP/1.1" "none") (get1 [] cert "/") ) )
       ; (* curl --cacert $CA --parallel 'https://localhost:8443/slow?[1-10]' *)
         test "ten requests of /slow on one connection take 1 s" (fun () ->
             run (fun cert ->
                 let streams = List.init 10 (fun i -> (2 * i) + 1) in
                 let start = Time.now Mono in
                 let bodies =
                   with_h2 ["h2"] cert (fun c ->
                       List.iter
                         (fun stream ->
                           Fake.request c ~stream (get_fields "/slow") )
                         streams ;
                       List.map
                         (fun stream -> (Fake.response c ~stream).body)
                         streams )
                 in
                 let took = Time.diff Mono (Time.now Mono) start in
                 equal (list string) (List.init 10 (fun _ -> "done\n")) bodies ;
                 equal int64 1_000_000_000L (Span.to_ns took) ) )
       ; test "another path gets 404" (fun () ->
             run (fun cert ->
                 with_h2 ["h2"] cert (fun c ->
                     Fake.request c ~stream:1 (get_fields "/other") ;
                     equal (option string) (Some "404")
                       (List.assoc_opt ":status"
                          (Fake.response c ~stream:1).head ) ) ) ) ]
