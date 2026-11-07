(* The server of make interop: Caravan_http2.auto, with Server.http1.

   interop_server.exe PORT serves HTTP/2 and HTTP/1.1 with no TLS on
   127.0.0.1:PORT. auto selects the engine from the first bytes.

   interop_server.exe PORT CA serves them over TLS, with the ALPN list ["h2";
   "http/1.1"]. The program makes a test CA and a certificate for localhost and
   127.0.0.1, and writes the CA to the file CA (PEM) for curl --cacert. The
   certificate is only for the tests.

   The handler reads the request body and answers 200 with the size of the body
   and the version of the request. *)

open Caravan

let handler req =
  let body = Body.to_string (Request.body req) in
  Response.string
    (Printf.sprintf "hello, %d bytes, %s\n" (String.length body)
       (Version.to_string (Request.version req)) )

(* The TLS engine with a new test certificate. The CA goes to the file [ca]. *)
let tls ca inner =
  let ca_cert, leaf, key =
    Tls_support.pki ~names:["localhost"]
      ~ips:[Ipaddr.of_string_exn "127.0.0.1"]
      ()
  in
  Out_channel.with_open_bin ca (fun oc ->
      output_string oc (X509.Certificate.encode_pem ca_cert) ) ;
  let config =
    match
      Tls.Config.server
        ~certificates:(`Single ([leaf], key))
        ~alpn_protocols:["h2"; "http/1.1"] ()
    with
    | Ok c ->
        c
    | Error (`Msg m) ->
        failwith m
  in
  Caravan_tls.engine config inner

let () =
  Mirage_crypto_rng_unix.use_default () ;
  let port = int_of_string Sys.argv.(1) in
  let auto = Caravan_http2.auto () in
  let engine =
    if Array.length Sys.argv > 2 then tls Sys.argv.(2) auto else auto
  in
  let config = {Config.default with addr= Weft.Net.Tcp ("127.0.0.1", port)} in
  (* make interop starts the clients after this line. *)
  let on_start _ = Printf.printf "listening on port %d\n%!" port in
  Server.run ~config ~on_start ~engine (module Weft_linux) handler
