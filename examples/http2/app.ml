(* HTTP/2 and HTTP/1.1 on one port, with TLS: the engine Caravan_http2.auto in
   the TLS engine of caravan.tls. The handler is the same for the two versions.

   Run dune exec examples/http2/main.exe. main makes a development certificate
   authority (CA) in memory and a certificate for localhost and 127.0.0.1, as
   the tls example does. It writes the CA to a file, and prints its path. In the
   lines that follow, $CA is that path.

   The ALPN list of the server is h2 and http/1.1. curl offers the two, and the
   server selects h2. The page shows the version and the ALPN protocol:

   curl -i --cacert $CA https://localhost:8443/

   A client of HTTP/1.1 gets HTTP/1.1 on the same port:

   curl -i --http1.1 --cacert $CA https://localhost:8443/

   A TLS client with no ALPN offer gets HTTP/1.1, because HTTP/2 over TLS needs
   ALPN. The page shows alpn: none:

   curl -i --no-alpn --cacert $CA https://localhost:8443/

   /slow answers after 1 s. HTTP/2 sends many requests at the same time on one
   connection. Thus this command prints done ten times in about 1 s:

   curl --cacert $CA --parallel 'https://localhost:8443/slow?[1-10]'

   nghttp (of nghttp2) shows each frame of HTTP/2. It does not check the
   certificate:

   nghttp -nv https://localhost:8443/ *)

open Weft
open Caravan

let info req =
  let alpn = Option.bind (Request.conn req) Conn_info.alpn in
  Response.string
    (Printf.sprintf "version: %s\nscheme: %s\nalpn: %s\n"
       (Version.to_string (Request.version req))
       (Request.scheme req)
       (Option.value ~default:"none" alpn) )

let slow _ =
  Time.sleep Mono (Span.s 1) ;
  Response.string "done\n"

let handler req =
  match Request.path_only req with
  | "/" ->
      info req
  | "/slow" ->
      slow req
  | _ ->
      Handler.not_found req
