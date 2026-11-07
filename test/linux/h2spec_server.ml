(* The server of make h2spec: HTTP/2 with prior knowledge (h2c) on the port of
   the first argument (8443 by default), with the public engine
   Caravan_http2.engine. The handler reads the request body and answers 200 with
   a body. *)

open Caravan

let handler req =
  let body = Body.to_string (Request.body req) in
  Response.string (Printf.sprintf "hello, %d bytes\n" (String.length body))

let () =
  let port =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 8443
  in
  let config = {Config.default with addr= Weft.Net.Tcp ("127.0.0.1", port)} in
  (* make h2spec starts h2spec after this line. *)
  let on_start _ = Printf.printf "listening on port %d\n%!" port in
  Server.run ~config ~on_start ~engine:(Caravan_http2.engine ())
    (module Weft_linux)
    handler
