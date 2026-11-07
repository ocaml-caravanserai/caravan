(* The HTTP/2 example on https://127.0.0.1:8443, with a development certificate
   (never for production). *)

open Example_dev

(* A development certificate, as in the tls example. The CA goes to a new file
   for curl --cacert. The program removes the file at its end. *)
let dev () =
  let time = Option.get (Ptime.of_float_s (Unix.gettimeofday ())) in
  let cert = Dev_cert.make ~time ~names:["localhost"; "127.0.0.1"] in
  let file = Filename.temp_file "caravan-ca" ".pem" in
  Dev_cert.write_ca cert file ;
  at_exit (fun () -> try Sys.remove file with Sys_error _ -> ()) ;
  Printf.printf "Development CA in %s. Try:\ncurl -i --cacert %s %s\n" file file
    "https://localhost:8443/" ;
  (* h2 and http/1.1: auto selects the engine from the ALPN protocol. *)
  Dev_cert.server_config ~alpn:["h2"; "http/1.1"] cert

let () =
  (* The generator of the OS: safe on many domains. *)
  Mirage_crypto_rng_unix.use_default () ;
  let tls = dev () in
  Example_common.serve ~port:8443 ~scheme:"https"
    ~engine:(Caravan_tls.engine tls (Caravan_http2.auto ()))
    Example_http2.App.handler
