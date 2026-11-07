open Weft

type t =
  { addr: Net.addr
  ; backlog: int
  ; max_connections: int
  ; buffer_size: int
  ; max_request_line: int
  ; max_header_bytes: int
  ; max_header_count: int
  ; max_body: int option
  ; max_drain: int
  ; header_timeout: Span.t
  ; idle_timeout: Span.t
  ; read_timeout: Span.t
  ; write_timeout: Span.t
  ; linger_timeout: Span.t
  ; shutdown_grace: Span.t
  ; date_header: bool
  ; cancel_on_disconnect: bool
  ; on_error: exn -> Printexc.raw_backtrace -> unit }

(* The default [on_error]. It writes with stdlib I/O, which blocks the domain
   when the standard error is slow. The world ignores SIGPIPE, thus a write to a
   broken pipe raises [Sys_error]. [on_error] must not raise, so it ignores this
   error. *)
let write_error exn bt =
  try
    Printf.eprintf "caravan: %s\n%s%!" (Printexc.to_string exn)
      (Printexc.raw_backtrace_to_string bt)
  with Sys_error _ -> ()

let default =
  { addr= Net.Tcp ("127.0.0.1", 8080)
  ; backlog= 4096
  ; max_connections= 1024
  ; buffer_size= 4096
  ; max_request_line= 8192
  ; max_header_bytes= 16384
  ; max_header_count= 100
  ; max_body= Some (8 * 1024 * 1024)
  ; max_drain= 65536
  ; header_timeout= Span.s 10
  ; idle_timeout= Span.s 30
  ; read_timeout= Span.s 30
  ; write_timeout= Span.s 30
  ; linger_timeout= Span.s 2
  ; shutdown_grace= Span.s 10
  ; date_header= true
  ; cancel_on_disconnect= true
  ; on_error= write_error }
