(* The settings of the HTTP/2 engine. See settings.mli. *)

type t =
  { max_concurrent_streams: int
  ; initial_window: int
  ; header_table_size: int
  ; max_header_list_size: int option
  ; reset_budget: int option
  ; frame_tokens: int
  ; frame_refill: int
  ; control_queue: int }

let default =
  { max_concurrent_streams= 100
  ; initial_window= 65535
  ; header_table_size= 4096
  ; max_header_list_size= None
  ; reset_budget= None
  ; frame_tokens= 1000
  ; frame_refill= 100
  ; control_queue= 1000 }

let million = Limits.max_tokens

let validate s =
  let check name lo hi v =
    if v < lo || v > hi then
      invalid_arg (Printf.sprintf "Caravan_http2.Settings: %s %d" name v)
  in
  check "max_concurrent_streams" 1 0xffff_ffff s.max_concurrent_streams ;
  check "initial_window" 65535 0x7fff_ffff s.initial_window ;
  check "header_table_size" 0 0xffff_ffff s.header_table_size ;
  Option.iter
    (check "max_header_list_size" 0 0xffff_ffff)
    s.max_header_list_size ;
  Option.iter (check "reset_budget" 0 million) s.reset_budget ;
  check "frame_tokens" 1 million s.frame_tokens ;
  check "frame_refill" 0 million s.frame_refill ;
  check "control_queue" 1 million s.control_queue
