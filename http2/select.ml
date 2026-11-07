(* The selection of the engine of a connection: HTTP/2 or HTTP/1.1. *)

open Weft
open Caravan
module Deadline = Caravan.Private.Deadline

let engine ?(settings = Settings.default) () =
  Settings.validate settings ;
  H2_server.serve ~settings ?hooks:None

(* The result of a sniff. [Http2 s] and [Http1 s]: the engine, and the bytes
   that the sniff read. [Close]: the end of the input before a decision, the
   deadline, or a stop of the server. *)
type sniff = Http2 of string | Http1 of string | Close

let preface = Frame.preface

(* The size of the buffer of a sniff. One read can take a small HTTP/1.1
   request, or the preface and the frames that come after it, thus a sniff
   usually needs one read only. The engine reads the bytes again from the
   prepended string. *)
let buffer_size = 1024

(* [same buf from upto] is [true] if each byte of [buf] in [from, upto) is the
   byte of the preface at the same index. *)
let rec same buf from upto =
  from >= upto
  || (Bytes.get buf from = preface.[from] && same buf (from + 1) upto)

(* [sniff ctx flow] compares the input with the preface of HTTP/2, as the bytes
   come, for at most [header_timeout]. At the first byte that differs, the input
   is HTTP/1.1. When the 24 bytes are equal, it is HTTP/2. The reads use
   [read_now] in a choose with the deadline and the stop, thus a client that
   sends a part of the preface and then nothing keeps the connection only for
   [header_timeout]. *)
let sniff (ctx : Engine.ctx) flow =
  let buf = Bytes.create buffer_size in
  let deadline = Deadline.from_now ctx.config.header_timeout in
  (* [got] bytes of [buf] are equal to the start of the preface. *)
  let rec read got =
    match Flow.read_now flow buf got (buffer_size - got) with
    | Some 0 ->
        Close
    | Some n ->
        let got' = got + n in
        let compared = Int.min got' (String.length preface) in
        if not (same buf got compared) then Http1 (Bytes.sub_string buf 0 got')
        else if compared = String.length preface then
          Http2 (Bytes.sub_string buf 0 got')
        else read got'
    | None -> (
      match
        sync
          (choose
             [ wrap (Flow.readable flow) (fun () -> `Input)
             ; wrap (Conn_info.stopping ctx.conn) (fun () -> `Stop)
             ; wrap (Deadline.at deadline) (fun () -> `Stop) ] )
      with
      | `Input ->
          read got
      | `Stop ->
          Close )
  in
  match read 0 with
  | result ->
      result
  | exception (Flow.Closed | Flow.Timeout) ->
      Close

let auto ?settings ?(http1 = Server.http1) () =
  let http2 = engine ?settings () in
  fun (ctx : Engine.ctx) flow handler ->
    match (Conn_info.alpn ctx.conn, Conn_info.scheme ctx.conn) with
    | Some "h2", _ ->
        http2 ctx flow handler
    (* RFC 9113 section 3.3: HTTP/2 over TLS needs the ALPN protocol h2. Thus
       the TLS check of h2 applies to each TLS session of HTTP/2. *)
    | Some _, _ | None, `Https ->
        http1 ctx flow handler
    | None, `Http -> (
      match sniff ctx flow with
      | Http2 bytes ->
          http2 ctx (Flow.prepend bytes flow) handler
      | Http1 bytes ->
          http1 ctx (Flow.prepend bytes flow) handler
      | Close ->
          () )
