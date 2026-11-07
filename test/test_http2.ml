(* The tests of caravan.http2: the error codes, the frames, the connection state
   and the messages. *)

open Windtrap
open Weft
module Error = Caravan_http2.Private.Error
module Frame = Caravan_http2.Private.Frame
module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer
module Flow = Caravan.Flow
open H2_support

let code = Testable.structural ~pp:Error.pp_code

let error_tests =
  group "Error"
    [ test "each code of RFC 9113 has its value, and back" (fun () ->
          let names =
            List.init 14 (fun n ->
                let c = Error.code_of_int n in
                equal Testable.int n (Error.code_to_int c) ;
                Format.asprintf "%a" Error.pp_code c )
          in
          equal
            Testable.(list string)
            [ "NO_ERROR"
            ; "PROTOCOL_ERROR"
            ; "INTERNAL_ERROR"
            ; "FLOW_CONTROL_ERROR"
            ; "SETTINGS_TIMEOUT"
            ; "STREAM_CLOSED"
            ; "FRAME_SIZE_ERROR"
            ; "REFUSED_STREAM"
            ; "CANCEL"
            ; "COMPRESSION_ERROR"
            ; "CONNECT_ERROR"
            ; "ENHANCE_YOUR_CALM"
            ; "INADEQUATE_SECURITY"
            ; "HTTP_1_1_REQUIRED" ]
            names )
    ; test "an unknown code keeps its value" (fun () ->
          List.iter
            (fun n ->
              let c = Error.code_of_int n in
              equal code (Error.Unknown_code n) c ;
              equal Testable.int n (Error.code_to_int c) )
            [0xe; 0xff; 0xffff_ffff] ;
          equal Testable.string "0xff"
            (Format.asprintf "%a" Error.pp_code (Error.code_of_int 0xff)) )
    ; test "a value out of 32 bits is refused" (fun () ->
          raises (Invalid_argument "Error.code_of_int: -1") (fun () ->
              Error.code_of_int (-1) ) ;
          raises (Invalid_argument "Error.code_of_int: 4294967296") (fun () ->
              Error.code_of_int 0x1_0000_0000 ) ) ]

(* {1 Frames} *)

let sprintf = Printf.sprintf

let of_hex h =
  let h = String.concat "" (String.split_on_char ' ' h) in
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

(* [writes ~max_frame f] is the bytes that [f w] writes to a new writer [w]. *)
let writes ?max_frame f =
  let out = Buffer.create 64 in
  let w = Writer.create ?max_frame (Flow.of_string "" out) in
  f w ; Writer.flush w ; Buffer.contents out

let encode f = writes (fun w -> Frame.write w f)

(* A reader of [s]. A read of the flow gives at most [chunk] bytes. *)
let reader ?size ?chunk s =
  Reader.create ?size (Flow.of_string ?chunk s (Buffer.create 0))

(* [at_end r] is [true] if [r] holds no byte and its stream ended. *)
let at_end r =
  Reader.buffered r = 0
  && match Reader.ensure r 1 with () -> false | exception End_of_file -> true

(* What [Frame.read] makes of [input]: the frame, and the bytes after it if
   there are some, or the error and the number of bytes that it consumed. *)
let outcome ?(max_size = 16384) input =
  let r = reader input in
  match Frame.read r ~max_size with
  | f ->
      if at_end r then show f
      else sprintf "%s, then %d bytes" (show f) (Reader.buffered r)
  | exception Frame.Connection_error {code; reason} ->
      sprintf "connection error %s: %s (consumed %d)" (show_code code) reason
        (Reader.consumed r)
  | exception End_of_file ->
      "end of input"

(* The outcomes of [cases], one line for each: the name, then the outcome. *)
let outcomes ?max_size cases =
  String.concat "\n"
    (List.map
       (fun (name, input) -> name ^ ": " ^ outcome ?max_size input)
       cases )

(* The generators of frames that [read] gives back from [write]: no
   Push_promise, Unknown or Malformed, and a [flow_length] that is the length of
   the data. *)
let frame_gen =
  let open Gen in
  let stream = int_range 1 0x7fff_ffff in
  let any_stream = int_range 0 0x7fff_ffff in
  let uint32 = int_range 0 0xffff_ffff in
  let code = map Error.code_of_int (one_of [int_range 0 15; uint32]) in
  let payload = string_of ~size:(int_range 0 40) char in
  let priority =
    let+ exclusive = bool
    and+ dependency = any_stream
    and+ weight = int_range 0 255 in
    {Frame.exclusive; dependency; weight}
  in
  with_pp
    (fun ppf f -> Format.pp_print_string ppf (show f))
    (one_of
       [ (let+ stream = stream and+ end_stream = bool and+ data = payload in
          Frame.Data {stream; end_stream; data; flow_length= String.length data}
         )
       ; (let+ stream = stream
          and+ end_stream = bool
          and+ end_headers = bool
          and+ priority = option priority
          and+ block = payload in
          Frame.Headers {stream; end_stream; end_headers; priority; block} )
       ; (let+ stream = stream and+ priority = priority in
          (* A PRIORITY frame that depends on its own stream is Malformed. *)
          let priority =
            if priority.dependency = stream then {priority with dependency= 0}
            else priority
          in
          Frame.Priority {stream; priority} )
       ; (let+ stream = stream and+ code = code in
          Frame.Rst_stream {stream; code} )
       ; (let+ ack = bool
          and+ settings =
            list ~size:(int_range 0 5)
              (pair (of_list [1; 2; 3; 4; 5; 6; 8]) uint32)
          in
          Frame.Settings {ack; settings= (if ack then [] else settings)} )
       ; (let+ ack = bool and+ data = string_of ~size:(constant 8) char in
          Frame.Ping {ack; data} )
       ; (let+ last_stream = any_stream and+ code = code and+ debug = payload in
          Frame.Goaway {last_stream; code; debug} )
       ; (let+ stream = any_stream and+ increment = int_range 0 0x7fff_ffff in
          Frame.Window_update {stream; increment} )
       ; (let+ stream = stream and+ end_headers = bool and+ block = payload in
          Frame.Continuation {stream; end_headers; block} ) ] )

(* A reader that waits for [gate] when it gave all the bytes of [pending]. *)
let gated_flow pending gate =
  let rec read buf off len =
    match (!pending, Ivar.peek gate) with
    | "", Some () ->
        0
    | "", None ->
        sync (Ivar.read gate) ;
        read buf off len
    | s, _ ->
        let n = Int.min len (String.length s) in
        Bytes.blit_string s 0 buf off n ;
        pending := String.sub s n (String.length s - n) ;
        n
  in
  Flow.make ~read
    ~read_now:(fun _ _ _ -> None)
    ~write:(fun _ _ _ -> ())
    ~readable:(Ivar.read gate) ~shutdown_send:ignore ~close:ignore
    ~set_read_deadline:ignore ~set_write_deadline:ignore ()

let frame_tests =
  group "Frame"
    [ prop "read inverts write" frame_gen (fun f ->
          let r = reader (encode f) in
          equal frame f (Frame.read r ~max_size:16384) ;
          is_true ~msg:"the frame is consumed" (at_end r) )
    ; prop "frames split in parts of 1 to 20 bytes read the same"
        Gen.(
          triple
            (list ~size:(int_range 1 5) frame_gen)
            (int_range 1 20) (int_range 1 32) )
        (fun (fs, chunk, size) ->
          let r = reader ~size ~chunk (String.concat "" (List.map encode fs)) in
          equal (list frame) fs
            (List.map (fun _ -> Frame.read r ~max_size:16384) fs) ;
          is_true ~msg:"all frames are consumed" (at_end r) )
    ; (* The rules of read, one test for each row. *)
      test "a frame longer than max_size" (fun () ->
          expect
            (outcomes ~max_size:5
               [ ("5 bytes", raw ~kind:0 ~stream:1 "abcde")
               ; ("6 bytes", raw ~kind:0 ~stream:1 "abcdef")
               ; ("unknown type", raw ~kind:0xff ~stream:1 "abcdef")
               ; ("no payload yet", raw ~length:6 ~kind:0 ~stream:1 "") ] )
          @@ __POS_OF__
               {|
               5 bytes: DATA 1 "abcde" flow_length=5
               6 bytes: connection error FRAME_SIZE_ERROR: a frame of 6 bytes, more than 5 (consumed 0)
               unknown type: connection error FRAME_SIZE_ERROR: a frame of 6 bytes, more than 5 (consumed 0)
               no payload yet: connection error FRAME_SIZE_ERROR: a frame of 6 bytes, more than 5 (consumed 0)
               |} )
    ; test "a stream frame on stream 0" (fun () ->
          let p = "\x00\x00\x00\x01\x00" in
          expect
            (outcomes
               [ ("DATA", raw ~kind:0 ~stream:0 "")
               ; ("HEADERS", raw ~kind:1 ~stream:0 "")
               ; ("PRIORITY", raw ~kind:2 ~stream:0 p)
               ; ("RST_STREAM", raw ~kind:3 ~stream:0 "\x00\x00\x00\x08")
               ; ("PUSH_PROMISE", raw ~kind:5 ~stream:0 "\x00\x00\x00\x02")
               ; ("CONTINUATION", raw ~kind:9 ~stream:0 "")
               ; ("reserved bit only", raw ~kind:0 ~stream:0x8000_0000 "")
               ; ("reserved bit and 1", raw ~kind:0 ~stream:0x8000_0001 "")
               ; ("PUSH_PROMISE 2", raw ~kind:5 ~stream:2 "\x00\x00\x00\x04")
               ; ("WINDOW_UPDATE", raw ~kind:8 ~stream:0 "\x00\x00\x00\x01")
               ; ("RST_STREAM, header only", raw ~length:4 ~kind:3 ~stream:0 "")
               ] )
          @@ __POS_OF__
               {|
               DATA: connection error PROTOCOL_ERROR: a DATA frame on stream 0 (consumed 0)
               HEADERS: connection error PROTOCOL_ERROR: a HEADERS frame on stream 0 (consumed 0)
               PRIORITY: connection error PROTOCOL_ERROR: a PRIORITY frame on stream 0 (consumed 0)
               RST_STREAM: connection error PROTOCOL_ERROR: a RST_STREAM frame on stream 0 (consumed 0)
               PUSH_PROMISE: connection error PROTOCOL_ERROR: a PUSH_PROMISE frame on stream 0 (consumed 0)
               CONTINUATION: connection error PROTOCOL_ERROR: a CONTINUATION frame on stream 0 (consumed 0)
               reserved bit only: connection error PROTOCOL_ERROR: a DATA frame on stream 0 (consumed 0)
               reserved bit and 1: DATA 1 "" flow_length=0
               PUSH_PROMISE 2: PUSH_PROMISE 2
               WINDOW_UPDATE: WINDOW_UPDATE 0 +1
               RST_STREAM, header only: connection error PROTOCOL_ERROR: a RST_STREAM frame on stream 0 (consumed 0)
               |} )
    ; test "a connection frame on another stream" (fun () ->
          expect
            (outcomes
               [ ("SETTINGS", raw ~kind:4 ~stream:1 "")
               ; ("PING", raw ~kind:6 ~stream:3 "01234567")
               ; ( "GOAWAY"
                 , raw ~kind:7 ~stream:0x7fff_ffff (String.make 8 '\x00') )
               ; ("SETTINGS, reserved bit", raw ~kind:4 ~stream:0x8000_0000 "")
               ; ("PING, header only", raw ~length:8 ~kind:6 ~stream:3 "") ] )
          @@ __POS_OF__
               {|
               SETTINGS: connection error PROTOCOL_ERROR: a SETTINGS frame on stream 1 (consumed 0)
               PING: connection error PROTOCOL_ERROR: a PING frame on stream 3 (consumed 0)
               GOAWAY: connection error PROTOCOL_ERROR: a GOAWAY frame on stream 2147483647 (consumed 0)
               SETTINGS, reserved bit: SETTINGS []
               PING, header only: connection error PROTOCOL_ERROR: a PING frame on stream 3 (consumed 0)
               |} )
    ; test "a PADDED frame with no Pad Length byte" (fun () ->
          expect
            (outcomes
               [ ("DATA", raw ~kind:0 ~flags:0x8 ~stream:1 "")
               ; ("HEADERS", raw ~kind:1 ~flags:0xc ~stream:1 "")
               ; ("DATA, 1 byte", raw ~kind:0 ~flags:0x8 ~stream:1 "\x00")
               ; ("HEADERS, 1 byte", raw ~kind:1 ~flags:0xc ~stream:1 "\x00") ] )
          @@ __POS_OF__
               {|
               DATA: connection error FRAME_SIZE_ERROR: a PADDED frame with no Pad Length byte (consumed 0)
               HEADERS: connection error FRAME_SIZE_ERROR: a PADDED frame with no Pad Length byte (consumed 0)
               DATA, 1 byte: DATA 1 "" flow_length=1
               HEADERS, 1 byte: HEADERS 1 END_HEADERS ""
               |} )
    ; test "a padding longer than the rest of the payload" (fun () ->
          let prio = "\x80\x00\x00\x03\x0f" in
          expect
            (outcomes
               [ ("DATA", raw ~kind:0 ~flags:0x8 ~stream:1 "\x03ab")
               ; ("HEADERS", raw ~kind:1 ~flags:0xc ~stream:1 "\x03ab")
               ; ( "HEADERS with PRIORITY"
                 , raw ~kind:1 ~flags:0x2c ~stream:1 ("\x03" ^ prio ^ "ab") )
               ; ( "HEADERS with PRIORITY, pad 2"
                 , raw ~kind:1 ~flags:0x2c ~stream:1 ("\x02" ^ prio ^ "ab") )
               ; ( "HEADERS with PRIORITY, pad 1"
                 , raw ~kind:1 ~flags:0x2c ~stream:1 ("\x01" ^ prio ^ "ab") ) ] )
          @@ __POS_OF__
               {|
               DATA: connection error PROTOCOL_ERROR: a padding of 3 bytes in a payload of 2 bytes (consumed 0)
               HEADERS: connection error PROTOCOL_ERROR: a padding of 3 bytes in a payload of 2 bytes (consumed 0)
               HEADERS with PRIORITY: connection error PROTOCOL_ERROR: a padding of 3 bytes in a payload of 2 bytes (consumed 0)
               HEADERS with PRIORITY, pad 2: HEADERS 1 END_HEADERS priority=exclusive:3/15 ""
               HEADERS with PRIORITY, pad 1: HEADERS 1 END_HEADERS priority=exclusive:3/15 "a"
               |} )
    ; test "a HEADERS frame with PRIORITY and less than 5 bytes" (fun () ->
          expect
            (outcomes
               [ ( "4 bytes"
                 , raw ~kind:1 ~flags:0x20 ~stream:1 "\x00\x00\x00\x03" )
               ; ( "PADDED, 4 bytes after the Pad Length byte"
                 , raw ~kind:1 ~flags:0x28 ~stream:1 "\x00\x00\x00\x00\x03" )
               ; ( "5 bytes"
                 , raw ~kind:1 ~flags:0x20 ~stream:1 "\x00\x00\x00\x03\x00" )
               ; ( "PADDED, 5 bytes after the Pad Length byte"
                 , raw ~kind:1 ~flags:0x28 ~stream:1 "\x00\x00\x00\x00\x03\x00"
                 ) ] )
          @@ __POS_OF__
               {|
               4 bytes: connection error FRAME_SIZE_ERROR: a HEADERS frame with PRIORITY and less than 5 bytes (consumed 0)
               PADDED, 4 bytes after the Pad Length byte: connection error FRAME_SIZE_ERROR: a HEADERS frame with PRIORITY and less than 5 bytes (consumed 0)
               5 bytes: HEADERS 1 priority=3/0 ""
               PADDED, 5 bytes after the Pad Length byte: HEADERS 1 priority=3/0 ""
               |} )
    ; test "a PRIORITY frame with a length other than 5" (fun () ->
          expect
            (outcomes
               [ ("4 bytes", raw ~kind:2 ~stream:1 "\x00\x00\x00\x03")
               ; ("6 bytes", raw ~kind:2 ~stream:1 "\x00\x00\x00\x03\x00\x00")
               ; ("0 bytes", raw ~kind:2 ~stream:1 "")
               ; ("5 bytes", raw ~kind:2 ~stream:1 "\x00\x00\x00\x03\x07") ] )
          @@ __POS_OF__
               {|
               4 bytes: MALFORMED 1 FRAME_SIZE_ERROR: a PRIORITY frame with 4 bytes, not 5
               6 bytes: MALFORMED 1 FRAME_SIZE_ERROR: a PRIORITY frame with 6 bytes, not 5
               0 bytes: MALFORMED 1 FRAME_SIZE_ERROR: a PRIORITY frame with 0 bytes, not 5
               5 bytes: PRIORITY 1 priority=3/7
               |} )
    ; test "a PRIORITY frame that depends on its own stream" (fun () ->
          expect
            (outcomes
               [ ("stream 3", raw ~kind:2 ~stream:3 "\x00\x00\x00\x03\x07")
               ; ("exclusive", raw ~kind:2 ~stream:3 "\x80\x00\x00\x03\x07")
               ; ("stream 3 on 1", raw ~kind:2 ~stream:1 "\x80\x00\x00\x03\x07")
               ] )
          @@ __POS_OF__
               {|
               stream 3: MALFORMED 3 PROTOCOL_ERROR: a PRIORITY frame that depends on its own stream
               exclusive: MALFORMED 3 PROTOCOL_ERROR: a PRIORITY frame that depends on its own stream
               stream 3 on 1: PRIORITY 1 priority=exclusive:3/7
               |} )
    ; test "RST_STREAM, WINDOW_UPDATE and PING with an incorrect length"
        (fun () ->
          expect
            (outcomes
               [ ("RST_STREAM 3", raw ~kind:3 ~stream:1 "\x00\x00\x08")
               ; ("RST_STREAM 5", raw ~kind:3 ~stream:1 "\x00\x00\x00\x08\x00")
               ; ("WINDOW_UPDATE 3", raw ~kind:8 ~stream:1 "\x00\x00\x01")
               ; ( "WINDOW_UPDATE 5"
                 , raw ~kind:8 ~stream:0 "\x00\x00\x00\x01\x00" )
               ; ("PING 7", raw ~kind:6 ~stream:0 "0123456")
               ; ("PING 9", raw ~kind:6 ~stream:0 "012345678")
               ; ("RST_STREAM 4", raw ~kind:3 ~stream:1 "\x00\x00\x00\x08")
               ; ("WINDOW_UPDATE 4", raw ~kind:8 ~stream:1 "\xff\xff\xff\xff")
               ; ("PING 8", raw ~kind:6 ~flags:0xff ~stream:0 "01234567")
               ; ( "RST_STREAM 5, header only"
                 , raw ~length:5 ~kind:3 ~stream:1 "" )
               ; ( "WINDOW_UPDATE 3, header only"
                 , raw ~length:3 ~kind:8 ~stream:1 "" )
               ; ("PING 9, header only", raw ~length:9 ~kind:6 ~stream:0 "") ] )
          @@ __POS_OF__
               {|
               RST_STREAM 3: connection error FRAME_SIZE_ERROR: a RST_STREAM frame with 3 bytes, not 4 (consumed 0)
               RST_STREAM 5: connection error FRAME_SIZE_ERROR: a RST_STREAM frame with 5 bytes, not 4 (consumed 0)
               WINDOW_UPDATE 3: connection error FRAME_SIZE_ERROR: a WINDOW_UPDATE frame with 3 bytes, not 4 (consumed 0)
               WINDOW_UPDATE 5: connection error FRAME_SIZE_ERROR: a WINDOW_UPDATE frame with 5 bytes, not 4 (consumed 0)
               PING 7: connection error FRAME_SIZE_ERROR: a PING frame with 7 bytes, not 8 (consumed 0)
               PING 9: connection error FRAME_SIZE_ERROR: a PING frame with 9 bytes, not 8 (consumed 0)
               RST_STREAM 4: RST_STREAM 1 CANCEL
               WINDOW_UPDATE 4: WINDOW_UPDATE 1 +2147483647
               PING 8: PING ACK "01234567"
               RST_STREAM 5, header only: connection error FRAME_SIZE_ERROR: a RST_STREAM frame with 5 bytes, not 4 (consumed 0)
               WINDOW_UPDATE 3, header only: connection error FRAME_SIZE_ERROR: a WINDOW_UPDATE frame with 3 bytes, not 4 (consumed 0)
               PING 9, header only: connection error FRAME_SIZE_ERROR: a PING frame with 9 bytes, not 8 (consumed 0)
               |} )
    ; test "a SETTINGS frame with an incorrect length" (fun () ->
          let one = "\x00\x03\x00\x00\x00\x64" in
          expect
            (outcomes
               [ ("ACK and 6 bytes", raw ~kind:4 ~flags:0x1 ~stream:0 one)
               ; ("5 bytes", raw ~kind:4 ~stream:0 (String.sub one 0 5))
               ; ("7 bytes", raw ~kind:4 ~stream:0 (one ^ "\x00"))
               ; ("ACK", raw ~kind:4 ~flags:0x1 ~stream:0 "")
               ; ("12 bytes", raw ~kind:4 ~stream:0 (one ^ one))
               ; ( "ACK and 6 bytes, header only"
                 , raw ~length:6 ~kind:4 ~flags:0x1 ~stream:0 "" )
               ; ("7 bytes, header only", raw ~length:7 ~kind:4 ~stream:0 "") ] )
          @@ __POS_OF__
               {|
               ACK and 6 bytes: connection error FRAME_SIZE_ERROR: a SETTINGS frame with ACK and a payload (consumed 0)
               5 bytes: connection error FRAME_SIZE_ERROR: a SETTINGS frame with 5 bytes, not a multiple of 6 (consumed 0)
               7 bytes: connection error FRAME_SIZE_ERROR: a SETTINGS frame with 7 bytes, not a multiple of 6 (consumed 0)
               ACK: SETTINGS ACK []
               12 bytes: SETTINGS [0x3=100; 0x3=100]
               ACK and 6 bytes, header only: connection error FRAME_SIZE_ERROR: a SETTINGS frame with ACK and a payload (consumed 0)
               7 bytes, header only: connection error FRAME_SIZE_ERROR: a SETTINGS frame with 7 bytes, not a multiple of 6 (consumed 0)
               |} )
    ; test "a SETTINGS frame keeps only the known identifiers" (fun () ->
          (* The identifiers 0, 1, 6, 7, 8, 9 and 0xffff, each with the value of
             its identifier. *)
          let setting id =
            let b = Bytes.create 6 in
            Bytes.set_uint16_be b 0 id ;
            Bytes.set_int32_be b 2 (Int32.of_int id) ;
            Bytes.to_string b
          in
          expect
            (outcome
               (raw ~kind:4 ~stream:0
                  (String.concat ""
                     (List.map setting [0; 1; 6; 7; 8; 9; 0xffff]) ) ) )
          @@ __POS_OF__ {|SETTINGS [0x1=1; 0x6=6; 0x8=8]|} )
    ; test "a GOAWAY frame with less than 8 bytes" (fun () ->
          expect
            (outcomes
               [ ("7 bytes", raw ~kind:7 ~stream:0 (String.make 7 '\x00'))
               ; ("0 bytes", raw ~kind:7 ~stream:0 "")
               ; ( "8 bytes"
                 , raw ~kind:7 ~stream:0 "\x80\x00\x00\x07\x00\x00\x00\x02" )
               ; ("7 bytes, header only", raw ~length:7 ~kind:7 ~stream:0 "") ] )
          @@ __POS_OF__
               {|
               7 bytes: connection error FRAME_SIZE_ERROR: a GOAWAY frame with 7 bytes, less than 8 (consumed 0)
               0 bytes: connection error FRAME_SIZE_ERROR: a GOAWAY frame with 0 bytes, less than 8 (consumed 0)
               8 bytes: GOAWAY 7 INTERNAL_ERROR ""
               7 bytes, header only: connection error FRAME_SIZE_ERROR: a GOAWAY frame with 7 bytes, less than 8 (consumed 0)
               |} )
    ; test "a frame of an unknown type is consumed" (fun () ->
          let ping = raw ~kind:6 ~stream:0 "01234567" in
          let r =
            reader (raw ~kind:0xa ~flags:0xff ~stream:5 "payload" ^ ping)
          in
          equal frame
            (Unknown {kind= 0xa; stream= 5})
            (Frame.read r ~max_size:16384) ;
          equal int 16 (Reader.consumed r) ;
          equal frame
            (Ping {ack= false; data= "01234567"})
            (Frame.read r ~max_size:16384) ;
          expect (outcome (raw ~kind:0xff ~stream:0 ""))
          @@ __POS_OF__ {|UNKNOWN 0xff 0|} )
    ; test "unknown flags are ignored" (fun () ->
          (* Each flag that the type does not define is set. *)
          expect
            (outcomes
               [ ("DATA", raw ~kind:0 ~flags:0xf6 ~stream:1 "ab")
               ; ("HEADERS", raw ~kind:1 ~flags:0xd2 ~stream:1 "ab")
               ; ( "PRIORITY"
                 , raw ~kind:2 ~flags:0xff ~stream:1 "\x00\x00\x00\x03\x07" )
               ; ( "RST_STREAM"
                 , raw ~kind:3 ~flags:0xff ~stream:1 "\x00\x00\x00\x00" )
               ; ("SETTINGS", raw ~kind:4 ~flags:0xfe ~stream:0 "")
               ; ("PING", raw ~kind:6 ~flags:0xfe ~stream:0 "01234567")
               ; ( "GOAWAY"
                 , raw ~kind:7 ~flags:0xff ~stream:0 (String.make 8 '\x00') )
               ; ( "WINDOW_UPDATE"
                 , raw ~kind:8 ~flags:0xff ~stream:1 "\x00\x00\x00\x01" )
               ; ("CONTINUATION", raw ~kind:9 ~flags:0xfb ~stream:1 "ab") ] )
          @@ __POS_OF__
               {|
               DATA: DATA 1 "ab" flow_length=2
               HEADERS: HEADERS 1 "ab"
               PRIORITY: PRIORITY 1 priority=3/7
               RST_STREAM: RST_STREAM 1 NO_ERROR
               SETTINGS: SETTINGS []
               PING: PING "01234567"
               GOAWAY: GOAWAY 0 NO_ERROR ""
               WINDOW_UPDATE: WINDOW_UPDATE 1 +1
               CONTINUATION: CONTINUATION 1 "ab"
               |} )
    ; test "an input that ends in a frame" (fun () ->
          let input = raw ~kind:0 ~stream:1 "abc" in
          equal (list string)
            (List.init (String.length input) (fun _ -> "end of input"))
            (List.init (String.length input) (fun n ->
                 outcome (String.sub input 0 n) ) ) )
    ; test "padding boundaries" (fun () ->
          expect
            (outcomes
               [ ("pad = rest", raw ~kind:0 ~flags:0x9 ~stream:1 "\x03abc")
               ; ("pad = rest + 1", raw ~kind:0 ~flags:0x9 ~stream:1 "\x04abc")
               ; ("pad = rest - 1", raw ~kind:0 ~flags:0x9 ~stream:1 "\x02abc")
               ; ("pad 0", raw ~kind:0 ~flags:0x8 ~stream:1 "\x00abc")
               ; ( "pad 255"
                 , raw ~kind:0 ~flags:0x8 ~stream:1
                     ("\xffxy" ^ String.make 255 '\x00') )
               ; ( "HEADERS pad = rest"
                 , raw ~kind:1 ~flags:0xc ~stream:1 "\x03abc" )
               ; ( "HEADERS pad = rest + 1"
                 , raw ~kind:1 ~flags:0xc ~stream:1 "\x04abc" ) ] )
          @@ __POS_OF__
               {|
               pad = rest: DATA 1 END_STREAM "" flow_length=4
               pad = rest + 1: connection error PROTOCOL_ERROR: a padding of 4 bytes in a payload of 3 bytes (consumed 0)
               pad = rest - 1: DATA 1 END_STREAM "a" flow_length=4
               pad 0: DATA 1 "abc" flow_length=4
               pad 255: DATA 1 "xy" flow_length=258
               HEADERS pad = rest: HEADERS 1 END_HEADERS ""
               HEADERS pad = rest + 1: connection error PROTOCOL_ERROR: a padding of 4 bytes in a payload of 3 bytes (consumed 0)
               |} )
    ; test "a cancel of read in the middle of a frame consumes nothing"
        (fun () ->
          let f =
            Frame.Data
              {stream= 1; end_stream= true; data= "hello world"; flow_length= 11}
          in
          let input = encode f in
          (* 5 bytes stop in the header, 12 bytes stop in the payload. *)
          List.iter
            (fun first ->
              Sim_support.run
              @@ fun sim ->
              let pending = ref (String.sub input 0 first)
              and gate = Ivar.create () in
              let r = Reader.create (gated_flow pending gate) in
              Fiber.scope
              @@ fun s ->
              let c =
                Sim_support.call s (fun () -> Frame.read r ~max_size:16384)
              in
              is_false ~msg:"read waits" (Sim_support.ended sim c) ;
              equal int first (Reader.buffered r) ;
              Sim_support.cancel c ;
              raises Promise.Cancelled (fun () -> ignore (Sim_support.await c)) ;
              equal int 0 (Reader.consumed r) ;
              equal int first (Reader.buffered r) ;
              pending := String.sub input first (String.length input - first) ;
              Ivar.fill gate () ;
              equal frame f (Frame.read r ~max_size:16384) ;
              is_true (at_end r) )
            [5; 12] )
    ; test "a Malformed frame is consumed" (fun () ->
          let r =
            reader
              ( raw ~kind:2 ~stream:1 "\x00\x00\x00\x03"
              ^ raw ~kind:6 ~stream:0 "01234567" )
          in
          equal frame
            (Malformed
               { stream= 1
               ; code= Frame_size_error
               ; reason= "a PRIORITY frame with 4 bytes, not 5" } )
            (Frame.read r ~max_size:16384) ;
          equal frame
            (Ping {ack= false; data= "01234567"})
            (Frame.read r ~max_size:16384) ;
          is_true (at_end r) )
    ; test "write_header_block splits a block in frames of max_size bytes"
        (fun () ->
          (* The length, the type, the flags and the stream of each frame, and
             the payloads joined. *)
          let split ~end_stream ~max_size block =
            let s =
              writes (fun w ->
                  Frame.write_header_block w ~stream:7 ~end_stream ~max_size
                    block )
            in
            let rec walk off =
              if off = String.length s then []
              else
                let length =
                  (Char.code s.[off] lsl 16) lor String.get_uint16_be s (off + 1)
                in
                ( sprintf "%d 0x%x 0x%x %ld" length
                    (Char.code s.[off + 3])
                    (Char.code s.[off + 4])
                    (String.get_int32_be s (off + 5))
                , String.sub s (off + 9) length )
                :: walk (off + 9 + length)
            in
            let frames = walk 0 in
            equal string block (String.concat "" (List.map snd frames)) ;
            String.concat "; " (List.map fst frames)
          in
          let block = String.init 40000 (fun i -> Char.chr (i land 255)) in
          expect
            (String.concat "\n"
               [ split ~end_stream:true ~max_size:16384 block
               ; split ~end_stream:false ~max_size:16384 block
               ; split ~end_stream:true ~max_size:16384
                   (String.sub block 0 32768)
               ; split ~end_stream:true ~max_size:16384
                   (String.sub block 0 16384)
               ; split ~end_stream:true ~max_size:16384
                   (String.sub block 0 16385)
               ; split ~end_stream:false ~max_size:16384 "abc"
               ; split ~end_stream:true ~max_size:1 ""
               ; split ~end_stream:false ~max_size:1 "ab" ] )
          @@ __POS_OF__
               {|
               16384 0x1 0x1 7; 16384 0x9 0x0 7; 7232 0x9 0x4 7
               16384 0x1 0x0 7; 16384 0x9 0x0 7; 7232 0x9 0x4 7
               16384 0x1 0x1 7; 16384 0x9 0x4 7
               16384 0x1 0x5 7
               16384 0x1 0x1 7; 1 0x9 0x4 7
               3 0x1 0x4 7
               0 0x1 0x5 7
               1 0x1 0x0 7; 1 0x9 0x4 7
               |} )
    ; test "frames built by hand" (fun () ->
          let settings =
            ( Frame.Settings {ack= false; settings= [(0x1, 4096); (0x3, 100)]}
            , [ "00000c" (* length 12: 2 settings of 6 bytes *)
              ; "04" (* type SETTINGS *)
              ; "00" (* no flags *)
              ; "00000000" (* stream 0 *)
              ; "0001" (* SETTINGS_HEADER_TABLE_SIZE *)
              ; "00001000" (* 4096 *)
              ; "0003" (* SETTINGS_MAX_CONCURRENT_STREAMS *)
              ; "00000064" (* 100 *) ] )
          in
          let ping =
            ( Frame.Ping {ack= true; data= "01234567"}
            , [ "000008" (* length 8 *)
              ; "06" (* type PING *)
              ; "01" (* ACK *)
              ; "00000000" (* stream 0 *)
              ; "3031323334353637" (* "01234567" in ASCII *) ] )
          in
          let goaway =
            ( Frame.Goaway {last_stream= 5; code= Protocol_error; debug= "bye"}
            , [ "00000b" (* length 11: 4 + 4 + 3 *)
              ; "07" (* type GOAWAY *)
              ; "00" (* no flags *)
              ; "00000000" (* stream 0 *)
              ; "00000005" (* last stream 5 *)
              ; "00000001" (* PROTOCOL_ERROR *)
              ; "627965" (* "bye" in ASCII *) ] )
          in
          let rst =
            ( Frame.Rst_stream
                {stream= 0x7fff_ffff; code= Unknown_code 0xffff_ffff}
            , [ "000004" (* length 4 *)
              ; "03" (* type RST_STREAM *)
              ; "00" (* no flags *)
              ; "7fffffff" (* stream 2^31 - 1 *)
              ; "ffffffff" (* an unknown code *) ] )
          in
          let headers =
            ( Frame.Headers
                { stream= 3
                ; end_stream= false
                ; end_headers= true
                ; priority= Some {exclusive= true; dependency= 1; weight= 255}
                ; block= "x" }
            , [ "000006" (* length 6: 5 bytes of priority, 1 byte of block *)
              ; "01" (* type HEADERS *)
              ; "24" (* END_HEADERS (0x4) and PRIORITY (0x20) *)
              ; "00000003" (* stream 3 *)
              ; "80000001" (* the E flag and the stream 1 *)
              ; "ff" (* the weight field 255 *)
              ; "78" (* the block "x" *) ] )
          in
          let priority =
            ( Frame.Priority
                { stream= 5
                ; priority= {exclusive= false; dependency= 3; weight= 16} }
            , [ "000005" (* length 5 *)
              ; "02" (* type PRIORITY *)
              ; "00" (* no flags *)
              ; "00000005" (* stream 5 *)
              ; "00000003" (* no E flag, the stream 3 *)
              ; "10" (* the weight field 16 *) ] )
          in
          let window =
            ( Frame.Window_update {stream= 0; increment= 0x7fff_ffff}
            , [ "000004" (* length 4 *)
              ; "08" (* type WINDOW_UPDATE *)
              ; "00" (* no flags *)
              ; "00000000" (* stream 0 *)
              ; "7fffffff" (* the increment 2^31 - 1 *) ] )
          in
          List.iter
            (fun (f, fields) ->
              let bytes = of_hex (String.concat "" fields) in
              equal string bytes (encode f) ;
              equal frame f (Frame.read (reader bytes) ~max_size:16384) )
            [settings; ping; goaway; rst; headers; priority; window] ;
          (* A DATA frame of 70000 bytes (0x011170) on stream 3, with
             END_STREAM. *)
          let data = String.make 70000 'd' in
          let f =
            Frame.Data {stream= 3; end_stream= true; data; flow_length= 70000}
          in
          let bytes =
            of_hex
              (String.concat ""
                 [ "011170" (* length 70000 *)
                 ; "00" (* type DATA *)
                 ; "01" (* END_STREAM *)
                 ; "00000003" (* stream 3 *) ] )
            ^ data
          in
          equal string bytes (encode f) ;
          equal frame f (Frame.read (reader bytes) ~max_size:70000) ;
          equal string "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" Frame.preface )
    ; test "write refuses a frame that it cannot write, and keeps no part of it"
        (fun () ->
          (* The flow refuses more than 64 KiB of output. Thus a write that does
             not end (for example frames with no payload, for ever) raises
             [Failure]. *)
          let invalid f =
            let out = Buffer.create 64 in
            let write buf off len =
              if Buffer.length out + len > 65536 then failwith "endless output" ;
              Buffer.add_subbytes out buf off len
            in
            let flow =
              Flow.make
                ~read:(fun _ _ _ -> 0)
                ~read_now:(fun _ _ _ -> Some 0)
                ~write ~readable:(always ()) ~shutdown_send:ignore ~close:ignore
                ~set_read_deadline:ignore ~set_write_deadline:ignore ()
            in
            let w = Writer.create ~max_frame:(1 lsl 25) flow in
            Frame.write w (Ping {ack= false; data= "before.."}) ;
            raises_match
              (function Invalid_argument _ -> true | _ -> false)
              (fun () -> f w) ;
            Frame.write w (Ping {ack= true; data= "after..."}) ;
            Writer.flush w ;
            equal string
              ( encode (Ping {ack= false; data= "before.."})
              ^ encode (Ping {ack= true; data= "after..."}) )
              (Buffer.contents out)
          in
          let data stream =
            Frame.Data {stream; end_stream= false; data= "x"; flow_length= 1}
          in
          List.iter
            (fun f -> invalid (fun w -> Frame.write w f))
            [ Push_promise {stream= 1}
            ; Unknown {kind= 0xa; stream= 1}
            ; Malformed {stream= 1; code= Cancel; reason= ""}
            ; data 0x8000_0000
            ; data (-1)
            ; Settings {ack= true; settings= [(0x1, 0)]}
            ; data 0
            ; Headers
                { stream= 0
                ; end_stream= false
                ; end_headers= true
                ; priority= None
                ; block= "" }
            ; Priority
                { stream= 0
                ; priority= {exclusive= false; dependency= 1; weight= 0} }
            ; Rst_stream {stream= 0; code= Cancel}
            ; Continuation {stream= 0; end_headers= true; block= ""}
            ; Ping {ack= false; data= "0123456"}
            ; Ping {ack= false; data= "012345678"}
            ; Data
                { stream= 1
                ; end_stream= false
                ; data= String.make (1 lsl 24) 'x'
                ; flow_length= 0 } ] ;
          invalid (fun w ->
              Frame.write_header_block w ~stream:1 ~end_stream:false ~max_size:0
                "a" ) ;
          invalid (fun w ->
              Frame.write_data w ~stream:1 ~end_stream:false (Bytes.create 4) 2
                3 ) ;
          (* The largest payload and stream. *)
          let max_data =
            Frame.Data
              { stream= 0x7fff_ffff
              ; end_stream= false
              ; data= String.make 0xff_ffff 'x'
              ; flow_length= 0xff_ffff }
          in
          let bytes =
            writes ~max_frame:(1 lsl 25) (fun w -> Frame.write w max_data)
          in
          equal string (of_hex "ffffff 00 00 7fffffff") (String.sub bytes 0 9) ;
          equal int (9 + 0xff_ffff) (String.length bytes) ) ]

(* {1 Connection state} *)

module Conn_state = Caravan_http2.Private.Conn_state

let max_window = 0x7fff_ffff

(* Two streams at most: the connection receive window is 131070 bytes. *)
let local =
  { Conn_state.max_concurrent_streams= 2
  ; initial_window= 65535
  ; header_table_size= 4096
  ; max_header_list_size= 16384 }

(* One stream at most: the connection receive window is 65535 bytes, as the
   window of a stream. Thus a credit of 32768 bytes or more sends a
   WINDOW_UPDATE frame on the connection too. *)
let one = {local with max_concurrent_streams= 1}

let show_use : Conn_state.use -> string = function
  | Open ->
      "open"
  | Trailers ->
      "trailers"
  | Refused ->
      "refused"
  | Reset c ->
      "reset " ^ show_code c
  | Ignored ->
      "ignored"

(* A string of more than 16 bytes prints as its length. *)
let show_bytes s =
  if String.length s > 16 then sprintf "len:%d" (String.length s)
  else sprintf "%S" s

let show_event : Conn_state.event -> string = function
  | Send f ->
      "send " ^ show f
  | Header_block {stream; block; end_stream; use} ->
      sprintf "block %d%s %s %s" stream
        (flags [(end_stream, "END_STREAM")])
        (show_bytes block) (show_use use)
  | Data {stream; data; end_stream} ->
      sprintf "data %d%s %s" stream
        (flags [(end_stream, "END_STREAM")])
        (show_bytes data)
  | Body_error {stream} ->
      sprintf "body error %d" stream
  | Peer_reset {stream} ->
      sprintf "peer reset %d" stream
  | Credit ->
      "credit"
  | Peer_goaway {last_stream; code} ->
      sprintf "peer goaway %d %s" last_stream (show_code code)
  | Stream_error {stream; code} ->
      sprintf "stream error %d %s" stream (show_code code)

let show_result = function
  | Ok events ->
      List.map show_event events
  | Error (c, _) ->
      ["error " ^ show_code c]

let texts = Testable.(list string)

(* A state after the client preface and its SETTINGS frame. *)
let started ?(local = local) () =
  let t = Conn_state.create local in
  ( match
      Conn_state.on_frame t ~live:0 (Frame.Settings {ack= false; settings= []})
    with
  | Ok _ ->
      ()
  | Error _ ->
      fail "the first SETTINGS frame gave an error" ) ;
  t

(* [gives t f expected]: the frame [f] of the client gives the events
   [expected], as text. *)
let gives ?__POS__ ?(live = 0) t f expected =
  equal ?__POS__ texts expected (show_result (Conn_state.on_frame t ~live f))

let error_text c = ["error " ^ show_code c]

let headers ?(end_stream = false) ?(end_headers = true) ?priority ?(block = "")
    stream =
  Frame.Headers {stream; end_stream; end_headers; priority; block}

let data ?(end_stream = false) ?(pad = 0) stream s =
  Frame.Data {stream; end_stream; data= s; flow_length= String.length s + pad}

let continuation ?(end_headers = true) stream block =
  Frame.Continuation {stream; end_headers; block}

let rst stream = Frame.Rst_stream {stream; code= Error.Cancel}

let window_update stream increment = Frame.Window_update {stream; increment}

let settings l = Frame.Settings {ack= false; settings= l}

let on stream = {Frame.exclusive= false; dependency= stream; weight= 15}

let priority stream dependency = Frame.Priority {stream; priority= on dependency}

let malformed stream =
  Frame.Malformed {stream; code= Error.Frame_size_error; reason= "a test"}

(* The texts of the expected events. *)
let block_text ?(end_stream = false) ?(block = "") stream use =
  show_event (Header_block {stream; block; end_stream; use})

let opened ?end_stream ?block stream = block_text ?end_stream ?block stream Open

let data_text ?(end_stream = false) stream s =
  show_event (Data {stream; data= s; end_stream})

let update stream increment =
  show_event (Send (Frame.Window_update {stream; increment}))

let ack = show_event (Send (Frame.Settings {ack= true; settings= []}))

let frames_text l = List.map show l

(* A WINDOW_UPDATE frame as [frames_text] shows it. *)
let sent_update stream increment = show (Frame.Window_update {stream; increment})

let open_stream ?end_stream t stream =
  gives t (headers ?end_stream stream) [opened ?end_stream stream]

(* A stream that ended in the two directions. *)
let ended t stream =
  open_stream ~end_stream:true t stream ;
  Conn_state.end_local t ~stream

(* A stream that we reset, and whose fiber ended. *)
let reset_and_discarded t stream =
  open_stream t stream ;
  ignore (Conn_state.reset_local t ~stream Error.Cancel) ;
  ignore (Conn_state.discard t ~stream)

(* A state for each closed kind, and one with a stream that is implicitly
   closed. Each item: the name, the state, the closed stream. *)
let closed_kinds ?(local = local) () =
  let reset_by_us () =
    let t = started ~local () in
    open_stream t 1 ;
    ignore (Conn_state.reset_local t ~stream:1 Error.Cancel) ;
    (t, 1)
  in
  let refused () =
    let t = started ~local () in
    gives ~live:local.max_concurrent_streams t (headers 1) [block_text 1 Refused] ;
    (t, 1)
  in
  let ignored () =
    let t = started ~local () in
    ignore (Conn_state.goaway t Error.No_error "") ;
    gives t (headers 1) [block_text 1 Ignored] ;
    (t, 1)
  in
  let ended_both () =
    let t = started ~local () in
    ended t 1 ; (t, 1)
  in
  let client_reset () =
    let t = started ~local () in
    open_stream t 1 ;
    gives t (rst 1) ["peer reset 1"] ;
    (t, 1)
  in
  let implicitly_closed () =
    let t = started ~local () in
    open_stream t 5 ; (t, 3)
  in
  List.map
    (fun (name, f) ->
      let t, stream = f () in
      (name, t, stream) )
    [ ("reset by us", reset_by_us)
    ; ("refused", refused)
    ; ("ignored", ignored)
    ; ("ended", ended_both)
    ; ("reset by the client", client_reset)
    ; ("implicitly closed", implicitly_closed) ]

let conn_state_rows =
  [ (* The HEADERS order of plan section 6. *)
    test "HEADERS order 1: an id in the table or in the closed set" (fun () ->
        (* Step 1 comes before step 2: these ids are not new. *)
        let t = started () in
        open_stream t 1 ;
        gives t
          (headers ~end_stream:true 1)
          [block_text ~end_stream:true 1 Trailers] ;
        open_stream t 3 ;
        ignore (Conn_state.reset_local t ~stream:3 Error.Cancel) ;
        gives t (headers 3) [block_text 3 Ignored] )
  ; test "HEADERS order 2: an even id (h2spec http2/5.1.1/1)" (fun () ->
        let t = started () in
        gives t (headers 2) (error_text Protocol_error) )
  ; test
      "HEADERS order 2: an id not more than the last id (h2spec http2/5.1.1/2)"
      (fun () ->
        let t = started () in
        open_stream t 5 ;
        gives t (headers 3) (error_text Protocol_error) ;
        let t = started () in
        open_stream t 5 ;
        gives t
          (headers ~end_stream:true 5)
          [block_text ~end_stream:true 5 Trailers] ;
        Conn_state.end_local t ~stream:5 ;
        ignore (Conn_state.discard t ~stream:5) ;
        gives t (headers 1) (error_text Protocol_error) )
  ; test "HEADERS order 3: a new id after our GOAWAY" (fun () ->
        let t = started () in
        open_stream t 1 ;
        open_stream t 3 ;
        let goaway = Conn_state.goaway t Error.No_error "" in
        equal frame
          (Frame.Goaway {last_stream= 3; code= Error.No_error; debug= ""})
          goaway ;
        (* Step 3 comes before step 4: two streams are active. *)
        gives t (headers 5) [block_text 5 Ignored] ;
        (* The id is in the closed set as "ignored". *)
        gives t (headers 5) [block_text 5 Ignored] ;
        gives t (data 5 "abc") [] ;
        (* Step 2 comes before step 3. *)
        gives t (headers 6) (error_text Protocol_error) ;
        equal int 2 (Conn_state.active_streams t) )
  ; test "HEADERS order 4: too many active streams (h2spec http2/5.1.2/1)"
      (fun () ->
        let t = started () in
        open_stream t 1 ;
        open_stream t 3 ;
        (* Step 4 comes before step 5. *)
        gives t (headers ~priority:(on 5) 5) [block_text 5 Refused] ;
        equal int 2 (Conn_state.active_streams t) ;
        (* The id is in the closed set as "reset by us". *)
        gives t (data 5 "abc") [] ;
        gives t (headers 5) [block_text 5 Ignored] ;
        equal (option frame) None
          (Conn_state.reset_local t ~stream:5 Error.Cancel) )
  ; test "HEADERS order 4: too many live fibers" (fun () ->
        let t = started () in
        gives ~live:2 t (headers 1) [block_text 1 Refused] ;
        equal int 0 (Conn_state.active_streams t) ;
        gives ~live:1 t (headers 3) [opened 3] )
  ; test
      "HEADERS order 5: a stream that depends on itself (h2spec http2/5.3.1/1)"
      (fun () ->
        let t = started () in
        gives t
          (headers ~priority:(on 1) 1)
          [block_text 1 (Reset Protocol_error)] ;
        equal int 0 (Conn_state.active_streams t) ;
        (* The id is in the closed set as "reset by us". *)
        gives t (data 1 "abc") [] ;
        gives t
          (headers ~end_stream:true 1)
          [block_text ~end_stream:true 1 Ignored] ;
        (* A dependency on another stream is not an error. *)
        gives t (headers ~priority:(on 1) 3) [opened 3] )
  ; test "HEADERS order 6: a new stream" (fun () ->
        let t = started () in
        gives t (headers ~block:"abc" 1) [opened ~block:"abc" 1] ;
        gives t (headers ~end_stream:true 3) [opened ~end_stream:true 3] ;
        equal int 2 (Conn_state.active_streams t) )
  ; (* The rules table of plan section 6, one test for each row. *)
    test "the first frame is not SETTINGS (h2spec http2/3.5/2)" (fun () ->
        List.iter
          (fun f ->
            let t = Conn_state.create local in
            gives t f (error_text Protocol_error) )
          [ Frame.Ping {ack= false; data= "12345678"}
          ; Frame.Settings {ack= true; settings= []}
          ; headers 1 ] ;
        let t = Conn_state.create local in
        gives t (settings []) [ack] )
  ; test "another frame in an open header block" (fun () ->
        (* h2spec http2/4.3/2, http2/4.3/3, http2/5.5/2, http2/6.2/1,
           http2/6.2/2, http2/6.10/2 *)
        List.iter
          (fun (cont, f) ->
            let t = started () in
            gives t (headers ~end_headers:false 1) [] ;
            if cont then gives t (continuation ~end_headers:false 1 "x") [] ;
            gives t f (error_text Protocol_error) )
          [ (false, priority 1 0)
          ; (false, headers 3)
          ; (false, Frame.Unknown {kind= 0xff; stream= 1})
          ; (true, data 1 "x")
          ; (false, continuation 3 "x")
          ; (false, Frame.Ping {ack= false; data= "12345678"}) ] )
  ; test "CONTINUATION with no open header block" (fun () ->
        (* h2spec http2/5.1/4, http2/6.10/4, http2/6.10/5, http2/6.10/6 *)
        let t = started () in
        gives t (continuation 1 "x") (error_text Protocol_error) ;
        let t = started () in
        open_stream t 1 ;
        gives t (continuation 1 "x") (error_text Protocol_error) ;
        let t = started () in
        gives t (headers ~end_headers:false 1) [] ;
        gives t (continuation 1 "x") [opened ~block:"x" 1] ;
        gives t (continuation 1 "y") (error_text Protocol_error) ;
        let t = started () in
        open_stream t 1 ;
        gives t (data 1 "x") [data_text 1 "x"] ;
        gives t (continuation 1 "y") (error_text Protocol_error) )
  ; test "HEADERS on an open stream with no END_STREAM (h2spec http2/8.1/1)"
      (fun () ->
        let t = started () in
        open_stream t 1 ;
        gives t (headers 1) [block_text 1 (Reset Protocol_error)] ;
        equal int 0 (Conn_state.active_streams t) ;
        (* Trailers that depend on their own stream. *)
        open_stream t 3 ;
        gives t
          (headers ~end_stream:true ~priority:(on 3) 3)
          [block_text ~end_stream:true 3 (Reset Protocol_error)] ;
        (* The stream is reset by us: no second RST_STREAM. *)
        equal (option frame) None
          (Conn_state.reset_local t ~stream:3 Error.Cancel) )
  ; test "HEADERS on a half-closed (remote) stream (h2spec http2/5.1/6)"
      (fun () ->
        let t = started () in
        open_stream ~end_stream:true t 1 ;
        gives t
          (headers ~end_stream:true 1)
          [block_text ~end_stream:true 1 (Reset Stream_closed)] ;
        equal int 0 (Conn_state.active_streams t) )
  ; test "WINDOW_UPDATE, RST_STREAM or PRIORITY on a closed id is ignored"
      (fun () ->
        List.iter
          (fun (_, t, stream) ->
            List.iter
              (fun f -> gives t f [])
              [ window_update stream 10
              ; window_update stream 0
              ; rst stream
              ; priority stream 0 ] )
          (closed_kinds ()) )
  ; test "a frame on an id that we reset, refused or ignored" (fun () ->
        List.iter
          (fun (name, t, stream) ->
            if List.mem name ["reset by us"; "refused"; "ignored"] then begin
              (* DATA gives connection credit at once. *)
              gives t (data stream (String.make 20000 'x')) [] ;
              gives t (data stream "" ~pad:12768) [update 0 32768] ;
              gives t
                (headers ~end_stream:true stream)
                [block_text ~end_stream:true stream Ignored] ;
              gives t (malformed stream) [] ;
              gives t (data ~end_stream:true stream "x") []
            end )
          (closed_kinds ~local:one ()) )
  ; test "HEADERS on an id that ended in two directions" (fun () ->
        (* h2spec http2/5.1/12; http2/5.1/9 after a RST_STREAM of the client *)
        let t = started () in
        ended t 1 ;
        gives t (headers ~end_stream:true 1) (error_text Stream_closed) ;
        let t = started () in
        open_stream t 1 ;
        gives t (rst 1) ["peer reset 1"] ;
        gives t (headers ~end_stream:true 1) (error_text Stream_closed) )
  ; test "DATA on an id that ended in two directions" (fun () ->
        (* h2spec http2/5.1/11; http2/5.1/8 after a RST_STREAM of the client *)
        let t = started () in
        ended t 1 ;
        gives t (data 1 "x") (error_text Stream_closed) ;
        let t = started () in
        open_stream t 1 ;
        gives t (rst 1) ["peer reset 1"] ;
        gives t (data 1 "x") (error_text Stream_closed) )
  ; test "a closed id that left the closed set" (fun () ->
        (* One stream at most: the closed set holds 4 ids. *)
        let t = started ~local:one () in
        List.iter (reset_and_discarded t) [1; 3; 5; 7; 9] ;
        (* 3 is in the set: its trailers are ignored. 1 left the set. *)
        gives t
          (headers ~end_stream:true 3)
          [block_text ~end_stream:true 3 Ignored] ;
        (* Step 2 of the HEADERS order: the id is not new. *)
        gives t (headers ~end_stream:true 1) (error_text Protocol_error) ;
        let t = started ~local:one () in
        List.iter (reset_and_discarded t) [1; 3; 5; 7; 9] ;
        (* h2spec http2/5.1/13: a CONTINUATION frame has no stream state. *)
        gives t (continuation 1 "x") (error_text Protocol_error) ;
        let t = started ~local:one () in
        List.iter (reset_and_discarded t) [1; 3; 5; 7; 9] ;
        (* DATA gives connection credit, and is ignored. *)
        gives t (data 1 (String.make 20000 'x')) [] ;
        gives t (data 1 "" ~pad:12768) [update 0 32768] ;
        List.iter
          (fun f -> gives t f [])
          [window_update 1 10; rst 1; priority 1 0; malformed 1] )
  ; test "refused ids push a reset id out of the closed set" (fun () ->
        (* The sequence of the correctness review of PR 4 (probe P1). The client
           sent DATA and trailers on stream 1 before it got our RST_STREAM. *)
        let t = started ~local:one () in
        reset_and_discarded t 1 ;
        open_stream t 3 ;
        List.iter
          (fun stream -> gives t (headers stream) [block_text stream Refused])
          [5; 7; 9; 11] ;
        gives t (data 1 "abc") [] ;
        gives t (data ~end_stream:true 1 "") [] ;
        gives t
          (headers ~end_stream:true 3)
          [block_text ~end_stream:true 3 Trailers] )
  ; test "the closed set holds at most 4096 ids" (fun () ->
        let t = started ~local:{local with max_concurrent_streams= 5000} () in
        let ids = List.init 4098 (fun i -> (2 * i) + 1) in
        List.iter (open_stream t) ids ;
        (* The last id closes first, then 4096 other ids. *)
        let last = 8195 in
        let close stream =
          ignore (Conn_state.reset_local t ~stream Error.Cancel) ;
          ignore (Conn_state.discard t ~stream)
        in
        close last ;
        List.iter close (List.filteri (fun i _ -> i < 4096) ids) ;
        equal int 1 (Conn_state.active_streams t) ;
        (* The last id left the set: it is closed, not idle. *)
        gives t (data last "x") [] ;
        gives t (headers ~end_stream:true last) (error_text Protocol_error) )
  ; test "DATA on an idle stream (h2spec http2/5.1/1, http2/6.1/1)" (fun () ->
        List.iter
          (fun stream ->
            let t = started () in
            open_stream t 1 ;
            gives t (data stream "x") (error_text Protocol_error) )
          [3; 2; 0] )
  ; test "DATA on a half-closed (remote) stream (h2spec http2/5.1/5)" (fun () ->
        let t = started ~local:one () in
        open_stream ~end_stream:true t 1 ;
        gives t
          (data 1 (String.make 40000 'x'))
          [update 0 40000; "stream error 1 STREAM_CLOSED"] ;
        equal int 0 (Conn_state.active_streams t) ;
        gives t (data 1 "x") [] )
  ; test "DATA over the stream window" (fun () ->
        let t = started () in
        open_stream t 1 ;
        gives t (data 1 (String.make 65536 'x')) (error_text Flow_control_error) ;
        let t = started () in
        open_stream t 1 ;
        gives t (data 1 "" ~pad:65535)
          [update 1 65535; update 0 65535; data_text 1 ""] ;
        gives t
          (data 1 (String.make 65535 'x'))
          [data_text 1 (String.make 65535 'x')] ;
        gives t (data 1 "x") (error_text Flow_control_error) )
  ; test "DATA over the connection window" (fun () ->
        (* The connection window is 131070 bytes, and half of it is 65535. *)
        let t = started () in
        open_stream t 1 ;
        open_stream t 3 ;
        let full = String.make 65535 'x' in
        gives t (data 1 full) [data_text 1 full] ;
        gives t
          (data 3 (String.make 40000 'x'))
          [data_text 3 (String.make 40000 'x')] ;
        equal texts
          [sent_update 3 40000]
          (frames_text (Conn_state.consumed t ~stream:3 40000)) ;
        (* Stream 3 has 65535 bytes of window, the connection 25535. *)
        gives t (data 3 (String.make 25536 'x')) (error_text Flow_control_error) ;
        let t = started () in
        open_stream t 1 ;
        open_stream t 3 ;
        gives t (data 1 full) [data_text 1 full] ;
        gives t (data 3 full) [data_text 3 full] ;
        ignore (Conn_state.reset_local t ~stream:1 Error.Cancel) ;
        gives t (data 1 "x") (error_text Flow_control_error) ;
        (* DATA on a stream that we reset, or that the client ended, counts in
           the connection window before its credit. *)
        let reset t =
          ignore (Conn_state.reset_local t ~stream:3 Error.Cancel)
        in
        let half_closed t =
          gives t
            (headers ~end_stream:true 3)
            [block_text ~end_stream:true 3 Trailers]
        in
        List.iter
          (fun close_3 ->
            let t = started () in
            open_stream t 1 ;
            open_stream t 3 ;
            gives t (data 1 full) [data_text 1 full] ;
            close_3 t ;
            ignore
              (Conn_state.on_frame t ~live:0 (data 3 (String.make 30000 'x'))) ;
            (* 131070 - 65535 - 30000 = 35535 bytes remain. *)
            open_stream t 5 ;
            gives t
              (data 5 (String.make 35536 'x'))
              (error_text Flow_control_error) )
          [reset; half_closed] )
  ; test "RST_STREAM or WINDOW_UPDATE on an idle stream" (fun () ->
        (* h2spec http2/5.1/2, http2/6.4/2, http2/5.1/3 *)
        List.iter
          (fun f ->
            let t = started () in
            open_stream t 1 ;
            gives t f (error_text Protocol_error) )
          [rst 3; window_update 3 10; rst 2; window_update 2 10] )
  ; test "WINDOW_UPDATE of 0 (h2spec http2/6.9/1, http2/6.9/2)" (fun () ->
        let t = started () in
        open_stream t 1 ;
        gives t (window_update 1 0) ["stream error 1 PROTOCOL_ERROR"] ;
        equal int 0 (Conn_state.active_streams t) ;
        gives t (window_update 0 0) (error_text Protocol_error) )
  ; test "WINDOW_UPDATE over 2^31-1 (h2spec http2/6.9.1/2, http2/6.9.1/3)"
      (fun () ->
        let t = started () in
        open_stream t 1 ;
        open_stream t 3 ;
        gives t (window_update 1 (max_window - 65535)) ["credit"] ;
        gives t (window_update 1 1) ["stream error 1 FLOW_CONTROL_ERROR"] ;
        equal int 1 (Conn_state.active_streams t) ;
        gives t (window_update 0 (max_window - 65535)) ["credit"] ;
        equal int 65535 (Conn_state.send_window t ~stream:3) ;
        gives t (window_update 0 1) (error_text Flow_control_error) )
  ; test "SETTINGS values (h2spec http2/6.5.2/1, /3, /4)" (fun () ->
        List.iter
          (fun (pairs, expected) ->
            let t = started () in
            gives t (settings pairs) expected )
          [ ([(0x2, 2)], error_text Protocol_error)
          ; ([(0x2, 0); (0x2, 1)], [ack])
          ; ([(0x5, 16383)], error_text Protocol_error)
          ; ([(0x5, 0x100_0000)], error_text Protocol_error) ] ;
        let t = started () in
        equal int 16384 (Conn_state.peer_max_frame_size t) ;
        gives t (settings [(0x5, 0xff_ffff)]) [ack] ;
        equal int 0xff_ffff (Conn_state.peer_max_frame_size t) ;
        gives t (settings [(0x5, 20000); (0x5, 16384)]) [ack] ;
        equal int 16384 (Conn_state.peer_max_frame_size t) ;
        (* A frame with an error changes nothing. *)
        gives t (settings [(0x5, 20000); (0x2, 2)]) (error_text Protocol_error) ;
        equal int 16384 (Conn_state.peer_max_frame_size t) )
  ; test "SETTINGS_ENABLE_CONNECT_PROTOCOL is ignored (plan section 15)"
      (fun () ->
        let t = started () in
        List.iter
          (fun v -> gives t (settings [(0x8, v)]) [ack])
          [0; 1; 2; 0xffff_ffff] )
  ; test
      "SETTINGS_INITIAL_WINDOW_SIZE over 2^31-1 (h2spec http2/6.5.2/2, \
       http2/6.9.2/3)" (fun () ->
        let t = started () in
        gives t
          (settings [(0x4, max_window + 1)])
          (error_text Flow_control_error) ;
        gives t (settings [(0x4, max_window)]) [ack; "credit"] ;
        (* A change that makes the window of a stream more than 2^31-1. *)
        let t = started () in
        open_stream t 1 ;
        gives t (window_update 1 (max_window - 65536)) ["credit"] ;
        gives t (settings [(0x4, 65536)]) [ack; "credit"] ;
        gives t (settings [(0x4, 65537)]) (error_text Flow_control_error) ;
        (* A stream that is closed does not count. *)
        ignore (Conn_state.reset_local t ~stream:1 Error.Cancel) ;
        gives t (settings [(0x4, 65537)]) [ack; "credit"] )
  ; test
      "SETTINGS_INITIAL_WINDOW_SIZE change (h2spec http2/6.9.2/1, /2, \
       http2/6.5.3/1)" (fun () ->
        let t = started () in
        open_stream t 1 ;
        gives t (settings [(0x4, 100); (0x4, 1000)]) [ack; "credit"] ;
        equal int 1000 (Conn_state.send_window t ~stream:1) ;
        Conn_state.sent t ~stream:1 600 ;
        gives t (settings [(0x4, 0)]) [ack; "credit"] ;
        equal int (-600) (Conn_state.send_window t ~stream:1) ;
        gives t (settings [(0x4, 0)]) [ack] ;
        gives t (settings [(0x4, 1600)]) [ack; "credit"] ;
        equal int 1000 (Conn_state.send_window t ~stream:1) ;
        (* A new stream starts with the last value. *)
        open_stream t 3 ;
        equal int 1600 (Conn_state.send_window t ~stream:3) )
  ; test "one delta for each SETTINGS frame (plan section 15)" (fun () ->
        (* With a delta for each pair, the first pair would make the window of
           stream 1 more than 2^31-1. *)
        let t = started () in
        open_stream t 1 ;
        gives t (window_update 0 (max_window - 65535)) ["credit"] ;
        gives t (window_update 1 (max_window - 65535 - 10)) ["credit"] ;
        gives t (settings [(0x4, 65535 + 100); (0x4, 65535)]) [ack] ;
        equal int (max_window - 10) (Conn_state.send_window t ~stream:1) )
  ; test "a SETTINGS frame gets an ACK (h2spec http2/6.5.3/2, http2/6.5.2/5)"
      (fun () ->
        let t = started () in
        gives t (settings [(0x1, 100); (0x3, 7); (0x6, 10)]) [ack] ;
        (* Frame.read keeps only the known ids: a frame with only unknown ids
           has no settings (plan section 15). *)
        gives t (settings []) [ack] ;
        gives t (Frame.Settings {ack= true; settings= []}) [] )
  ; test "PING (h2spec http2/6.7/1, http2/6.7/2)" (fun () ->
        let t = started () in
        gives t
          (Frame.Ping {ack= false; data= "abcdefgh"})
          [show_event (Send (Frame.Ping {ack= true; data= "abcdefgh"}))] ;
        gives t (Frame.Ping {ack= true; data= "abcdefgh"}) [] )
  ; test "PUSH_PROMISE from a client (h2spec http2/8.2/1)" (fun () ->
        let t = started () in
        open_stream t 1 ;
        gives t (Frame.Push_promise {stream= 1}) (error_text Protocol_error) )
  ; test "GOAWAY from the client" (fun () ->
        let t = started () in
        gives t
          (Frame.Goaway
             {last_stream= 7; code= Error.Enhance_your_calm; debug= "x"} )
          ["peer goaway 7 ENHANCE_YOUR_CALM"] )
  ; test "PRIORITY and unknown frames do nothing (h2spec http2/5.5/1)"
      (fun () ->
        let t = started () in
        gives t (priority 3 1) [] ;
        gives t (Frame.Unknown {kind= 0xff; stream= 3}) [] ;
        gives t (Frame.Unknown {kind= 0xff; stream= 0}) [] ;
        (* Stream 3 stays idle. *)
        open_stream t 3 ;
        gives t (priority 3 0) [] ;
        gives t (Frame.Unknown {kind= 0xff; stream= 3}) [] ;
        equal int 1 (Conn_state.active_streams t) )
  ; test "Malformed on an open or half-closed stream" (fun () ->
        let t = started () in
        open_stream t 1 ;
        gives t (malformed 1) ["stream error 1 FRAME_SIZE_ERROR"] ;
        open_stream ~end_stream:true t 3 ;
        gives t (malformed 3) ["stream error 3 FRAME_SIZE_ERROR"] ;
        open_stream t 5 ;
        Conn_state.end_local t ~stream:5 ;
        gives t (malformed 5) ["stream error 5 FRAME_SIZE_ERROR"] ;
        equal int 0 (Conn_state.active_streams t) )
  ; test "Malformed on an idle id (h2spec http2/5.3.1/2)" (fun () ->
        let t = started () in
        gives t
          (Frame.Malformed
             {stream= 1; code= Error.Protocol_error; reason= "self"} )
          (error_text Protocol_error) ;
        let t = started () in
        gives t (malformed 2) (error_text Frame_size_error) )
  ; test
      "Malformed on a closed id: a stream error after the two ends, else none"
      (fun () ->
        List.iter
          (fun (name, t, stream) ->
            if name = "ended" then begin
              (* h2spec http2/6.3/2: our END_STREAM can wait in the queue. *)
              gives t (malformed stream)
                [sprintf "stream error %d FRAME_SIZE_ERROR" stream] ;
              (* The stream is then "reset by us": DATA is ignored, and a second
                 malformed frame gives nothing. *)
              gives t (malformed stream) [] ;
              gives t (data stream "x") []
            end
            else gives t (malformed stream) [] )
          (closed_kinds ()) ;
        (* The same after the engine discarded the stream: only the closed set
           has it. *)
        let t = started () in
        ended t 1 ;
        ignore (Conn_state.discard t ~stream:1) ;
        gives t (malformed 1) ["stream error 1 FRAME_SIZE_ERROR"] ;
        gives t (malformed 1) [] ;
        gives t (data 1 "x") [] )
  ; test "a bad local value" (fun () ->
        Conn_state.validate local ;
        Conn_state.validate
          { Conn_state.max_concurrent_streams= 0xffff_ffff
          ; initial_window= max_window
          ; header_table_size= 0xffff_ffff
          ; max_header_list_size= 0xffff_ffff } ;
        Conn_state.validate
          {one with header_table_size= 0; max_header_list_size= 0} ;
        List.iter
          (fun (l, text) ->
            raises
              (Invalid_argument ("Conn_state.validate: " ^ text))
              (fun () -> Conn_state.validate l) )
          [ ({local with max_concurrent_streams= 0}, "max_concurrent_streams 0")
          ; ( {local with max_concurrent_streams= 0x1_0000_0000}
            , "max_concurrent_streams 4294967296" )
          ; ({local with initial_window= 65534}, "initial_window 65534")
          ; ( {local with initial_window= max_window + 1}
            , "initial_window 2147483648" )
          ; ({local with header_table_size= -1}, "header_table_size -1")
          ; ( {local with header_table_size= 0x1_0000_0000}
            , "header_table_size 4294967296" )
          ; ({local with max_header_list_size= -1}, "max_header_list_size -1")
          ; ( {local with max_header_list_size= 0x1_0000_0000}
            , "max_header_list_size 4294967296" ) ] )
  ; test "start_frames" (fun () ->
        let settings_of l =
          Frame.Settings
            { ack= false
            ; settings=
                [ (0x1, l.Conn_state.header_table_size)
                ; (0x3, l.max_concurrent_streams)
                ; (0x4, l.initial_window)
                ; (0x6, l.max_header_list_size) ] }
        in
        let start l = Conn_state.start_frames (Conn_state.create l) in
        equal (list frame)
          [settings_of local; window_update 0 65535]
          (start local) ;
        (* 65535 needs no WINDOW_UPDATE frame. *)
        equal (list frame) [settings_of one] (start one) ;
        let large = {local with max_concurrent_streams= 0xffff_ffff} in
        equal (list frame)
          [settings_of large; window_update 0 (max_window - 65535)]
          (start large) ;
        (* 32768 * 65535 is less than 2^31-1, 32769 * 65535 is more. *)
        let edge n = {local with max_concurrent_streams= n} in
        equal (list frame)
          [settings_of (edge 32768); window_update 0 ((32768 * 65535) - 65535)]
          (start (edge 32768)) ;
        equal (list frame)
          [settings_of (edge 32769); window_update 0 (max_window - 65535)]
          (start (edge 32769)) )
  ; test "our GOAWAY" (fun () ->
        let t = started () in
        let last t =
          match Conn_state.goaway t Error.No_error "" with
          | Frame.Goaway {last_stream; _} ->
              last_stream
          | _ ->
              fail "not a GOAWAY frame"
        in
        let t2 = started () in
        equal int 0 (last t2) ;
        open_stream t 1 ;
        open_stream t 3 ;
        gives t (headers 5) [block_text 5 Refused] ;
        equal frame
          (Frame.Goaway {last_stream= 5; code= Error.Protocol_error; debug= "d"})
          (Conn_state.goaway t Error.Protocol_error "d") ;
        gives t (headers 7) [block_text 7 Ignored] ;
        equal int 5 (last t) )
  ; test "reset_local and end_local keep the RST rule" (fun () ->
        let t = started () in
        let rst_frame stream =
          Some (Frame.Rst_stream {stream; code= Error.Cancel})
        in
        equal (option frame) None
          (Conn_state.reset_local t ~stream:1 Error.Cancel) ;
        open_stream t 1 ;
        equal (option frame) (rst_frame 1)
          (Conn_state.reset_local t ~stream:1 Error.Cancel) ;
        equal (option frame) None
          (Conn_state.reset_local t ~stream:1 Error.Cancel) ;
        equal int 0 (Conn_state.active_streams t) ;
        (* Ended in the two directions: no RST_STREAM. *)
        open_stream t 3 ;
        Conn_state.end_local t ~stream:3 ;
        equal int 1 (Conn_state.active_streams t) ;
        gives t (data ~end_stream:true 3 "x") [data_text ~end_stream:true 3 "x"] ;
        equal int 0 (Conn_state.active_streams t) ;
        equal (option frame) None
          (Conn_state.reset_local t ~stream:3 Error.Cancel) ;
        (* A stream that the client reset. *)
        open_stream t 5 ;
        gives t (rst 5) ["peer reset 5"] ;
        equal (option frame) None
          (Conn_state.reset_local t ~stream:5 Error.Cancel) ;
        (* end_local on a stream that is not active does nothing. *)
        Conn_state.end_local t ~stream:9 ;
        Conn_state.end_local t ~stream:5 ;
        equal int 0 (Conn_state.active_streams t) ) ]

(* [length t stream n] sets the content-length [n] of [stream], and makes sure
   that the result is [gives]. *)
let length ?(gives = []) t stream n =
  equal texts gives
    (List.map show_event (Conn_state.set_content_length t ~stream n))

let conn_state_flows =
  [ test "send_window after sent, WINDOW_UPDATE and SETTINGS" (fun () ->
        let t = started () in
        open_stream t 1 ;
        open_stream t 3 ;
        let window stream = Conn_state.send_window t ~stream in
        equal int 65535 (window 1) ;
        Conn_state.sent t ~stream:1 1000 ;
        (* The connection window limits stream 3. *)
        equal int 64535 (window 1) ;
        equal int 64535 (window 3) ;
        gives t (window_update 0 10000) ["credit"] ;
        equal int 64535 (window 1) ;
        equal int 65535 (window 3) ;
        gives t (window_update 1 5) ["credit"] ;
        equal int 64540 (window 1) ;
        (* A delta of -64535 on each stream. *)
        gives t (settings [(0x4, 1000)]) [ack; "credit"] ;
        equal int 5 (window 1) ;
        equal int 1000 (window 3) ;
        (* A negative stream window. *)
        Conn_state.sent t ~stream:3 3000 ;
        equal int (-2000) (window 3) ;
        (* A delta of +69000. The connection window is 71535. *)
        gives t (settings [(0x4, 70000)]) [ack; "credit"] ;
        equal int 67000 (window 3) ;
        equal int 69005 (window 1) ;
        Conn_state.sent t ~stream:1 69005 ;
        equal int 0 (window 1) ;
        equal int 2530 (window 3) ;
        (* A stream that the state does not know. *)
        equal int 0 (window 99) ;
        Conn_state.sent t ~stream:99 30 ;
        equal int 2500 (window 3) )
  ; test "receive windows: consumed, padding and discard" (fun () ->
        (* One stream at most: the two windows have 65535 bytes, and a credit of
           32768 bytes or more sends a WINDOW_UPDATE frame. *)
        let t = started ~local:one () in
        open_stream t 1 ;
        let big = String.make 32768 'x' in
        gives t (data 1 big) [data_text 1 big] ;
        let consumed n = frames_text (Conn_state.consumed t ~stream:1 n) in
        equal texts [] (consumed 32767) ;
        equal texts [sent_update 1 32768; sent_update 0 32768] (consumed 1) ;
        (* No byte is held: [consumed] gives back nothing. *)
        equal texts [] (consumed 40000) ;
        equal texts [] (consumed (-5)) ;
        (* The padding goes back at once. *)
        gives t (data 1 "abc" ~pad:32765) [data_text 1 "abc"] ;
        gives t (data 1 "" ~pad:3)
          [update 1 32768; update 0 32768; data_text 1 ""] ;
        equal texts [] (consumed 3) ;
        (* discard gives back the held bytes as consumed does. *)
        let t = started ~local:one () in
        open_stream t 1 ;
        let n20 = String.make 20000 'x' in
        gives t (data 1 n20) [data_text 1 n20] ;
        equal texts [] (frames_text (Conn_state.consumed t ~stream:1 20000)) ;
        gives t (data 1 n20) [data_text 1 n20] ;
        equal texts
          [sent_update 1 40000; sent_update 0 40000]
          (frames_text (Conn_state.discard t ~stream:1)) ;
        equal texts [] (frames_text (Conn_state.discard t ~stream:1)) ;
        (* After discard, the bytes of DATA go back at once. The stream window
           is full again: 65535 bytes. *)
        let n32 = String.make 32768 'x' in
        gives t (data 1 n32) [update 1 32768; update 0 32768; data_text 1 n32] ;
        gives t
          (data 1 (String.make 65535 'x'))
          [update 1 65535; update 0 65535; data_text 1 (String.make 65535 'x')] ;
        equal int 1 (Conn_state.active_streams t) ;
        equal (option frame)
          (Some (Frame.Rst_stream {stream= 1; code= Error.Cancel}))
          (Conn_state.reset_local t ~stream:1 Error.Cancel) ;
        (* The stream was discarded and is closed: the state forgot it. *)
        equal int 0 (Conn_state.send_window t ~stream:1) ;
        gives t (data 1 "x") [] )
  ; test "a stream that the client ended gets no WINDOW_UPDATE frame" (fun () ->
        let t = started ~local:one () in
        open_stream t 1 ;
        let big = String.make 40000 'x' in
        gives t (data ~end_stream:true 1 big) [data_text ~end_stream:true 1 big] ;
        equal texts
          [sent_update 0 40000]
          (frames_text (Conn_state.consumed t ~stream:1 40000)) )
  ; test "a closed stream keeps its held bytes until discard" (fun () ->
        let t = started ~local:one () in
        open_stream t 1 ;
        let big = String.make 40000 'x' in
        gives t (data 1 big) [data_text 1 big] ;
        gives t (rst 1) ["peer reset 1"] ;
        equal texts
          [sent_update 0 40000]
          (frames_text (Conn_state.consumed t ~stream:1 40000)) ;
        open_stream t 3 ;
        gives t (data ~end_stream:true 3 big) [data_text ~end_stream:true 3 big] ;
        Conn_state.end_local t ~stream:3 ;
        equal int 0 (Conn_state.active_streams t) ;
        equal texts
          [sent_update 0 40000]
          (frames_text (Conn_state.discard t ~stream:3)) ;
        (* The stream was closed and discarded: the state forgot it. *)
        equal int 0 (Conn_state.send_window t ~stream:3) )
  ; test "a WINDOW_UPDATE frame at exactly half of the window" (fun () ->
        (* An even window: 65536 bytes for the stream and the connection. *)
        let t = started ~local:{one with initial_window= 65536} () in
        open_stream t 1 ;
        gives t (data 1 "" ~pad:32767) [data_text 1 ""] ;
        gives t (data 1 "" ~pad:1)
          [update 1 32768; update 0 32768; data_text 1 ""] )
  ; (* Plan section 6, test 4. *)
    test "CONTINUATION after a Refused, a Reset and an Ignored block" (fun () ->
        let split t stream expected =
          gives t (headers ~end_headers:false ~block:"ab" stream) [] ;
          gives t (continuation ~end_headers:false stream "cd") [] ;
          gives t (continuation ~end_headers:false stream "") [] ;
          gives t (continuation stream "ef") [expected] ;
          (* The block is closed: a PING frame is legal. *)
          gives t
            (Frame.Ping {ack= false; data= "12345678"})
            [show_event (Send (Frame.Ping {ack= true; data= "12345678"}))]
        in
        let t = started ~local:one () in
        open_stream t 1 ;
        split t 3 (block_text ~block:"abcdef" 3 Refused) ;
        let t = started () in
        open_stream t 1 ;
        split t 1 (block_text ~block:"abcdef" 1 (Reset Protocol_error)) ;
        let t = started () in
        gives t (headers ~end_headers:false ~block:"ab" ~priority:(on 3) 3) [] ;
        gives t (continuation 3 "cd")
          [block_text ~block:"abcd" 3 (Reset Protocol_error)] ;
        let t = started () in
        ignore (Conn_state.goaway t Error.No_error "") ;
        split t 1 (block_text ~block:"abcdef" 1 Ignored) ;
        let t = started () in
        gives t (headers ~end_stream:true ~end_headers:false ~block:"ab" 1) [] ;
        gives t (continuation 1 "cd") [opened ~end_stream:true ~block:"abcd" 1] ;
        (* The next frame is a new frame. *)
        gives t
          (Frame.Ping {ack= false; data= "12345678"})
          [show_event (Send (Frame.Ping {ack= true; data= "12345678"}))] )
  ; (* Plan section 6, test 5: the content-length rule (h2spec http2/8.1.2.6/1,
       http2/8.1.2.6/2). *)
    test "content-length: more bytes than declared" (fun () ->
        let t = started () in
        open_stream t 1 ;
        length t 1 5 ;
        gives t (data 1 "abc") [data_text 1 "abc"] ;
        gives t (data 1 "def") ["body error 1"] ;
        equal int 0 (Conn_state.active_streams t) ;
        gives t (data 1 "x") [] )
  ; test "content-length: fewer bytes than declared" (fun () ->
        let t = started () in
        open_stream t 1 ;
        length t 1 5 ;
        gives t (data 1 "ab") [data_text 1 "ab"] ;
        gives t (data ~end_stream:true 1 "c") ["body error 1"] ;
        equal int 0 (Conn_state.active_streams t) )
  ; test "content-length: the declared length, with padding" (fun () ->
        let t = started () in
        open_stream t 1 ;
        length t 1 5 ;
        gives t (data 1 "abc" ~pad:10) [data_text 1 "abc"] ;
        gives t
          (data ~end_stream:true 1 "de")
          [data_text ~end_stream:true 1 "de"] )
  ; test "content-length: END_STREAM on the HEADERS frame" (fun () ->
        let t = started () in
        open_stream ~end_stream:true t 1 ;
        length ~gives:["body error 1"] t 1 5 ;
        equal int 0 (Conn_state.active_streams t) ;
        let t = started () in
        open_stream ~end_stream:true t 1 ;
        length t 1 0 )
  ; test "content-length: the bytes before set_content_length count" (fun () ->
        let t = started () in
        open_stream t 1 ;
        gives t (data 1 "abcdef") [data_text 1 "abcdef"] ;
        length ~gives:["body error 1"] t 1 5 )
  ; test "content-length: trailers that end a short body" (fun () ->
        let t = started () in
        open_stream t 1 ;
        length t 1 5 ;
        gives t (data 1 "abc") [data_text 1 "abc"] ;
        gives t
          (headers ~end_stream:true 1)
          [block_text ~end_stream:true 1 (Reset Protocol_error)] ;
        equal int 0 (Conn_state.active_streams t) )
  ; test "content-length: a stream that is not active" (fun () ->
        let t = started () in
        open_stream t 1 ;
        gives t
          (data ~end_stream:true 1 "abcdef")
          [data_text ~end_stream:true 1 "abcdef"] ;
        Conn_state.end_local t ~stream:1 ;
        length t 1 2 ;
        length t 7 2 )
  ; (* A Body_error after our END_STREAM gives a RST_STREAM (plan section
       18). *)
    test "content-length: a Body_error after end_local" (fun () ->
        let t = started () in
        open_stream t 1 ;
        length t 1 5 ;
        Conn_state.end_local t ~stream:1 ;
        gives t (data ~end_stream:true 1 "abc") ["body error 1"] ;
        equal int 0 (Conn_state.active_streams t) )
  ; test "a header block over 4 * max_header_list_size" (fun () ->
        (* The limit is 4 * 20000 = 80000 bytes. *)
        let large = {local with max_header_list_size= 20000} in
        let block n = String.make n 'a' in
        let t = started ~local:large () in
        gives t (headers ~block:(block 80000) 1) [opened ~block:(block 80000) 1] ;
        gives t (headers ~block:(block 80001) 3) (error_text Enhance_your_calm) ;
        let t = started ~local:large () in
        gives t (headers ~end_headers:false ~block:(block 40000) 1) [] ;
        gives t (continuation ~end_headers:false 1 (block 40000)) [] ;
        gives t (continuation 1 "c") (error_text Enhance_your_calm) ;
        let t = started ~local:large () in
        gives t (headers ~end_headers:false ~block:(block 40000) 1) [] ;
        gives t (continuation 1 (block 40000)) [opened ~block:(block 80000) 1] )
  ; test "a header block limit of 65536 bytes at least" (fun () ->
        (* A small max_header_list_size: the engine gives 431, not GOAWAY. *)
        let block n = String.make n 'a' in
        List.iter
          (fun max_header_list_size ->
            let small = {local with max_header_list_size} in
            let t = started ~local:small () in
            gives t (headers ~block:(block 100) 1) [opened ~block:(block 100) 1] ;
            gives t
              (headers ~block:(block 65536) 3)
              [opened ~block:(block 65536) 3] ;
            let t = started ~local:small () in
            gives t
              (headers ~block:(block 65537) 1)
              (error_text Enhance_your_calm) )
          [0; 10; 16384] )
  ; test "known_streams goes back to 0" (fun () ->
        let t = started () in
        equal int 0 (Conn_state.known_streams t) ;
        open_stream t 1 ;
        open_stream ~end_stream:true t 3 ;
        equal int 2 (Conn_state.known_streams t) ;
        (* Stream 1: reset, then discarded. *)
        ignore (Conn_state.reset_local t ~stream:1 Error.Cancel) ;
        equal int 2 (Conn_state.known_streams t) ;
        ignore (Conn_state.discard t ~stream:1) ;
        equal int 1 (Conn_state.known_streams t) ;
        (* Stream 3: discarded, then ended. *)
        ignore (Conn_state.discard t ~stream:3) ;
        equal int 1 (Conn_state.known_streams t) ;
        Conn_state.end_local t ~stream:3 ;
        equal int 0 (Conn_state.known_streams t) ;
        (* Refused and ignored ids have no record. *)
        gives ~live:2 t (headers 5) [block_text 5 Refused] ;
        ignore (Conn_state.goaway t Error.No_error "") ;
        gives t (headers 7) [block_text 7 Ignored] ;
        equal int 0 (Conn_state.known_streams t) ) ]

(* {2 The stateful test of plan section 6, test 2}

   Programs of client frames and engine calls on a connection, against a model
   of the client view of the windows and of the stream states. The model judges
   each outcome, and the invariant of the connection runs after each call. *)

module Model = struct
  let mcs = 3

  let iw = 65535

  let conn_start = mcs * iw

  type phase = Active | Reset_by_us | Ended

  type stream =
    { id: int
    ; mutable phase: phase
    ; opened: bool (* [Open] made a record for it *)
    ; mutable remote_ended: bool
    ; mutable local_ended: bool
    ; mutable live: bool (* the fake engine has a fiber for it *)
    ; mutable discarded: bool
    ; mutable rst_pending: bool (* a RST_STREAM NO_ERROR after our END *)
    ; mutable window: int (* the receive window of the client view *)
    ; mutable held: int
    ; mutable send: int
    ; mutable client_reset: bool
    ; mutable input_ended: bool
    ; mutable reset_seen: bool }

  type t =
    { mutable streams: stream list (* the newest first *)
    ; mutable skipped: int list (* the ids that the client did not use *)
    ; mutable closed_set:
        int list (* the closed set of the state, newest first *)
    ; mutable next: int
    ; mutable dead: bool (* after a connection error *)
    ; mutable conn_window: int
    ; mutable conn_send: int
    ; mutable peer_initial: int }

  let create () =
    { streams= []
    ; skipped= []
    ; closed_set= []
    ; next= 1
    ; dead= false
    ; conn_window= conn_start
    ; conn_send= 65535
    ; peer_initial= 65535 }

  let find m id = List.find (fun s -> s.id = id) m.streams

  let count f m = List.length (List.filter f m.streams)

  let active = count (fun s -> s.phase = Active)

  let live = count (fun s -> s.live)

  (* A record stays until the stream is closed and discarded. *)
  let known =
    count (fun s -> s.opened && not (s.phase <> Active && s.discarded))

  (* The ids of the streams that pass [f]. A dead connection has none: each call
     on it is skipped. *)
  let ids f m =
    if m.dead then []
    else
      List.rev
        (List.filter_map (fun s -> if f s then Some s.id else None) m.streams)

  let alive m = not m.dead

  (* The closed set of the state: [min (4 * mcs) 4096] ids. *)
  let capacity = 4 * mcs

  (* [close m s phase] closes the stream [s], in the same order as the state. *)
  let close m s phase =
    s.phase <- phase ;
    m.closed_set <- List.filteri (fun i _ -> i < capacity) (s.id :: m.closed_set)
end

(* The system: the state, with the client ids and the live count of a fake
   engine. *)
type client = {cs: Conn_state.t; mutable next_id: int; mutable live: int}

type outcome =
  | Events of (Conn_state.event list, Error.code * string) result
  | Frames of Frame.t list
  | Exit of Frame.t option * Frame.t list
  | Done

let pp_outcome ppf o =
  Format.pp_print_string ppf
    ( match o with
    | Events r ->
        "[" ^ String.concat "; " (show_result r) ^ "]"
    | Frames l ->
        "[" ^ String.concat "; " (List.map show l) ^ "]"
    | Exit (r, l) ->
        sprintf "%s, [%s]"
          (match r with Some f -> show f | None -> "no RST_STREAM")
          (String.concat "; " (List.map show l))
    | Done ->
        "()" )

let outcome = Testable.structural ~pp:pp_outcome

(* No function raises. *)
let got = function
  | Ok o ->
      o
  | Error e ->
      failf "the system raised %s" (Printexc.to_string e)

let events r =
  match got r with
  | Events (Ok l) ->
      l
  | Events (Error (c, reason)) ->
      failf "a connection error %s: %s" (show_code c) reason
  | o ->
      failf "not events: %a" pp_outcome o

let frames r =
  match got r with Frames fs -> fs | o -> failf "not frames: %a" pp_outcome o

(* [apply_update m f] applies the WINDOW_UPDATE frame [f] of the server to the
   client view. *)
let apply_update (m : Model.t) = function
  | Frame.Window_update {stream= 0; increment} ->
      cover "a WINDOW_UPDATE frame on the connection" true ;
      is_true ~msg:"an increment more than 0" (increment > 0) ;
      m.conn_window <- m.conn_window + increment
  | Frame.Window_update {stream; increment} ->
      let s = Model.find m stream in
      cover "a WINDOW_UPDATE frame on a stream" true ;
      is_true ~msg:"a WINDOW_UPDATE frame on a stream that can get DATA"
        (s.phase = Active && not s.remote_ended) ;
      is_true ~msg:"an increment more than 0" (increment > 0) ;
      s.window <- s.window + increment
  | f ->
      failf "not a WINDOW_UPDATE frame: %s" (show f)

(* The events that are not WINDOW_UPDATE frames, as text. The WINDOW_UPDATE
   frames go to the client view. *)
let others m evs =
  List.map show_event
    (List.filter
       (function
         | Conn_state.Send (Frame.Window_update _ as f) ->
             apply_update m f ; false
         | _ ->
             true )
       evs )

(* A stream gets at most one end of its request body. *)
let end_input (s : Model.stream) =
  is_true ~msg:"one end of the request body" (not s.input_ended) ;
  s.input_ended <- true

(* A stream gets at most one reset, and no event after it: the judges expect no
   event on a stream that is not active. *)
let reset_event (s : Model.stream) =
  is_true ~msg:"one reset event" (not s.reset_seen) ;
  s.reset_seen <- true

let invariant (m : Model.t) c =
  let cs = c.cs in
  (* active_streams is at most max_concurrent_streams. *)
  is_true ~msg:"active_streams <= max_concurrent_streams"
    (Conn_state.active_streams cs <= Model.mcs) ;
  equal ~msg:"active streams" int (Model.active m)
    (Conn_state.active_streams cs) ;
  equal ~msg:"live fibers" int (Model.live m) c.live ;
  (* The state forgets each stream that is closed and discarded. *)
  equal ~msg:"known streams" int (Model.known m) (Conn_state.known_streams cs) ;
  (* The connection receive window: window + unsent credit = start - held bytes,
     with an unsent credit from 0 to less than half of the window. A credit that
     is too large gives a negative unsent credit. *)
  let held =
    List.fold_left (fun n (s : Model.stream) -> n + s.held) 0 m.streams
  in
  let unsent = Model.conn_start - held - m.conn_window in
  is_true ~msg:"connection: no credit more than the held bytes" (unsent >= 0) ;
  is_true ~msg:"connection: an unsent credit less than half"
    (2 * unsent < Model.conn_start) ;
  List.iter
    (fun (s : Model.stream) ->
      if s.phase = Active then begin
        (* The send window of each active stream is the one of the model. *)
        equal ~msg:"send_window" int (min m.conn_send s.send)
          (Conn_state.send_window cs ~stream:s.id) ;
        (* The same rule for the receive window of each stream that can get
           DATA, also after discard. *)
        if not s.remote_ended then begin
          let unsent = Model.iw - s.held - s.window in
          is_true ~msg:"stream: no credit more than the held bytes" (unsent >= 0) ;
          is_true ~msg:"stream: an unsent credit less than half"
            (2 * unsent < Model.iw)
        end
      end )
    m.streams

let stateful_local =
  {local with max_concurrent_streams= Model.mcs; initial_window= Model.iw}

let conn =
  abstract "conn" ~invariant ~pp:(fun ppf (m : Model.t) ->
      Format.fprintf ppf "%d streams, %d active, %d live%s"
        (List.length m.streams) (Model.active m) (Model.live m)
        (if m.dead then ", dead" else "") )

(* The client can send DATA and trailers. A stream that we reset is in the list:
   the client did not get the RST_STREAM frame yet. *)
let sendable =
  among int conn
    (Model.ids (fun s -> (not s.remote_ended) && not s.client_reset))

let resettable =
  among int conn (Model.ids (fun s -> (not s.client_reset) && s.phase <> Ended))

let any_stream = among int conn (Model.ids (fun _ -> true))

let skipped =
  among int conn (fun (m : Model.t) -> if m.dead then [] else m.skipped)

let holding = among int conn (Model.ids (fun s -> s.live && s.held > 0))

let responding =
  among int conn
    (Model.ids (fun s -> s.phase = Active && s.live && not s.local_ended))

let living = among int conn (Model.ids (fun s -> s.live))

let pending = among int conn (Model.ids (fun s -> s.rst_pending))

let pp_int = Format.pp_print_int

let int_gen l = Gen.with_pp pp_int (Gen.frequency l)

let zero = Gen.constant ~pp:pp_int 0

(* Most DATA frames are large, so that the windows fill to half. *)
let size_gen = int_gen [(1, Gen.int_range 0 100); (2, Gen.int_range 10000 40000)]

let pad_gen = int_gen [(3, zero); (1, Gen.int_range 1 300)]

(* END_STREAM on one DATA frame in five, so that most streams can get a
   WINDOW_UPDATE frame. *)
let end_gen =
  Gen.with_pp Format.pp_print_bool
    (Gen.frequency
       [ (1, Gen.constant ~pp:Format.pp_print_bool true)
       ; (4, Gen.constant ~pp:Format.pp_print_bool false) ] )

(* The client skips an id in one HEADERS frame in ten. A HEADERS frame on a
   skipped id ends the connection, so the gaps must be rare. *)
let gap_gen = int_gen [(9, zero); (1, Gen.int_range 1 2)]

let increment_gen = Gen.int_range 1 100_000

(* Small windows and large sends, so that some send windows go negative. *)
let window_gen =
  int_gen [(1, Gen.int_range 0 1000); (1, Gen.int_range 0 200_000)]

let send_gen = int_gen [(1, Gen.int_range 0 2000); (1, Gen.int_range 0 70_000)]

(* Large reads, so that the credit reaches half of a window. *)
let consume_gen =
  int_gen [(1, Gen.int_range 1 100); (3, Gen.int_range 30000 70_000)]

let on_frame c f = Events (Conn_state.on_frame c.cs ~live:c.live f)

(* A command [n] times: windtrap draws it [n] times as often. *)
let weighted n c = List.init n (fun _ -> c)

let new_stream (m : Model.t) id ~end_stream ~opened =
  let s =
    { Model.id
    ; phase= Active
    ; opened
    ; remote_ended= end_stream
    ; local_ended= false
    ; live= opened
    ; discarded= false
    ; rst_pending= false
    ; window= Model.iw
    ; held= 0
    ; send= m.peer_initial
    ; client_reset= false
    ; input_ended= false
    ; reset_seen= false }
  in
  m.streams <- s :: m.streams ;
  s

let stateful_commands =
  List.concat
    [ weighted 1
      @@ command "connect"
           (Gen.unit @-> makes conn)
           (fun () -> Model.create ())
           (fun () ->
             {cs= started ~local:stateful_local (); next_id= 1; live= 0} )
    ; (* 3: the streams open fast, so that the limit refuses some. *)
      weighted 3
      @@ command "open"
           ~pre:(fun m _ _ -> Model.alive m)
           (conn ^-> gap_gen @-> Gen.bool @-> judges outcome)
           (fun (m : Model.t) gap end_stream r ->
             let evs = events r in
             m.skipped <- List.init gap (fun i -> m.next + (2 * i)) @ m.skipped ;
             let id = m.next + (2 * gap) in
             m.next <- id + 2 ;
             let refused =
               Model.active m >= Model.mcs || Model.live m >= Model.mcs
             in
             match evs with
             | [Header_block {stream; block= ""; end_stream= e; use= Refused}]
               when refused && stream = id && e = end_stream ->
                 cover "a refused stream" true ;
                 let s = new_stream m id ~end_stream ~opened:false in
                 Model.close m s Reset_by_us ;
                 reset_event s
             | [Header_block {stream; block= ""; end_stream= e; use= Open}]
               when (not refused) && e = end_stream ->
                 (* The harness makes the ids, so a check that they grow cannot
                    fail here. The command "stale headers" covers the rule:
                    HEADERS on an id that is not new is an error. *)
                 equal ~msg:"the id" int id stream ;
                 let s = new_stream m id ~end_stream ~opened:true in
                 if end_stream then end_input s
             | _ ->
                 failf "refused %b, but the events %a" refused pp_outcome
                   (Events (Ok evs)) )
           (fun c gap end_stream ->
             let id = c.next_id + (2 * gap) in
             c.next_id <- id + 2 ;
             let r =
               Conn_state.on_frame c.cs ~live:c.live (headers ~end_stream id)
             in
             ( match r with
             | Ok [Header_block {use= Open; _}] ->
                 c.live <- c.live + 1
             | _ ->
                 () ) ;
             Events r )
    ; (* Six HEADERS frames at the limit: they are refused and push older ids
         out of the closed set (the path of the H1 fix of PR 4). 3: so that each
         run has DATA and trailers on such ids. *)
      weighted 3
      @@ command "refused burst"
           ~pre:(fun m _ ->
             Model.alive m
             && (Model.active m >= Model.mcs || Model.live m >= Model.mcs) )
           (conn ^-> Gen.unit @-> judges outcome)
           (fun (m : Model.t) () r ->
             let expected =
               List.init 6 (fun _ ->
                   let id = m.next in
                   m.next <- id + 2 ;
                   let s = new_stream m id ~end_stream:false ~opened:false in
                   Model.close m s Reset_by_us ;
                   reset_event s ;
                   block_text id Refused )
             in
             equal texts expected
               ( match got r with
               | Events r ->
                   show_result r
               | o ->
                   failf "not events: %a" pp_outcome o ) )
           (fun c () ->
             let rec burst acc n =
               if n = 0 then Events (Ok (List.rev acc))
               else begin
                 let id = c.next_id in
                 c.next_id <- id + 2 ;
                 match Conn_state.on_frame c.cs ~live:c.live (headers id) with
                 | Ok evs ->
                     burst (List.rev_append evs acc) (n - 1)
                 | Error _ as e ->
                     Events e
               end
             in
             burst [] 6 )
    ; (* A HEADERS frame on an id that the client skipped: the id is not new. *)
      weighted 1
      @@ command "stale headers"
           (conn ^-> skipped ^-> judges outcome)
           (fun (m : Model.t) _ r ->
             cover "a stale id" true ;
             equal texts
               (error_text Protocol_error)
               ( match got r with
               | Events r ->
                   show_result r
               | o ->
                   failf "not events: %a" pp_outcome o ) ;
             m.dead <- true )
           (fun c id -> on_frame c (headers ~end_stream:true id))
    ; (* 4: DATA fills the windows. *)
      weighted 4
      @@ command "data"
           ~pre:(fun (m : Model.t) id size pad _ ->
             let s = Model.find m id in
             size + pad <= s.window && size + pad <= m.conn_window )
           ( conn ^-> sendable ^-> size_gen @-> pad_gen @-> end_gen
           @-> judges outcome )
           (fun (m : Model.t) id size pad end_stream r ->
             let evs = events r in
             let s = Model.find m id in
             m.conn_window <- m.conn_window - (size + pad) ;
             s.window <- s.window - (size + pad) ;
             if end_stream then s.remote_ended <- true ;
             let rest = others m evs in
             match s.phase with
             | Active ->
                 equal texts
                   [data_text ~end_stream id (String.make size 'x')]
                   rest ;
                 if not s.discarded then s.held <- s.held + size ;
                 if end_stream then begin
                   end_input s ;
                   if s.local_ended then Model.close m s Ended
                 end
             | Reset_by_us | Ended ->
                 cover "DATA on a stream that we reset" true ;
                 (* The path of the H1 fix of PR 4: DATA in flight on an id that
                    refused ids pushed out of the closed set. *)
                 cover "DATA on an id that left the closed set"
                   (not (List.mem id m.closed_set)) ;
                 equal texts [] rest )
           (fun c id size pad end_stream ->
             on_frame c (data ~end_stream ~pad id (String.make size 'x')) )
    ; (* 2: trailers on old reset ids. *)
      weighted 2
      @@ command "trailers"
           (conn ^-> sendable ^-> judges outcome)
           (fun (m : Model.t) id r ->
             let rest =
               match got r with
               | Events (Error _ as e) ->
                   show_result e
               | _ ->
                   others m (events r)
             in
             let s = Model.find m id in
             s.remote_ended <- true ;
             match s.phase with
             | Active ->
                 equal texts [block_text ~end_stream:true id Trailers] rest ;
                 end_input s ;
                 if s.local_ended then Model.close m s Ended
             | (Reset_by_us | Ended) when List.mem id m.closed_set ->
                 equal texts [block_text ~end_stream:true id Ignored] rest
             | Reset_by_us | Ended ->
                 (* The id left the closed set: HEADERS on it is a connection
                    error (step 2 of the HEADERS order). *)
                 cover "trailers on an id that left the closed set" true ;
                 equal texts (error_text Protocol_error) rest ;
                 m.dead <- true )
           (fun c id -> on_frame c (headers ~end_stream:true id))
    ; weighted 1
      @@ command "reset"
           (conn ^-> resettable ^-> judges outcome)
           (fun (m : Model.t) id r ->
             let rest = others m (events r) in
             let s = Model.find m id in
             s.client_reset <- true ;
             match s.phase with
             | Active ->
                 equal texts [sprintf "peer reset %d" id] rest ;
                 reset_event s ;
                 Model.close m s Ended
             | Reset_by_us | Ended ->
                 equal texts [] rest )
           (fun c id -> on_frame c (rst id))
    ; weighted 1
      @@ command "window update"
           ~pre:(fun (m : Model.t) id increment ->
             let s = Model.find m id in
             s.phase <> Active || s.send + increment <= max_window )
           (conn ^-> any_stream ^-> increment_gen @-> judges outcome)
           (fun (m : Model.t) id increment r ->
             let rest = others m (events r) in
             let s = Model.find m id in
             match s.phase with
             | Active ->
                 equal texts ["credit"] rest ;
                 s.send <- s.send + increment
             | Reset_by_us | Ended ->
                 equal texts [] rest )
           (fun c id increment -> on_frame c (window_update id increment))
    ; weighted 1
      @@ command "connection window update"
           ~pre:(fun (m : Model.t) increment ->
             Model.alive m && m.conn_send + increment <= max_window )
           (conn ^-> increment_gen @-> judges outcome)
           (fun (m : Model.t) increment r ->
             equal texts ["credit"] (others m (events r)) ;
             m.conn_send <- m.conn_send + increment )
           (fun c increment -> on_frame c (window_update 0 increment))
    ; (* 2: some send windows go negative. *)
      weighted 2
      @@ command "settings"
           ~pre:(fun (m : Model.t) v ->
             Model.alive m
             && List.for_all
                  (fun (s : Model.stream) ->
                    s.phase <> Active
                    || s.send + v - m.peer_initial <= max_window )
                  m.streams )
           (conn ^-> window_gen @-> judges outcome)
           (fun (m : Model.t) v r ->
             let delta = v - m.peer_initial in
             equal texts
               (if delta = 0 then [ack] else [ack; "credit"])
               (others m (events r)) ;
             m.peer_initial <- v ;
             List.iter
               (fun (s : Model.stream) -> s.send <- s.send + delta)
               m.streams ;
             cover "a negative send window"
               (List.exists
                  (fun (s : Model.stream) -> s.phase = Active && s.send < 0)
                  m.streams ) )
           (fun c v -> on_frame c (settings [(0x4, v)]))
    ; weighted 1
      @@ command "ping"
           ~pre:(fun m _ -> Model.alive m)
           (conn ^-> Gen.int_range 0 99_999_999 @-> judges outcome)
           (fun (m : Model.t) n r ->
             let data = sprintf "%08d" n in
             equal texts
               [show_event (Send (Frame.Ping {ack= true; data}))]
               (others m (events r)) )
           (fun c n ->
             on_frame c (Frame.Ping {ack= false; data= sprintf "%08d" n}) )
    ; (* 3: the handlers read, so that the credit reaches half. *)
      weighted 3
      @@ command "consume"
           (conn ^-> holding ^-> consume_gen @-> judges outcome)
           (fun (m : Model.t) id n r ->
             let fs = frames r in
             let s = Model.find m id in
             s.held <- s.held - min n s.held ;
             List.iter (apply_update m) fs )
           (fun c id n -> Frames (Conn_state.consumed c.cs ~stream:id n))
    ; (* 2: the server sends, so that the send windows go down. *)
      weighted 2
      @@ command "send"
           ~pre:(fun (m : Model.t) id n ->
             let s = Model.find m id in
             n <= min m.conn_send s.send )
           (conn ^-> responding ^-> send_gen @-> judges outcome)
           (fun (m : Model.t) id n r ->
             equal outcome Done (got r) ;
             let s = Model.find m id in
             m.conn_send <- m.conn_send - n ;
             s.send <- s.send - n )
           (fun c id n ->
             Conn_state.sent c.cs ~stream:id n ;
             Done )
    ; weighted 1
      @@ command "respond"
           (conn ^-> responding ^-> judges outcome)
           (fun (m : Model.t) id r ->
             equal outcome Done (got r) ;
             let s = Model.find m id in
             s.local_ended <- true ;
             if s.remote_ended then Model.close m s Ended )
           (fun c id ->
             Conn_state.end_local c.cs ~stream:id ;
             Done )
    ; (* 2: the fibers end, so that new streams can open. *)
      weighted 2
      @@ command "exit"
           (conn ^-> living ^-> judges outcome)
           (fun (m : Model.t) id r ->
             match got r with
             | Exit (reset, fs) ->
                 let s = Model.find m id in
                 equal (option frame)
                   ( if s.phase = Active then
                       Some (Frame.Rst_stream {stream= id; code= Error.No_error})
                     else None )
                   reset ;
                 if s.phase = Active then Model.close m s Reset_by_us ;
                 s.held <- 0 ;
                 s.live <- false ;
                 s.discarded <- true ;
                 List.iter (apply_update m) fs
             | o ->
                 failf "not an exit: %a" pp_outcome o )
           (fun c id ->
             let reset =
               Conn_state.reset_local c.cs ~stream:id Error.No_error
             in
             let fs = Conn_state.discard c.cs ~stream:id in
             c.live <- c.live - 1 ;
             Exit (reset, fs) )
    ; (* The fiber sends its END_STREAM and ends. The stream can stay active,
         with no reader, until the writer takes the RST_STREAM. *)
      weighted 1
      @@ command "end and exit"
           (conn ^-> responding ^-> judges outcome)
           (fun (m : Model.t) id r ->
             let fs = frames r in
             let s = Model.find m id in
             s.local_ended <- true ;
             if s.remote_ended then Model.close m s Ended
             else begin
               cover "an active stream after discard" true ;
               s.rst_pending <- true
             end ;
             s.held <- 0 ;
             s.live <- false ;
             s.discarded <- true ;
             List.iter (apply_update m) fs )
           (fun c id ->
             Conn_state.end_local c.cs ~stream:id ;
             let fs = Conn_state.discard c.cs ~stream:id in
             c.live <- c.live - 1 ;
             Frames fs )
    ; (* The writer takes the RST_STREAM NO_ERROR after our END_STREAM. *)
      weighted 1
      @@ command "writer resets"
           (conn ^-> pending ^-> judges outcome)
           (fun (m : Model.t) id r ->
             let s = Model.find m id in
             equal outcome
               (Exit
                  ( ( if s.phase = Active then
                        Some
                          (Frame.Rst_stream {stream= id; code= Error.No_error})
                      else None )
                  , [] ) )
               (got r) ;
             if s.phase = Active then Model.close m s Reset_by_us ;
             s.rst_pending <- false )
           (fun c id ->
             Exit (Conn_state.reset_local c.cs ~stream:id Error.No_error, []) )
    ]

let conn_state_tests =
  group "Conn_state"
    ( conn_state_rows @ conn_state_flows
    @ [ stateful ~count:300 ~steps:120 "behaves like the model of the client"
          stateful_commands ] )

(* {1 Messages} *)

module Message = Caravan_http2.Private.Message
module Request = Caravan.Request
module Response = Caravan.Response
module Headers = Caravan.Headers
module Status = Caravan.Status
module Method = Caravan.Method
module Body = Caravan.Body

let show_fields l =
  String.concat "; " (List.map (fun (n, v) -> sprintf "%s: %S" n v) l)

let show_head (h : Message.request_head) =
  sprintf "%s %S authority=%s [%s] content_length=%s expect=%s"
    (Method.to_string h.meth) h.path
    (Option.value ~default:"none" h.authority)
    (show_fields (Headers.to_list h.headers))
    (Option.fold ~none:"none" ~some:string_of_int h.content_length)
    ( match h.expect with
    | `None ->
        "none"
    | `Continue ->
        "continue"
    | `Unknown ->
        "unknown" )

(* The request that the engine makes from [h] (plan section 16). *)
let request_of_head (h : Message.request_head) =
  Request.make ~version:Caravan.Version.Http_2 ~scheme:"https"
    ?authority:h.authority ~headers:h.headers h.meth h.path

let show_request fields =
  match Message.request fields with
  | Ok h ->
      show_head h
  | Error (Malformed reason) ->
      "malformed: " ^ reason
  | Error Misdirected ->
      "misdirected"

(* The outcomes of [cases], one line for each: the name, then the outcome. *)
let lines show cases =
  String.concat "\n" (List.map (fun (name, x) -> name ^ ": " ^ show x) cases)

let get =
  [ (":method", "GET")
  ; (":scheme", "https")
  ; (":authority", "a.example")
  ; (":path", "/") ]

(* [fields] with the value [v] for the pseudo-field [name], or without it if [v]
   is [None]. *)
let with_pseudo name v fields =
  List.filter_map
    (fun (n, old) ->
      if n <> name then Some (n, old) else Option.map (fun v -> (n, v)) v )
    fields

let connect = [(":method", "CONNECT"); (":authority", "a.example:443")]

let show_plan = function
  | Message.Send (fields, body) ->
      sprintf "send [%s] %s" (show_fields fields)
        ( match body with
        | No_body ->
            "no body"
        | Length n ->
            sprintf "length %d" n
        | Unknown_length ->
            "unknown length" )
  | Fail_500 reason ->
      "500: " ^ reason

let show_response ?(meth = Method.GET) ?date r =
  show_plan (Message.response ~meth ~date r)

let h = Headers.of_list

(* The bytes of the generators: each byte 0 to 255, more often the visible ASCII
   bytes, and more often the bytes that the rules look at. *)
let byte_gen =
  Gen.(
    frequency
      [ (1, char)
      ; (3, char_range '!' '~')
      ; ( 1
        , of_list [' '; '\t'; '\x7f'; '\x00'; 'A'; 'a'; ':'; '/'; '@'; '*'; '5']
        ) ] )

let bytes_gen = Gen.(string_of ~size:(int_range 0 4) byte_gen)

(* A value from [common], or bytes. *)
let value_gen common = Gen.(frequency [(4, of_list common); (1, bytes_gen)])

(* A regular field. The names that a rule refuses are less frequent. *)
let regular_gen =
  Gen.(
    pair
      (frequency
         [ ( 6
           , value_gen
               ["host"; "cookie"; "content-length"; "expect"; "accept"; "x-a"]
           )
         ; (1, of_list ["te"; "connection"; "Accept"]) ] )
      (value_gen ["a.example"; "5"; "trailers"; "100-continue"; "x=1"]) )

(* A list of fields: the four pseudo-fields, each one at times absent, then the
   regular fields. At times a pseudo-field more comes first or last. Thus each
   rule of [request] can refuse a list, and many lists get to the end of the
   rules. *)
let fields_gen =
  Gen.(
    with_pp
      (fun ppf l ->
        List.iter (fun (n, v) -> Format.fprintf ppf "(%S, %S) " n v) l )
      (let+ meth = option (value_gen ["GET"; "POST"; "CONNECT"; "OPTIONS"])
       and+ scheme = option (value_gen ["http"; "https"])
       and+ authority = option (value_gen ["a.example"; "a.example:80"])
       and+ path =
         option
           (one_of
              [ of_list ["/"; "*"; "/a?b"]
              ; map (fun s -> "/" ^ s) bytes_gen
              ; bytes_gen ] )
       and+ regular = list ~size:(int_range 0 3) regular_gen
       and+ extra =
         frequency
           [ (8, constant [])
           ; ( 1
             , map
                 (fun f -> [f])
                 (pair (of_list [":status"; ":x"; ":path"]) bytes_gen) ) ]
       and+ extra_first = bool in
       let pseudo =
         List.filter_map
           (fun (name, v) -> Option.map (fun v -> (name, v)) v)
           [ (":method", meth)
           ; (":scheme", scheme)
           ; (":authority", authority)
           ; (":path", path) ]
       in
       if extra_first then extra @ pseudo @ regular
       else pseudo @ regular @ extra ) )

let message_tests =
  group "Message"
    [ (* Test 1 of plan section 5: one row for each rule of Malformed. *)
      test "the malformed requests" (fun () ->
          expect
            (lines show_request
               [ ( "pseudo after regular"
                 , [ (":method", "GET")
                   ; (":scheme", "https")
                   ; ("accept", "*/*")
                   ; (":path", "/") ] )
               ; ("unknown pseudo", (":foo", "x") :: get)
               ; (":status", (":status", "200") :: get)
               ; ("two :method", (":method", "GET") :: get)
               ; ("two :scheme", (":scheme", "https") :: get)
               ; ("two :authority", (":authority", "a.example") :: get)
               ; ("two :path", (":path", "/") :: get)
               ; ("uppercase name", get @ [("Accept", "*/*")])
               ; ("name not a token", get @ [("a b", "x")])
               ; ("empty name", get @ [("", "x")])
               ; ("NUL in a value", get @ [("x", "a\x00b")])
               ; ("SP at the start", get @ [("x", " a")])
               ; ("HTAB at the end", get @ [("x", "a\t")])
               ; ("invalid pseudo value", with_pseudo ":path" (Some "/\x00") get)
               ; ("connection", get @ [("connection", "close")])
               ; ("keep-alive", get @ [("keep-alive", "5")])
               ; ("proxy-connection", get @ [("proxy-connection", "x")])
               ; ("transfer-encoding", get @ [("transfer-encoding", "chunked")])
               ; ("upgrade", get @ [("upgrade", "h2c")])
               ; ("te gzip", get @ [("te", "trailers, gzip")])
               ; ("no :method", with_pseudo ":method" None get)
               ; ("no :scheme", with_pseudo ":scheme" None get)
               ; ("no :path", with_pseudo ":path" None get)
               ; ("empty :path", with_pseudo ":path" (Some "") get)
               ; ("relative :path", with_pseudo ":path" (Some "a") get)
               ; ("* for GET", with_pseudo ":path" (Some "*") get)
               ; ("CONNECT with :scheme", connect @ [(":scheme", "https")])
               ; ("CONNECT with :path", connect @ [(":path", "/")])
               ; ("CONNECT, no :authority", [(":method", "CONNECT")])
               ; ( "CONNECT, host only"
                 , [(":method", "CONNECT"); ("host", "a.example")] )
               ; ("userinfo", with_pseudo ":authority" (Some "u@a.example") get)
               ; ("empty :authority", with_pseudo ":authority" (Some "") get)
               ; ( "invalid host"
                 , with_pseudo ":authority" None get @ [("host", "a/b")] )
               ; ("host differs", get @ [("host", "b.example")])
               ; ( "two hosts differ"
                 , with_pseudo ":authority" None get
                   @ [("host", "a.example"); ("host", "b.example")] )
               ; ("content-length 1, 2", get @ [("content-length", "1, 2")])
               ; ( "content-length 1 and 2"
                 , get @ [("content-length", "1"); ("content-length", "2")] )
               ; ("content-length x", get @ [("content-length", "x")])
               ; ( "content-length 19 digits"
                 , get @ [("content-length", String.make 19 '1')] )
               ; ("content-length empty", get @ [("content-length", "")])
               ; ("method not a token", with_pseudo ":method" (Some "G T") get)
               ; ("SP in :path", with_pseudo ":path" (Some "/a b") get)
               ; ("HTAB in :path", with_pseudo ":path" (Some "/a\tb") get)
               ; ("DEL in :path", with_pseudo ":path" (Some "/a\x7f") get)
               ; ( "byte 0xc3 in :path"
                 , with_pseudo ":path" (Some "/\xc3\xa9") get )
               ; ( "CONNECT, no port"
                 , [(":method", "CONNECT"); (":authority", "a.example")] )
               ; ( "CONNECT, empty port"
                 , [(":method", "CONNECT"); (":authority", "a.example:")] )
               ; ( "empty host, then a host"
                 , with_pseudo ":authority" None get
                   @ [("host", ""); ("host", "a.example")] )
               ; ("empty host and :authority", get @ [("host", "")]) ] )
          @@ __POS_OF__
               {|
            pseudo after regular: malformed: a pseudo-field ":path" after a regular field
            unknown pseudo: malformed: an unknown pseudo-field ":foo"
            :status: malformed: a :status field in a request
            two :method: malformed: two :method fields
            two :scheme: malformed: two :scheme fields
            two :authority: malformed: two :authority fields
            two :path: malformed: two :path fields
            uppercase name: malformed: an uppercase field name "Accept"
            name not a token: malformed: an invalid field name or value
            empty name: malformed: an invalid field name or value
            NUL in a value: malformed: an invalid field name or value
            SP at the start: malformed: an invalid field name or value
            HTAB at the end: malformed: an invalid field name or value
            invalid pseudo value: malformed: an invalid value of ":path"
            connection: malformed: a connection-specific field "connection"
            keep-alive: malformed: a connection-specific field "keep-alive"
            proxy-connection: malformed: a connection-specific field "proxy-connection"
            transfer-encoding: malformed: a connection-specific field "transfer-encoding"
            upgrade: malformed: a connection-specific field "upgrade"
            te gzip: malformed: a te field other than trailers
            no :method: malformed: no :method
            no :scheme: malformed: no :scheme
            no :path: malformed: no :path
            empty :path: malformed: an empty :path
            relative :path: malformed: a :path that does not start with /
            * for GET: malformed: a :path that does not start with /
            CONNECT with :scheme: malformed: a CONNECT with :scheme
            CONNECT with :path: malformed: a CONNECT with :path
            CONNECT, no :authority: malformed: a CONNECT with no :authority
            CONNECT, host only: malformed: a CONNECT with no :authority
            userinfo: malformed: an invalid authority
            empty :authority: malformed: an invalid authority
            invalid host: malformed: an invalid authority
            host differs: malformed: a host field that is not the authority
            two hosts differ: malformed: a host field that is not the authority
            content-length 1, 2: malformed: an invalid content-length
            content-length 1 and 2: malformed: an invalid content-length
            content-length x: malformed: an invalid content-length
            content-length 19 digits: malformed: an invalid content-length
            content-length empty: malformed: an invalid content-length
            method not a token: malformed: a :method that is not a token
            SP in :path: malformed: a :path with a byte that is not visible ASCII
            HTAB in :path: malformed: a :path with a byte that is not visible ASCII
            DEL in :path: malformed: an invalid value of ":path"
            byte 0xc3 in :path: malformed: a :path with a byte that is not visible ASCII
            CONNECT, no port: malformed: a CONNECT :authority that is not host:port
            CONNECT, empty port: malformed: a CONNECT :authority that is not host:port
            empty host, then a host: malformed: a host field that is not the authority
            empty host and :authority: malformed: a host field that is not the authority
            |} )
    ; (* Test 2. *)
      test "valid requests" (fun () ->
          let show fields =
            match Message.request fields with
            | Ok head ->
                sprintf "%s; cookies [%s]" (show_head head)
                  (show_fields (Request.cookies (request_of_head head)))
            | Error _ ->
                show_request fields
          in
          expect
            (lines show
               [ ("GET", get)
               ; ("GET, host and :authority", get @ [("host", "A.example")])
               ; ( "GET, host only"
                 , with_pseudo ":authority" None get
                   @ [("host", "a.example:80"); ("host", "a.example:80")] )
               ; ("GET, no authority", with_pseudo ":authority" None get)
               ; ("CONNECT", connect)
               ; ( "OPTIONS *"
                 , with_pseudo ":method" (Some "OPTIONS")
                     (with_pseudo ":path" (Some "*") get) )
               ; ( "two cookie fields"
                 , get @ [("cookie", "a=1; b=2"); ("x", "y"); ("cookie", "c=3")]
                 )
               ; ( "empty cookie values"
                 , get @ [("cookie", ""); ("cookie", "a=1"); ("cookie", "")] )
               ; ("one cookie field", get @ [("cookie", "a=1")])
               ; ( "POST with a length"
                 , with_pseudo ":method" (Some "POST") get
                   @ [("content-length", "5, 5"); ("te", "Trailers")] )
               ; ("expect", get @ [("expect", "100-Continue")])
               ; ("other expect", get @ [("expect", "x")])
               ; ( "two expect"
                 , get @ [("expect", "100-continue"); ("expect", "100-continue")]
                 )
               ; ( "expect list"
                 , get @ [("expect", "100-continue, 100-continue")] )
               ; ("expect list, other", get @ [("expect", "100-continue, x")])
               ; ("empty expect", get @ [("expect", "")])
               ; ( "empty host only"
                 , with_pseudo ":authority" None get @ [("host", "")] )
               ; ( "two empty hosts"
                 , with_pseudo ":authority" None get
                   @ [("host", ""); ("host", "")] )
               ; ( "IPv6 authority"
                 , with_pseudo ":authority" (Some "[::1]:8443") get )
               ; ("extension method", with_pseudo ":method" (Some "PURGE") get)
               ; ("value with inner SP", get @ [("x", "a b")]) ] )
          @@ __POS_OF__
               {|
            GET: GET "/" authority=a.example [] content_length=none expect=none; cookies []
            GET, host and :authority: GET "/" authority=a.example [host: "A.example"] content_length=none expect=none; cookies []
            GET, host only: GET "/" authority=a.example:80 [host: "a.example:80"; host: "a.example:80"] content_length=none expect=none; cookies []
            GET, no authority: GET "/" authority=none [] content_length=none expect=none; cookies []
            CONNECT: CONNECT "" authority=a.example:443 [] content_length=none expect=none; cookies []
            OPTIONS *: OPTIONS "*" authority=a.example [] content_length=none expect=none; cookies []
            two cookie fields: GET "/" authority=a.example [cookie: "a=1; b=2; c=3"; x: "y"] content_length=none expect=none; cookies [a: "1"; b: "2"; c: "3"]
            empty cookie values: GET "/" authority=a.example [cookie: "a=1"] content_length=none expect=none; cookies [a: "1"]
            one cookie field: GET "/" authority=a.example [cookie: "a=1"] content_length=none expect=none; cookies [a: "1"]
            POST with a length: POST "/" authority=a.example [content-length: "5, 5"; te: "Trailers"] content_length=5 expect=none; cookies []
            expect: GET "/" authority=a.example [expect: "100-Continue"] content_length=none expect=continue; cookies []
            other expect: GET "/" authority=a.example [expect: "x"] content_length=none expect=unknown; cookies []
            two expect: GET "/" authority=a.example [expect: "100-continue"; expect: "100-continue"] content_length=none expect=continue; cookies []
            expect list: GET "/" authority=a.example [expect: "100-continue, 100-continue"] content_length=none expect=continue; cookies []
            expect list, other: GET "/" authority=a.example [expect: "100-continue, x"] content_length=none expect=unknown; cookies []
            empty expect: GET "/" authority=a.example [expect: ""] content_length=none expect=none; cookies []
            empty host only: GET "/" authority=none [host: ""] content_length=none expect=none; cookies []
            two empty hosts: GET "/" authority=none [host: ""; host: ""] content_length=none expect=none; cookies []
            IPv6 authority: GET "/" authority=[::1]:8443 [] content_length=none expect=none; cookies []
            extension method: PURGE "/" authority=a.example [] content_length=none expect=none; cookies []
            value with inner SP: GET "/" authority=a.example [x: "a b"] content_length=none expect=none; cookies []
            |} )
    ; (* Test 3. *)
      test "the scheme of a request" (fun () ->
          expect
            (lines show_request
               [ ("https", get)
               ; ("http", with_pseudo ":scheme" (Some "http") get)
               ; ("HTTPS", with_pseudo ":scheme" (Some "HTTPS") get)
               ; ("foo", with_pseudo ":scheme" (Some "foo") get)
               ; ( "foo and malformed"
                 , with_pseudo ":scheme" (Some "foo") get @ [("connection", "x")]
                 ) ] )
          @@ __POS_OF__
               {|
            https: GET "/" authority=a.example [] content_length=none expect=none
            http: GET "/" authority=a.example [] content_length=none expect=none
            HTTPS: GET "/" authority=a.example [] content_length=none expect=none
            foo: misdirected
            foo and malformed: malformed: a connection-specific field "connection"
            |} )
    ; (* Test 4. *)
      test "trailers" (fun () ->
          let show fields =
            match Message.trailers fields with
            | Ok t ->
                sprintf "[%s]" (show_fields (Headers.to_list t))
            | Error reason ->
                "error: " ^ reason
          in
          expect
            (lines show
               [ ("valid", [("x-checksum", "abc"); ("te", "trailers")])
               ; ("empty", [])
               ; ("pseudo-field", [(":path", "/")])
               ; ("connection", [("connection", "close")])
               ; ("uppercase name", [("X-Checksum", "abc")])
               ; ("name not a token", [("a b", "x")])
               ; ("invalid value", [("x", "a\nb")])
               ; ("te gzip", [("te", "gzip")]) ] )
          @@ __POS_OF__
               {|
            valid: [x-checksum: "abc"; te: "trailers"]
            empty: []
            pseudo-field: error: a pseudo-field ":path" in trailers
            connection: error: a connection-specific field "connection"
            uppercase name: error: an uppercase field name "X-Checksum"
            name not a token: error: an invalid field name or value
            invalid value: error: an invalid field name or value
            te gzip: error: a te field other than trailers
            |} )
    ; (* Test 5. *)
      test "responses" (fun () ->
          let ok = Response.string "hello" in
          let fields =
            h
              [ ("Connection", "X-Foo, keep-alive")
              ; ("X-Foo", "1")
              ; ("X-Bar", "2")
              ; ("Keep-Alive", "5")
              ; ("Transfer-Encoding", "chunked")
              ; ("Upgrade", "h2c")
              ; ("TE", "trailers")
              ; ("Proxy-Connection", "x")
              ; ("Content-Length", "99")
              ; ("Set-Cookie", "a=1")
              ; ("Set-Cookie", "b=2") ]
          in
          let unknown = Body.of_writer (fun _ -> ()) in
          let content_fields =
            h
              [ ("Content-Type", "text/html")
              ; ("Content-Encoding", "gzip")
              ; ("Content-Language", "en")
              ; ("Content-Range", "bytes 0-1/2")
              ; ("ETag", "\"x\"") ]
          in
          expect
            (lines
               (fun f -> f ())
               [ ("200", fun () -> show_response ok)
               ; ("200, date", fun () -> show_response ~date:"D" ok)
               ; ( "200, own date"
                 , fun () ->
                     show_response ~date:"D"
                       (Response.string ~headers:(h [("Date", "E")]) "x") )
               ; ( "Connection: date"
                 , fun () ->
                     show_response ~date:"D"
                       (Response.string
                          ~headers:(h [("Connection", "Date"); ("Date", "E")])
                          "x" ) )
               ; ( "unknown length"
                 , fun () -> show_response (Response.make unknown) )
               ; ("HEAD", fun () -> show_response ~meth:HEAD ok)
               ; ( "HEAD, unknown length"
                 , fun () ->
                     show_response ~meth:HEAD
                       (Response.make
                          ~headers:(h [("Content-Length", "7")])
                          unknown ) )
               ; ("204", fun () -> show_response (Response.empty ()))
               ; ( "304"
                 , fun () ->
                     show_response
                       (Response.make ~status:(Status.v 304)
                          ~headers:content_fields (Body.of_string "x") ) )
               ; ( "200, content fields"
                 , fun () ->
                     show_response
                       (Response.make ~headers:content_fields Body.empty) )
               ; ( "205"
                 , fun () ->
                     show_response (Response.string ~status:(Status.v 205) "x")
                 )
               ; ("CONNECT 200", fun () -> show_response ~meth:CONNECT ok)
               ; ( "CONNECT 404"
                 , fun () ->
                     show_response ~meth:CONNECT
                       (Response.string ~status:(Status.v 404) "no") )
               ; ("upgrade", fun () -> show_response (Response.upgrade ignore))
               ; ( "CONNECT upgrade"
                 , fun () ->
                     show_response ~meth:CONNECT
                       (Response.upgrade ~status:Status.ok ignore) )
               ; ( "fields"
                 , fun () ->
                     show_response (Response.make ~headers:fields Body.empty) )
               ; ( "no connection field"
                 , fun () ->
                     show_response
                       (Response.make
                          ~headers:(h [("Keep-Alive", "5"); ("X-A", "1")])
                          Body.empty ) ) ] )
          @@ __POS_OF__
               {|
            200: send [:status: "200"; content-type: "text/plain; charset=utf-8"; content-length: "5"] length 5
            200, date: send [:status: "200"; date: "D"; content-type: "text/plain; charset=utf-8"; content-length: "5"] length 5
            200, own date: send [:status: "200"; date: "E"; content-type: "text/plain; charset=utf-8"; content-length: "1"] length 1
            Connection: date: send [:status: "200"; date: "D"; content-type: "text/plain; charset=utf-8"; content-length: "1"] length 1
            unknown length: send [:status: "200"] unknown length
            HEAD: send [:status: "200"; content-type: "text/plain; charset=utf-8"; content-length: "5"] no body
            HEAD, unknown length: send [:status: "200"; content-length: "7"] no body
            204: send [:status: "204"] no body
            304: send [:status: "304"; etag: "\"x\""] no body
            200, content fields: send [:status: "200"; content-type: "text/html"; content-encoding: "gzip"; content-language: "en"; content-range: "bytes 0-1/2"; etag: "\"x\""; content-length: "0"] length 0
            205: send [:status: "205"; content-type: "text/plain; charset=utf-8"; content-length: "0"] no body
            CONNECT 200: 500: a 2xx response to CONNECT: the HTTP/2 engine has no tunnels
            CONNECT 404: send [:status: "404"; content-type: "text/plain; charset=utf-8"; content-length: "2"] length 2
            upgrade: 500: an upgrade response: the HTTP/2 engine has no upgrades
            CONNECT upgrade: 500: an upgrade response: the HTTP/2 engine has no upgrades
            fields: send [:status: "200"; x-bar: "2"; set-cookie: "a=1"; set-cookie: "b=2"; content-length: "0"] length 0
            no connection field: send [:status: "200"; x-a: "1"; content-length: "0"] length 0
            |} )
    ; test "interim responses" (fun () ->
          equal
            (list (pair string string))
            [(":status", "100")]
            (Message.interim 100) ;
          equal
            (list (pair string string))
            [(":status", "199")]
            (Message.interim 199) ;
          List.iter
            (fun code ->
              raises
                (Invalid_argument (sprintf "Message.interim: %d" code))
                (fun () -> Message.interim code) )
            [99; 101; 200] )
    ; (* Test 6. *)
      prop ~count:1000
        "request never raises, and an accepted head makes a request" fields_gen
        (fun fields ->
          match Message.request fields with
          | Ok head ->
              ignore (request_of_head head) ;
              (* The join of the cookie fields keeps the fields valid. *)
              ignore (Headers.of_list (Headers.to_list head.headers))
          | Error _ ->
              () ) ]

let () =
  exit
    (Windtrap.run "http2"
       [error_tests; frame_tests; conn_state_tests; message_tests] )
