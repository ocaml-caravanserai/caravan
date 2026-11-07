(* The tests of the HTTP/2 engine over TCP pairs of the simulator, and the tests
   of the checks of the fake client that they use. Each test runs 1 seed on 1
   domain, except a test whose name starts with "race:". *)

open Windtrap
open Weft
open Caravan_http2.Private
module Flow = Caravan.Flow
module Tcp = Caravan.Tcp
module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer
module Fake = Fake_h2_client
module Settings = Caravan_http2.Settings

let show = H2_support.show

let frames_text l = String.concat "\n" (List.map show l)

let failed text = Sim_support.Failed text

(* The identifier of SETTINGS_INITIAL_WINDOW_SIZE. *)
let initial_window_size = 0x4

(* {1 The fake client} *)

(* The server side of a test of the checks: the test writes the frames of the
   server by hand. *)
type peer = {r: Reader.t; w: Writer.t; flow: Flow.t}

let peer_write p frames =
  List.iter (Frame.write p.w) frames ;
  Writer.flush p.w

let peer_raw p s = Writer.write_string p.w s ; Writer.flush p.w

let peer_read p = Frame.read p.r ~max_size:16384

(* [with_peer sim ~settings ~server f] connects a fake client with [settings] to
   a peer that the test drives. The peer reads the preface, the SETTINGS frame
   and the PING of the client. It sends a SETTINGS frame with [server] (and a
   WINDOW_UPDATE frame of [update], if any), the ACK of the client settings and
   the PING ACK, and reads the ACK of the client. Then [f c p ~start] runs.
   [start] is the frames of the client that the peer read. At the end of [f],
   the client and the peer end their streams, and [Fake.finish] raises the
   failure of a check, if any. *)
let with_peer ?(settings = []) ?(server = []) ?update sim f =
  Sim_support.with_pair sim (fun client socket ->
      let flow = Tcp.flow socket in
      let p = {r= Reader.create flow; w= Writer.create flow; flow} in
      Fiber.scope (fun s ->
          let start =
            Sim_support.call ~name:"peer" s (fun () ->
                Reader.ensure p.r 24 ;
                let preface = Reader.sub_string p.r 0 24 in
                Reader.consume p.r 24 ;
                let first = peer_read p in
                let ping = peer_read p in
                let ping_data =
                  match ping with Frame.Ping {data; _} -> data | _ -> ""
                in
                peer_write p
                  ( Frame.Settings {ack= false; settings= server}
                    :: Option.to_list
                         (Option.map
                            (fun increment ->
                              Frame.Window_update {stream= 0; increment} )
                            update )
                  @ [ Frame.Settings {ack= true; settings= []}
                    ; Frame.Ping {ack= true; data= ping_data} ] ) ;
                let ack = peer_read p in
                (preface = Frame.preface, [first; ping; ack]) )
          in
          let c = Fake.connect ~settings s (Tcp.flow client) in
          let preface_ok, start = Sim_support.await start in
          equal Testable.bool true preface_ok ;
          let v = f c p ~start in
          Fake.close c ;
          (try Flow.shutdown_send p.flow with Flow.Closed -> ()) ;
          Fake.finish c ; v ) )

(* [fails ~settings text f] makes sure that a run of [with_peer] with [f] raises
   the failure [text]. *)
let fails ?settings ?server text f =
  raises (failed text) (fun () ->
      Sim_support.run (fun sim -> with_peer ?settings ?server sim f) )

(* [fails_with ~settings ~setup frames text] sends [frames] from the peer after
   [setup c], and makes sure that the client fails with [text]. *)
let fails_with ?settings ?(setup = fun _ -> ()) frames text =
  fails ?settings text (fun c p ~start:_ -> setup c ; peer_write p frames)

let get = [(":method", "GET"); (":scheme", "http"); (":path", "/")]

(* Stream 1, which the client does not end: a RST of the server after its
   END_STREAM is legal there. *)
let open_1 c = Fake.request ~end_stream:false c ~stream:1 get

let data ?(end_stream = false) stream s =
  Frame.Data {stream; end_stream; data= s; flow_length= String.length s}

let headers ?(end_stream = false) ?(end_headers = true) stream fields =
  let buf = Buffer.create 64 in
  Hpack.encode buf fields ;
  Frame.Headers
    {stream; end_stream; end_headers; priority= None; block= Buffer.contents buf}

let ping data = Frame.Ping {ack= false; data}

(* [plain fields] is the block of [fields], with no table size update at its
   start: the encoder starts each block with one (the byte 0x20). *)
let plain fields =
  let b = Buffer.create 64 in
  Hpack.encode b fields ;
  let b = Buffer.contents b in
  String.sub b 1 (String.length b - 1)

(* [ok f] runs [with_peer] with [f], and makes sure that no check fails. *)
let ok ?settings ?server ?update f =
  Sim_support.run (fun sim -> with_peer ?settings ?server ?update sim f)

let over_stream n w =
  Printf.sprintf "DATA of %d bytes on stream 1 over the stream window of %d" n w

let fake_client_tests =
  group "Fake_h2_client"
    [ test "connect: the preface, the SETTINGS frames, the ACK" (fun () ->
          ok
            ~settings:[(initial_window_size, 1000)]
            ~server:[(0x3, 100); (initial_window_size, 65535)]
            ~update:((100 * 65535) - 65535)
            (fun c p ~start ->
              equal Testable.string
                "SETTINGS [0x4=1000]\nPING \"start000\"\nSETTINGS ACK []"
                (frames_text start) ;
              equal
                Testable.(list (pair int int))
                [(0x3, 100); (initial_window_size, 65535)]
                (Fake.server_settings c) ;
              (* The WINDOW_UPDATE frame of the start is not in the mailbox. *)
              peer_write p [ping "12345678"] ;
              equal Testable.string {|PING "12345678"|} (show (Fake.next c)) ) )
    ; test "a first frame of the server other than SETTINGS fails" (fun () ->
          raises
            (failed
               {|the first frame of the server is not SETTINGS: PING "12345678"|} )
            (fun () ->
              Sim_support.run (fun sim ->
                  Sim_support.with_pair sim (fun client socket ->
                      let flow = Tcp.flow socket in
                      let p =
                        {r= Reader.create flow; w= Writer.create flow; flow}
                      in
                      peer_write p
                        [ ping "12345678"
                        ; Frame.Settings {ack= false; settings= []} ] ;
                      Fiber.scope (fun s ->
                          ignore (Fake.connect s (Tcp.flow client)) ) ) ) ) )
    ; test
        "a RST_STREAM frame on a stream that ended in the two directions fails"
        (fun () ->
          fails_with
            ~setup:(fun c -> Fake.request c ~stream:1 get)
            [data ~end_stream:true 1 "a"; Rst_stream {stream= 1; code= No_error}]
            "a RST_STREAM frame on stream 1, which ended in the two directions" )
    ; test "a DATA frame over the stream window fails" (fun () ->
          fails
            ~settings:[(initial_window_size, 10)]
            (over_stream 1 0)
            (fun c p ~start:_ ->
              open_1 c ;
              peer_write p [data 1 "0123456789"] ;
              equal Testable.string {|DATA 1 "0123456789" flow_length=10|}
                (show (Fake.next c)) ;
              peer_write p [data 1 "a"] ) )
    ; test "a DATA frame over the connection window fails" (fun () ->
          fails_with
            ~settings:[(initial_window_size, 100_000)]
            ~setup:open_1
            (List.init 4 (fun _ -> data 1 (String.make 16384 'x')))
            "DATA of 16384 bytes on stream 1 over the connection window of \
             16383" )
    ; test "a WINDOW_UPDATE frame of the client adds to its windows" (fun () ->
          fails
            ~settings:[(initial_window_size, 10)]
            (over_stream 1 0)
            (fun c p ~start:_ ->
              open_1 c ;
              Fake.credit c ~stream:1 5 ;
              peer_write p [data 1 (String.make 15 'x')] ;
              ignore (Fake.next c) ;
              peer_write p [data 1 "a"] ) )
    ; test "a smaller initial window applies at the SETTINGS ACK" (fun () ->
          fails
            ~settings:[(initial_window_size, 100)]
            (over_stream 1 (-40))
            (fun c p ~start:_ ->
              open_1 c ;
              ignore (peer_read p) ;
              Fake.send c
                (Settings {ack= false; settings= [(initial_window_size, 10)]}) ;
              ignore (peer_read p) ;
              (* Before the ACK, the old window applies. *)
              peer_write p [data 1 (String.make 50 'x')] ;
              ignore (Fake.next c) ;
              peer_write p [Settings {ack= true; settings= []}; data 1 "a"] ) )
    ; test "a larger initial window applies at once" (fun () ->
          fails
            ~settings:[(initial_window_size, 10)]
            (over_stream 1 0)
            (fun c p ~start:_ ->
              open_1 c ;
              Fake.send c
                (Settings {ack= false; settings= [(initial_window_size, 30)]}) ;
              peer_write p [data 1 (String.make 30 'x')] ;
              ignore (Fake.next c) ;
              peer_write p [data 1 "a"] ) )
    ; test "a frame in a header block fails" (fun () ->
          fails_with ~setup:open_1
            [headers ~end_headers:false 1 [(":status", "200")]; data 1 "a"]
            {|a frame in the header block of stream 1: DATA 1 "a" flow_length=1|} )
    ; test "a CONTINUATION frame of another stream in a block fails" (fun () ->
          fails_with
            ~setup:(fun c ->
              open_1 c ;
              Fake.request c ~stream:3 get )
            [ headers ~end_headers:false 1 [(":status", "200")]
            ; Continuation {stream= 3; end_headers= true; block= ""} ]
            {|a frame in the header block of stream 1: CONTINUATION 3 END_HEADERS ""|} )
    ; test "a CONTINUATION frame with no block fails" (fun () ->
          fails_with ~setup:open_1
            [Continuation {stream= 1; end_headers= true; block= ""}]
            "a CONTINUATION frame on stream 1 with no open block" )
    ; test "a header block that does not decode fails" (fun () ->
          fails_with ~setup:open_1
            [ Headers
                { stream= 1
                ; end_stream= false
                ; end_headers= true
                ; priority= None
                ; block= "\x80" } ]
            "a header block that does not decode: the index 0" )
    ; test "after END_STREAM, only RST_STREAM, WINDOW_UPDATE and PRIORITY"
        (fun () ->
          ok (fun c p ~start:_ ->
              open_1 c ;
              let after =
                [ data ~end_stream:true 1 "a"
                ; Window_update {stream= 1; increment= 1}
                ; Priority
                    { stream= 1
                    ; priority= {exclusive= false; dependency= 0; weight= 1} }
                ; Rst_stream {stream= 1; code= No_error} ]
              in
              peer_write p after ;
              equal Testable.string (frames_text after)
                (frames_text (List.init 4 (fun _ -> Fake.next c))) ) )
    ; test "DATA after END_STREAM fails" (fun () ->
          fails_with ~setup:open_1
            [data ~end_stream:true 1 "a"; data 1 "b"]
            "a frame on stream 1 after END_STREAM" )
    ; test "END_STREAM of a HEADERS frame applies at its END_HEADERS" (fun () ->
          fails_with ~setup:open_1
            [ headers ~end_stream:true ~end_headers:false 1 [(":status", "200")]
            ; Continuation {stream= 1; end_headers= true; block= ""}
            ; data 1 "b" ]
            "a frame on stream 1 after END_STREAM" )
    ; test "a frame after a RST_STREAM frame of the server fails" (fun () ->
          fails_with ~setup:open_1
            [ Rst_stream {stream= 1; code= Cancel}
            ; Window_update {stream= 1; increment= 1} ]
            "a frame on stream 1 after its RST_STREAM frame" )
    ; test "reset: frames before the PING ACK are legal, not after" (fun () ->
          fails
            {|a frame on stream 1 after the barrier of its reset: DATA 1 "b" flow_length=1|}
            (fun c p ~start:_ ->
              open_1 c ;
              ignore (peer_read p) ;
              Fake.reset c ~stream:1 Cancel
                ~also:[Window_update {stream= 0; increment= 1}] ;
              let rst = peer_read p in
              let also = peer_read p in
              let barrier = peer_read p in
              equal Testable.string
                "RST_STREAM 1 CANCEL\nWINDOW_UPDATE 0 +1\nPING \"barr0001\""
                (frames_text [rst; also; barrier]) ;
              peer_write p
                [data 1 "a"; Ping {ack= true; data= "barr0001"}; data 1 "b"] ;
              (* The PING ACK of the barrier is not in the mailbox. *)
              equal Testable.string {|DATA 1 "a" flow_length=1|}
                (show (Fake.next c)) ) )
    ; test "a PING ACK of another data goes in the mailbox" (fun () ->
          ok (fun c p ~start:_ ->
              peer_write p [Ping {ack= true; data= "abcdefgh"}] ;
              equal Testable.string {|PING ACK "abcdefgh"|} (show (Fake.next c)) ) )
    ; test "a frame over the maximum frame size fails" (fun () ->
          fails
            "the server sent a bad frame: a frame of 16385 bytes, more than \
             16384" (fun c p ~start:_ ->
              open_1 c ;
              peer_raw p
                (H2_support.raw ~kind:0 ~stream:1 (String.make 16385 'x')) ) )
    ; test "a larger SETTINGS_MAX_FRAME_SIZE of the client applies" (fun () ->
          ok
            ~settings:[(initial_window_size, 100_000); (0x5, 20000)]
            (fun c p ~start:_ ->
              open_1 c ;
              peer_write p [data 1 (String.make 20000 'x')] ;
              equal Testable.int 20000
                ( match Fake.next c with
                | Data {flow_length; _} ->
                    flow_length
                | _ ->
                    -1 ) ) )
    ; test "PUSH_PROMISE fails" (fun () ->
          fails "the server sent PUSH_PROMISE" (fun c p ~start:_ ->
              open_1 c ;
              peer_raw p (H2_support.raw ~kind:5 ~stream:1 "\000\000\000\002") ) )
    ; test "a frame on a stream that the client did not open fails" (fun () ->
          fails_with
            [headers 2 [(":status", "200")]]
            "a frame on stream 2, which the client did not open" )
    ; test "a malformed frame fails" (fun () ->
          fails
            "the server sent a malformed frame: a PRIORITY frame that depends \
             on its own stream" (fun c p ~start:_ ->
              open_1 c ;
              peer_raw p
                (H2_support.raw ~kind:2 ~stream:1 "\000\000\000\001\000") ) )
    ; test "a frame of an unknown type fails" (fun () ->
          fails "the server sent a frame of the unknown type 0xfa"
            (fun _ p ~start:_ ->
              peer_raw p (H2_support.raw ~kind:0xfa ~stream:0 "") ) )
    ; test "response: the heads, the body, the trailers; the rest stays"
        (fun () ->
          ok (fun c p ~start:_ ->
              open_1 c ;
              Fake.request c ~stream:3 get ;
              let b = plain [(":status", "200"); ("x", "y")] in
              peer_write p
                [ headers 1 [(":status", "100")]
                ; ping "00000000"
                ; Headers
                    { stream= 1
                    ; end_stream= false
                    ; end_headers= false
                    ; priority= None
                    ; block= String.sub b 0 2 }
                ; Continuation
                    { stream= 1
                    ; end_headers= true
                    ; block= String.sub b 2 (String.length b - 2) }
                ; data 1 "ab"
                ; data 3 "z"
                ; data 1 "cd"
                ; headers ~end_stream:true 1 [("t", "1")]
                ; Rst_stream {stream= 1; code= No_error} ] ;
              let r = Fake.response c ~stream:1 in
              let fields l =
                String.concat " " (List.map (fun (n, v) -> n ^ "=" ^ v) l)
              in
              equal Testable.string
                "interim :status=100; head :status=200 x=y; body abcd; \
                 trailers t=1; no reset"
                (Printf.sprintf "interim %s; head %s; body %s; trailers %s; %s"
                   (String.concat ", " (List.map fields r.interim))
                   (fields r.head) r.body (fields r.trailers)
                   ( match r.reset with
                   | None ->
                       "no reset"
                   | Some code ->
                       H2_support.show_code code ) ) ;
              equal Testable.string
                "PING \"00000000\"\n\
                 DATA 3 \"z\" flow_length=1\n\
                 RST_STREAM 1 NO_ERROR"
                (frames_text (List.init 3 (fun _ -> Fake.next c))) ) )
    ; test "response: a reset ends it, and so does END_STREAM on a head"
        (fun () ->
          ok (fun c p ~start:_ ->
              open_1 c ;
              Fake.request c ~stream:3 get ;
              Fake.request c ~stream:5 get ;
              let b = plain [(":status", "204")] in
              peer_write p
                [ headers 1 [(":status", "200")]
                ; Rst_stream {stream= 1; code= Cancel}
                ; headers ~end_stream:true 3 [(":status", "204")]
                ; Headers
                    { stream= 5
                    ; end_stream= true
                    ; end_headers= false
                    ; priority= None
                    ; block= String.sub b 0 1 }
                ; Continuation
                    { stream= 5
                    ; end_headers= true
                    ; block= String.sub b 1 (String.length b - 1) } ] ;
              let rs =
                List.map (fun stream -> Fake.response c ~stream) [1; 3; 5]
              in
              equal
                Testable.(list (option string))
                [Some "CANCEL"; None; None]
                (List.map
                   (fun (r : Fake.response) ->
                     Option.map H2_support.show_code r.reset )
                   rs ) ;
              equal
                Testable.(list (list (pair string string)))
                [ [(":status", "200")]
                ; [(":status", "204")]
                ; [(":status", "204")] ]
                (List.map (fun (r : Fake.response) -> r.head) rs) ) )
    ; test "frames_until gives the frames up to the first match" (fun () ->
          ok (fun c p ~start:_ ->
              peer_write p [ping "00000001"; ping "00000002"; ping "00000003"] ;
              equal Testable.string "PING \"00000001\"\nPING \"00000002\""
                (frames_text
                   (Fake.frames_until c (function
                     | Ping {data; _} ->
                         data = "00000002"
                     | _ ->
                         false )) ) ;
              equal Testable.string {|PING "00000003"|} (show (Fake.next c)) ) )
    ; test "next fails at the end of the stream of the server" (fun () ->
          ok (fun c p ~start:_ ->
              Flow.shutdown_send p.flow ;
              raises
                (failed "the server closed the connection before the next frame")
                (fun () -> Fake.next c ) ) )
    ; test "data waits for the credit of the server" (fun () ->
          Sim_support.run (fun sim ->
              with_peer
                ~server:[(initial_window_size, 10)]
                sim
                (fun c p ~start:_ ->
                  Fake.request ~end_stream:false c ~stream:1 get ;
                  ignore (peer_read p) ;
                  Fiber.scope (fun s ->
                      let send =
                        Sim_support.call s (fun () ->
                            Fake.data ~end_stream:true c ~stream:1
                              (String.make 25 'x') )
                      in
                      equal Testable.string
                        {|DATA 1 "xxxxxxxxxx" flow_length=10|}
                        (show (peer_read p)) ;
                      equal Testable.bool false (Sim_support.ended sim send) ;
                      peer_write p [Window_update {stream= 1; increment= 10}] ;
                      equal Testable.string
                        {|DATA 1 "xxxxxxxxxx" flow_length=10|}
                        (show (peer_read p)) ;
                      peer_write p [Window_update {stream= 1; increment= 100}] ;
                      equal Testable.string
                        {|DATA 1 END_STREAM "xxxxx" flow_length=5|}
                        (show (peer_read p)) ;
                      Sim_support.await send ) ) ) )
    ; test "data stops at a reset of the server" (fun () ->
          ok
            ~server:[(initial_window_size, 0)]
            (fun c p ~start:_ ->
              Fake.request ~end_stream:false c ~stream:1 get ;
              peer_write p [Rst_stream {stream= 1; code= Cancel}] ;
              ignore (Fake.next c) ;
              raises (failed "the server reset stream 1 during a send")
                (fun () -> Fake.data c ~stream:1 "x" ) ;
              raises (failed "DATA on stream 3, which is not open") (fun () ->
                  Fake.data c ~stream:3 "x" ) ) )
    ; test "pause_reading stops the reads until resume_reading" (fun () ->
          Sim_support.run (fun sim ->
              with_peer sim (fun c p ~start:_ ->
                  Fake.pause_reading c ;
                  Fiber.scope (fun s ->
                      (* More than the buffers of the socket and of the
                         reader. *)
                      let pings = List.init 20_000 (fun _ -> ping "pppppppp") in
                      let writes =
                        Sim_support.call s (fun () -> peer_write p pings)
                      in
                      equal Testable.bool false (Sim_support.ended sim writes) ;
                      Fake.resume_reading c ;
                      Sim_support.await writes ) ) ) ) ]

(* {1 The engine} *)

module Config = Caravan.Config
module Conn_info = Caravan.Conn_info
module Response = Caravan.Response
module Request = Caravan.Request
module Body = Caravan.Body
module Headers = Caravan.Headers
module Status = Caravan.Status

(* The configuration of the tests: no date, and short timeouts. *)
let config =
  { Config.default with
    date_header= false
  ; read_timeout= Span.ms 300
  ; write_timeout= Span.ms 300
  ; linger_timeout= Span.ms 100 }

(* The context of an engine. [stop] is the stop of the server: it never comes by
   default. *)
let ctx ?(stop = Ivar.create ()) config =
  { Caravan.Engine.config
  ; conn=
      Conn_info.make ~id:1
        ~peer:(Net.Tcp ("127.0.0.1", 50000))
        ~local:(Net.Tcp ("127.0.0.1", 8080))
        ~stopping:(Ivar.read stop) ()
  ; is_stopping= (fun () -> Option.is_some (Ivar.peek stop)) }

(* What a run of the engine gives: the value of the test, the reports, and the
   probe of the connection at the end. *)
type 'a run =
  {value: 'a; reports: string list; state: Conn_state.t; records: int; live: int}

(* The engine of a test, for [f] of [serve_with]: the end of the engine, and its
   probe. *)
type engine =
  { await_engine: unit -> unit
  ; engine_ended: unit -> bool
  ; abort: unit -> unit (* the server cancels the connection *)
  ; probe: H2_server.probe }

(* [serve_with ~h2 ~config ~settings ~wrap ~stop ~hooks sim handler f] runs the
   engine with the settings [h2] and [handler] on a TCP pair, and [f c e] with a
   fake client [c] connected with [settings] and the engine [e]. The engine gets
   the flow [wrap flow], the stop [stop] and the hooks [hooks] (with its own
   [inspect]). After [f], the client ends its stream, and the engine must return
   or be cancelled. The server side ends its stream at the end of the engine,
   also after a cancel. *)
let serve_with ?h2 ?(config = config) ?settings ?(wrap = Fun.id) ?stop
    ?(hooks = H2_server.no_hooks) sim handler f =
  let reports = ref [] and probe = ref None in
  let config =
    { config with
      on_error= (fun e _ -> reports := Printexc.to_string e :: !reports) }
  in
  Sim_support.with_pair sim (fun client server ->
      Fiber.scope (fun s ->
          let flow = Tcp.flow server in
          let call =
            Sim_support.call ~name:"engine" s (fun () ->
                Fun.protect
                  ~finally:(fun () ->
                    Fiber.shield (fun () ->
                        try Flow.shutdown_send flow with Flow.Closed -> () ) )
                  (fun () ->
                    H2_server.serve ?settings:h2
                      ~hooks:{hooks with inspect= (fun p -> probe := Some p)}
                      (ctx ?stop config) (wrap flow) handler ) )
          in
          let c = Fake.connect ?settings s (Tcp.flow client) in
          let probe : H2_server.probe = Option.get !probe in
          let value =
            f c
              { await_engine= (fun () -> Sim_support.await call)
              ; engine_ended= (fun () -> Sim_support.ended sim call)
              ; abort= (fun () -> Sim_support.cancel call)
              ; probe }
          in
          Fake.close c ;
          (* An abort cancels the engine. *)
          ( try Sim_support.await call with Promise.Cancelled -> () ) ;
          Fake.finish c ;
          { value
          ; reports= List.rev !reports
          ; state= probe.state
          ; records= probe.records ()
          ; live= probe.live () } ) )

let serve ?h2 ?config ?settings ?wrap ?stop sim handler f =
  serve_with ?h2 ?config ?settings ?wrap ?stop sim handler (fun c _ -> f c)

(* The time since [t0], in nanoseconds. *)
let since t0 = Span.to_ns (Time.diff Mono (Time.now Mono) t0)

(* A flow whose writes wait while the gate is closed, and that keeps the bytes
   of each write call. *)
type gate = {closed: bool Var.t; calls: string list ref}

let gate () = {closed= Var.create false; calls= ref []}

let close_gate g = Var.set g.closed true

let open_gate g = Var.set g.closed false

let gated g inner =
  let rec wait () =
    let v = Var.view g.closed in
    if Var.value v then begin
      ignore (sync (Var.changed v)) ;
      wait ()
    end
  in
  let write buf off len =
    wait () ;
    g.calls := Bytes.sub_string buf off len :: !(g.calls) ;
    Flow.write inner buf off len
  in
  Flow.make ~read:(Flow.read inner) ~read_now:(Flow.read_now inner) ~write
    ~readable:(Flow.readable inner)
    ~shutdown_send:(fun () -> Flow.shutdown_send inner)
    ~close:(fun () -> Flow.close inner)
    ~set_read_deadline:(Flow.set_read_deadline inner)
    ~set_write_deadline:(Flow.set_write_deadline inner)
    ~set_read_timeout:(Flow.set_read_timeout inner)
    ~set_write_timeout:(Flow.set_write_timeout inner)
    ()

(* The frames of the write calls of [g], after the first [skip] calls: for each
   call, the frames whose last byte is in the call. *)
let frames_by_call ?(skip = 0) g =
  let calls = List.filteri (fun i _ -> i >= skip) (List.rev !(g.calls)) in
  let r =
    Reader.create (Flow.of_string (String.concat "" calls) (Buffer.create 0))
  in
  let rec frames acc =
    match Frame.read r ~max_size:16384 with
    | f ->
        frames ((f, Reader.consumed r) :: acc)
    | exception End_of_file ->
        List.rev acc
  in
  let frames = frames [] in
  let _, groups =
    List.fold_left
      (fun (start, acc) call ->
        let stop = start + String.length call in
        ( stop
        , List.filter_map
            (fun (f, e) -> if e > start && e <= stop then Some f else None)
            frames
          :: acc ) )
      (0, []) calls
  in
  List.rev groups

let show_fields l = String.concat " " (List.map (fun (n, v) -> n ^ "=" ^ v) l)

let show_response (r : Fake.response) =
  String.concat "; "
    ( List.map (fun l -> "interim " ^ show_fields l) r.interim
    @ ["head " ^ show_fields r.head]
    @ (if r.body = "" then [] else ["body " ^ r.body])
    @ (if r.trailers = [] then [] else ["trailers " ^ show_fields r.trailers])
    @
    match r.reset with
    | None ->
        []
    | Some c ->
        ["reset " ^ H2_support.show_code c] )

let hello _ = Response.string "hello"

(* A response whose body is one write: one loan. The bodies of [Response.string]
   flush after each chunk. *)
let one_write body _ =
  Response.stream (fun w -> Body.Writer.write_string w body)

let sprintf = Printf.sprintf

(* A short text of a frame: the type, the stream, the length of DATA, the
   flags. *)
let brief (f : Frame.t) =
  let flag b name = if b then " " ^ name else "" in
  match f with
  | Headers {stream; end_stream; end_headers; _} ->
      sprintf "HEADERS %d%s%s" stream
        (flag end_stream "END_STREAM")
        (flag (not end_headers) "(more)")
  | Continuation {stream; end_headers; _} ->
      sprintf "CONTINUATION %d%s" stream (flag (not end_headers) "(more)")
  | Data {stream; end_stream; data; _} ->
      sprintf "DATA %d %d%s" stream (String.length data)
        (flag end_stream "END_STREAM")
  | f ->
      show f

let briefs l = String.concat "\n" (List.map brief l)

(* [ends stream f] is [true] for the last frame of a response on [stream]. *)
let ends stream (f : Frame.t) =
  match f with
  | Data {stream= s; end_stream= true; _}
  | Headers {stream= s; end_stream= true; end_headers= true; _}
  | Rst_stream {stream= s; _} ->
      s = stream
  | _ ->
      false

let request ?(meth = "GET") ?(path = "/") ?(fields = []) () =
  [ (":method", meth)
  ; (":scheme", "http")
  ; (":path", path)
  ; (":authority", "a.example") ]
  @ fields

let post ?path ?length ?(fields = []) () =
  request ~meth:"POST" ?path
    ~fields:
      ( ( match length with
          | Some n ->
              [("content-length", string_of_int n)]
          | None ->
              [] )
      @ fields )
    ()

(* The fields of a header block of the server. The server never uses the dynamic
   table, thus a new decoder decodes each block. *)
let decode block =
  match
    Hpack.Decoder.decode
      (Hpack.Decoder.create ~max_table_size:4096)
      ~max_list_size:max_int ~max_fields:max_int block
  with
  | `Fields l ->
      l
  | `Too_large ->
      []

let status_of (f : Frame.t) =
  match f with
  | Headers {block; _} ->
      Option.value (List.assoc_opt ":status" (decode block)) ~default:"-"
  | _ ->
      "-"

(* A handler that dispatches on the path. *)
let paths l req =
  match List.assoc_opt (Request.path req) l with
  | Some h ->
      h req
  | None ->
      hello req

let opened = [(initial_window_size, 0x7fff_ffff)]

let is_data = function Frame.Data _ -> true | _ -> false

let is_rst = function Frame.Rst_stream _ -> true | _ -> false

(* The client gives the largest connection window. *)
let open_window c =
  Fake.send c (Window_update {stream= 0; increment= 0x7fff_ffff - 65535})

let log () =
  let l = ref [] in
  ((fun s -> l := s :: !l), fun () -> List.rev !l)

let hook record name =
  Body.on_end (fun how ~bytes ->
      record
        (sprintf "%s %s %d" name
           ( match how with
           | Body.Ended ->
               "Ended"
           | Failed e ->
               "Failed " ^ Printexc.to_string e
           | Abandoned ->
               "Abandoned" )
           bytes ) )

(* A block that puts x-dyn: yes in the dynamic table, at index 62. The blocks of
   the fake client start with a table size update to 0. Thus this block first
   sets the size to 4096 (0x3f 0xe1 0x1f), then has a literal with incremental
   indexing. *)
let dynamic_entry = "\x3f\xe1\x1f\x40\x05x-dyn\x03yes"

(* [uses_entry fields] is the block of [fields], then the indexed field 62. *)
let uses_entry fields = plain fields ^ "\xbe"

(* A HEADERS frame with the block [block], and END_HEADERS by default. *)
let headers_frame ?(end_stream = false) ?(end_headers = true) ?priority stream
    block =
  Frame.Headers {stream; end_stream; end_headers; priority; block}

let reports = Testable.(list string)

(* The end of a test: no report, no live stream fiber, and no record of a stream
   in the engine or in its state. After an end with an error, W does not write
   the items of the streams, and the streams stay open: their records stay until
   the engine returns ([~error_end:true]). *)
let clean_end ?(error_end = false) ?(block_open = false) r =
  equal reports [] r.reports ;
  equal Testable.int 0 r.live ;
  if not error_end then begin
    equal Testable.int 0 r.records ;
    (* The state keeps the stream of a header block that did not end. *)
    if not block_open then
      equal Testable.int 0 (Conn_state.known_streams r.state)
  end

(* [with_writer_blocked c f]: the client stops to read, and stream 1 gets a
   response of 300 KiB, so that W waits in a write to the flow. Then [f ()]
   runs. Then the client reads again, and takes the response of stream 1. The
   result is the frames that stay in the mailbox after it. The handler must
   answer "/big". *)
let with_writer_blocked sim c f =
  open_window c ;
  Fake.pause_reading c ;
  Fake.request c ~stream:1 (request ~path:"/big" ()) ;
  Weft_sim.settle sim ;
  f () ;
  Weft_sim.settle sim ;
  Fake.resume_reading c ;
  ignore (Fake.response c ~stream:1) ;
  Weft_sim.settle sim ;
  Fake.take_all c

let big _ = Response.string (String.make (300 * 1024) 'b')

let engine_tests =
  group "Engine"
    [ test "GET: HEADERS, DATA, and END_STREAM" (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve sim hello (fun c ->
                    Fake.request c ~stream:1 get ;
                    let frames = Fake.frames_until c (ends 1) in
                    ( briefs frames
                    , match frames with
                      | Headers {block; _} :: _ ->
                          show_fields (decode block)
                      | _ ->
                          "" ) ) )
          in
          equal Testable.string "HEADERS 1\nDATA 1 5\nDATA 1 0 END_STREAM"
            (fst r.value) ;
          equal Testable.string
            ":status=200 content-type=text/plain; charset=utf-8 \
             content-length=5"
            (snd r.value) ;
          clean_end r )
    ; test "a stream that waits does not stop the next one" (fun () ->
          let release = Ivar.create () in
          let handler =
            paths
              [ ( "/wait"
                , fun _ ->
                    sync (Ivar.read release) ;
                    Response.string "late" ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request c ~stream:1 (request ~path:"/wait" ()) ;
                    Fake.request c ~stream:3 get ;
                    let r3 = Fake.response c ~stream:3 in
                    Ivar.fill release () ;
                    let r1 = Fake.response c ~stream:1 in
                    (r3.body, r1.body) ) )
          in
          equal Testable.(pair string string) ("hello", "late") r.value ;
          clean_end r )
    ; test "a POST of 200 KiB: WINDOW_UPDATE frames, the body is intact"
        (fun () ->
          let body = String.init (200 * 1024) (fun i -> Char.chr (i mod 251)) in
          let handler req =
            let got = Body.to_string (Request.body req) in
            Response.string (sprintf "%d %b" (String.length got) (got = body))
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request ~end_stream:false c ~stream:1
                      (post ~length:(String.length body) ()) ;
                    Fake.data ~end_stream:true c ~stream:1 body ;
                    let r = Fake.response c ~stream:1 in
                    let updates =
                      List.filter
                        (function
                          | Frame.Window_update {stream= 1; _} ->
                              true
                          | _ ->
                              false )
                        (Fake.take_all c)
                    in
                    (r.body, List.length updates > 0) ) )
          in
          equal Testable.(pair string bool) ("204800 true", true) r.value ;
          clean_end r ;
          equal Testable.int (100 * 65535) (Conn_state.receive_window r.state) )
    ; test "a client window of 1000: 1000 bytes, then the rest at credit"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 1000)]
                  sim
                  (fun _ -> Response.string (String.make 3000 'x'))
                  (fun c ->
                    Fake.request c ~stream:1 get ;
                    let first = Fake.frames_until c is_data in
                    Weft_sim.settle sim ;
                    let waiting = Fake.take_all c in
                    Fake.credit c ~stream:1 5000 ;
                    let rest = Fake.frames_until c (ends 1) in
                    briefs (first @ waiting @ rest) ) )
          in
          equal Testable.string
            "HEADERS 1\nDATA 1 1000\nDATA 1 2000\nDATA 1 0 END_STREAM" r.value ;
          clean_end r )
    ; test "a smaller INITIAL_WINDOW_SIZE: a negative window, no DATA"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 2000)]
                  sim
                  (fun _ -> Response.string (String.make 10000 'x'))
                  (fun c ->
                    Fake.request c ~stream:1 get ;
                    let first = Fake.frames_until c is_data in
                    (* The window of stream 1: 0, then -1000, then -500. *)
                    Fake.send c
                      (Settings
                         {ack= false; settings= [(initial_window_size, 1000)]}
                      ) ;
                    Fake.send c (Window_update {stream= 1; increment= 500}) ;
                    Weft_sim.settle sim ;
                    let waiting = Fake.take_all c in
                    Fake.send c (Window_update {stream= 1; increment= 1500}) ;
                    let next = Fake.frames_until c is_data in
                    briefs (first @ waiting @ next) ) )
          in
          equal Testable.string "HEADERS 1\nDATA 1 2000\nDATA 1 1000" r.value ;
          clean_end r )
    ; test "a head of 40 KiB: HEADERS and 2 CONTINUATION, contiguous" (fun () ->
          (* 60000 hexadecimal digits are about 40 KiB with the Huffman code. *)
          let big =
            String.init 60000 (fun i -> "0123456789abcdef".[i * 7 mod 16])
          in
          let handler =
            paths
              [ ( "/big"
                , fun _ ->
                    Response.string
                      ~headers:(Headers.of_list [("x-big", big)])
                      "b" )
              ; ("/data", fun _ -> Response.string (String.make 100_000 'd')) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~settings:opened sim handler (fun c ->
                    open_window c ;
                    Fake.request c ~stream:1 (request ~path:"/data" ()) ;
                    Fake.request c ~stream:3 (request ~path:"/big" ()) ;
                    let r3 = Fake.response c ~stream:3 in
                    let r1 = Fake.response c ~stream:1 in
                    ( List.assoc_opt "x-big" r3.head = Some big
                    , String.length r1.body ) ) )
          in
          equal Testable.(pair bool int) (true, 100_000) r.value ;
          clean_end r )
    ; test "the frames of a head of 40 KiB" (fun () ->
          let big =
            String.init 60000 (fun i -> "0123456789abcdef".[i * 7 mod 16])
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim
                  (fun _ ->
                    Response.string
                      ~headers:(Headers.of_list [("x-big", big)])
                      "b" )
                  (fun c ->
                    Fake.request c ~stream:1 get ;
                    briefs (Fake.frames_until c (ends 1)) ) )
          in
          equal Testable.string
            "HEADERS 1 (more)\n\
             CONTINUATION 1 (more)\n\
             CONTINUATION 1\n\
             DATA 1 1\n\
             DATA 1 0 END_STREAM"
            r.value )
    ; test "a reset while the handler waits: cancel, on_refused, stream 3"
        (fun () ->
          let record, events = log () in
          let started = Ivar.create () and cancelled = Ivar.create () in
          let handler =
            paths
              [ ( "/wait"
                , fun _ ->
                    Ivar.fill started () ;
                    match sync never with
                    | () ->
                        Response.string "never"
                    | exception Fiber.Cancelled _ ->
                        record "cancelled" ;
                        Ivar.fill cancelled () ;
                        Response.upgrade
                          ~headers:(Headers.of_list [("upgrade", "x")])
                          ~on_refused:(fun () -> record "on_refused")
                          (fun _ -> record "upgraded") ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request c ~stream:1 (request ~path:"/wait" ()) ;
                    sync (Ivar.read started) ;
                    Fake.reset c ~stream:1 Cancel ;
                    (* The reset cancels the handler, not the end of the
                       connection. *)
                    sync (Ivar.read cancelled) ;
                    Fake.request c ~stream:3 get ;
                    (Fake.response c ~stream:3).body ) )
          in
          equal Testable.string "hello" r.value ;
          equal Testable.(list string) ["cancelled"; "on_refused"] (events ()) ;
          clean_end r )
    ; test "a reset while the stream waits for credit" (fun () ->
          let record, events = log () in
          let handler =
            paths
              [ ( "/slow"
                , fun _ ->
                    Response.make
                      (hook record "body"
                         (Body.of_string (String.make 1000 'x')) ) ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 100)]
                  sim handler
                  (fun c ->
                    Fake.request c ~stream:1 (request ~path:"/slow" ()) ;
                    ignore (Fake.frames_until c is_data) ;
                    (* A larger initial window after the reset: a loan that
                       stayed in the queue would go after the barrier. *)
                    Fake.reset c ~stream:1 Cancel
                      ~also:
                        [ Settings
                            { ack= false
                            ; settings= [(initial_window_size, 100_000)] } ] ;
                    Fake.request c ~stream:3 get ;
                    (Fake.response c ~stream:3).body ) )
          in
          equal Testable.string "hello" r.value ;
          equal
            Testable.(list string)
            ["body Failed Weft_flow__Flow.Closed 0"]
            (events ()) ;
          clean_end r )
    ; test
        "a handler that raises, a producer that raises before and after its \
         output" (fun () ->
          let handler =
            paths
              [ ("/raise", fun _ -> failwith "handler")
              ; ("/early", fun _ -> Response.stream (fun _ -> failwith "early"))
              ; ( "/late"
                , fun _ ->
                    Response.stream (fun w ->
                        Body.Writer.write_string w "abc" ;
                        failwith "late" ) ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    List.map
                      (fun (stream, path) ->
                        Fake.request c ~stream (request ~path ()) ;
                        show_response (Fake.response c ~stream) )
                      [(1, "/raise"); (3, "/early"); (5, "/late")] ) )
          in
          equal
            Testable.(list string)
            [ "head :status=500 content-length=0"
            ; "head :status=500 content-length=0"
            ; "head :status=200; body abc; reset INTERNAL_ERROR" ]
            r.value ;
          equal reports
            ["Failure(\"handler\")"; "Failure(\"early\")"; "Failure(\"late\")"]
            r.reports )
    ; test "a body that breaks its content-length: RST PROTOCOL_ERROR"
        (fun () ->
          let record, events = log () in
          let release = Ivar.create () in
          (* The read raises at once: the time does not move. *)
          let reading req =
            let t0 = Time.now Mono in
            ( match Body.to_string (Request.body req) with
            | s ->
                record ("read " ^ s)
            | exception Body.Truncated ->
                record
                  (sprintf "Truncated after %Ld ns"
                     (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) ) ) ;
            Response.string "late"
          in
          (* A body error does not cancel the handler: its response is
             abandoned. *)
          let waiting _ =
            sync (Ivar.read release) ;
            Response.make (hook record "late" (Body.of_string "late"))
          in
          (* A handler that does not catch the exception of the read: no
             report. *)
          let raising req =
            ignore (Body.to_string (Request.body req)) ;
            hello req
          in
          let handler =
            paths [("/read", reading); ("/wait", waiting); ("/raise", raising)]
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    List.map
                      (fun (stream, path, data) ->
                        Fake.request ~end_stream:false c ~stream
                          (post ~path ~length:10 ()) ;
                        Fake.data
                          ~end_stream:(String.length data < 10)
                          c ~stream data ;
                        show_response (Fake.response c ~stream) )
                      [ (1, "/read", "12345")
                      ; (3, "/read", String.make 15 'x')
                      ; (5, "/wait", "12345")
                      ; (7, "/wait", String.make 15 'x')
                      ; (9, "/raise", "12345") ]
                    |> fun l -> Ivar.fill release () ; Weft_sim.settle sim ; l ) )
          in
          equal
            Testable.(list string)
            (List.init 5 (fun _ -> "head ; reset PROTOCOL_ERROR"))
            r.value ;
          equal
            Testable.(list string)
            [ "Truncated after 0 ns"
            ; "Truncated after 0 ns"
            ; "late Abandoned 0"
            ; "late Abandoned 0" ]
            (events ()) ;
          clean_end r )
    ; test "a response with no read of the body: END_STREAM, then RST NO_ERROR"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve sim hello (fun c ->
                    Fake.request ~end_stream:false c ~stream:1
                      (post ~length:100_000 ()) ;
                    Fake.data c ~stream:1 (String.make 50_000 'x') ;
                    briefs (Fake.frames_until c is_rst) ) )
          in
          equal Testable.string
            "HEADERS 1\nDATA 1 5\nDATA 1 0 END_STREAM\nRST_STREAM 1 NO_ERROR"
            r.value ;
          clean_end r ;
          equal Testable.int (100 * 65535) (Conn_state.receive_window r.state) )
    ; test "expect 100-continue: the 100 first; an echo; 417" (fun () ->
          let handler =
            paths
              [ ( "/read"
                , fun req -> Response.string (Body.to_string (Request.body req))
                )
              ; ( "/echo"
                , fun req ->
                    Response.stream (fun w ->
                        Body.Writer.write_string w
                          (Body.to_string (Request.body req)) ) ) ]
          in
          let expect v = [("expect", v)] in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    let one stream path =
                      Fake.request ~end_stream:false c ~stream
                        (post ~path ~length:5 ~fields:(expect "100-continue") ()) ;
                      let continue = status_of (Fake.next c) in
                      Fake.data ~end_stream:true c ~stream "hello" ;
                      continue ^ " then "
                      ^ show_response (Fake.response c ~stream)
                    in
                    let read = one 1 "/read" in
                    let echo = one 3 "/echo" in
                    Fake.request c ~stream:5 (request ~fields:(expect "foo") ()) ;
                    [read; echo; show_response (Fake.response c ~stream:5)] ) )
          in
          equal
            Testable.(list string)
            [ "100 then head :status=200 content-type=text/plain; \
               charset=utf-8 content-length=5; body hello"
            ; "100 then head :status=200; body hello"
            ; "head :status=417 content-length=0" ]
            r.value ;
          clean_end r )
    ; test "request trailers, response trailers" (fun () ->
          let handler req =
            let body = Body.to_string (Request.body req) in
            let t = Headers.to_list (Body.trailers (Request.body req)) in
            Response.stream (fun w ->
                Body.Writer.write_string w (body ^ " " ^ show_fields t) ;
                Body.Writer.set_trailers w (Headers.of_list [("X-R", "2")]) )
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fake.data c ~stream:1 "abc" ;
                    Fake.request ~end_stream:true c ~stream:1 [("x-t", "1")] ;
                    show_response (Fake.response c ~stream:1) ) )
          in
          equal Testable.string
            "head :status=200; body abc x-t=1; trailers x-r=2" r.value ;
          clean_end r )
    ; test "trailers without END_STREAM: RST, and the block is decoded"
        (fun () ->
          let handler req =
            Response.string
              (Option.value
                 (Headers.find "x-dyn" (Request.headers req))
                 ~default:"none" )
          in
          let indexed = dynamic_entry in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fake.data c ~stream:1 "abc" ;
                    Fake.send c (headers_frame 1 indexed) ;
                    let r1 = show_response (Fake.response c ~stream:1) in
                    Fake.send c
                      (headers_frame ~end_stream:true 3 (uses_entry get)) ;
                    [r1; (Fake.response c ~stream:3).body] ) )
          in
          equal
            Testable.(list string)
            ["head ; reset PROTOCOL_ERROR"; "yes"]
            r.value ;
          clean_end r )
    ; test "HEAD, 204, 304: no DATA, and the hook of the body runs" (fun () ->
          let record, events = log () in
          let handler =
            paths
              [ ( "/head"
                , fun _ ->
                    Response.make (hook record "head" (Body.of_string "hello"))
                )
              ; ( "/204"
                , fun _ ->
                    Response.make ~status:Status.no_content
                      (hook record "204" (Body.of_string "")) )
              ; ( "/304"
                , fun _ ->
                    Response.make ~status:Status.not_modified
                      (hook record "304" (Body.of_string "x")) ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    List.map
                      (fun (stream, meth, path) ->
                        Fake.request c ~stream (request ~meth ~path ()) ;
                        let frames = Fake.frames_until c (ends stream) in
                        let length =
                          match frames with
                          | [Headers {block; _}] ->
                              List.assoc_opt "content-length" (decode block)
                          | _ ->
                              None
                        in
                        (briefs frames, length) )
                      [ (1, "HEAD", "/head")
                      ; (3, "GET", "/204")
                      ; (5, "GET", "/304") ] ) )
          in
          (* HEAD keeps the content-length of the body. *)
          equal
            Testable.(list (pair string (option string)))
            [ ("HEADERS 1 END_STREAM", Some "5")
            ; ("HEADERS 3 END_STREAM", None)
            ; ("HEADERS 5 END_STREAM", None) ]
            r.value ;
          equal
            Testable.(list string)
            ["head Abandoned 0"; "204 Abandoned 0"; "304 Abandoned 0"]
            (events ()) ;
          clean_end r )
    ; test "a full duplex echo of 300 KiB, read and write in two fibers"
        (fun () ->
          let body = String.init (300 * 1024) (fun i -> Char.chr (i mod 253)) in
          let duplex req =
            Response.stream (fun w ->
                let m = Fiber.Mutex.create ()
                and changed = Fiber.Condition.create () in
                let chunks = Queue.create () and finished = ref false in
                let locked f = Fiber.Mutex.use_rw ~protect:false m f in
                Fiber.scope (fun sc ->
                    Fiber.fork sc (fun () ->
                        let buf = Bytes.create 4096 in
                        let rec loop () =
                          match Body.read (Request.body req) buf 0 4096 with
                          | 0 ->
                              locked (fun () ->
                                  finished := true ;
                                  Fiber.Condition.broadcast changed )
                          | n ->
                              let chunk = Bytes.sub_string buf 0 n in
                              locked (fun () ->
                                  Queue.push chunk chunks ;
                                  Fiber.Condition.broadcast changed ) ;
                              loop ()
                        in
                        loop () ) ;
                    let rec write () =
                      let next =
                        locked (fun () ->
                            while Queue.is_empty chunks && not !finished do
                              Fiber.Condition.wait changed m
                            done ;
                            Queue.take_opt chunks )
                      in
                      match next with
                      | Some chunk ->
                          Body.Writer.write_string w chunk ;
                          write ()
                      | None ->
                          ()
                    in
                    write () ) )
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~settings:opened sim duplex (fun c ->
                    open_window c ;
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fiber.scope (fun s ->
                        let send =
                          Sim_support.call s (fun () ->
                              Fake.data ~end_stream:true c ~stream:1 body )
                        in
                        let r = Fake.response c ~stream:1 in
                        Sim_support.await send ; r.body = body ) ) )
          in
          equal Testable.bool true r.value ;
          clean_end r )
    ; test "an event stream that flushes is not behind a download" (fun () ->
          let seen = Array.init 5 (fun _ -> Ivar.create ()) in
          let handler =
            paths
              [ (* One loan: W takes a part of it in each round, and is never
                   idle until its end. *)
                ("/download", one_write (String.make (2 * 1024 * 1024) 'd'))
              ; ( "/events"
                , fun _ ->
                    Response.stream (fun w ->
                        Array.iter
                          (fun iv ->
                            Body.Writer.write_string w (String.make 1024 'e') ;
                            Body.Writer.flush w ;
                            sync (Ivar.read iv) )
                          seen ) ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~settings:opened sim handler (fun c ->
                    open_window c ;
                    Fake.request c ~stream:1 (request ~path:"/download" ()) ;
                    Fake.request c ~stream:3 (request ~path:"/events" ()) ;
                    let events = ref 0 in
                    let rec loop () =
                      match Fake.next c with
                      | Data {stream= 3; data; _} when String.length data = 1024
                        ->
                          Ivar.fill seen.(!events) () ;
                          incr events ;
                          loop ()
                      | Data {stream= 1; end_stream= true; _} ->
                          !events
                      | _ ->
                          loop ()
                    in
                    loop () ) )
          in
          equal Testable.int 5 r.value ;
          clean_end r )
    ; test "a zero window: RST CANCEL after write_timeout; stream 3 served"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 0)]
                  sim hello
                  (fun c ->
                    let t0 = Time.now Mono in
                    Fake.request c ~stream:1 get ;
                    Fake.request c ~stream:3 get ;
                    Fake.send c (Window_update {stream= 3; increment= 5}) ;
                    let r3 = (Fake.response c ~stream:3).body in
                    let r1 = show_response (Fake.response c ~stream:1) in
                    (r1, r3, Span.to_ns (Time.diff Mono (Time.now Mono) t0)) ) )
          in
          equal
            Testable.(triple string string int64)
            ( "head :status=200 content-type=text/plain; charset=utf-8 \
               content-length=5; reset CANCEL"
            , "hello"
            , Span.to_ns config.write_timeout )
            r.value ;
          clean_end r )
    ; test "101 streams: REFUSED_STREAM, and its block is decoded" (fun () ->
          let release = Ivar.create () in
          let handler req =
            match Headers.find "x-dyn" (Request.headers req) with
            | Some v ->
                Response.string v
            | None ->
                sync (Ivar.read release) ;
                hello req
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    for i = 0 to 99 do
                      Fake.request c ~stream:((2 * i) + 1) get
                    done ;
                    Fake.send c
                      (headers_frame ~end_stream:true 201
                         (dynamic_entry ^ plain get) ) ;
                    let refused = Fake.next c in
                    Ivar.fill release () ;
                    for i = 0 to 99 do
                      ignore (Fake.response c ~stream:((2 * i) + 1))
                    done ;
                    Fake.send c
                      (headers_frame ~end_stream:true 203 (uses_entry get)) ;
                    (show refused, (Fake.response c ~stream:203).body) ) )
          in
          equal
            Testable.(pair string string)
            ("RST_STREAM 201 REFUSED_STREAM", "yes")
            r.value ;
          clean_end r )
    ; test "2 * max_concurrent_streams resets, then a new stream" (fun () ->
          let started = ref (Ivar.create ()) in
          let handler =
            paths [("/wait", fun _ -> Ivar.fill !started () ; sync never)]
          in
          let h2 = {Settings.default with max_concurrent_streams= 4} in
          let r =
            Sim_support.run (fun sim ->
                serve ~h2 sim handler (fun c ->
                    for i = 0 to 7 do
                      started := Ivar.create () ;
                      Fake.request c
                        ~stream:((2 * i) + 1)
                        (request ~path:"/wait" ()) ;
                      sync (Ivar.read !started) ;
                      Fake.reset c ~stream:((2 * i) + 1) Cancel
                    done ;
                    Fake.request c ~stream:17 get ;
                    (Fake.response c ~stream:17).body ) )
          in
          equal Testable.string "hello" r.value ;
          clean_end r )
    ; test "an upgrade, a 2xx to CONNECT: 500 and one report each" (fun () ->
          let record, events = log () in
          let handler req =
            match Request.meth req with
            | CONNECT ->
                Response.make (hook record "tunnel" (Body.of_string "tunnel"))
            | _ ->
                Response.upgrade
                  ~headers:(Headers.of_list [("upgrade", "x")])
                  ~on_refused:(fun () -> record "on_refused")
                  (fun _ -> record "upgraded")
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request c ~stream:1 get ;
                    let up = show_response (Fake.response c ~stream:1) in
                    Fake.request c ~stream:3
                      [(":method", "CONNECT"); (":authority", "a.example:443")] ;
                    [up; show_response (Fake.response c ~stream:3)] ) )
          in
          equal
            Testable.(list string)
            [ "head :status=500 content-length=0"
            ; "head :status=500 content-length=0" ]
            r.value ;
          equal
            Testable.(list string)
            ["on_refused"; "tunnel Abandoned 0"]
            (events ()) ;
          equal reports
            [ "Failure(\"an upgrade response: the HTTP/2 engine has no \
               upgrades\")"
            ; "Failure(\"a 2xx response to CONNECT: the HTTP/2 engine has no \
               tunnels\")" ]
            r.reports )
    ; test "a head over max_header_list_size: 431, the next one decodes"
        (fun () ->
          let h2 = {Settings.default with max_header_list_size= Some 1000} in
          let handler req =
            Response.string
              (Option.value
                 (Headers.find "x-dyn" (Request.headers req))
                 ~default:"none" )
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~h2 sim handler (fun c ->
                    Fake.send c
                      (headers_frame ~end_stream:true 1
                         ( dynamic_entry
                         ^ plain (get @ [("x-big", String.make 2000 'a')]) ) ) ;
                    let r1 = show_response (Fake.response c ~stream:1) in
                    Fake.send c
                      (headers_frame ~end_stream:true 3 (uses_entry get)) ;
                    [r1; (Fake.response c ~stream:3).body] ) )
          in
          equal
            Testable.(list string)
            ["head :status=431 content-length=0"; "yes"]
            r.value ;
          clean_end r )
    ; test "a connection error with streams in the states b, d and e" (fun () ->
          let record, events = log () in
          let wait name =
            match sync never with
            | () ->
                ()
            | exception (Fiber.Cancelled _ as e) ->
                record (name ^ " cancelled") ;
                raise e
          in
          let handler =
            paths
              [ (* A handler that catches its cancel: its late response is
                   abandoned. *)
                ( "/b"
                , fun _ ->
                    match sync never with
                    | () ->
                        Response.string "never"
                    | exception Fiber.Cancelled _ ->
                        record "b cancelled" ;
                        Response.make (hook record "b" (Body.of_string "late"))
                )
              ; ( "/d"
                , fun _ ->
                    Response.make
                      (hook record "d"
                         (Body.of_writer (fun w ->
                              Body.Writer.write_string w (String.make 1024 'd') ;
                              Body.Writer.flush w ;
                              wait "d" ) ) ) )
              ; ( "/e"
                , fun _ ->
                    Response.make
                      (hook record "e" (Body.of_string (String.make 100 'e')))
                ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 0)]
                  sim handler
                  (fun c ->
                    Fake.request ~end_stream:false c ~stream:1
                      (post ~path:"/b" ()) ;
                    Fake.data c ~stream:1 "abc" ;
                    Fake.request c ~stream:3 (request ~path:"/d" ()) ;
                    Fake.send c (Window_update {stream= 3; increment= 100_000}) ;
                    Fake.request c ~stream:5 (request ~path:"/e" ()) ;
                    ignore
                      (Fake.frames_until c (function
                        | Data {stream= 3; _} ->
                            true
                        | _ ->
                            false )) ;
                    ignore
                      (Fake.frames_until c (function
                        | Headers {stream= 5; _} ->
                            true
                        | _ ->
                            false )) ;
                    Weft_sim.settle sim ;
                    ignore (Fake.take_all c) ;
                    Fake.send c (data 7 "x") ;
                    show (Fake.next c) ) )
          in
          equal Testable.string
            {|GOAWAY 5 PROTOCOL_ERROR "DATA on an idle stream"|} r.value ;
          equal
            Testable.(list string)
            [ "b Abandoned 0"
            ; "b cancelled"
            ; "d Failed Weft__Sched.Cancelled(_) 1024"
            ; "d cancelled"
            ; "e Failed Weft_flow__Flow.Closed 0" ]
            (List.sort compare (events ())) ;
          clean_end ~error_end:true r )
    ; test "race: 50 streams with resets and large bodies, on 4 domains"
        (fun () ->
          let request_body =
            String.init 20_000 (fun i -> Char.chr (i mod 241))
          in
          let answer = String.make 40_000 'y' in
          Sim_support.race (fun sim ->
              let created = Atomic.make 0 and hooks = Atomic.make 0 in
              let closes = Atomic.make 0 in
              (* A source body with a close, as the body of an upstream: the
                 close runs one time for each response, also after a reset. *)
              let source s =
                let pos = ref 0 in
                Body.of_source ~length:(String.length s)
                  ~close:(fun () -> Atomic.incr closes)
                  (fun buf off len ->
                    let n = min len (String.length s - !pos) in
                    Bytes.blit_string s !pos buf off n ;
                    pos := !pos + n ;
                    n )
              in
              let handler req =
                let got = Body.to_string (Request.body req) in
                Atomic.incr created ;
                Response.make
                  (Body.on_end
                     (fun _ ~bytes:_ -> Atomic.incr hooks)
                     (source (if got = request_body then answer else "no")) )
              in
              let reset i = i mod 3 = 0 in
              let r =
                serve ~settings:opened sim handler (fun c ->
                    open_window c ;
                    for i = 0 to 49 do
                      let stream = (2 * i) + 1 in
                      Fake.request ~end_stream:false c ~stream
                        (post ~length:(String.length request_body) ()) ;
                      Fake.data ~end_stream:true c ~stream request_body ;
                      if reset i then Fake.reset c ~stream Cancel
                    done ;
                    List.for_all
                      (fun i ->
                        reset i
                        ||
                        let r = Fake.response c ~stream:((2 * i) + 1) in
                        r.body = answer && r.reset = None )
                      (List.init 50 Fun.id) )
              in
              equal Testable.bool true r.value ;
              equal Testable.int (Atomic.get created) (Atomic.get hooks) ;
              equal Testable.int (Atomic.get created) (Atomic.get closes) ;
              clean_end r ;
              equal Testable.int (100 * 65535)
                (Conn_state.receive_window r.state) ) )
    ; test "1 MiB with a window of 65535 that the client opens: no RST"
        (fun () ->
          let size = 1024 * 1024 in
          let r =
            Sim_support.run (fun sim ->
                serve sim
                  (fun _ -> Response.string (String.make size 'x'))
                  (fun c ->
                    Fake.request c ~stream:1 get ;
                    let rec loop got =
                      match Fake.next c with
                      | Data {stream= 1; data; end_stream; _} ->
                          let n = String.length data in
                          if n > 0 then Fake.credit c ~stream:1 n ;
                          if end_stream then got + n else loop (got + n)
                      | _ ->
                          loop got
                    in
                    let got = loop 0 in
                    Weft_sim.settle sim ;
                    (got, List.exists is_rst (Fake.take_all c)) ) )
          in
          equal Testable.(pair int bool) (size, false) r.value ;
          clean_end r )
    ; test
        "a body with no length over max_body: RST CANCEL, the 413 is abandoned"
        (fun () ->
          let record, events = log () in
          let handler req =
            match Body.to_string (Request.body req) with
            | _ ->
                hello req
            | exception Body.Too_large ->
                Response.make ~status:Status.content_too_large
                  (hook record "413" (Body.of_string "too large"))
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~config:{config with max_body= Some 1000} sim handler
                  (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fake.data c ~stream:1 (String.make 1500 'x') ;
                    let r = show_response (Fake.response c ~stream:1) in
                    Weft_sim.settle sim ;
                    (r, List.length (Fake.take_all c)) ) )
          in
          equal Testable.(pair string int) ("head ; reset CANCEL", 0) r.value ;
          equal Testable.(list string) ["413 Abandoned 0"] (events ()) ;
          clean_end r )
    ; test "END_STREAM of the client while a RST NO_ERROR waits: no RST"
        (fun () ->
          let answered = Ivar.create () in
          let handler =
            paths
              [ ("/big", big)
              ; ( "/204"
                , fun _ ->
                    Ivar.fill answered () ;
                    Response.empty ~status:Status.no_content () ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~settings:opened sim handler (fun c ->
                    briefs
                      (with_writer_blocked sim c (fun () ->
                           (* The head of stream 3 with END_STREAM, then
                              RST_STREAM NO_ERROR, wait in its queue. *)
                           Fake.request ~end_stream:false c ~stream:3
                             (post ~path:"/204" ()) ;
                           sync (Ivar.read answered) ;
                           Weft_sim.settle sim ;
                           Fake.data ~end_stream:true c ~stream:3 "" ) ) ) )
          in
          equal Testable.string "HEADERS 3 END_STREAM" r.value ;
          clean_end r )
    ; test
        "a malformed frame after our END_STREAM is queued: RST \
         FRAME_SIZE_ERROR (h2spec http2/6.3/2)" (fun () ->
          (* The handler of stream 3 answers at once, and its END_STREAM waits
             in the queue: the state has the stream as ended in the two
             directions. Then a PRIORITY frame of 4 bytes comes. The RST
             replaces the items of stream 3 that W did not take. *)
          let answered = Ivar.create () in
          let handler =
            paths
              [ ("/big", big)
              ; ( "/204"
                , fun _ ->
                    Ivar.fill answered () ;
                    Response.empty ~status:Status.no_content () ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~settings:opened sim handler (fun c ->
                    briefs
                      (with_writer_blocked sim c (fun () ->
                           Fake.request c ~stream:3 (request ~path:"/204" ()) ;
                           sync (Ivar.read answered) ;
                           Weft_sim.settle sim ;
                           Fake.send_raw c
                             (H2_support.raw ~kind:2 ~stream:3
                                "\000\000\000\001" ) ) ) ) )
          in
          equal Testable.string "RST_STREAM 3 FRAME_SIZE_ERROR" r.value ;
          clean_end r )
    ; test "a client reset after our END_STREAM is queued: no frame after it"
        (fun () ->
          let answered = Ivar.create () in
          let handler =
            paths
              [ ("/big", big)
              ; ( "/204"
                , fun _ ->
                    Ivar.fill answered () ;
                    Response.empty ~status:Status.no_content () ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~settings:opened sim handler (fun c ->
                    briefs
                      (with_writer_blocked sim c (fun () ->
                           (* The head of stream 3 with END_STREAM waits in its
                              queue: the state has the stream as closed. *)
                           Fake.request c ~stream:3 (request ~path:"/204" ()) ;
                           sync (Ivar.read answered) ;
                           Weft_sim.settle sim ;
                           Fake.reset c ~stream:3 Cancel ) ) ) )
          in
          equal Testable.string "" r.value ;
          clean_end r )
    ; test "the read timeout: 408 with END_STREAM, then RST NO_ERROR" (fun () ->
          (* The handler catches the exception of the read: the queue refuses
             its response, which is abandoned. *)
          let record, events = log () in
          let handler req =
            match Body.to_string (Request.body req) with
            | s ->
                Response.string s
            | exception Body.Truncated ->
                (* A second read raises at once. *)
                let t0 = Time.now Mono in
                ( match Body.to_string (Request.body req) with
                | _ ->
                    record "read"
                | exception Body.Truncated ->
                    record
                      (sprintf "Truncated after %Ld ns"
                         (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) ) ) ;
                Response.make (hook record "late" (Body.of_string "late"))
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    let t0 = Time.now Mono in
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    let frames = Fake.frames_until c is_rst in
                    ( List.map status_of frames
                    , briefs frames
                    , Span.to_ns (Time.diff Mono (Time.now Mono) t0) ) ) )
          in
          equal
            Testable.(triple (list string) string int64)
            ( ["408"; "-"]
            , "HEADERS 1 END_STREAM\nRST_STREAM 1 NO_ERROR"
            , Span.to_ns config.read_timeout )
            r.value ;
          equal
            Testable.(list string)
            ["Truncated after 0 ns"; "late Abandoned 0"]
            (events ()) ;
          clean_end r ) ]

(* The rules that the tests of the group "Engine" do not examine. *)

(* [read_all flow] reads [flow] to its end. *)
let read_all flow =
  let buf = Bytes.create 4096 and out = Buffer.create 64 in
  let rec loop () =
    match Flow.read flow buf 0 4096 with
    | 0 ->
        Buffer.contents out
    | n ->
        Buffer.add_subbytes out buf 0 n ;
        loop ()
  in
  loop ()

let rule_tests =
  group "Engine rules"
    [ test "the default settings in our SETTINGS frame" (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve ~config:{config with max_header_bytes= 1234} sim hello
                  (fun c -> Fake.server_settings c ) )
          in
          equal
            Testable.(list (pair int int))
            [(0x1, 4096); (0x3, 100); (initial_window_size, 65535); (0x6, 1234)]
            r.value ;
          clean_end r )
    ; test "W serves the streams in turn: one DATA frame of each" (fun () ->
          let handler =
            paths
              [ ("/a", one_write (String.make 50_000 'a'))
              ; ("/b", one_write (String.make 50_000 'b')) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 0)]
                  sim handler
                  (fun c ->
                    open_window c ;
                    Fake.request c ~stream:1 (request ~path:"/a" ()) ;
                    Fake.request c ~stream:3 (request ~path:"/b" ()) ;
                    ignore
                      (Fake.frames_until c (function
                        | Headers {stream= 3; _} ->
                            true
                        | _ ->
                            false )) ;
                    Weft_sim.settle sim ;
                    ignore (Fake.take_all c) ;
                    (* The two loans wait; then the two streams get credit. *)
                    Fake.send_all c
                      [ Window_update {stream= 1; increment= 50_000}
                      ; Window_update {stream= 3; increment= 50_000} ] ;
                    let frames =
                      Fake.frames_until c (function
                        | Data {stream= 3; end_stream= true; _} ->
                            true
                        | _ ->
                            false )
                    in
                    String.concat " "
                      (List.filter_map
                         (function
                           | Frame.Data {stream; data; _} when data <> "" ->
                               Some (sprintf "%d%c" stream data.[0])
                           | _ ->
                               None )
                         frames ) ) )
          in
          equal Testable.string "1a 3b 1a 3b 1a 3b 1a 3b" r.value ;
          clean_end r )
    ; test "a HEADERS frame that depends on its own stream: RST, decoded"
        (fun () ->
          let handler req =
            Response.string
              (Option.value
                 (Headers.find "x-dyn" (Request.headers req))
                 ~default:"none" )
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.send c
                      (headers_frame ~end_stream:true
                         ~priority:{exclusive= false; dependency= 1; weight= 1}
                         1
                         (dynamic_entry ^ plain get) ) ;
                    let r1 = show (Fake.next c) in
                    Fake.send c
                      (headers_frame ~end_stream:true 3 (uses_entry get)) ;
                    [r1; (Fake.response c ~stream:3).body] ) )
          in
          equal
            Testable.(list string)
            ["RST_STREAM 1 PROTOCOL_ERROR"; "yes"]
            r.value ;
          clean_end r )
    ; test "HEADERS on a half-closed stream: RST STREAM_CLOSED, no cancel"
        (fun () ->
          let record, events = log () in
          let release = Ivar.create () in
          let handler _ =
            sync (Ivar.read release) ;
            Response.make (hook record "late" (Body.of_string "late"))
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request c ~stream:1 get ;
                    Fake.send c
                      (headers_frame ~end_stream:true 1 (plain [("x-t", "1")])) ;
                    let rst = show (Fake.next c) in
                    Ivar.fill release () ; Weft_sim.settle sim ; rst ) )
          in
          equal Testable.string "RST_STREAM 1 STREAM_CLOSED" r.value ;
          equal Testable.(list string) ["late Abandoned 0"] (events ()) ;
          clean_end r )
    ; test
        "a write of W that times out: each fiber is cancelled, the engine \
         returns" (fun () ->
          let record, events = log () in
          let handler =
            paths
              [ ( "/big"
                , fun _ ->
                    Response.make
                      (hook record "big"
                         (Body.of_string (String.make (1024 * 1024) 'x')) ) )
              ; ( "/wait"
                , fun _ ->
                    match sync never with
                    | () ->
                        Response.string "never"
                    | exception (Fiber.Cancelled _ as e) ->
                        record "wait cancelled" ; raise e ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve_with ~settings:opened sim handler (fun c e ->
                    open_window c ;
                    let t0 = Time.now Mono in
                    Fake.request c ~stream:1 (request ~path:"/wait" ()) ;
                    Fake.pause_reading c ;
                    Fake.request c ~stream:3 (request ~path:"/big" ()) ;
                    e.await_engine () ;
                    let d = since t0 in
                    Fake.resume_reading c ; d ) )
          in
          equal Testable.int64 (Span.to_ns config.write_timeout) r.value ;
          equal
            Testable.(list string)
            (* The producer gave 81920 bytes (5 chunks of 16 KiB) before W
               waited: the socket of the simulator and the buffer of W hold
               them. *)
            ["big Failed Weft_flow__Flow.Closed 81920"; "wait cancelled"]
            (List.sort compare (events ())) ;
          clean_end ~error_end:true r )
    ; test "the date field: on a response and on a head of the engine"
        (fun () ->
          let handler = paths [("/raise", fun _ -> failwith "x")] in
          let r =
            Sim_support.run (fun sim ->
                serve ~config:{config with date_header= true} sim handler
                  (fun c ->
                    Fake.request c ~stream:1 get ;
                    Fake.request c ~stream:3 (request ~path:"/raise" ()) ;
                    List.map
                      (fun stream ->
                        Option.is_some
                          (List.assoc_opt "date" (Fake.response c ~stream).head) )
                      [1; 3] ) )
          in
          equal Testable.(list bool) [true; true] r.value ;
          equal reports ["Failure(\"x\")"] r.reports )
    ; test "a handler that raises Body.Truncated itself: an error" (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve sim
                  (fun _ -> raise Body.Truncated)
                  (fun c ->
                    Fake.request c ~stream:1 get ;
                    show_response (Fake.response c ~stream:1) ) )
          in
          equal Testable.string "head :status=500 content-length=0" r.value ;
          equal reports ["Caravan__Body.Truncated"] r.reports )
    ; test
        "a body over max_body with a writer that waits: its write raises at \
         once" (fun () ->
          let record, events = log () in
          let handler _ =
            Response.make
              (hook record "body"
                 (Body.of_writer (fun w ->
                      match Body.Writer.write_string w "abc" with
                      | () ->
                          ()
                      | exception e ->
                          record (Printexc.to_string e) ;
                          raise e ) ) )
          in
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 0)]
                  ~config:{config with max_body= Some 1000}
                  sim handler
                  (fun c ->
                    let t0 = Time.now Mono in
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    ignore (Fake.next c) ;
                    Fake.data c ~stream:1 (String.make 1001 'x') ;
                    let r = show_response (Fake.response c ~stream:1) in
                    Weft_sim.settle sim ;
                    (r, Span.to_ns (Time.diff Mono (Time.now Mono) t0)) ) )
          in
          equal Testable.(pair string int64) ("head ; reset CANCEL", 0L) r.value ;
          equal
            Testable.(list string)
            ["Weft_flow__Flow.Closed"; "body Failed Weft_flow__Flow.Closed 0"]
            (events ()) ;
          clean_end r )
    ; test "max_body: a body of max_body bytes, and no limit" (fun () ->
          let handler req =
            Response.string
              (string_of_int
                 (String.length (Body.to_string (Request.body req))) )
          in
          let run max_body size =
            Sim_support.run (fun sim ->
                serve ~config:{config with max_body} sim handler (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fake.data ~end_stream:true c ~stream:1
                      (String.make size 'x') ;
                    (Fake.response c ~stream:1).body ) )
          in
          equal Testable.string "1000" (run (Some 1000) 1000).value ;
          equal Testable.string "20000" (run None 20000).value )
    ; test "a write from a shield after a reset or an error raises at once"
        (fun () ->
          (* The producer writes from a shield after the event: the cancel of
             the fiber does not reach it. *)
          let run ?error_end event =
            let record, events = log () in
            let release = Ivar.create () in
            let handler _ =
              Response.stream (fun w ->
                  Body.Writer.write_string w "a" ;
                  Body.Writer.flush w ;
                  Fiber.shield (fun () ->
                      sync (Ivar.read release) ;
                      let t0 = Time.now Mono in
                      match Body.Writer.write_string w "b" with
                      | () ->
                          record "written"
                      | exception e ->
                          record
                            (sprintf "%s after %Ld ns" (Printexc.to_string e)
                               (since t0) ) ) )
            in
            let r =
              Sim_support.run (fun sim ->
                  serve sim handler (fun c ->
                      Fake.request c ~stream:1 get ;
                      ignore (Fake.frames_until c is_data) ;
                      event c ;
                      Weft_sim.settle sim ;
                      Ivar.fill release () ;
                      Weft_sim.settle sim ) )
            in
            clean_end ?error_end r ; events ()
          in
          let closed = ["Weft_flow__Flow.Closed after 0 ns"] in
          equal
            Testable.(list string)
            closed
            (run (fun c -> Fake.reset c ~stream:1 Cancel)) ;
          (* A DATA frame on an idle stream: a connection error. *)
          equal
            Testable.(list string)
            closed
            (run ~error_end:true (fun c -> Fake.send c (data 3 "x"))) )
    ; test "early responses: 421, 413, then RST NO_ERROR; a malformed request"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve ~config:{config with max_body= Some 10} sim hello
                  (fun c ->
                    Fake.request c ~stream:1
                      [ (":method", "GET")
                      ; (":scheme", "foo")
                      ; (":path", "/")
                      ; (":authority", "a") ] ;
                    let r1 =
                      String.concat " "
                        (List.map status_of (Fake.frames_until c (ends 1)))
                    in
                    Fake.request ~end_stream:false c ~stream:3
                      (post ~length:11 ()) ;
                    let r3 = Fake.frames_until c is_rst in
                    Fake.request c ~stream:5 (get @ [("X-Up", "1")]) ;
                    let r5 = show (Fake.next c) in
                    Fake.request ~end_stream:true c ~stream:7
                      (post ~length:5 ()) ;
                    let r7 = show (Fake.next c) in
                    [ r1
                    ; String.concat " " (List.map status_of r3)
                    ; briefs r3
                    ; r5
                    ; r7 ] ) )
          in
          equal
            Testable.(list string)
            [ "421"
            ; "413 -"
            ; "HEADERS 3 END_STREAM\nRST_STREAM 3 NO_ERROR"
            ; "RST_STREAM 5 PROTOCOL_ERROR"
            ; "RST_STREAM 7 PROTOCOL_ERROR" ]
            r.value ;
          clean_end r )
    ; test "malformed trailers: RST PROTOCOL_ERROR, Body.Truncated" (fun () ->
          let record, events = log () in
          let handler req =
            ( match Body.to_string (Request.body req) with
            | s ->
                record s
            | exception Body.Truncated ->
                record "Truncated" ) ;
            hello req
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fake.data c ~stream:1 "abc" ;
                    Fake.send c
                      (headers_frame ~end_stream:true 1 (plain [("X-T", "1")])) ;
                    show_response (Fake.response c ~stream:1) ) )
          in
          equal Testable.string "head ; reset PROTOCOL_ERROR" r.value ;
          equal Testable.(list string) ["Truncated"] (events ()) ;
          clean_end r )
    ; test
        "a producer with no output: the head with END_STREAM, or with trailers"
        (fun () ->
          let handler =
            paths
              [ ("/none", fun _ -> Response.stream (fun _ -> ()))
              ; ( "/trailers"
                , fun _ ->
                    Response.stream (fun w ->
                        Body.Writer.set_trailers w
                          (Headers.of_list [("x-t", "1")]) ) ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    List.map
                      (fun (stream, path) ->
                        Fake.request c ~stream (request ~path ()) ;
                        briefs (Fake.frames_until c (ends stream)) )
                      [(1, "/none"); (3, "/trailers")] ) )
          in
          equal
            Testable.(list string)
            ["HEADERS 1 END_STREAM"; "HEADERS 3\nHEADERS 3 END_STREAM"]
            r.value ;
          clean_end r )
    ; test "a body that is not its declared length: RST or 500, a report"
        (fun () ->
          let handler =
            paths
              [ ( "/short"
                , fun _ ->
                    Response.stream ~length:10 (fun w ->
                        Body.Writer.write_string w "12345" ) )
              ; ( "/split"
                , fun _ ->
                    Response.stream ~length:3 (fun w ->
                        Body.Writer.write_string w "12" ;
                        Body.Writer.write_string w "345" ) )
              ; ( "/long"
                , fun _ ->
                    Response.stream ~length:3 (fun w ->
                        Body.Writer.write_string w "12345" ) ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    List.map
                      (fun (stream, path) ->
                        Fake.request c ~stream (request ~path ()) ;
                        show_response (Fake.response c ~stream) )
                      [(1, "/short"); (3, "/long"); (5, "/split")] ) )
          in
          equal
            Testable.(list string)
            [ "head :status=200 content-length=10; body 12345; reset \
               INTERNAL_ERROR"
            ; "head :status=500 content-length=0"
            ; "head :status=200 content-length=3; body 12; reset INTERNAL_ERROR"
            ]
            r.value ;
          equal reports
            (List.init 3 (fun _ ->
                 "response body length differs from its Content-Length" ) )
            r.reports )
    ; test "a close of the body that raises after a complete body: a report"
        (fun () ->
          let handler _ =
            let sent = ref false in
            Response.make
              (Body.of_source ~length:2
                 ~close:(fun () -> failwith "close")
                 (fun buf off _ ->
                   if !sent then 0
                   else begin
                     sent := true ;
                     Bytes.blit_string "ok" 0 buf off 2 ;
                     2
                   end ) )
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request c ~stream:1 get ;
                    show_response (Fake.response c ~stream:1) ) )
          in
          equal Testable.string "head :status=200 content-length=2; body ok"
            r.value ;
          equal reports ["Failure(\"close\")"] r.reports )
    ; test "the read timeout after the head: RST CANCEL at once" (fun () ->
          let release = Ivar.create () in
          (* The handler catches the exception of the read, then waits: the RST
             does not wait for its end. *)
          let handler req =
            Response.stream (fun w ->
                Body.Writer.flush w ;
                match Body.to_string (Request.body req) with
                | s ->
                    Body.Writer.write_string w s
                | exception Body.Truncated ->
                    sync (Ivar.read release) )
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    let r = show_response (Fake.response c ~stream:1) in
                    Ivar.fill release () ; r ) )
          in
          equal Testable.string "head :status=200; reset CANCEL" r.value ;
          clean_end r )
    ; test "the end of the input during a body: RST CANCEL after the head"
        (fun () ->
          let release = Ivar.create () in
          let handler _ =
            Response.stream (fun w ->
                Body.Writer.write_string w "a" ;
                Body.Writer.flush w ;
                sync (Ivar.read release) )
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request c ~stream:1 get ;
                    ignore (Fake.frames_until c is_data) ;
                    Fake.close c ;
                    show (Fake.next c) ) )
          in
          equal Testable.string "RST_STREAM 1 CANCEL" r.value ;
          clean_end r )
    ; test "expect 100-continue: no 100 after DATA or after the head" (fun () ->
          let release = Ivar.create () in
          let handler =
            paths
              [ ( "/late"
                , fun req ->
                    sync (Ivar.read release) ;
                    Response.string (Body.to_string (Request.body req)) )
              ; ( "/head"
                , fun req ->
                    Response.stream (fun w ->
                        Body.Writer.flush w ;
                        Body.Writer.write_string w
                          (Body.to_string (Request.body req)) ) ) ]
          in
          let expect = [("expect", "100-continue")] in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    (* DATA before the first read: the read that waits for more
                       sends no 100. *)
                    Fake.request ~end_stream:false c ~stream:1
                      (post ~path:"/late" ~fields:expect ()) ;
                    Fake.data c ~stream:1 "a" ;
                    Weft_sim.settle sim ;
                    Ivar.fill release () ;
                    Weft_sim.settle sim ;
                    Fake.data ~end_stream:true c ~stream:1 "b" ;
                    let r1 = show_response (Fake.response c ~stream:1) in
                    Fake.request ~end_stream:false c ~stream:3
                      (post ~path:"/head" ~fields:expect ()) ;
                    let head = status_of (Fake.next c) in
                    Fake.data ~end_stream:true c ~stream:3 "cd" ;
                    [r1; head; show_response (Fake.response c ~stream:3)] ) )
          in
          equal
            Testable.(list string)
            [ "head :status=200 content-type=text/plain; charset=utf-8 \
               content-length=2; body ab"
            ; "200"
            ; "head ; body cd" ]
            r.value ;
          clean_end r )
    ; test "empty and 1-byte DATA frames: the body is intact" (fun () ->
          let handler req =
            Response.string (Body.to_string (Request.body req))
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fake.data c ~stream:1 "" ;
                    Fake.data c ~stream:1 "a" ;
                    Fake.data ~end_stream:true c ~stream:1 "bc" ;
                    let one = (Fake.response c ~stream:1).body in
                    (* One byte, then the end in an empty frame. *)
                    Fake.request ~end_stream:false c ~stream:3 (post ()) ;
                    Fake.data c ~stream:3 "a" ;
                    Weft_sim.settle sim ;
                    Fake.data ~end_stream:true c ~stream:3 "" ;
                    (one, (Fake.response c ~stream:3).body) ) )
          in
          equal Testable.(pair string string) ("abc", "a") r.value ;
          clean_end r )
    ; test "a PING gets its ACK" (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve sim hello (fun c ->
                    Fake.send c (Ping {ack= false; data= "12345678"}) ;
                    show (Fake.next c) ) )
          in
          equal Testable.string {|PING ACK "12345678"|} r.value ;
          clean_end r )
    ; test
        "a WINDOW_UPDATE overflow: RST FLOW_CONTROL_ERROR, a cancel, the loan \
         goes" (fun () ->
          let record, events = log () in
          let handler =
            paths
              [ ( "/loan"
                , fun _ ->
                    Response.make
                      (hook record "loan"
                         (Body.of_string (String.make 100 'x')) ) )
              ; ( "/wait"
                , fun _ ->
                    Response.stream (fun w ->
                        Body.Writer.flush w ;
                        match sync never with
                        | () ->
                            ()
                        | exception (Fiber.Cancelled _ as e) ->
                            record "wait cancelled" ; raise e ) ) ]
          in
          let overflow c stream =
            Fake.send_all c
              [ Window_update {stream; increment= 1}
              ; Window_update {stream; increment= 0x7fff_ffff} ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 10)]
                  sim handler
                  (fun c ->
                    (* Stream 1: a loan waits for credit. *)
                    Fake.request c ~stream:1 (request ~path:"/loan" ()) ;
                    ignore (Fake.frames_until c is_data) ;
                    Weft_sim.settle sim ;
                    overflow c 1 ;
                    let rst1 = show (Fake.next c) in
                    (* Stream 3: the handler waits, and the reset cancels it. *)
                    Fake.request c ~stream:3 (request ~path:"/wait" ()) ;
                    ignore (Fake.next c) ;
                    overflow c 3 ;
                    let rst3 = show (Fake.next c) in
                    Weft_sim.settle sim ;
                    (rst1, rst3, events ()) ) )
          in
          equal
            Testable.(triple string string (list string))
            ( "RST_STREAM 1 FLOW_CONTROL_ERROR"
            , "RST_STREAM 3 FLOW_CONTROL_ERROR"
            , ["loan Failed Weft_flow__Flow.Closed 0"; "wait cancelled"] )
            r.value ;
          clean_end r )
    ; test "a flush waits until W wrote the bytes to the flow" (fun () ->
          let g = gate () and record, events = log () in
          (* W wrote the bytes of the loan, but did not flush them: the gate
             stops its flush. *)
          let handler _ =
            Response.stream (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                record "flushed" )
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~wrap:(gated g) sim handler (fun c ->
                    close_gate g ;
                    Fake.request c ~stream:1 get ;
                    Weft_sim.settle sim ;
                    let before = events () in
                    open_gate g ;
                    let body = (Fake.response c ~stream:1).body in
                    (before, body) ) )
          in
          equal Testable.(pair (list string) string) ([], "abc") r.value ;
          equal Testable.(list string) ["flushed"] (events ()) ;
          clean_end r )
    ; test "a flush before the first write sends the head" (fun () ->
          let release = Ivar.create () in
          let handler _ =
            Response.stream (fun w ->
                Body.Writer.flush w ;
                sync (Ivar.read release) )
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request c ~stream:1 get ;
                    let head = brief (Fake.next c) in
                    Ivar.fill release () ;
                    ignore (Fake.response c ~stream:1) ;
                    head ) )
          in
          equal Testable.string "HEADERS 1" r.value ;
          clean_end r )
    ; test "W does not flush each DATA frame, but a round with a PING ACK"
        (fun () ->
          let g = gate () and go = Ivar.create () in
          let handler _ =
            Response.stream (fun w ->
                (* After a flush, W does not flush each round. *)
                Body.Writer.flush w ;
                sync (Ivar.read go) ;
                Body.Writer.write_string w (String.make (128 * 1024) 'x') )
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~settings:opened ~wrap:(gated g) sim handler (fun c ->
                    open_window c ;
                    Weft_sim.settle sim ;
                    let skip = List.length !(g.calls) in
                    Fake.request c ~stream:1 get ;
                    ignore (Fake.next c) ;
                    (* W waits in the write of a full buffer, in the loan. *)
                    close_gate g ;
                    Ivar.fill go () ;
                    Weft_sim.settle sim ;
                    Fake.send c (Ping {ack= false; data= "12345678"}) ;
                    Weft_sim.settle sim ;
                    open_gate g ;
                    ignore (Fake.response c ~stream:1) ;
                    Weft_sim.settle sim ;
                    List.map
                      (fun l -> String.concat "," (List.map brief l))
                      (frames_by_call ~skip g) ) )
          in
          equal
            Testable.(list string)
            [ "HEADERS 1"
            ; "DATA 1 16384,DATA 1 16384,DATA 1 16384"
            ; {|DATA 1 16384,PING ACK "12345678",DATA 1 16384|}
            ; "DATA 1 16384,DATA 1 16384,DATA 1 16384"
            ; "DATA 1 0 END_STREAM" ]
            r.value ;
          clean_end r )
    ; test "a connection error: the engine lingers until the client closes"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve_with sim hello (fun c e ->
                    Fake.send c (data 1 "x") ;
                    let goaway = show (Fake.next c) in
                    Weft_sim.settle sim ;
                    (goaway, e.engine_ended ()) ) )
          in
          equal
            Testable.(pair string bool)
            ({|GOAWAY 0 PROTOCOL_ERROR "DATA on an idle stream"|}, false)
            r.value ;
          clean_end r )
    ; test "a connection error: the linger ends at linger_timeout" (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve_with sim hello (fun c e ->
                    let t0 = Time.now Mono in
                    Fake.send c (data 1 "x") ;
                    ignore (Fake.next c) ;
                    e.await_engine () ;
                    since t0 ) )
          in
          equal Testable.int64 (Span.to_ns config.linger_timeout) r.value )
    ; test
        "a connection error while W waits: the last write ends at \
         linger_timeout" (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve_with ~settings:opened sim
                  (fun _ -> Response.string (String.make (1024 * 1024) 'x'))
                  (fun c e ->
                    open_window c ;
                    Fake.request c ~stream:1 get ;
                    ignore (Fake.next c) ;
                    Fake.pause_reading c ;
                    Weft_sim.settle sim ;
                    let t0 = Time.now Mono in
                    Fake.send c (data 3 "x") ;
                    e.await_engine () ;
                    let d = since t0 in
                    Fake.resume_reading c ; d ) )
          in
          equal Testable.int64 (Span.to_ns config.linger_timeout) r.value )
    ; test
        "the end of the input without cancel_on_disconnect: the body fails, \
         the response goes" (fun () ->
          let record, events = log () in
          let handler req =
            ( match Body.to_string (Request.body req) with
            | _ ->
                record "read"
            | exception Body.Truncated ->
                record "Truncated" ) ;
            Response.string "x"
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~config:{config with cancel_on_disconnect= false}
                  sim handler (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fake.data c ~stream:1 "ab" ;
                    Weft_sim.settle sim ;
                    Fake.close c ;
                    (Fake.response c ~stream:1).body ) )
          in
          equal Testable.string "x" r.value ;
          equal Testable.(list string) ["Truncated"] (events ()) ;
          clean_end r )
    ; test "a writer fiber that the producer cancels: its loan does not go"
        (fun () ->
          (* The test orders the steps: each loan waits in the queue (a zero
             window) before the next step. *)
          let second = Ivar.create () and cancel = Ivar.create () in
          let handler _ =
            Response.stream (fun w ->
                Fiber.scope (fun s ->
                    let a =
                      Fiber.fork_promise s (fun () ->
                          Body.Writer.write_string w "AAAA" )
                    in
                    sync (Ivar.read second) ;
                    (* A second loan waits behind the first one. *)
                    let c =
                      Fiber.fork_promise s (fun () ->
                          Body.Writer.write_string w "CCCC" )
                    in
                    sync (Ivar.read cancel) ;
                    Promise.cancel a ;
                    (try Promise.await a with Promise.Cancelled -> ()) ;
                    Promise.await c ) ;
                Body.Writer.write_string w "BBBB" )
          in
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 0)]
                  sim handler
                  (fun c ->
                    Fake.request c ~stream:1 get ;
                    ignore (Fake.next c) ;
                    Weft_sim.settle sim ;
                    Ivar.fill second () ;
                    Weft_sim.settle sim ;
                    Ivar.fill cancel () ;
                    Weft_sim.settle sim ;
                    Fake.send c (Window_update {stream= 1; increment= 100}) ;
                    (Fake.response c ~stream:1).body ) )
          in
          equal Testable.string "CCCCBBBB" r.value ;
          clean_end r )
    ; test "a client reset of an early response that waits: no frame after it"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve ~settings:opened sim
                  (paths [("/big", big)])
                  (fun c ->
                    briefs
                      (with_writer_blocked sim c (fun () ->
                           Fake.request c ~stream:3
                             (request ~fields:[("expect", "foo")] ()) ;
                           Weft_sim.settle sim ;
                           Fake.reset c ~stream:3 Cancel ) ) ) )
          in
          equal Testable.string "" r.value ;
          clean_end r )
    ; test "a connection error in the first write: SETTINGS goes first"
        (fun () ->
          Sim_support.run (fun sim ->
              Sim_support.with_pair sim (fun client server ->
                  Fiber.scope (fun s ->
                      let flow = Tcp.flow client in
                      let engine =
                        Sim_support.call s (fun () ->
                            H2_server.serve (ctx config) (Tcp.flow server) hello )
                      in
                      (* The preface, the SETTINGS frame and a DATA frame on an
                         idle stream in one write: R reads them with no wait. *)
                      let w = Writer.create flow and r = Reader.create flow in
                      Writer.write_string w Frame.preface ;
                      Frame.write w (Settings {ack= false; settings= []}) ;
                      Frame.write w (data 1 "x") ;
                      Writer.flush w ;
                      let rec frames acc =
                        match Frame.read r ~max_size:16384 with
                        | Goaway _ as f ->
                            List.rev (f :: acc)
                        | f ->
                            frames (f :: acc)
                      in
                      let got = List.map brief (frames []) in
                      Flow.shutdown_send flow ;
                      Sim_support.await engine ;
                      equal
                        Testable.(list string)
                        [ "SETTINGS [0x1=4096; 0x3=100; 0x4=65535; 0x6=16384]"
                        ; "WINDOW_UPDATE 0 +6487965"
                        ; "SETTINGS ACK []"
                        ; {|GOAWAY 0 PROTOCOL_ERROR "DATA on an idle stream"|}
                        ]
                        got ) ) ) )
    ; test "the end of the body drops a loan of another fiber of the producer"
        (fun () ->
          let record, events = log () in
          let outer = ref None and lent = Ivar.create () in
          (* A fiber of the producer in a scope of the test: its write waits for
             window when the producer returns. *)
          let handler _ =
            Response.stream (fun w ->
                Fiber.fork (Option.get !outer) (fun () ->
                    Ivar.fill lent () ;
                    match Body.Writer.write_string w "late" with
                    | () ->
                        record "written"
                    | exception e ->
                        record (Printexc.to_string e) ) ;
                sync (Ivar.read lent) )
          in
          let r =
            Sim_support.run (fun sim ->
                Fiber.scope (fun s ->
                    outer := Some s ;
                    serve
                      ~settings:[(initial_window_size, 0)]
                      sim handler
                      (fun c ->
                        Fake.request c ~stream:1 get ;
                        let frames = Fake.frames_until c (ends 1) in
                        Fake.send c (Window_update {stream= 1; increment= 100}) ;
                        Weft_sim.settle sim ;
                        briefs (frames @ Fake.take_all c) ) ) )
          in
          (* The body is not complete: RST_STREAM INTERNAL_ERROR in place of
             END_STREAM. *)
          equal Testable.string "HEADERS 1\nRST_STREAM 1 INTERNAL_ERROR" r.value ;
          equal Testable.(list string) ["Weft_flow__Flow.Closed"] (events ()) ;
          clean_end r )
    ; test "a write after the 408 of a read timeout: Flow.Closed, no report"
        (fun () ->
          let handler req =
            Response.stream (fun w ->
                match Body.to_string (Request.body req) with
                | s ->
                    Body.Writer.write_string w s
                | exception Body.Truncated ->
                    Body.Writer.write_string w "x" )
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    List.map status_of (Fake.frames_until c is_rst) ) )
          in
          equal Testable.(list string) ["408"; "-"] r.value ;
          clean_end r )
    ; test "each reset fails the request body for a reader in another fiber"
        (fun () ->
          (* Our RST_STREAM CANCEL of a write timeout: the reader fiber of the
             handler does not wait for its read_timeout of 1 s. *)
          let record, events = log () in
          let duplex req =
            Response.stream (fun w ->
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        let t0 = Time.now Mono in
                        match Body.to_string (Request.body req) with
                        | _ ->
                            record "read"
                        | exception Body.Truncated ->
                            record (sprintf "Truncated after %Ld ns" (since t0)) ) ;
                    try Body.Writer.write_string w "x" with Flow.Timeout -> () ) )
          in
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~config:{config with read_timeout= Span.s 1}
                  ~settings:[(initial_window_size, 0)]
                  sim duplex
                  (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    show_response (Fake.response c ~stream:1) ) )
          in
          equal Testable.string "head :status=200; reset CANCEL" r.value ;
          equal
            Testable.(list string)
            [sprintf "Truncated after %Ld ns" (Span.to_ns config.write_timeout)]
            (events ()) ;
          clean_end r ;
          (* A RST_STREAM of the client: a read in a shield, which the cancel of
             the fiber does not reach. *)
          let record, events = log () in
          let shielded req =
            Fiber.shield (fun () ->
                let t0 = Time.now Mono in
                match Body.to_string (Request.body req) with
                | _ ->
                    record "read"
                | exception Body.Truncated ->
                    record (sprintf "Truncated after %Ld ns" (since t0)) ) ;
            hello req
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim shielded (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Weft_sim.settle sim ;
                    Fake.reset c ~stream:1 Cancel ;
                    Weft_sim.settle sim ;
                    (* Before the end of the input. *)
                    events () ) )
          in
          equal Testable.(list string) ["Truncated after 0 ns"] r.value ;
          clean_end r )
    ; test "DATA after a read timeout gives its credit back at once" (fun () ->
          let release = Ivar.create () in
          let handler req =
            match Body.to_string (Request.body req) with
            | s ->
                Response.string s
            | exception Body.Truncated ->
                sync (Ivar.read release) ;
                hello req
          in
          (* Two streams: a connection window of 131070, with a WINDOW_UPDATE at
             65535 bytes of credit. *)
          let h2 = {Settings.default with max_concurrent_streams= 2} in
          let r =
            Sim_support.run (fun sim ->
                serve ~h2 sim handler (fun c ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    ignore (Fake.response c ~stream:1) ;
                    Fake.data c ~stream:1 (String.make 65535 'x') ;
                    Weft_sim.settle sim ;
                    let frames = Fake.take_all c in
                    Ivar.fill release () ;
                    List.exists
                      (fun f ->
                        show f
                        = show
                            (Frame.Window_update {stream= 0; increment= 65535}) )
                      frames ) )
          in
          equal Testable.bool true r.value ;
          clean_end r )
    ; test "an error of a producer after its cancel is reported" (fun () ->
          (* The end of the input cancels the fiber (cancel_on_disconnect). The
             producer catches its cancel and raises an error of its own. *)
          let handler _ =
            Response.stream (fun w ->
                Body.Writer.write_string w "a" ;
                Body.Writer.flush w ;
                match sync never with
                | () ->
                    ()
                | exception Fiber.Cancelled _ ->
                    failwith "after the cancel" )
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request c ~stream:1 get ;
                    ignore (Fake.frames_until c is_data) ) )
          in
          equal reports ["Failure(\"after the cancel\")"] r.reports )
    ; test "1-byte DATA frames: the memory of the body stays in the window"
        (fun () ->
          let release = Ivar.create () in
          let handler req =
            sync (Ivar.read release) ;
            hello req
          in
          let frame = H2_support.raw ~kind:0 ~stream:1 "x" in
          let r =
            Sim_support.run (fun sim ->
                serve_with sim handler (fun c e ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fake.send_raw c
                      (String.concat "" (List.init 65535 (fun _ -> frame))) ;
                    Weft_sim.settle sim ;
                    let bytes = e.probe.inbound () in
                    Ivar.fill release () ; bytes ) )
          in
          equal Testable.int 65535 r.value ;
          clean_end r )
    ; test "a round of W takes at most 4 DATA parts" (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve_with
                  ~settings:[(initial_window_size, 0)]
                  sim
                  (one_write (String.make 65536 'x'))
                  (fun c e ->
                    open_window c ;
                    let streams = List.init 8 (fun i -> (2 * i) + 1) in
                    List.iter (fun stream -> Fake.request c ~stream get) streams ;
                    (* The 8 loans wait; then they all get credit at once. *)
                    Weft_sim.settle sim ;
                    Fake.send_all c
                      (List.map
                         (fun stream ->
                           Frame.Window_update {stream; increment= 65536} )
                         streams ) ;
                    List.iter
                      (fun stream -> ignore (Fake.response c ~stream))
                      streams ;
                    e.probe.scratch () ) )
          in
          equal Testable.int 4 r.value ;
          clean_end r )
    ; test
        "END_STREAM of the client after our queued END_STREAM closes the stream"
        (fun () ->
          (* The state closes the stream when the client ends its side, before W
             writes our END_STREAM: a later DATA frame on the stream is a
             connection error. *)
          let answered = Ivar.create () in
          let handler =
            paths
              [ ("/big", big)
              ; ( "/204"
                , fun _ ->
                    Ivar.fill answered () ;
                    Response.empty ~status:Status.no_content () ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~settings:opened sim handler (fun c ->
                    open_window c ;
                    Fake.pause_reading c ;
                    Fake.request c ~stream:1 (request ~path:"/big" ()) ;
                    Weft_sim.settle sim ;
                    Fake.request ~end_stream:false c ~stream:3
                      (post ~path:"/204" ()) ;
                    sync (Ivar.read answered) ;
                    Weft_sim.settle sim ;
                    Fake.send_all c [data ~end_stream:true 3 ""; data 3 "x"] ;
                    Weft_sim.settle sim ;
                    Fake.resume_reading c ;
                    show
                      (List.hd
                         (List.rev
                            (Fake.frames_until c (function
                              | Goaway _ ->
                                  true
                              | _ ->
                                  false )) ) ) ) )
          in
          equal Testable.string
            {|GOAWAY 3 STREAM_CLOSED "DATA on a closed stream"|} r.value ;
          clean_end ~error_end:true r )
    ; test "a small response goes in one write" (fun () ->
          (* W yields one time before its idle flush: the stream fiber queues
             the END marker first. *)
          let g = gate () in
          let r =
            Sim_support.run (fun sim ->
                serve ~wrap:(gated g) sim hello (fun c ->
                    Weft_sim.settle sim ;
                    let skip = List.length !(g.calls) in
                    Fake.request c ~stream:1 get ;
                    ignore (Fake.response c ~stream:1) ;
                    Weft_sim.settle sim ;
                    List.map
                      (fun l -> String.concat "," (List.map brief l))
                      (frames_by_call ~skip g) ) )
          in
          equal
            Testable.(list string)
            ["HEADERS 1,DATA 1 5,DATA 1 0 END_STREAM"]
            r.value ;
          clean_end r )
    ; test "the buffer of the request DATA: it moves its bytes, then grows"
        (fun () ->
          let first = Ivar.create () and read = Ivar.create () in
          let release = Ivar.create () in
          let handler req =
            sync (Ivar.read first) ;
            let buf = Bytes.create 500 in
            ignore (Body.read (Request.body req) buf 0 500) ;
            Ivar.fill read () ;
            sync (Ivar.read release) ;
            hello req
          in
          let r =
            Sim_support.run (fun sim ->
                serve_with sim handler (fun c e ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fake.data c ~stream:1 (String.make 1000 'a') ;
                    Weft_sim.settle sim ;
                    let one = e.probe.inbound () in
                    Ivar.fill first () ;
                    sync (Ivar.read read) ;
                    (* 500 bytes unread, at the offset 500: 500 more fit. *)
                    Fake.data c ~stream:1 (String.make 500 'b') ;
                    Weft_sim.settle sim ;
                    let two = e.probe.inbound () in
                    Fake.data c ~stream:1 (String.make 600 'c') ;
                    Weft_sim.settle sim ;
                    let three = e.probe.inbound () in
                    Ivar.fill release () ; [one; two; three] ) )
          in
          equal Testable.(list int) [1000; 1000; 2000] r.value ;
          clean_end r )
    ; test "a body over max_body gives its credit back at once" (fun () ->
          let release = Ivar.create () in
          let handler req =
            match Body.to_string (Request.body req) with
            | _ ->
                hello req
            | exception Body.Too_large ->
                sync (Ivar.read release) ;
                hello req
          in
          let r =
            Sim_support.run (fun sim ->
                serve_with ~config:{config with max_body= Some 1000} sim handler
                  (fun c e ->
                    Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                    Fake.data c ~stream:1 (String.make 1500 'x') ;
                    Weft_sim.settle sim ;
                    let window = Conn_state.receive_window e.probe.state in
                    Ivar.fill release () ; window ) )
          in
          equal Testable.int (100 * 65535) r.value ;
          clean_end r )
    ; test "an early response gives no WINDOW_UPDATE on its stream" (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve ~settings:opened sim
                  (paths [("/big", big)])
                  (fun c ->
                    briefs
                      (with_writer_blocked sim c (fun () ->
                           Fake.request ~end_stream:false c ~stream:3
                             (request ~fields:[("expect", "foo")] ()) ;
                           Weft_sim.settle sim ;
                           (* The state gives credit for these bytes at once. *)
                           Fake.data c ~stream:3 (String.make 40_000 'x') ) ) ) )
          in
          equal Testable.string "HEADERS 3 END_STREAM\nRST_STREAM 3 NO_ERROR"
            r.value ;
          clean_end r )
    ; test
        "a window that ends in the middle of a loan: RST CANCEL after \
         write_timeout" (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 1000)]
                  sim
                  (fun _ -> Response.string (String.make 3000 'x'))
                  (fun c ->
                    let t0 = Time.now Mono in
                    Fake.request c ~stream:1 get ;
                    let frames = Fake.frames_until c (ends 1) in
                    (briefs frames, since t0) ) )
          in
          equal
            Testable.(pair string int64)
            ( "HEADERS 1\nDATA 1 1000\nRST_STREAM 1 CANCEL"
            , Span.to_ns config.write_timeout )
            r.value ;
          clean_end r )
    ; test
        "a window that ends before the first take: RST CANCEL after \
         write_timeout" (fun () ->
          (* The write starts with a window of 10000, and W waits in a write of
             the flow. Then a SETTINGS frame of the client makes the window
             0. *)
          let g = gate () in
          let r =
            Sim_support.run (fun sim ->
                serve
                  ~settings:[(initial_window_size, 10000)]
                  ~wrap:(gated g) sim
                  (fun _ -> Response.string (String.make 3000 'x'))
                  (fun c ->
                    close_gate g ;
                    Fake.send c (Ping {ack= false; data= "12345678"}) ;
                    Weft_sim.settle sim ;
                    Fake.request c ~stream:1 get ;
                    Weft_sim.settle sim ;
                    Fake.send c
                      (Settings
                         {ack= false; settings= [(initial_window_size, 0)]} ) ;
                    Weft_sim.settle sim ;
                    let t0 = Time.now Mono in
                    open_gate g ;
                    let frames = Fake.frames_until c (ends 1) in
                    (briefs frames, since t0) ) )
          in
          equal
            Testable.(pair string int64)
            ( {|PING ACK "12345678"
HEADERS 1
RST_STREAM 1 CANCEL|}
            , Span.to_ns config.write_timeout )
            r.value ;
          clean_end r )
    ; test "the END marker and the trailers of a body close the stream at once"
        (fun () ->
          (* W waits in a flush when the producer ends: our END_STREAM waits in
             the queue. The state closes the stream when the client ends its
             side: a later DATA frame is a connection error. *)
          let run handler =
            let g = gate () in
            let r =
              Sim_support.run (fun sim ->
                  serve ~wrap:(gated g) sim handler (fun c ->
                      Weft_sim.settle sim ;
                      close_gate g ;
                      Fake.request ~end_stream:false c ~stream:1 (post ()) ;
                      Weft_sim.settle sim ;
                      Fake.send_all c [data ~end_stream:true 1 ""; data 1 "x"] ;
                      Weft_sim.settle sim ;
                      open_gate g ;
                      show
                        (List.hd
                           (List.rev
                              (Fake.frames_until c (function
                                | Goaway _ ->
                                    true
                                | _ ->
                                    false )) ) ) ) )
            in
            clean_end ~error_end:true r ;
            r.value
          in
          let goaway = {|GOAWAY 1 STREAM_CLOSED "DATA on a closed stream"|} in
          equal Testable.string goaway
            (run (fun _ ->
                 Response.stream (fun w -> Body.Writer.write_string w "a") ) ) ;
          equal Testable.string goaway
            (run (fun _ ->
                 Response.stream (fun w ->
                     Body.Writer.write_string w "a" ;
                     Body.Writer.set_trailers w (Headers.of_list [("x-t", "1")]) ) )
            ) )
    ; test "one visit of W takes all items of a stream that are not loans"
        (fun () ->
          (* Two responses of a head and trailers wait while W waits in a write
             of the flow. Then the round takes the two items of stream 1, then
             the two of stream 3. *)
          let g = gate () in
          let handler _ =
            Response.stream (fun w ->
                Body.Writer.set_trailers w (Headers.of_list [("x-t", "1")]) )
          in
          let r =
            Sim_support.run (fun sim ->
                serve ~wrap:(gated g) sim handler (fun c ->
                    close_gate g ;
                    Fake.send c (Ping {ack= false; data= "12345678"}) ;
                    Weft_sim.settle sim ;
                    Fake.request c ~stream:1 get ;
                    Fake.request c ~stream:3 get ;
                    Weft_sim.settle sim ;
                    open_gate g ;
                    briefs
                      (Fake.frames_until c (function
                        | Headers {stream= 3; end_stream= true; _} ->
                            true
                        | _ ->
                            false )) ) )
          in
          equal Testable.string
            {|PING ACK "12345678"
HEADERS 1
HEADERS 1 END_STREAM
HEADERS 3
HEADERS 3 END_STREAM|}
            r.value ;
          clean_end r )
    ; test "with two live streams, W yields before its idle flush" (fun () ->
          (* Two responses of one write each, with no known length: the yield
             lets the second one in the flush of the first one. *)
          let g = gate () in
          let r =
            Sim_support.run (fun sim ->
                serve ~wrap:(gated g) sim (one_write "a") (fun c ->
                    Weft_sim.settle sim ;
                    let skip = List.length !(g.calls) in
                    Fake.send_all c
                      [ headers_frame ~end_stream:true 1 (plain get)
                      ; headers_frame ~end_stream:true 3 (plain get) ] ;
                    ignore (Fake.response c ~stream:1) ;
                    ignore (Fake.response c ~stream:3) ;
                    Weft_sim.settle sim ;
                    List.map
                      (fun l -> String.concat "," (List.map brief l))
                      (frames_by_call ~skip g) ) )
          in
          (* Without the yield, each response would end in its own flush: four
             writes. *)
          equal
            Testable.(list string)
            [ "HEADERS 1,DATA 1 1,DATA 1 0 END_STREAM,HEADERS 3,DATA 3 1"
            ; "DATA 3 0 END_STREAM" ]
            r.value ;
          clean_end r )
    ; test "a loan with window waits for W with no timer" (fun () ->
          (* W waits in a write of the flow for 1 s (the gate has no timeout).
             The loan has send window: its writer has no timer. *)
          let g = gate () in
          let r =
            Sim_support.run (fun sim ->
                serve ~wrap:(gated g) sim (one_write "abc") (fun c ->
                    close_gate g ;
                    Fake.send c (Ping {ack= false; data= "12345678"}) ;
                    Weft_sim.settle sim ;
                    Fake.request c ~stream:1 get ;
                    Time.sleep Mono (Span.s 1) ;
                    open_gate g ;
                    show_response (Fake.response c ~stream:1) ) )
          in
          equal Testable.string "head :status=200; body abc" r.value ;
          clean_end r )
    ; test "a wrong preface: no frame, and the engine ends at once" (fun () ->
          Sim_support.run (fun sim ->
              Sim_support.with_pair sim (fun client server ->
                  let flow = Tcp.flow client and server = Tcp.flow server in
                  (* The client does not close: the engine does not linger. *)
                  Flow.write_string flow "GET / HTTP/1.1\r\nHost: a\r\n\r\n" ;
                  let t0 = Time.now Mono in
                  H2_server.serve (ctx config) server hello ;
                  equal Testable.int64 0L (since t0) ;
                  Flow.shutdown_send server ;
                  equal Testable.string "" (read_all flow) ) ) )
    ; test
        "a client that waits for the SETTINGS of the server after its preface"
        (fun () ->
          Sim_support.run (fun sim ->
              Sim_support.with_pair sim (fun client server ->
                  Fiber.scope (fun s ->
                      let flow = Tcp.flow client in
                      let engine =
                        Sim_support.call s (fun () ->
                            H2_server.serve (ctx config) (Tcp.flow server) hello )
                      in
                      let w = Writer.create flow and r = Reader.create flow in
                      Writer.write_string w Frame.preface ;
                      Writer.flush w ;
                      let first = show (Frame.read r ~max_size:16384) in
                      Frame.write w (Settings {ack= false; settings= []}) ;
                      Writer.flush w ;
                      Flow.shutdown_send flow ;
                      Sim_support.await engine ;
                      equal Testable.bool true
                        (String.starts_with ~prefix:"SETTINGS [" first) ) ) ) )
    ]

(* {1 The lifecycle of the connection} *)

(* Short timers for the lifecycle tests. *)
let timed = {config with header_timeout= Span.ms 200; idle_timeout= Span.ms 400}

(* [wait_ivar iv] waits for [iv]. *)
let wait_ivar iv = sync (Ivar.read iv)

let is_goaway = function Frame.Goaway _ -> true | _ -> false

let is_ack = function Frame.Ping {ack; _} -> ack | _ -> false

(* [round_trip c]: the client sends a PING and waits for its ACK. R handled each
   frame before the PING. *)
let round_trip c =
  Fake.send c (Ping {ack= false; data= "12345678"}) ;
  ignore (Fake.frames_until c is_ack)

(* The GOAWAY frame of the client with [code]. *)
let goaway code = Frame.Goaway {last_stream= 0; code; debug= ""}

(* The waits of R in a window where R waits only a few times: a busy loop gives
   many more before the PING round trip of the window ends. *)
let few_waits = 20

(* The simulator does not see a busy loop of R as a livelock: probably it counts
   the mutex or a ready [sync] as progress. Thus [counting ()] gives hooks that
   count the waits of R, and that fail the test at once after [busy_limit]
   entries of R in its steps, its waits and its guarded read. The second and the
   third results give the count of the waits and of the steps. *)
let busy_limit = 10_000

let counting () =
  let entries = Atomic.make 0
  and waits = Atomic.make 0
  and steps = Atomic.make 0 in
  let enter () =
    if Atomic.fetch_and_add entries 1 >= busy_limit then
      raise (Sim_support.Failed "a busy loop of R")
  in
  ( { H2_server.no_hooks with
      on_yield= enter
    ; on_step= (fun () -> enter () ; Atomic.incr steps)
    ; on_wait= (fun () -> enter () ; Atomic.incr waits) }
  , (fun () -> Atomic.get waits)
  , fun () -> Atomic.get steps )

(* A handler that ends in the yield of R: after R read the state of its step
   (one live stream), and before R reads the input. The handler fills [started],
   then waits. After [arm ()], the hook [hooks.on_yield] releases it from the
   second entry of the guarded read: the first entry can read the frame that the
   test sent after the arm. On many domains, the first entry can come before the
   arm, thus the test sends frames until [released ()]. [ended ()] is the time
   of the end of the handler. *)
type release =
  { handler: Request.t -> Response.t
  ; hooks: H2_server.hooks
  ; started: unit Ivar.t
  ; arm: unit -> unit
  ; released: unit -> bool
  ; ended: unit -> Clock.mono }

let release_in_yield () =
  let started = Ivar.create () and release = Ivar.create () in
  let ended = ref None in
  let entries = Atomic.make min_int and released = Atomic.make false in
  let on_yield () =
    if
      Atomic.fetch_and_add entries 1 >= 1
      && Atomic.compare_and_set released false true
    then Ivar.fill release ()
  in
  { handler=
      (fun req ->
        Ivar.fill started () ;
        wait_ivar release ;
        ended := Some (Time.now Mono) ;
        hello req )
  ; hooks= {H2_server.no_hooks with on_yield}
  ; started
  ; arm= (fun () -> Atomic.set entries 0)
  ; released= (fun () -> Atomic.get released)
  ; ended= (fun () -> Option.get !ended) }

let lifecycle_tests =
  group "Engine lifecycle"
    [ test "the end of the input with no stream: the engine returns at once"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve_with sim hello (fun c e ->
                    let t0 = Time.now Mono in
                    Fake.close c ; e.await_engine () ; since t0 ) )
          in
          equal Testable.int64 0L r.value ;
          clean_end r )
    ; test "a stop: GOAWAY with the last stream, the stream ends, then the end"
        (fun () ->
          let stop = Ivar.create () and release = Ivar.create () in
          let handler req = wait_ivar release ; hello req in
          let hooks, waits, _ = counting () in
          let r =
            Sim_support.run (fun sim ->
                serve_with ~stop ~hooks sim handler (fun c e ->
                    Fake.request c ~stream:1 get ;
                    Weft_sim.settle sim ;
                    let waits0 = waits () in
                    let t0 = Time.now Mono in
                    Ivar.fill stop () ;
                    let goaway = show (Fake.next c) in
                    let at = since t0 in
                    (* A new stream after the GOAWAY: no response, but the state
                       of HPACK takes its block. *)
                    let before = e.probe.table_size () in
                    Fake.send c
                      (headers_frame ~end_stream:true 3
                         (dynamic_entry ^ plain get) ) ;
                    round_trip c ;
                    let after = e.probe.table_size () in
                    Ivar.fill release () ;
                    let body = (Fake.response c ~stream:1).body in
                    e.await_engine () ;
                    Weft_sim.settle sim ;
                    ( [ goaway
                      ; Int64.to_string at
                      ; body
                      ; string_of_int before
                      ; string_of_int after ]
                    , Fake.take_all c
                    , waits () - waits0 ) ) )
          in
          let texts, rest, waits = r.value in
          equal
            Testable.(list string)
            [{|GOAWAY 1 NO_ERROR "the server stops"|}; "0"; "hello"; "0"; "40"]
            texts ;
          equal Testable.string "" (briefs rest) ;
          (* R waited at least for the PING, and at most a few times. *)
          equal Testable.bool true (waits >= 1 && waits < few_waits) ;
          clean_end r )
    ; test "a stop before the preface: the connection closes at once" (fun () ->
          Sim_support.run (fun sim ->
              Sim_support.with_pair sim (fun client server ->
                  let stop = Ivar.create () in
                  Ivar.fill stop () ;
                  let t0 = Time.now Mono in
                  H2_server.serve (ctx ~stop config) (Tcp.flow server) hello ;
                  equal Testable.int64 0L (since t0) ;
                  ignore client ) ) )
    ; test
        "an abort during a response: the handler is cancelled, no time passes"
        (fun () ->
          let record, events = log () in
          let handler _ =
            Response.stream (fun w ->
                Body.Writer.write_string w "a" ;
                Body.Writer.flush w ;
                match sync never with
                | () ->
                    ()
                | exception (Fiber.Cancelled _ as e) ->
                    record "cancelled" ; raise e )
          in
          let r =
            Sim_support.run (fun sim ->
                serve_with sim handler (fun c e ->
                    Fake.request c ~stream:1 get ;
                    ignore (Fake.frames_until c is_data) ;
                    let t0 = Time.now Mono in
                    e.abort () ;
                    Weft_sim.settle sim ;
                    (e.engine_ended (), since t0, Fake.take_all c) ) )
          in
          let ended, time, rest = r.value in
          equal Testable.(pair bool int64) (true, 0L) (ended, time) ;
          equal Testable.string "" (briefs rest) ;
          equal Testable.(list string) ["cancelled"] (events ()) )
    ; test "idle: GOAWAY NO_ERROR at idle_timeout after the last stream"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve_with ~config:timed sim
                  (paths
                     [ ( "/slow"
                       , fun req ->
                           Time.sleep Mono (Span.ms 1000) ;
                           hello req ) ] )
                  (fun c e ->
                    (* A stream longer than idle_timeout keeps the
                       connection. *)
                    Fake.request c ~stream:1 (request ~path:"/slow" ()) ;
                    ignore (Fake.response c ~stream:1) ;
                    let t0 = Time.now Mono in
                    (* A PING does not start the idle time again. *)
                    Time.sleep Mono (Span.ms 200) ;
                    Fake.send c (Ping {ack= false; data= "12345678"}) ;
                    ignore (Fake.next c) ;
                    let goaway = show (Fake.next c) in
                    let at = since t0 in
                    e.await_engine () ; (goaway, at) ) )
          in
          equal
            Testable.(pair string int64)
            ({|GOAWAY 1 NO_ERROR "idle"|}, Span.to_ns timed.idle_timeout)
            r.value ;
          clean_end r )
    ; test "idle_timeout shorter than header_timeout: GOAWAY at idle_timeout"
        (fun () ->
          (* The first SETTINGS frame moves the deadline from the preface timer
             to the earlier idle timer: it kicks the watchdog. The client sends
             no frame after it, not even the ACK of our SETTINGS. *)
          Sim_support.run (fun sim ->
              Sim_support.with_pair sim (fun client server ->
                  Fiber.scope (fun s ->
                      let flow = Tcp.flow client in
                      let engine =
                        Sim_support.call s (fun () ->
                            H2_server.serve
                              (ctx {config with idle_timeout= Span.ms 150})
                              (Tcp.flow server) hello )
                      in
                      let t0 = Time.now Mono in
                      let w = Writer.create flow and r = Reader.create flow in
                      Writer.write_string w Frame.preface ;
                      Frame.write w (Settings {ack= false; settings= []}) ;
                      Writer.flush w ;
                      let rec goaway () =
                        match Frame.read r ~max_size:16384 with
                        | Goaway _ as f ->
                            brief f
                        | _ ->
                            goaway ()
                      in
                      let got = goaway () in
                      let at = since t0 in
                      Flow.shutdown_send flow ;
                      Sim_support.await engine ;
                      equal
                        Testable.(pair string int64)
                        ({|GOAWAY 0 NO_ERROR "idle"|}, Span.to_ns (Span.ms 150))
                        (got, at) ) ) ) )
    ; test "a request at the idle deadline: the GOAWAY, or a stale timer"
        (fun () ->
          (* At the idle deadline, the client sends a request. If the watchdog
             runs first and R sees its wake first, the GOAWAY comes, and the
             request is ignored. Else R reads the request first: then R finds
             that no deadline passed, and kicks the watchdog. The idle GOAWAY
             then comes at idle_timeout after the end of the stream. Some seeds
             must give the second order. *)
          let stale = Atomic.make 0 in
          let run sim =
            let ended = ref None in
            let handler req =
              ended := Some (Time.now Mono) ;
              hello req
            in
            let hooks, _, _ = counting () in
            let r =
              serve_with ~config:timed ~hooks sim handler (fun c e ->
                  Time.sleep Mono timed.idle_timeout ;
                  Fake.request c ~stream:1 get ;
                  let frames = Fake.frames_until c is_goaway in
                  let at = Time.now Mono in
                  e.await_engine () ;
                  (briefs frames, at) )
            in
            let frames, at = r.value in
            ( match !ended with
            | None ->
                equal Testable.string {|GOAWAY 0 NO_ERROR "idle"|} frames
            | Some ended ->
                Atomic.incr stale ;
                equal Testable.string
                  "HEADERS 1\n\
                   DATA 1 5\n\
                   DATA 1 0 END_STREAM\n\
                   GOAWAY 1 NO_ERROR \"idle\""
                  frames ;
                equal Testable.int64
                  (Span.to_ns timed.idle_timeout)
                  (Span.to_ns (Time.diff Mono at ended)) ) ;
            clean_end r
          in
          Sim_support.race ~seeds:50 run ;
          equal Testable.bool true (Atomic.get stale > 0) )
    ; test "no preface: the connection closes at header_timeout" (fun () ->
          Sim_support.run (fun sim ->
              Sim_support.with_pair sim (fun client server ->
                  let t0 = Time.now Mono in
                  let server = Tcp.flow server in
                  H2_server.serve (ctx timed) server hello ;
                  equal Testable.int64
                    (Span.to_ns timed.header_timeout)
                    (since t0) ;
                  Flow.shutdown_send server ;
                  equal Testable.string "" (read_all (Tcp.flow client)) ) ) )
    ; test "the preface with no SETTINGS frame: the close at header_timeout"
        (fun () ->
          Sim_support.run (fun sim ->
              Sim_support.with_pair sim (fun client server ->
                  Fiber.scope (fun s ->
                      let flow = Tcp.flow client in
                      let engine =
                        Sim_support.call s (fun () ->
                            H2_server.serve (ctx timed) (Tcp.flow server) hello )
                      in
                      let t0 = Time.now Mono in
                      Flow.write_string flow Frame.preface ;
                      Sim_support.await engine ;
                      equal Testable.int64
                        (Span.to_ns timed.header_timeout)
                        (since t0) ) ) ) )
    ; test "a header block that does not end: GOAWAY ENHANCE_YOUR_CALM"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve ~config:timed sim hello (fun c ->
                    let t0 = Time.now Mono in
                    Fake.send c
                      (headers_frame ~end_stream:true ~end_headers:false 1
                         (plain get) ) ;
                    let goaway = show (Fake.next c) in
                    (goaway, since t0) ) )
          in
          equal
            Testable.(pair string int64)
            ( {|GOAWAY 1 ENHANCE_YOUR_CALM "a header block that did not end in time"|}
            , Span.to_ns timed.header_timeout )
            r.value ;
          clean_end ~error_end:true r )
    ; test "a header block that ends in CONTINUATION: no header block timer"
        (fun () ->
          (* Stream 1 stays live for longer than header_timeout. *)
          let release = Ivar.create () in
          let handler req = wait_ivar release ; hello req in
          let r =
            Sim_support.run (fun sim ->
                serve ~config:timed sim handler (fun c ->
                    let block = plain get in
                    let half = String.length block / 2 in
                    Fake.send c
                      (headers_frame ~end_stream:true ~end_headers:false 1
                         (String.sub block 0 half) ) ;
                    Fake.send c
                      (Continuation
                         { stream= 1
                         ; end_headers= true
                         ; block=
                             String.sub block half (String.length block - half)
                         } ) ;
                    Time.sleep Mono (Span.ms 300) ;
                    Fake.request c ~stream:3 get ;
                    Ivar.fill release () ;
                    let b1 = (Fake.response c ~stream:1).body in
                    let b3 = (Fake.response c ~stream:3).body in
                    (b1, b3, briefs (Fake.take_all c)) ) )
          in
          equal
            Testable.(triple string string string)
            ("hello", "hello", "") r.value ;
          clean_end r )
    ; test "a CONTINUATION frame does not start the header block timer again"
        (fun () ->
          let r =
            Sim_support.run (fun sim ->
                serve ~config:timed sim hello (fun c ->
                    let block = plain get in
                    let t0 = Time.now Mono in
                    Fake.send c
                      (headers_frame ~end_stream:true ~end_headers:false 1
                         (String.sub block 0 1) ) ;
                    Time.sleep Mono (Span.ms 150) ;
                    Fake.send c
                      (Continuation
                         { stream= 1
                         ; end_headers= false
                         ; block= String.sub block 1 1 } ) ;
                    let goaway = show (Fake.next c) in
                    (goaway, since t0) ) )
          in
          equal
            Testable.(pair string int64)
            ( {|GOAWAY 1 ENHANCE_YOUR_CALM "a header block that did not end in time"|}
            , Span.to_ns timed.header_timeout )
            r.value ;
          clean_end ~error_end:true r )
    ; test "a header block that opens no stream fiber: GOAWAY at the idle time"
        (fun () ->
          (* The end of the block moves the deadline from the block timer (at
             500 ms) to the earlier idle timer (at 400 ms): it kicks the
             watchdog. The engine answers 417 with no fiber. *)
          let r =
            Sim_support.run (fun sim ->
                serve ~config:timed sim hello (fun c ->
                    let t0 = Time.now Mono in
                    let block =
                      plain (request ~fields:[("expect", "foo")] ())
                    in
                    let half = String.length block / 2 in
                    Time.sleep Mono (Span.ms 300) ;
                    Fake.send c
                      (headers_frame ~end_stream:true ~end_headers:false 1
                         (String.sub block 0 half) ) ;
                    Time.sleep Mono (Span.ms 50) ;
                    Fake.send c
                      (Continuation
                         { stream= 1
                         ; end_headers= true
                         ; block=
                             String.sub block half (String.length block - half)
                         } ) ;
                    let status = status_of (Fake.next c) in
                    let goaway =
                      show (Fake.frames_until c is_goaway |> List.rev |> List.hd)
                    in
                    (status, goaway, since t0) ) )
          in
          equal
            Testable.(triple string string int64)
            ("417", {|GOAWAY 1 NO_ERROR "idle"|}, Span.to_ns timed.idle_timeout)
            r.value ;
          clean_end r )
    ; test
        "the end of the input with cancel_on_disconnect: the handlers are \
         cancelled" (fun () ->
          let record, events = log () in
          let handler _ =
            match sync never with
            | () ->
                Response.string "never"
            | exception (Fiber.Cancelled _ as e) ->
                record "cancelled" ; raise e
          in
          let r =
            Sim_support.run (fun sim ->
                serve sim handler (fun c ->
                    Fake.request c ~stream:1 get ;
                    Fake.request c ~stream:3 get ;
                    Weft_sim.settle sim ) )
          in
          equal Testable.(list string) ["cancelled"; "cancelled"] (events ()) ;
          clean_end ~error_end:true r )
    ; test "a GOAWAY NO_ERROR of the client: no new stream, the streams end"
        (fun () ->
          let release = Ivar.create () in
          let handler req = wait_ivar release ; hello req in
          let r =
            Sim_support.run (fun sim ->
                serve_with sim handler (fun c e ->
                    Fake.request c ~stream:1 get ;
                    Fake.send c (goaway No_error) ;
                    Fake.request c ~stream:3 get ;
                    Weft_sim.settle sim ;
                    Ivar.fill release () ;
                    let frames = Fake.frames_until c (ends 1) in
                    e.await_engine () ;
                    Weft_sim.settle sim ;
                    briefs (frames @ Fake.take_all c) ) )
          in
          (* Our GOAWAY tells the last stream (RFC 9113 section 6.8). No frame
             of stream 3. *)
          equal Testable.string
            "GOAWAY 1 NO_ERROR \"the client sent GOAWAY\"\n\
             HEADERS 1\n\
             DATA 1 5\n\
             DATA 1 0 END_STREAM"
            r.value ;
          clean_end r )
    ; test "a GOAWAY of the client with an error: the handler is cancelled"
        (fun () ->
          let record, events = log () in
          let handler _ =
            match sync never with
            | () ->
                Response.string "never"
            | exception (Fiber.Cancelled _ as e) ->
                record "cancelled" ; raise e
          in
          let r =
            Sim_support.run (fun sim ->
                serve_with sim handler (fun c e ->
                    Fake.request c ~stream:1 get ;
                    Weft_sim.settle sim ;
                    Fake.send c (goaway Internal_error) ;
                    e.await_engine () ) )
          in
          equal Testable.(list string) ["cancelled"] (events ()) ;
          clean_end ~error_end:true r )
    ; test "the last stream ends while R waits: GOAWAY at idle_timeout after it"
        (fun () ->
          (* This end does not wake R: R wakes at its check deadline. *)
          let run sim =
            let h = release_in_yield () in
            let r =
              serve_with ~config:timed ~hooks:h.hooks sim h.handler (fun c e ->
                  Fake.request c ~stream:1 get ;
                  wait_ivar h.started ;
                  h.arm () ;
                  while not (h.released ()) do
                    round_trip c
                  done ;
                  ignore (Fake.frames_until c is_goaway) ;
                  let at = Time.now Mono in
                  e.await_engine () ;
                  Span.to_ns (Time.diff Mono at (h.ended ())) )
            in
            equal Testable.int64 (Span.to_ns timed.idle_timeout) r.value ;
            clean_end r
          in
          Sim_support.run run ;
          Sim_support.race ~seeds:20 run )
    ; test "the last stream ends while R waits in a drain: the end at once"
        (fun () ->
          (* The client sends GOAWAY NO_ERROR, then the handler ends in the
             yield of R: the end must wake R. After the release, R ends the
             drain and stops to read, thus a PING round trip can wait for ever.
             The test sends PING frames with a pause of 1 ms in its place. *)
          let run sim =
            let h = release_in_yield () in
            let r =
              serve_with ~hooks:h.hooks sim h.handler (fun c e ->
                  Fake.request c ~stream:1 get ;
                  wait_ivar h.started ;
                  Fake.send c (goaway No_error) ;
                  round_trip c ;
                  h.arm () ;
                  while not (h.released ()) do
                    Fake.send c (Ping {ack= false; data= "12345678"}) ;
                    Time.sleep Mono (Span.ms 1)
                  done ;
                  e.await_engine () ;
                  Span.to_ns (Time.diff Mono (Time.now Mono) (h.ended ())) )
            in
            (* R ends at once, then it lingers: the client does not close. *)
            equal Testable.int64 (Span.to_ns config.linger_timeout) r.value ;
            clean_end r
          in
          Sim_support.run run ;
          Sim_support.race ~seeds:20 run )
    ; test
        "the end of the input after a request, no cancel: the response, no \
         busy loop, no header block timer" (fun () ->
          let release = Ivar.create () in
          let handler req = wait_ivar release ; hello req in
          let hooks, waits, steps = counting () in
          let r =
            Sim_support.run (fun sim ->
                serve_with ~config:{timed with cancel_on_disconnect= false}
                  ~hooks sim handler (fun c _ ->
                    Fake.request c ~stream:1 get ;
                    (* A header block that does not end before the end of the
                       input. *)
                    Fake.send c
                      (headers_frame ~end_stream:true ~end_headers:false 3
                         (plain get) ) ;
                    Weft_sim.settle sim ;
                    let before = waits () and steps0 = steps () in
                    Fake.close c ;
                    Weft_sim.settle sim ;
                    let waits = waits () - before
                    and steps = steps () - steps0 in
                    (* The handler ends after header_timeout. *)
                    Time.sleep Mono (Span.ms 300) ;
                    Ivar.fill release () ;
                    ((Fake.response c ~stream:1).body, waits, steps) ) )
          in
          let body, waits, steps = r.value in
          equal Testable.string "hello" body ;
          (* After the end of the input, R waits one time for each event, and
             takes a few steps. *)
          equal Testable.bool true (waits >= 1 && waits < few_waits) ;
          equal Testable.bool true (steps >= 1 && steps < few_waits) ;
          clean_end ~block_open:true r )
    ; test "a header block timeout while a stream waits for credit: at once"
        (fun () ->
          (* Stream 1 waits for credit. The handler of stream 3 waits with no
             write: only its cancel ends it. *)
          let record, events = log () in
          let handler =
            paths
              [ ( "/"
                , fun _ ->
                    Response.stream (fun w ->
                        match Body.Writer.write_string w "abc" with
                        | () ->
                            ()
                        | exception e ->
                            record (Printexc.to_string e) ;
                            raise e ) )
              ; ( "/wait"
                , fun _ ->
                    match sync never with
                    | () ->
                        Response.string "never"
                    | exception (Fiber.Cancelled _ as e) ->
                        record "wait cancelled" ; raise e ) ]
          in
          let r =
            Sim_support.run (fun sim ->
                serve_with
                  ~config:{timed with write_timeout= Span.s 1}
                  ~settings:[(initial_window_size, 0)]
                  sim handler
                  (fun c e ->
                    Fake.request c ~stream:1 get ;
                    Fake.request c ~stream:3 (request ~path:"/wait" ()) ;
                    Weft_sim.settle sim ;
                    let t0 = Time.now Mono in
                    Fake.send c
                      (headers_frame ~end_stream:true ~end_headers:false 5
                         (plain get) ) ;
                    ignore (Fake.frames_until c is_goaway) ;
                    Fake.close c ;
                    e.await_engine () ;
                    since t0 ) )
          in
          equal Testable.int64 (Span.to_ns timed.header_timeout) r.value ;
          equal
            Testable.(list string)
            ["Weft_flow__Flow.Closed"; "wait cancelled"]
            (List.sort compare (events ())) ;
          clean_end ~error_end:true r )
    ; test
        "W waits with no live stream: Closing writes the items of the streams"
        (fun () ->
          (* The head of stream 1, a 204 with a field of 400000 bytes, is larger
             than the buffers between W and the client: the TCP socket of the
             simulator holds 144 KiB (a receive buffer of 128 KiB and a send
             buffer of 16 KiB), and the client reads at most one frame after its
             pause. In manual mode, no deadline fires while W waits. *)
          let big =
            String.init 400_000 (fun i -> "0123456789abcdef".[i * 7 mod 16])
          in
          let s3_ended = Ivar.create () in
          let handler =
            paths
              [ ( "/big"
                , fun _ ->
                    Response.empty ~status:Status.no_content
                      ~headers:(Headers.of_list [("x-big", big)])
                      () )
              ; ( "/small"
                , fun _ ->
                    Ivar.fill s3_ended () ;
                    Response.empty ~status:Status.no_content () ) ]
          in
          let r =
            Sim_support.manual (fun sim ->
                serve sim handler (fun c ->
                    let t0 = Time.now Mono in
                    Fake.pause_reading c ;
                    Fake.request c ~stream:1 (request ~path:"/big" ()) ;
                    Weft_sim.settle sim ;
                    Fake.request c ~stream:3 (request ~path:"/small" ()) ;
                    wait_ivar s3_ended ;
                    Weft_sim.settle sim ;
                    Fake.send c (goaway No_error) ;
                    Weft_sim.settle sim ;
                    (* No GOAWAY came: the client reads one frame after its
                       pause at most. *)
                    let early = List.exists is_goaway (Fake.take_all c) in
                    Fake.resume_reading c ;
                    let r3 = show_response (Fake.response c ~stream:3) in
                    (early, r3, since t0) ) )
          in
          equal
            Testable.(triple bool string int64)
            (false, "head :status=204", 0L)
            r.value ;
          clean_end r )
    ; test "idle_timeout 0: GOAWAY at the end of the stream, no busy loop"
        (fun () ->
          (* The client sends the preface, its SETTINGS frame and a request in
             one write, as curl does. The handler takes 50 ms. With no check
             deadline, the end of the stream wakes R. *)
          let handler req =
            Time.sleep Mono (Span.ms 50) ;
            hello req
          in
          let hooks, _, _ = counting () in
          Sim_support.run (fun sim ->
              Sim_support.with_pair sim (fun client server ->
                  Fiber.scope (fun s ->
                      let flow = Tcp.flow client in
                      let engine =
                        Sim_support.call s (fun () ->
                            H2_server.serve ~hooks
                              (ctx {config with idle_timeout= Span.zero})
                              (Tcp.flow server) handler )
                      in
                      let t0 = Time.now Mono in
                      let w = Writer.create flow and r = Reader.create flow in
                      Writer.write_string w Frame.preface ;
                      Frame.write w (Settings {ack= false; settings= []}) ;
                      Frame.write w
                        (headers_frame ~end_stream:true 1 (plain get)) ;
                      Writer.flush w ;
                      let rec frames acc =
                        match Frame.read r ~max_size:16384 with
                        | Goaway _ as f ->
                            List.rev (f :: acc)
                        | f ->
                            frames (f :: acc)
                      in
                      let got = List.map brief (frames []) in
                      let at = since t0 in
                      Flow.shutdown_send flow ;
                      Sim_support.await engine ;
                      equal
                        Testable.(list string)
                        [ "SETTINGS [0x1=4096; 0x3=100; 0x4=65535; 0x6=16384]"
                        ; "WINDOW_UPDATE 0 +6487965"
                        ; "SETTINGS ACK []"
                        ; "HEADERS 1"
                        ; "DATA 1 5"
                        ; "DATA 1 0 END_STREAM"
                        ; {|GOAWAY 1 NO_ERROR "idle"|} ]
                        got ;
                      equal Testable.int64 (Span.to_ns (Span.ms 50)) at ) ) ) )
    ]

let () =
  exit
    (Windtrap.run "h2_engine"
       [fake_client_tests; engine_tests; rule_tests; lifecycle_tests] )
