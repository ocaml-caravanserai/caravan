(* Integration tests on the simulator of weft: the server over sockets, as
   clients use it. Each test runs in Sim_support: 1 seed and 1 domain, except a
   test whose name starts with "race:" (100 seeds on 4 domains). A test that
   checks a time runs in manual mode.

   The groups of the HTTP/1.1 behavior use the H1 engine. The groups "Server
   stop" and "Signals" use Fake_engine, so that they examine only the server. *)

open Windtrap
open Weft
open Caravan
open Server_support

let fake = Fake_engine.serve

let h1 = Private.H1_server.serve

(* [read_head r] reads the head of a response and gives its status line. *)
let read_head r =
  let status = Buf_reader.read_line r ~max:1000 in
  let rec fields () =
    if Buf_reader.read_line r ~max:1000 <> "" then fields ()
  in
  fields () ; status

let eof r = is_true ~msg:"the end of the stream" (is_eof r)

let integration_tests =
  group "Server over sockets"
    [ test "the default backlog is 4096" (fun () ->
          equal int 4096 Config.default.backlog )
    ; test "a burst of pipelined requests in one write" (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:h1
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow
            (get "/hello" ^ get "/empty" ^ get ~conn:"close" "/stream") ;
          let responses = List.init 3 (fun _ -> read_response r) in
          equal (list string)
            [{|200 "hello"|}; {|204 ""|}; {|200 "abcd" close|}]
            responses ;
          Flow.close flow )
    ; test "a client that waits for 100 Continue gets it from a streaming echo"
        (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:h1
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow
            "POST /echo HTTP/1.1\r\n\
             Host: x\r\n\
             Content-Length: 5\r\n\
             Expect: 100-continue\r\n\
             \r\n" ;
          (* As curl: the body goes only after the server asked for it. *)
          equal string "HTTP/1.1 100 Continue" (Buf_reader.read_line r ~max:100) ;
          equal string "" (Buf_reader.read_line r ~max:100) ;
          Flow.write_string flow "hello" ;
          equal string {|200 "hello"|} (read_response r) ;
          Flow.close flow )
    ; test "an HTTP/1.0 client sees a body cut short as a reset" (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:h1 ~config:{config with on_error= (fun _ _ -> ())}
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow "GET /partial HTTP/1.0\r\n\r\n" ;
          ignore (read_head r) ;
          equal string "partial" (read_buffered_exactly r 7) ;
          raises Flow.Closed (fun () ->
              Buf_reader.read_into r (Bytes.create 1) 0 1 ) ;
          Flow.close flow )
    ; test "race: 20 concurrent clients are all answered" (fun () ->
          Sim_support.race
          @@ fun _ ->
          with_server ~engine:h1
          @@ fun server ->
          let client () =
            let flow, r = connect server in
            Flow.write_string flow (get ~conn:"close" "/hello") ;
            let response = read_response r in
            Flow.close flow ; response
          in
          Fiber.scope
          @@ fun s ->
          let clients =
            List.init 20 (fun _ -> Sim_support.call ~name:"client" s client)
          in
          equal (list string)
            (List.init 20 (fun _ -> {|200 "hello" close|}))
            (List.map Sim_support.await clients) )
    ; test "a chunked upload in many small writes is echoed" (fun () ->
          (* The writes can come together. The h1_server tests split the input
             at known points. *)
          Sim_support.run
          @@ fun sim ->
          Weft_sim.net_faults sim {Weft_sim.no_faults with short_io= true} ;
          with_server ~engine:h1
          @@ fun server ->
          let flow, r = connect server in
          let data = String.init 3000 (fun i -> Char.chr (65 + (i mod 26))) in
          Flow.write_string flow
            "POST /echo HTTP/1.1\r\n\
             Host: x\r\n\
             Transfer-Encoding: chunked\r\n\
             \r\n" ;
          let rec send i =
            if i < String.length data then begin
              let n = Int.min 97 (String.length data - i) in
              let chunk =
                Printf.sprintf "%x\r\n%s\r\n" n (String.sub data i n)
              in
              (* Each chunk in two writes too. *)
              let half = String.length chunk / 2 in
              Flow.write_string flow (String.sub chunk 0 half) ;
              Flow.write_string flow
                (String.sub chunk half (String.length chunk - half)) ;
              send (i + n)
            end
          in
          send 0 ;
          Flow.write_string flow "0\r\n\r\n" ;
          equal string (summary 200 data false) (read_response r) ;
          Flow.close flow )
    ; test "1 MiB echoed while it uploads" (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:h1
          @@ fun server ->
          let flow, r = connect server in
          let data =
            String.init (1024 * 1024) (fun i -> Char.chr (i land 0xff))
          in
          Fiber.scope
          @@ fun s ->
          let upload =
            Sim_support.call ~name:"upload" s (fun () ->
                Flow.write_string flow
                  (Printf.sprintf
                     "POST /echo HTTP/1.1\r\n\
                      Host: x\r\n\
                      Content-Length: %d\r\n\
                      \r\n"
                     (String.length data) ) ;
                Flow.write_string flow data )
          in
          let response = read_response r in
          Sim_support.await upload ;
          is_true ~msg:"echoed exactly"
            (String.equal response (summary 200 data false)) ;
          Flow.close flow )
    ; test "a streamed response arrives as it is produced" (fun () ->
          (* The handler writes chunk i + 1 only after the client got chunk i
             and filled [got.(i - 1)]. *)
          let got = Array.init 2 (fun _ -> Ivar.create ()) in
          let handler _ =
            Response.stream (fun w ->
                for i = 1 to 3 do
                  Body.Writer.write_string w (string_of_int i) ;
                  Body.Writer.flush w ;
                  if i < 3 then sync (Ivar.read got.(i - 1))
                done )
          in
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:h1 ~handler
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/") ;
          ignore (read_head r) ;
          (* A chunk: the size line, then the data line. *)
          let chunk i =
            ignore (Buf_reader.read_line r ~max:100) ;
            let data = Buf_reader.read_line r ~max:100 in
            if i < 2 then Ivar.fill got.(i) () ;
            data
          in
          equal (list string) ["1"; "2"; "3"] (List.init 3 chunk) ;
          Flow.close flow )
    ; test "clients that leave mid-request do not disturb others" (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:h1
          @@ fun server ->
          let a, _ = connect server in
          Flow.write_string a "GET /hello HTTP/1.1\r\nHo" ;
          Flow.close a ;
          let b, _ = connect server in
          Flow.write_string b
            "POST /echo HTTP/1.1\r\nHost: x\r\nContent-Length: 10\r\n\r\nabc" ;
          Flow.close b ;
          let c, r = connect server in
          Flow.write_string c (get "/hello") ;
          equal string {|200 "hello"|} (read_response r) ;
          Flow.close c )
    ; test "a closing response over an unread body is not reset" (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:h1
          @@ fun server ->
          let flow, r = connect server in
          let body = Length (String.make 60_000 'x') in
          Flow.write_string flow
            (serialize
               { meth= "POST"
               ; path= "/ignore"
               ; v10= false
               ; conn= Some "close"
               ; body } ) ;
          equal string {|200 "ignored" close|} (read_response r) ;
          eof r ;
          Flow.close flow ) ]

(* Upgrades over sockets *)

let upgrade_tests =
  group "Upgrades over sockets"
    [ test
        "after a 101, bytes are echoed as they arrive, without HTTP's timeouts"
        (fun () ->
          (* The engine sees its flow through [counted]: [at_switch] gets the
             last write deadline that it set before the new protocol starts. *)
          Sim_support.run ~guard:(Span.s 120)
          @@ fun _ ->
          let t0 = Time.now Mono in
          (* The last write deadline and write timeout that the engine set, and
             their values when the new protocol starts. *)
          let deadline = Atomic.make (Some t0) and timeout = Atomic.make None in
          let at_switch = Atomic.make None in
          let engine ctx flow handler =
            let c =
              counted ~on_write_deadline:(Atomic.set deadline)
                ~on_write_timeout:(Atomic.set timeout) flow
            in
            h1 ctx c.flow handler
          in
          let handler _ =
            Response.upgrade
              ~headers:(Headers.of_list [("Upgrade", "echo")])
              (fun flow ->
                Atomic.set at_switch
                  (Some (Atomic.get deadline, Atomic.get timeout)) ;
                copy flow flow )
          in
          with_server ~engine ~handler ~config:timed_config
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow
            "GET / HTTP/1.1\r\n\
             Host: x\r\n\
             Upgrade: echo\r\n\
             Connection: Upgrade\r\n\
             \r\n" ;
          equal string "HTTP/1.1 101 Switching Protocols" (read_head r) ;
          List.iter
            (fun msg ->
              Flow.write_string flow msg ;
              equal string msg (read_buffered_exactly r (String.length msg)) )
            ["ping"; "pong"; String.make 10_000 'x'] ;
          (* After all the HTTP deadlines of the connection. *)
          Time.sleep Mono (Span.s 60) ;
          Flow.write_string flow "still there" ;
          equal string "still there" (read_buffered_exactly r 11) ;
          Flow.shutdown_send flow ;
          eof r ;
          Flow.close flow ;
          is_true
            ~msg:
              "no write deadline and no write timeout when the new protocol \
               starts"
            (Atomic.get at_switch = Some (None, None)) )
    ; test "a CONNECT tunnel reaches another server" (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:h1
          @@ fun server ->
          let upstream = Tcp.Listener.listen loopback in
          Fiber.scope
          @@ fun s ->
          let echo =
            Sim_support.call ~name:"echo" s (fun () ->
                let sock, _ = Tcp.Listener.accept upstream in
                let flow = Tcp.flow sock in
                Fun.protect
                  ~finally:(fun () -> Flow.close flow)
                  (fun () -> copy flow flow) )
          in
          let target =
            Format.asprintf "%a" Conn_info.pp_addr (Tcp.Listener.addr upstream)
          in
          let flow, r = connect server in
          Flow.write_string flow
            (Printf.sprintf "CONNECT %s HTTP/1.1\r\nHost: %s\r\n\r\n" target
               target ) ;
          equal string "HTTP/1.1 200 OK" (read_head r) ;
          let msg = "through the tunnel and back" in
          Flow.write_string flow msg ;
          equal string msg (read_buffered_exactly r (String.length msg)) ;
          Flow.shutdown_send flow ;
          eof r ;
          Sim_support.await echo ;
          Tcp.Listener.close upstream ;
          Flow.close flow ) ]

(* Clients that leave while their handler runs *)

let disconnect_tests =
  group "Client disconnects"
    [ test "a client that closes cancels its handler" (fun () ->
          Sim_support.manual
          @@ fun _ ->
          reset_ivars () ;
          with_server ~engine:h1
          @@ fun server ->
          let flow, _ = connect server in
          Flow.write_string flow (get "/wait") ;
          sync (Ivar.read !waiting) ;
          Flow.close flow ;
          (* Manual mode: the cancel comes with no time. *)
          sync (Ivar.read !cancelled) )
    ; test "the cancel reaches the upstream connection of the handler"
        (fun () ->
          Sim_support.manual
          @@ fun _ ->
          reset_ivars () ;
          with_server ~engine:h1
          @@ fun server ->
          let l = Tcp.Listener.listen loopback in
          upstream := Tcp.Listener.addr l ;
          let flow, _ = connect server in
          Flow.write_string flow (get "/fetch") ;
          let sock, _ = Tcp.Listener.accept l in
          let up = Tcp.flow sock in
          equal string "question" (read_exactly up 8) ;
          Flow.close flow ;
          (* The upstream never answers: the proxy side closes it. *)
          equal int ~msg:"upstream closed by the server" 0
            (Flow.read up (Bytes.create 1) 0 1) ;
          is_true ~msg:"cancelled" (Ivar.peek !cancelled <> None) ;
          Flow.close up ;
          Tcp.Listener.close l ) ]

(* The stop of the server, as a handler sees it. *)

let stop_tests =
  let config = {config with shutdown_grace= Span.s 30} in
  group "Server stop"
    [ test "before the stop, the stopping event is not ready" (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:fake ~config
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/is-stopping") ;
          equal string {|200 "no"|} (read_response r) ;
          Flow.close flow )
    ; test "a handler sees that the server stops" (fun () ->
          Sim_support.manual
          @@ fun _ ->
          reset_ivars () ;
          with_server ~engine:fake ~config
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/stopping") ;
          sync (Ivar.read !waiting) ;
          Server.shutdown server ;
          (* Manual mode: the response comes at the time of the shutdown. The
             handler gives "timeout" after 2 s with no stop. *)
          equal string {|200 "stopping" close|} (read_response r) ;
          Flow.close flow )
    ; test "a grace that overflows never cancels the handlers" (fun () ->
          let grace = Span.of_ns Int64.max_int in
          let started = Ivar.create ()
          and release = Ivar.create ()
          and cancelled = Ivar.create () in
          let handler _ =
            Ivar.fill started () ;
            match sync (Ivar.read release) with
            | () ->
                Response.string "done"
            | exception (Fiber.Cancelled _ as e) ->
                Ivar.fill cancelled () ; raise e
          in
          Sim_support.manual
          @@ fun sim ->
          with_server ~engine:fake
            ~config:{config with shutdown_grace= grace}
            ~handler
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/hold") ;
          sync (Ivar.read started) ;
          Server.shutdown server ;
          Sim_support.advance sim (Span.s 100_000) ;
          is_true ~msg:"no cancel" (Ivar.peek cancelled = None) ;
          Ivar.fill release () ;
          equal string {|200 "done" close|} (read_response r) ;
          Flow.close flow )
    ; test "the scope of the caller waits for the requests after a shutdown"
        (fun () ->
          let handler _ =
            Time.sleep Mono (Span.s 1) ;
            Response.string "done"
          in
          Sim_support.manual
          @@ fun sim ->
          let t0 = Time.now Mono in
          let client = Ivar.create () in
          Fiber.scope
          @@ fun s ->
          let caller =
            Sim_support.call ~name:"caller" s (fun () ->
                Fiber.scope (fun sw ->
                    let server = Server.start ~config ~engine:fake sw handler in
                    let flow, r = connect server in
                    Ivar.fill client (flow, r) ;
                    Flow.write_string flow (get "/") ;
                    (* The handler runs. The body returns with no wait. *)
                    Weft_sim.settle sim ;
                    Server.shutdown server ) )
          in
          (* The client is not a fiber of the scope of the server. *)
          let flow, r = sync (Ivar.read client) in
          let response = Sim_support.call s (fun () -> read_response r) in
          Sim_support.ends_at sim ~t0 (Span.s 1) caller ;
          equal string {|200 "done" close|} (Sim_support.await response) ;
          Flow.close flow )
    ; test "race: an abort while handlers return reports no error" (fun () ->
          (* The handlers return at t0 + 1 s, and the abort comes at the same
             time. *)
          let handler _ =
            Time.sleep Mono (Span.s 1) ;
            Response.string "done"
          in
          Sim_support.race
          @@ fun _ ->
          let errors = Atomic.make 0 in
          let config =
            {config with on_error= (fun _ _ -> Atomic.incr errors)}
          in
          Fiber.scope
          @@ fun sw ->
          let server = Server.start ~config ~engine:h1 sw handler in
          Fiber.scope (fun s ->
              let clients =
                List.init 20 (fun _ ->
                    Sim_support.call ~name:"client" s (fun () ->
                        let flow, r = connect server in
                        Fun.protect ~finally:(fun () -> Flow.close flow)
                        @@ fun () ->
                        Flow.write_string flow (get "/") ;
                        (* The abort can come before the response. *)
                        try ignore (read_response r)
                        with Flow.Closed | Buf_reader.Eof -> () ) )
              in
              Time.sleep Mono (Span.s 1) ;
              Server.abort server ;
              Server.wait server ;
              List.iter Sim_support.await clients ) ;
          equal int ~msg:"errors reported" 0 (Atomic.get errors) ) ]

(* The signal stop of the server on the simulator. The world watches SIGINT and
   SIGTERM. The runs are in manual mode, with no guard: a hang gives
   [Weft_sim.Deadlock]. The handlers are the handlers of serve_until_signal. *)

let signal_run f =
  Weft_sim.run ~auto:false ~signals:[Signal.SIGINT; Signal.SIGTERM] f

(* Filled by the handlers: [started] at the start, [stopping] when the server
   starts to stop, [cancelled] when the server cancels /stuck or /deaf. A test
   makes new Ivars before it starts. *)
type signal_marks =
  {started: unit Ivar.t; stopping: unit Ivar.t; cancelled: unit Ivar.t}

let marks () =
  {started= Ivar.create (); stopping= Ivar.create (); cancelled= Ivar.create ()}

(* /slow gives "done" when the server starts to stop, or "timeout" after 2 s.
   /stuck waits for the stop, then waits 10 s. /deaf does as /stuck, but when
   the server cancels it, it waits 10 s in a shield, deaf to the cancel. *)
let signal_handler m req =
  let stopping = Conn_info.stopping (Option.get (Request.conn req)) in
  let wait_for_stop () =
    Ivar.fill m.started () ;
    sync stopping ;
    Ivar.fill m.stopping () ;
    Time.sleep Mono (Span.s 10)
  in
  match Request.path_only req with
  | "/slow" ->
      Ivar.fill m.started () ;
      Response.string
        (sync
           (choose
              [ wrap stopping (fun () -> "done")
              ; wrap (Time.after Mono (Span.s 2)) (fun () -> "timeout") ] ) )
  | "/stuck" -> (
    match wait_for_stop () with
    | () ->
        Response.string "done"
    | exception (Fiber.Cancelled _ as e) ->
        Ivar.fill m.cancelled () ; raise e )
  | "/deaf" -> (
    match wait_for_stop () with
    | () ->
        Response.string "done"
    | exception Fiber.Cancelled _ ->
        Ivar.fill m.cancelled () ;
        Fiber.shield (fun () -> Time.sleep Mono (Span.s 10)) ;
        Response.string "done" )
  | _ ->
      Response.string "hello"

(* [with_signal_server m f] runs [f server] with a server of [signal_handler m]
   and a daemon fiber that calls [Server.stop_on_signals]. Then it waits for the
   server. The signal config has a grace of 10 s. *)
let with_signal_server m f =
  Fiber.scope (fun sw ->
      let server =
        Server.start
          ~config:{config with shutdown_grace= Span.s 10}
          ~engine:fake sw (signal_handler m)
      in
      Fiber.fork_daemon ~name:"signals" sw (fun () ->
          Server.stop_on_signals server ) ;
      f server ;
      Server.wait server )

let signal_tests =
  group "Signals"
    [ test "SIGTERM: the request in progress finishes, then the server ends"
        (fun () ->
          signal_run
          @@ fun sim ->
          let m = marks () in
          with_signal_server m
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/slow") ;
          sync (Ivar.read m.started) ;
          Weft_sim.signal sim Signal.SIGTERM ;
          (* Manual mode: the response comes with no time, not at the 2 s of
             /slow. *)
          equal string {|200 "done" close|} (read_response r) ;
          Flow.close flow )
    ; test "a second SIGINT aborts the request in progress" (fun () ->
          signal_run
          @@ fun sim ->
          let m = marks () in
          with_signal_server m
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/stuck") ;
          sync (Ivar.read m.started) ;
          Weft_sim.signal sim Signal.SIGINT ;
          sync (Ivar.read m.stopping) ;
          Weft_sim.signal sim Signal.SIGINT ;
          (* Manual mode: the cancel comes with no time, not after the 10 s of
             /stuck. *)
          sync (Ivar.read m.cancelled) ;
          is_true ~msg:"no response" (is_eof r) ;
          Flow.close flow )
    ; test "SIGTERM with nothing in progress: the server ends at once"
        (fun () ->
          signal_run
          @@ fun sim ->
          let m = marks () in
          with_signal_server m
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/hello") ;
          equal string {|200 "hello"|} (read_response r) ;
          Weft_sim.signal sim Signal.SIGTERM ;
          (* Manual mode: the end comes with no time, not at the end of the
             grace. *)
          is_true ~msg:"the idle connection is closed" (is_eof r) ;
          Flow.close flow )
    ; test "a third signal ends the program" (fun () ->
          raises (Weft_sim.Killed Signal.SIGINT) (fun () ->
              signal_run
              @@ fun sim ->
              let m = marks () in
              with_signal_server m
              @@ fun server ->
              let flow, _ = connect server in
              Flow.write_string flow (get "/deaf") ;
              sync (Ivar.read m.started) ;
              Weft_sim.signal sim Signal.SIGINT ;
              sync (Ivar.read m.stopping) ;
              Weft_sim.signal sim Signal.SIGINT ;
              sync (Ivar.read m.cancelled) ;
              (* The handler is deaf to the cancel. Only the third signal can
                 end the program. *)
              Weft_sim.signal sim Signal.SIGINT ;
              Weft_sim.settle sim ;
              Flow.close flow ) )
    ; test "two signals in one commit abort at once" (fun () ->
          signal_run
          @@ fun sim ->
          let m = marks () in
          Fiber.scope
          @@ fun sw ->
          let server =
            Server.start ~config ~engine:fake sw (signal_handler m)
          in
          let flow, r = connect server in
          Flow.write_string flow (get "/stuck") ;
          sync (Ivar.read m.started) ;
          (* No fiber waits for the signals: the world adds them to one count,
             and the first commit of stop_on_signals takes the count 2. *)
          Weft_sim.signal sim Signal.SIGINT ;
          Weft_sim.signal sim Signal.SIGINT ;
          Weft_sim.settle sim ;
          Fiber.fork_daemon ~name:"signals" sw (fun () ->
              Server.stop_on_signals server ) ;
          Weft_sim.settle sim ;
          is_true ~msg:"the handler is cancelled at once"
            (Ivar.peek m.cancelled <> None) ;
          is_true ~msg:"no response" (is_eof r) ;
          Server.wait server ;
          Flow.close flow )
    ; test "Server.run: on_start gets the server, and SIGTERM ends the run"
        (fun () ->
          let m = marks () in
          let started = ref false in
          Server.run
            ~config:{config with shutdown_grace= Span.s 10}
            ~engine:fake
            ~on_start:(fun server ->
              started := true ;
              let flow, r = connect server in
              Flow.write_string flow (get "/slow") ;
              sync (Ivar.read m.started) ;
              Weft_sim.signal (Option.get !os_sim) Signal.SIGTERM ;
              equal string {|200 "done" close|} (read_response r) ;
              Flow.close flow )
            (module Sim_os)
            (signal_handler m) ;
          is_true ~msg:"on_start is called" !started ) ]

(* The services of [Server.run]. A fake service adds its start and its stop to a
   log. The handler of [gate_handler] fills [started], then waits for [gate]. *)

exception Boom

let fake_service ?(start = ignore) ?(stop = ignore) log name =
  Server.service
    ~start:(fun sw ->
      log := ("start " ^ name) :: !log ;
      start sw )
    ~stop:(fun ~grace ->
      log := Printf.sprintf "stop %s %Ld ns" name (Span.to_ns grace) :: !log ;
      stop () )

let gate_handler ~started ~gate _ =
  ignore (Ivar.try_fill started ()) ;
  sync (Ivar.read gate) ;
  Response.string "done"

let grace_config = {config with shutdown_grace= Span.s 10}

exception Other_scope

(* A listener whose [accept] raises the [Cancelled] of another scope: a failure
   of the listener. *)
module Foreign_cancel_listener = struct
  type t = Tcp.Listener.t

  let addr = Tcp.Listener.addr

  let accept _ = raise (Fiber.Cancelled Other_scope)

  let close = Tcp.Listener.close
end

let sim () = Option.get !os_sim

(* A listener whose [accept] raises EBADF: the server stops, and its [wait]
   raises the error. *)
module Bad_fd_listener = struct
  type t = Tcp.Listener.t

  let addr = Tcp.Listener.addr

  let accept _ = raise (Net.Error (Bad_fd, "Net.accept"))

  let close = Tcp.Listener.close
end

(* [Sim_os] with no free fd: the bind of [Server.run] fails. *)
module No_fds_os : Weft.OS = struct
  let run ?slack:_ ?domains ?signals ?watchdog:_ main =
    Weft_sim.run ~auto:false ?domains ?signals (fun sim ->
        Weft_sim.set_fd_limit sim (Some 0) ;
        main () )
end

let service_tests =
  group "Server.run: services"
    [ test "the services start first and stop last, with the grace that remains"
        (fun () ->
          let log = ref [] in
          let started = Ivar.create () and gate = Ivar.create () in
          Server.run ~config:grace_config ~engine:fake
            ~services:[fake_service log "a"; fake_service log "b"]
            ~on_start:(fun server ->
              equal (list string) ["start a"; "start b"] (List.rev !log) ;
              let flow, r = connect server in
              Flow.write_string flow (get "/") ;
              sync (Ivar.read started) ;
              Weft_sim.signal (sim ()) Signal.SIGTERM ;
              Weft_sim.settle (sim ()) ;
              Sim_support.advance (sim ()) (Span.s 3) ;
              equal (list string) ["start a"; "start b"] (List.rev !log) ;
              Ivar.fill gate () ;
              equal string {|200 "done" close|} (read_response r) ;
              Flow.close flow )
            (module Sim_os)
            (gate_handler ~started ~gate) ;
          equal (list string)
            [ "start a"
            ; "start b"
            ; "stop b 7000000000 ns"
            ; "stop a 7000000000 ns" ]
            (List.rev !log) )
    ; test "the requests took all of the grace: the services get zero"
        (fun () ->
          let log = ref [] in
          let m = marks () in
          Server.run ~config:grace_config ~engine:fake
            ~services:[fake_service log "a"]
            ~on_start:(fun server ->
              let flow, _ = connect server in
              Flow.write_string flow (get "/deaf") ;
              sync (Ivar.read m.started) ;
              Weft_sim.signal (sim ()) Signal.SIGTERM ;
              Weft_sim.settle (sim ()) ;
              (* /deaf takes 10 s after the end of the grace. *)
              Sim_support.advance (sim ()) (Span.s 20) ;
              Flow.close flow )
            (module Sim_os)
            (signal_handler m) ;
          equal (list string) ["start a"; "stop a 0 ns"] (List.rev !log) )
    ; test
        "the listener fails: the services get the full grace, then both errors \
         go out" (fun () ->
          let log = ref [] in
          match
            Server.run_with ~config:grace_config ~engine:fake
              ~services:[fake_service ~stop:(fun () -> raise Boom) log "a"]
              (module Sim_os)
              (module Bad_fd_listener)
              (fun () -> Tcp.Listener.listen loopback)
              (fun _ -> Response.string "x")
          with
          | () ->
              fail "the run ended"
          | exception Fiber.Multiple [(Net.Error (Bad_fd, _), _); (Boom, _)] ->
              equal (list string)
                ["start a"; "stop a 10000000000 ns"]
                (List.rev !log)
          | exception e ->
              fail (Printexc.to_string e) )
    ; test "an error of the signal fiber goes out alone, with no services"
        (fun () ->
          (* The world does not watch the signals: the wait of the signal fiber
             raises. *)
          let module No_signals_os : Weft.OS = struct
            let run ?slack:_ ?domains ?signals:_ ?watchdog:_ main =
              Weft_sim.run ~auto:false ?domains (fun _ -> main ())
          end
          in
          match
            Server.run ~config ~engine:fake
              (module No_signals_os)
              (fun _ -> Response.string "x")
          with
          | () ->
              fail "the run ended"
          | exception Invalid_argument _ ->
              ()
          | exception e ->
              fail (Printexc.to_string e) )
    ; test "the listener fails: the services share the grace" (fun () ->
          let log = ref [] in
          let slow () = Time.sleep Mono (Span.s 6) in
          match
            Server.run_with ~config:grace_config ~engine:fake
              ~services:[fake_service log "a"; fake_service ~stop:slow log "b"]
              (module Sim_auto_os)
              (module Bad_fd_listener)
              (fun () -> Tcp.Listener.listen loopback)
              (fun _ -> Response.string "x")
          with
          | () ->
              fail "the run ended"
          | exception Net.Error (Bad_fd, _) ->
              equal (list string)
                [ "start a"
                ; "start b"
                ; "stop b 10000000000 ns"
                ; "stop a 4000000000 ns" ]
                (List.rev !log) )
    ; test "a Cancelled of another scope from the listener: the stops run"
        (fun () ->
          let log = ref [] in
          match
            Server.run_with ~config:grace_config ~engine:fake
              ~services:[fake_service log "a"]
              (module Sim_os)
              (module Foreign_cancel_listener)
              (fun () -> Tcp.Listener.listen loopback)
              (fun _ -> Response.string "x")
          with
          | () ->
              fail "the run ended"
          | exception Fiber.Cancelled Other_scope ->
              equal (list string)
                ["start a"; "stop a 10000000000 ns"]
                (List.rev !log) )
    ; test "the second signal: the error of an earlier stop goes out" (fun () ->
          let log = ref [] in
          let deaf () =
            Weft_sim.signal (sim ()) Signal.SIGINT ;
            sync never
          in
          raises Boom (fun () ->
              Server.run ~config:grace_config ~engine:fake
                ~services:
                  [ fake_service ~stop:deaf log "a"
                  ; fake_service ~stop:(fun () -> raise Boom) log "b" ]
                ~on_start:(fun _ -> Weft_sim.signal (sim ()) Signal.SIGTERM)
                (module Sim_os)
                (fun _ -> Response.string "x") ) ;
          equal (list string)
            [ "start a"
            ; "start b"
            ; "stop b 10000000000 ns"
            ; "stop a 10000000000 ns" ]
            (List.rev !log) )
    ; test "a stop raises the Cancelled of another scope: it goes out"
        (fun () ->
          let log = ref [] in
          raises (Fiber.Cancelled Other_scope) (fun () ->
              Server.run ~config:grace_config ~engine:fake
                ~services:
                  [ fake_service
                      ~stop:(fun () -> raise (Fiber.Cancelled Other_scope))
                      log "a" ]
                ~on_start:(fun _ -> Weft_sim.signal (sim ()) Signal.SIGTERM)
                (module Sim_os)
                (fun _ -> Response.string "x") ) )
    ; test "a failure of the bind starts no service" (fun () ->
          let log = ref [] in
          match
            Server.run ~config ~engine:fake
              ~services:[fake_service log "a"]
              (module No_fds_os)
              (fun _ -> Response.string "x")
          with
          | () ->
              fail "the run ended"
          | exception Net.Error _ ->
              equal (list string) [] !log )
    ; test "a start raises: the started services stop with no grace" (fun () ->
          let log = ref [] in
          raises Boom (fun () ->
              Server.run ~config:grace_config ~engine:fake
                ~services:
                  [ fake_service log "a"
                  ; fake_service log "b"
                  ; fake_service ~start:(fun _ -> raise Boom) log "c"
                  ; fake_service log "d" ]
                ~on_start:(fun _ -> fail "the server started")
                (module Sim_os)
                (fun _ -> Response.string "x") ) ;
          equal (list string)
            ["start a"; "start b"; "start c"; "stop b 0 ns"; "stop a 0 ns"]
            (List.rev !log) )
    ; test "the server cannot start: the services stop with no grace" (fun () ->
          let log = ref [] in
          match
            Server.run
              ~config:{grace_config with max_connections= 0}
              ~engine:fake
              ~services:[fake_service log "a"; fake_service log "b"]
              (module Sim_os)
              (fun _ -> Response.string "x")
          with
          | () ->
              fail "the run ended"
          | exception Invalid_argument _ ->
              equal (list string)
                ["start a"; "start b"; "stop b 0 ns"; "stop a 0 ns"]
                (List.rev !log) )
    ; test "a stop raises: the other stops run, then the run raises" (fun () ->
          let log = ref [] in
          let stop_on_start server =
            ignore server ;
            Weft_sim.signal (sim ()) Signal.SIGTERM
          in
          raises Boom (fun () ->
              Server.run ~config:grace_config ~engine:fake
                ~services:
                  [ fake_service log "a"
                  ; fake_service ~stop:(fun () -> raise Boom) log "b" ]
                ~on_start:stop_on_start
                (module Sim_os)
                (fun _ -> Response.string "x") ) ;
          equal (list string)
            [ "start a"
            ; "start b"
            ; "stop b 10000000000 ns"
            ; "stop a 10000000000 ns" ]
            (List.rev !log) ;
          log := [] ;
          match
            Server.run ~config:grace_config ~engine:fake
              ~services:
                [ fake_service ~stop:(fun () -> raise Exit) log "a"
                ; fake_service ~stop:(fun () -> raise Boom) log "b" ]
              ~on_start:stop_on_start
              (module Sim_os)
              (fun _ -> Response.string "x")
          with
          | () ->
              fail "the run ended"
          | exception Fiber.Multiple [(Boom, _); (Exit, _)] ->
              ()
          | exception e ->
              fail (Printexc.to_string e) )
    ; test "a service fails: the server stops with its grace, the run raises"
        (fun () ->
          let log = ref [] in
          let started = Ivar.create () and fail_now = Ivar.create () in
          let failing sw =
            Fiber.fork sw (fun () ->
                sync (Ivar.read fail_now) ;
                raise Boom )
          in
          (* The handler waits for the stop: a stop with a grace, not a
             cancel. *)
          let handler req =
            Ivar.fill started () ;
            match sync (Conn_info.stopping (Option.get (Request.conn req))) with
            | () ->
                log := "handler: stop" :: !log ;
                Response.string "done"
            | exception (Fiber.Cancelled _ as e) ->
                log := "handler: cancel" :: !log ;
                raise e
          in
          raises Boom (fun () ->
              Server.run ~config:grace_config ~engine:fake
                ~services:[fake_service ~start:failing log "a"]
                ~on_start:(fun server ->
                  let flow, _ = connect server in
                  Flow.write_string flow (get "/") ;
                  sync (Ivar.read started) ;
                  Ivar.fill fail_now () )
                (module Sim_os)
                handler ) ;
          equal (list string) ["start a"; "handler: stop"] (List.rev !log) ;
          (* The same with the [Cancelled] of another scope as the failure. *)
          log := [] ;
          let started = Ivar.create () and fail_now = Ivar.create () in
          let failing sw =
            Fiber.fork sw (fun () ->
                sync (Ivar.read fail_now) ;
                raise (Fiber.Cancelled Other_scope) )
          in
          let handler req =
            Ivar.fill started () ;
            match sync (Conn_info.stopping (Option.get (Request.conn req))) with
            | () ->
                log := "handler: stop" :: !log ;
                Response.string "done"
            | exception (Fiber.Cancelled _ as e) ->
                log := "handler: cancel" :: !log ;
                raise e
          in
          ( match
              Server.run ~config:grace_config ~engine:fake
                ~services:[fake_service ~start:failing log "a"]
                ~on_start:(fun server ->
                  let flow, _ = connect server in
                  Flow.write_string flow (get "/") ;
                  sync (Ivar.read started) ;
                  Ivar.fill fail_now () )
                (module Sim_os)
                handler
            with
          | () ->
              fail "the run ended"
          | exception Fiber.Cancelled Other_scope ->
              () ) ;
          equal (list string) ["start a"; "handler: stop"] (List.rev !log) )
    ; test "the second signal cancels a service in its stop" (fun () ->
          let log = ref [] in
          let stopping = Ivar.create () in
          let second sw =
            Fiber.fork sw (fun () ->
                sync (Ivar.read stopping) ;
                Weft_sim.signal (sim ()) Signal.SIGINT )
          in
          let deaf_stop () =
            Ivar.fill stopping () ;
            match sync never with
            | () ->
                ()
            | exception (Fiber.Cancelled _ as e) ->
                log := "cancelled" :: !log ;
                raise e
          in
          Server.run ~config:grace_config ~engine:fake
            ~services:[fake_service ~start:second ~stop:deaf_stop log "a"]
            ~on_start:(fun _ -> Weft_sim.signal (sim ()) Signal.SIGTERM)
            (module Sim_os)
            (fun _ -> Response.string "x") ;
          equal (list string)
            ["start a"; "stop a 10000000000 ns"; "cancelled"]
            (List.rev !log) ) ]

let () =
  exit
    (Windtrap.run "server"
       [ integration_tests
       ; upgrade_tests
       ; disconnect_tests
       ; stop_tests
       ; signal_tests
       ; service_tests ] )
