open Weft

let limits (c : Config.t) : H1_parse.limits =
  { max_request_line= c.max_request_line
  ; max_header_bytes= c.max_header_bytes
  ; max_header_count= c.max_header_count }

(* [is_fatal e] is [true] only for a cancel of the fiber ({!Cancel.is_own}). The
   engine does not report it and raises it again: a weft cancel stays, and the
   next sync raises it again. A [Fiber.Cancelled] of a scope that the handler
   cancelled is not fatal: weft gives it as a failure of the handler. A
   [Promise.Cancelled] is not fatal: a handler can raise it, for example from
   the await of a child that it cancelled. *)
let is_fatal = Cancel.is_own

(* The response to a malformed request; the connection closes after it. A client
   that already left makes the write raise Flow.Closed, which ends the
   connection like any other network failure. *)
let error_response ?date ?(meth = Method.GET) w (e : H1_error.t) =
  let status = H1_error.status e in
  let body = Body.of_string (Status.reason status ^ "\n") in
  let r =
    Response.make ~status
      ~headers:(Headers.of_list [("Content-Type", "text/plain; charset=utf-8")])
      body
  in
  (* A HEAD request gets the head only. *)
  let framing = H1_write.framing ~meth ~version:Version.Http_1_1 r in
  H1_write.head w ?date ~version:Version.Http_1_1 ~keep_alive:false framing r ;
  H1_write.body w framing body ;
  `Linger

let request_keep_alive (h : H1_parse.head) =
  match h.version with
  | Version.Http_1_0 ->
      Headers.has_token "connection" "keep-alive" h.headers
  | _ ->
      not (Headers.has_token "connection" "close" h.headers)

(* RFC 9110 §10.1.1: [100-continue] is the only expectation, and HTTP/1.0
   clients' is ignored. *)
let expectation (h : H1_parse.head) =
  match Headers.tokens "expect" h.headers with
  | [] ->
      `None
  | ts when List.for_all (fun t -> String.lowercase_ascii t = "100-continue") ts
    ->
      if h.version = Version.Http_1_1 then `Continue else `None
  | _ ->
      `Unknown

let expects_continue h = expectation h = `Continue

(* RFC 9112 §9.6: a connection that the server ends closes in stages, for at
   most [linger_timeout] and [max_drain] bytes. *)
let linger (config : Config.t) flow =
  Flow.set_read_deadline flow (Deadline.from_now config.linger_timeout) ;
  Flow.linger flow ~max:config.max_drain

(* A protocol of an Upgrade field: its name and its version, split at the first
   '/'. *)
let protocol p =
  match String.index_opt p '/' with
  | Some i ->
      (String.sub p 0 i, Some (String.sub p (i + 1) (String.length p - i - 1)))
  | None ->
      (p, None)

(* RFC 9110 §7.8: the names compare without case. RFC 9110 has no rule for the
   versions: they must be the same text, and no version matches only no
   version. *)
let same_protocol a b =
  let name, version = protocol a and name', version' = protocol b in
  String.lowercase_ascii name = String.lowercase_ascii name'
  && version = version'

(* Why [response], an upgrade, cannot answer the request [head], if it cannot:
   RFC 9110 §7.8, §9.3.6 and §15.2.2. *)
let upgrade_error (head : H1_parse.head) response =
  if Status.code (Response.status response) = 101 then
    if head.version <> Version.Http_1_1 then Some "101 to an HTTP/1.0 request"
    else
      let offered = Headers.tokens "upgrade" head.headers in
      if
        not
          ( offered <> []
          && Headers.has_token "connection" "upgrade" head.headers )
      then Some "101 to a request that did not ask for an upgrade"
      else
        match Headers.tokens "upgrade" (Response.headers response) with
        | [] ->
            Some "101 without an Upgrade field"
        | protocols -> (
          (* RFC 9110 §7.8: no switch to a protocol that the client did not
             offer. *)
          match
            List.find_opt
              (fun p -> not (List.exists (same_protocol p) offered))
              protocols
          with
          | Some p ->
              Some ("101 to a protocol that the request did not offer: " ^ p)
          | None ->
              None )
  else if not (Method.equal head.meth Method.CONNECT) then
    Some (Format.asprintf "2xx upgrade to a %a request" Method.pp head.meth)
  else None

(* RFC 9110 §7.8: a 101 to a request that expects 100-continue comes after a
   100, also when the request has no content. A CONNECT request has no content
   (§9.3.6): a tunnel gets the 100 only for a body still owed. *)
let continue_before_switch response body =
  if Status.code (Response.status response) = 101 || H1_body.continue_owed body
  then H1_body.send_continue body

let internal_error () =
  Response.string ~status:Status.internal_server_error "Internal Server Error\n"

let serve_http1 : Engine.t =
 fun ctx flow handler ->
  let config = ctx.config in
  let limits = limits config in
  (* Each write that waits gets [write_timeout] from the start of its first
     wait. Thus the engine sets the timeout one time for the connection. *)
  Flow.set_write_timeout flow (Some config.write_timeout) ;
  (* The read limits of the phase of the connection: an absolute deadline and a
     timeout for each read that waits. A head and a drain each get a deadline
     for all of their reads. Thus a client that sends one byte at a time cannot
     keep the connection. The deadline is lazy: a phase reads the clock only
     when it reads the flow. The body of a request has no deadline, and each of
     its reads has [read_timeout]. *)
  let body_phase = (Lazy.from_val None, Some config.read_timeout) in
  let phase = ref body_phase in
  let new_phase span = phase := (lazy (Deadline.from_now span), None) in
  let r =
    Buf_reader.make ~size:config.buffer_size ~read_now:(Flow.read_now flow)
      (fun buf off len ->
        let deadline, timeout = !phase in
        Flow.set_read_deadline flow (Lazy.force deadline) ;
        Flow.set_read_timeout flow timeout ;
        Flow.read flow buf off len )
  in
  (* The engine discards a request body that the handler did not read, within
     one deadline for all of it, as the linger does. *)
  let drain body =
    new_phase config.read_timeout ;
    H1_body.drain body ~max:config.max_drain
  in
  let w = Buf_writer.make ~size:config.buffer_size (Flow.write flow) in
  let scheme =
    match Conn_info.scheme ctx.conn with `Http -> "http" | `Https -> "https"
  in
  let report e = config.on_error e (Printexc.get_raw_backtrace ()) in
  (* A response that the engine does not send: its body closes, or an upgrade
     runs its [on_refused]. Response.abandon runs in a shield, also in a
     cancelled fiber. The engine reports its exception. *)
  let abandon response = try Response.abandon response with e -> report e in
  let date () =
    if config.date_header then Some (Date.of_posix (Time.now Posix)) else None
  in
  let error ?meth e = error_response ?date:(date ()) ?meth w e in
  (* The exchange of each request is a fiber of this scope. *)
  Fiber.scope
  @@ fun s ->
  (* Waits at most [idle] for the next request. A stopping server takes no new
     request, also one that it received. *)
  let rec next_request ~idle =
    if ctx.is_stopping () then
      if Buf_reader.buffered r > 0 then `Linger else `Close
    else if Buf_reader.buffered r > 0 then exchange ()
    else wait_for_input (Deadline.from_now idle)
  (* A wake can find nothing to read: the readiness can be stale, or a TLS
     record incomplete. Then the wait continues, to the same deadline. A read
     that waits would ignore the deadline and a stopping server. *)
  and wait_for_input deadline =
    match
      sync
        (choose
           [ wrap (Conn_info.stopping ctx.conn) (fun () -> `Stop)
           ; wrap (Flow.readable flow) (fun () -> `Input)
           ; wrap (Deadline.at deadline) (fun () -> `Idle) ] )
    with
    | `Stop ->
        `Close
    | `Input -> (
      match Buf_reader.fill_now r with
      | `Bytes ->
          input ()
      | `Eof ->
          `Close
      | `Would_block ->
          wait_for_input deadline )
    (* Input and a passed deadline can be ready at the same time, and weft
       selects one at random. Thus the engine reads what came: the input
       wins. *)
    | `Idle -> (
      match Buf_reader.fill_now r with
      | `Bytes ->
          input ()
      | `Eof | `Would_block ->
          `Close )
  (* The stop and the input can be ready at the same time, and weft selects one
     at random. A stopping server starts no new exchange. *)
  and input () = if ctx.is_stopping () then `Linger else exchange ()
  and exchange () =
    new_phase config.header_timeout ;
    match H1_parse.request_head limits r with
    | exception Buf_reader.Eof ->
        `Close (* the client left in the middle of a request *)
    | exception Flow.Timeout ->
        error H1_error.Request_timeout
    | exception H1_error.E e ->
        error e
    | head -> (
      match
        let framing = H1_parse.request_framing head in
        let target = H1_parse.request_target ~scheme head in
        if expectation head = `Unknown then
          raise (H1_error.E H1_error.Expectation_failed) ;
        (framing, target)
      with
      | exception H1_error.E e ->
          error ~meth:head.meth e
      | framing, target -> (
          phase := body_phase ;
          let committed = ref false in
          let expect_continue =
            if not (expects_continue head) then None
            else
              Some
                (fun () ->
                  (* A client that cannot take it left. *)
                  if not !committed then
                    try
                      Buf_writer.write_string w "HTTP/1.1 100 Continue\r\n\r\n" ;
                      Buf_writer.flush w
                    with Flow.Closed | Flow.Timeout -> raise Body.Truncated )
          in
          match
            H1_body.make ?max_body:config.max_body ?expect_continue ~limits r
              framing
          with
          | exception H1_error.E e ->
              error ~meth:head.meth e
          | body ->
              respond head target body ~committed ) )
  and respond (head : H1_parse.head) (target : H1_parse.target) body ~committed
      =
    let req =
      Request.make ~version:head.version ~scheme:target.scheme
        ?authority:target.authority ~headers:head.headers
        ~body:(H1_body.body body) ~conn:ctx.conn head.meth target.path
    in
    (* [ended] is [true] after the end of the handler, also when it raises. *)
    let ended = Atomic.make false in
    let run_handler () =
      Fun.protect
        ~finally:(fun () -> Atomic.set ended true)
        (fun () -> handler req)
    in
    match Fiber.fork_promise ~name:"caravan.exchange" s run_handler with
    | exception Fiber.Scope_closed ->
        (* The scope of the connection is open, so only a cancel of an outer
           scope gives it. The connection ends. *)
        `Close
    | p -> (
      match await p body ~ended with
      (* Only a failure of the body of the request is the fault of the client.
         The same exceptions from the other flows of the handler, an upstream,
         are errors. *)
      | exception H1_error.E e when H1_body.failed body ->
          error ~meth:head.meth e
      | exception Body.Truncated when H1_body.failed body ->
          `Close (* the client left *)
      (* A cancel of the connection can end the exchange before the connection
         fiber sees its own cancel. Else the handler raised it: an error. *)
      | ((exception Promise.Cancelled)
         [@mutate
           off
             "true only on OS domains: the exchange ends with \
              Promise.Cancelled before the connection fiber reads its cancel"]
         )
        when Cancel.is_cancelled () ->
          `Close
      | exception (Fiber.Cancelled _ as c) when is_fatal c ->
          (* The connection is cancelled, and so is the exchange. Wait for its
             end, so that the engine reports a failure of its cleanup. The
             cancel gives Promise.Cancelled, or Scope_closed to a fork of the
             handler. A Fiber.Cancelled is a failure: weft gives it only for a
             scope that the handler cancelled. *)
          let bt = Printexc.get_raw_backtrace () in
          (* A handler that catches its cancel can give a response: abandon it,
             in a shield. There, a cancel can come only from another scope, thus
             it is a failure to report. *)
          ( match Fiber.shield (fun () -> Promise.await p) with
          | response ->
              abandon response
          | exception (Promise.Cancelled | Fiber.Scope_closed) ->
              ()
          (* The client left during the request body, as on the path with no
             cancel: not an error of the server. *)
          | exception Body.Truncated when H1_body.failed body ->
              ()
          | exception e ->
              report e ) ;
          Printexc.raise_with_backtrace c bt
      | exception e ->
          report e ; fail_500 head
      | `Disconnected ->
          `Close
      | `Response response -> (
        match Response.kind response with
        | Response.Upgrade f -> (
          (* The decision, and the head of the switch. The handler of an
             exception covers only these steps, never [switch f]: thus
             [on_refused] never runs after [f] started. *)
          match
            match upgrade_error head response with
            | Some e ->
                report (Failure ("Response.upgrade: " ^ e)) ;
                `Fail_500
            (* A server that stops opens no new upgrade or tunnel (#26), and
               sends no 100 for it. *)
            | None when ctx.is_stopping () ->
                `Fail_503
            (* A body too large to drain gets the 500 with no 100: after a 100
               the client sends it (RFC 9110 §10.1.1). *)
            | None
              when Option.fold ~none:false
                     ~some:(fun n -> n > config.max_drain)
                     (H1_body.unread body) ->
                `Fail_500
            | None -> (
              match continue_before_switch response body with
              | exception Body.Truncated ->
                  `Left
              (* What follows the request body is for the new protocol. *)
              | () when not (drain body) ->
                  `Fail_500
              (* The stop can start during the 100 or the drain. *)
              | () when ctx.is_stopping () ->
                  `Fail_503
              | () ->
                  (* The writer ignores keep_alive for an upgrade. *)
                  ignore
                    (write ~committed ~body
                       ~keep_alive:(fun () ->
                         (true
                         [@mutate
                           off "the head of an upgrade has no keep-alive"] ) )
                       head response ) ;
                  `Switch )
          with
          | `Switch ->
              switch f
          (* The release comes before the error response: its write can wait for
             a client that does not read. *)
          | `Fail_500 ->
              abandon response ; fail_500 head
          | `Fail_503 ->
              abandon response ; fail_503 head
          | `Left ->
              abandon response ; `Close (* the client left *)
          | exception e ->
              let bt = Printexc.get_raw_backtrace () in
              abandon response ;
              Printexc.raise_with_backtrace e bt )
        (* RFC 9110 §9.3.6: a 2xx to CONNECT opens a tunnel. *)
        | Response.Body b
          when Method.equal head.meth Method.CONNECT
               && Status.kind (Response.status response) = `Success ->
            (* Close the body first, thus its hook runs before the 500. *)
            (try Body.close b with e when not (is_fatal e) -> report e) ;
            report
              (Failure "a 2xx response to CONNECT must be a Response.upgrade") ;
            fail_500 head
        | Response.Body _ ->
            (* The engine decides when it writes the head. A streamed response
               writes it at its first output: before that, it can read the
               request body and get a 100 Continue sent. The engine knows if it
               can drain an unread body only after the response. *)
            let keep_alive () =
              request_keep_alive head
              && (not (H1_write.wants_close response))
              && (not (H1_body.continue_owed body))
              (* Its framing is not correct any more. *)
              && (not (H1_body.failed body))
              && not (ctx.is_stopping ())
            in
            if write ~committed ~body ~keep_alive head response && drain body
            then begin
              (* The other connections of this domain run before the next
                 request of this connection. Without the yield, the order of the
                 fibers can change, but no result changes. The fiber also waits
                 in the await of each exchange. *)
              begin[@mutate off "order only"]
                Fiber.yield ()
              end ;
              next_request ~idle:config.idle_timeout
            end
            else `Linger ) )
  (* The response of the handler, or [`Disconnected] (the exchange is cancelled)
     if the client leaves first. The engine watches the connection only after
     the handler read the request body, because the handler reads the body until
     then. Bytes that come are a pipelined request: the engine keeps them for
     later. *)
  and await p body ~ended =
    let response = Promise.ready p in
    let read = wrap (H1_body.completed body) (fun () -> `Read) in
    (* The engine cancelled the exchange because the client left. A failure of
       the handler during its cleanup is an error. The engine sends a response
       that came anyway. *)
    let left () =
      Promise.cancel p ;
      match Promise.await p with
      | r ->
          `Response r
      | exception Promise.Cancelled ->
          `Disconnected
      | exception e when not (is_fatal e) ->
          report e ; `Disconnected
    in
    (* A wake can find nothing to read, as in [wait_for_input]. Then the engine
       syncs the same event again. *)
    let watch () =
      let wake =
        choose
          [ wrap response (fun r -> `Response r)
          ; wrap (Flow.readable flow) (fun () -> `Readable) ]
      in
      let rec loop () =
        match sync wake with
        | `Response r ->
            `Response r
        | `Readable -> (
          match Buf_reader.fill_now r with
          | `Bytes ->
              `Response (Promise.await p)
          | `Would_block ->
              loop ()
          | `Eof | (exception Flow.Closed) ->
              left ()
          (* A different failure of the read: the engine cannot use the
             connection either. *)
          | exception e when not (is_fatal e) ->
              report e ; left () )
      in
      loop ()
    in
    if not config.cancel_on_disconnect then `Response (Promise.await p)
    else
      match sync (choose [wrap response (fun r -> `Response r); read]) with
      | `Response r ->
          `Response r
      (* Without this case, watch reads the buffered bytes. *)
      | (`Read [@mutate off "the same result"])
        when ( Buf_reader.buffered r > 0 )
             [@mutate off "watch finds the buffered bytes: the same result"] ->
          `Response (Promise.await p)
      | `Read ->
          (* After the read of a request, the readiness of the flow is stale.
             Thus the first watch reads no data. On one domain, the exchange
             runs in this yield. A handler that ends at once then gives its
             response with no watch. *)
          Fiber.yield () ;
          if Atomic.get ended then `Response (Promise.await p) else watch ()
  (* Gives the connection to [f], with the bytes that came first. The new
     protocol sets its own deadlines and timeouts. *)
  and switch f =
    Flow.set_read_deadline flow None ;
    Flow.set_write_deadline flow None ;
    Flow.set_read_timeout flow None ;
    Flow.set_write_timeout flow None ;
    ( match f (Flow.prepend (Buf_reader.take_buffered r) flow) with
    | () | (exception (Flow.Closed | Flow.Timeout)) ->
        ()
    | exception e when not (is_fatal e) ->
        report e ) ;
    `Close
  (* A 500, and the connection ends. *)
  and fail_500 head =
    ignore (write ~keep_alive:(fun () -> false) head (internal_error ())) ;
    `Linger
  (* No Retry-After: a server that stops does not know when it comes back. *)
  and fail_503 head =
    ignore
      (write
         ~keep_alive:(fun () -> false)
         head
         (Response.string ~status:Status.service_unavailable
            "Service Unavailable\n" ) ) ;
    `Linger
  (* Writes [response]. The result is [true] if the engine wrote all of it and
     the connection can stay open, or switch protocols. The engine writes the
     head, and sets [committed], just before the first output of the body. It
     calls [keep_alive ()] then. Until that point, the engine can answer a
     failure of the body correctly. *)
  and write
      ?(committed =
        ref (false [@mutate off "write_head sets it before each failure"]))
      ?body ~keep_alive (head : H1_parse.head) response =
    let request_failed () = Option.exists H1_body.failed body in
    let framing =
      H1_write.framing ~meth:head.meth ~version:head.version response
    in
    let kept_alive =
      ref
        (false [@mutate off "H1_write.body calls write_head before it returns"])
    in
    let write_head () =
      committed := true ;
      kept_alive := keep_alive () && framing <> H1_write.Close_delimited ;
      H1_write.head w ?date:(date ()) ~version:head.version
        ~keep_alive:!kept_alive framing response
    in
    (* A response cut short after its head. Chunked and Content-Length framings
       show it, but a close-delimited body would look complete after a clean
       close: reset the connection. *)
    match Response.kind response with
    | Response.Upgrade _ ->
        write_head () ;
        Buf_writer.flush w ;
        true [@mutate off "the caller of an upgrade ignores the result"]
    | Response.Body b -> (
      match H1_write.body ~before:write_head w framing b with
      | () ->
          !kept_alive
      | exception e -> (
          let bt = Printexc.get_raw_backtrace () in
          (* Cut short after its head, for each cause: a body that fails, a
             write timeout, a cancel. [Flow.reset] does no sync, so a cancelled
             fiber can call it. *)
          if !committed && framing = H1_write.Close_delimited then
            Flow.reset flow ;
          match e with
          | Flow.Closed | Flow.Timeout ->
              Printexc.raise_with_backtrace e bt
          (* [bt] is taken: [is_fatal] raises inside. A cancel of a different
             scope is a failure of the producer, as for a handler. *)
          | Fiber.Cancelled _ when is_fatal e ->
              Printexc.raise_with_backtrace e bt
          (* A request body that the response streams was cut short or is
             malformed: the fault of the client, not of the server. *)
          | Body.Truncated when request_failed () ->
              false [@mutate off "drain gives false for a failed request body"]
          | H1_error.E e when request_failed () ->
              if not !committed then ignore (error ~meth:head.meth e) ;
              false [@mutate off "drain gives false for a failed request body"]
          | e ->
              config.on_error e bt ;
              if not !committed then ignore (fail_500 head) ;
              false ) )
  in
  try
    match next_request ~idle:config.header_timeout with
    | `Close ->
        ()
    | `Linger ->
        linger config flow
  with Flow.Closed | Flow.Timeout -> ()

(* A client that got the ALPN protocol h2 sends HTTP/2. The HTTP/1.1 engine
   cannot serve it: the TLS configuration offers h2, but the inner engine is not
   an HTTP/2 engine. That is an error of the configuration, thus the engine
   reports it, and the server closes the connection. *)
let serve : Engine.t =
 fun ctx flow handler ->
  match Conn_info.alpn ctx.conn with
  | Some "h2" ->
      ctx.config.on_error
        (Failure
           "Caravan.Server.http1: the ALPN protocol of the connection is h2. \
            Use Caravan_http2.auto or Caravan_http2.engine." )
        (Printexc.get_callstack 0
         [@mutate off "the report has no stack: no test reads it"] )
  | _ ->
      serve_http1 ctx flow handler
