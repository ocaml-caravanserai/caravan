# caravan

A web programming library for OCaml 5, built on
[weft](https://github.com/ocaml-caravanserai/weft). caravan has an HTTP/1.1
server, client and WebSocket, and on top of them a framework layer
similar to ASP.NET Core: routing, middleware, sessions, authentication,
compression, static files and TLS. weft is a concurrency library with
fibers, structured concurrency, cancellation, CML events and a
deterministic simulator: the test host of caravan runs a whole
application in that simulator.

```ocaml
let () =
  Caravan.Server.run (module Weft_os) (fun _req ->
      Caravan.Response.string "Hello!\n" )
```

`Caravan.Server.run` serves the handler on 127.0.0.1:8080 until the
process gets SIGINT or SIGTERM. `Weft_os` (library `weft.os`) is the
world of weft for the OS of the build: Linux, macOS, FreeBSD or Windows.
On another system, `Weft_os.run` raises `Failure`. The build compiles
this example (examples/dune).

> **Status:** the server, the client, WebSocket and the framework
> layer are complete but young, and their API can change. The HTTP/2
> engine is new. HTTP/3 and a reverse proxy are not there yet.

## Contents

- [Libraries](#libraries)
- [Examples](#examples)
- [What the server does](#what-the-server-does)
- [Routing](#routing)
- [Middleware](#middleware)
- [HTML](#html)
- [Queries and forms](#queries-and-forms)
- [Cookies](#cookies)
- [Sessions](#sessions)
- [CSRF](#csrf)
- [Authentication](#authentication)
- [Users and policies](#users-and-policies)
- [OpenID Connect](#openid-connect)
- [Codes by email](#codes-by-email)
- [Passkeys](#passkeys)
- [API keys](#api-keys)
- [Compression](#compression)
- [File uploads](#file-uploads)
- [Static files](#static-files)
- [TLS](#tls)
- [HTTP/2](#http2)
- [WebSocket](#websocket)
- [Client](#client)
- [Background jobs](#background-jobs)
- [Testing](#testing)
- [Requirements](#requirements)
- [Development](#development)
- [License](#license)

## Libraries

Each part is a library. Use only the libraries that you need: the
optional dependencies are necessary only for the library that uses
them.

| Library | Content | Optional dependencies |
|---|---|---|
| `caravan` | The HTTP/1.1 server, requests, responses, handlers, middleware, cookies, queries, forms and file uploads | |
| `caravan.client` | The HTTP/1.1 client | |
| `caravan.websocket` | WebSocket for the server (RFC 6455) | |
| `caravan.router` | Routes, path parameters, 405 with `Allow`, groups | |
| `caravan.middleware` | Forwarded fields, request IDs, access log, CORS, error page, HTTPS redirect, HSTS, cross-origin protection, rate limit, concurrency limit, health checks, cache headers | |
| `caravan.session` | Sessions, signed cookies, CSRF tokens | digestif, eqaf, mirage-crypto-rng |
| `caravan.auth` | JWT, bearer and cookie schemes, users, policies, OpenID Connect, codes by email, passkeys | the dependencies of `caravan.session`, and mirage-crypto-pk, mirage-crypto-ec, x509, yojson, zarith |
| `caravan.compress` | gzip and deflate responses, decompression of request bodies | decompress |
| `caravan.static` | Static files with ETag, conditional requests and ranges | |
| `caravan.html` | HTML elements and attributes, with escaping and URL checks | |
| `caravan.jobs` | Background jobs in the process, with retries, that stop with the server. Durable jobs in a store of the application, unique jobs, schedules (`every`, `cron`) and recurring jobs | |
| `caravan.tls` | The TLS server, with ALPN, and HTTPS for the client | weft-tls, tls, x509, domain-name, ipaddr, mirage-crypto-rng |
| `caravan.http2` | The HTTP/2 server engine, and the selection of HTTP/2 or HTTP/1.1 for each connection | |
| `caravan.testing` | A test host: it runs a handler stack and the client in the weft simulator, with no sockets | |

The byte streams come from weft. `Caravan.Flow` is `Weft_flow.Flow` of
the library `weft.flow`, with `Flow.linger`, and `Caravan.Tcp` is
`Weft_flow.Tcp`. The TLS flow of `caravan.tls` is that of the package
`weft-tls` (`Weft_tls`).

caravan and weft are not on opam. Get them from their repositories, or
use the package `caravan` of the Nix flake, which has all the optional
dependencies. Then add the libraries to the dune stanza of your
executable:

```dune
(executable
 (name main)
 (libraries caravan caravan.router caravan.middleware weft weft.os))
```

## Examples

Each directory in the table is a small program for one area, with
tests. `app.ml` is the example, and `main.ml` starts the server.
`test.ml` tests each request of the comment at the top of `app.ml`,
with `caravan.testing`. Run an example with `dune exec
examples/<name>/main.exe`, and its tests with `dune test
examples/<name>`. [examples/](examples) also has the build rules that
compile the examples of this README.

| Example | It shows |
|---|---|
| [hello](examples/hello/app.ml) | Plain and streamed responses, the configuration of the server, a graceful stop, a service |
| [middleware](examples/middleware/app.ml) | Middleware, typed attributes on requests and responses |
| [router](examples/router/app.ml) | Routes, parameters, groups with middleware, 405 and `OPTIONS` |
| [forms](examples/forms/app.ml) | Queries, urlencoded forms and their errors, a refusal of cross-origin `POST`s |
| [cookies](examples/cookies/app.ml) | Cookies: set, read, list and remove, a refusal of cross-origin `POST`s |
| [uploads](examples/uploads/app.ml) | File uploads, streamed or in memory, a refusal of cross-origin `POST`s |
| [html](examples/html/app.ml) | HTML with `caravan.html`, escaping, JSON for scripts, htmx from a local file |
| [proxy_headers](examples/proxy_headers/app.ml) | An application behind a proxy: forwarded fields, request IDs, access log, health checks, HSTS, HTTPS redirect, error page |
| [limits](examples/limits/app.ml) | CORS, rate limit, concurrency limit, cross-origin protection, cache headers |
| [static](examples/static/app.ml) | Static files: ETag, 304, ranges, directories, a hidden dot file |
| [compress](examples/compress/app.ml) | gzip and deflate responses, a page with a secret not compressed (BREACH), compressed request bodies with a size limit |
| [tls](examples/tls/app.ml) | HTTPS with a certificate for development, ALPN, HSTS |
| [http2](examples/http2/app.ml) | HTTP/2 and HTTP/1.1 on one port over TLS, the selection without ALPN, many streams on one connection |
| [client](examples/client/app.ml) | The HTTP client: a gateway sketch that streams to one upstream, timeouts, 502 and 504, and a program that fetches a URL |
| [websocket](examples/websocket/app.ml) | A chat room: origins, a subprotocol, a broadcast that a peer that reads nothing delays for at most 5 s, ping, the close at a stop |
| [jobs](examples/jobs/app.ml) | Background jobs in the process: retries, delays, a full queue, counts |
| [durable_jobs](examples/durable_jobs/app.ml) | Durable jobs: a store of the application, unique and delayed jobs, recurring jobs on a schedule, a rate limit |
| [sessions](examples/sessions/app.ml) | Sessions with a store, CSRF tokens, signed links |
| [auth](examples/auth/app.ml) | Login with a session, policies on roles, the return path, logout |
| [api](examples/api/app.ml) | A JSON API: API keys with an expiry and a revocation by the key text, bearer tokens (JWT), a custom scheme, scopes, a rate limit for each user |
| [email_codes](examples/email_codes/app.ml) | Sign-in with a one-time code by mail, a job that sends it, limits |
| [passkeys](examples/passkeys/app.ml) | Passkeys (WebAuthn): registration and sign-in with a browser page |
| [oidc](examples/oidc/app.ml) | Sign-in with an OpenID provider (for example Google): login, callback, users |
| [testing](examples/testing/test.ml) | The test host: races over seeds, manual time, signals, errors, latency |

## What the server does

- A strict RFC 9112 parser, written from scratch: CRLF only, no
  obs-fold, Content-Length and Transfer-Encoding rules that resist
  request smuggling, and limits on the request line, the header section
  and bodies (414, 431, 413).
- Streaming request and response bodies, with backpressure: chunked
  encoding with trailers, `Expect: 100-continue`, keep-alive and
  pipelining.
- Header, idle, read and write timeouts. A lingering close, so that a
  connection reset does not lose a response. A maximum number of
  connections.
- Graceful shutdown on SIGINT or SIGTERM: stop accepting, let requests
  in progress finish within a grace period, then cancel them. A second
  signal aborts. A third signal gets its default action, thus it also
  ends a handler that ignores the cancel. A handler can see the stop with
  `Conn_info.stopping`. `Server.run` does this in an OS world of weft.
- `Response.upgrade` hands the connection over after a `101 Switching
  Protocols` or a `2xx` to `CONNECT`, the basis for WebSocket and
  tunnels. If the switch does not happen (a 500, a 503 while the server
  stops, a client that left), its `?on_refused` function runs in place
  of the upgrade, for example to close an upstream connection.
- A handler whose client disconnects is cancelled, along with what it
  started, such as an upstream request.

Handlers are plain functions, `Request.t -> Response.t`, and each
request runs in its own fiber, in parallel across domains.

A middleware is a handler that wraps another, and `Handler.compose`
puts middleware around a handler. Typed attributes (`Attrs`) let a
middleware give data to the handlers after it, for example the user of
a token. A handler can also give data back to an outer middleware in
its response, for example a route for an access log. Attributes never
go on the wire:

```ocaml
let user : string Attrs.key = Attrs.key ~name:"user" ()

let auth : Handler.middleware =
 fun next req -> next (Request.with_attr user "ann" req)

let hello req =
  match Request.find_attr user req with
  | Some name -> Response.string ("Hello, " ^ name ^ "!\n")
  | None -> Response.string ~status:Status.unauthorized ""

let handler = Handler.compose [auth] hello
```

[examples/middleware](examples/middleware/app.ml) has a full example.

All libraries except `caravan.testing` depend on no world of weft:
`caravan`, `caravan.client`, `caravan.websocket`, `caravan.router`,
`caravan.middleware`, `caravan.session`, `caravan.auth`,
`caravan.compress`, `caravan.static`, `caravan.html`, `caravan.jobs`,
`caravan.tls` and `caravan.http2`. They run in each world, for example
the OS world (`Weft.run (module Weft_os)`) and the simulator of weft
(`Weft_sim.run`). `caravan.testing` runs only in the simulator.

[examples/hello](examples/hello/app.ml) has streamed responses, the
configuration of the server, the graceful stop and a service.

## Routing

The `caravan.router` library gives a handler for each method and path
pattern:

```ocaml
let app =
  Caravan_router.(
    handler
      [ get "/" (fun _ -> Caravan.Response.string "home\n")
      ; group "/users"
          [ get "" (fun _ -> Caravan.Response.string "all users\n")
          ; get "/:id" (fun req ->
                Caravan.Response.string ("user " ^ param "id" req ^ "\n") ) ]
      ] )
```

- A parameter `:id` matches one segment, and `**` matches the rest of
  the path. The router decodes each segment after the split.
- `/users` and `/users/` are different paths.
- A literal comes before a parameter, and a parameter before `**`, also
  when the order of the list is different.
- The router gives `405` with `Allow`, a `204` with `Allow` to
  `OPTIONS`, and the `GET` route to `HEAD`. It gives `400` to a path
  with `.` or `..` segments. But a decoded value can contain `../`
  (from `%2F`), thus a parameter or the rest is not a safe file path.
- A group can have its own middleware. The template of the route
  (`Caravan_router.template`) is on the request and on the response,
  for an access log.

[examples/router](examples/router/app.ml) has a full example.

## Middleware

The `caravan.middleware` library has middleware for the requests that
come through a reverse proxy, request IDs, an access log, CORS, an
error page, the HTTPS redirect, HSTS, a protection against CSRF, a
rate limit, a concurrency limit, health checks and cache headers:

```ocaml
open Caravan_middleware

let app handler =
  Caravan.Handler.compose
    [ Forwarded.middleware ~trusted:(Forwarded.trust_ips ["10.0.0.1"]) ()
    ; Request_id.middleware ()
    ; Access_log.middleware
        ~log:(fun e -> print_string (Access_log.line e ^ "\n"))
        ()
    ; Hsts.middleware ()
    ; Https_redirect.middleware ()
    ; Cors.middleware
        ~origins:(Cors.Only ["https://app.example"])
        ~allow_headers:["content-type"]
        ()
    ; Error_page.middleware
        ~report:(fun _req e _bt -> prerr_endline (Printexc.to_string e))
        ()
    ; Cross_origin.middleware
        ~trusted_origins:["https://app.example"]
        () ]
    handler
```

- `Forwarded` reads `X-Forwarded-For` and `X-Forwarded-Proto` of a
  trusted proxy, and `X-Forwarded-Host` if you turn it on. It walks the
  elements from the right, at most `limit` hops (1 by default), thus it
  does not read a value that a client adds before the elements of the
  trusted proxies. `Forwarded.client` gives the client, and the request
  gets the scheme and the authority that the proxy tells. The proxy
  must set or overwrite each field that is on, and a bad value gives the
  client `Unknown`, never the proxy.
- `Request_id` keeps a clean `X-Request-Id` of the request, or makes a
  new ID. The ID goes on the request and on the response.
- `Access_log` gives one entry for each request at the end of its
  response: the client, the request ID, the route, the status, the
  bytes, the duration, and if the body was complete, incomplete,
  failed, cancelled (the client left), not sent or upgraded.
  `Access_log.line` gives one line of text.
- `Cors` answers the preflights itself, and adds the CORS fields to the
  responses for the permitted origins (`Any`, `Only` a list, or
  `Matching` a predicate), with a correct `Vary`. It refuses `Any` with
  credentials. CORS is not access control: the handler runs for each
  origin, and only the read by the page is blocked. `Cors.permits` gives
  the same check to `Caravan_websocket.accept ~origin`.
- `Error_page` gives a page (by default a plain 500) for an exception of
  the handler, after a call of `report`. A cancel and the error of a
  request body go to the server as before.
- `Https_redirect` sends each `http` request to `https` with a 307 (308
  is better in production, when HTTPS works). A request with no valid
  authority, or with a path that does not start with `/`, gets 400.
- `Hsts` adds `Strict-Transport-Security` (365 days by default) to the
  responses of `https` requests, but not for `localhost` (also
  `*.localhost`), `127.0.0.1` and `[::1]`.
- `Cross_origin` refuses (403) a request with a method other than GET,
  HEAD and OPTIONS that a browser sends from another origin, as
  `CrossOriginProtection` of Go 1.25. It reads `Sec-Fetch-Site`, or else
  `Origin`, and needs no token. A `same-site` request is refused too:
  thus a page of `https://app.example.com` that sends a POST to
  `api.example.com` needs `https://app.example.com` in
  `trusted_origins`. Put the CORS origins with credentials there too.
  `exempt` lets a path, for example a webhook, take cross-origin POSTs.
- `Rate_limit` gives each client (an IPv4 address, or the /64 of an
  IPv6 address) a token bucket of `burst` requests, with one new token
  `every` span, and refuses the other requests with 429 and
  `Retry-After`. `Concurrency_limit` serves at most `limit` requests at
  the same time, and refuses the others with 503.
- `Health.middleware ~path:"/health" checks` runs the checks in
  parallel, each with a timeout, and answers 200 (`Healthy`,
  `Degraded`) or 503 (`Unhealthy`). Put it before the redirect and the
  limits, so that a probe gets the true status.
- `Cache_control.make` builds a checked `Cache-Control` value, and
  `Cache_control.middleware` adds it to the cacheable responses with no
  field. A response for one user (`Authorization`, `Cookie`,
  `Set-Cookie`) gets the private form of a value for a shared cache.
- The order: `Forwarded` first (it gives the scheme of the client), then
  `Request_id` and `Access_log` (so that each redirect and each refusal
  has a log entry), then `Health`, then `Hsts`, `Https_redirect` and
  `Cors`, then the limits, `Error_page` and `Cross_origin` (a refusal
  gets its CORS fields, and a preflight takes no token), then the
  compression and `Cache_control`, then the sessions and the router.
  Add `Retry-After` to the `expose_headers` of `Cors`. A middleware
  that changes the path goes after `Https_redirect`, so that the
  redirect keeps the path of the client.

[examples/proxy_headers](examples/proxy_headers/app.ml) has the
middleware for an application behind a proxy, and
[examples/limits](examples/limits/app.ml) has CORS, the limits, the
protection against CSRF and the cache headers.

## HTML

The `caravan.html` library builds HTML in OCaml code. `El` has the
elements and `At` has the attributes. The library escapes text and
attribute values, thus data of a user cannot make markup:

```ocaml
open Caravan_html

let item (name, done_) =
  El.li ~at:[At.classes ["item"; (if done_ then "done" else "")]] [txt name]

let page ~user items =
  list
    [ doctype
    ; El.html ~at:[At.lang "en"]
        [ El.head [El.meta ~at:[At.v "charset" "utf-8"] (); El.title "Tasks"]
        ; El.body
            [ El.h1 [txtf "Tasks of %s" user]
            ; El.ul (List.map item items)
            ; El.a ~at:[At.href "/new"] [txt "New task"] ] ] ]

let handler _req = response (page ~user:"<ada>" [("Write", true); ("Test", false)])

(* The hidden input of Caravan_session.Csrf, for a form. *)
let csrf_input req =
  El.input
    ~at:
      [ At.type_ "hidden"
      ; At.name "_csrf"
      ; At.value (Caravan_session.Csrf.token req) ]
    ()
```

- `response` sends `Content-Type: text/html; charset=utf-8`. The
  escaping is safe only in UTF-8.
- A URL attribute (`href`, `src`, `action`, the `hx-get` family of
  htmx, ...) keeps only a relative URL or `http`, `https`, `mailto` and
  `tel`. Other schemes, for example `javascript:`, become
  `about:invalid#caravan`.
- `At.v` refuses the names whose values run as script: `on*`, `srcdoc`,
  `hx-on*`, the `_` of hyperscript and others. It also refuses an
  `hx-vals`, `hx-headers` or `hx-request` value that is not empty and is
  not a JSON object, and an `hx-trigger` value with a filter (`[`). Use
  `At.unsafe` for a value of your code.
- The attributes of other client frameworks (Alpine, Vue, Datastar, ...)
  run their values as script. Do not give them data of a user.
- `El.script` and `El.style` refuse a text that can close the element.
  For data of a user, use `El.json_script`.
- All classes of an element join. Another duplicate attribute raises
  `Invalid_argument` when the code builds the element.
- The library does not check the content model of HTML5.

[examples/html](examples/html/app.ml) has a full example.

## Queries and forms

`Request.query_params` gives the pairs of the query, and
`Request.form` reads an `application/x-www-form-urlencoded` body:

```ocaml
open Caravan

let search req =
  match Form.find "q" (Request.query_params req) with
  | Some q ->
      Response.string ("results for " ^ q ^ "\n")
  | None ->
      Response.string ~status:Status.bad_request "no q\n"

let login req =
  match Request.form ~max_bytes:4096 req with
  | Ok f ->
      let user = Option.value ~default:"" (Form.find "user" f) in
      Response.string ("hello " ^ user ^ "\n")
  | Error e ->
      Response.string ~status:(Form.error_status e)
        (Format.asprintf "%a\n" Form.pp_error e)
```

- A pair list (`Form.t`) keeps the order and the duplicates.
  `Form.find` gives the first value, and `Form.find_all` gives all of
  them. Names are case-sensitive.
- The parse follows the WHATWG URL Standard: `+` is a space, and an
  incorrect `%` stays as it is. The parse never fails.
- `Request.form` reads the body, thus call it one time. It gives
  `Unsupported_media_type`, `Too_large` (1 MiB by default) or
  `Too_many_fields` (1000 by default). `Form.error_status` gives `415`
  or `413`. A body that declares a length larger than the limit is not
  read, thus the client gets no `100 Continue`.
- `Request.with_form` gives a request a form body, for the client or for
  a test.

[examples/forms](examples/forms/app.ml) has a full example.

## Cookies

`Request.cookie` reads a cookie of the request, and
`Response.add_cookie` sends a `Set-Cookie` field:

```ocaml
open Caravan

let theme = Cookie.make ~secure:true ~max_age:(30 * 24 * 3600) "theme" ""

let show req =
  match Request.cookie "theme" req with
  | Some t ->
      Response.string ("theme " ^ t ^ "\n")
  | None ->
      Response.string "no theme\n"

let set req =
  match Form.find "t" (Request.query_params req) with
  | Some (("light" | "dark") as t) ->
      Response.string "set\n"
      |> Response.add_cookie (Cookie.with_value t theme)
  | _ ->
      Response.string ~status:Status.bad_request "t: light or dark\n"

let clear _ =
  Response.string "cleared\n" |> Response.add_cookie (Cookie.expired theme)
```

- `Cookie.make` validates the name, the value and the attributes. It
  raises `Invalid_argument` for a value with a space or a `;`, for
  `SameSite=None` with no `Secure`, or for a `__Host-` name with a
  domain. Encode other bytes, for example with base64url.
- The defaults are `Path=/`, `HttpOnly` and `SameSite=Lax`. A cookie
  that a script reads needs `~http_only:false`. Use `~secure:true` in
  production.
- `Cookie.with_value` keeps the attributes, and `Cookie.expired` gives
  the cookie that removes it from the user agent.
- `Request.cookie` gives the first cookie with the name. The user agent
  gives the order, and a sibling subdomain can set a cookie with the
  same name: use a `__Host-` name for a security cookie.
- `Cookie.pp` shows the length of the value, not the value.

[examples/cookies](examples/cookies/app.ml) has a full example.

## Sessions

The optional `caravan.session` library keeps a session for each client,
with a signed cookie and a store:

```ocaml
let () = Mirage_crypto_rng_unix.use_default ()

(* A key of 32 random bytes, from the configuration of all the
   processes. *)
let key = Caravan_session.Key.of_base64url (Sys.getenv "SESSION_KEY")

let app =
  Caravan.Handler.compose
    [ Caravan_session.middleware ~keys:[key]
        ~store:(Caravan_session.memory_store ())
        () ]
    (fun req ->
      match Caravan.Request.path req with
      | "/login" ->
          (* A new ID after a login: no session fixation. *)
          Caravan_session.renew req ;
          Caravan_session.set "user" "ada" req ;
          Caravan.Response.string "welcome"
      | "/logout" ->
          Caravan_session.clear req ;
          Caravan.Response.string "bye"
      | _ ->
          Caravan.Response.string
            (Option.value ~default:"guest" (Caravan_session.find "user" req)) )
```

- The cookie is `__Host-session` by default: `Secure`, `HttpOnly`,
  `SameSite=Lax`, `Path=/`. A response that sets it gets
  `Cache-Control: no-store`.
- A session ends at most 30 min after the last request (between 15 and
  30 min: the limit moves only after half of it), and 8 h after its
  creation. `clear` ends it on the server: a request that runs at the
  same time cannot bring it back.
- `Caravan_session.store` connects a store of the application, for
  example a database. The store keys are hashes of the IDs.
- `Caravan_session.Signed` signs other values with the same keys. The
  first key signs, and the other keys verify: a rotation.
- Make no session for an anonymous user: a session starts at the first
  `set` (or `id`).

[examples/sessions](examples/sessions/app.ml) has a full example, with
CSRF tokens and signed links.

## CSRF

`Caravan_middleware.Cross_origin` stops the cross-origin requests of
the browsers with no token. `Caravan_session.Csrf` adds tokens bound to
the session, also for older browsers:

```ocaml
let () = Mirage_crypto_rng_unix.use_default ()

let key = Caravan_session.Key.of_base64url (Sys.getenv "SESSION_KEY")

let app =
  Caravan.Handler.compose
    [ Caravan_middleware.Cross_origin.middleware ()
    ; Caravan_session.middleware ~keys:[key]
        ~store:(Caravan_session.memory_store ())
        ()
    ; Caravan_session.Csrf.middleware () ]
    (fun req ->
      match Caravan.Request.meth req with
      | GET ->
          Caravan.Response.string ~content_type:"text/html; charset=utf-8"
            ( "<form method=post>\
               <input type=hidden name=_csrf value=\""
            ^ Caravan_session.Csrf.token req
            ^ "\"><button>Send</button></form>" )
      | _ ->
          Caravan.Response.string "sent" )
```

- The order: `Cross_origin`, then the session, then `Csrf`.
- `Csrf.token` gives a new text for each call (a random mask against
  BREACH), and the response gets `Cache-Control: no-store`. A login
  (`renew`) and a logout (`clear`) end the tokens.
- The middleware reads the token from `X-CSRF-Token`, or from the field
  `_csrf` of an urlencoded form. The handler gets the same bytes of the
  form. Another body (JSON, multipart) needs the header.
- A request with no session has no token: by default it gets 403. A
  login form for anonymous users has no session: exempt it, and
  `Cross_origin` protects it.
- Use `Csrf` only for the routes of the cookie sessions. A JSON API
  with a bearer token needs no token against CSRF.

[examples/sessions](examples/sessions/app.ml) has CSRF tokens in forms,
and a multipart upload that calls `Csrf.check`.

## Authentication

The optional `caravan.auth` library verifies JSON Web Tokens, for
example the bearer tokens of an identity provider:

```ocaml
(* The keys of the identity provider: its JWK Set, from the
   configuration. *)
let keys =
  match Caravan_auth.Jwt.Key.of_jwks (Sys.getenv "JWKS") with
  | Ok keys -> keys
  | Error e -> failwith e

(* The user of a request with "Authorization: Bearer <token>". The
   name of the scheme is case-insensitive (RFC 9110 §11.1). *)
let user req =
  match Caravan.Headers.find "authorization" (Caravan.Request.headers req) with
  | Some v
    when String.starts_with ~prefix:"bearer " (String.lowercase_ascii v) -> (
      let token = String.sub v 7 (String.length v - 7) in
      match
        Caravan_auth.Jwt.verify
          ~issuer:(One_of ["https://id.example"])
          ~audience:(One_of ["api://orders"])
          ~keys token
      with
      | Ok claims -> Caravan_auth.Jwt.subject claims
      | Error _ -> None )
  | _ -> None
```

- The algorithms are HS256, RS256, ES256 and EdDSA (Ed25519). A key
  has one algorithm, thus a token cannot choose another one. `none` is
  always refused.
- The keys come from a JWK Set, a PEM (a public key or a certificate),
  or a secret of 32 bytes or more. A key never comes from a token.
- `verify` checks the header and the signature first, then `exp`
  (required by default) and `nbf` with a leeway of 60 s, then `iss`
  and `aud`. The issuer and the audience are required arguments: `Any`
  skips a check.
- The JSON must be strict RFC 8259: no comments, no `NaN`, no
  duplicate member names.

[examples/api](examples/api/app.ml) verifies bearer tokens with a JWK
Set.

## Users and policies

`Caravan_auth.middleware` finds the user of each request with a list of
schemes, and `Caravan_auth.require` protects the routes:

```ocaml
let () = Mirage_crypto_rng_unix.use_default ()

let session_key =
  Caravan_session.Key.of_base64url (Sys.getenv "SESSION_KEY")

let jwks =
  match Caravan_auth.Jwt.Key.of_jwks (Sys.getenv "JWKS") with
  | Ok keys -> keys
  | Error e -> failwith e

let bearer =
  Caravan_auth.Scheme.bearer
    ~issuer:(One_of ["https://id.example"])
    ~audience:(One_of ["api://orders"])
    ~keys:(fun () -> jwks)
    ()

let session = Caravan_auth.Scheme.session ~login:"/login" ()

let routes =
  Caravan_router.(
    handler
      [ get "/" (fun _ -> Caravan.Response.string "home")
      ; post "/login" (fun req ->
            (* Check the password of the form here. *)
            Caravan_auth.sign_in req
              (Caravan_auth.User.make ~roles:["admin"] "ada") ;
            Caravan.Response.string "welcome" )
      ; post "/logout" (fun req ->
            Caravan_auth.sign_out req ;
            Caravan.Response.string "bye" )
      ; group
          ~middleware:
            [Caravan_auth.require ~policy:(Caravan_auth.role "admin") ()]
          "/admin"
          [get "" (fun _ -> Caravan.Response.string "admin")] ] )

let app =
  Caravan.Handler.compose
    [ Caravan_middleware.Cross_origin.middleware ()
    ; Caravan_session.middleware ~keys:[session_key]
        ~store:(Caravan_session.memory_store ())
        ()
    ; Caravan_auth.middleware ~schemes:[session; bearer] ()
    ; Caravan_session.Csrf.middleware
        ~exempt:(fun req ->
          Caravan.Request.path_only req = "/login"
          ||
          match Caravan_auth.user req with
          | Some u -> Caravan_auth.User.scheme u = "Bearer"
          | None -> false )
        () ]
    routes
```

- A scheme gives `Authenticated`, `No_credentials` (the next scheme)
  or `Failed` (no user, and no other scheme). The schemes run at the
  first call of `user`, `failure`, `challenge` or `require`: a public
  `GET` costs no check. The user is that of the session at the start of
  the request: a `sign_in` in the handler shows in the next request.
  The middleware never refuses a request.
- `require` gives the challenge of the scheme to a request with no
  user, and 403 to a user that the policy refuses (`authenticated`,
  `role`, `scope`, `all`, `any`). The first scheme gives the
  challenge: here the session, with a 303 to the login page for the
  navigation of a browser and a 401 for the other requests. On the
  routes of an API, use `require ~challenge:"Bearer"`. A response of
  the protected handler with no `Cache-Control` gets
  `Cache-Control: private`.
- `sign_in` gives the session a new ID. A second `sign_in` ends the
  old session and its data. `User.scheme` is the scheme that
  authenticated the user for this request: the bearer users are exempt
  from the CSRF tokens, because a browser never sends a bearer token by
  itself.
- The login form is exempt from the CSRF tokens: an anonymous user has
  no session yet. `Cross_origin` protects it, but a browser older than
  2020 can send it from another site (a login CSRF).
- After a login, redirect only to `Caravan_auth.return_path`.

[examples/auth](examples/auth/app.ml) has a full example.

## OpenID Connect

`Caravan_auth.Oidc` is for the sign-in with an OpenID provider, for
example Google. `Oidc.Provider.discover` reads the discovery document of
the issuer, and `Oidc.verify_id_token` checks an ID token with the keys
of the provider:

```ocaml
let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let () =
  Mirage_crypto_rng_unix.use_default () ;
  let tls =
    Caravan_tls.https
      (ok (Tls.Config.client ~authenticator:(ok (Ca_certs.authenticator ())) ()))
  in
  Weft.run (module Weft_os) @@ fun () ->
  Caravan_client.run ~tls @@ fun client ->
  match Caravan_auth.Oidc.Provider.discover client "https://accounts.google.com" with
  | Error e -> prerr_endline e
  | Ok google ->
      (* In a handler: [token] is the ID token of the provider. *)
      let check token =
        Caravan_auth.Oidc.verify_id_token google ~client_id:"123.apps" token
      in
      ignore check
```

- The provider keeps a cache of its keys: for the `max-age` of the
  response, from 5 minutes to 24 hours. One load at a time, with a
  deadline of 10 s. A failed load keeps the old keys. A token of an
  unknown key loads the keys again, at most once a minute.
- `verify_id_token` checks the signature, `iss`, `aud`, `exp`, `azp`,
  `sub` and, if you give it, the nonce. It refuses an access token
  (`typ: at+jwt`).
- The tokens of the "Sign in with Google" button have one of two
  issuers: give `~issuers:["https://accounts.google.com";
  "accounts.google.com"]`.

`Oidc.login` and `Oidc.callback` do the sign-in with the authorization
code flow and PKCE. Your function gets the checked tokens, finds or
makes the user, and signs the user in:

```ocaml
let google provider =
  Caravan_auth.Oidc.client ~secret:"..." ~provider ~id:"123.apps"
    ~redirect_uri:"https://app.example/auth/callback" ()

(* The key of an account is the issuer and the subject. Never find or link
   an account by the email claim. *)
let signed_in req (t : Caravan_auth.Oidc.tokens) ~return =
  let issuer = Option.get (Caravan_auth.Jwt.issuer t.claims) in
  let user = find_or_make_user ~issuer ~subject:t.subject in
  Caravan_auth.sign_in req user ;
  Caravan.Response.empty ~status:Caravan.Status.see_other
    ~headers:(Caravan.Headers.of_list [("Location", return)]) ()

let routes client =
  Caravan_router.
    [ get "/auth/login" (Caravan_auth.Oidc.login client)
    ; get "/auth/callback" (Caravan_auth.Oidc.callback client signed_in) ]
```

- `login` keeps a random state, nonce and PKCE verifier in the session
  (at most 4 logins, 10 minutes each), and sends the browser to the
  provider. Put a rate limit on the login route: each login of a
  browser with no session makes a session.
- `callback` takes the login of the `state` from the session (one use),
  checks the client and the `iss` parameter (RFC 9207), gets the
  tokens with the secret (`Basic` or `Post`), and checks the ID token
  with the nonce of the login.
- Give each client its own redirect URI. The session cookie must be
  `SameSite=Lax` (the default) or `None`.

[examples/oidc](examples/oidc/app.ml) has a full example, with a fake
provider in its tests.

## Codes by email

`Caravan_auth.Code` signs a user in with a one-time code that your
application sends to an address. caravan sends no mail: use SMTP, or the
HTTP API of a mail service through `Caravan_client`.

```ocaml
let codes = Caravan_auth.Code.make ()

(* POST /email *)
let email req address =
  ( match Caravan_auth.Code.start codes req ~address with
  | Ok code -> ignore (Caravan_jobs.enqueue jobs send_code (address, code))
  | Error _wait -> () ) ;
  page "We sent a code if the address is correct."

(* POST /code *)
let code req text =
  match Caravan_auth.Code.check codes req text with
  | Ok address -> Caravan_auth.sign_in req (find_or_make_user address) ; redirect "/"
  | Error _ -> page "The code is not correct."
```

- A code has 8 digits and lives 10 minutes. The session keeps a salted
  hash of it, with 5 attempts. A code works one time, and only in the
  browser that asked for it.
- `sends` limits the codes for each address (3, then one each 10
  minutes), and `checks` limits the checks for each address over all
  sessions (10, then one each hour). `check` takes a token before the
  compare. Thus an attacker without the mailbox has about 0.009 % a
  year for one address.
- The limits are functions of a key: give your own, for example of a
  store that all your processes share
  (`Caravan_middleware.Rate_limit.Limiter` is the default).
- Give the same response for known and unknown addresses, put a rate
  limit on the two routes, and give a second way to sign in: an
  attacker can keep the code sign-in of an address closed.

[examples/email_codes](examples/email_codes/app.ml) has a full example.

## Passkeys

`Caravan_auth.Passkey` does the server part of WebAuthn: the options
for `navigator.credentials.create()` and `.get()`, and the checks of
their responses.

```ocaml
let rp =
  Caravan_auth.Passkey.rp ~id:"example.com" ~name:"Example"
    ~origins:["https://example.com"]

(* Registration, for a signed-in user. *)
let options req user =
  Caravan_auth.Passkey.creation_options rp req ~user_id:user.handle
    ~user_name:user.email ~display_name:user.name

let register req body =
  match Caravan_auth.Passkey.register rp req ~registered:credential_exists body with
  | Ok c -> store c.id c.stored
  | Error e -> refuse e

(* Sign-in: request_options, then authenticate. *)
let sign_in req body =
  match Caravan_auth.Passkey.authenticate rp req ~lookup:find_credential body with
  | Ok a when a.cloned -> refuse_or_warn a
  | Ok a ->
      update_sign_count a.credential_id a.sign_count ;
      Caravan_auth.sign_in req (user_of_handle a.user_handle)
  | Error e -> refuse e
```

In the browser, `PublicKeyCredential.parseCreationOptionsFromJSON` and
`parseRequestOptionsFromJSON` read the options, and
`credential.toJSON()` gives the body of the response.

- The keys: ES256, EdDSA and RS256 (COSE). The attestation is none: a
  statement of another format is not verified.
- Each ceremony keeps a random challenge in the session (at most 4, for
  5 minutes by default). A response uses its challenge one time.
- `authenticate` checks the user handle of the response against the
  stored credential, and gives the stored handle: the handle of a
  response is not signed.
- The counter: `cloned` tells a counter that did not grow, except for
  passkeys that sync and always send 0.
- `allow` needs `user`: the server checks the user of the credential,
  not the list.
- Read the body of a response with a limit of 64 KiB, and put a rate
  limit on the routes of the options: each one can make a session. Two
  copies of one response sent at the same time can both pass.

[examples/passkeys](examples/passkeys/app.ml) has a full example, with a
software authenticator in its tests.

## API keys

`Caravan_auth.Api_key` makes keys for a web API, and finds the user of
a key with its hash:

```ocaml
let live = Caravan_auth.Api_key.format ~prefix:"myapp_"

(* POST /keys, for a signed-in user: show the key one time, store the
   hash. *)
let create_key user =
  let k = Caravan_auth.Api_key.generate live in
  store_key ~owner:user ~hash:k.hash ;
  k.key

let api_key =
  Caravan_auth.Api_key.scheme live ~lookup:(fun hash ->
      match find_key hash with
      | Some row when not row.revoked ->
          Ok (Caravan_auth.User.make ~roles:row.roles row.owner)
      | Some _ -> Error "a revoked key"
      | None -> Error "an unknown key" )

let api =
  Caravan.Handler.compose
    [ (* Each client: this limits the reads of the store. *)
      Caravan_middleware.Rate_limit.middleware ~burst:20 ~every:(Span.s 1) ()
    ; Caravan_auth.middleware ~schemes:[api_key; bearer] ()
    ; (* Each user: the keys of one user share it. *)
      Caravan_middleware.Rate_limit.middleware ~burst:100
        ~every:(Span.ms 100)
        ~key:(fun req -> Option.map Caravan_auth.User.id (Caravan_auth.user req))
        ()
    ; Caravan_auth.require ~challenge:"ApiKey" () ]
    routes
```

- A key is the prefix and 43 base62 characters (256 bits). A secret
  scanner finds a leaked key by its prefix. The client sends
  `Authorization: Bearer <key>`, or the field that `~field` gives (for
  example `X-API-Key`). A key in the query of a URL is never read.
- Store only the hash (SHA-256, in hex). The lookup checks the expiry
  and the revocation, and must raise if the store fails: an `Error`
  gives a 401, and a client can then discard a good key.
- A token with a `.` is never a key. Thus a JWT is never a key, and the
  API key scheme and the JWT scheme share `Authorization`: put the API
  key scheme first. Two formats
  (for example `myapp_` and `myapp_test_`) also share one field.
- `Api_key.cache` keeps the user of a key for a short time. A revoked
  key works until its entry expires: call `Api_key.forget` after the
  revocation is in the store. Give a cache to one scheme only.
- A browser never sends a key by itself: with a session scheme too,
  exempt `"ApiKey"` from the CSRF tokens by its name.

[examples/api](examples/api/app.ml) has a full example, with API keys,
bearer tokens and a scheme of its own.

## Compression

The optional `caravan.compress` library compresses the responses with
gzip or deflate, and decodes compressed request bodies, with the pure
OCaml library decompress:

```ocaml
let app handler =
  Caravan.Handler.compose
    [ Caravan_compress.middleware ~level:6 ()
    ; Caravan_compress.decompress ~max_bytes:(4 * 1024 * 1024) ()
    ; Caravan_static.middleware ~root:"/srv/www" () ]
    handler
```

- The coding comes from `Accept-Encoding` (gzip on a tie). A response
  that depends on it gets `Vary: Accept-Encoding`.
- Only text types (`text/*` except `text/event-stream`, JSON,
  JavaScript, XML, SVG) with a known length of 2 KiB to 8 MiB are
  compressed. A stream is not: decompress cannot flush a part of a
  stream. `~streams:true` compresses streams too, but the client then
  gets the bytes in large blocks.
- A compressed response loses `Content-Length` and `Accept-Ranges`, and
  its ETag becomes weak (`W/"t"`, or `W/"t-deflate"` for deflate). The
  304 of `caravan.static` gets the same ETag. `If-Match` never matches a weak ETag: give the routes that use
  it the attribute, with
  `Caravan.Response.with_attr Caravan_compress.skip ()`.
- Each compressed response costs about 770 KiB of memory and 0.1 to
  0.2 ms while it goes out. At most 64 run at the same time
  (`max_active`): the other responses go out with no compression.
- BREACH: do not compress a page with a secret and text of the client.
- `decompress` decodes a request body with `Content-Encoding: gzip` or
  `deflate` (zlib), with a limit of decoded bytes (1 MiB by default)
  against zip bombs: 413 above it, 400 for a body that does not decode,
  415 for another coding. Put it after `Error_page`.
- For pure OCaml, add `checkseum.ocaml` to the libraries of your
  executable: the default checksums of decompress have C code.

[examples/compress](examples/compress/app.ml) has a full example.

## File uploads

`Request.multipart` streams the parts of a `multipart/form-data` body,
and `Request.multipart_form` keeps them in memory:

```ocaml
open Caravan

(* Counts the bytes of each file, with no copy in memory. *)
let upload req =
  let buf = Bytes.create 65536 in
  let rec size body n =
    match Body.read body buf 0 (Bytes.length buf) with
    | 0 ->
        n
    | k ->
        size body (n + k)
  in
  let count acc part =
    match Multipart.filename part with
    | Some _ ->
        (Multipart.name part, size (Multipart.body part) 0) :: acc
    | None ->
        acc
  in
  match Request.multipart req ~init:[] count with
  | Ok files ->
      Response.string
        (String.concat ""
           (List.map (fun (n, s) -> Printf.sprintf "%s: %d bytes\n" n s) files) )
  | Error e ->
      Response.string ~status:(Multipart.error_status e)
        (Format.asprintf "%a\n" Multipart.pp_error e)
```

- The body of a part is valid only in the callback: copy it to keep it.
  If the request body ends before the part ends, its read raises
  `Body.Truncated`, thus a copy does not end as if it were complete.
- The limits are `max_parts` (1000 by default) and `max_header_bytes`
  (16 KiB by default, for the head of each part). `multipart_form` also
  has `max_bytes` (8 MiB by default).
- A file name comes from the client: it is not a safe path.
- `Request.with_multipart` gives a request a multipart body, for the
  client or for a test.

[examples/uploads](examples/uploads/app.ml) has a full example.

## Static files

The `caravan.static` library serves the files of a directory. It reads
them with `Weft.File`, on the thread pool of weft, thus a read does not
stop the domain:

```ocaml
open Caravan

(* /assets/app.css is the file public/app.css. Other paths go to the
   handler. *)
let app handler =
  Handler.compose
    [ Caravan_static.middleware ~prefix:"/assets" ~root:"public"
        ~cache_control:(fun rel ->
          if rel = "index.html" then Some "no-cache" else Some "max-age=3600" )
        () ]
    handler
```

- A response has `ETag`, `Last-Modified` and `X-Content-Type-Options:
  nosniff`. `If-None-Match` and `If-Modified-Since` give `304`, and
  `If-Match` and `If-Unmodified-Since` give `412`.
- A `GET` with one byte range gets `206`, and a range out of the file
  gets `416`. `If-Range` is supported. More than one satisfiable range
  gives the full file.
- The middleware refuses `..`, `%2F`, a backslash, the reserved names
  of Windows, and the names that start with a dot (`~dotfiles:true`
  permits them). The rules are the same on each OS.
- A directory gets its `index.html`. A path of a directory with an
  `index.html` and with no `/` at the end gets a `301` to the path with
  a `/`.
- Each `GET` under the prefix, with a permitted path, costs one open
  of a file on the pool, also when the handler gets the request. Thus give a prefix, or use
  `Caravan_static.dir` on a route.
- The library follows symbolic links: do not let a client make files in
  the root.
- With `caravan.router`, `Caravan_static.dir ~root (Caravan_router.rest req)
  req` serves a route `/static/**`. `Caravan_static.file` serves one
  file.

[examples/static](examples/static/app.ml) has a full example.

## TLS

The optional `caravan.tls` library serves HTTPS with
[ocaml-tls](https://github.com/mirleft/ocaml-tls), a TLS library in
pure OCaml. Its engine does the handshake of each connection, then
gives the TLS flow to the HTTP/1.1 engine:

```ocaml
let read_file path = In_channel.with_open_bin path In_channel.input_all

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let () =
  (* The generator of the OS: safe on many domains. *)
  Mirage_crypto_rng_unix.use_default () ;
  let certs = ok (X509.Certificate.decode_pem_multiple (read_file "cert.pem")) in
  let key = ok (X509.Private_key.decode_pem (read_file "key.pem")) in
  let tls = ok (Tls.Config.server ~certificates:(`Single (certs, key)) ()) in
  Caravan.Server.run
    ~engine:(Caravan_tls.engine tls Caravan.Server.http1)
    (module Weft_os)
    (fun req -> Caravan.Response.string (Caravan.Request.scheme req))
```

- A request has the scheme `https`, and `Conn_info.alpn` tells the ALPN
  protocol. The ALPN list of the configuration can have `h2` and
  `http/1.1`, and its lowest version must be TLS 1.2 or later. With
  `h2`, the inner engine must serve HTTP/2 (see [HTTP/2](#http2)).
  `Caravan.Server.http1` refuses a session with the protocol `h2`.
- The engine refuses a configuration that permits the early data of
  TLS 1.3 (`zero_rtt` above 0): an attacker can replay early data, and
  caravan does not do the rules of RFC 8470.
- The handshake has the deadline `header_timeout`, and a stop of the
  server ends it. A handshake that fails is not an error of the server:
  the server reports nothing.
- `Caravan_tls.https` gives HTTPS to `caravan.client` (see
  [Client](#client)).
- `Caravan_tls.server` and `Caravan_tls.client` do a handshake on any
  `Caravan.Flow.t`, for example for a test or a proxy. They are
  `Weft_tls.server` and `Weft_tls.client` of the package `weft-tls`,
  whose doc gives the rules of the TLS flow. The client checks the
  `peer_name` of its `Tls.Config`: give one for each server on the
  Internet.
- A handshake can read at most 64 KiB. A session permits 64 small
  records with no plaintext (for example a KeyUpdate), and one more for
  each 16 KiB of plaintext.
- A client that ends the TCP connection with no `close_notify` gives
  `Flow.Closed` to a read: it can be a truncation.

[examples/tls](examples/tls/app.ml) has a full example, with a
certificate for development.

## HTTP/2

The `caravan.http2` library serves HTTP/2 (RFC 9113, with HPACK of RFC
7541). The handler is the same for HTTP/2 and HTTP/1.1: a request of
HTTP/2 has the version `Http_2`. This program serves HTTP/2 and
HTTP/1.1 on one port, over TLS:

```ocaml
let read_file path = In_channel.with_open_bin path In_channel.input_all

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let () =
  Mirage_crypto_rng_unix.use_default () ;
  let certs = ok (X509.Certificate.decode_pem_multiple (read_file "cert.pem")) in
  let key = ok (X509.Private_key.decode_pem (read_file "key.pem")) in
  (* The ALPN list: the client and the server agree on h2 or http/1.1. *)
  let tls =
    ok
      (Tls.Config.server
         ~certificates:(`Single (certs, key))
         ~alpn_protocols:["h2"; "http/1.1"] () )
  in
  Caravan.Server.run
    ~engine:(Caravan_tls.engine tls (Caravan_http2.auto ()))
    (module Weft_os)
    (fun req ->
      Caravan.Response.string
        (Caravan.Version.to_string (Caravan.Request.version req)) )
```

An engine serves the connections of a server (`?engine` of
`Server.run` and `Server.start`):

| Engine | It serves |
|---|---|
| `Caravan.Server.http1` | HTTP/1.1. The default engine. A connection with the ALPN protocol `h2` gets no response, and the server reports a `Failure`. `Caravan_tls.engine` refuses `Server.http1` itself with `h2` in the ALPN list. |
| `Caravan_http2.engine ()` | HTTP/2 only. The client must start with the preface of HTTP/2: it knows that the server has HTTP/2 (prior knowledge, for example behind a proxy), or TLS selected `h2`. It closes a connection of HTTP/1.1 with no report: with `http/1.1` in the ALPN list of TLS too, use `auto`. |
| `Caravan_http2.auto ()` | HTTP/2 and HTTP/1.1. The ALPN protocol `h2` selects HTTP/2. Another ALPN protocol, or TLS with no ALPN protocol, selects HTTP/1.1 (`?http1`, `Server.http1` by default): HTTP/2 over TLS needs ALPN (RFC 9113 section 3.3). With no TLS, `auto` compares the first bytes with the preface of HTTP/2 (24 bytes), within `header_timeout`. |
| `Caravan_tls.engine tls inner` | TLS, then the engine `inner`, with the ALPN protocol of the session in `Conn_info.alpn`. |

- Without TLS, `auto` also serves the two versions on one port:
  `curl --http2-prior-knowledge` gets HTTP/2, and a plain `curl` gets
  HTTP/1.1. An HTTP/1.1 request differs from the preface in its first 3
  bytes, thus the comparison usually needs one read. `auto` does not do
  the upgrade `Upgrade: h2c` of HTTP/1.1, which RFC 9113 removed.
- With `h2` in its ALPN list and TLS 1.2 permitted, `Caravan_tls.engine`
  refuses a cipher suite that RFC 9113 forbids: each one must have an
  ephemeral key exchange (ECDHE or DHE) and an AEAD cipher. The
  default list of ocaml-tls obeys this rule.
- `Caravan_http2.Settings` holds the values of the SETTINGS frame of the
  server (100 concurrent streams, windows of 64 KiB, a header table of
  4 KiB) and the limits against floods: rapid reset (CVE-2023-44487),
  floods of SETTINGS, PING, empty DATA or CONTINUATION, PRIORITY,
  WINDOW_UPDATE, RST_STREAM and unknown frames, the CONTINUATION flood,
  and HPACK bombs. A client over a limit gets GOAWAY
  ENHANCE_YOUR_CALM. In the tests, these clients trip no limit: a fake
  client that sends as a browser does, a fake client that sends PING
  frames as gRPC-Go does during a download, and curl, nghttp, h2load
  and h2spec. No real browser and no real gRPC client ran in the tests.
- The timeouts of `Config` apply: `header_timeout` for the preface
  and for each header block, `idle_timeout` with no stream, and the read
  and write timeouts for each stream. A stop of the server sends GOAWAY:
  the streams in progress finish.
- The engine sends no server push. A response of HTTP/2 has no
  connection fields: the engine drops them. `Response.upgrade` (for
  example WebSocket) and a 2xx response to `CONNECT` (a tunnel) need
  HTTP/1.1: under HTTP/2 the engine reports an error and sends 500.
  A browser opens a WebSocket on a new connection of HTTP/1.1, because
  the server does not offer WebSocket over HTTP/2 (RFC 8441).
- h2spec (147 cases, strict) passes, and so do curl, nghttp and h2load
  (`make h2spec` and `make interop`).

[examples/http2](examples/http2/app.ml) has a full example, with a
certificate for development.

## WebSocket

The `caravan.websocket` library adds WebSocket
([RFC 6455](https://www.rfc-editor.org/rfc/rfc6455)) to the server. A
handler gives the response of `Caravan_websocket.accept`:

```ocaml
let handler req =
  Caravan_websocket.accept req (fun ws ->
      let rec echo () =
        Caravan_websocket.(send ws (recv ws));
        echo ()
      in
      echo ())
```

- `accept` examines the request and sends the `101` response. By
  default, it permits browser pages only from the origin of the server.
- `recv` gives full messages. The library joins fragments, sends a Pong
  for each Ping, and examines the UTF-8 of text. It examines each part
  of a text frame as it arrives, thus incorrect text closes the
  connection before the end of the frame or of the message.
- Many fibers can send at the same time, and `close` closes the
  connection from any fiber, with a time limit.
- When the server stops, each connection sends a Close frame with the
  code 1001 and waits for the reply of the peer, with the same time
  limit.
- A write timeout (30 s by default) stops a send to a peer that reads
  nothing. A read timeout (30 s by default) closes a connection when a
  frame stops in the middle. An optional idle timeout closes a connection
  when no frame comes in time.
- All the cases of the [Autobahn](https://github.com/crossbario/autobahn-testsuite)
  test suite give OK, or INFORMATIONAL for three cases, except the
  compression cases: caravan does not accept `permessage-deflate`.

[examples/websocket](examples/websocket/app.ml) has a full example.

## Client

The `caravan.client` library is an HTTP/1.1 client. It uses the parser
and the writer of the server:

```ocaml
Weft.run (module Weft_os) @@ fun () ->
Caravan_client.run @@ fun client ->
Caravan_client.with_response client
  (Caravan.Request.make ~authority:"example.com" Caravan.Method.GET "/")
  (fun resp -> Caravan.Body.to_string (Caravan_client.body resp))
```

- The client keeps idle connections for each scheme, host and port, and gives a
  connection back when the response body ends.
- The request body goes while the caller reads the response. The writer
  of the body belongs to the client, not to the caller. Thus a server
  handler can forward a request and stream the two bodies at the same
  time.
- `Dns.resolve` of weft does the DNS lookup on the thread pool of the
  world, thus a lookup does not stop the fibers of its domain.
- Time limits for the connect, the response head, each read and each
  write. One retry of an idempotent request without a body when the
  server closed an idle connection.
- `https` URLs need the `~tls` of `Caravan_client.run`. The function
  `Caravan_tls.https` of the optional `caravan.tls` gives it (see the
  example after this list).
- The client has no proxy support, no redirects, no cookies and no
  content codings.

HTTPS, with the certificate authorities of the system from the
[ca-certs](https://github.com/mirage/ca-certs) package:

```ocaml
let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let () =
  Mirage_crypto_rng_unix.use_default () ;
  let config =
    ok (Tls.Config.client ~authenticator:(ok (Ca_certs.authenticator ())) ())
  in
  Weft.run (module Weft_os) @@ fun () ->
  Caravan_client.run ~tls:(Caravan_tls.https config) @@ fun client ->
  Caravan_client.with_response client
    (Caravan.Request.make ~scheme:"https" ~authority:"example.com"
       Caravan.Method.GET "/")
    (fun resp -> print_string (Caravan.Body.to_string (Caravan_client.body resp)))
```

- For each connection, `Caravan_tls.https` takes the name or the
  address of the URL. The client sends a name in SNI, and the
  authenticator checks the name or the address of the certificate. Thus
  the configuration must have no peer name, no IP address and no cached
  session.
- One deadline, `connect_timeout`, covers the DNS lookup, the TCP
  connect and the TLS handshake. A failed handshake raises
  `Caravan_client.Error (Tls _)`.
- An `http` and an `https` connection to the same host and port are
  different connections.
- A response body that ends with the connection (no `Content-Length`,
  not chunked) needs a `close_notify` of the server. Else the client
  gives `Error Truncated`.

[examples/client](examples/client/app.ml) has a gateway that forwards
requests with the client, and a program that fetches a URL
([fetch.ml](examples/client/fetch.ml)).

## Background jobs

`Caravan_jobs` runs work after the response, in fibers of the process,
for example to send a mail. Give the queue to `Server.run` as a
service: it starts before the server, and it stops after the server.

```ocaml
(* Your mail code: SMTP, or the HTTP API of a mail service. *)
let send_mail ~to_ ~text = ignore (to_, text)

let welcome : string Caravan_jobs.job =
  Caravan_jobs.job ~name:"welcome"
    ~on_failure:(fun address _ -> prerr_endline ("no welcome for " ^ address))
    (fun address -> send_mail ~to_:address ~text:"Welcome!")

let jobs = Caravan_jobs.create ~workers:8 ()

let sign_up _req =
  match Caravan_jobs.enqueue jobs welcome "ann@example.com" with
  | Ok () ->
      Caravan.Response.string "Welcome."
  | Error (`Full | `Stopped) ->
      Caravan.Response.string ~status:Caravan.Status.service_unavailable
        "Try again later."

let () =
  Caravan.Server.run
    ~services:[Caravan_jobs.service jobs]
    (module Weft_os) sign_up
```

- A job that raises gets a retry after a backoff: 5 attempts, a wait
  of 1 s that doubles each time, at most 5 min, less a jitter of up to
  15 %. Give `~retry` for a different policy, or `Caravan_jobs.no_retry`.
- `enqueue ~delay` runs a job later. `enqueue` never waits: when
  `capacity` jobs wait (10 000 by default), it gives `` `Full ``.
- The queue is not durable. At a stop, the server and the queue share
  one grace (`shutdown_grace`). The handlers can enqueue during the
  grace, and the workers run the jobs that wait until its end. Then
  the queue cancels the jobs that run. Each job that did not succeed
  goes to `on_failure` (with its argument) and to the `on_error` of
  the queue (the standard error by default).
- A job can run more than one time (a retry after a failure in its
  middle). Make each job safe to run again.
- `Caravan_jobs.stats` gives the counts, for example for a health check.
  In a test, `Caravan_jobs.drain` waits for all jobs, also the delayed
  ones, in virtual time.

[examples/jobs](examples/jobs/app.ml) has a full example.

### Durable jobs

`Caravan_jobs.Durable` keeps the jobs in a store of your application,
for example a table of its database. A job survives a restart, and
many processes can share one store. You write the store: seven
functions, with the SQL for PostgreSQL in the documentation of
`Durable.store`. `Durable.memory_store` is for tests.

```ocaml
let send_code =
  Caravan_jobs.Durable.job ~name:"send_code"
    ~encode:(fun (address, code) -> address ^ "\n" ^ code)
    ~decode:(fun s ->
      match String.split_on_char '\n' s with
      | [address; code] -> (address, code)
      | _ -> failwith "send_code: bad payload" )
    (fun _info (address, code) -> send_mail ~to_:address ~text:code)

let jobs = Caravan_jobs.Durable.create ~jobs:[Job send_code] store

(* In a handler: the store keeps the job, and a worker runs it. *)
let _id = Caravan_jobs.Durable.enqueue jobs send_code (address, code)
```

- A worker holds a lease on its job, and extends it while the job runs.
  After a crash, another process runs the job when its lease ends (60 s
  by default). Thus a job runs at least one time: use `info.id` as an
  idempotency key.
- A queue claims only the names of its job types. Thus two queues can
  share one store, and during a deploy an old process does not claim a
  new type. Give a new name to a job type whose payload format changes.
- At a stop, the queue gives the running jobs back to the store. They
  use no attempt.
- `Durable.row` gives the row of a job, for an insert in the
  transaction of your application. Call `Durable.wake` after the
  commit.
- The store keeps each job: `Durable.enqueue` has no limit. Put a rate
  limit on the routes that make jobs.

### Unique and recurring jobs

`Durable.enqueue_unique ~key` adds a job only if the store has no job of
the same name and key, for example one welcome mail for each user. The
key is held from the push to the end of the job: it prevents a second
job, but it is not a debounce.

`Caravan_jobs.Recurring` runs jobs at the ticks of a schedule:
`Schedule.every` (multiples of a span since 1970) or `Schedule.cron`
(5 fields, names, macros such as `@daily`, UTC).

```ocaml
let nightly =
  Caravan_jobs.Recurring.create
    [ Caravan_jobs.Recurring.durable ~name:"report"
        (Caravan_jobs.Schedule.cron "0 3 * * *")
        jobs report (fun tick -> tick) ]

let () =
  Caravan.Server.run
    ~services:
      [Caravan_jobs.Durable.service jobs; Caravan_jobs.Recurring.service nightly]
    (module Weft_os) handler
```

- Each process runs each entry. With `Recurring.durable`, the store
  keeps the last tick of each entry: one job for each tick over all
  processes, also from a process whose clock is late. Give each durable
  entry a name that is unique in the store, for example
  `"billing.report"`.
- A tick that is late (a slow action, a step of the clock) runs one time,
  then the entry waits for the next tick: there is no catch-up.
- Put `Recurring` after its queues in `~services`, and give a new name
  to an entry whose schedule changes.

[examples/durable_jobs](examples/durable_jobs/app.ml) has a store, unique
jobs and recurring jobs.

## Testing

The `caravan.testing` library runs a handler through the real server
and client, in the simulator of weft. The time is virtual, thus a test
of a 30 s timeout takes no real time, and a run is the same for a seed:

```ocaml
let hi _req = Caravan.Response.string "hi"

let () =
  Caravan_testing.run hi (fun t ->
      let reply = Caravan_testing.get t "/" in
      assert (reply.body = "hi") )

(* One run for each seed, on 4 virtual domains. A failure tells its seed:
   replay it with ~replay. *)
let () =
  Caravan_testing.race ~seeds:20 hi (fun t ->
      assert ((Caravan_testing.get t "/").body = "hi") )

(* Manual time: only advance moves the time. The request goes in the
   background while the test advances the time. *)
let () =
  let slow _req =
    Weft.Time.sleep Mono (Weft.Span.s 20) ;
    Caravan.Response.string "late"
  in
  Caravan_testing.run ~manual:true slow (fun t ->
      let p =
        Caravan_testing.background t (fun () -> Caravan_testing.get t "/")
      in
      Caravan_testing.advance t (Weft.Span.s 20) ;
      assert ((Weft.Promise.await p).body = "late") )
```

- An error of the handler fails the run, unless the test takes it with
  `take_errors`.
- In auto mode, a guard (10 min of virtual time by default) stops a test
  that waits for nothing. In manual mode, such a test raises
  `Weft_sim.Deadlock`.
- `background` runs a request (or other code) while the test continues.
  At the end of the test, the run cancels it, as a client that leaves. A
  failure of a background fiber must be awaited.
- `run ~stop_on_signals:true` stops the server at SIGINT and SIGTERM, as
  `Server.run` does, and `signal` sends a signal. Thus a test can check a
  graceful stop.
- `race` runs the test for each seed and stops at the first failure.
  A replay is exact if the handler and the test keep no state from one
  run to the next.
- `Caravan_testing.sim` gives the simulator, for latency, network faults
  and names (`Weft_sim.set_dns`).

[examples/testing](examples/testing/test.ml) shows each function of
`caravan.testing`, and each example has a `test.ml`.

## Requirements

- OCaml >= 5.5.0
- [weft](https://github.com/ocaml-caravanserai/weft) (not on opam),
  with the library `weft.flow`, and the package `weft-tls` for
  `caravan.tls`
- [http-date](https://ocaml.org/p/http-date/latest) >= 0.2
- dune >= 3.20
- For tests: windtrap and ppx_windtrap >= 0.2.0, from the fork
  [bikallem/windtrap](https://github.com/bikallem/windtrap).

The Nix flake gives all of these, at the commits that flake.nix and
flake.lock pin.

## Development

The repository includes a Nix flake. Its dev shell provides OCaml 5.5,
dune, weft, http-date, windtrap, ppx_windtrap, ocaml-lsp, utop,
ocamlformat, odoc, and h2spec, curl and nghttp2 for the HTTP/2 tests.
`nix build` builds the caravan package (all the libraries of the table
in [Libraries](#libraries), with no tests).

With [direnv](https://direnv.net) and
[nix-direnv](https://github.com/nix-community/nix-direnv) installed,
run this once and the environment then loads whenever you enter the
directory:

```sh
direnv allow
```

Without direnv, enter the shell manually:

```sh
nix develop
```

Then:

```sh
make build      # dune build
make test       # all test suites (dune test --force)
make fmt        # format with ocamlformat (dune fmt)
make mutate     # mutation testing of the suites of test/, in parallel
make mutate MUTATE=websocket/   # only the mutants of the files under a prefix
make coverage   # test coverage report of the suites of test/
make autobahn   # the Autobahn WebSocket test suite, with podman or docker
make h2spec     # the h2spec suite of HTTP/2, strict (Linux)
make interop    # curl, nghttp, h2load and h2spec over TLS against HTTP/2 (Linux)
```

The suites of test/ run in the simulator of weft, thus they use no real
time and no real network. The suite of test/linux runs on Linux in the
OS world of weft, with real sockets, signals and child processes.

The CI (.github/workflows/ci.yml) runs `dune build`, `dune build @fmt`
and `dune runtest` in the dev shell on Linux and macOS, `make h2spec`
and `make interop` on Linux, and `nix build` on Linux, for each push to
main and each pull request.

The programs of the examples use `Weft_os`, but we run them only on
Linux. Their tests run in the simulator, as the suites of test/ do. Try
the example server, and stop it with Ctrl-C:

```sh
dune exec examples/hello/main.exe
curl -i 127.0.0.1:8080/hello
```

For WebSocket, run `dune exec examples/websocket/main.exe` and open
<http://127.0.0.1:8080/> in two tabs of a browser. While the hello example runs,
`dune exec examples/client/fetch.exe -- http://127.0.0.1:8080/hello`
fetches a page with the client.

## License

[MPL-2.0](LICENSE)
