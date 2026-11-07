#!/usr/bin/env bash
# The interop tests of make interop: real HTTP/2 clients (curl, nghttp,
# h2load, h2spec) against interop_server.exe (Caravan_http2.auto with
# Server.http1). One server has no TLS, on 127.0.0.1:PORT. The other has TLS,
# on 127.0.0.1:PORT+1, with a test CA in DIR/ca.pem.
#
# Usage: interop.sh SERVER DIR PORT
#
# The script fails at the first check that fails, and shows the log of the
# servers.

set -euo pipefail

server=$1
dir=$2
port=$3
tls_port=$((port + 1))
ca=$dir/ca.pem
plain_log=$dir/interop-plain.log
tls_log=$dir/interop-tls.log
pids=()

stop() {
  for pid in "${pids[@]}"; do kill "$pid" 2> /dev/null || true; done
}
trap stop EXIT

fail() {
  echo "FAIL: $1"
  echo "--- server without TLS"
  cat "$plain_log"
  echo "--- server with TLS"
  cat "$tls_log"
  exit 1
}

# [start LOG ARGS...] starts the server with ARGS, and waits for its line
# "listening".
start() {
  local log=$1
  shift
  "$server" "$@" > "$log" 2>&1 &
  pids+=($!)
  for _ in $(seq 50); do
    grep -q '^listening' "$log" && return 0
    sleep 0.1
  done
  fail "the server $* did not start"
}

start "$plain_log" "$port"
start "$tls_log" "$tls_port" "$ca"

plain=http://127.0.0.1:$port/
secure=https://localhost:$tls_port/

# [expect NAME WANT GOT] compares the output GOT of a client with WANT.
expect() {
  if [ "$2" != "$3" ]; then
    fail "$1: expected '$2', got '$3'"
  fi
  echo "ok: $1"
}

# [curl_check NAME VERSION ARGS...] runs curl with ARGS, and compares the HTTP
# version and the body with VERSION.
curl_check() {
  local name=$1 version=$2
  shift 2
  local out body
  out=$(curl -sS --fail -w ' %{http_version}' "$@") || fail "$name: curl"
  case $version in
    2) body="hello, 0 bytes, HTTP/2" ;;
    1.1) body="hello, 0 bytes, HTTP/1.1" ;;
    *) fail "$name: unknown version $version" ;;
  esac
  expect "$name" "$body
 $version" "$out"
}

curl_check "curl --http2-prior-knowledge" 2 --http2-prior-knowledge "$plain"
curl_check "curl HTTP/1.1 without TLS (the preface sniff)" 1.1 --http1.1 "$plain"
curl_check "curl --http2 over TLS (ALPN h2)" 2 --http2 --cacert "$ca" "$secure"
curl_check "curl --http1.1 over TLS (ALPN http/1.1)" 1.1 --http1.1 --cacert "$ca" "$secure"
curl_check "curl over TLS without ALPN" 1.1 --no-alpn --cacert "$ca" "$secure"

# A POST body over HTTP/2.
out=$(curl -sS --fail --http2 --cacert "$ca" --data-binary @"$ca" "$secure") \
  || fail "curl POST: curl"
expect "curl POST over TLS" "hello, $(wc -c < "$ca" | tr -d ' ') bytes, HTTP/2" "$out"

# nghttp checks each frame. It gives a status other than 0 for an error.
nghttp -nv "$plain" > "$dir/nghttp-plain.log" 2>&1 \
  || { cat "$dir/nghttp-plain.log"; fail "nghttp -nv without TLS"; }
echo "ok: nghttp -nv without TLS"
nghttp -nv "$secure" > "$dir/nghttp-tls.log" 2>&1 \
  || { cat "$dir/nghttp-tls.log"; fail "nghttp -nv over TLS"; }
grep -qx 'The negotiated protocol: h2' "$dir/nghttp-tls.log" \
  || fail "nghttp over TLS: no h2"
echo "ok: nghttp -nv over TLS"

# [h2load_check NAME URL] runs 1000 requests on 10 connections with 10 streams
# each, and expects 1000 successes and no error.
h2load_check() {
  local out
  out=$(h2load -n 1000 -c 10 -m 10 "$2" 2>&1) || { echo "$out"; fail "$1"; }
  echo "$out" | grep -q '1000 succeeded, 0 failed, 0 errored, 0 timeout' \
    || { echo "$out"; fail "$1"; }
  echo "$out" | grep -q 'status codes: 1000 2xx' || { echo "$out"; fail "$1"; }
  echo "ok: $1"
}

h2load_check "h2load without TLS" "$plain"
h2load_check "h2load over TLS" "$secure"

# h2spec over TLS (-t), with no check of the certificate (-k), in strict mode.
# Its output goes to a log, which the script shows only on a failure.
h2spec -S -t -k -h 127.0.0.1 -p "$tls_port" -o 3 > "$dir/h2spec-tls.log" 2>&1 \
  || { cat "$dir/h2spec-tls.log"; fail "h2spec over TLS"; }
echo "ok: h2spec over TLS ($(tail -1 "$dir/h2spec-tls.log"))"
