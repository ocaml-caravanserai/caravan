
.PHONY: build fmt test mutate coverage autobahn h2spec interop

build:
	dune build

fmt:
	dune fmt

test:
	dune test --force

# Mutation testing. dune runs each suite of test/ with its mutants, but not
# the suite of test/linux. That suite kills no mutant that the other suites do
# not kill, and it needs a new process for each mutant. windtrap tests the
# mutants of a suite in parallel children. It keeps the verdicts of each suite
# in a file of its own in _build/_mutants. The report at the end merges these
# files and lists the mutants that no suite kills.
# The target fails if the run of a suite without mutants fails, or if a
# mutant survives. Options:
# - MUTATE=websocket/ tests only the mutants of the files under this prefix.
#   A run without it first removes all verdicts.
# - MUTATE_JOBS is the number of children of each suite at the same time,
#   auto for one for each CPU.
# - MUTATE_STATEFUL_COUNT is the number of programs of the stateful test
#   (CARAVAN_STATEFUL_COUNT), 300 in a normal run. With less than 200, the
#   test can fail its own rule that each cover label must occur.
# - The race tests run 10 seeds for each mutant (CARAVAN_RACE_SEEDS), 100 in
#   a normal run.
MUTATE ?=
MUTATE_JOBS ?= auto
MUTATE_STATEFUL_COUNT ?= 200

mutate:
	dune build --instrument-with ppx_windtrap.mutate @@test/all
	$(if $(MUTATE),,rm -rf _build/_mutants)
	WINDTRAP_MUTATE=$(or $(MUTATE),1) WINDTRAP_MUTATE_JOBS=$(MUTATE_JOBS) \
	  CARAVAN_STATEFUL_COUNT=$(MUTATE_STATEFUL_COUNT) CARAVAN_RACE_SEEDS=10 \
	  dune build @@test/runtest --force --instrument-with ppx_windtrap.mutate
	dune exec --instrument-with ppx_windtrap.mutate windtrap -- mutants

coverage:
	find _build -name '*.coverage' -delete
	dune runtest test --force --instrument-with ppx_windtrap.coverage
	dune exec windtrap -- coverage

# The Autobahn WebSocket test suite, in a container: podman by default,
# CONTAINER=docker for docker. All cases except compression (12.x, 13.x).
# The configuration is in $(AUTOBAHN), and the reports go to
# $(AUTOBAHN)/reports. The target fails if a case fails.
CONTAINER ?= podman
AUTOBAHN := test/linux/autobahn

autobahn:
	dune build test/linux/autobahn_server.exe
	mkdir -p $(AUTOBAHN)/reports
	: > $(AUTOBAHN)/reports/server.log
	./_build/default/test/linux/autobahn_server.exe > $(AUTOBAHN)/reports/server.log 2>&1 & server=$$!; \
	trap 'kill $$server 2>/dev/null' EXIT INT TERM; \
	for i in $$(seq 50); do grep -q '^listening' $(AUTOBAHN)/reports/server.log && break; sleep 0.1; done; \
	grep -q '^listening' $(AUTOBAHN)/reports/server.log || { echo "The server did not start."; exit 1; }; \
	$(CONTAINER) run --rm --network host \
	  -v "$(CURDIR)/$(AUTOBAHN):/config:Z" -v "$(CURDIR)/$(AUTOBAHN)/reports:/reports:Z" \
	  docker.io/crossbario/autobahn-testsuite \
	  wstest -m fuzzingclient -s /config/fuzzingclient.json > $(AUTOBAHN)/reports/wstest.log 2>&1; \
	status=$$?; test $$status -eq 0
	jq -r '.caravan | to_entries | group_by(.value.behavior) | .[] | "\(.[0].value.behavior): \(length)"' \
	  $(AUTOBAHN)/reports/servers/index.json
	jq -e '[.caravan[] | select(.behavior == "FAILED" or .behaviorClose == "FAILED")] | length == 0' \
	  $(AUTOBAHN)/reports/servers/index.json > /dev/null

# The h2spec conformance suite of HTTP/2 (h2spec of the dev shell) in strict
# mode (also the SHOULD cases), against HTTP/2 with prior knowledge (h2c) on
# 127.0.0.1:$(H2SPEC_PORT). The server reads each request body and answers 200
# with a body. Its output goes to $(H2SPEC_LOG). The target fails if a case
# fails, and then shows that output. The dune root is the directory of this
# Makefile, also in a worktree.
H2SPEC_PORT ?= 18443
H2SPEC_LOG := _build/h2spec-server.log

h2spec:
	dune build --root . test/linux/h2spec_server.exe
	./_build/default/test/linux/h2spec_server.exe $(H2SPEC_PORT) > $(H2SPEC_LOG) 2>&1 & server=$$!; \
	trap 'kill $$server 2>/dev/null' EXIT INT TERM; \
	for i in $$(seq 50); do grep -q '^listening' $(H2SPEC_LOG) && break; sleep 0.1; done; \
	grep -q '^listening' $(H2SPEC_LOG) || { echo "The server did not start."; exit 1; }; \
	h2spec -S -h 127.0.0.1 -p $(H2SPEC_PORT) -o 3 || { cat $(H2SPEC_LOG); exit 1; }

# The interop tests of HTTP/2: curl, nghttp, h2load and h2spec (of the dev
# shell) against Caravan_http2.auto with Server.http1, without TLS on
# 127.0.0.1:$(INTEROP_PORT) and with TLS on the next port. The script
# test/linux/interop.sh fails at the first check that fails, and shows the logs
# of the servers. The dune root is the directory of this Makefile.
INTEROP_PORT ?= 18450

interop:
	dune build --root . test/linux/interop_server.exe
	test/linux/interop.sh ./_build/default/test/linux/interop_server.exe _build $(INTEROP_PORT)
