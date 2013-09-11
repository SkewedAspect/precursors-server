REPO        ?= precursors_server

.PHONY: rel deps

all: compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

clean: testclean
	./rebar clean

distclean: clean relclean
	./rebar delete-deps


TEST_LOG_FILE := eunit.log
testclean:
	@rm -f $(TEST_LOG_FILE)

eunit: clean deps compile
	./rebar eunit skip_deps=true

test: deps compile testclean
	./rebar eunit skip_deps=true

script: compile
	./rebar escriptize skip_deps=true

rel: compile
	./rebar generate skip_deps=true

relclean:
	rm -rf rel/$(REPO)

devrel: rel
	rm -rf rel/$(REPO)/lib/$(REPO)*; ln -sf $(abspath .) rel/$(REPO)/lib/$(REPO)-1;
