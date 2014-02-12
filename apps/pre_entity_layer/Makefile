# Variables
REBAR ?= "../../rebar"
REPO ?= pre_entity_layer
PRIVDIR ?= priv
RELDIR ?= $(REPO)
DEPSDIR ?= ../../deps


# The default target
all: deps compile


# Targets that don't correspond to the name of a file
.PHONY: all help compile deps update-deps
.PHONY: clean-deps clean-test clean distclean


.DEFAULT:
	$(error "Unrecognized target '$@'! Try 'make help'.")


# Help!
help:
	@echo "Targets:"
	@echo "    all            comple"
	@echo "    help           display this help message"
	@echo
	@echo "    deps           fetch all dependencies"
	@echo "    compile        compile the project"
	@echo "    test           run unit tests"
	@echo
	@echo "    clean          clean up after 'compile' and 'test'"
	@echo "    clean-deps     clean up after 'deps'"
	@echo "    clean-test     clean up after 'test'"
	@echo "    distclean      clean up everything possible"


# Building
deps:
	$(REBAR) get-deps deps_dir=$(DEPSDIR)

update-deps:
	$(REBAR) update-deps deps_dir=$(DEPSDIR)

compile: deps
	$(REBAR) compile deps_dir=$(DEPSDIR)

# Testing
eunit test: clean-test compile
	$(REBAR) eunit skip_deps=true deps_dir=$(DEPSDIR)


# Cleanup
clean-deps: clean
	$(REBAR) delete-deps deps_dir=$(DEPSDIR)
	-rm -rf deps

TEST_LOG_FILE := eunit.log
clean-test:
	-rm -f $(TEST_LOG_FILE)

clean: clean-test
	$(REBAR) clean deps_dir=$(DEPSDIR)

distclean: clean clean-deps
