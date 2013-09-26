# Variables
REPO ?= precursors_server
PRIVDIR ?= priv
RELDIR ?= $(REPO)
DEVRELDIR ?= dev


# The default target
all: compile


# Targets that don't correspond to the name of a file
.PHONY: all help deps compile script cert rel
.PHONY: clean-deps clean-cert clean-rel clean-test clean distclean


.DEFAULT:
	$(error "Unrecognized target '$@'! Try 'make help'.")


# Help!
help:
	@echo "Targets:"
	@echo "    all            same as 'compile', below"
	@echo "    help           display this help message"
	@echo
	@echo "    deps           fetch all dependencies"
	@echo "    compile        compile the project"
	@echo "    script         run 'escriptize' to generate an executable script for the project"
	@echo "    devcert        generate an SSL certificate for development"
	@echo "    rel            generate a production release"
	@echo "    devrel         generate a development release, and a ./devrel runner script"
	@echo
	@echo "    clean          clean up after 'compile' and 'test'"
	@echo "    clean-deps     clean up after 'deps'"
	@echo "    clean-devcert  clean up after 'devcert'"
	@echo "    clean-rel      clean up after 'rel'"
	@echo "    clean-devrel   clean up after 'devrel'"
	@echo "    clean-test     clean up after 'test'"
	@echo "    distclean      clean up everything possible"


# Building
deps:
	./rebar get-deps

compile: deps
	./rebar compile

script: compile
	./rebar escriptize skip_deps=true


# SSL certificates
SSL_CERT ?= $(PRIVDIR)/precursors.crt
SSL_CSR ?= $(PRIVDIR)/precursors.csr
SSL_KEY ?= $(PRIVDIR)/key
SSL_PUBKEY ?= $(SSL_KEY).pub
SSL_CSR_SUBJECT ?= "/C=US/ST=Texas/L=Lubbock/O=Skewed Aspect/CN=$(shell hostname).$(shell dnsdomainname)"

$(SSL_KEY):
	@echo "Generating RSA key..."
	openssl genpkey -algorithm rsa -pass pass: -out $@

$(SSL_CSR): $(SSL_KEY)
	@echo "Generating Certificate Signing Request..."
	openssl req -new -subj $(SSL_CSR_SUBJECT) -key $< -out $@

$(SSL_CERT): $(SSL_CSR) $(SSL_KEY)
	@echo "Generating self-signed for a year..."
	openssl x509 -req -days 365 -in $< -signkey $(SSL_KEY) -out $@

devcert: $(SSL_CERT)


# Building releases
rel/$(RELDIR) rel/$(DEVRELDIR): compile
	./rebar generate target_dir=$(@F) skip_deps=true

rel: rel/$(RELDIR)

devrel: rel/$(DEVRELDIR)
	@echo "#!/bin/bash" > devrel
	@echo 'exec $(abspath rel/$(DEVRELDIR)/bin/$(REPO)) "$$@"' >> devrel
	chmod +x devrel
	-rm -rf rel/$(DEVRELDIR)/lib/$(REPO)*
	ln -sf $(abspath .) rel/$(DEVRELDIR)/lib/$(REPO)-1


# Testing
#FIXME: Why are these two identical, with slightly different deps?
eunit: clean deps compile
	./rebar eunit skip_deps=true

test: deps compile testclean
	./rebar eunit skip_deps=true


# Cleanup
clean-deps: clean
	./rebar delete-deps

clean-devcert:
	-rm -f $(SSL_CERT) $(SSL_CSR) $(SSL_KEY) $(SSL_PUBKEY)

clean-rel:
	-rm -rf rel/$(RELDIR)

clean-devrel:
	-rm -rf devrel rel/$(DEVRELDIR)

TEST_LOG_FILE := eunit.log
clean-test:
	-rm -f $(TEST_LOG_FILE)

clean: clean-test
	./rebar clean

distclean: clean clean-deps clean-devcert clean-rel clean-devrel
