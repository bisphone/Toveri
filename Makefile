.PHONY: all compile shell test clean distclean

APP = toveri
REBAR = $(shell which rebar3)

test = $(CURDIR)/test
build = $(CURDIR)/_build
lib = $(build)/default/lib

all: compile

compile:
	$(REBAR) compile

shell: all
	erl -pa $(lib)/*/ebin

test:
	$(REBAR) ct

clean:
	rm -rf $(lib)/$(APP)

distclean:
	rm -rf $(build)
