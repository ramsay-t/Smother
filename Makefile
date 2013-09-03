WRANGLER_ROOT=deps/wrangler
LIBS=-pa $(WRANGLER_ROOT)/ebin
SOURCES=$(wildcard src/*.erl)
OBJS=$(patsubst src/%.erl,ebin/%.beam, $(SOURCES))
SED=sed
VPATH=src

all: header compile $(WRANGLER_ROOT) $(OBJS)

header:
	@$(SED) -i.tmp 's|-include(".*wrangler/include/wrangler.hrl")\.|-include("$(WRANGLER_ROOT)/include/wrangler.hrl")\.|' include/install.hrl
	@rm include/*.tmp

compile: deps
	./rebar compile

deps:
	./rebar get-deps

doc:
	./rebar skip_deps=true doc

xref:
	./rebar skip_deps=true xref

clean:
	./rebar clean

test:
	./rebar skip_deps=true eunit

.PHONY: test


