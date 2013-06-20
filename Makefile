WRANGLER_ROOT=../wrangler
LIBS=-pa $(WRANGLER_ROOT)/ebin
SOURCES=$(wildcard src/*.erl)
SED=gsed

default: all
	$(SED) -i.tmp 's|-include(".*wrangler/include/wrangler.hrl")\.|-include("$(WRANGLER_ROOT)/include/wrangler.hrl")\.|' include/install.hrl
	rm include/*.tmp

ebin/%.beam: src/%.erl
	erlc -o ebin $(LIBS) $<

clean:
	rm ebin/*

all: $(patsubst src/%.erl,ebin/%.beam,$(SOURCES))


