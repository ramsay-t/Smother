WRANGLER_ROOT=deps/wrangler
LIBS=-pa $(WRANGLER_ROOT)/ebin
SOURCES=$(wildcard src/*.erl)
OBJS=$(patsubst src/%.erl,ebin/%.beam, $(SOURCES))
SED=sed
VPATH=src

all: header ebin $(WRANGLER_ROOT) $(OBJS)

header:
	@$(SED) -i.tmp 's|-include(".*wrangler/include/wrangler.hrl")\.|-include("$(WRANGLER_ROOT)/include/wrangler.hrl")\.|' include/install.hrl
	@rm include/*.tmp

ebin/%.beam: %.erl
	erlc -o ebin $(LIBS) $<

ebin:
	mkdir -p ebin

$(WRANGLER_ROOT):
	mkdir -p deps && cd deps && git clone https://github.com/RefactoringTools/wrangler.git

clean:
	rm ebin/*



