all: compile $(WRANGLER_ROOT) $(OBJS)

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

escriptize: compile
	./rebar escriptize

.PHONY: test


