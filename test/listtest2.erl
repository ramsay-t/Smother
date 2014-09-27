-module(listtest2).
-export([f/1,g/1]).

f([X | []]) ->
    X;
f(_) ->
    wibble.

g([X]) ->
    X;
g(_) ->
    wibble.

