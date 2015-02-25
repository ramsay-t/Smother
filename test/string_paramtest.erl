-module(string_paramtest).

-export([test/1,tt/0,vol_test/1,comp_test/1]).

-define(MIN,0).
-define(MAX,100).

test(P) ->
    io:format("~w~n",[P]).

vol_test(P) when P > ?MIN,
		 P < ?MAX ->
    io:format("~p is NOT ok!!!~n",[P]),
    throw("Oh No!!!!!");
vol_test(P) ->
    io:format("~p is ok.~n",[P]).

tt() ->
    test([101]),
    test([-1]),
    test([278]),
    test([97]),
    test(10.500000000000000000000000000001).

comp_test("Hello, " ++ World) ->
    io:format("Hello, ~s~n",[World]);
comp_test(String) ->
    io:format("Thats not very polite!~n").

