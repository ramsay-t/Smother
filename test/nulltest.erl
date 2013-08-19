-module(nulltest).
-export([do_nothing/0]).

do_nothing() ->
    io:format("I'm not doing this...~n").

internal([]) ->
    io:format("Wait, waht?").
