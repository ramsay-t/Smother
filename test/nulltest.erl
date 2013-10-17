-module(nulltest).
-export([do_nothing/0,do_little/1]).

do_nothing() ->
    io:format("I'm not doing this...~n").

do_little(_) ->
    io:format("I don't care what you told me...~n").

internal([]) ->
    io:format("Wait, what?~n").
