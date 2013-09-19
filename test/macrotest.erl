-module(macrotest).
-export([dv/2]).

-include("macrotest.hrl").

dv(A,B) ->
    if ?TEST_MAC ->
	    B / 1;
       true ->
	    B / A
    end.
