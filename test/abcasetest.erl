-module(abcasetest).
-export([dv/2]).

dv(A,B) ->
    case {A,B} of
	{0,5} ->
	    B / 1;
	_ ->
	    B / A
    end.
