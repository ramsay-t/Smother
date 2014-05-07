-module(ablisttest).
-export([dv/2]).

dv(A,B) ->
    case [A,B] of
	[0,5] ->
	    B;
	_ ->
	    B / A
    end.
