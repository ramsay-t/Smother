-module(ablisttest).
-export([dv/1]).

dv(Arg) ->
    case Arg of
	[0,5] ->
	    5;
	[A,B] ->
	    B / A
    end.
