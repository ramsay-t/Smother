-module(abiftest).
-export([dv/2]).

dv(A,B) ->
    if (A == 0) and (B > 4) ->
	    B;
       true ->
	    B / A
    end.
