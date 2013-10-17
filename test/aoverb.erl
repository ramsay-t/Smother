-module(aoverb).
-export([dv/2]).

dv(A,B) when (A == 0) and (B > 4) ->
    B / 1;
dv(A,B) ->
    B / A.
