-module(abcasetest).

-export([dv/2]).

dv(A,B) ->
    smother_server:log("test/abcasetest.erl", {{4, 1}, {10, 7}}, [A,B]),
    begin
        EVal = {A,B}, 
        VarList = [EVal], 
        smother_server:log("test/abcasetest.erl", {{5, 5}, {10, 7}}, VarList), 
        case EVal of
            {0,5} ->
                         B / 1;
            _ ->
                         B / A
        end
    end.
