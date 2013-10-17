-module(abiftest).

-export([dv/2]).

dv(A,B) ->
    smother_server:log("test/abiftest.erl", {{4, 1}, {9, 7}}, [A,B]),
    begin
        smother_server:log("test/abiftest.erl", {{5, 5}, {9, 7}}, [A, B]), 
        if (A == 0) and (B > 4) -> B / 1;
           true -> B / A
        end
    end.
