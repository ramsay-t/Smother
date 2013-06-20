-module(aoverb_tests).

-include_lib("eunit/include/eunit.hrl").
-export([statement_coverage/0]).
zero_test() ->
    ?assert(aoverb:dv(0,5) =:= 5.0).
one_test() ->
    ?assert(aoverb:dv(1,5) =:= 5.0).
two_test() ->
    ?assert(aoverb:dv(2,5) =:= 2.5).
two_twos_test() ->
    ?assert(aoverb:dv(2,2) =:= 1.0).
five_test() ->
    ?assert(aoverb:dv(5,5) =:= 1.0).

statement_coverage() ->
    cover:compile(aoverb),
    test(),
    cover:analyse_to_file(aoverb,"aoverb_coverage.html",[html]).
