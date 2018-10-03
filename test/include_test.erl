-module(include_test).
-export([testfun/0]).

-include_lib("eunit/include/eunit.hrl").

-include("include_test1.hrl").
-include_lib("include_test2.hrl").

testfun() ->
    ifun1(),
    ifun2(),
    {?IMAC1(),?IMAC2()}.

compile_test() ->
    smother:compile(?FILE, [preprocess]),
    smother:analyse_to_file(?MODULE).

