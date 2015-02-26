-module(include_test).
-export([testfun/0,test/0]).

-include("include_test1.hrl").
-include_lib("test/include_test2.hrl").

testfun() ->
    ifun1(),
    ifun2(),
    ?IMAC1(),
    ?IMAC2(),
    ok.

test() ->
    smother:compile(?FILE,[preprocess,{i,"test"}]),
    smother:analyse_to_file(?MODULE).
