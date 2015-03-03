-module(spectest).

-export([testfun/0]).

-include_lib("eunit/include/eunit.hrl").

-type testtype() :: {integer(), integer()}.

-spec testfun() -> testtype().
testfun() ->
    {1,2}.

compile_test() ->
    smother:compile("../" ++ ?FILE,[preprocess]),
    smother:analyse_to_file(?MODULE).
