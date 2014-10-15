-module(abtupletest).
-export([dv/1]).

-include_lib("eunit/include/eunit.hrl").

dv({0,5}) ->
    5;
dv({A,B}) ->
    B / A.

%% full_test() ->
%%     {_Z,NZ,_P} = abtest:do_dv_tests(?MODULE,[{0,5},{5,5},{5,0},{0,2}]),
%%     ?assert(abtest:contains(NZ,[
%% 			 {matched,{{7,4},{7,6}},[]},
%% 			 {non_matched,{{7,4},{7,6}},[]},
%% 			 {matched,{{7,4},{7,4}},[{non_matched,{{7,4},{7,6}}}]},
%% 			 {non_matched,{{7,4},{7,4}},[{non_matched,{{7,4},{7,6}}}]},
%% 			 {matched,{{7,6},{7,6}},[{non_matched,{{7,4},{7,6}}}]},
%% 			 {non_matched,{{7,6},{7,6}},[{non_matched,{{7,4},{7,6}}}]},
%% 			 {matched,{{7,4},{7,4}},[{{7,4},{7,6}}]},
%% 			 {non_matched,{{7,4},{7,4}},[{{7,4},{7,6}}]},
%% 			 {matched,{{7,6},{7,6}},[{{7,4},{7,6}}]},
%% 			 {non_matched,{{7,6},{7,6}},[{{7,4},{7,6}}]},
%% 			 {matched,{{9,4},{9,6}},[]}
%% 			])).

%% cond_dec_test() ->
%%     {Z,NZ,_P} = abtest:do_dv_tests(?MODULE,[{0,5},{5,5},{5,0}]),
%%     ?assert(abtest:contains(NZ,[
%% 			 {matched,{{7,4},{7,6}},[]},
%% 			 {non_matched,{{7,4},{7,6}},[]},
%% 			 {non_matched,{{7,4},{7,4}},[{non_matched,{{7,4},{7,6}}}]},
%% 			 {matched,{{7,6},{7,6}},[{non_matched,{{7,4},{7,6}}}]},
%% 			 {non_matched,{{7,6},{7,6}},[{non_matched,{{7,4},{7,6}}}]},
%% 			 {matched,{{7,4},{7,4}},[{{7,4},{7,6}}]},
%% 			 {non_matched,{{7,4},{7,4}},[{{7,4},{7,6}}]},
%% 			 {matched,{{7,6},{7,6}},[{{7,4},{7,6}}]},
%% 			 {non_matched,{{7,6},{7,6}},[{{7,4},{7,6}}]},
%% 			 {matched,{{9,4},{9,6}},[]}
%% 			])),
%%     ?assert(abtest:contains(Z, [
%% 			 {never_matched,{{7,4},{7,4}},[{non_matched,{{7,4},{7,6}}}]}
%% 			])).

