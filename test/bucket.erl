-module(bucket).
-export([filter_bucket/4]).

-include_lib("eunit/include/eunit.hrl").

filter_bucket(_, [], Fb, Fc) -> 
    {Fb,Fc};
filter_bucket(F, [E|Bkt], Fb, Fc) ->
    case F(E) of
	true -> 
	    filter_bucket(F, Bkt, [E|Fb], Fc);
	false -> 
	    filter_bucket(F, Bkt, Fb, Fc+1)
    end.

%% buk_fun(E) ->
%%     E == 1.

%% full_test() ->
%%     {_Z,NZ,_P} = abtest:do_tests(?MODULE,[
%% 					  {filter_bucket,[fun buk_fun/1, [], [], 0]}
%% 					  ,{filter_bucket,[fun buk_fun/1, [2], [], 0]}
%% 					  ,{filter_bucket,[fun buk_fun/1, [1], [], 0]}
%% 					 ]),
%%     smother:analyse_to_file(?MODULE),
%%     ?assert(abtest:contains(NZ,[
%% 				{matched,{{6,15},{6,27}},[]},
%% 				{non_matched,{{6,15},{6,27}},[]},
%% 				{matched,{{6,15},{6,15}},[{non_matched,{{6,15},{6,27}}}]},
%% 				{non_matched,{{6,18},{6,19}},[{non_matched,{{6,15},{6,27}}}]},
%% 				{matched,{{6,22},{6,23}},[{non_matched,{{6,15},{6,27}}}]},
%% 				{matched,{{6,26},{6,27}},[{non_matched,{{6,15},{6,27}}}]},
%% 				{matched,{{6,15},{6,15}},[{{6,15},{6,27}}]},
%% 				{matched,{{6,18},{6,19}},[{{6,15},{6,27}}]},
%% 				{non_matched,{{6,18},{6,19}},[{{6,15},{6,27}}]},
%% 				{matched,{{6,22},{6,23}},[{{6,15},{6,27}}]},
%% 				{matched,{{6,26},{6,27}},[{{6,15},{6,27}}]},
%% 				{matched,{{8,15},{8,32}},[]},
%% 				{matched,{{8,15},{8,15}},[{{8,15},{8,32}}]},
%% 				{matched,{{8,18},{8,24}},[{{8,15},{8,32}}]},
%% 				{matched,{{8,27},{8,28}},[{{8,15},{8,32}}]},
%% 				{matched,{{8,31},{8,32}},[{{8,15},{8,32}}]},
%% 				{matched,{{10,9},{10,12}},[]},
%% 				{non_matched,{{10,9},{10,12}},[]},
%% 				{matched,{{12,9},{12,13}},[]} 
%% 			       ])).

