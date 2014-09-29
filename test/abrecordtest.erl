-module(abrecordtest).
-export([dv/1]).

-include_lib("eunit/include/eunit.hrl").

-record(abrecord,
	{
	  a,
	  b,
	  c=12,
	  d=42
	}).

-record(subrecord,
	{
	  i=0,
	  j=0
	}).

dv(#abrecord{a=_,b=#subrecord{i=I,j=J}}) when (J == 0) ->
    I;
dv(#abrecord{a=_,b=#subrecord{i=I,j=J}}) ->
    I / J;
dv(#abrecord{a=0,b=5}) ->
    5;
dv(#abrecord{a=A,b=B}) ->
    B / A.

compile_test() ->
    smother:compile("../test/" ++ atom_to_list(?MODULE) ++ ".erl"),
    ?assertEqual(0.5, abrecordtest:dv(#abrecord{a=1,b=#subrecord{i=2,j=4}})).

full_test() ->
    {_Z,NZ,_P} = abtest:do_dv_tests(?MODULE,[{0,5},{5,5},{5,0},{0,2}]),
    ?assert(false),
    ?assert(abtest:contains(NZ,[
				{matched,{{14,4},{14,21}},[]},
				{non_matched,{{14,4},{14,21}},[]},
				{matched,{{14,14},{14,16}},[{non_matched,{{14,4},{14,21}}}]},
				{non_matched,{{14,14},{14,16}},[{non_matched,{{14,4},{14,21}}}]},
				{matched,{{14,18},{14,21}},[{non_matched,{{14,4},{14,21}}}]},
				{non_matched,{{14,18},{14,21}},[{non_matched,{{14,4},{14,21}}}]},
				{matched,{{14,14},{14,16}},[{{14,4},{14,21}}]},
				{non_matched,{{14,14},{14,16}},[{{14,4},{14,21}}]},
				{matched,{{14,18},{14,21}},[{{14,4},{14,21}}]},
				{non_matched,{{14,18},{14,21}},[{{14,4},{14,21}}]},
				{matched,{{16,4},{16,21}},[]}
			       ])).

cond_dec_test() ->
    {Z,NZ,_P} = abtest:do_dv_tests(?MODULE,[{0,5},{5,5},{5,0}]),
    ?assert(abtest:contains(NZ,[
				{matched,{{14,4},{14,21}},[]},
				{non_matched,{{14,4},{14,21}},[]},
				{non_matched,{{14,14},{14,16}},[{non_matched,{{14,4},{14,21}}}]},
				{matched,{{14,18},{14,21}},[{non_matched,{{14,4},{14,21}}}]},
				{non_matched,{{14,18},{14,21}},[{non_matched,{{14,4},{14,21}}}]},
				{matched,{{14,14},{14,16}},[{{14,4},{14,21}}]},
				{non_matched,{{14,14},{14,16}},[{{14,4},{14,21}}]},
				{matched,{{14,18},{14,21}},[{{14,4},{14,21}}]},
				{non_matched,{{14,18},{14,21}},[{{14,4},{14,21}}]},
				{matched,{{16,4},{16,21}},[]}
			       ])),
    ?assert(abtest:contains(Z, [
				{never_matched,{{14,14},{14,16}},[{non_matched,{{14,4},{14,21}}}]}
			       ])).

