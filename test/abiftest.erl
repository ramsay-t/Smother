-module(abiftest).
-export([dv/2]).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

dv(A,B) ->
    if (A == 0) and (B > 4) ->
	    B;
       true ->
	    B / A
    end.

zero1_test() ->
    {Z,NZ,_P} = abtest:do_tests(?MODULE,[{dv,[5,5]}]),
    ?assertEqual([{never_matched,{{8,9},{8,26}},[]},
 {never_matched,{{8,9},{8,14}},[{false,{{8,9},{8,26}}}]},
 {never_non_matched,{{8,22},{8,26}},[{false,{{8,9},{8,26}}}]},
 {never_matched,{{8,9},{8,14}},[{{8,9},{8,26}}]},
 {never_non_matched,{{8,22},{8,26}},[{{8,9},{8,26}}]},
 {never_matched,{{14,11},{14,12}},[]},
 {never_matched,{{32,11},{32,12}},[]},
 {never_matched,{{53,11},{53,12}},[]},
 {never_matched,{{74,11},{74,12}},[]}],Z), 
    ?assertEqual([{matched,{{7,4},{7,6}},[]},
 {non_matched,{{8,9},{8,26}},[]},
 {non_matched,{{8,9},{8,14}},[{false,{{8,9},{8,26}}}]},
 {matched,{{8,22},{8,26}},[{false,{{8,9},{8,26}}}]},
 {non_matched,{{8,9},{8,14}},[{{8,9},{8,26}}]},
 {matched,{{8,22},{8,26}},[{{8,9},{8,26}}]},
 {matched,{{10,8},{10,11}},[]}],NZ).
zero2_test() ->
    {Z,NZ,_P} = abtest:do_tests(?MODULE,[
					 {dv,[5,5]}
					 ,{dv,[5,0]}
					]),
    ?assertEqual([{never_matched,{{8,9},{8,26}},[]},
 {never_matched,{{8,9},{8,14}},[{false,{{8,9},{8,26}}}]},
 {never_matched,{{8,9},{8,14}},[{{8,9},{8,26}}]},
 {never_matched,{{14,11},{14,12}},[]},
 {never_matched,{{32,11},{32,12}},[]},
 {never_matched,{{53,11},{53,12}},[]},
 {never_matched,{{74,11},{74,12}},[]}],Z), 
    ?assertEqual([{matched,{{7,4},{7,6}},[]},
 {non_matched,{{8,9},{8,26}},[]},
 {non_matched,{{8,9},{8,14}},[{false,{{8,9},{8,26}}}]},
 {matched,{{8,22},{8,26}},[{false,{{8,9},{8,26}}}]},
 {non_matched,{{8,22},{8,26}},[{false,{{8,9},{8,26}}}]},
 {non_matched,{{8,9},{8,14}},[{{8,9},{8,26}}]},
 {matched,{{8,22},{8,26}},[{{8,9},{8,26}}]},
 {non_matched,{{8,22},{8,26}},[{{8,9},{8,26}}]},
 {matched,{{10,8},{10,11}},[]}], NZ).
zero3_test() ->
    {Z,NZ,_P} = abtest:do_tests(?MODULE,[{dv,[5,5]}
					 ,{dv,[5,0]}
					 ,{dv,[0,5]}
					]),
    ?assertEqual([{never_matched,{{8,9},{8,14}},[{false,{{8,9},{8,26}}}]},
 {never_matched,{{14,11},{14,12}},[]},
 {never_matched,{{32,11},{32,12}},[]},
 {never_matched,{{53,11},{53,12}},[]},
 {never_matched,{{74,11},{74,12}},[]}],Z), 
    ?assertEqual([{matched,{{7,4},{7,6}},[]},
 {matched,{{8,9},{8,26}},[]},
 {non_matched,{{8,9},{8,26}},[]},
 {non_matched,{{8,9},{8,14}},[{false,{{8,9},{8,26}}}]},
 {matched,{{8,22},{8,26}},[{false,{{8,9},{8,26}}}]},
 {non_matched,{{8,22},{8,26}},[{false,{{8,9},{8,26}}}]},
 {matched,{{8,9},{8,14}},[{{8,9},{8,26}}]},
 {non_matched,{{8,9},{8,14}},[{{8,9},{8,26}}]},
 {matched,{{8,22},{8,26}},[{{8,9},{8,26}}]},
 {non_matched,{{8,22},{8,26}},[{{8,9},{8,26}}]},
 {matched,{{10,8},{10,11}},[]}],NZ).
zero4_test() ->
    {Z,NZ,_P} = abtest:do_tests(?MODULE,[{dv,[5,5]}
					 ,{dv,[5,0]}
					 ,{dv,[0,5]}
					 ,{dv,[0,0]}
					]),
    %% Only the test functions are not hit.
    ?assertEqual([{never_matched,{{14,11},{14,12}},[]},
 {never_matched,{{32,11},{32,12}},[]},
 {never_matched,{{53,11},{53,12}},[]},
 {never_matched,{{74,11},{74,12}},[]}],Z), 
    ?assertEqual([{matched,{{7,4},{7,6}},[]},
 {matched,{{8,9},{8,26}},[]},
 {non_matched,{{8,9},{8,26}},[]},
 {matched,{{8,9},{8,14}},[{false,{{8,9},{8,26}}}]},
 {non_matched,{{8,9},{8,14}},[{false,{{8,9},{8,26}}}]},
 {matched,{{8,22},{8,26}},[{false,{{8,9},{8,26}}}]},
 {non_matched,{{8,22},{8,26}},[{false,{{8,9},{8,26}}}]},
 {matched,{{8,9},{8,14}},[{{8,9},{8,26}}]},
 {non_matched,{{8,9},{8,14}},[{{8,9},{8,26}}]},
 {matched,{{8,22},{8,26}},[{{8,9},{8,26}}]},
 {non_matched,{{8,22},{8,26}},[{{8,9},{8,26}}]},
 {matched,{{10,8},{10,11}},[]}],NZ).

