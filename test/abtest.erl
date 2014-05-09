-module(abtest).
-export([dv/2]).
-export([do_tests/2,contains/2]).

-include_lib("eunit/include/eunit.hrl").

dv(0,5) ->
    5;
dv(A,B) ->
    B / A.

do_tests(Module,Ts) ->
    smother:compile("../test/" ++ atom_to_list(Module) ++ ".erl"),
    lists:map(fun({A,B}) ->
		      catch Module:dv(A,B)
	      end,Ts),
    smother:analyse_to_file(Module),
    {smother:get_zeros(Module), smother:get_nonzeros(Module),smother:get_percentage(Module)}.

contains(_List,[]) ->
    true;
contains(List,[I|Is]) ->
    case lists:member(I,List) of
	false ->
	    false;
	_ ->
	    contains(List,Is)
    end.

full_test() ->
    {_Z,NZ,_P} = do_tests(?MODULE,[{0,5},{5,5},{5,0},{0,2}]),
    ?assert(contains(NZ,[
			 {matched,{{7,4},{7,6}},[]},
			 {non_matched,{{7,4},{7,6}},[]},
			 {matched,{{7,4},{7,4}},[{non_matched,{{7,4},{7,6}}}]},
			 {non_matched,{{7,4},{7,4}},[{non_matched,{{7,4},{7,6}}}]},
			 {matched,{{7,6},{7,6}},[{non_matched,{{7,4},{7,6}}}]},
			 {non_matched,{{7,6},{7,6}},[{non_matched,{{7,4},{7,6}}}]},
			 {matched,{{7,4},{7,4}},[{{7,4},{7,6}}]},
			 {non_matched,{{7,4},{7,4}},[{{7,4},{7,6}}]},
			 {matched,{{7,6},{7,6}},[{{7,4},{7,6}}]},
			 {non_matched,{{7,6},{7,6}},[{{7,4},{7,6}}]},
			 {matched,{{9,4},{9,6}},[]}
			])).

cond_dec_test() ->
    {Z,NZ,_P} = do_tests(?MODULE,[{0,5},{5,5},{5,0}]),
    ?assert(contains(NZ,[
			 {matched,{{7,4},{7,6}},[]},
			 {non_matched,{{7,4},{7,6}},[]},
			 {non_matched,{{7,4},{7,4}},[{non_matched,{{7,4},{7,6}}}]},
			 {matched,{{7,6},{7,6}},[{non_matched,{{7,4},{7,6}}}]},
			 {non_matched,{{7,6},{7,6}},[{non_matched,{{7,4},{7,6}}}]},
			 {matched,{{7,4},{7,4}},[{{7,4},{7,6}}]},
			 {non_matched,{{7,4},{7,4}},[{{7,4},{7,6}}]},
			 {matched,{{7,6},{7,6}},[{{7,4},{7,6}}]},
			 {non_matched,{{7,6},{7,6}},[{{7,4},{7,6}}]},
			 {matched,{{9,4},{9,6}},[]}
			])),
    ?assert(contains(Z, [
			 {never_matched,{{7,4},{7,4}},[{non_matched,{{7,4},{7,6}}}]}
			])).

