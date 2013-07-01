-module(smother_server).
-behaviour(gen_server).

-export([log/3,declare/3,analyse/1,analyse/2,clear/1]).
-export([init/1,handle_call/2,handle_cast/2,terminate/2,handle_call/3,code_change/3,handle_info/2]).

-compile([export_all]).

init(Dict) ->
    {ok,Dict}.

handle_call(Action,_From,State) ->
    handle_call(Action,State).

handle_call({clear,File},State) ->
    {reply,ok,lists:keystore(File,1,State,{File,[]})};
handle_call({declare,File,Loc,Declaration},State) ->
    FDict = case lists:keyfind(File,1,State) of
		false -> [];
		{File, FD} -> FD
	    end,
    FDict2 = 
	case length(lists:filter(fun({L,_V}) -> within_loc(L,Loc) end, FDict)) of
	    0 ->
		case Declaration of
		    {if_expr,VarNames,Content} ->
			io:format("If Declaration:~n~p~n~p~n",[VarNames,Content]),
			ExpRecords = lists:map(fun ?MODULE:build_bool_record/1,Content),
			lists:keystore(Loc,1,FDict,{Loc,{if_expr,VarNames,ExpRecords}});
		    {case_expr,Expr,VarNames,Content} ->
			io:format("Case Declaration:~n~p~n~p~n~p~n",[Expr,VarNames,Content]),
			ExpRecords = lists:map(fun ?MODULE:build_pattern_record/1,Content),
			lists:keystore(Loc,1,FDict,{Loc,{case_expr,Expr,VarNames,ExpRecords}});
		    _D ->
			io:format("Unknown smother declaration: ~p~n",[Declaration]),
			FDict
		end;
	    _ ->
		io:format("Decision point in ~p at ~p already declared~n", [File,Loc]),
		FDict
	end,
    {reply,ok,lists:keystore(File,1,State,{File,FDict2})};
handle_call({log,File,Loc,LogData},State) ->
    FDict = case lists:keyfind(File,1,State) of
		false -> [];
		{File, FD} -> FD
	    end,
    if FDict == [] ->
	    {reply,failed,State};
       true ->
	    case lists:keyfind(Loc,1,FDict) of
		false -> 
		    io:format("No relevant condition for location ~p~n",[Loc]),
		    {reply,failed,FDict};
		{Loc,{if_expr,VarNames,ExRecords}} ->
		    io:format("Logging instance for ~p at ~p: ~p~n",[File,Loc,LogData]),
		    Bindings = lists:zip(VarNames,LogData),
		    ExRecords2 = apply_log(Bindings,ExRecords),
		    NewFDict = lists:keystore(Loc,1,FDict,{Loc,{if_expr,VarNames,ExRecords2}}),
		    {reply,ok,lists:keystore(File,1,State,{File,NewFDict})};
		{Loc,{case_expr,Expr,VarNames,ExRecords}}  ->
		    [EVal | TrueLogData] = LogData,
		    io:format("Logging case expr~nExp: ~p~nExp Val: ~p~nVarNames: ~p~nLogData: ~p~n", [Expr,EVal,VarNames,TrueLogData]),
		    Bindings = lists:zip(VarNames,TrueLogData),
		    ExRecords2 = apply_pattern_log(EVal,ExRecords,Bindings),
		    NewFDict = lists:keystore(Loc,1,FDict,{Loc,{case_expr,Expr,VarNames,ExRecords2}}),
		    {reply,ok,lists:keystore(File,1,State,{File,NewFDict})};
		D ->
		    io:format("Unknown declaration: ~p~n", [D]),
		    {reply,ok,lists:keystore(File,1,State,{File,FDict})}
	    end
    end;
handle_call({analyse,File},State) ->
    case lists:keyfind(File,1,State) of
	{File,FDict} ->
	    %% FIXME analysis?
	    Analysis = FDict,
	    {reply,{ok,Analysis},State};
	_ ->
	    {reply,{error,no_record_found,File},State}
    end;
handle_call({analyse,File,Loc},State) ->
    case lists:keyfind(File,1,State) of
	{File,FDict} ->
	    %% FIXME analysis?
	    Analysis = lists:filter(fun({L,_V}) -> within_loc(Loc,L) end,FDict),
	    {reply,{ok,Analysis},State};
	_ ->
	    {reply,{error,no_record_found,File},State}
    end;
handle_call(Action,State) ->
    io:format("Unexpected call to the smother server: ~p~n", [Action]),
    {reply,unknown_call,State}.

handle_cast(stop,S) ->
    {stop,normal,S};
handle_cast(M,S) ->
    io:format("Unexpected cast msg to smother server:~w~n", [M]),
    {noreply,S}.

terminate(normal,_State) ->
    ok.

handle_info(Info,State) ->
    io:format("Smother server recieved information: ~p~n",[Info]),
    {noreply,State}.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.


%% API functions

analyse(File) ->
    start_if_needed(),
    gen_server:call({global,smother_server},{analyse,File}).

analyse(File,Loc) ->
    start_if_needed(),
    gen_server:call({global,smother_server},{analyse,File,Loc}).

    
declare(File,Loc,Declaration) ->
    start_if_needed(),
    gen_server:call({global,smother_server},{declare,File,Loc,Declaration}).

log(File,Loc,ParamValues) ->
    start_if_needed(),
    gen_server:call({global,smother_server},{log,File,Loc,ParamValues}).

clear(File) ->
    start_if_needed(),
    gen_server:call({global,smother_server},{clear,File}).
    

start_if_needed() ->
    case global:whereis_name(smother_server) of
	undefined ->
	    gen_server:start({global,smother_server},smother_server,[],[]);
	_ ->
	    ok
    end.


within_loc({{Sl,Sp},{El,Ep}}, {{SSl,SSp},{SEl,SEp}}) ->
    io:format("Is ~p within ~p?~n",[{{SSl,SSp},{SEl,SEp}},{{Sl,Sp},{El,Ep}}]),
    (
      (Sl < SSl)
      or ((Sl == SSl) and (Sp =< SSp))
    ) and (
	(El > SEl)
	or ((El == SEl) and (Ep >= SEp))
       ).

apply_log(Bindings,[]) ->
    [];
apply_log(Bindings,[{E,TCount,FCount,TSubs,FSubs} | Es]) ->
    io:format("Evaluating:~n~p~nUnder: ~p~n",[E,Bindings]),
    {value,Eval,_} = erl_eval:expr(E,Bindings),
    io:format("Evals to ~p~n",[Eval]),
    case Eval of
	true ->
	    NTSubs = apply_log(Bindings,TSubs),
	    [{E,TCount+1,FCount,NTSubs,FSubs} | apply_log(Bindings,Es)];
	false ->
	    NFSubs = apply_log(Bindings,FSubs),
	    [{E,TCount,FCount+1,TSubs,NFSubs} | apply_log(Bindings,Es)];
	Unexpected ->
	    exit({"Expected boolean expression",Unexpected,E,Bindings})
    end.
    
get_bool_subcomponents({op,Line,Image,Left,Right}) ->
    case lists:any(fun(E) -> Image == E end,['and','or','xor']) of
	true ->
	    [Left,Right];
	false ->
	    []
    end;
get_bool_subcomponents({_Type,_,_}) ->	
    [];
get_bool_subcomponents(V) ->
    io:format("UNKNOWN expression type:~n~p~n~n", [V]),
    [].

get_pattern_subcomponents({tuple,Line,Content}) ->
    Content;
get_pattern_subcomponents({_Type,_,_}) ->	
    [];
get_pattern_subcomponents(V) ->
    io:format("UNKNOWN expression type:~n~p~n~n", [V]),
    [].

build_bool_record(E) ->
    Subs = lists:map(fun ?MODULE:build_bool_record/1,get_bool_subcomponents(E)),
    {E,0,0,Subs,Subs}.

build_pattern_record({E,G}) ->
    Subs = lists:map(fun ?MODULE:build_pattern_record/1,get_pattern_subcomponents(E)),
    Extras = make_extras(E),
    {{E,G},0,0,Subs,Extras};
build_pattern_record(E) ->
    Subs = lists:map(fun ?MODULE:build_pattern_record/1,get_pattern_subcomponents(E)),
    Extras = make_extras(E),
    {{E,{atom,0,'true'}},0,0,Subs,Extras}.

apply_pattern_log(EVal,[],Bindings) ->
    [];
apply_pattern_log(EVal,[{{E,G},MCount,NMCount,Subs,Extras} | Es],Bindings) ->
    %%io:format("Comparing ~p to pattern ~p under guard ~p~n", [EVal,E,G]),
    ValStx = erl_syntax:revert(erl_syntax:abstract(EVal)),
    try
	%%io:format("E = ~p.~nValStx = ~p.~nBindings = ~p.~n",[E,ValStx,Bindings]),
	Comps = erl_eval:expr(erl_syntax:revert(erl_syntax:match_expr(E,ValStx)),Bindings),
	%%% FIXME test guard....
	%%io:format("Match...(~p)~n",[Comps]),

	%% Don't continue on the other patterns once a pattern matches, they should not show any evaluation
	[{{E,G},MCount+1,NMCount,Subs,Extras} | Es]
    catch
	error:Msg ->
	    %%io:format("No Match...(~p)~n", [Msg]),
	    case process_subs(E,Subs,EVal,Bindings) of
		{ok,NewSubs} ->
		    [{{E,G},MCount,NMCount+1,NewSubs,Extras} | apply_pattern_log(EVal,Es,Bindings)];
		{extra,Extra} ->
		    NewExtras = case lists:keyfind(Extra,1,Extras) of
				    {Extra,ECount} ->
					lists:keyreplace(Extra,1,Extras,{Extra,ECount+1});
				    false ->
					io:format("Unknown extra result: ~p~n",[Extra]),
					Extras
				end,
		    [{{E,G},MCount,NMCount+1,Subs,NewExtras} | apply_pattern_log(EVal,Es,Bindings)]
	    end
    end.

process_subs(_E,[],_Eval,_Bindings) ->
    {ok,[]};
process_subs({tuple,Line,Content},Subs,EVal,Bindings) ->
    case erl_syntax:revert(erl_syntax:abstract(EVal)) of
	{tuple,_OLine,ValContent} ->
	    if length(Content) /= length(Subs) ->
		    {extra, tuple_size_mismatch};
	       true ->
		    {ok, lists:flatten(lists:map(fun({VC,S}) ->
					   apply_pattern_log(erl_parse:normalise(VC),[S],Bindings)
				   end,
				   lists:zip(ValContent,Subs)
				  ))
		    }
	    end;
	Val ->
	    %%io:format("~p is not a tuple...~n",[Val]),
	    {extra,not_a_tuple}
    end.

make_extras({tuple,_,_}) ->
    [{tuple_size_mismatch,0},{not_a_tuple,0}];
make_extras(_) ->
    [].
