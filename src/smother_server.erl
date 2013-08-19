-module(smother_server).
-behaviour(gen_server).

-include("include/install.hrl").
-include("include/eval_records.hrl").
-include("include/analysis_reports.hrl").

-export([log/3,declare/3,analyse/1,analyse/2,clear/1,analyse_to_file/1,analyse_to_file/2]).
-export([init/1,handle_call/2,handle_cast/2,terminate/2,handle_call/3,code_change/3,handle_info/2]).

-export([build_pattern_record/1,build_bool_record/1,within_loc/2]).

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
			%%io:format("If Declaration:~n~p~n~p~n",[VarNames,Content]),
			ExpRecords = lists:map(fun ?MODULE:build_bool_record/1,Content),
			lists:keystore(Loc,1,FDict,{Loc,{if_expr,VarNames,ExpRecords}});
		    {case_expr,Expr,VarNames,Content} ->
			%%io:format("Case Declaration:~n~p~n~p~n~p~n",[Expr,VarNames,Content]),
			ExpRecords = lists:map(fun ?MODULE:build_pattern_record/1,Content),
			lists:keystore(Loc,1,FDict,{Loc,{case_expr,Expr,VarNames,ExpRecords}});
		    {fun_case,F,Arity,Args,_Guard} ->
			%%io:format("<~p, ~p> Function ~p/~p: ~p G:~p~n",[File,Loc,F,Arity,Args,Guard]),
			%% ArgRecords = lists:map(fun ?MODULE:build_pattern_record/1,Args),
			%%ArgRecords = build_pattern_record({fun_declaration,Loc,Args}),
			ArgRecords = 
			    case Args of 
				[] ->
				    {{StartLine,StartChar},{_EndLine,_EndChar}} = Loc,
				    BStart = StartChar + length(atom_to_list(F)),
				    BracketLoc = {{StartLine,BStart},{StartLine,BStart+1}},
				    build_pattern_record({wrapper,nil,{attr,BracketLoc,[{range,BracketLoc}],none},{nil,BracketLoc}});
				_ ->
				    build_pattern_record(list_from_list(Args))
			    end,
			%%Find function declaration and add a pattern...
			{OldLoc, {fun_expr,F,Arity,Patterns}} = 
			    case lists:filter(fun({_Loc,Rec}) ->
						      case Rec of
							  {fun_expr,F,Arity,_Patterns} ->
							      true;
							  _ ->
							      false
						      end
					      end
					      ,FDict) of
				[Rec] ->
				    Rec;
				[] ->
				    {Loc,{fun_expr,F,Arity,[]}}
			    end,
			NewFRecord = {fun_expr,F,Arity,Patterns ++ [{Loc,ArgRecords}]},
			%% This assumes declarations will arrive in order...
			{Start,_End} = OldLoc,
			{_NewStart,NewEnd} = Loc,
			NewLoc = {Start,NewEnd},
			lists:keystore(NewLoc,1,lists:keydelete(OldLoc,1,FDict),{NewLoc,NewFRecord});
		    _D ->
			io:format("Unknown smother declaration: ~p~n",[Declaration]),
			FDict
		end;
	    _ ->
		io:format("Decision point in ~p at ~p already declared~n", [File,Loc]),
		FDict
	end,
    {reply,ok,lists:keystore(File,1,State,{File,FDict2})};
handle_call({analyse,File},State) ->
    case lists:keyfind(File,1,State) of
	{File,FDict} ->
	    Analysis = FDict,
	    {reply,{ok,Analysis},State};
	_ ->
	    {reply,{error,no_record_found,File},State}
    end;
handle_call({analyse,File,Loc},State) ->
    case lists:keyfind(File,1,State) of
	{File,FDict} ->
	    Analysis = lists:filter(fun({L,_V}) -> within_loc(Loc,L) end,FDict),
	    {reply,{ok,Analysis},State};
	_ ->
	    {reply,{error,no_record_found,File},State}
    end;
handle_call({analyse_to_file,File,Outfile},State) ->
    case lists:keyfind(File,1,State) of
	{File,FDict} ->
	    {ok, OF} = file:open(Outfile, [write]),
	    Result = smother_analysis:make_html_analysis(File,FDict,OF),
	    file:close(OF),
	    {reply,{Result,Outfile},State};
	_ ->
	    {reply,{error,no_record_found,File},State}
    end;
handle_call(Action,State) ->
    io:format("Unexpected call to the smother server: ~p~n", [Action]),
    {reply,unknown_call,State}.

handle_cast(stop,S) ->
    {stop,normal,S};
handle_cast({log,File,Loc,LogData},State) ->
    FDict = case lists:keyfind(File,1,State) of
		false -> [];
		{File, FD} -> FD
	    end,
    if FDict == [] ->
	    {noreply,State};
       true ->
	    case lists:filter(fun({L,_R}) -> within_loc(L,Loc) end, FDict) of
		[] -> 
		    io:format("No relevant condition for location ~p~n",[Loc]),
		    {noreply,FDict};
		[{Loc,{if_expr,VarNames,ExRecords}}] ->
		    %io:format("Logging instance for ~p at ~p: ~p~n",[File,Loc,LogData]),
		    Bindings = lists:zip(VarNames,LogData),
		    ExRecords2 = apply_bool_log(Bindings,ExRecords,false),
		    NewFDict = lists:keystore(Loc,1,FDict,{Loc,{if_expr,VarNames,ExRecords2}}),
		    {noreply,lists:keystore(File,1,State,{File,NewFDict})};
		[{Loc,{case_expr,Expr,VarNames,ExRecords}}]  ->
		    [EVal | TrueLogData] = LogData,
		    %io:format("Logging case expr~nExp: ~p~nExp Val: ~p~nVarNames: ~p~nLogData: ~p~n", [Expr,EVal,VarNames,TrueLogData]),
		    Bindings = lists:zip(VarNames,TrueLogData),
		    ExRecords2 = apply_pattern_log(EVal,ExRecords,Bindings),
		    NewFDict = lists:keystore(Loc,1,FDict,{Loc,{case_expr,Expr,VarNames,ExRecords2}}),
		    {noreply,lists:keystore(File,1,State,{File,NewFDict})};
		[{ParentLoc, {fun_expr,F,Arity,Patterns}}] ->
		    NewPatterns = apply_fun_log(Loc,LogData,Patterns),
		    NewFDict = lists:keystore(ParentLoc,1,FDict,{ParentLoc,{fun_expr,F,Arity,NewPatterns}}),
		    {noreply,lists:keystore(File,1,State,{File,NewFDict})};
		D ->
		    io:format("Unknown declaration: ~p~n", [D]),
		    {noreply,lists:keystore(File,1,State,{File,FDict})}
	    end
    end;
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

analyse_to_file(File,Outfile) ->
    start_if_needed(),
    gen_server:call({global,smother_server},{analyse_to_file,File,Outfile}).
analyse_to_file(File) ->
    start_if_needed(),
    Outfile = lists:flatten(io_lib:format("~s-SMOTHER.html",[File])),
    gen_server:call({global,smother_server},{analyse_to_file,File,Outfile}).
    
    
declare(File,Loc,Declaration) ->
    start_if_needed(),
    gen_server:call({global,smother_server},{declare,File,Loc,Declaration}).

log(File,Loc,ParamValues) ->
    start_if_needed(),
    gen_server:cast({global,smother_server},{log,File,Loc,ParamValues}).

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


within_loc({{Sl,Sp},{El,Ep}} = _Loc, {{SSl,SSp},{SEl,SEp}} = _SubLoc) ->
    (
      (Sl < SSl)
      or ((Sl == SSl) and (Sp =< SSp))
    ) and (
	(El > SEl)
	or ((El == SEl) and (Ep >= SEp))
       ).

apply_bool_log(_Bindings,[],_All) ->
    [];
%%apply_bool_log(Bindings,[{E,TCount,FCount,TSubs,FSubs} | Es]) ->
apply_bool_log(Bindings,[#bool_log{}=Log | Es],All) ->
    %% Wrangler expects everything to be lists of things...
    E = revert(Log#bool_log.exp),
    %%io:format("Evaluating:~n~p~nUnder: ~p~n",[E,Bindings]),
    {value,Eval,_} = erl_eval:expr(E,Bindings),
    %%io:format("Evals to ~p~n",[Eval]),
    case Eval of
	true ->
	    NTSubs = apply_bool_log(Bindings,Log#bool_log.tsubs,true),
	    %% Don't continue applying once a condition matches?
	    if All ->
		    [Log#bool_log{tcount=Log#bool_log.tcount+1,tsubs=NTSubs} | apply_bool_log(Bindings,Es,All)];
	       true ->
		    [Log#bool_log{tcount=Log#bool_log.tcount+1,tsubs=NTSubs} | Es]
	    end;
	false ->
	    NFSubs = apply_bool_log(Bindings,Log#bool_log.fsubs,true),
	    [Log#bool_log{fcount=Log#bool_log.fcount+1,fsubs=NFSubs} | apply_bool_log(Bindings,Es,All)];
	Unexpected ->
	    exit({"Expected boolean expression",Unexpected,E,Bindings})
    end.
    
get_bool_subcomponents([]) ->
    [];
get_bool_subcomponents({tree,infix_expr,_Attrs,{infix_expr,Op,Left,Right}}) ->
    {tree,operator,_OpAttrs,Image} = Op,
    case lists:any(fun(E) -> Image == E end,['and','or','xor']) of
	true ->
	    [Left,Right];
	false ->
	    []
    end;
get_bool_subcomponents([_V | _VMore] = VList) ->
    io:format("Got a list with ~p elements...~n", [length(VList)]),
    [];
get_bool_subcomponents({wrapper,atom,_Attrs,_Atom}) ->
    [];
get_bool_subcomponents(V) ->
    VList = tuple_to_list(V),
    io:format("Expression with ~p elements, starting with {~p,~p,... ",[length(VList),lists:nth(1,VList),lists:nth(2,VList)]),
    io:format("UNKNOWN bool expression type:~n~p~n~n", [V]),
    [].

get_pattern_subcomponents({tree,tuple,_Attrs,Content}) ->
    Content;
get_pattern_subcomponents({tree,list,_Attrs,{list,[Head],none}}) ->
    [Head];
get_pattern_subcomponents({tree,list,_Attrs,{list,[Head],Tail}}) ->
    [Head | get_pattern_subcomponents(Tail)];
get_pattern_subcomponents({wrapper,underscore,_Attrs,_Image}) ->	
    [];
get_pattern_subcomponents({fun_declaration,_Loc,Args}) ->
    Args;
get_pattern_subcomponents(_V) ->
    %%io:format("UNKNOWN pattern expression type:~n~p~n~n", [V]),
    [].

build_bool_record([E]) ->
    Subs = lists:map(fun ?MODULE:build_bool_record/1,get_bool_subcomponents(E)),
    #bool_log{exp=E,tsubs=Subs,fsubs=Subs};
build_bool_record(E) ->
    Subs = lists:map(fun ?MODULE:build_bool_record/1,get_bool_subcomponents(E)),
    #bool_log{exp=E,tsubs=Subs,fsubs=Subs}.

build_pattern_record({[E],[]}) ->
    build_pattern_record({[E],[{atom,0,'true'}]});
build_pattern_record({[E],[G]}) ->
    %%io:format("PairHIT:~p~n",[{E,G}]),
    Subs = lists:map(fun ?MODULE:build_pattern_record/1,get_pattern_subcomponents(E)),
    Extras = make_extras(E),
    #pat_log{exp=E,guard=G,subs=Subs,extras=Extras,matchedsubs=Subs};
build_pattern_record([E]) ->
    %%io:format("HIT:~p~n",[E]),
    Subs = lists:map(fun ?MODULE:build_pattern_record/1,get_pattern_subcomponents(E)),
    Extras = make_extras(E),
    #pat_log{exp=E,subs=Subs,extras=Extras,matchedsubs=Subs};
build_pattern_record(E) ->
    %%io:format("Single HIT:~p~n",[E]),
    Subs = lists:map(fun ?MODULE:build_pattern_record/1,get_pattern_subcomponents(E)),
    Extras = make_extras(E),
    #pat_log{exp=E,subs=Subs,extras=Extras,matchedsubs=Subs}.


apply_pattern_log(_EVal,[],_Bindings) ->
    [];
apply_pattern_log(_EVal,[#pat_log{exp={wrapper,nil,_Attr,_Image}}=PatLog | Es],_Bindings) ->
    [PatLog#pat_log{
	   mcount=PatLog#pat_log.mcount+1
	  } | Es];
apply_pattern_log(EVal,[#pat_log{}=PatLog | Es],Bindings) ->
    %%io:format("Reverting ~p~nAgainst: ~p~n~n",[EVal,PatLog]),
    ValStx = abstract_revert(EVal),
    try
	%%io:format("Reverting ~p~n~n",[PatLog#pat_log.exp]),
	TrueExp = revert(PatLog#pat_log.exp),
	%%io:format("Comparing ~p to pattern ~p~nFrom: ~p vs ~p~n", [ValStx,TrueExp,EVal,PatLog#pat_log.exp]),
	
	_Comps = erl_eval:expr(erl_syntax:revert(erl_syntax:match_expr(TrueExp,ValStx)),Bindings),
	%% Don't continue on the other patterns once a pattern matches, they should not show any evaluation
	[PatLog#pat_log{
	   mcount=PatLog#pat_log.mcount+1,
	   matchedsubs=lists:map(fun(S) -> S#pat_log{mcount=S#pat_log.mcount+1} end,PatLog#pat_log.matchedsubs)
	  } | Es]
    catch
	error:_Msg ->
	    %%io:format("No Match: ~p vs ~p~n~p~n",[revert(PatLog#pat_log.exp),ValStx,Msg]),
	    case process_subs(PatLog,EVal,Bindings) of
		{NewSubs,Extra} ->
		    NewExtras = 
			case Extra of
			    no_extras ->
				PatLog#pat_log.extras;
			    _ ->
				case lists:keyfind(Extra,1,PatLog#pat_log.extras) of
				    {Extra,EMCount,ENMCount} ->
					lists:keyreplace(Extra,1,PatLog#pat_log.extras,{Extra,EMCount+1,ENMCount});
				    _ ->
					io:format("Unknown extra result: ~p~n",[Extra]),
					PatLog#pat_log.extras
				end
			end,
		    [PatLog#pat_log{nmcount=PatLog#pat_log.nmcount+1,subs=NewSubs,extras=NewExtras}| apply_pattern_log(EVal,Es,Bindings)];
		Err ->
		    exit({"Unexpected result from process_subs",Err})
	    end
    end.

%%process_subs(_E,[],_Eval,_Bindings) ->
process_subs(#pat_log{exp={tree,tuple,_Attrs,Content}=_Exp,subs=Subs},EVal,Bindings) ->
    %%io:format("Trying to evaluate ~p~n with ~p under ~p~n",[Exp,EVal,Bindings]),
    case abstract_revert(EVal) of
	{tuple,_OLine,ValContent} ->
	    if length(Content) /= length(ValContent) ->
		    {Subs,tuple_size_mismatch};
	       true ->
		    %% Tuple subs should always be the same order as the tuple content...
		    ZipList = lists:zip(Subs,ValContent),
		    NewSubs=lists:flatten(lists:map(fun({S,VC}) -> 
							    apply_pattern_log(
							      erl_parse:normalise(VC)
							      ,[S]
							      ,Bindings) 
						    end,
						    ZipList)
					 ),
		    {NewSubs, no_extras}
	    end;
	_Val ->
	    %%io:format("~p is not a tuple...~n",[Val]),
	    {Subs,not_a_tuple}
    end;
process_subs(#pat_log{exp={wrapper,integer,_Attrs,_Image},subs=Subs},_EVal,_Bindings) ->
    {Subs,no_extras};
process_subs(#pat_log{exp={wrapper,nil,_Attrs,_Image},subs=Subs},_EVal,_Bindings) ->
    {Subs,no_extras};
process_subs(#pat_log{exp={tree,list,_Attrs,_Content},subs=Subs},EVal,Bindings) ->
    case abstract_revert(EVal) of
	{cons,_OLine,Head,ValContent} ->
	    ContentList = [Head | list_to_list(ValContent)],
	    if length(Subs) == 0 ->
		    {Subs,non_empty_list};
	       length(Subs) /= length(ContentList) ->
		    {Subs,list_size_mismatch};
	       true ->
		    ZipList = lists:zip(Subs,ContentList),
		    NewSubs = lists:flatten(lists:map(fun({S,VC}) ->
							      apply_pattern_log(
								erl_parse:normalise(VC)
								,[S]
								,Bindings)
						      end,
						      ZipList)
					   ),
		    {NewSubs, no_extras}
	       end;
	{nil,_OLine} ->
	    if length(Subs) /= 0 ->
		    {Subs, empty_list};
	       true ->
		    {Subs, no_extras}
	    end;
	Val ->
	    io:format("~p is not a list...~n",[Val]),
	    {Subs,not_a_list}
    end;
process_subs(#pat_log{exp={fun_declaration,Loc,Rec},subs=Subs},_Eval,_Bindings) ->
    io:format("Fun pattern: ~p ~p~n",[Loc,Rec]),
    {Subs,no_extras};
process_subs(#pat_log{exp={wrapper,atom,_Attrs,_Image},subs=Subs},_EVal,_Bindings) ->
    {Subs,no_extras};
process_subs(#pat_log{subs=Subs}=S,EVal,Bindings) ->
    io:format("Don't know how to process sub: ~p~nwith ~p under ~p~n", [S,EVal,Bindings]),
    {Subs,no_extras}.

make_extras({tree,tuple,_,_}) ->
    [{tuple_size_mismatch,0,0},{not_a_tuple,0,0}];
make_extras({tree,list,_Attrs,none}) ->
    [{non_empty_list,0,0},{not_a_list,0,0}];
make_extras({tree,list,_Attrs,{list,_,_}}) ->
    [{empty_list,0,0},{list_size_mismatch,0,0},{not_a_list,0,0}];
make_extras(_P) ->
    %%io:format("No extras for ~p~n",[P]),
    [].



list_to_list({nil,_Line}) ->
    [];
list_to_list({cons,_Line,Item,Tail}) ->
    [Item | list_to_list(Tail)].

list_from_list([]) ->
    none;
list_from_list([Item|More]) ->
    {IStart,IEnd} = smother_analysis:get_range(Item),
    {_EndStart,EndLoc} = 
	case More of
	    [] ->
		{IStart,IEnd};
	    _ ->
		smother_analysis:get_range(lists:nth(length(More),More))
	end,
    NewLoc = {IStart,EndLoc},
    {tree,list,{attr,NewLoc,[{range,NewLoc}],none},{list,[Item],list_from_list(More)}}.

revert(Exp) ->
    hd(wrangler_syntax:revert_forms([Exp])).


apply_fun_log(_Loc,_LogData,[]) ->
    [];
apply_fun_log(Loc,LogData,[{Loc,Rec} | Ps]) ->
    %%io:format("MATCH: ~p vs ~p~n",[LogData,Rec]),
    NewSubs = hd(apply_pattern_log(LogData,[Rec],[])),
    %%ZipList = lists:zip(LogData,Subs),
    %% FIXME content
    %%NewSubs = lists:map(fun({D,S}) -> 
%%			       hd(apply_pattern_log(D,[S],[]))
%%		       end,
%%		       ZipList),
    [{Loc,NewSubs} | Ps];
apply_fun_log(Loc,LogData,[{PreLoc,Rec} | Ps]) ->
    %%io:format("NONMATCH: ~p vs ~p~n",[LogData,Rec]),
    NewSubs = hd(apply_pattern_log(LogData,[Rec],[])),
%%    ZipList = lists:zip(LogData,Subs),
    %% FIXME content
%%    NewSubs = lists:map(fun({D,S}) -> 
%%			       hd(apply_pattern_log(D,[S],[]))
%%		       end,
%%		       ZipList),
    [{PreLoc,NewSubs} | apply_fun_log(Loc,LogData,Ps)].


abstract_revert(EVal) ->
    try
	erl_syntax:revert(erl_syntax:abstract(EVal))
    catch
	error:PIDMsg ->
	    %% PID types can't ba abstracted
	    %%io:format("PID problem: ~p~n",[PIDMsg]),
	    %%{nil,0}
	    {nil,0}
    end.

	
