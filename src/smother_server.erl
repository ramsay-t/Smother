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
    %%FIXME content
    FDict = case lists:keyfind(File,1,State) of
		false -> [];
		{File, FD} -> FD
	    end,
    io:format("Filtering ~p~n",[FDict]),
    FDict2 = 
	case length(lists:filter(fun({L,_V}) -> within_loc(L,Loc) end, FDict)) of
	    0 ->
		io:format("Declaring decision point in ~p at ~p:~n~p~n", [File,Loc,Declaration]),
		%% FIXME - sub components?
		lists:keystore(Loc,1,FDict,{Loc,{Declaration,0,0}});
	    _ ->
		io:format("Decision point in ~p at ~p already declared~n", [File,Loc]),
		FDict
	end,
    {reply,ok,lists:keystore(File,1,State,{File,FDict2})};
handle_call({log,File,Loc,LogData},State) ->
    %%FIXME content
    io:format("Logging instance for ~p at ~p: ~p~n",[File,Loc,LogData]),
    {reply,ok,State};
handle_call({analyse,File},State) ->
    case lists:keyfind(File,1,State) of
	{File,FDict} ->
	    %% FIXME analysis?
	    Analysis = FDict,
	    {reply,{ok,Analysis},State};
	error ->
	    {reply,{error,no_record_found,File},State}
    end;
handle_call({analyse,File,Loc},State) ->
    case lists:keyfind(File,1,State) of
	{File,FDict} ->
	    %% FIXME analysis?
	    Analysis = lists:filter(fun({L,V}) -> within_loc(Loc,L) end,FDict),
	    {reply,{ok,Analysis},State};
	error ->
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

	
