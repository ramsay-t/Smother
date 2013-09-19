-module(smother_annotater).

-compile([export_all]).

-exports([annotate/2, get_forms/1, make_tmp_file/2, get_tmp/0]).

-include_lib("wrangler/include/wrangler.hrl").

annotate(_File, FDict) ->
    io:format("~p~n", [FDict]).


f(File,FDict) ->
    wrangler_ast_server:start_ast_server(),
    {ok,{tree,form_list,_Attrs,Content}} = api_refac:get_ast(File),
    Result = lists:map(fun(C) -> annotate_item(C,FDict) end, Content),
    lists:flatten(Result).

annotate_item({tre,attribute,_,_} = C, _FDict) ->
    ?PP(C) ++ "\n";
annotate_item(C, _FDict) ->
    io:format("------------------~n~p~n------------------~n", [C]),
    ?PP(C) ++ "\n".

    
open_file(File) ->
    {ok, IF} = file:open(File,[read]),
    Content = read_file(IF,""),
    file:close(IF),
    Content.

read_file(IF, Content) ->
    case file:read(IF,1) of
	eof ->
	    lists:flatten(lists:reverse(Content));
	{error, Reason} ->
	    exit(Reason);
	{ok, Data} ->
	    read_file(IF,[Data | Content])
    end.

%% Convert a code string into a form suitable for compiling

get_forms(Code) ->
    {ok, Ts, _} = erl_scan:string(Code),
    {MTs,ETs,FTs} = sift_terms(split_exprs(Ts,[],[]),{[],[],[]}),
    io:format("~p~n",[{MTs,ETs,FTs}]),
    {ok,MFs} =  erl_parse:parse_form(MTs),
    {ok,EFs} =  erl_parse:parse_form(ETs),
    {ok,FFs} =  erl_parse:parse_form(FTs),
    {MFs,EFs,FFs}.

sift_terms([],{MTs,ETs,FTs}) ->
    {
      lists:flatten(lists:reverse(MTs))
      ,lists:flatten(lists:reverse(ETs))
      ,lists:flatten(lists:reverse(FTs))
    };
sift_terms([[{'-',Line},{atom,Line,module}| _Mod] = M| More], {MTs,ETs,FTs}) ->
    sift_terms(More, {[M|MTs], ETs, FTs});
sift_terms([[{'-',Line},{atom,Line,export} | _Ex] = E| More], {MTs,ETs,FTs}) ->
    sift_terms(More, {MTs, [E|ETs], FTs});
sift_terms([[{'-',Line},{atom,Line,include}| _Inc] = M| More], {MTs,ETs,FTs}) ->
    io:format("Caught ~p~n",[M]),
    sift_terms(More, {[M|MTs], ETs, FTs});
sift_terms([[{'-',Line},{atom,Line,include_lib}| _Inc] = M| More], {MTs,ETs,FTs}) ->
    io:format("Caught ~p~n",[M]),
    sift_terms(More, {[M|MTs], ETs, FTs});
sift_terms([F | More], {MTs,ETs,FTs}) ->
    sift_terms(More, {MTs, ETs, [F|FTs]}).


split_exprs([], Current, Results) ->
    lists:reverse([Current | Results]);
split_exprs([{dot,Line} | More], Current, Results) ->
    split_exprs(More, [], [lists:reverse([{dot,Line}|Current]) | Results]);
split_exprs([E | More], Current, Results) ->
    split_exprs(More, [E | Current], Results).

make_tmp_file(ModName,Code) ->
    FName = get_tmp() ++ atom_to_list(ModName) ++ ".erl",
    io:format("Making ~p~n",[FName]),
    file:write_file(FName,Code++"\n"),
%%    {ok, F} = file:open(FName,[write]),
%%    io:fwrite(F,Code,[]),
%%    file:close(F),
    FName.

get_tmp() ->
    case os:getenv("TMPDIR") of
	false ->
	    case os:getenv("TMP") of
		false ->
		    "/tmp/";
		Dir ->
		    Dir
		end;
	Dir ->
	    Dir
    end.
