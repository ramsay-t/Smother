-module(smother_annotater).

-compile([export_all]).

-exports([annotate/2, get_forms/1]).

-include("include/install.hrl").

annotate(File, FDict) ->
    io:format("~p~n", [FDict]).


f(File,FDict) ->
    wrangler_ast_server:start_ast_server(),
    {ok,{tree,form_list,Attrs,Content}} = api_refac:get_ast(File),
    Result = lists:map(fun(C) -> annotate_item(C,FDict) end, Content),
    lists:flatten(Result).

annotate_item({tre,attribute,_,_} = C, _FDict) ->
    ?PP(C) ++ "\n";
annotate_item(C, FDict) ->
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
    sift_terms(More, {MTs ++ [M], ETs, FTs});
sift_terms([[{'-',Line},{atom,Line,export} | _Ex] = E| More], {MTs,ETs,FTs}) ->
    sift_terms(More, {MTs, ETs ++ [E], FTs});
sift_terms([[{'-',Line},{atom,Line,include}| _Inc] = M| More], {MTs,ETs,FTs}) ->
    io:format("Caught ~p~n",[M]),
    sift_terms(More, {MTs ++ [M], ETs, FTs});
sift_terms([[{'-',Line},{atom,Line,include_lib}| _Inc] = M| More], {MTs,ETs,FTs}) ->
    io:format("Caught ~p~n",[M]),
    sift_terms(More, {MTs ++ [M], ETs, FTs});
sift_terms([F | More], {MTs,ETs,FTs}) ->
    sift_terms(More, {MTs, ETs, FTs ++ [F]}).


split_exprs([], Current, Results) ->
    lists:reverse([Current | Results]);
split_exprs([{dot,Line} | More], Current, Results) ->
    split_exprs(More, [], [lists:reverse([{dot,Line}|Current]) | Results]);
split_exprs([E | More], Current, Results) ->
    split_exprs(More, [E | Current], Results).
