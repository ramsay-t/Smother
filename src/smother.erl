-module(smother).
-export([compile/1,analyse/1,analyze/1]).

-include("include/install.hrl").

compile(Filename) ->
    wrangler_ast_server:start_ast_server(),
    smother_server:clear(Filename),
    %%{ok,AST} = api_refac:get_ast(Filename),
    {ok, [{{Filename, Filename}, AST2}]} = instrument(Filename),
    Code = ?PP(AST2),
    %%io:format("Transformed code to~n~s~n", [Code]),
    {MFs,EFs,FTs} = get_forms(Code),
    %Forms = wrangler_syntax:revert_forms(AST2),
    %%io:format("~n~nForms:~p~n~n",[{MFs,EFs,FTs}]),
    case compile:forms([MFs,EFs,FTs],[debug_info]) of
	{ok,Module,Binary} ->
	    code:load_binary(Module,Filename,Binary);
	_Error ->
	    exit("Failed to compile the transformed module.")
    end.

analyse(File) ->
    smother_server:analyse(File).

analyze(File) ->
    analyse(File).

instrument(File) ->
    ?FULL_TD_TP([?RULE(?T("f@(Args@@) when Guard@@ -> Body@@;"),
		       begin
			   %%ArgNames = get_arg_names(Args@@),
			   Loc = api_refac:start_end_loc(_This@),
			   LocString = get_loc_string(_This@),
			   Declare = {fun_case,wrangler_syntax:revert_forms(Args@@),wrangler_syntax:revert_forms(Guard@@)},
			   smother_server:declare(File,Loc,Declare),
			   ?TO_AST("f@(Args@@) when Guard@@-> smother_server:log(\"" ++ File ++ "\"," ++ LocString ++ ",[Args@@]), Body@@;")
		       end
		       ,true)
		], 
		[File]).

get_loc_string(_This@) ->
    Loc = api_refac:start_end_loc(_This@),
    lists:flatten(io_lib:format("~p", [Loc])).

get_arg_names(Args@@) ->
    lists:flatten("[" ++ "\"" ++ re:replace(?PP(Args@@), ",", "\",\"", [{return,list},global]) ++ "\"" ++ "]").

%% Convert a pretty printed code string into a form suitable for compiling

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
    sift_terms(More, {[M | MTs], ETs, FTs});
sift_terms([[{'-',Line},{atom,Line,export} | _Ex] = E| More], {MTs,ETs,FTs}) ->
    sift_terms(More, {MTs, [E | ETs], FTs});
sift_terms([F | More], {MTs,ETs,FTs}) ->
    sift_terms(More, {MTs, ETs, [F | FTs]}).


split_exprs([], Current, Results) ->
    lists:reverse([Current | Results]);
split_exprs([{dot,Line} | More], Current, Results) ->
    split_exprs(More, [], [lists:reverse([{dot,Line}|Current]) | Results]);
split_exprs([E | More], Current, Results) ->
    split_exprs(More, [E | Current], Results).
