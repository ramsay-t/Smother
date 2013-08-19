-module(smother).
-export([compile/1,compile/2,analyse/1,analyze/1,analyse_to_file/1,analyze_to_file/1]).

-include("include/install.hrl").

compile(Filename) ->
    compile(Filename,[]).

compile(Filename,Includes) ->
    wrangler_ast_server:start_ast_server(),
    smother_server:clear(Filename),
    {ok, [{{Filename, Filename}, AST2}]} = instrument(Filename),
    Code = ?PP(AST2),

    {ok, ModInfo} = api_refac:get_module_info(Filename),
    {module,ModName} = lists:keyfind(module,1,ModInfo),

    TmpFile = smother_annotater:make_tmp_file(ModName,Code),
    {ok,Forms} = epp:parse_file(TmpFile,Includes,[]),

    %%{MFs,EFs,FTs} = smother_annotater:get_forms(TmpFile,Includes),
    %%case compile:forms([MFs,EFs,FTs],[debug_info]) of
    %%case compile:file(TmpFile,[binary,debug_info]) of
    case compile:forms(Forms,[binary,debug_info]) of
	{ok,Module,Binary} ->
	    code:load_binary(Module,Filename,Binary);
	Error ->
	    exit({"Failed to compile the transformed module.",Error})
    end.

analyse(File) ->
    smother_server:analyse(File).

analyze(File) ->
    analyse(File).

analyse_to_file(File) ->
    smother_server:analyse_to_file(File).

analyze_to_file(File) ->
    analyse_to_file(File).


instrument(File) ->
    ?STOP_TD_TP([
		 ?RULE(?T("f@(Args@@) when Guard@@ -> Body@@;"),
		       begin
			   %%ArgNames = get_arg_names(Args@@),
			   Loc = api_refac:start_end_loc(_This@),
			   LocString = get_loc_string(_This@),
			   FName = erl_parse:normalise(wrangler_syntax:revert(_W_f@)),
			   Declare = {fun_case,FName,length(Args@@),Args@@,Guard@@},
			   smother_server:declare(File,Loc,Declare),
			   ?TO_AST("f@(Args@@) when Guard@@-> smother_server:log(\"" ++ File ++ "\"," ++ LocString ++ ",[Args@@]), Body@@;")
		       end
		       ,true),
		 ?RULE(?T("if Guards@@@ -> Body@@@ end"),
		       begin
			   Loc = api_refac:start_end_loc(_This@),
			   LocString = get_loc_string(_This@),
			   %%GuardList@@@ = lists:flatten(lists:map(fun(G) -> io:format("G: ~p~n", [G]), wrangler_syntax:revert_forms(G) end, Guards@@@)),
			   VarList = lists:flatten(lists:map(fun(G) -> api_refac:free_var_names(G) end, Guards@@@)),
			   VarListString = re:replace(lists:flatten(io_lib:format("~p", [VarList])),"'","",[{return,list},global]),
			   Declare = {if_expr,VarList,Guards@@@},
			   smother_server:declare(File,Loc,Declare),
			   ?TO_AST("begin smother_server:log(\"" ++ File ++ "\"," ++ LocString ++ "," ++ VarListString ++ "), if Guards@@@ -> Body@@@ end end")
		       end
		       ,true),
		 ?RULE(?T("case Expr@@ of Pats@@@ when Guards@@@ -> Body@@@ end"),
		       begin
			   Loc = api_refac:start_end_loc(_This@),
			   LocString = get_loc_string(_This@),
			   ExprStx = hd(lists:flatten(wrangler_syntax:revert_forms(Expr@@))),

			   VarList = lists:flatten(lists:map(fun(G) -> api_refac:free_var_names(G) end, Guards@@@)),
			   VarListString = re:replace(lists:flatten(io_lib:format("~p", [VarList])),"'","",[{return,list},global]),

			   Declare = {case_expr,ExprStx,VarList,lists:zip(Pats@@@,Guards@@@)},
			   smother_server:declare(File,Loc,Declare),
			   ?TO_AST("begin EVal = Expr@@, VarList = [EVal | " ++ VarListString ++ "], smother_server:log(\"" ++ File ++ "\"," ++ LocString ++ ",VarList), case EVal of Pats@@@ when Guards@@@ -> Body@@@ end end")
		       end
		       ,true)
		], 
		[File]).

get_loc_string(_This@) ->
    Loc = api_refac:start_end_loc(_This@),
    lists:flatten(io_lib:format("~p", [Loc])).

