-module(smother).
-export([compile/1,analyse/1,analyze/1,analyse_to_file/1,analyze_to_file/1]).

-include("include/install.hrl").

compile(Filename) ->
    wrangler_ast_server:start_ast_server(),
    smother_server:clear(Filename),
    %%{ok,AST} = api_refac:get_ast(Filename),
    {ok, [{{Filename, Filename}, AST2}]} = instrument(Filename),
    Code = ?PP(AST2),
    %%io:format("Transformed code to~n~s~n", [Code]),
    {MFs,EFs,FTs} = smother_annotater:get_forms(Code),
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

			   GuardList = lists:map(fun(G) -> 
							 case wrangler_syntax:revert_forms(G) of 
							     [] -> {atom,0,true}; 
							     GG -> GG 
							 end 
						 end, Guards@@@),
			   PatList = lists:map(fun(G) -> 
						       hd(wrangler_syntax:revert_forms(G))
					       end, Pats@@@),

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

get_arg_names(Args@@) ->
    lists:flatten("[" ++ "\"" ++ re:replace(?PP(Args@@), ",", "\",\"", [{return,list},global]) ++ "\"" ++ "]").

