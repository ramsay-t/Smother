-module(smother).
-export([compile/1,compile/2,analyse/1,analyze/1,analyse_to_file/1,analyze_to_file/1,analyse_to_file/2,show_files/0]).

-export([var_server/1]).

-include("include/install.hrl").

compile(Filename) ->
    compile(Filename,[]).

compile(Filename,Includes) ->
    wrangler_ast_server:start_ast_server(),
    smother_server:clear(Filename),
    AST2 = instrument(Filename),
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

show_files() ->
    smother_server:show_files().

analyse(File) ->
    smother_server:analyse(File).

analyze(File) ->
    analyse(File).

analyse_to_file(File) ->
    smother_server:analyse_to_file(File).

analyze_to_file(File) ->
    analyse_to_file(File).

analyse_to_file(File,OutFile) ->
    smother_server:analyse_to_file(File,OutFile).

start_var_server() ->
    case global:whereis_name(smother_free_var_server) of
	undefined ->
	    PID = spawn_link(?MODULE,var_server,[1]),
	    global:register_name(smother_free_var_server, PID),
	    PID;
	PID ->
	    PID
    end.
    
next_free_var_number() ->
    VS = start_var_server(),
    VS ! {req, self()},
    receive
	V -> 
	    lists:flatten(io_lib:format("~p", [V]))
    end.

reset_var_server() ->
    VS = start_var_server(),
    VS ! reset.


var_server(N) ->
    receive 
	{req, From} ->
	    From ! N,
	    var_server(N+1);
	reset ->
	    var_server(1)
    end.

fix_range({Type,Thing,{attr,Loc,Attrs,End},Image},OldAttrs) ->
    Range = smother_analysis:get_range(OldAttrs),
    NewAttrs = {attr,Loc,lists:keystore(range,1,Attrs,{range,Range}),End},
    {Type,Thing,NewAttrs,Image}.

rules(File) ->
    [
     ?RULE(?T("f@(Args@@) when Guard@@ -> Body@@;"),
	   begin
	       %%io:format("FUN RULE HIT~n"),
	       %%ArgNames = get_arg_names(Args@@),
	       Loc = api_refac:start_end_loc(_This@),
	       LocString = get_loc_string(_This@),
	       FName = erl_parse:normalise(wrangler_syntax:revert(_W_f@)),
	       reset_var_server(),
	       NewArgs@@ = lists:map(fun(A) ->
					     case A of 
						 {wrapper,underscore,Attrs,_Image} ->
						     fix_range(?TO_AST("SMOTHER_UNDERSCORE_" ++ next_free_var_number()), Attrs);
						 _ ->
						     A 
					     end
				   end, Args@@),
	       Declare = {fun_case,FName,length(Args@@),Args@@,Guard@@},
	       smother_server:declare(File,Loc,Declare),
	       %%NewBody@@ = sub_instrument(Body@@,rules(File)),
	       ?TO_AST("f@(NewArgs@@) when Guard@@-> smother_server:log(\"" ++ File ++ "\"," ++ LocString ++ ",[NewArgs@@]), Body@@;")
	   end
	   ,true),
     ?RULE(?T("if Guards@@@ -> Body@@@ end"),
	   begin
	       %%io:format("IF RULE HIT~n"),
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
	       %%io:format("CASE RULE HIT~n"),
	       Loc = api_refac:start_end_loc(_This@),
	       LocString = get_loc_string(_This@),
	       ExprStx = hd(lists:flatten(wrangler_syntax:revert_forms(Expr@@))),
	       
	       VarList = lists:flatten(lists:map(fun(G) -> api_refac:free_var_names(G) end, Guards@@@)),
	       VarListString = re:replace(lists:flatten(io_lib:format("~p", [VarList])),"'","",[{return,list},global]),
	       
	       Declare = {case_expr,ExprStx,VarList,lists:zip(Pats@@@,Guards@@@)},
	       smother_server:declare(File,Loc,Declare),

	       ?TO_AST("begin EVal = Expr@@, VarList = [EVal | " ++ VarListString ++ "], smother_server:log(\"" ++ File ++ "\"," ++ LocString ++ ",VarList), case EVal of Pats@@@ when Guards@@@ -> Body@@@ end end")
	   end
	   ,true),
     ?RULE(?T("receive Pats@@@ when Guards@@@ -> Body@@@ end"),
	   begin
	       %%io:format("RECEIVE RULE HIT~n"),
	       Loc = api_refac:start_end_loc(_This@),
	       LocString = get_loc_string(_This@),
	       

	       {NewPats@@@,NewBody@@@} =lists:unzip( lists:map(
			    fun({{P@@,G@@},B@@}) ->
				    NewP@@ = [?TO_AST("P@@ = SMOTHER_REC_PATTERN")],
				    VarList = api_refac:free_var_names(G@@),

				    VarPairStringList = lists:map(fun(V) ->
									  {atom_to_list(V),V}
							    end,
							    VarList),

				    VarListString = re:replace(
						      re:replace(
							lists:flatten(io_lib:format("~p", [VarPairStringList]))
							,"'","",[{return,list},global]
						       ),"\"","'",[{return,list},global]),
				    {NewP@@,[?TO_AST("begin smother_server:log(\"" ++ File ++ "\"," ++ LocString ++ ",[SMOTHER_REC_PATTERN | " ++ VarListString ++ "]), B@@ end")]}
			    end,
			    lists:zip(lists:zip(Pats@@@,Guards@@@),Body@@@)
			   )),

	       Declare = {receive_expr,lists:zip(Pats@@@,Guards@@@)},
	       smother_server:declare(File,Loc,Declare),

	       ?TO_AST("receive NewPats@@@ when Guards@@@ -> NewBody@@@ end")
	   end
	   ,true)

    ].
	
instrument(File) ->
    {ok, AST} = api_refac:get_ast(File),
    sub_instrument(AST,rules(File)).

sub_instrument(AST,[]) ->
    AST;
sub_instrument(AST,[R | MoreRules]) ->
    %%io:format("APPLYING ~p RULES TO ~p~n~n",[length(MoreRules)+1,?PP(AST)]),
    {ok, AST2} = ?STOP_TD_TP([R],AST),
    %%io:format("MADE ~p~n~n",[?PP(AST2)]),
    sub_instrument(AST2,MoreRules).

get_loc_string(_This@) ->
    Loc = api_refac:start_end_loc(_This@),
    lists:flatten(io_lib:format("~p", [Loc])).

