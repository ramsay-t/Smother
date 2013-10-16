-module(smother).
-export([compile/1,compile/2,analyse/1,analyze/1,analyse_to_file/1,analyze_to_file/1,analyse_to_file/2,show_files/0,get_zeros/1,get_nonzeros/1,get_split/1,get_percentage/1,reset/1,get_reports/1]).

-export([var_server/1]).

-include_lib("wrangler/include/wrangler.hrl").

%% @doc Read the specified source file, insert instrumentation, and load the module.
%% All subsequent smother API calls should refer to the module name, rather than the source file.
compile(Filename) ->
    compile(Filename,[]).

%% @doc Read the specified source file, insert instrumentation, and load the module.
%% Options inclue {i,Folder} to include folders, and the atom preprocess to run the preprocessor
%% before analysing the source file. This will produce output based on the preprocssed source, which allows
%% analysis of decisions containing macros etc.
compile(Filename,Options) ->
    {ok, ModInfo} = api_refac:get_module_info(Filename),
    {module,ModName} = lists:keyfind(module,1,ModInfo),

    wrangler_ast_server:start_ast_server(),
    smother_server:clear(ModName),

    Includes = [I || {i,I} <- Options],
    TrueFile = case lists:filter(fun(O) -> O == preprocess end, Options) of
		   [] ->
		       Filename;
		   _ ->
		       make_pp_file(Filename,Includes)
	       end,

    AST2 = instrument(ModName,TrueFile),
    Code = wrangler_prettypr:print_ast('unix',AST2),

    TmpFile = smother_annotater:make_tmp_file(ModName,Code),
    {ok,Forms} = epp:parse_file(TmpFile,Includes,[]),

    smother_server:store_zero(),

    case compile:forms(Forms,[binary,debug_info,verbose,report_errors,report_warnings]) of
	{ok,Module,Binary} ->
	    code:load_binary(Module,TrueFile,Binary);
	Error ->
	    Error
    end.

%% @doc List all modules currently being instrumented.
show_files() ->
    smother_server:show_files().

%% @doc Access the analysis tree structure for a particular module.
analyse(Module) ->
    smother_server:analyse(Module).

%% @doc Access the analysis tree structure for a particular module.
analyze(Module) ->
    analyse(Module).

%% @doc Produce an annotated HTML file showing MC/DC information.
analyse_to_file(Module) ->
    smother_server:analyse_to_file(Module).

%% @doc Produce an annotated HTML file showing MC/DC information.
analyze_to_file(Module) ->
    analyse_to_file(Module).

%% @doc Produce an annotated HTML file showing MC/DC information.
%% OutFile specifies the output file name.
analyse_to_file(Module,OutFile) ->
    smother_server:analyse_to_file(Module,OutFile).

%% @hidden
start_var_server() ->
    case global:whereis_name(smother_free_var_server) of
	undefined ->
	    PID = spawn_link(?MODULE,var_server,[1]),
	    global:register_name(smother_free_var_server, PID),
	    PID;
	PID ->
	    PID
    end.

%% @hidden
next_free_var_number() ->
    VS = start_var_server(),
    VS ! {req, self()},
    receive
	V -> 
	    lists:flatten(io_lib:format("~p", [V]))
    end.

%% @hidden
reset_var_server() ->
    VS = start_var_server(),
    VS ! reset.

%% @hidden
var_server(N) ->
    receive 
	{req, From} ->
	    From ! N,
	    var_server(N+1);
	reset ->
	    var_server(1)
    end.

%% @ hidden
fix_range({Type,Thing,{attr,Loc,Attrs,End},Image},OldAttrs) ->
    Range = smother_analysis:get_range(OldAttrs),
    NewAttrs = {attr,Loc,lists:keystore(range,1,Attrs,{range,Range}),End},
    {Type,Thing,NewAttrs,Image}.

%% @hidden
rename_underscores(A) ->
    case A of 
	{wrapper,underscore,Attrs,_Image} ->
	    fix_range(?TO_AST("SMOTHER_UNDERSCORE_" ++ next_free_var_number()), Attrs);
	{tree,tuple,Attrs,Contents} ->
	    {tree,tuple,Attrs,lists:map(fun rename_underscores/1,Contents)};
	{tree,list,Attrs,{list,Contents,Tail}} ->
	    {tree,list,Attrs,{list,lists:map(fun rename_underscores/1,Contents),rename_underscores(Tail)}};
	_ ->
	    A 
    end.

%% @private
%% @doc The mutation rules to insert instrumentation and "declare" the analysis point to the server.
rules(Module) ->
    [
     ?RULE(?T("f@(Args@@) when Guard@@ -> Body@@;"),
	   begin
	       %%io:format("FUN RULE HIT~n"),
	       %%ArgNames = get_arg_names(Args@@),
	       Loc = api_refac:start_end_loc(_This@),
	       LocString = get_loc_string(_This@),
	       FName = erl_parse:normalise(wrangler_syntax:revert(_W_f@)),
	       reset_var_server(),
	       NewArgs@@ = lists:map(fun rename_underscores/1, Args@@),
	       Declare = {fun_case,FName,length(Args@@),Args@@,Guard@@},
	       smother_server:declare(Module,Loc,Declare),
	       %%NewBody@@ = sub_instrument(Body@@,rules(Module)),
	       ?TO_AST("f@(NewArgs@@) when Guard@@-> smother_server:log(" ++ atom_to_list(Module) ++ "," ++ LocString ++ ",[NewArgs@@]), Body@@;")
	   end
	   ,api_refac:type(_This@)/=attribute),
     ?RULE(?T("if Guards@@@ -> Body@@@ end"),
	   begin
	       %%io:format("IF RULE HIT~n"),
	       Loc = api_refac:start_end_loc(_This@),
	       LocString = get_loc_string(_This@),
	       %%GuardList@@@ = lists:flatten(lists:map(fun(G) -> io:format("G: ~p~n", [G]), wrangler_syntax:revert_forms(G) end, Guards@@@)),
	       VarList = lists:flatten(lists:map(fun(G) -> api_refac:free_var_names(G) end, Guards@@@)),
	       VarListString = re:replace(lists:flatten(io_lib:format("~p", [VarList])),"'","",[{return,list},global]),
	       Declare = {if_expr,VarList,Guards@@@},
	       smother_server:declare(Module,Loc,Declare),
	       ?TO_AST("begin smother_server:log(" ++ atom_to_list(Module) ++ "," ++ LocString ++ "," ++ VarListString ++ "), if Guards@@@ -> Body@@@ end end")



	   end
	   ,api_refac:type(_This@)/=attribute),
     ?RULE(?T("case Expr@@ of Pats@@@ when Guards@@@ -> Body@@@ end"),
	   begin
	       %%io:format("CASE RULE HIT~n"),
	       Loc = api_refac:start_end_loc(_This@),
	       LocString = get_loc_string(_This@),
	       %%ExprStx = hd(lists:flatten(wrangler_syntax:revert_forms(Expr@@))),
	       
	       {NewPats@@@,NewBody@@@} =lists:unzip( lists:map(
			    fun({{P@@,G@@},B@@}) ->
				    NewP@@ = [?TO_AST("P@@ = SMOTHER_CASE_PATTERN")],
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
				    {NewP@@,[?TO_AST("begin smother_server:log(" ++ atom_to_list(Module) ++ "," ++ LocString ++ ",[SMOTHER_CASE_PATTERN | " ++ VarListString ++ "]), B@@ end")]}
			    end,
			    lists:zip(lists:zip(Pats@@@,Guards@@@),Body@@@)
			   )),

	       Declare = {case_expr,lists:zip(Pats@@@,Guards@@@)},
	       smother_server:declare(Module,Loc,Declare),

	       ?TO_AST("case Expr@@ of NewPats@@@ when Guards@@@ -> NewBody@@@ end")
	   end
	   ,api_refac:type(_This@)/=attribute),
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
				    {NewP@@,[?TO_AST("begin smother_server:log(" ++ atom_to_list(Module) ++ "," ++ LocString ++ ",[SMOTHER_REC_PATTERN | " ++ VarListString ++ "]), B@@ end")]}
			    end,
			    lists:zip(lists:zip(Pats@@@,Guards@@@),Body@@@)
			   )),

	       Declare = {receive_expr,lists:zip(Pats@@@,Guards@@@)},
	       smother_server:declare(Module,Loc,Declare),

	       ?TO_AST("receive NewPats@@@ when Guards@@@ -> NewBody@@@ end")
	   end
	   ,api_refac:type(_This@)/=attribute)

    ].
	
%% @private
%% @doc Apply the instrumentation/analysis rules.
instrument(MName,File) ->
    {ok, AST} = api_refac:get_ast(File),
    sub_instrument(AST,rules(MName)).

%% @hidden
sub_instrument(AST,[]) ->
    AST;
sub_instrument(AST,[R | MoreRules]) ->
    %%io:format("APPLYING ~p RULES TO ~p~n~n",[length(MoreRules)+1,?PP(AST)]),
    {ok, AST2} = ?STOP_TD_TP([R],AST),
    %%io:format("MADE ~p~n~n",[?PP(AST2)]),
    sub_instrument(AST2,MoreRules).

%% @hidden
get_loc_string(_This@) ->
    Loc = api_refac:start_end_loc(_This@),
    lists:flatten(io_lib:format("~p", [Loc])).

%% @doc Produce a list of MC/DC tree leaves that have not been covered.
get_zeros(Module) ->
    smother_server:get_zeros(Module).
%% @doc Produce a list of MC/DC tree leaves that have been covered.
%% This is the complement of get_zeros(Module).
get_nonzeros(Module) ->
    smother_server:get_nonzeros(Module).
%% @doc Show the counts of zeros and non-zeros.
%% Produces a pair of integers, the first being the count of uncovered MC/DC tree leaves, 
%% the second is the count of covered MC/DC leaves. 
%% This gives a useful, quick measure of coverage in both percentage and absolute terms.
get_split(Module) ->
    smother_server:get_split(Module).
%% @doc Returns the percentage of coverage.
%% Calculated as the size of the list returned by get_nonzeros, against the sum of that list
%% plus the list of zeros. 
get_percentage(Module) ->
    smother_server:get_percentage(Module).
%% @doc Clears all analysis data for the specified module.
reset(Module) ->
    smother_server:reset(Module).

%% @hidden
make_pp_file(Filename,Includes) ->
    {ok, ModInfo} = api_refac:get_module_info(Filename),
    {module,ModName} = lists:keyfind(module,1,ModInfo),

    {ok, Cont} = epp:parse_file(Filename,Includes,[]),
    Code = lists:flatten([erl_prettypr:format(C) ++ "\n" || C <- Cont]),
    
    FName = smother_annotater:get_tmp() ++ atom_to_list(ModName) ++ ".epp",
    io:format("Making ~p~n",[FName]),
    file:write_file(FName,Code++"\n"),
    FName.

%% @doc Produces readable analysis reports.
%% This returns a list of analysis_report records for each of the program analysis points.
%% analysis_report sub-components, such as matchedsubs, contain further analysis_report records
%% and the analysis_report record itself contains context information. 
get_reports(Module) ->
    {ok,FDict} = analyse(Module),
    smother_analysis:get_reports(FDict).

    
