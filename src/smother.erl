-module(smother).
-export([compile/1,compile/2,compile_batch/2,analyse/1,analyze/1,analyse_to_file/1,analyze_to_file/1,analyse_to_file/2,analyse_to_file_batch/1,show_files/0,get_zeros/1,get_nonzeros/1,get_split/1,get_percentage/1,reset/1,get_reports/1,mailbox_size/0,wait_for_logging_to_finish/0]).

-export([var_server/1]).

%% Escript API
-export([main/1]).

-include_lib("wrangler/include/wrangler.hrl").

-export([reset2/1]).
reset2(M) ->
    reset(M).

%% @doc Read the specified source file, insert instrumentation, and load the module.
%% All subsequent smother API calls should refer to the module name, rather than the source file.
compile(Module) when is_atom(Module) ->
    ComDetails = apply(Module,module_info,[compile]),
    {source,Source} = lists:keyfind(source,1,ComDetails),
    {options,Options} = lists:keyfind(options,1,ComDetails),
    compile(Source,Options);
compile(Filename) ->
    compile(Filename,[]).

%% @doc Read the specified source file, insert instrumentation, and load the module.
%% Options inclue {i,Folder} to include folders, and the atom preprocess to run the preprocessor
%% before analysing the source file. This will produce output based on the preprocssed source, which allows
%% analysis of decisions containing macros etc.
compile(Module, Options) when is_atom(Module) ->
    ComDetails = apply(Module,module_info,[compile]),
    {source,Source} = lists:keyfind(source,1,ComDetails),
    {options,ComOptions} = lists:keyfind(options,1,ComDetails),
    compile(Source,Options++ComOptions);
compile(Filename,Options) ->
    wrangler_ast_server:start_ast_server(),
    {ok, ModInfo} = api_refac:get_module_info(Filename),
    {module,ModName} = lists:keyfind(module,1,ModInfo),

    wrangler_ast_server:start_ast_server(),
    smother_server:init_file(ModName,Filename),
%%    smother_server:clear(ModName),

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
    {ok,Forms} = epp:parse_file(TmpFile,Includes,[{'TEST', true}]),

    smother_server:store_zero(),

    case compile:forms(Forms,[binary,debug_info,verbose,report_errors,report_warnings]) of
	{ok,Module,Binary} ->
	    code:load_binary(Module,TrueFile,Binary);
	Error ->
	    Error
    end.

%% @doc Compiles several files, returning 2 lists: successes and errors
compile_batch(Files, Options) ->
    lists:foldl(fun (F, {Succ, Err}) ->
                case compile(F, Options) of
                    {module, M} -> {[M|Succ], Err};
                    error -> {Succ, [{F, error}|Err]};
                    {error, _E, _W}=E -> {Succ, [{F, E}|Err]}
                end
        end, {[], []}, Files).

%% @doc List all modules currently being instrumented.
show_files() ->
    wait_for_logging_to_finish(),
    smother_server:show_files().

%% @doc Access the analysis tree structure for a particular module.
analyse(Module) ->
    wait_for_logging_to_finish(),
    smother_server:analyse(Module).

%% @doc Access the analysis tree structure for a particular module.
analyze(Module) ->
    analyse(Module).

%% @doc Produce an annotated HTML file showing MC/DC information.
analyse_to_file(Module) ->
    wait_for_logging_to_finish(),
    smother_server:analyse_to_file(Module).

%% @doc Produce an annotated HTML file showing MC/DC information.
analyze_to_file(Module) ->
    analyse_to_file(Module).

%% @doc Produce an annotated HTML file showing MC/DC information.
%% OutFile specifies the output file name.
analyse_to_file(Module,OutFile) ->
    wait_for_logging_to_finish(),
    smother_server:analyse_to_file(Module,OutFile).

%% @doc Runs analysis on several modules, returning 2 lists: successes and errors
analyse_to_file_batch(Modules) ->
    lists:foldl(fun (M, {Succ, Err}) ->
                case analyse_to_file(M) of
                    {ok, Report} -> {[Report|Succ], Err};
                    {error, _}=E -> {Succ, [{M, E}|Err]}
                end
        end, {[], []}, Modules).

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

%% @hidden
get_useful_args(A) ->
    case A of
	{tree,match_expr,_Attrs,{match_expr, Left,Right}} ->
	    case Left of
		{wrapper,variable,VAttrs,Image} ->
		    {wrapper,variable,VAttrs,Image};
		_ ->
		    case Right of
			{wrapper,variable,VAttrs,Image} ->
			    {wrapper,variable,VAttrs,Image};
			_ ->
			    A
		    end
	    end;
	_ ->
	    A
    end.

%% @private
%% @doc The mutation rules to insert instrumentation and "declare" the analysis point to the server.
rules(Module) ->
    [
     ?RULE(?T("f@(Args@@) when Guard@@ -> Body@@;"),
	   begin
	       %%ArgNames = get_arg_names(Args@@),
	       Loc = api_refac:start_end_loc(_This@),
	       %%io:format("FUN RULE HIT ~p~n",[Loc]),
	       put(smother_instrumented,[Loc | get(smother_instrumented)]),
	       LocString = get_loc_string(_This@),
	       FName = erl_parse:normalise(wrangler_syntax:revert(_W_f@)),
	       reset_var_server(),
	       NewArgs@@ = lists:map(fun rename_underscores/1, Args@@),
	       OnlyUsefulArgs@@ = lists:map(fun get_useful_args/1, NewArgs@@),
	       Declare = {fun_case,FName,length(Args@@),Args@@,Guard@@},
	       smother_server:declare(Module,Loc,Declare),
	       %%NewBody@@ = sub_instrument(Body@@,rules(Module)),
	       %%io:format("f NewBody@@: ~p~n", [length(NewBody@@)]),
	       ?TO_AST("f@(NewArgs@@) when Guard@@-> smother_server:log(" ++ atom_to_list(Module) ++ "," ++ LocString ++ ",[OnlyUsefulArgs@@]), Body@@;")
	   end
	   ,(api_refac:type(_This@)/=attribute) and not lists:member(api_refac:start_end_loc(_This@), get(smother_instrumented)) and not (api_refac:start_end_loc(_This@) == {{0,0},{0,0}})),
     ?RULE(?T("if Guards@@@ -> Body@@@ end"),
	   begin
	       Loc = api_refac:start_end_loc(_This@),
	       %%io:format("IF RULE HIT at ~p~n",[Loc]),
	       put(smother_instrumented,[Loc | get(smother_instrumented)]),
	       LocString = get_loc_string(_This@),
	       %%GuardList@@@ = lists:flatten(lists:map(fun(G) -> io:format("G: ~p~n", [G]), wrangler_syntax:revert_forms(G) end, Guards@@@)),
	       VarList = lists:flatten(lists:map(fun(G) -> api_refac:free_var_names(G) end, Guards@@@)),
	       VarListString = re:replace(lists:flatten(io_lib:format("~p", [VarList])),"'","",[{return,list},global]),
	       Declare = {if_expr,VarList,Guards@@@},
	       smother_server:declare(Module,Loc,Declare),
	       %%NewBody@@@ = lists:map(fun(B) -> sub_instrument(B,rules(Module)) end, Body@@@),
	       %%io:format("If NewBody@@@: ~p~n",[length(NewBody@@@)]),
	       ?TO_AST("begin smother_server:log(" ++ atom_to_list(Module) ++ "," ++ LocString ++ "," ++ VarListString ++ "), if Guards@@@ -> Body@@@ end end")
	   end
	   ,(api_refac:type(_This@)/=attribute) and not lists:member(api_refac:start_end_loc(_This@), get(smother_instrumented)) and not (api_refac:start_end_loc(_This@) == {{0,0},{0,0}})),
     ?RULE(?T("case Expr@@ of Pats@@@ when Guards@@@ -> Body@@@ end"),
	   begin
	       Loc = api_refac:start_end_loc(_This@),
	       %%io:format("CASE RULE HIT at ~p~n",[Loc]),
	       put(smother_instrumented,[Loc | get(smother_instrumented)]),
	       LocString = get_loc_string(_This@),
	       %%ExprStx = hd(lists:flatten(wrangler_syntax:revert_forms(Expr@@))),

	       %% Guard evaluation needs the maximal list of free variables so that guards for other lines
	       %% can be evaluated
	       VarList = api_refac:free_var_names(_This@),
	       VarPairStringList = lists:map(fun(V) ->
						     {atom_to_list(V),V}
					     end,
					     VarList),
	       VarListString = re:replace(
				 re:replace(
				   lists:flatten(io_lib:format("~p", [VarPairStringList]))
				   ,"'","",[{return,list},global]
				  ),"\"","'",[{return,list},global]),
	       %%io:format("~s~n",[VarListString]),
	       {NewPats@@@,NewBody@@@} =lists:unzip( lists:map(
			    fun({{P@@,G@@},B@@}) ->
				    CP = next_free_var_number(),
				    NewP@@ = [?TO_AST("P@@ = SMOTHER_CASE_PATTERN" ++ CP)],
				    %%NewB@@ = sub_instrument(B@@,rules(Module)),
				    {NewP@@,[?TO_AST("begin smother_server:log(" ++ atom_to_list(Module) ++ "," ++ LocString ++ ",[SMOTHER_CASE_PATTERN" ++ CP ++ " | " ++ VarListString ++ "]), B@@ end")]}
			    end,
			    lists:zip(lists:zip(Pats@@@,Guards@@@),Body@@@)
			   )),

	       Declare = {case_expr,lists:zip(Pats@@@,Guards@@@)},
	       smother_server:declare(Module,Loc,Declare),
	       %%io:format("case NewBody@@@: ~p~n",[length(NewBody@@@)]),
	       ?TO_AST("case Expr@@ of NewPats@@@ when Guards@@@ -> NewBody@@@ end")
	   end
	   ,(api_refac:type(_This@)/=attribute) and not lists:member(api_refac:start_end_loc(_This@), get(smother_instrumented)) and not (api_refac:start_end_loc(_This@) == {{0,0},{0,0}})),
     ?RULE(?T("receive Pats@@@ when Guards@@@ -> Body@@@ end"),
	   begin
	       Loc = api_refac:start_end_loc(_This@),
	       %%io:format("RECEIVE RULE HIT at ~p~n", [Loc]),
	       put(smother_instrumented,[Loc | get(smother_instrumented)]),
	       LocString = get_loc_string(_This@),

	       {NewPats@@@,NewBody@@@} =lists:unzip( lists:map(
			    fun({{P@@,G@@},B@@}) ->
				    CP = next_free_var_number(),
				    NewP@@ = [?TO_AST("P@@ = SMOTHER_REC_PATTERN" ++ CP)],
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

				    %%NewB@@ = sub_instrument(B@@,rules(Module)),

				    {NewP@@,[?TO_AST("begin smother_server:log(" ++ atom_to_list(Module) ++ "," ++ LocString ++ ",[SMOTHER_REC_PATTERN" ++ CP ++ " | " ++ VarListString ++ "]), B@@ end")]}
			    end,
			    lists:zip(lists:zip(Pats@@@,Guards@@@),Body@@@)
			   )),

	       Declare = {receive_expr,lists:zip(Pats@@@,Guards@@@)},
	       smother_server:declare(Module,Loc,Declare),
	       %%io:format("Receive NewBody@@@: ~p~n",[length(NewBody@@@)]),
	       ?TO_AST("receive NewPats@@@ when Guards@@@ -> NewBody@@@ end")
	   end
	   ,(api_refac:type(_This@)/=attribute) and not lists:member(api_refac:start_end_loc(_This@), get(smother_instrumented)) and not (api_refac:start_end_loc(_This@) == {{0,0},{0,0}}))

    ].

%% @private
%% @doc Apply the instrumentation/analysis rules.
instrument(MName,File) ->
    put(smother_instrumented,[]),
    {ok, AST} = api_refac:get_ast(File),
    sub_instrument(AST,rules(MName)).

%% @hidden
sub_instrument(AST,[]) ->
    AST;
sub_instrument(AST,[R | MoreRules]) ->
    %%io:format("APPLYING ~p RULES~n",[length(MoreRules)+1]),
    {ok, AST2} = ?FULL_TD_TP([R],AST),
    %%io:format("MADE ~p~n~n",[?PP(AST2)]),
    sub_instrument(AST2,MoreRules).

%% @hidden
get_loc_string(_This@) ->
    Loc = api_refac:start_end_loc(_This@),
    lists:flatten(io_lib:format("~p", [Loc])).

%% @doc Produce a list of MC/DC tree leaves that have not been covered.
get_zeros(Module) ->
    wait_for_logging_to_finish(),
    smother_server:get_zeros(Module).
%% @doc Produce a list of MC/DC tree leaves that have been covered.
%% This is the complement of get_zeros(Module).
get_nonzeros(Module) ->
    wait_for_logging_to_finish(),
    smother_server:get_nonzeros(Module).
%% @doc Show the counts of zeros and non-zeros.
%% Produces a pair of integers, the first being the count of uncovered MC/DC tree leaves,
%% the second is the count of covered MC/DC leaves.
%% This gives a useful, quick measure of coverage in both percentage and absolute terms.
get_split(Module) ->
    wait_for_logging_to_finish(),
    smother_server:get_split(Module).
%% @doc Returns the percentage of coverage.
%% Calculated as the size of the list returned by get_nonzeros, against the sum of that list
%% plus the list of zeros.
get_percentage(Module) ->
    wait_for_logging_to_finish(),
    smother_server:get_percentage(Module).
%% @doc Clears all analysis data for the specified module.
reset(Module) ->
    wait_for_logging_to_finish(),
    smother_server:reset(Module).

%% @hidden
make_pp_file(Filename,Includes) ->
    {ok, ModInfo} = api_refac:get_module_info(Filename),
    {module,ModName} = lists:keyfind(module,1,ModInfo),

    {ok, Cont} = epp:parse_file(Filename,Includes,[{d, 'TEST'}]),
    Code = lists:flatten([erl_prettypr:format(C) ++ "\n" || C <- Cont]),

    FName = smother_annotater:get_tmp() ++ atom_to_list(ModName) ++ ".epp",
    %%io:format("Making ~p~n",[FName]),
    file:write_file(FName,Code++"\n"),
    FName.

%% @doc Produces readable analysis reports.
%% This returns a list of analysis_report records for each of the program analysis points.
%% analysis_report sub-components, such as matchedsubs, contain further analysis_report records
%% and the analysis_report record itself contains context information.
get_reports(Module) ->
    wait_for_logging_to_finish(),
    case analyse(Module) of
	{ok,FDict} ->
	    smother_analysis:get_reports(FDict);
	{error,Reason} ->
	    {error,Reason}
    end.

mailbox_size() ->
    case global:whereis_name(smother_server) of
	undefined ->
	    {error, smother_not_started};
	PID ->
	    element(2,hd(erlang:process_info(PID,[message_queue_len])))
    end.

%% @doc Waits for all log messages to be processed.
%% Instrumentation can generate massive numbers of logging messages, so even after testing is complete
%% it can take some time for the smother_server mailbox to empty. This function will check the mailbox size
%% and wait until it empties. This is useful, since any functions that rely on the server will result in
%% gen_server call timeouts if the get stuck in the mailbox queue for too long.
wait_for_logging_to_finish() ->
   wait_for_logging(mailbox_size()).

measure_mailbox_speed() ->
    First = mailbox_size(),
    timer:sleep(100),
    Second = mailbox_size(),
    (First - Second) * 10.

wait_for_logging({error,smother_not_started}) ->
    {error,smother_not_started};
wait_for_logging(MBS) ->
    if MBS =< 0 ->
	    ok;
       MBS < 10000 ->
	    timer:sleep(50),
	    wait_for_logging(mailbox_size());
       true ->
	    MPS = measure_mailbox_speed(),
	    TotSec = MBS / MPS,
	    Mins = trunc(TotSec / 60),
	    Sec = TotSec - (Mins * 60),
	    io:format("Waiting for the smother_server mailbox to clear...[~p messages, ~p msg/s - ~p mins ~.2f sec]~n",[MBS,MPS,Mins,Sec]),
	    %% Wait one tenth of the expected time...
	    timer:sleep(trunc(TotSec * 100)),
	    wait_for_logging(mailbox_size())
    end.

%% @hidden
main(Args) ->
    smother_escript:main(Args).

