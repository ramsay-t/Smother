-module(smother_eqc_eval).

-export([make_tests/6,compare/4,compare_n/8,suite_name/4]).

suite_name(Module,NumTests,ID,SuiteFolder) ->
    filename:join(SuiteFolder,lists:flatten(io_lib:format("~p_~p_~p",[Module,NumTests,ID]))).

make_tests(Module, EQC, Prop, NumTests, ID, SuiteFolder) ->
    GStart = now(),
    {random,Tests} = eqc_suite:random(eqc:numtests(NumTests,apply(EQC,Prop,[]))),
    GEnd = now(),
    Name = suite_name(Module,NumTests,ID,SuiteFolder),
    io:format("Generated ~p tests in ~.2f sec.~n",[length(Tests),timer:now_diff(GEnd,GStart)/1000000]),
    eqc_suite:write(Name,{random,Tests}),
    {ok,length(Tests)}.

compare_n(Module,EQC,Prop,Start,End,Step,ID,SuiteFolder) ->
    SuiteFile = suite_name(Module,End,ID,SuiteFolder) ++ ".suite",
    io:format("Will use suite file: ~p~n",[SuiteFile]),
    case filelib:is_file(SuiteFile) of
	true ->
	    ok;
	false ->
	    make_tests(Module,EQC,Prop,End,ID,SuiteFolder)
    end,
    {ok, Binary} = file:read_file(SuiteFile),
    {random,Tests} = binary_to_term(Binary),
    lists:map(fun(N) ->
		      {SubTests,_} = lists:split(N,Tests),
		      {CPerc,SPerc,COut,SOut} = compare(Module,EQC,Prop,{random,SubTests}),
		      CSV = lists:flatten(io_lib:format("~w,~w,~w,~w,~w~n",[N,CPerc,SPerc,COut,SOut])),
		      FileName = lists:flatten(io_lib:format("~w-~w-~w-~w_stat_results.csv",[Module,EQC,Prop,N])),
		      file:write_file(FileName,CSV,[append])
	      end,
	      lists:seq(Start,length(Tests),Step)).

compare(Module,EQC,Prop,Suite) ->
    %%io:format("Performing Cover vs Smother comparison on module ~p, with suite ~p~n",[Module,Suite]),
    ComDetails = apply(Module,module_info,[compile]),
    {source,Source} = lists:keyfind(source,1,ComDetails),
    {options,Options} = lists:keyfind(options,1,ComDetails),

    %%%%io:format("Smother compiling ~p~n",[Source]),
    smother:compile(Source,Options),
    _SStart = now(),
    SRes = eqc_suite:run(apply(EQC,Prop,[]),Suite),
    _SEnd = now(),
    _Smother = smother:analyse(Module),
    smother:analyse_to_file(Module),
    SPerc = smother:get_percentage(Module),
    %%io:format("[~.2f sec] ~p:~p() -> ~p~nSmother: ~.2f%~n",[timer:now_diff(SEnd,SStart)/1000000,EQC,Prop,SRes,SPerc]),
    
    %%io:format("Cover compiling ~p~n",[Source]),
    cover:compile(Source,Options),
    _CStart = now(),
    CRes = eqc_suite:run(apply(EQC,Prop,[]),Suite),
    _CEnd = now(),
    cover:analyse_to_file(Module,[html]),
    {ok, CLines} = cover:analyse(Module,coverage,line),
    Covered = lists:foldl(fun({_,{C,_}},Acc) -> Acc + C end, 0, CLines),
    CPerc = (Covered * 100) / length(CLines),
    %%io:format("[~.2f sec] ~p:~p() -> ~p~nCover: ~.2f%~n",[timer:now_diff(CEnd,CStart)/1000000,EQC,Prop,CRes,CPerc]),
    

    %% Outputting the complete error trace makes this a bit huge for big test sets...
    COut = case CRes of
	       [] ->
		   [];
	       _ ->
		   [failed]
	   end,
    SOut = case SRes of
	       [] ->
		   [];
	       _ ->
		   [failed]
	   end,


    {CPerc,SPerc,COut,SOut}.
    %%{Cover,Smother}.
    %%ok.


