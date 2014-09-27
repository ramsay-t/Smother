-module(smother_eqc_eval).

-export([make_tests/5,compare/4,suite_name/3]).

suite_name(Module,NumTests,ID) ->
    list_to_atom(lists:flatten(io_lib:format("~p_~p_~p",[Module,NumTests,ID]))).

make_tests(Module,EQC,Prop,NumTests,ID) ->
    Name = suite_name(Module,NumTests,ID),
    GStart = now(),
    Tests = eqc_suite:random(eqc:numtests(NumTests,apply(EQC,Prop,[]))),
    eqc_suite:write(Name,Tests),
    GEnd = now(),
    io:format("<~p> Generated ~p tests in ~.2f sec.~n",[Name,NumTests,timer:now_diff(GEnd,GStart)/1000000]),
    Name.

compare(Module,EQC,Prop,Suite) ->
    io:format("Performing Cover vs Smother comparison on module ~p, with suite ~p~n",[Module,Suite]),
    ComDetails = apply(Module,module_info,[compile]),
    {source,Source} = lists:keyfind(source,1,ComDetails),
    {options,Options} = lists:keyfind(options,1,ComDetails),

    io:format("Smother compiling ~p~n",[Source]),
    smother:compile(Source,Options),
    SStart = now(),
    SRes = eqc_suite:run(apply(EQC,Prop,[]),Suite),
    SEnd = now(),
    _Smother = smother:analyse(Module),
    smother:analyse_to_file(Module),
    SPerc = smother:get_percentage(Module),
    io:format("[~.2f sec] ~p:~p() -> ~p~nSmother: ~.2f%~n",[timer:now_diff(SEnd,SStart)/1000000,EQC,Prop,SRes,SPerc]),
    
    io:format("Cover compiling ~p~n",[Source]),
    cover:compile(Source,Options),
    CStart = now(),
    CRes = eqc_suite:run(apply(EQC,Prop,[]),Suite),
    CEnd = now(),
    cover:analyse_to_file(Module,[html]),
    {ok, CLines} = cover:analyse(Module,coverage,line),
    Covered = lists:foldl(fun({_,{C,_}},Acc) -> Acc + C end, 0, CLines),
    CPerc = (Covered * 100) / length(CLines),
    io:format("[~.2f sec] ~p:~p() -> ~p~nCover: ~.2f%~n",[timer:now_diff(CEnd,CStart)/1000000,EQC,Prop,CRes,CPerc]),
    
    {CPerc,SPerc,CRes,SRes}.
    %%{Cover,Smother}.
    %%ok.


