-module(smother_eqc_eval).

-export([compare/4]).


compare(Module,EQC,Prop,NumTests) ->
    ComDetails = apply(Module,module_info,[compile]),
    {source,Source} = lists:keyfind(source,1,ComDetails),
    {options,Options} = lists:keyfind(options,1,ComDetails),

    Tests = eqc_suite:random(eqc:numtests(NumTests,apply(EQC,Prop,[]))),
    eqc_suite:write(Module,Tests),

    io:format("Smother compiling ~p~n",[Source]),
    smother:compile(Source,Options),
    SStart = now(),
    %%SRes = eqc:quickcheck(eqc:numtests(NumTests,apply(EQC,Prop,[]))),
    SRes = eqc_suite:run(apply(EQC,Prop,[]),Module),
    SEnd = now(),
    _Smother = smother:analyse(Module),
    smother:analyse_to_file(Module),
    SPerc = smother:get_percentage(Module),
    io:format("[~.2f sec] ~p:~p() -> ~p~nSmother: ~.2f%~n",[timer:now_diff(SEnd,SStart)/1000000,EQC,Prop,SRes,SPerc]),
    
    io:format("Cover compiling ~p~n",[Source]),
    cover:compile(Source,Options),
    CStart = now(),
    %%CRes = eqc:quickcheck(eqc:numtests(NumTests,apply(EQC,Prop,[]))),  
    CRes = eqc_suite:run(apply(EQC,Prop,[]),Module),
    CEnd = now(),
    _Cover = cover:analyse(Module),
    cover:analyse_to_file(Module,[html]),
    {ok, {Module,{Covered,TotalLines}}} = cover:analyse(Module,module),
    CPerc = (Covered * 100) / TotalLines,
    io:format("[~.2f sec] ~p:~p() -> ~p~nCover: ~.2f%~n",[timer:now_diff(CEnd,CStart)/1000000,EQC,Prop,CRes,CPerc]),
    
    {CPerc,SPerc,CRes,SRes}.
    %%{Cover,Smother}.
    %%ok.

