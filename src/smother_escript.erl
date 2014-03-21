-module(smother_escript).

-export([main/1]).

%% @doc Acts as the main interface for a self-contained script capable of running smother analysys
%% for rebar based projects. This method can be easily extended to support generic path inclusions
%% instead of asuming the existence of a classic rebar repo structure with a deps folder
%%
%% The following directory structure is asumed in the indicated base dir
%% .
%% ├── include
%% │   └── inclusions.hrl
%% ├── src
%% │   ├── module1.erl
%% │   └── module2.erl
%% ├── deps
%% └── test
%%     ├── tests1.erl
%% @todo Add support for the preprocess option
main([]) ->
    main([file:get_cwd()]);
main([BaseDir]) ->
    main([BaseDir, filename:join([BaseDir, ".eunit", "smother"])]);
main([BaseDir, DestDir]) ->
    % Create DestDir/README
    TestFile = filename:join([DestDir, "README"]),
    ok = filelib:ensure_dir(TestFile),
    ok = file:write_file(TestFile, io_lib:format("~ts~n", [smother_dir_readme()])),
    % Locate code, deps and includes
    SrcModules = get_modules(BaseDir, "src", ".erl") ++ get_modules(BaseDir, "test", ".erl"),
    Includes = [{i, D} || D <- filelib:wildcard(filename:join([BaseDir, "deps", "*", "include"]))]
               ++ [{i, filename:join([BaseDir, "include"])}],
    Paths = [filename:join([BaseDir, "ebin"])
            | filelib:wildcard(filename:join([BaseDir, "deps", "*", "ebin"]))],
    ok = code:add_paths(Paths),
    % smother compile code
    case smother:compile_batch(SrcModules, Includes) of
        {Modules, []} ->
            % Run the analysys and generate the report
            Tests = lists:filter(fun (M) -> erlang:function_exported(M, test, 0) end, Modules),
            ok = lists:foreach(fun (M) -> io:format("~nTesting ~p:~n", [M]), apply(M, test, []) end, Tests),
            ok = run_analysis(Modules, DestDir),
            io:format("~nDone!~n", []),
            halt(0);
        {_, L} when L/=[] ->
            io:format("Errors were found during compilation!~n~p~n", [L]),
            halt(1)
    end;
main(_) ->
    help().

help() ->
    io:format("Usage: ./smother <BaseProjectDir> [DestDir]~n", []),
    halt(1).

smother_dir_readme() ->
    <<"This folder contains [Smother](https://github.com/ramsay-t/Smother) compiled code.">>.

get_modules(BaseDir, Dir, Extension) ->
    [filename:join([BaseDir, Dir, F])
     || F <- list_dir(filename:join([BaseDir, Dir])), lists:suffix(Extension, F)].

run_analysis(Modules, DestDir) ->
    {Reports, Errors} = smother:analyse_to_file_batch(Modules),
    io:format("~nGenerating results:~n", []),
    [io:format("Error in analysis: ~p~n", [E]) || E <- Errors],
    ok = lists:foreach(fun(R) ->
                ReportDest = filename:join([DestDir, R]),
                {ok, _} = file:copy(R, ReportDest),
                ok = file:delete(R),
                io:format("~p~n", [ReportDest])
        end, Reports).

list_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} -> Files;
        {error, enoent} -> []
    end.


