-module(smother_analysis).
-export([get_range/1,get_zeros/1,get_nonzeros/1,get_percentage/1,get_reports/1,exp_printer/1]).
-export([json_reports/1,report_to_json/1,make_html_json_analysis/3]).
-include_lib("wrangler/include/wrangler.hrl").
-include("include/eval_records.hrl").
-include("include/analysis_reports.hrl").

make_html_json_analysis(File,FDict,Outfile) ->
    OutPath = filename:dirname(Outfile),

    %% Ok, this does assume that Smother was compiled from source and the source dir hasn't been moved...
    ComDetails = apply(smother,module_info,[compile]),
    {source,SmotherSource} = lists:keyfind(source,1,ComDetails),
    SmotherPath = filename:dirname(SmotherSource),
    SupportPath = SmotherPath ++ "/../reports/",
    lists:map(fun(F) -> 
		      file:copy(SupportPath ++ "/" ++ F,OutPath ++ "/" ++ F)
	      end,
	      ["smother.js","smother.css"]), 

    Reports = get_reports(FDict),
    io:format("~p reports:~n~p~n~n",[length(Reports),
				     lists:map(fun(#analysis_report{loc=Loc,exp=Exp}) -> {Loc,exp_printer(Exp)} end,lists:sort(fun loc_sort/2, Reports))
				    ]),
    case file:open(Outfile, [write]) of
	{ok, OF} ->
	    io:fwrite(OF,"<html>
<head>
<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js\"></script>
<script src=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js\"></script>
<link rel=\"stylesheet\" type=\"text/css\" href=\"smother.css\">
<script src=\"smother.js\"></script>
<script type=\"text/javascript\">
reports = ",[]),

    JSON = json_reports(Reports),
    io:fwrite(OF,JSON,[]),

    io:fwrite(OF,"
</script>
</head>
<body>
<div id=\"container\">
<div id=\"info\">
<h2 id=\"infoexp\"></h2>
<div>
<ul>
<li>matched: <span id=\"infomatched\">0</span></li>
<li>non-matched: <span id=\"infononmatched\">0</span></li>
</ul>
</div>
<div id=\"infomatchedsubs\"></div>
<div id=\"infononmatchedsubs\"></div>
<div id=\"infocomment\"></div>
</div><!-- info -->
<div id=\"code\">
<pre>
",[]),

    {ok,Bin} = file:read_file(File),
    Chars = binary_to_list(Bin), 
    ok = analyse_to_html(Chars,OF,Reports,{1,1}),
    io:fwrite(OF,"
</pre>
</div><!-- code -->
</div><!-- container -->
</body>
</html>
",[]),
		   file:close(OF),
                   ok;
		Error ->
                  {error, Error}
            end.

analyse_to_html([],_OF,_Reports,{_FLocLine,_FLocChar} = _FLoc) ->
    ok;
analyse_to_html(Chars,OF,Reports,{FLocLine,FLocChar} = FLoc) ->
    case hd(Chars) of
	$\n ->
	    io:fwrite(OF,"\n",[]),
	    analyse_to_html(tl(Chars),OF,Reports,{FLocLine+1,1});
	$\t ->
	    io:fwrite(OF,"\t",[]),
	    analyse_to_html(tl(Chars),OF,Reports,{FLocLine,FLocChar+8});
	$~ ->
	    io:fwrite(OF,"~~",[]),
	    analyse_to_html(tl(Chars),OF,Reports,{FLocLine,FLocChar+1});
	Data ->
	    %%io:format("~p: ~p~n",[FLoc,Data]),
	    {Starts,Ends,NewReports} = get_relevant(Reports,FLoc),
	    lists:map(fun(#analysis_report{loc=Loc}=R) ->
			      %%RHTML = make_report_html(R),
			      Class = determine_class(R),
			      io:fwrite(OF,"<span class=\"condition ~p\" analysis=\"~p\">",[Class,Loc])
		      end,
		      Starts),
	    io:fwrite(OF,[Data],[]),
	    lists:map(fun(_R) ->
			      io:fwrite(OF,"</span>",[])
		      end,
		      Ends),
	    analyse_to_html(tl(Chars),OF,NewReports,{FLocLine,FLocChar+1})
	end.

get_relevant([],_FLoc) ->
    {[],[],[]};
get_relevant([#analysis_report{loc={Start,End}}=R | MoreReports],FLoc) ->
    {SubStart,SubEnd,SubNew} = get_relevant(MoreReports,FLoc),
    if Start==FLoc ->
	    %% Single-character patterns are possible - e.g. "_"
	    if End==FLoc ->
		    {[R | SubStart],[R | SubEnd],SubNew};
	       true ->
		    {[R | SubStart],SubEnd,[R | SubNew]}
	    end;
       End==FLoc ->
	    %% Deliberately drops R from the NewReports, we don't need to consider it once we are past the end of it...
	    {SubStart,[R | SubEnd],SubNew};
       true ->
	    {SubStart,SubEnd,[R | SubNew]}
    end.

determine_class(Coverage) ->
    if (Coverage#analysis_report.nonmatched == 0) and (Coverage#analysis_report.matched == 0) ->
	    unsmothered;
       (Coverage#analysis_report.matched < 0) ->
	    if (Coverage#analysis_report.nonmatched > 0) -> 
		    if (Coverage#analysis_report.nmsubsproportion == 1) ->
			    smothered;
		       true ->
			    partiallysmothered
		    end;
	       true ->
		    unsmothered
	    end;
       (Coverage#analysis_report.nonmatched < 0) ->
	    if (Coverage#analysis_report.matched > 0) ->
		    if (Coverage#analysis_report.msubsproportion == 1) ->
			    smothered;
		       true ->
			    partiallysmothered
		    end;
	       true ->
		    unsmothered
	    end;
       (Coverage#analysis_report.nonmatched > 0) and (Coverage#analysis_report.matched > 0) and
       (Coverage#analysis_report.msubsproportion == 1) and (Coverage#analysis_report.nmsubsproportion == 1) ->
	    smothered;
       true ->
	    partiallysmothered
    end.

%% Measures coverage and returns a quad: {Match count, NonMatch count, Subs average matched, Subs average unmatched}
measure_coverage(#bool_log{exp={wrapper,atom,_Attrs,{atom,_Loc,true}}=Exp,tcount=TCount},Context) ->
    #analysis_report{
		  exp=Exp,
		  context=Context,
		  loc=get_range(Exp),
		  matched=TCount,
		  nonmatched=-1
		 };
measure_coverage(#bool_log{tcount=TCount,fcount=FCount,tsubs=TSubs,fsubs=FSubs,exp={tree,infix_expr,_Attrs,{infix_expr,{tree,operator,_OpAttrs,Image},_Left,_Right}}=Exp},Context) ->
    Loc=get_range(Exp),
    MSubs = lists:map(fun(S) -> measure_coverage(S,Context++[{true,Loc}]) end, TSubs),
    NMSubs = lists:map(fun(S) -> measure_coverage(S,Context++[{false,Loc}]) end, FSubs),
    TMSubs = case Image of
		 'and' ->
		     [];
		 _ ->
		     MSubs
	     end,
    TNMSubs = case Image of
		  'or' ->
		      [];
		  _ ->
		      NMSubs
	      end,
    #analysis_report{
		      exp=Exp,
		      context=Context,
		      loc=get_range(Exp),
		      matched=TCount,
		      nonmatched=FCount,
		      matchedsubs=TMSubs,
		      nonmatchedsubs=TNMSubs,
		      msubsproportion=coverage_average(TMSubs),
		      nmsubsproportion=coverage_average(TNMSubs)
		    };
measure_coverage(#bool_log{exp=Exp,tcount=TCount,fcount=FCount,tsubs=TSubs,fsubs=FSubs},Context) ->
    Loc=get_range(Exp),
    MSubs = lists:map(fun(S) -> measure_coverage(S,Context++[{true,Loc}]) end, TSubs),
    NMSubs = lists:map(fun(S) -> measure_coverage(S,Context++[{false,Loc}]) end, FSubs),
    #analysis_report{
		      exp=Exp,
		      context=Context,
		      loc=get_range(Exp),
		      matched=TCount,
		      nonmatched=FCount,
		      matchedsubs=MSubs,
		      nonmatchedsubs=NMSubs,
		      msubsproportion=coverage_average(MSubs),
		      nmsubsproportion=coverage_average(NMSubs)
		    };
measure_coverage(#pat_log{mcount=MCount,exp={wrapper,underscore,_Attrs,_Image}=Exp},Context) ->
    #analysis_report{
		  exp=Exp,
		  context=Context,
		  loc=get_range(Exp),
		  type=pat,
		  matched=MCount,
		  nonmatched=-1
		 };
measure_coverage(#pat_log{mcount=MCount,exp={wrapper,variable,_Attrs,_Image}=Exp},Context) ->
    #analysis_report{
		  exp=Exp,
		  context=Context,
		  loc=get_range(Exp),
		  type=pat,
		  matched=MCount,
		  nonmatched=-1
		 };
measure_coverage(#pat_log{mcount=MCount,exp={wrapper,nil,_Attrs,_Image}=Exp},Context) ->
    #analysis_report{
		  exp=Exp,
		  context=Context,
		  loc=get_range(Exp),
		  type=pat,
		  matched=MCount,
		  nonmatched=-1
		 };
measure_coverage(#pat_log{exp=Exp,mcount=MCount,nmcount=NMCount,subs=Subs,extras=Extras},Context) ->
    Loc = get_range(Exp),
    NMSubs = lists:map(fun(S) -> measure_coverage(S,Context++[{non_matched,Loc}]) end, Subs),
    ESubs = lists:map(fun(S) -> measure_coverage(S,Context++[{extra,Loc}]) end, Extras),
    #analysis_report{
		      exp=Exp,
		      context=Context,
		      loc=get_range(Exp),
		      type=pat,
		      matched=MCount,
		      nonmatched=NMCount,
		      nonmatchedsubs=NMSubs,
		      matchedsubs=ESubs,
		      nmsubsproportion=coverage_average(NMSubs),
		      msubsproportion=coverage_average(ESubs)
		    };
measure_coverage({Name,MCount,NMCount},Context) ->
    %% Extras
    #analysis_report{
		  exp=atom_to_list(Name),
		  context=Context,
		  type=extra,
		  matched=MCount,
		  nonmatched=NMCount
		 };
measure_coverage(Unk,Context) ->
    io:format("Unhandled coverage measure: ~p [Context:~p]~n", [Unk,Context]),
    #analysis_report{
	       exp=lists:flatten(io_lib:format("~p",[Unk])),
	       context=Context
	      }.

%% Create a numeric average of the coverages from a list of conds
coverage_average(List) ->
    LL = length(List),
    if LL == 0 -> 1;
       true ->
	    Res = lists:foldl(fun(Coverage,V) ->
				      V1 = if Coverage#analysis_report.matched > 0 -> Coverage#analysis_report.msubsproportion; Coverage#analysis_report.matched < 0 -> 1; true -> 0 end,
				      V2 = if Coverage#analysis_report.nonmatched > 0 -> Coverage#analysis_report.nmsubsproportion; Coverage#analysis_report.nonmatched < 0 -> 1; true -> 0 end,
				      N = (V1 + V2) / 2,
				      N + V 
			      end,
			      0,
			      List),
	    Res / LL
    end.

get_range({attr,_ALoc,Attrs,_}) ->
   case lists:keyfind(range,1,Attrs) of
        {range, Rng} ->
	   Rng;
       _ ->
	   io:format("Couldn't get range from ~p~n",[Attrs]),
	   {{0,0},{0,0}}
   end;
get_range({Record,_Item,Attrs,_Exp}) ->
    case Attrs of
	{attr,_ALoc,_As,_} ->
	    get_range(Attrs);
	_ ->
	    io:format("Er, whut? ~p~n~p~n~n",[Record,Attrs]),
	    {{0,0},{0,0}}
    end;
get_range(R) ->
    io:format("Er, Whut? Getting range from something weird...~n~p~n", [R]), 
    {{0,0},{0,0}}.

get_zeros(FDict) ->
    Reports = get_reports(FDict),
    get_zs(zero,Reports).

get_nonzeros(FDict) ->
    Reports = get_reports(FDict),
    get_zs(nonzero,Reports).

get_zs(V,Reports) ->
    lists:flatten(
      lists:map(fun(#analysis_report{loc=Loc,context=Ctx,matched=M,nonmatched=NM,matchedsubs=MSubs,nonmatchedsubs=NMSubs}) ->
			MRes = if (V == zero) and (M == 0) ->
				       [{never_matched,Loc,Ctx}];
				  (V /= zero) and (M > 0) ->
				       [{matched,Loc,Ctx}];
				  true ->
				       []
			       end,
			NMRes = if (V == zero) and (NM == 0) ->
					[{never_non_matched,Loc,Ctx}];
				   (V /= zero) and (NM > 0) ->
					[{non_matched,Loc,Ctx}];
				   true ->
					[]
				end,
			MSRes = get_zs(V,MSubs),
			NMSRes = get_zs(V,NMSubs),
			MRes ++ NMRes ++ MSRes ++ NMSRes
		end,
		Reports)
     ).

get_percentage(Analysis) ->
  Zeros = length(get_zeros(Analysis)),
  NonZeros = length(get_nonzeros(Analysis)),
  Total = Zeros + NonZeros,
  if Total == 0 -> 100;
     true ->
       (NonZeros / Total) * 100
  end.

get_reports(FDict) ->
    %%io:format("~nGetting reports from ~p~n",[FDict]),
    get_reports(FDict,[]).

get_reports([],_Context) ->
    [];
get_reports([{_Loc,{case_expr,Content}} | More],Context) ->
    get_reports(Content,Context) ++ get_reports(More,Context);
get_reports([{_Loc, {if_expr,_VarNames,ExpRecords}} | More],Context) ->
    get_reports(ExpRecords,Context) ++ get_reports(More,Context);
get_reports([{_Loc, {receive_expr,Patterns}} | More],Context) ->
    get_reports(Patterns,Context) ++ get_reports(More,Context);
get_reports([{_Loc, {fun_expr,_F,_Arity,Patterns}} | More],Context) ->
    get_reports(Patterns,Context) ++ get_reports(More,Context);
get_reports([L = #bool_log{exp=Exp,tsubs=TSubs,fsubs=FSubs} | More],Context) ->
    LReport = measure_coverage(L,Context),
    Loc = get_range(Exp),
    MergedSubs = merge_evals(TSubs,FSubs),
    [ LReport#analysis_report{context=Context}
     | get_reports(MergedSubs,Context++[Loc]) ++ get_reports(More,Context)];
get_reports([L = #pat_log{exp=Exp,guards=Gs,subs=Subs,matchedsubs=MatchedSubs} | More],Context) ->
    LReport = measure_coverage(L,Context),
    Loc = get_range(Exp),
    MergedSubs = merge_evals(Subs,MatchedSubs),
    [ LReport#analysis_report{context=Context}
     | get_reports(MergedSubs,Context++[Loc]) ++ get_reports(lists:flatten(Gs),Context++[Loc]) ++ get_reports(More,Context)];
get_reports([{_Loc,L=#pat_log{}} | More],Context) ->
    get_reports([L | More],Context);
get_reports([E | More],Context) ->
    io:format("Unhandled report: ~p in context ~p~n",[E,Context]),
    get_reports(More,Context).


merge_evals([],[]) ->
    [];
merge_evals([#bool_log{exp=LExp,tcount=LTC,fcount=LFC,tsubs=LTS,fsubs=LFS}|LMore],[#bool_log{exp=RExp,tcount=RTC,fcount=RFC,tsubs=RTS,fsubs=RFS}|RMore]) ->
    if not (LExp==RExp) ->
	    exit({"Merging miss-matched logs",?PP(LExp),?PP(RExp)});
       true ->
	    [#bool_log{
		exp=LExp,
		tcount=LTC+RTC,
		fcount=LFC+RFC,
		tsubs=merge_evals(LTS,RTS),
		fsubs=merge_evals(LFS,RFS)
	       }   
	     | merge_evals(LMore,RMore)]
    end;
merge_evals([#pat_log{exp=LExp,guards=LGS,mcount=LMC,nmcount=LNMC,subs=LS,extras=LE,matchedsubs=LMS} | LMore],[#pat_log{exp=RExp,guards=RGS,mcount=RMC,nmcount=RNMC,subs=RS,extras=RE,matchedsubs=RMS} | RMore]) -> 
    if not (LExp==RExp) ->
	    exit({"Merging miss-matched logs",?PP(LExp),?PP(RExp)});
       true ->
	    [#pat_log{
		exp=LExp,
		guards=merge_evals(LGS,RGS),
		mcount=LMC+RMC,
		nmcount=LNMC+RNMC,
		subs=merge_evals(LS,RS),
		extras=merge_evals(LE,RE),
		matchedsubs=merge_evals(LMS,RMS)
	       }
	     | merge_evals(LMore,RMore)]
    end;
merge_evals([{LEName,LMC,LNMC} | LMore],[{REName,RMC,RNMC} | RMore]) ->
    %% Extras
    if not (LEName == REName) ->
	    exit({"Merging miss-matched extras",LEName,REName});
       true ->
	    [{LEName,LMC+RMC,LNMC+RNMC}
	     | merge_evals(LMore,RMore)]
    end;
merge_evals(A,B) ->    
    exit({"Merging unequal branches",A,B}).

exp_printer(Exp) ->
    case io_lib:printable_list(Exp) of
	true ->
	    Exp;
	_ ->
	    case io_lib:printable_unicode_list(Exp) of
		true ->
		    Exp;
		_ ->
		    ?PP(Exp)
	    end
    end.

json_to_list([]) ->
    [];
json_to_list([Elem]) ->
    [Elem];
json_to_list([E | More]) ->
    [E, 
     lists:map(fun(R) ->
		       case R of 
			   [] -> "";
			   _ ->
			       "," ++ R
		       end
	       end,
	       More)].

json_reports(Reports) ->
    JReports = lists:map(fun report_to_json/1, Reports),
    Content = json_to_list(JReports),
    binary:list_to_bin("[" ++ Content ++ "]").

loc_to_json_compat({{SL,SC},{EL,EC}}) ->
    [
     {startline,SL}
     ,{startchar,SC}
     ,{endline,EL}
     ,{endchar,EC}
    ].

report_to_json(Report=#analysis_report{}) ->
    Fields = lists:zip(record_info(fields, analysis_report),
		       lists:seq(2, record_info(size, analysis_report))),
    Items = lists:map(fun({Name,I}) -> 
			      Val = element(I,Report),
			      case Name of
				  exp ->
				      {exp,binary:list_to_bin(exp_printer(Val))};
				  context ->
				      {context,binary:list_to_bin(io_lib:format("~p",[Val]))};
				  loc ->
				      {loc, loc_to_json_compat(Val)};
				  matchedsubs ->
				      {Name, []};
				  nonmatchedsubs ->
				      {Name, []};
				  _ ->
				      {Name,Val}
			      end
		      end, 
		      Fields
		     ),
    JSON = jsx:encode(Items),
    MSubs = make_sub_reports(Report#analysis_report.matchedsubs),
    NMSubs = make_sub_reports(Report#analysis_report.nonmatchedsubs),
    J1 = re:replace(JSON,"\",matchedsubs\":\\[\\]","\",matchedsubs\":[" ++ json_to_list(MSubs) ++ "]",[{return,list}]),
    re:replace(J1,"\"nonmatchedsubs\":\\[\\]","\"nonmatchedsubs\":[" ++ json_to_list(NMSubs) ++ "]",[{return,list}]).
    
make_sub_reports([]) ->
    [];
make_sub_reports([#analysis_report{exp=Exp,matched=M,nonmatched=NM} | More]) ->
    [ jsx:encode([{exp,binary:list_to_bin(exp_printer(Exp))},{matched,M},{nonmatched,NM}])
     | make_sub_reports(More)].

loc_sort(#analysis_report{loc=L1},L2) ->
    loc_sort(L1,L2);
loc_sort(L1,#analysis_report{loc=L2}) ->
    loc_sort(L1,L2);
loc_sort({{SL1,SC1},{EL1,EC1}}, {{SL2,SC2},{EL2,EC2}}) ->
    if SL1 < SL2 ->
            true;
       SL1 > SL2 ->
            false;
       true ->
            if SC1 < SC2 ->
                    true;
               SC1 > SC2 ->
                    false;
               true ->
                    %% Equal start points
                    %% Compare end positions
                    if EL1 < EL2 ->
                            true;
                       EL1 > EL2 ->
                            false;
                       true ->
                            if EC1 < EC2 ->
                                    true;
                               EC1 > EC2 ->
                                    false;
                               true ->
                                    %% Equal.
                                    true
                            end
                    end
            end
    end.
