-module(smother_analysis).
-export([make_html_analysis/3,get_range/1,get_zeros/1,get_nonzeros/1,get_percentage/1,get_reports/1,exp_printer/1]).

-include_lib("wrangler/include/wrangler.hrl").
-include("include/eval_records.hrl").
-include("include/analysis_reports.hrl").

make_html_analysis(File,FDict,OF) ->
    io:fwrite(OF,"<html>
<head>
<style>
.ui-tooltip {
	padding: 8px;
	position: absolute;
	z-index: 9999;
	max-width: 300px;
	-webkit-box-shadow: 0 0 5px #aaa;
	box-shadow: 0 0 5px #aaa;
        border-width:2px;
        background-color: white;
}
.condition {
  border-style: dashed;
  border-width:1px;

}
.smothered {
  color:green;
}
.partiallysmothered {
  color:orange;
}
.unsmothered {
  color:red;
}
</style>
<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js\"></script>
<script src=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js\"></script>
<script type=\"text/javascript\">
function tipfun() {
		 return $(this).prop('title');
}

$(document).ready(function() {
  $('.smothered').tooltip({content: tipfun});
  $('.partiallysmothered').tooltip({content: tipfun});
  $('.unsmothered').tooltip({content: tipfun});
});
</script>
</head>
<body>

<pre>
",[]),
	
    Reports = lists:sort(fun(#analysis_report{loc={{LSLine,LSChar},_}},
                             #analysis_report{loc={{RSLine,RSChar},_}}) -> 
                                       if LSLine == RSLine -> 
                                                LSChar =< RSChar; 
                                          true -> 
                                                LSLine < RSLine 
                                       end 
                         end, 
                         get_reports(FDict)),
    {ok,Bin} = file:read_file(File),
    Chars = binary_to_list(Bin), 
    ok = analyse_to_html(Chars,OF,Reports,{1,1}),
    io:fwrite(OF,"
</pre>

</body>
</html>
",[]),
    ok.

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
	    lists:map(fun(R) ->
			      RHTML = make_report_html(R),
			      io:fwrite(OF,"~s",[RHTML])
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

make_report_html(Coverage=#analysis_report{type=bool}) ->
    %% io:format("Making messages for ~s (~p,~p)~n",[Coverage#analysis_report.exp,length(Coverage#analysis_report.matchedsubs),length(Coverage#analysis_report.nonmatchedsubs)]),
    Class = determine_class(Coverage),

    MatchMsg = make_msg(
		 "When true",
		 Coverage#analysis_report.matched,
		 Coverage#analysis_report.msubsproportion,
		 Coverage#analysis_report.matchedsubs
		),
    NonMatchMsg = make_msg(
		    "When false",
		    Coverage#analysis_report.nonmatched,
		    Coverage#analysis_report.nmsubsproportion,
		    Coverage#analysis_report.nonmatchedsubs
		   ),
    
    Msg = lists:flatten(io_lib:format("<h3>~s</h3>
<ul>
<li>Matched: ~s times</li>
<li>Non-Matched: ~s times</li>
</ul>
~s
~s
",
				      [
				       exp_printer(Coverage#analysis_report.exp),
				       colourise(Coverage#analysis_report.matched),
				       colourise(Coverage#analysis_report.nonmatched),
				       MatchMsg,
				       NonMatchMsg
				      ])),
    DeQMsg = re:replace(Msg,"\"","\\&quot;",[{return,list},global]),
    lists:flatten(io_lib:format("<span class=\"condition ~p\" title=\"~s\">",[Class,DeQMsg]));
make_report_html(Coverage=#analysis_report{type=pat}) ->
    %%io:format("Making messages for ~s~n",[Coverage#analysis_report.exp]),

    Class = determine_class(Coverage),

    ExtraMsg = make_msg(
		 "Extras",
		 Coverage#analysis_report.matched,
		 Coverage#analysis_report.msubsproportion,
		 Coverage#analysis_report.matchedsubs
		),
    NonMatchMsg = make_msg(
		    "When non-matched",
		    Coverage#analysis_report.nonmatched,
		    Coverage#analysis_report.nmsubsproportion,
		    Coverage#analysis_report.nonmatchedsubs
		   ),

    Msg = lists:flatten(io_lib:format("<h3>~s</h3>
<ul>
<li>Matched: ~s times</li>
<li>Non-Matched: ~s times</li>
</ul>
~s
~s
",
				      [
				       exp_printer(Coverage#analysis_report.exp),
				       colourise(Coverage#analysis_report.matched),
				       colourise(Coverage#analysis_report.nonmatched),
				       NonMatchMsg,
				       ExtraMsg
				      ])),
    DeQMsg = re:replace(Msg,"\"","\\&quot;",[{return,list},global]),
    lists:flatten(io_lib:format("<span class=\"condition ~p\" title=\"~s\">",[Class,DeQMsg])).

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

make_msg(Status,Proportion,SubProportion,Reports) ->
%%io:format("      Message for \"~s\": ~p ~p ~p ~n",[Status,Proportion,SubProportion,length(Reports)]),
    Percentage = if Proportion == 0 -> 
		 colourise(0); 
	    Proportion < 0 -> 
		 colourise(-1); 
	    true -> 
		 colourise(SubProportion,100) 
	 end,
    if length(Reports) > 0 ->
	    MatchReports = lists:map(fun(#analysis_report{exp=Exp,matched=M,nonmatched=NM}) -> 
					     EString = exp_printer(Exp),
					     io_lib:format("<tr><td>~s</td><td>~s</td><td>~s</td></tr>",[EString,colourise(M),colourise(NM)]) 
				     end,
				     Reports),
	    io_lib:format("<div>
<strong>~s</strong>: ~s% sub-component coverage
<table>
<tr><th>&nbsp;</th><th>matched</th><th>non-matched</th></tr>
~s
</table>
</div>", [Status,Percentage,MatchReports]);
       true ->
	    ""
    end.

colourise(N) ->
    colourise(N,1).

colourise(N,Scale) ->
    lists:flatten(
      if N == 0 ->
	      io_lib:format("<font color=red>~p</font>",[N*Scale]);
	 N < 0 ->
	      io_lib:format("N/A",[]);
	 N < 1 ->
	      io_lib:format("<font color=blue>~p</font>",[N*Scale]);
	 true ->
	      io_lib:format("<font color=green>~p</font>",[N*Scale])
      end
     ).

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
