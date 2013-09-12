-module(smother_analysis).
-export([make_html_analysis/3,get_range/1,get_zeros/1,get_nonzeros/1,get_percentage/1]).

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
	
    {ok,IF} = file:open(File,[read]),
    ok = analyse_to_html(IF,OF,FDict,{1,1}),
    file:close(IF),
    io:fwrite(OF,"
</pre>

</body>
</html>
",[]),
    ok.


analyse_to_html(IF,OF,FDict,{FLocLine,FLocChar} = FLoc) ->
    case file:read(IF,1) of
	eof ->
	    ok;
	{error, Reason} ->
	    {error, Reason};
	{ok, "\n"} ->
	    io:fwrite(OF,"\n",[]),
	    analyse_to_html(IF,OF,FDict,{FLocLine+1,1});
	{ok, "\t"} ->
	    io:fwrite(OF,"\t",[]),
	    analyse_to_html(IF,OF,FDict,{FLocLine,FLocChar+8});
	{ok, Data} ->
	    %%io:format("~p: ~p~n",[FLoc,Data]),
	    case get_analysis(FDict,FLoc) of
		none ->
		    io:fwrite(OF,Data,[]),
		    analyse_to_html(IF,OF,FDict,{FLocLine,FLocChar+1});
		{conditions,List} ->
		    Ends = lists:filter(fun(T) -> element(1,T) == condition_end end, List),
		    Starts = lists:filter(fun(T) -> element(1,T) == condition_start end, List),
		    lists:map(fun({condition_start,Coverage}) -> 
				      Report = make_report(Coverage),
				      io:fwrite(OF,"~s",[Report])
			      end, Starts),
		    io:fwrite(OF,"~s",[Data]),
		    lists:map(fun(_) -> io:fwrite(OF,"</span>",[]) end, Ends),
		    analyse_to_html(IF,OF,FDict,{FLocLine,FLocChar+1})
	    end;
	Err ->
	    {error, Err}
	end.

make_report(Coverage=#analysis_report{type=bool}) ->
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
				       Coverage#analysis_report.exp,
				       colourise(Coverage#analysis_report.matched),
				       colourise(Coverage#analysis_report.nonmatched),
				       MatchMsg,
				       NonMatchMsg
				      ])),
    DeQMsg = re:replace(Msg,"\"","\\&quot;",[{return,list},global]),
    lists:flatten(io_lib:format("<span class=\"condition ~p\" title=\"~s\">",[Class,DeQMsg]));
make_report(Coverage=#analysis_report{type=pat}) ->
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
				       Coverage#analysis_report.exp,
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
					     io_lib:format("<tr><td>~s</td><td>~s</td><td>~s</td></tr>",[Exp,colourise(M),colourise(NM)]) 
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

get_analysis(FDict,FLoc) ->
    case lists:filter(fun ({Loc, _Cond}) -> 
			      smother_server:within_loc(Loc,{FLoc,FLoc})
		      end, FDict) of
	[] ->
	    none;
	List ->
	    {conditions,lists:flatten(lists:map(fun({{_Start,_End}, Cond}) ->
							%% The Dict entires always have the 
							%% declared content as the last component,
							%% except function cases, which are a bit weirder...
							case Cond of
							    {fun_expr,_Name,_Arity,Patterns} ->
								lists:map(fun({_Loc,Content}) ->
										  get_analysis_from_subs(Content,FLoc) 
									  end, Patterns);
							    _ ->
								L = tuple_to_list(Cond),
								Content = lists:nth(length(L),L),
								lists:map(fun(C) -> 
										  get_analysis_from_subs(C,FLoc) 
									  end, Content)
							end
						end,
						List))}
    end.

get_analysis_from_subs(Cond = #bool_log{},FLoc) ->
    %%io:format("Analysing Guard for ~p: ~p~n", [FLoc,get_range(Cond#bool_log.exp)]),
    {Start, End} = get_range(Cond#bool_log.exp),
    SubConds = analyse_both_branches(
		 lists:flatten(lists:map(fun(Sub) -> get_analysis_from_subs(Sub,FLoc) end, Cond#bool_log.tsubs))
		 , 
		 lists:flatten(lists:map(fun(Sub) -> get_analysis_from_subs(Sub,FLoc) end, Cond#bool_log.fsubs))),
    %% A single char pattern can both start and end at the same time...
    R1 = 
	if FLoc == End ->
		[{condition_end} | SubConds];
	   true -> []
	end,
    R2 =
	if FLoc == Start ->
                %%io:format("==> ~p~n",[measure_coverage(Cond)]),
		[{condition_start,measure_coverage(Cond)} | SubConds];
	   true ->
		SubConds
	end,
    R1 ++ R2;
get_analysis_from_subs(Cond = #pat_log{},FLoc) ->
    {Start, End} = get_range(Cond#pat_log.exp),
    SubConds = analyse_both_branches(
		 lists:flatten(lists:map(fun(Sub) -> get_analysis_from_subs(Sub,FLoc) end, Cond#pat_log.subs)),
		 lists:flatten(lists:map(fun(Sub) -> get_analysis_from_subs(Sub,FLoc) end, Cond#pat_log.matchedsubs))),

    %% Add Guards...
    GuardItems = lists:flatten(lists:map(fun(G) -> 
						 get_analysis_from_subs(G, FLoc)
					 end,
					 lists:flatten(Cond#pat_log.guards))
			      ),
    R1 = 
	if FLoc == End ->
		[{condition_end} | SubConds];
	   true -> []
	end,
    R2 =
	if FLoc == Start ->
		[{condition_start,measure_coverage(Cond)} | SubConds];
	   true ->
		SubConds
	end,
    R1 ++ R2 ++ GuardItems;
get_analysis_from_subs(Cond,_FLoc) ->
    io:format("UNHANDLED analysis sub: ~p~n",[Cond]),
    [].

analyse_both_branches([], []) ->
    [];
analyse_both_branches([[]], [[]]) ->
    [];
analyse_both_branches([], B) ->
    B;
analyse_both_branches(B, []) ->
    B;
analyse_both_branches(MBranch, NMBranch) ->
    %% Assume (??) that each side will have the same order. It should, since they are usually built as copies
    %% If this was Ocaml I could just use map2...
    Pairs = lists:zip(MBranch,NMBranch),
    lists:map(fun({M,NM}) -> merge_branches(M,NM) end, Pairs).

merge_branches({condition_start,#analysis_report{exp=LExp,matched=LM,nonmatched=LNM,matchedsubs=LMS,nonmatchedsubs=LNMS}},{condition_start,#analysis_report{exp=RExp,matched=RM,nonmatched=RNM,matchedsubs=RMS,nonmatchedsubs=RNMS}}) ->
    if not (LExp==RExp) ->
	    exit({"merging different expressions",LExp,RExp});
       true ->
	    MRep = #analysis_report{
		      exp=LExp,
		      matched=LM+RM,
		      nonmatched=LNM+RNM,
                      matchedsubs=merge_coverage(LMS,RMS),
                      nonmatchedsubs=merge_coverage(LNMS,RNMS)		      
		     },
	    {condition_start,MRep}
    end;
merge_branches({condition_end},{condition_end}) ->
    {condition_end};
merge_branches(M,NM) ->
    io:format("Don't know how to merge ~p and ~p~n",[M,NM]),
    [].

merge_coverage([],[]) ->
  [];
merge_coverage([#analysis_report{exp=Exp,matched=LM,nonmatched=LNM,matchedsubs=LMS,nonmatchedsubs=LNMS}| LMore],[#analysis_report{exp=RExp,matched=RM,nonmatched=RNM,matchedsubs=RMS,nonmatchedsubs=RNMS} | RMore]) ->
  [#analysis_report{
		      exp=Exp,
		      matched=LM+RM,
		      nonmatched=LNM+RNM,
                      matchedsubs=merge_coverage(LMS,RMS),
                      nonmatchedsubs=merge_coverage(LNMS,RNMS)		      
		     } 
   | merge_coverage(LMore, RMore)];
merge_coverage(L,R) ->
  exit({"Merging different coverage.",{L,R}}).

%% Measures coverage and returns a quad: {Match count, NonMatch count, Subs average matched, Subs average unmatched}
measure_coverage(#bool_log{exp={wrapper,atom,_Attrs,{atom,_Loc,true}}=Exp,tcount=TCount}) ->
    #analysis_report{
       exp=?PP(Exp),
       matched=TCount,
       nonmatched=-1
      };
measure_coverage(#bool_log{tcount=TCount,fcount=FCount,tsubs=TSubs,fsubs=FSubs,exp={tree,infix_expr,_Attrs,{infix_expr,{tree,operator,_OpAttrs,Image},_Left,_Right}}=Exp}) ->
    MSubs = lists:map(fun measure_coverage/1, TSubs),
    NMSubs = lists:map(fun measure_coverage/1, FSubs),
    Report = #analysis_report{
		exp=?PP(Exp),
		matched=TCount,
		nonmatched=FCount,
		matchedsubs=MSubs,
		nonmatchedsubs=NMSubs,
		msubsproportion=coverage_average(MSubs),
		nmsubsproportion=coverage_average(NMSubs)
	       };
measure_coverage(#bool_log{tcount=TCount,fcount=FCount,tsubs=TSubs,fsubs=FSubs,exp=Exp}) ->
    MSubs = lists:map(fun measure_coverage/1, TSubs),
    NMSubs = lists:map(fun measure_coverage/1, FSubs),
    #analysis_report{
		exp=?PP(Exp),
		matched=TCount,
		nonmatched=FCount,
		matchedsubs=MSubs,
		nonmatchedsubs=NMSubs,
		msubsproportion=coverage_average(MSubs),
		nmsubsproportion=coverage_average(NMSubs)
	       };
measure_coverage(#pat_log{mcount=MCount,exp={wrapper,underscore,_Attrs,_Image}=Exp}) ->
    #analysis_report{
       exp=?PP(Exp),
       type=pat,
       matched=MCount,
       nonmatched=-1
      };
measure_coverage(#pat_log{mcount=MCount,exp={wrapper,variable,_Attrs,_Image}=Exp}) ->
    #analysis_report{
       exp=?PP(Exp),
       type=pat,
       matched=MCount,
       nonmatched=-1
      };
measure_coverage(#pat_log{mcount=MCount,exp={wrapper,nil,_Attrs,_Image}=Exp}) ->
    #analysis_report{
       exp=?PP(Exp),
       type=pat,
       matched=MCount,
       nonmatched=-1
      };
measure_coverage(#pat_log{mcount=MCount,nmcount=NMCount,subs=Subs,extras=Extras,exp=Exp}) ->
    NMSubs = lists:map(fun measure_coverage/1, Subs),
    ESubs = lists:map(fun measure_coverage/1, Extras),
    #analysis_report{
		exp=?PP(Exp),
		type=pat,
		matched=MCount,
		nonmatched=NMCount,
		nonmatchedsubs=NMSubs,
		matchedsubs=ESubs,
		nmsubsproportion=coverage_average(NMSubs),
		msubsproportion=coverage_average(ESubs)
	       };
measure_coverage({Name,MCount,NMCount}) ->
    %% Extras
    #analysis_report{
       exp=lists:flatten(io_lib:format("~p",[Name])),
       type=pat,
       matched=MCount,
       nonmatched=NMCount
      };
measure_coverage(Unk) ->
    io:format("Unhandled coverage measure: ~p~n", [Unk]),
    #analysis_report{
       exp=lists:flatten(io_lib:format("~p",[Unk]))
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

get_zeros(A) ->
  get_zs(zero,A).
get_nonzeros(A) ->
  get_zs(nonzero,A).

get_zs(_V,[]) ->
    [];
get_zs(V,[{Loc,{case_expr,_Expr,Content}} | More]) ->
    lists:flatten(lists:map(fun(C) -> get_z_leaves(V,C) end, Content)) ++ get_zs(V,More);
get_zs(V,[{Loc,{if_expr,_VarNames,Content}} | More]) ->
    lists:flatten(lists:map(fun(C) -> get_z_leaves(V,C) end, Content)) ++ get_zs(V,More);
get_zs(V,[{Loc,{receive_expr,Content}} | More]) ->
    lists:flatten(lists:map(fun(C) -> get_z_leaves(V,C) end, Content)) ++ get_zs(V,More);
get_zs(V,[{Loc,{fun_expr,F,Arity,Patterns}} | More]) ->
    lists:flatten(lists:map(fun(C) -> get_z_leaves(V,C) end, lists:map(fun(P) -> element(2,P) end, Patterns))) ++ get_zs(V,More);
get_zs(V,C) ->
    exit({unimplemented_get_zs,C}). 

 

get_z_leaves(V, #pat_log{exp=Exp,mcount=MCount,nmcount=NMCount,subs=NMSubs,extras=Extras}) ->
    MZs = z_context(matched,Exp,lists:flatten(lists:map(fun(S) -> get_z_leaves(V,S) end,Extras))),
    MZ = if (V == zero) and (MCount == 0) ->
               [{never_matched,get_range(Exp),[]}];
            (V /= zero) and (MCount > 0) ->
               [{matched,get_range(Exp),[]}];
           true ->
	       []
         end,
    NMZs = z_context(non_matched,Exp,lists:flatten(lists:map(fun(S) -> get_z_leaves(V,S) end,NMSubs))),
    NMZ =      
        if (V == zero) and (MCount == 0) ->
               [{never_non_matched,get_range(Exp),[]}];
           (V /= zero) and (MCount > 0) ->
                [{non_matched,get_range(Exp),[]}];
           true ->
	       []
         end,
    MZ ++ MZs ++ NMZ ++ NMZs;
get_z_leaves(V, #bool_log{exp=Exp,tcount=TCount,fcount=FCount,fsubs=FSubs,tsubs=TSubs}) ->
    TZs = z_context(true,Exp,lists:flatten(lists:map(fun(S) -> get_z_leaves(V,S) end,TSubs))),
    TZ = if (V == zero) and (TCount == 0) ->
               [{never_true,get_range(Exp),[]}];
           (V /= zero) and (TCount > 0) ->
               [{true,get_range(Exp),[]}];
           true ->
	       []
         end,
    FZs = z_context(false,Exp,lists:flatten(lists:map(fun(S) -> get_z_leaves(V,S) end,FSubs))),
    FZ =      
        if (V == zero) and (FCount == 0) ->
               [{never_false,get_range(Exp),[]}];
           (V /= zero) and (FCount > 0) ->
               [{false,get_range(Exp),[]}];
           true ->
	       []
         end,
    TZ ++ TZs ++ FZ ++ FZs;
get_z_leaves(V, {Extra,MCount,NMCount}) ->
    MC = if (V == zero) and (MCount == 0) ->
          [{never_used_extra,Extra,[]}];
        (V /= zero) and (MCount > 0) ->
          [{used_extra,Extra,[]}];
      true ->
          []
    end,
    NMC = if (V == zero) and (NMCount == 0) ->
             [{never_unused_extra,Extra,[]}];
           (V /= zero) and (NMCount > 0) ->
             [{unused_extra,Extra,[]}];
           true ->
             []
    end,
    MC ++ NMC;
get_z_leaves(V, C) ->
    exit({unimplemented_get_leaves,C}). 

z_context(Status, Exp, Zs) ->
	       E = get_range(Exp),
	       [{MNM,R,[{Status, E} | Ctx]} || {MNM,R,Ctx} <- Zs].

get_percentage(Analysis) ->
  Zeros = length(get_zeros(Analysis)),
  NonZeros = length(get_nonzeros(Analysis)),
  Total = Zeros + NonZeros,
  if Total == 0 -> 100;
     true ->
       (NonZeros / Total) * 100
  end.
