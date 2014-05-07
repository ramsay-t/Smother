function loc_string(loc) {
       return "{{" + loc.startline.toString() 
	+ "," + loc.startchar.toString() 
	+ "},{" 
	+ loc.endline.toString() 
	+ "," 
	+ loc.endchar.toString() 
	+ "}}";
}

function find_report(loc) {
       for(var i = 0; i < reports.length; i++) {
	    rls = loc_string(reports[i].loc);
	    if(rls === loc) {
		return reports[i]
	    }
       }
       return null;
}

function numcolour(val) {
    if(val < 0) {
	return "<span style=\"color:black;\">N/A</span>"
    } else if(val == undefined) {
	return "<span style=\"color:red;\">UNDEFINED</span>"
    } else if(val == 0) {
	return "<span style=\"color:red;\">0</span>"
    } else {
	return "<span style=\"color:green;\">" + val.toString() + "</span>"
    }
}

function bestpos(string, character, bestpos) {
    var pos = string.indexOf(character,8);
    if(pos < 0) {
	return bestpos;
    } else if(pos > bestpos) {
	return bestpos;
    } else {
	return pos;
    }
}

function load_subs(elem,sublist) {
    var table = "";
    table += "<table class=\"subtable\"><tr><td>&nbsp;</td><td>matched</td><td>non-matched</td></tr>";
    for(var i = 0; i < sublist.length; i++) {
	r = sublist[i];
	var exp = "";
	var moreexp = r.exp;
	while(moreexp.length > 20) {
	    // Find a comma or space or something else sensible to break after
	    var pos = bestpos(moreexp," ",20);
	    pos = bestpos(moreexp,",",pos);
	    pos = bestpos(moreexp,"}",pos);
	    pos = bestpos(moreexp,")",pos);
	    pos = bestpos(moreexp,"_",pos);
	    exp += moreexp.substring(0,pos) + "<br />&nbsp;&nbsp;&nbsp;&nbsp;";
	    moreexp = moreexp.substring(pos);
	}
	exp += moreexp;
	table += "<tr><td>" 
		    + exp 
		    + "</td><td>" 
		    + numcolour(r.matched)
		    + "</td><td>" 
		    + numcolour(r.nonmatched)
		    + "</td></tr>";
    } 
    table += "</table>";
    elem.append(table);
}

function load_report(report) {
    $('#infoexp').html(report.exp);
    $('#infomatched').html(numcolour(report.matched));
    $('#infononmatched').html(numcolour(report.nonmatched));

    $('#infomatchedsubs').empty();
    $('#infononmatchedsubs').empty();
    if(report.type=="pat") {
	if((report.nonmatchedsubs.length > 0)||(report.matchedsubs.length > 0)) {
    	    $('#infononmatchedsubs').append("<h2>When non-matched:</h2>");
	    load_subs($('#infononmatchedsubs'),report.nonmatchedsubs.concat(report.matchedsubs));
	}
    } else {
	if(report.nonmatchedsubs.length > 0) {
	    $('#infononmatchedsubs').append("<h2>When false:</h2>");
	    load_subs($('#infononmatchedsubs'),report.nonmatchedsubs);
	} 
	if(report.matchedsubs.length > 0) {
		$('#infomatchedsubs').append("<h2>When True:</h2>");
		load_subs($('#infomatchedsubs'),report.matchedsubs);
	}
    }

    //$('#infocomment').html(JSON.stringify(report));
}

function tipfun(e) {
       e.stopPropagation();
       report = find_report($(this).attr('analysis'));
       if(report != null) {
	   load_report(report);
	   $('#info').show();
       } else {
           alert("Not found");
           $('#info').hide()
       }
}
function cleartip(e) {
       e.stopPropagation();
       $('#info').hide();
}

function resize() {
  $('#info').css('left',($(window).width()-400) + "px");
}

$(document).ready(function() {
    $('#info').hide();
    $('.smothered').mouseover(tipfun);
    $('.smothered').mouseout(cleartip);
    $('.partiallysmothered').mouseover(tipfun);
    $('.partiallysmothered').mouseout(cleartip);
    $('.unsmothered').mouseover(tipfun);
    $('.unsmothered').mouseout(cleartip);
    resize();
    $( window ).resize(resize);
});
