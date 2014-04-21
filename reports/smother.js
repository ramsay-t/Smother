function loc_string(loc) {
       return "{{" + loc.startline.toString() 
	+ "," + loc.startchar.toString() 
	+ "},{" 
	+ loc.endline.toString() 
	+ "," 
	+ loc.endchar.toString() 
	+ "}}";
}

function strcmp(s1, s2) {
		 if(s1.length == 0) {
		     if(s2.length == 0) {
			 return true;
		     } else {
			 return false;
		     }
		 }
		 if(s2.length == 0) {
		     if(s1.length == 0) {
			 return true;
		     } else {
			 return false;
		     }
		 }
		 if(s1.charAt(0) == s2.charAt(0)) {
		     alert("Compare ok at " + s1.charAt(0) + " vs " + s2.charAt(0));
		     return strcmp(s1.slice(1), s2.slice(1));
		 } else {
		     alert("Compare failed at " + s1.charAt(0) + " vs " + s2.charAt(0));
		     return false;
		 }
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

function load_subs(elem,sublist) {
    elem.append("Found " + sublist.length.toString() + " subs<br />");
    elem.append("<table class=\"subtable\">");
    for(var i = 0; i < sublist.length; i++) {
	r = find_report(loc_string(sublist[i]));
	if(r == null) {
	    alert("Could not find report for location " + loc_string(sublist[i]));
	} else {
	    elem.append("<tr><td>" 
			+ r.exp 
			+ "</td><td>" 
			+ numcolour(r.matched)
			+ "</td><td>" 
			+ numcolour(r.nonmatched)
			+ "</td></tr>");
	}
    } 
    elem.append("</table>");    
}

function load_report(report) {
    $('#infoexp').html(report.exp);
    $('#infomatched').html(numcolour(report.matched));
    $('#infononmatched').html(numcolour(report.nonmatched));

    $('#infomatchedsubs').empty();
    if(report.matchedsubs.length > 0) {
	$('#infomatchedsubs').append("<h2>When matched:</h2>");
	load_subs($('#infomatchedsubs'),report.matchedsubs);
    }
    $('#infononmatchedsubs').empty();
    if(report.nonmatchedsubs.length > 0) {
	$('#infononmatchedsubs').append("<h2>When non-matched:</h2>");
	load_subs($('#infononmatchedsubs'),report.nonmatchedsubs);
	
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
  $('.smothered').mouseover(tipfun);
  $('.smothered').mouseout(cleartip);
  $('.partiallysmothered').mouseover(tipfun);
  $('.partiallysmothered').mouseout(cleartip);
  $('.unsmothered').mouseover(tipfun);
  $('.unsmothered').mouseout(cleartip);
  resize();
  $( window ).resize(resize);
});
