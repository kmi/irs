function uponLoad () {
    var tabView = new YAHOO.widget.TabView('eventspage');
    tabView.set('activeIndex', 0);

    ensurePane("irs");

    doServiceInfo();
    doEventsInfo();
}

function doServiceInfo () {
    irs.listServices(
		     function (services) {
		     document.getElementById("services").innerHTML = "";
		     for (i=0; i<services.length; i++) {
			 var service = services[i];
			 document.getElementById("services").innerHTML +=
			     "<tr class='event'><td>" + service.service +
			     "</td><td>" + service.host + ":" + service.port
			     + "</td></tr>";
		     }
		 });
}

function doEventsInfo () {
    irs.listEvents(function(data) {
	eventsListUpdater(data);
	eventsPaneUpdater(data);});
}

// This is the list of events by time version.
function eventsListUpdater (events) {
    for (var i=0; i<events.length; i++) {
	var event = events[i];
	pushEventLine(document.getElementById("events"), event, true);
    }
}

// This is the events-by-service version.
function eventsPaneUpdater (events) {
    for (var i=0; i<events.length; i++) {
	var event = events[i];
	if (event.operation == "GOAL-CALL"
	    || event.operation == "GOAL-RETURN"
	    || event.operation == "REGISTER-SERVICE") {
	    pushEventLine(ensurePane("irs"), event, true);
	} else {
	    pushEventLine(ensurePane(event.name), event, false);
	}
    }
}

function addXmlTooltip(id, event) {
    var myTooltip = new YAHOO.widget.Tooltip("tooltip" + id, {
	context: id, text: replaceAll(xmlEscape(event.content), "gt;", "gt;<br>"),
	autoDismissDelay: 10000, monitorResize: true});
}

// String of HTML for sticking into a table.  Includes service name.
function pushEventLine(container, event, includeServiceName) {
    var id = gensym();
    var row = document.createElement("tr");
    var maybeServiceName = "";
    var elided = "";

    var snippet = function(string) {
	var n = 66;

	if (string.length > n) {
	    string = string.substring(0,n);
	    elided = "...";
	}
	string = xmlEscape(string);
	return (string + elided);
    };

    row.setAttribute("class", "event2");
    if (includeServiceName) {
	maybeServiceName = "<td class='eventService'>" + event.name + "</td>";
    }
    container.insertBefore(row,container.firstChild);
    row.innerHTML =
	"<td class='eventTime'>" + event.time + "</td>"
	+ "<td class='eventClass'>" + event.operation + "</td>"
	+ maybeServiceName
	+ "<td id='" + id + "' class='eventContent'>"
	+ "<tt>" +  snippet(event.content) + "</tt></td>";
    addXmlTooltip(id,event);
}

function ensurePane(serviceName) {
    var pane = document.getElementById(paneName(serviceName));
    if (pane) {
	return pane;
    } else {
	var id = paneName(serviceName);
	var container = document.getElementById("events-by-service");
	container.innerHTML +=
	    "<tr><td><table width='100%'>"
	    + "<tr class='servicePaneHeader'><td>"
	    + serviceName + "</td></tr>"
	    + "<tr><td><div class='servicePane'><table width='100%' id='" + id + "'></table></td></tr>"
	    + "</table></div></td></tr>";
	return document.getElementById(id);
    }
}

function paneName(serviceName) {
    return "wspanel_" + serviceName;
}
