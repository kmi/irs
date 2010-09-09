// Copyright © 2007,2008 The Open University

// This is an API to the JavaScript support on the IRS server.

/*
The httpget function has now been moved outside of the 'irs' function.
Also, I added an 'asynchronous' parameter because there are times when we want to make the request to the server 
in a synchronous rather than asynchronous manner (e.g. when receiving the data in bulk for an ontology).
(But honestly, I'm not sure what is peculiar about getting all the ontology data that means we have to change
from asynchronous to synchronous.  I just know we have to, otherwise it won't work)

Note also, the 'onload' Event Listener for 'request' has been removed because this is only supported by Mozilla.  
It has been replaced with the 'onreadystatechange' Event Listener
*/
    function httpget(url,callback, asynchronous, multipart) {
	
	//In case the calling function didn't bother to say whether GET should be asynchronous or synchronous, then make it asynchronous by default.
	asynchronous = typeof(asynchronous) !== 'undefined' ? asynchronous : true;

	var request = new XMLHttpRequest();
	request.multipart = multipart;

	request.open("GET", url, true); //asynchronous
	request.onreadystatechange = function () {
	 if(request.readyState == 4 || request.readyState == 0) {
	    if (request.status == 200) {
		callback(request.responseText); 
	    }
	}
	}
	request.send(null);
    }
	
var irs = function() {

    function baseUri () {
        return document.getElementById("baseid").href;
    }

    // The absolute IRS URI, computed from the relUri and the base href.
    function irsUri (relUri) {
        return baseUri() + relUri;
    }

    // CALLBACK is a function of one argument, an array of services.
    function listServices (callback) {
	httpget(irsUri("/api-javascript/events/services"),
		function (response) {callback(eval(response))});
    };

    // Call CALLBACK with one argument, an array of events.  CALLBACK will
    // be called repeatedly, every IRS.WEB::*KEEP-ALIVE-INTERVAL* seconds.
    function listEvents (callback) {
	httpget(irsUri("/api-javascript/events/events-info"),
		function (response) {callback(eval(response))},
		true, true);
    };

    function listGoalSlots (callback, ontology, goal) {
	url = irsUri("/api-javascript/goal/slots?ontology=")
	    + ontology + "&concept=" + goal;
	httpget(url, function (response) {callback(eval(response))});
    };

    // Achieve a goal
    function achieveGoal (callback, ontology, goal, slotValuePairs) {
	httpget(achieveGoalURL(ontology, goal, slotValuePairs), callback);
    };

    function achieveGoalURL (ontology, goal, slotValuePairs) {
	var slotArgs = "";
	for (var s=0; s<slotValuePairs.length; s++) {
	    slotArgs += "&" + encodeURIComponent(slotValuePairs[s].name)
		+ "=" + encodeURIComponent(slotValuePairs[s].value);
	}
	return irsUri("/api-rest/achieve-goal") + "?ontology=" + ontology
	    + "&goal=" + goal + slotArgs;
    };

    function ocmlQuery (callback,ontology, query) {
	httpget(irsUri("/api-rest/query?o=") + ontology + "&q=" + encodeURIComponent(query),
		callback);
    }

    function getClassTree(callback, ontology, filters) {
	httpget(irsUri("/api-javascript/class-tree?")
		+ "ontology=" + ontology
		+ "&filters=" + encodeURIComponent(filters),
		function (response) {callback(eval(response))});
    };

    function getWsmoEntitiesTree(ontology, filters, callback) {
	httpget(irsUri("/api-javascript/wsmo-entities-tree?")
		+ "ontology=" + ontology
		+ "&filters=" + encodeURIComponent(filters),
		function (response) {callback(eval(response)[0]);});
    };

    function getFunctionList(callback, ontology, filters) {
	httpget(irsUri("/api-javascript/function-list?ontology=") + ontology
		+ "&filters=" + encodeURIComponent(filters),
	        function (response) {callback(eval(response));});
    };

    function getConceptGraph(callback,ontology,concept) {
	httpget(irsUri("/api-javascript/graph/data?ontology=") + ontology
		+ "&concept=" + encodeURIComponent(concept),
	        function (response) {callback(eval(response));});
    }

    function getInstanceList(callback,ontology,filters) {
	httpget(irsUri("/api-javascript/instance-list?ontology=") + ontology
		+ "&filters=" + encodeURIComponent(filters),
	        function (response) {callback(eval(response));});
    }

    function getOntologyList(callback) {
	httpget(irsUri("/api-javascript/ontology-list"),
	        function (response) {callback(eval(response));});
    };

    function getRelationList(callback, ontology, filters) {
	httpget(irsUri("/api-javascript/relation-list?ontology=") + ontology
		+ "&filters=" + encodeURIComponent(filters),
		function (response) {callback(eval(response));});
    }

    function getRuleList(callback, ontology, filters) {
	httpget(irsUri("/api-javascript/rule-list?ontology=") + ontology
		+ "&filters=" + encodeURIComponent(filters),
		function (response) {callback(eval(response));});
    }

    /* XXX For some reason, Firefox doesn't like eval'ing singleton
       JSON objects. So, we'll just wrap it in an array, and read
       that. */
    function evalSingletonObject(str) {
      str = "[" + str + "]";
      val = eval(str);
      return val[0];
    }

    function getClass(callback, ontology, classname) {
      httpget(irsUri("/api-javascript/class?ontology=") + ontology
	      + "&class=" + encodeURIComponent(classname),
		function (response) {callback(evalSingletonObject(response));});
    };

    function getFunction(callback, ontology, func) {
	httpget(irsUri("/api-javascript/function?ontology=") + ontology
		+ "&function=" + encodeURIComponent(func),
		function (response) {callback(evalSingletonObject(response));});
    };

    function getInstance(callback, ontology, instance) {
	httpget(irsUri("/api-javascript/instance?ontology=") + ontology
		+ "&instance=" + encodeURIComponent(instance),
		function (response) {callback(evalSingletonObject(response));});
    };

    function getOntology(callback, ontology) {
	httpget(irsUri("/api-javascript/ontology?ontology=") + ontology,
		function (response) {callback(evalSingletonObject(response));});
    };

    function getRelation(callback, ontology, relation) {
	httpget(irsUri("/api-javascript/relation?ontology=") + ontology
		+ "&relation=" + encodeURIComponent(relation),
		function (response) {callback(evalSingletonObject(response));});
    };

    function getRule(callback, ontology, rule) {
	httpget(irsUri("/api-javascript/rule?ontology=") + ontology
		+ "&rule=" + encodeURIComponent(rule),
		function (response) {callback(evalSingletonObject(response));});
    };

    function updateServer(klass, ontology, type, callback) {
	httpget(irsUri("/irsb-update-server?class=") + klass
		+ "&onto=" + ontology + "&typeclass=" + encodeURIComponent(type),
		callback);
    };


    // These are the exported functions:
    return {listServices: listServices,
	    listEvents: listEvents,
	    listGoalSlots: listGoalSlots,
	    achieveGoal: achieveGoal,
	    ocmlQuery: ocmlQuery,
	    getWsmoEntitiesTree: getWsmoEntitiesTree,
	    getClass: getClass,
	    getOntology: getOntology,
	    updateServer: updateServer,
	    getClassTree: getClassTree,
	    getConceptGraph: getConceptGraph,
	    getFunction: getFunction,
	    getFunctionList: getFunctionList,
	    getRelation: getRelation,
	    getInstanceList: getInstanceList,
	    getInstance: getInstance,
	    getOntologyList: getOntologyList,
	    getRelationList: getRelationList,
	    getRule: getRule,
	    getRuleList: getRuleList,
	    irsUri: irsUri
	   };
} ();

// {{{ Lisp/JavaScript identifier munging

// Translate NAME to a JavaScript compatible name.
function lispNameToJs (name) {
    var cleanHyphen = name.replace(/-/g, "_");
    var cleanColon = cleanHyphen.replace(/:/g, "$");
    return cleanColon;
}

function JsNameToLisp (name) {
    var hypenated = name.replace(/_/g, "-");
    var coloned = hypenated.replace(/$/g, ":");
    return coloned;
}
// }}}

// {{{ Utilities

// XXX Not strictly part of the API, but we'll leave them here for the
// moment.

function replaceAll(string, oldString, newString) {
    return string.replace(eval("/"+oldString+"/g"), newString);
}

function xmlEscape(string) {
    return replaceAll(replaceAll(replaceAll(replaceAll(replaceAll(string,'&','&amp;'),'<','&lt;'), '>','&gt;'),'\'','&apos;'),'"','&quot;');
}


// }}}

// {{{ Gensym functionality

// For generatig sort-of unique identifiers.
function genGen() {
    var number = 1;
    return function () {
	var n = number++;
	return "gensym" + n;
    };
}

var gensym = genGen();
// }}}

// {{{ OCML
var ocml = function() {
    // {{{ OCML identifier handling

    // These are wrong!  At some point, when we enable the use of
    // namespaced identifiers, we will need to stop case-fiddling.
    // But if you use these functions instead of doing it by hand, it
    // should be easier to switch later.

    function internSymbol(str) {
	return str;
    }

    function externSymbol(str) {
	return str;
    }
    // }}}

    return {internSymbol: internSymbol,
	    externSymbol: externSymbol};
}();
// }}}

