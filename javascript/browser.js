// Copyright © 2007,2008 The Open University

/*
The code has been updated so that the current browser object is passed to the 'init' function whenever "YAHOO.util.Event.onContentReady" is called.
*/
var browser; //Holds a reference to the current browser instance

// Sort objects by their ‘name’ property.
/* function nameGreaterThan(a,b) {
    return (a.name > b.name);
} */

function nameGreaterThan(a,b) { //Neil B.: The original didn't work as expected was rewritten on 08/08/2008 to fit 'compareFunction()' spec at http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Global_Objects:Array:sort

    if (a.name < b.name)
		return -1;
	if (a.name > b.name)
		return 1;	
	return 0;	// a.name is equal to b.name
}

function replSubmitHook() {
    var inputarea = document.getElementById("repl-input-area");
    var query = inputarea.value;

    replOutput("OCML> " + query);
    irs.ocmlQuery(replOutput, browser.selectedOntology,  query);

    // Stop the form being submitted normally.
    return false;
}

function replOutput(string) {
    var outputArea = document.getElementById("repl-output-area");
    outputArea.value = outputArea.value + string + "\n";
    outputArea.scrollTop = outputArea.scrollHeight;
}

// Creates ontology selector with filter buttons.
function OntologyControl (browser) { 

	var self = this; /*According to (http://www.crockford.com/javascript/private.html), we should follow the convention
					       to add a 'self' (which is not accessible from outiside the Editor object but is global within the object) in order to make the current instance
 					       available to the inner methods. Apparently, this is a workaround for an error in the ECMAScript Language
					      Specification which causes this to be set incorrectly for inner functions.*/

    this.numFilters = 3;
    this.filterButtons = new Array(this.numFilters);

    this.resetFilters = function() {
	for (var i=0;i<this.numFilters;i++) {
	    this.filterButtons[i].set("checked", true);
	}
    };

    // Return array of bools showing current filter state.
    this.filters = function () {
	var result = new Array(self.numFilters);
	for (var i=0;i<self.numFilters;i++) {
	    result[i] = self.filterButtons[i].get("checked");
	}
	return result;
    };

    // Called on state change, to perform filter application.
    this.getcheckboxes = function() {
	browser.drawPanels(browser.selectedOntology);
    };

    this.drawFilterButtons = function(hole) {
	var self = this;
	var importedHoleId = gensym();
	var baseHoleId = gensym();
	var wsmoHoleId = gensym();

	document.getElementById(hole).innerHTML =
	    "<form><table><tr><td>Filter ontologies</td>"
	    + "<td><span id='" + importedHoleId + "'/><td>imported</td>"
	    + "<td><span id='" + baseHoleId + "'/></td><td>base</td>"
	    + "<td><span id='" + wsmoHoleId + "'/></td><td>wsmo</td></tr></table></form>";

	var callback = {fn: self.getcheckboxes};
	this.filterButtons[0] = new YAHOO.widget.Button({type: "checkbox", checked: true, container: importedHoleId, onclick: callback});
	this.filterButtons[1] = new YAHOO.widget.Button({type: "checkbox", checked: true, container: baseHoleId, onclick: callback});
	this.filterButtons[2] = new YAHOO.widget.Button({type: "checkbox", checked: true, container: wsmoHoleId, onclick: callback});
    };

    this.drawOntologySelector = function() { 
	
	var oMenu = new YAHOO.widget.Menu("mymenu", { visible: true });
	var button = new YAHOO.widget.Button({
	    label: "Select ontology",
	    id: "ontologySelectorButton",
	    type: "split",
	    menu: oMenu,
	    container: "ontology-selector-hole"});
	oMenu.subscribe("click", function onClick(p_sType, p_aArgs) {
	//By default, YUI Menu event handlers are called with two arguments:
	//a string representing the name of the event ('p_sType') and
	//an array of arguments sent when the event fired ('p_aArgs'). 
	//Events based on DOM events (e.g. "mouseover," "mouseout," "click," etc.) pass back the DOM event object to the listener as the first argument in the arguments array; 
	//if the target of the event was a MenuItem instance, it will be passed back as the second argument ('p_aArgs[1]'). (See http://developer.yahoo.com/yui/menu/)
            var oMenuItem = p_aArgs[1];
            if (oMenuItem) {
		var o = oMenuItem.cfg.getProperty("text"); 
		browser.selectOntology(o);
		button.set("label", o);
            }});
			
			
	return button; 
    };

    this.updateOntologySelectorMenu = function() {
	
	irs.getOntologyList(function(list) {
	    var menuItems = [];
	    for (var i=0; i<list.length; i++) {
		var item = list[i];
		menuItems.push({text: item.name, value: item.name}); 
	    }

	    var oMenu = button.get("menu");		
		
	    oMenu.addItems(menuItems);
	    oMenu.render("ontology-selector-hole"); //Original line was oMenu.render("ontologySelectorButton").  But that gave unpredictable results
	    oMenu.cfg.setProperty("visible", false); 
	});
    };

    this.init = function() {
	var hole = document.getElementById("ontology-control-hole");
	hole.innerHTML = "<table padding='0' border='0'><tr><td><span id='ontology-selector-hole'/></td><td align='left' id='filter-hole'></td></tr></table>";
	this.drawFilterButtons("filter-hole");
	button = this.drawOntologySelector(); 
    }; 
	
//The following line has been commented out because it is now called from within 'browser.init'
    //this.init(); 
}

function addNavbar(el) {
    var history = browser.inspectorHistory;
    var back = history.lookBack();
    var forward = history.lookForward();

    this.backButtonId = gensym();
    this.forwardButtonId = gensym();

    el.innerHTML += "<div id='buttonHole'></div>";

    var backButton = new YAHOO.widget.Button({
	label: "Back", id: this.backButtonId,
      	container: "buttonHole",
      	onclick: {fn: function (event) {goBack();}}});
    if (!back) {
            backButton.set("disabled", true);
    }

    var forwardButton = new YAHOO.widget.Button({
	label: "Forward", id: this.forwardButtonId,
      	container: "buttonHole",
      	onclick: {fn: function (event) {goForward();}}});

    if (!forward) {
            forwardButton.set("disabled", true);
    }
    el.innerHTML += "<hr/>";
}

function goBack() {
    var history = browser.inspectorHistory;
    history.goBack();
    var place = history.current();
    inspectBypassHistory(place.ontology,place.object,place.type);
}

function goForward() {
    var history = browser.inspectorHistory;
    history.goForward();
    var place = history.current();
    inspectBypassHistory(place.ontology,place.object,place.type);
}

function inspect(object, ontology, type) {
  browser.inspectorHistory.add({object: object, ontology: ontology, type: type});
  inspectBypassHistory(ontology,object,type);
}

// Create a clickable <span> for THING which runs inspect.
function inspectLink(ontology, thing, klass) {
    var cssClass;
    if (klass == 'CLASS') {
	cssClass = "ocmlClassLink";
    } else if (klass == 'FUNCTION') {
	cssClass = "ocmlFunctionLink";
    } else if (klass == 'INSTANCE') {
	cssClass = "ocmlInstanceLink";
    } else if (klass == 'RELATION') {
	cssClass = "ocmlRelationLink";
    } else if (klass == 'RULE') {
	cssClass = "ocmlRuleLink";
    } else if (klass == 'ONTOLOGY') {
	cssClass = "ocmlOntologyLink";
	return  "<span class='" + cssClass + "' onclick=\"inspect('" + thing
	    + "','" + thing + "','" + klass +"')\">" + thing + "</span>";
    } else {
	cssClass = klass;
    }
    var str = "<span class='" + cssClass + "' onclick=\"inspect('" + thing
	+ "','" + ontology + "','" + klass +"')\">" + thing + "</span>";
    return str;
}

function inspectLinko(thing, klass) {
  return inspectLink(browser.selectedOntology, thing, klass);
}

function renderClassSlots(slots) {
    var el = "<table><tr><td class='field' width='40%'>Slot name</td><td class='field' width='40%'>Type</td><td class='field'>Cardinality</td></tr>";
    for (var i=0; i<slots.length; i++) {
	var slot = slots[i];
	el += "<tr>";
	el += "<td>" + inspectLinko(slot.name,'RELATION') + "</td>";
	el += "<td>" + inspectLinko(slot.type,'CLASS') + "</td>";
	el += "<td>" + (slot.cardinalityMin || '0') + " → "
	    + (slot.cardinalityMax || '∞') + "</td>";
	if (slot.values.length > 0) {
	    el += "</tr>";
	    for (v=0; v<slot.values.length; v++) {
		el += "<tr>";
		el += "<td class='field-small'>Value</td>";
		el += "<td colspan='2'>" + slot.values[v] + "</td></tr>";
	    }
	}
	if (slot.documentation) {
	    el += "<tr><td class='field-small'>Documentation</td><td colspan='2'>" + slot.documentation + "</td></tr>";
	}
    }
    el += "</table>";
    return el;
}

function memberOf(el,array) {
  for (var i=0; i<array.length; i++) {
    if (el == array[i]) {
      return true;
    }
  }
  return false;
}

function GoalPanel(ontology, goal) {
  this.ontology = ontology;
  this.goal = goal;
  this.goalClass;
  this.panel;
  this.id = gensym();
  this.innerFieldId = gensym();
  this.achieveButtonId = gensym();

  this.draw = function() {
    this.panel = new YAHOO.widget.Panel(this.id, {
		constraintoviewport: true,
		underlay:"shadow",
		close:true,
		fixedcenter: true,
		visible:false,
		draggable:true,
		width: "400px"});
    this.panel.setHeader("Achieve goal | " + this.ontology + ":" + this.goal);
    this.drawBody();
      this.panel.render(theActive("browser-table"));
    this.panel.show();
      var resize = new YAHOO.util.Resize(this.id, {handles: ['br']});
  };

  this.drawBodyCallback = function(klassData) {
    var self = this;
    var klass = new OcmlClass(klassData);
    this.goalClass = klass;
    var el = document.getElementById(this.innerFieldId);
    var html = "";

    var inputRoles = klass.getSlotClassValues('HAS-INPUT-ROLE');
    html += "<form name='" + this.id + "_form'><table>";
    for (var i=0; i<inputRoles.length; i++) {
      var role = inputRoles[i];
	html += "<tr><td width=90>" + role + "</td>";
	html += "<td><input type='text' size='25' name='" + this.formSlotName(role) +"'/></td>" ;
	html += "<td>" + inspectLinko(klass.getSlotType(role), 'CLASS') + "</td>";
	html += "</tr>";
      }
      html += "</table></form><br/><div style='display: table-cell; width: 100%'></div><div style='display: table-cell'id='" + this.achieveButtonId + "-hole'/>"; //Neil B. - Added code to make the button display to the right of the panel
      el.innerHTML = html;

      var achieveGoalButton = new YAHOO.widget.Button({
	label: "Achieve", id: this.achieveButtonId,
      	container: this.achieveButtonId + "-hole",
      	onclick: {fn: function (event) {self.achieveGoal();}}});
  };

  this.formSlotName = function(slot) {
    return (this.id + "_inputrole_" + lispNameToJs(slot));
  };

  this.getFieldValue = function (slot) {
    var evalee = "document." + this.id + "_form." + this.formSlotName(slot) + ".value";
    return eval(evalee);
  };

  this.drawBody = function () {
    var self = this;
    var html = "<div id='" + this.innerFieldId + "'/>";
    this.panel.setBody(html);
    irs.getClass(function (data) {self.drawBodyCallback(data);},
		 this.ontology, this.goal);
  };

  /* Collect the field values from the form and make a request to the
   server. */
  this.achieveGoal = function() {
    var self = this;
    var slotValuePairs = new Array();
    var inputRoles = this.goalClass.getSlotClassValues('HAS-INPUT-ROLE');
    for (i=0; i<inputRoles.length; i++) {
          slotValuePairs[i] = {name: inputRoles[i],
			   value: this.getFieldValue(inputRoles[i])};
    }
    irs.achieveGoal(function (data) {self.goalAchievedCallback(data);},
		    this.ontology,this.goal,slotValuePairs);
  };

//Modified 06-Oct-08 by Neil B. so that output role is part of achieve goal result display
  this.goalAchievedCallback = function(data) {
  	var outputRole = this.goalClass.getSlotClassValues('HAS-OUTPUT-ROLE');
    document.getElementById(this.innerFieldId).innerHTML =
		'<div>' + outputRole[0] + '</div>' + 
		'<textarea>'+ data + '</textarea>';
	
	
  };

};

function renderClassInspection(klass) {
 var html = "<h3>Class: " + klass.name + "</h3>";

 if (memberOf('GOAL', klass.superClasses)) {
 var achieveGoalButton = new YAHOO.widget.Button({
	label: "Achieve goal", id: "achieveGoalButton",
	container: "buttonHole",
	onclick: {fn: function (event) {
 var gp = new GoalPanel(browser.selectedOntology, klass.name);
 gp.draw();
 }}});
 }

 html += "<table><tr><td class='field' width='20%'>Home ontology</td><td>";
 html += inspectLinko(klass.homeOntology, 'ONTOLOGY') + "</td></tr>";
 html += "<tr><td class='field'>Superclasses</td><td>";
 for (var i=0; i<klass.superClasses.length; i++) {
 html += inspectLinko(klass.superClasses[i],'CLASS') + " ";
 }
 html += "</td></tr>";
 html += "<tr><td class='field'>Subclasses</td><td>";
 for (var i=0; i<klass.subClasses.length; i++) {
 html += inspectLinko(klass.subClasses[i],'CLASS') + " ";
 }
 html += "</td></tr>";
 html += "<tr><td class='field'>Documentation</td><td>"+ klass.documentation + "</td></tr>";
 html += "</table>";

 html += "<br>";

 html += renderClassSlots(klass.slots);

 // If there's an achieve-button-hole element
 // here, add an effective button to it.
 if (document.getElementById("achieve-goal-hole")) {
	var achieveGoalButton = new YAHOO.widget.Button({
	label:"Achieve goal", id:"achieveGoalButton",
	container:"achieve-goal-hole",
	onclick: {fn: function (event) {
 self.location = irs.irsUri("/irs/goal?ontology=") + ontology
 + "&concept=" + object;
 }}});
	}
 document.getElementById("info").innerHTML += html;
 }

function renderFunctionInspection(functionData) {
    var fun = new OcmlFunction(functionData);
    var html = "<h3>Function: " + fun.name + "</h3>";
    html += "<table>";
    html += "<tr><td class='field'>Home ontology</td><td>"
	+ inspectLink(fun.homeOntology,fun.homeOntology,'ONTOLOGY') + "</td></tr>";
    html += "<tr><td class='field'>Documentation</td><td>"+ fun.documentation + "</td></tr>";
    html += "<tr><td class='field'>Schema</td><td>(";
    for (var i=0; i<fun.schema.length; i++) {
	html += fun.schema[i];
	if (fun.schema.length - i > 1) {
	    html += " ";
	}
    }
    html += ")</td></tr>";

    if (fun.constraint) {
	html += "<td><tr class='field'>constraint</td><td>"
	+ fun.constraint + "</td></tr>";
    }

    if (fun.definition) {
	html += "<td><tr class='field'>definition</td><td>"
	+ fun.definition + "</td></tr>";
    }

    if (fun.body) {
	html += "<td><tr class='field'>body</td><td>"
	+ fun.body + "</td></tr>";
    }

    if (fun.lispFun) {
	html += "<td><tr class='field'>lisp-fun</td><td>"
	+ fun.lispFun + "</td></tr>";
    }


    html += "</table>";

    document.getElementById("info").innerHTML += html;
 }

function renderInstanceInspection(instanceData) {
    var inst = new OcmlInstance(instanceData);
    var html = "<h3>Instance: " + inst.name + "</h3>";
    html += "<table>";
    html += "<tr><td class='field'>Class</td><td>"
	+ inspectLinko(inst.class,'CLASS') + "</td></tr>";
    html += "<tr><td class='field'>Home ontology</td><td>"
	+ inspectLink(inst.homeOntology,inst.homeOntology,'ONTOLOGY') + "</td></tr>";
    html += "<tr><td class='field'>Documentation</td><td>"+ inst.documentation + "</td></tr>";
    html += "</table>";

    html += "<h4>Slots</h4><table>";
    for (s=0; s<inst.slots.length; s++) {
	var slot = inst.slots[s];
	html += "<tr><td class='field'>" + slot.name + "</td>";
	if (slot.values.length > 0) {
	    html += "<td>" + slot.values[0] + "</td></tr>";
	    for (v=1; v<slot.values.length; v++) {
		html += "<tr><td/><td>" + slot.values[v] + "</td></tr>";
	    }
	}
    }
    html += "</table>";

    document.getElementById("info").innerHTML += html;
}

function renderRelationInspection(relationData) {
    var rel = new OcmlRelation(relationData);
    var html = "<h3>Relation: " + rel.name + "</h3>";
    html += "<table>";
    html += "<tr><td class='field'>Home ontology</td><td>"
	+ inspectLink(rel.homeOntology,rel.homeOntology,'ONTOLOGY') + "</td></tr>";
    html += "<tr><td class='field'>Documentation</td><td>"+ rel.documentation + "</td></tr>";
    html += "<tr><td class='field'>Schema</td><td>(";
    for (var i=0; i<rel.schema.length; i++) {
	html += rel.schema[i];
	if (rel.schema.length - i > 1) {
	    html += " ";
	}
    }
    html += ")</td></tr>";

    html += "<tr><td class='field'>Local slot of</td><td>";
    for (i=0; i<rel.localSlotOf.length; i++) {
	html += inspectLinko(rel.localSlotOf[i],'CLASS') + " ";
    }
    html += "</td></tr>";

    html += "<tr><td class='field'>Slot of</td><td>";
    for (i=0; i<rel.slotOf.length; i++) {
	html += inspectLinko(rel.slotOf[i],'CLASS') + " ";
    }
    html += "</td></tr>";

    if (rel.constraint) {
	html += "<td><tr class='field'>constraint</td><td>"
	+ rel.constraint + "</td></tr>";
    }

    if (rel.iffDef) {
	html += "<td><tr class='field'>iff-def</td><td>"
	+ rel.iffDef + "</td></tr>";
    }

    if (rel.sufficient) {
	html += "<td><tr class='field'>sufficient</td><td>"
	+ rel.sufficient + "</td></tr>";
    }

    if (rel.proveBy) {
	html += "<td><tr class='field'>prove-by</td><td>"
	+ rel.proveBy + "</td></tr>";
    }

    if (rel.lispFun) {
	html += "<td><tr class='field'>lisp-fun</td><td>"
	+ rel.lispFun + "</td></tr>";
    }

    html += "<tr><td class='field'>Associated rules</td><td>";
    for (i=0; i<rel.associatedRules.length; i++) {
	html += rel.associatedRules[i] + inspectLinko(rel.name,'RULES') + " ";
    }
    html += "</td></tr>";

    html += "<tr><td class='field'>Instances</td><td>";
    for (i=0; i<rel.instances.length; i++) {
	html += rel.instances[i] + " ";
    }
    html += "</td></tr>";

    html += "</table>";

    document.getElementById("info").innerHTML += html;
 }

function renderRuleInspection(ruleData) {
    var rule = new OcmlRule(ruleData);
    var html = "<h3>Rule: " + rule.name + "</h3>";
    html += "<table>";
    html += "<tr><td class='field'>Direction</td><td>" + rule.direction;
    html += "<tr><td class='field'>Documentation</td><td>"
	+ rule.documentation + "</td></tr>";
    html += "<tr><td class='field'>Home ontology</td><td>"
	+ inspectLinko(rule.homeOntology,'ONTOLOGY') + "</td></tr>";
    if (rule.direction == 'BACKWARD') {
	html += "<tr><td class='field'>Defines relation</td><td>"
	    + rule.definesRelation + "</td></tr>";
    }
    html += "</table>";

    document.getElementById("info").innerHTML += html;
 }

function renderOntologyInspection(ontologyData) {
    var ont = new OcmlOntology(ontologyData);
    var html = "<h3>Ontology: " + ont.name + "</h3>";

    html += "<table>";
    html += "<tr><td class='field'>Documentation</td><td>"+ ont.documentation + "</td></tr>";
    html += "<tr><td class='field'>Ontology type</td><td>"+ ont.type + "</td></tr>";
    html += "<tr><td class='field'>Namespace URI</td><td>"+ ont.namespaceUri + "</td></tr>";
    html += "<tr><td class='field'>Includes</td><td>";
    for (var i=0; i<ont.includes.length; i++) {
	html += inspectLink(ont.includes[i],ont.includes[i],'ONTOLOGY') + " ";
    }
    html += "</td></tr>";
    html += "<tr><td class='field'>Included by</td><td>";
    for (var i=0; i<ont.includedBy.length; i++) {
	html += inspectLink(ont.includes[i],ont.includedBy[i],'ONTOLOGY') + " ";
    }
    html += "</td></tr>";
    html += "<tr><td class='field'>Namespace prefixes</td><td><table>";
    for (i=0; i<ont.namespacePrefixes.length; i++) {
	html += "<tr><td>" + ont.namespacePrefixes[i].prefix
	    + "</td><td>" + ont.namespacePrefixes[i]. namespace + "</td></tr>";
    }
    html += "</table></td></tr>";
    html += "<tr><td class='field'>Author</td><td>"+ ont.author + "</td></tr>";
    html += "<tr><td class='field'>Editors</td><td>";
    for (var i=0; i<ont.editors.length; i++) {
	html += ont.editors[i] + " ";
    }
    html += "</td></tr>";
    html += "<tr><td class='field'>Files</td><td>";
    for (var i=0; i<ont.files.length; i++) {
	html += ont.files[i] + " ";
    }
    html += "</td></tr>";

    html += "</table>";

    document.getElementById("info").innerHTML += html;
 }

function inspectBypassHistory(ontology,object,type) {
    function callback(renderer) {
	return function callback(data) {
	    var el = document.getElementById("info");
	    el.innerHTML = "";
	    addNavbar(el);
	    renderer(data);
	};
    }
    if (type == 'CLASS') {
	irs.getClass(callback(renderClassInspection),ontology,object);
    } else if (type == 'FUNCTION') {
	irs.getFunction(callback(renderFunctionInspection),ontology,object);
    } else if (type == 'INSTANCE') {
	irs.getInstance(callback(renderInstanceInspection),ontology,object);
    } else if (type == 'ONTOLOGY') {
	irs.getOntology(callback(renderOntologyInspection),ontology,object); 
    } else if (type == 'RELATION') {
	irs.getRelation(callback(renderRelationInspection),ontology,object);
    } else if (type == 'RULE') {
	irs.getRule(callback(renderRuleInspection),ontology,object);
    }
    rightTab.set('activeIndex', 0);
}

function drawRelationList(data,ontology) {
 var pane = document.getElementById("relationlist");
 pane.innerHTML = "<div><div id='relationList'></div></div>";

 data = data.sort(nameGreaterThan);
 var tree = new YAHOO.widget.TreeView("relationList");
 var root = tree.getRoot();
 for (var i=0; i<data.length; i++) {
 var obj = {label: ocml.internSymbol(data[i].name),
 href: "javascript:inspect('" + data[i].name + "', '" + ontology + "', 'RELATION')"};
	var node = new YAHOO.widget.TextNode(obj, root, false);
 }
    blessThisTree(tree);
    tree.render();
}

function drawRuleList(data,ontology) {
    var pane = document.getElementById("rulelist");
    pane.innerHTML = "<div><div id='ruleList'></div></div>";

    data = data.sort(nameGreaterThan);
    var tree = new YAHOO.widget.TreeView("ruleList");
    var root = tree.getRoot();
    for (var i=0; i<data.length; i++) {
	var obj = {label: ocml.internSymbol(data[i].name),
	href: "javascript:inspect('" + data[i].name + "', '"
		   + ontology + "', 'RULE')"};
	var node = new YAHOO.widget.TextNode(obj, root, false);
    }
    blessThisTree(tree);
    tree.render();
}

function drawFunctionList(data,ontology) {
    var pane = document.getElementById("functionlist");
    pane.innerHTML = "<div><div id='functionList'></div></div>";

    data = data.sort(nameGreaterThan);
    var tree = new YAHOO.widget.TreeView("functionList");
    var root = tree.getRoot();
    for (var i=0; i<data.length; i++) {
	var obj = {label: ocml.internSymbol(data[i].name),
	href: "javascript:inspect('" + data[i].name + "', '" + ontology + "', 'FUNCTION')"};
	var node = new YAHOO.widget.TextNode(obj, root, false);
    }
    blessThisTree(tree);
    tree.render();
}

// {{{ Class tree
function drawClassTree(data,ontology) {
 var container = document.getElementById("classtree");
 container.innerHTML = "<div><div id='treewidget'></div></div>";
 
 
 var tree = new YAHOO.widget.TreeView("treewidget");
 var root = tree.getRoot();
 
 drawTree(root,ontology,data); 
 
    blessThisTree(tree);
    tree.render();
}

function drawTree(root,ontology,data) {
 if (data.length > 0) {
 
	data = data.sort(nameGreaterThan);
	
	for (var i=0; i<data.length; i++) {
 var obj = {label: ocml.internSymbol(data[i].name),
	    href: "javascript:inspect('" + data[i].name + "','"
		+ ontology + "','CLASS')"};
 var node = new YAHOO.widget.TextNode(obj, root, false);
 drawTree(node,ontology,data[i].children);
 
	}
 }
}

// }}}
// {{{ WSMO entities tree

function drawWsmoEntitiesTree(data,ontology) {
 var container = document.getElementById("classlist");
 container.innerHTML = "<div><div id='wsmotreewidget'></div></div>";
 var tree = new YAHOO.widget.TreeView("wsmotreewidget");
 var root = tree.getRoot();
 drawWsmoTree(root,ontology,data);
    blessThisTree(tree);
    tree.render();
}

function inspectWsmoClass(ontology,symbol) {
    inspect(symbol,ontology,'CLASS');
    updateServerVis(symbol,ontology,'CLASS');
}

function inspectWsmoInstance(ontology,symbol) {
	inspect(symbol, ontology, 'INSTANCE');
}

function drawWsmoTree(root,ontology,data) {
 function branch(data,label,func) {
 var branchTopLabel = {label: label + "[" + data.length + "]"};
	var branchTop = new YAHOO.widget.TextNode(branchTopLabel, root, false);
	for (var i=0; i<data.length; i++) {
 var obj = {label: ocml.internSymbol(data[i].name),
 href: "javascript:"+ func +"('" + ontology + "','"
 + ocml.externSymbol(data[i].name) + "')"};
 var node = new YAHOO.widget.TextNode(obj, branchTop, false);
	}
 }

 
 branch(data.goals,"Goals","inspectWsmoClass");
 branch(data.services,"Services","inspectWsmoClass");
 branch(data.mediators,"Mediators","inspectWsmoClass");
 branch(data.concepts,"Concepts","inspectWsmoClass");
 branch(data.instances,"Instances","inspectWsmoInstance");
}

// }}}

//This function has been amended to handle instance data as a JSON list [{name: "Instance1", homeOntology: "home-ont"}, {name: "Instance2", homeOntology: "home-ont"}...]
//Just needed to change data[i] to data[i].name
function drawInstanceList(data,ontology) {
    var container = document.getElementById("instancelist");
    var tree = new YAHOO.widget.TreeView("instancelist");
    var root = tree.getRoot();

    data = data.sort(function(a,b){return (a>b);});
    for (var i=0; i<data.length; i++) {
	var obj = {label: ocml.internSymbol(data[i].name),
	href: "javascript:inspect('" + ocml.externSymbol(data[i].name)
		   + "', '" + ontology + "', 'INSTANCE')"};
	var node = new YAHOO.widget.TextNode(obj, root, false);
    }
    blessThisTree(tree);
    tree.render();
}

function drawInstancePanel(ontology,klass) {
    irs.getInstanceList(function(data) {
	drawInstanceList(data,ontology,klass);
	}, ontology,klass);
}

/* UPDATES THE SERVER VISUALIZATION */
function updateServerVis (selection, ontology, type, select) {
    irs.updateServer(selection, ontology, type,
		     function(data) {
			 document.getElementById("servervis").innerHTML = data;
		     });
    if (select) {
	rightTab.set('activeIndex', 1);
    }
}

function viewOcml(ontology) {
    window.open (irs.irsUri("/irs/ontology/ocml/") + ontology + "?format=raw",
		 "OCML view of " + ontology,
		 'status=0,toolbar=0,menubar=0,scrollbars=1,resizable=1,width=400,height=400');
}
function viewRdf(ontology) {
    window.open (irs.irsUri("/irs/ontology/rdf/") + ontology + "?format=raw",
		 "RDF view of " + ontology,
		 'status=0,toolbar=0,menubar=0,scrollbars=1,resizable=1,width=400,height=400');
}

// {{{ Menubar

var menuData =
    [{text: "View",
      submenu: {id: "aMenu", itemdata:
		[{text: "Ontology as OCML", onclick: {fn: function () {viewOcml(browser.selectedOntology);}}},
		{text: "Ontology as RDF", onclick: {fn: function () {viewRdf(browser.selectedOntology);}}}]}}
    ];

function drawMenubar () {
    var oMenuBar = new YAHOO.widget.MenuBar("mymenubar",
	{lazyload: true, itemdata: menuData});

    oMenuBar.render(document.getElementById("menubar-hole"));
    /*
        Add a "show" event listener that keeps the left-most
        submenu against the left edge of the browser viewport.
    */
    function onSubmenuShow() {
        if (this.id == "yahoo") {
            this.cfg.setProperty("x", 0);
        }
    }

    // Subscribe to the "show" event for each submenu
    oMenuBar.subscribe("show", onSubmenuShow);
};

// }}}

// {{{ History

// How does a history work in a browser?  I've never been very happy
// with it.  It should be more like a tree.  But it's probably going
// to have to be linear.  So, when we wind back, and then change
// direction, we have to change the ‘future’.
function History () {
    this.places = new Array();
    this.at = -1;

    this.add = function(place) {
	this.places[++this.at] = place;
	// Invalidate the ex-future places.
	this.length = this.at + 1;
    }

    this.current = function() {
	return this.places[this.at];
    }

    this.lookBack = function() {
	if (this.at < 1) {
	    return null;
	} else {
	    return this.places[this.at-1];
	}
    }

    this.lookForward = function() {
	if (this.at + 1 < this.places.length) {
	    return this.places[this.at+1];
	} else {
	    return null;
	}
    }

    this.goBack = function() {
	if (this.lookBack()) {
	    return this.places[--this.at];
	} else {
	    return null;
	}
    }

    this.goForward = function() {
	if (this.lookForward()) {
	    return this.places[++this.at];
	} else {
	    return null;
	}
    }
}

// }}}

// {{{ Browser
function Browser() {

	this.inspectorHistory = new History();
    this.leftTab;
    this.rightTab;
    this.selectedOntology;
    this.ontologySelector; 

    this.init = function() {	

	drawMenubar();
	leftTab = new YAHOO.widget.TabView('leftTab');
	leftTab.set('activeIndex', 0);
	rightTab = new YAHOO.widget.TabView('rightTab');
	rightTab.set('activeIndex', 0);
	replTab = new YAHOO.widget.TabView('repl-tab');

	// Capture the OCML listener form's submit event and subvert it to our cause.
	var form = document.getElementById("repl-form");
	form.onsubmit = replSubmitHook;
	
	browser.ontologySelector = new OntologyControl(browser); 
	browser.ontologySelector.init(); 
	browser.ontologySelector.updateOntologySelectorMenu(); 
	
    };

    this.drawPanels = function(ontology) {
	irs.getClassTree(function(data) {
	    drawClassTree(data, ontology);
	}, ontology, this.ontologySelector.filters());
	irs.getRelationList(function(data) {
	    drawRelationList(data,ontology);
	}, ontology, this.ontologySelector.filters());
	irs.getRuleList(function(data) {
	     drawRuleList(data,ontology);
	}, ontology, this.ontologySelector.filters());
	irs.getFunctionList(function(data) {
	    drawFunctionList(data,ontology);
	}, ontology, this.ontologySelector.filters());
	irs.getInstanceList(function(data) {drawInstanceList(data,ontology);},
	    ontology, this.ontologySelector.filters());
	irs.getWsmoEntitiesTree(ontology, this.ontologySelector.filters,
				function(data) {
				    drawWsmoEntitiesTree(data,ontology);
				});
    }

    this.selectOntology = function(ontology) {
	this.selectedOntology = ontology;
	inspect(ontology,ontology,'ONTOLOGY');
	this.ontologySelector.resetFilters();
	this.drawPanels(ontology);
	leftTab.set('activeIndex', 0);
    }

}
// }}}

// Somewhere between YUI 2.5.2 and 2.7.0, tree nodes with hrefs
// stopped working...  This seems to fix it.
function blessThisTree(tree) {
    tree.subscribe("clickEvent", function(node) {
	return false;
    });
}

function theActive(thingName) {
    var activeThingExtension = "";
    if (browser.renderer) {
	    activeThingExtension = browser.renderer.activeWindowID;
    }
    return thingName + activeThingExtension;
}
