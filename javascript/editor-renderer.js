//The following function definitions override the definitions in BROWSER.JS
//The main change is that the 'render' functions now need to know the
//ID of the ontology window they will be rendering to

//What about introducing a Renderer() class that redefines all the rendering functions
//The current instance of Renderer would also store the value of the currently active
//ontology window, and that value would be globally available to all the renderer functions
//This would mean I don't have to change the parameter list for the renderer functions to include
//the ID of the current ontology window.

function Renderer(editor){
	
	var self = this;
	
//Eventually need to move this activeWindow stuff into Editor instead of Renderer	
	var _activeWindow = editor.ontologyWindows[0]; //the 'Default Window'
	var _activeWindowID = '';
	
	self.activeWindowID = _activeWindowID; //XXX Hack to make it easier to access the currently active window
	self.activeWindow = _activeWindow; //XXX Hack to make it easier to access the currently active window
	
		
	this.setActiveWindowID = function() {
		var _activeWindowPanel = editor.ontologyWindowsManager.getActive();
		
		for (i = 0; i < editor.ontologyWindows.length; i++) {
			if (_activeWindowPanel == editor.ontologyWindows[i].panel) {
				_activeWindow = editor.ontologyWindows[i];
				_activeWindowID = _activeWindow.windowID;
				
				self.activeWindowID = _activeWindowID; //XXX Hack
				self.activeWindow = _activeWindow; //XXX Hack
			}
		}
	}	

	window.renderClassInspection = function (klass){
	
		var html = "<h3>Class: " + klass.name + "</h3>";
		
		if (memberOf('GOAL', klass.superClasses)) {
		
			var achieveGoalButton = new YAHOO.widget.Button({
				label: "Achieve goal",
				id: "achieveGoalButton",
				container: "buttonHole"+_activeWindowID,
				onclick: {
					fn: function(event){
						var gp = new GoalPanel(self.activeWindow.windowTitle, klass.name);
						gp.draw();
					}
				}
			});
		}
		
		html += "<table><tr><td class='field' width='20%'>Home ontology</td><td>";
		html += inspectLinko(klass.homeOntology, 'ONTOLOGY') + "</td></tr>";
		html += "<tr><td class='field'>Superclasses</td><td>";
		
		for (var i = 0; i < klass.superClasses.length; i++) {
			html += inspectLinko(klass.superClasses[i], 'CLASS') + " ";
		}
		
		html += "</td></tr>";
		html += "<tr><td class='field'>Subclasses</td><td>";
		
		for (var i = 0; i < klass.subClasses.length; i++) {
			html += inspectLinko(klass.subClasses[i], 'CLASS') + " ";
		}
		
		html += "</td></tr>";
		html += "<tr><td class='field'>Documentation</td><td>" + (klass.documentation || "") + "</td></tr>";
		html += "</table>";
		
		html += "<br>";
		
		html += renderClassSlots(klass.slots);
		

		//This line is the behaviour being overriden
		document.getElementById("info"+_activeWindowID).innerHTML += html;

	}
	
	window.renderFunctionInspection = function(functionData){
	
		var fun = new OcmlFunction(functionData);
		var html = "<h3>Function: " + fun.name + "</h3>";
		html += "<table>";
		html += "<tr><td class='field'>Home ontology</td><td>" +
		inspectLink(fun.homeOntology, fun.homeOntology, 'ONTOLOGY') +
		"</td></tr>";
		html += "<tr><td class='field'>Documentation</td><td>" + (fun.documentation || "") + "</td></tr>";
		html += "<tr><td class='field'>Schema</td><td>(";
		
		for (var i = 0; i < fun.schema.length; i++) {
		
			html += fun.schema[i];
			
			if (fun.schema.length - i > 1) {
				html += " ";
			}
		}
		
		html += ")</td></tr>";
		
		if (fun.constraint) {
			html += "<td><tr class='field'>constraint</td><td>" +
			fun.constraint +
			"</td></tr>";
		}
		
		if (fun.definition) {
			html += "<td><tr class='field'>definition</td><td>" +
			fun.definition +
			"</td></tr>";
		}
		
		if (fun.body) {
			html += "<td><tr class='field'>body</td><td>" +
			fun.body +
			"</td></tr>";
		}
		
		if (fun.lispFun) {
			html += "<td><tr class='field'>lisp-fun</td><td>" +
			fun.lispFun +
			"</td></tr>";
		}
		
		
		html += "</table>";
		
		//This line is the behaviour being overriden
		document.getElementById("info"+_activeWindowID).innerHTML += html;

	}
	
	window.renderInstanceInspection = function(instanceData){
	
		var inst = new OcmlInstance(instanceData);
		var html = "<h3>Instance: " + inst.name + "</h3>";
		html += "<table>";
		html += "<tr><td class='field'>Class</td><td>" +
		inspectLinko(inst.class, 'CLASS') +
		"</td></tr>";
		html += "<tr><td class='field'>Home ontology</td><td>" +
		inspectLink(inst.homeOntology, inst.homeOntology, 'ONTOLOGY') +
		"</td></tr>";
		html += "<tr><td class='field'>Documentation</td><td>" + (inst.documentation || "") + "</td></tr>";
		html += "</table>";
		
		html += "<h4>Slots</h4><table>";
		
		for (s = 0; s < inst.slots.length; s++) {
			var slot = inst.slots[s];
			html += "<tr><td class='field'>" + slot.name + "</td>";
			
			if (slot.values.length > 0) {
				html += "<td>" + slot.values[0] + "</td></tr>";
				
				for (v = 1; v < slot.values.length; v++) {
					html += "<tr><td/><td>" + slot.values[v] + "</td></tr>";
				}
			}
		}
		
		html += "</table>";
		
		//This line is the behaviour being overriden
		document.getElementById("info"+_activeWindowID).innerHTML += html;

	}
	
	window.renderRelationInspection = function(relationData, windowID){
	
		var rel = new OcmlRelation(relationData);
		var html = "<h3>Relation: " + rel.name + "</h3>";
		html += "<table>";
		html += "<tr><td class='field'>Home ontology</td><td>" +
		inspectLink(rel.homeOntology, rel.homeOntology, 'ONTOLOGY') +
		"</td></tr>";
		html += "<tr><td class='field'>Documentation</td><td>" + (rel.documentation || "") + "</td></tr>";
		html += "<tr><td class='field'>Schema</td><td>(";
		
		for (var i = 0; i < rel.schema.length; i++) {
		
			html += rel.schema[i];
			
			if (rel.schema.length - i > 1) {
				html += " ";
			}
		}
		
		html += ")</td></tr>";
		
		html += "<tr><td class='field'>Local slot of</td><td>";
		
		for (i = 0; i < rel.localSlotOf.length; i++) {
			html += inspectLinko(rel.localSlotOf[i], 'CLASS') + " ";
		}
		
		html += "</td></tr>";
		
		html += "<tr><td class='field'>Slot of</td><td>";
		
		for (i = 0; i < rel.slotOf.length; i++) {
			html += inspectLinko(rel.slotOf[i], 'CLASS') + " ";
		}
		
		html += "</td></tr>";
		
		if (rel.constraint) {
			html += "<td><tr class='field'>constraint</td><td>" +
			rel.constraint +
			"</td></tr>";
		}
		
		if (rel.iffDef) {
			html += "<td><tr class='field'>iff-def</td><td>" +
			rel.iffDef +
			"</td></tr>";
		}
		
		if (rel.sufficient) {
			html += "<td><tr class='field'>sufficient</td><td>" +
			rel.sufficient +
			"</td></tr>";
		}
		
		if (rel.proveBy) {
			html += "<td><tr class='field'>prove-by</td><td>" +
			rel.proveBy +
			"</td></tr>";
		}
		
		if (rel.lispFun) {
			html += "<td><tr class='field'>lisp-fun</td><td>" +
			rel.lispFun +
			"</td></tr>";
		}
		
		html += "<tr><td class='field'>Associated rules</td><td>";
		
		for (i = 0; i < rel.associatedRules.length; i++) {
			html += rel.associatedRules[i] + inspectLinko(rel.name, 'RULES') + " ";
		}
		
		html += "</td></tr>";
		
		html += "<tr><td class='field'>Instances</td><td>";
		
		for (i = 0; i < rel.instances.length; i++) {
			html += rel.instances[i] + " ";
		}
		
		html += "</td></tr>";
		
		html += "</table>";
		
		//This line is the behaviour being overriden
		document.getElementById("info"+_activeWindowID).innerHTML += html;

	}
	
	window.renderRuleInspection = function(ruleData, windowID){
	
		var rule = new OcmlRule(ruleData);
		var html = "<h3>Rule: " + rule.name + "</h3>";
		html += "<table>";
		html += "<tr><td class='field'>Direction</td><td>" + rule.direction;
		html += "<tr><td class='field'>Documentation</td><td>" +
		(rule.documentation || "") +
		"</td></tr>";
		html += "<tr><td class='field'>Home ontology</td><td>" +
		inspectLinko(rule.homeOntology, 'ONTOLOGY') +
		"</td></tr>";
		if (rule.direction == 'BACKWARD') {
			html += "<tr><td class='field'>Defines relation</td><td>" +
			rule.definesRelation +
			"</td></tr>";
		}
		
		html += "</table>";
		
		//This line is the behaviour being overriden
		document.getElementById("info"+_activeWindowID).innerHTML += html;

	}
	
	window.renderOntologyInspection = function(ontologyData, windowID){
	
		var ont = new OcmlOntology(ontologyData);
		//var html = "<h3>Ontology: " + ont.name + "</h3>";
		var html = "";
		html += "<table>";
		html += "<tr><td class='field'>Documentation</td><td>" + (ont.documentation || "") + "</td></tr>";
		html += "<tr><td class='field'>Ontology type</td><td>" + ont.type + "</td></tr>";
		html += "<tr><td class='field'>Namespace URI</td><td>" + ont.namespaceUri + "</td></tr>";
		html += "<tr><td class='field'>Includes</td><td>";
		
		for (var i = 0; i < ont.includes.length; i++) {
			html += inspectLink(ont.includes[i], ont.includes[i], 'ONTOLOGY') + " ";
		}
		
		html += "</td></tr>";
		html += "<tr><td class='field'>Included by</td><td>";
		
		for (var i = 0; i < ont.includedBy.length; i++) {
			html += inspectLink(ont.includes[i], ont.includedBy[i], 'ONTOLOGY') + " ";
		}
		
		html += "</td></tr>";
		html += "<tr><td class='field'>Namespace prefixes</td><td><table>";
		
		for (i = 0; i < ont.namespacePrefixes.length; i++) {
			html += "<tr><td>" + ont.namespacePrefixes[i].prefix +
			"</td><td>" +
			ont.namespacePrefixes[i].namespace +
			"</td></tr>";
		}
		
		html += "</table></td></tr>";
		html += "<tr><td class='field'>Author</td><td>" + ont.author + "</td></tr>";
		html += "<tr><td class='field'>Editors</td><td>";
		
		for (var i = 0; i < ont.editors.length; i++) {
			html += ont.editors[i] + " ";
		}
		
		html += "</td></tr>";
		html += "<tr><td class='field'>Files</td><td>";
		
		for (var i = 0; i < ont.files.length; i++) {
			html += ont.files[i] + " ";
		}
		
		html += "</td></tr>";
		
		html += "</table>";
		
		//Changed so that ontology info is displayed above the two main panels in a new container called 'ontology-info-hole'
						
		//This line is the behaviour being overriden
		document.getElementById("ontology-info-hole"+_activeWindowID).innerHTML = '';
		document.getElementById("ontology-info-hole"+_activeWindowID).innerHTML += html;

	
	}
	
	//Overriding this function so that clicking on an ontology link
	//produces the same behaviour as selecting an ontology from menu
	window.inspectLink = function(ontology, thing, klass) {
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
	/*
return  "<span class='" + cssClass + "' onclick=\"inspect('" + thing
	    + "','" + thing + "','" + klass +"')\">" + thing + "</span>";
*/
		return  "<span class='" + cssClass + "' onclick=\"onClickSelectOntology('" + thing
	    + "')\">" + thing + "</span>";
    } else {
	cssClass = klass;
    }
    var str = "<span class='" + cssClass + "' onclick=\"inspect('" + thing
	+ "','" + ontology + "','" + klass +"')\">" + thing + "</span>";
    return str;
}
	//This isn't an override; this is a new function defined
	//to make clicking on an ontology link in the browser
	//launch the ontology in a new window
	window.onClickSelectOntology = function (ontologyName) {
		editor.selectOntology(ontologyName)
	}
	
	window.inspect = function(object, ontology, type) {
  		_activeWindow.inspectorHistory.add({object: object, ontology: ontology, type: type});
  		inspectBypassHistory(ontology,object,type);
		
		//It is useful to know the element currently on display in the 'Inspect' tab
		self.activeWindow.currentInspectionElement = object;
		
		//Now want to jump to the relevant definition in the 'Code' viewing tab
		var myFrame = frames['codeviewer'+_activeWindowID]; 
		//XXX not the best coding approach to hardcode this URL
		//Actually doesn't work, not sure why
		object = object.toLowerCase();
		myFrame.open(irs.irsUri('/irs/ontology/ocml/')+ontology+'?format=raw#'+ object, '_self').scrollTop;
		
		
	}
	
	window.inspectBypassHistory = function(ontology, object, type){
	 	
		function callback(renderer){
			return function callback(data){
				var el = document.getElementById("info"+_activeWindowID);
				el.innerHTML = "";
				addNavbar(el);
				renderer(data);
			};
		}
		if (type == 'CLASS') {
			irs.getClass(callback(renderClassInspection), ontology, object);
		}
		else 
			if (type == 'FUNCTION') {
				irs.getFunction(callback(renderFunctionInspection), ontology, object);
			}
			else 
				if (type == 'INSTANCE') {
					irs.getInstance(callback(renderInstanceInspection), ontology, object);
				}
				else 
					if (type == 'ONTOLOGY') {
						irs.getOntology(callback(renderOntologyInspection), ontology, object);
					}
					else 
						if (type == 'RELATION') {
							irs.getRelation(callback(renderRelationInspection), ontology, object);
						}
						else 
							if (type == 'RULE') {
								irs.getRule(callback(renderRuleInspection), ontology, object);
							}
		//rightTab.set('activeIndex', 0);
	}
	
	window.inspectWsmoClass = function (ontology,symbol) {
    	inspect(symbol,ontology,'CLASS');
		
		//The following line is commented out from the original
    	//updateServerVis(symbol,ontology,'CLASS');
	}

	window.addNavbar = function (el) {
    	var history = _activeWindow.inspectorHistory;
    	var back = history.lookBack();
    	var forward = history.lookForward();

    	this.backButtonId = gensym();
    	this.forwardButtonId = gensym();
		
		
    	el.innerHTML += '<div id="buttonHole'+_activeWindowID+'"></div>';
		
    	var backButton = new YAHOO.widget.Button({
			label: "Back", id: this.backButtonId,
      		container: "buttonHole"+_activeWindowID,
      		onclick: {fn: function (event) {goBack();}}});
    	if (!back) {
            backButton.set("disabled", true);
    	}

    	var forwardButton = new YAHOO.widget.Button({
			label: "Forward", id: this.forwardButtonId,
      		container: "buttonHole"+_activeWindowID,
      		onclick: {fn: function (event) {goForward();}}});

    	if (!forward) {
            forwardButton.set("disabled", true);
    	}
    	el.innerHTML += "<hr/>";
	}

	window.goBack = function() {
    	var history = _activeWindow.inspectorHistory;//Overriden behaviour
    	history.goBack();
    	var place = history.current();
    	inspectBypassHistory(place.ontology,place.object,place.type);
	}

	window.goForward = function () {
    	var history = _activeWindow.inspectorHistory;//Overriden behaviour
    	history.goForward();
    	var place = history.current();
    	inspectBypassHistory(place.ontology,place.object,place.type);
	}
	
	window.drawRelationList = function(data, ontology){
		
		//This line is the behaviour being overriden
		var pane = document.getElementById("relationlist"+_activeWindowID);
		pane.innerHTML = '<div><div id="relationList'+_activeWindowID+'"></div></div>';

		data = data.sort(nameGreaterThan);
		var tree = new YAHOO.widget.TreeView("relationList"+_activeWindowID);
		var root = tree.getRoot();
		for (var i = 0; i < data.length; i++) {
			var obj = {
				label: ocml.internSymbol(data[i].name),
				href: "javascript:inspect('" + data[i].name + "', '" + ontology + "', 'RELATION')"
			};
			var node = new YAHOO.widget.TextNode(obj, root, false);
		}
	    blessThisTree(tree);
	    tree.render();

	}
	
	
	window.drawRuleList = function(data, ontology){
		
		//This line is the behaviour being overriden		
		var pane = document.getElementById("rulelist"+_activeWindowID);
		pane.innerHTML = '<div><div id="ruleList'+_activeWindowID+'"></div></div>';

		data = data.sort(nameGreaterThan);
		var tree = new YAHOO.widget.TreeView("ruleList"+_activeWindowID);
		var root = tree.getRoot();
		for (var i = 0; i < data.length; i++) {
			var obj = {
				label: ocml.internSymbol(data[i].name),
				href: "javascript:inspect('" + data[i].name + "', '" +
				ontology +
				"', 'RULE')"
			};
			var node = new YAHOO.widget.TextNode(obj, root, false);
		}
	    blessThisTree(tree);
	    tree.render();

	}
	
	
	window.drawFunctionList = function(data, ontology){
		
		//This line is the behaviour being overriden		
		var pane = document.getElementById("functionlist"+_activeWindowID);
		pane.innerHTML = '<div><div id="functionList'+_activeWindowID+'"></div></div>';

		data = data.sort(nameGreaterThan);
		var tree = new YAHOO.widget.TreeView("functionList"+_activeWindowID);
		var root = tree.getRoot();
		for (var i = 0; i < data.length; i++) {
			var obj = {
				label: ocml.internSymbol(data[i].name),
				href: "javascript:inspect('" + data[i].name + "', '" + ontology + "', 'FUNCTION')"
			};
			var node = new YAHOO.widget.TextNode(obj, root, false);
		}
	    blessThisTree(tree);
	    tree.render();

	}
	
	// {{{ Class tree
	window.drawClassTree = function(data, ontology){
		
		//This line is the behaviour being overriden
		var container = document.getElementById("classtree"+_activeWindowID);
		container.innerHTML = '<div><div id="treewidget'+_activeWindowID+'"></div></div>';

		
		var tree = new YAHOO.widget.TreeView("treewidget"+_activeWindowID);
		var root = tree.getRoot();
		
		drawTree(root, ontology, data);
	    blessThisTree(tree)
	    tree.render();
	}
	
	//This has been redefined so that appropriate WSMO icon (G, WS, M, or C)
	//is placed in front of classes in the Class tree
	window.drawTree = function(root, ontology, data){
		if (data.length > 0) {
		
			data = data.sort(nameGreaterThan);
			
			for (var i = 0; i < data.length; i++) {
				var obj = {
					label: ocml.internSymbol(data[i].name),
					href: "javascript:inspect('" + data[i].name + "','" +
					ontology +
					"','CLASS')"
				};
				var node = new YAHOO.widget.TextNode(obj, root, false);
				
			/*
	if (memberOf('GOAL', data[i].superClasses)) {
					node.labelStyle = 'ygtvlabel icon-wsmo-g';
				}
*/
				
				drawTree(node, ontology, data[i].children);
				
			}
		}
	}
	
	// }}}
	// {{{ WSMO entities tree
	
	window.drawWsmoEntitiesTree = function(data, ontology){
		
		//This line is the behaviour being overriden
		var container = document.getElementById("classlist"+_activeWindowID);
		container.innerHTML = '<div><div id="wsmotreewidget'+_activeWindowID+ '"></div></div>';
		
		var tree = new YAHOO.widget.TreeView("wsmotreewidget"+_activeWindowID);
		var root = tree.getRoot();
		drawWsmoTree(root, ontology, data);
	    blessThisTree(tree);
	    tree.render();

	}
	
	//This has been redefined to include WSMO icons
	window.drawWsmoTree = function(root, ontology, data){
		function branch(data, label, func){
			var branchTopLabel = {
				label: label + "[" + data.length + "]"
			};
			var branchTop = new YAHOO.widget.TextNode(branchTopLabel, root, false);
			
			//Add the icons to the top level
			if (label == "Goals") {
				branchTop.labelStyle = 'ygtvlabel icon-wsmo-g'
			}
			if (label == "Services") {
				branchTop.labelStyle = 'ygtvlabel icon-wsmo-ws'
			}
			if (label == "Mediators") {
				branchTop.labelStyle = 'ygtvlabel icon-wsmo-m'
			}
			if (label == "Concepts") {
				branchTop.labelStyle = 'ygtvlabel icon-wsmo-c'
			}
			
			for (var i = 0; i < data.length; i++) {
				var obj = {
					label: ocml.internSymbol(data[i].name),
					href: "javascript:" + func + "('" + ontology + "','" +
					ocml.externSymbol(data[i].name) +
					"')"
				};
				var node = new YAHOO.widget.TextNode(obj, branchTop, false);
			}
		}
		
		
		branch(data.goals, "Goals", "inspectWsmoClass");
		branch(data.services, "Services", "inspectWsmoClass");
		branch(data.mediators, "Mediators", "inspectWsmoClass");
		branch(data.concepts, "Concepts", "inspectWsmoClass");
		//branch(data.instances, "Instances", "inspectWsmoInstance");//Do we need to show instances in the WSMO view?
	}
	
	// }}}
	
	//This function has been amended to handle instance data as a JSON list [{name: "Instance1", homeOntology: "home-ont"}, {name: "Instance2", homeOntology: "home-ont"}...]
	//Just needed to change data[i] to data[i].name
	 window.drawInstanceList = function(data, ontology){
	 	
		//This line has been overriden, but it isn't clear what its original purpose was
		//considering that 'container' is never actually used
		var container = document.getElementById("instancelist"+_activeWindowID);
		
		//This is the real behaviour being overriden
		var tree = new YAHOO.widget.TreeView("instancelist"+_activeWindowID);
		var root = tree.getRoot();
		
		data = data.sort(nameGreaterThan);
		for (var i = 0; i < data.length; i++) {
			var obj = {
				label: ocml.internSymbol(data[i].name),
				href: "javascript:inspect('" + ocml.externSymbol(data[i].name) +
				"', '" +
				ontology +
				"', 'INSTANCE')"
			};
			var node = new YAHOO.widget.TextNode(obj, root, false);
		}
	     blessThisTree(tree);
	     tree.render();
	 }
	
	
	/* UPDATES THE SERVER VISUALIZATION */
	window.updateServerVis = function(selection, ontology, type, select){
		irs.updateServer(selection, ontology, type, function(data){
			//This line is the behaviour being overriden
			document.getElementById("servervis"+_activeWindowID).innerHTML = data;
/*
			
			//But for now, keep the original rendering as well - just for testing
			document.getElementById("servervis").innerHTML = data;
*/
		});
		if (select) {
			rightTab.set('activeIndex', 1);
		}
	}
} 