//	EDITOR.JS
//	Summary:	This file contains the main object descriptions for the IRS III editor.
//	Dependencies: 	BROWSER.JS
//					IRS-API.JS
//					OCML-API.JS
//					EDITOR-DIALOGS.JS
//					EDITOR-MENUBAR.JS
//					EDITOR-ONTOLOGYWINDOW.JS
//					EDITOR-RENDERER.JS
//					<<YUI LIBRARY>>
//
//	Author:		Neil Benn (building on code by Dave Lambert and Michele Pasin)
//	Date (creation):	01-Aug-2008
//	Date (last mod.):	25-Sep-2008
//
//	Notes:	(1)	Many 'class' definitions start with the statement "var self = this". According to
//				(http://www.crockford.com/javascript/private.html), we should follow the convention
//				to add a 'self' in order to make the current instance available to the inner methods. 
//				Apparently, this is a workaround for an error in the ECMAScript Language Spec which
//				causes this to be set incorrectly for inner functions.
	


var FULL_EDITOR_MODE = false;

function run (/*String*/ optionalInitialOntology) {
	var editor = new Editor(optionalInitialOntology);
	
	YAHOO.util.Event.onContentReady('menubarhole', editor.init, editor);
}

function yuiAlert (/*String*/ msg) {
	//summary:	Intended to replace use of window.alert
	//description:	This function has been introduced so that alerts, warnings, etc.
	//		can take on the same YUI look and feel as the rest of the interface
	//		rather than have the look and feel of the typical web browser alerts
	
	var alertBox = 	new YAHOO.widget.SimpleDialog("alertbox", 
						{ 
							//width: "300px",
			   				fixedcenter: true,
			   				visible: true,
			   				draggable: false,
			   				close: true,
							modal:true,
			   				text: msg,
			   				icon: YAHOO.widget.SimpleDialog.ICON_WARN,
			   				constraintoviewport: true,
			   				buttons: [ 
								{ 
									text:"OK",
									handler: {
										fn: function() {
											this.hide()
										}
									},
									isDefault:true
								}
							]
			 			}
					);
					
	alertBox.render(document.body);
	
	
}

function deepCloneObject (obj) {
	//From http://bytes.com/forum/thread715567.html
	//Improvement on cloneObject defined below, which is unable to handle JSON arrays
	
	var c = obj instanceof Array ? [] : {};
 
    for (var i in obj) {
        var prop = obj[i];
 
        if (typeof prop == 'object') {
           if (prop instanceof Array) {
               c[i] = [];
 
               for (var j = 0; j < prop.length; j++) {
                   if (typeof prop[j] != 'object') {
                       c[i].push(prop[j]);
                   } else {
                       c[i].push(deepCloneObject(prop[j]));
                   }
               }
           } else {
               c[i] = deepCloneObject(prop);
           }
        } else {
           c[i] = prop;
        }
    }
 
    return c;
}

function OntologyDataset () {
	//summary:	Stores all data for a single ontology
	//description:	The OntologyDataset 'class' has been introduced
	//		so that all the data for any given ontology can be retrieved and stored

	
	this.metaData;	//Ontology name, namespace, ontology-includes, etc.
					//Holds an instance of 'OcmlOntology()' (which is defined in OCML-API.JS)
					
	this.classTree;
	this.relationList;
	this.ruleList;
	this.functionList;
	this.instanceList;
	this.wsmoEntitiesList;
}


function EditorOntologyControl (/*Object*/ editor) {
	//summary:	
	//description:	This prototypes (i.e. inherits from) 'OntologyControl()' in BROWSER.JS

	OntologyControl.call(this, editor);	//This allows the 'OntologyControl()' object to correctly handle requests not handled by EditorOntologyControl
										//Doing this means the parent (OntologyControl) is operating on the same 'editor' reference that has been passed to the child
										//(EditorOntologyControl).
										//See http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Guide:The_Employee_Example:More_Flexible_Constructors

	var self = this;	


	this.selectorPanel;
	this.ontologyTable;					
						
	//Redefining the way that filters are drawn
	this.drawFilterButtons = function () { 	

		var _activeWindowID = editor.renderer.activeWindowID;
		var _activeWindow = editor.renderer.activeWindow;
	
		var panelbodyhtml = 
		'<div style ="border: medium solid lightGrey; padding: 5px"' +
			'<div style ="display: table-row">' + 
				'<div style="padding: 0px 15px 0px 0px; display: table-cell;">Filters: </div>' +
				'<div id = "i-filter'+_activeWindowID+'" style="display: table-cell"></div>' + 
				'<div style="padding: 0px 15px 0px 0px; display: table-cell;">Imported</div>' + 
				'<div id = "b-filter'+_activeWindowID+'" style="display: table-cell"></div>' + 
				'<div style="padding: 0px 15px 0px 0px; display: table-cell;">Base</div>' + 
				'<div id = "w-filter'+_activeWindowID+'" style="display: table-cell"></div>' + 
				'<div style="display: table-cell;">Wsmo</div>' + 	
			'</div>'+
		'</div>'	;
			

		document.getElementById("filter-container"+_activeWindowID).innerHTML = panelbodyhtml;

		_activeWindow.filterButtons[0] = 
			new YAHOO.widget.Button({type: "checkbox", checked: true, container: "i-filter"+_activeWindowID, onclick: {fn: self.getcheckboxes, obj: 0}});
		_activeWindow.filterButtons[1] = 
			new YAHOO.widget.Button({type: "checkbox", checked: true, container: "b-filter"+_activeWindowID, onclick: {fn: self.getcheckboxes, obj: 1}});
		_activeWindow.filterButtons[2] = 
			new YAHOO.widget.Button({type: "checkbox", checked: true, container: "w-filter"+_activeWindowID, onclick: {fn: self.getcheckboxes, obj: 2}});
			
		document.getElementById("i-filter"+_activeWindowID).className = "filter-button";
		document.getElementById("b-filter"+_activeWindowID).className = "filter-button";
		document.getElementById("w-filter"+_activeWindowID).className = "filter-button";	
			
	}//END FUNCTION this.drawFilterButtons()

	//Redefining what happens when user clicks a filter button
	this.getcheckboxes = function (/*String*/ p_sType, /*int*/ buttonNum) {

		var button = editor.renderer.activeWindow.filterButtons[buttonNum];

/*
		if (button.get('checked') == false)
		{
			button.set('label','Off');
		}
		else
		{
			button.set('label','On');
		}
*/
		editor.drawPanels (editor.renderer.activeWindow.windowTitle)
	}
	
	//Redefining the way the status of each filter button is retrieved
	this.filters = function () {
		var result = new Array(self.numFilters);
		for (var i=0;i<self.numFilters;i++) {
	    	result[i] = editor.renderer.activeWindow.filterButtons[i].get("checked");
		}
		return result;
    };
	
	//Redefining how the ontology selector is drawn
    this.drawOntologySelector = function() { 
	
		self.selectorPanel = new YAHOO.widget.Panel("selectorpanel", { 
																appendtodocumentbody: true,		
																visible:false, 
																close:true,													
																underlay: "none",
																constraintoviewport: true,
																draggable: true,
																modal:true,
																fixedcenter:true,
																appendtodocumentbody: true,
															} );
		self.selectorPanel.setHeader('Select an ontology...');
		
		//This div will hold the DataTable with the ontology list
		self.selectorPanel.setBody('<div id="ontologytable"></div>');
		
		self.selectorPanel.setFooter('<div style="display: table-cell; width:0%"></div><div id="selectorpanelfooter" style="display: table-cell"></div>');
		
		self.selectorPanel.render(document.body);
		
		//Now add buttons to the panel.  YUI provides a Dialog subclass of Panel which has a 'buttons' component.
		//However, the buttons in the Dialog class aren't very flexible (e.g. a button can't be disabled)
		//So the best option is to add our own button group to the Panel object.
		self.selectorPanel.buttons = new YAHOO.widget.ButtonGroup ({id: "selectorpanelbuttons", container: "selectorpanelfooter"});
		 self.selectorPanel.buttons.addButtons ([
			{label: "Load", disabled: true, onclick: {fn: handleLoad}, type: "push"},
			{label: "Cancel", onclick: {fn: handleCancel}, type: "push"}
		]); 
		
	 	//Put the focus on the 'Cancel' button
		self.selectorPanel.buttons.getButton(1).focus();  
		
		function handleLoad() {
			
			//Get index of the selected row (assuming that it is the only row selected - indeed, we have constrained selection to single row)
			var row = self.ontologyTable.getSelectedRows()[0];
			
			//Get the name of the ontology from the currently selected row in the datatable (ontologyTable)
			var ontologyName = self.ontologyTable.getRecord(row).getData("name");
			
			self.selectorPanel.hide();
						
			//Go ahead and retrieve the data for that ontology
			editor.selectOntology(ontologyName);		
			
			//self.drawFilterButtons(); //Now moved inside the 'selectOntology()' function
		}
		
		function handleCancel() {
			self.selectorPanel.hide();
		}	
		
		function handleSelect(eventObject) {
			var clickedRow = YAHOO.util.Event.getTarget(eventObject); //gets the particular row that was the target of the click
			
			self.ontologyTable.unselectAllRows(); //So that only one row can be selected at a time
			self.ontologyTable.selectRow(clickedRow);
			self.selectorPanel.buttons.getButton(0).set("disabled", false);
			self.selectorPanel.buttons.getButton(0).focus();
		}
		
		var myColumnDefs = [
			{key:"name", label:""}
		];
		
		var myDataSource = new YAHOO.util.DataSource([]);//The data will be obtained when'updateOntologySelectorMenu' is called elsewhere
		
		myDataSource.responseType = YAHOO.util.DataSource.TYPE_JSARRAY;
        myDataSource.responseSchema = {
            fields: ["name"]
        };
		YAHOO.widget.DataTable.MSG_EMPTY = "No ontologies found"
		self.ontologyTable = new YAHOO.widget.DataTable("ontologytable", myColumnDefs, myDataSource, {scrollable:true, height:"20em", selectionMode: "single"})
		
		// Subscribe to events for row selection 
		self.ontologyTable.subscribe("rowMouseoverEvent", self.ontologyTable.onEventHighlightRow);
		self.ontologyTable.subscribe("rowMouseoutEvent", self.ontologyTable.onEventUnhighlightRow);
		self.ontologyTable.subscribe("rowClickEvent", handleSelect);
		self.ontologyTable.subscribe("rowDblclickEvent", handleLoad);
				
    };//this.drawOntologySelector

	
	
	//Redefining 'updateOntologySelectorMenu'
	    this.updateOntologySelectorMenu = function() {
	
			irs.getOntologyList(function(list) {
				
								
				//Clone the list for save measure			
				var data = YAHOO.widget.DataTable._cloneObject(list);
				
				//Now add the new data (replacing anything that was there before)
				//Note that API documentation at http://developer.yahoo.com/yui/docs/RecordSet.html
				//actually says the method is called 'replace' rather than 'replaceRecords'
				//But 'replace' doesn't work.  Was given 'replaceRecords' as the correct option by
				//http://tech.groups.yahoo.com/group/ydn-javascript/message/28401	
				self.ontologyTable.getRecordSet().replaceRecords(data);	
				
				self.ontologyTable.render();//For some reason we need to render the DataTable again		
				
		});
    };
	
	
	//Redefining the init function
	this.init = function() {
		this.drawOntologySelector(); 
    }; 
}

 
function Editor(/*String*/ optionalInitialOntology) {
	//summary:	
	//description:	When a new Editor() instance is created it will update the behaviour
	//		of some of the existing 'classes'.
	
	var self = this;	
	
	this.menuBar;
	this.ontologyWindows = new Array();	
	
	//Need an OverlayManager to take care of which ontology window is in focus, etc.
	this.ontologyWindowsManager = new YAHOO.widget.OverlayManager();
	
	var _GENID = 1; //Used for generating unique IDs for each ontology window
		
	
	this.init = function (/*Object*/ currentEditorObj) { 
		//summary:	This overrides the 'init()' function inherited from 'Browser()'
							
		
		// XXX Hack. Some functions in BROWSER.JS reference the variable 'browser'
		// so it needs to hold the current editor object for them to work
		browser = currentEditorObj; 
		
		currentEditorObj.menuBar = new EditorMenuBar(currentEditorObj);
		currentEditorObj.renderer = new Renderer(currentEditorObj);
		
		//Now officially make EditorOntologyControl inherit from OntologyControl
		EditorOntologyControl.prototype = new OntologyControl; 
	
		currentEditorObj.ontologySelector = new EditorOntologyControl(currentEditorObj); 
		currentEditorObj.ontologySelector.init(); 
		currentEditorObj.ontologySelector.updateOntologySelectorMenu();
				
		currentEditorObj.dialogs = new EditorDialogs(currentEditorObj);
		currentEditorObj.dialogs.init();
		
				
		//Handle case when the browser is launched with the option to open a particular ontology
		if ((optionalInitialOntology != undefined) && optionalInitialOntology != "") {
			currentEditorObj.selectOntology(optionalInitialOntology);
		}
		

	}
	
	
	this.noOntologyData = true;

	
	//A new property has been added which stores all of the current unfiltered ontology data.  
	this.currentOntologyData = new OntologyDataset();	
	
	
	this.setOntologyMetadata = function (/*JSON Object*/ data) {
		self.currentOntologyData.metaData = new OcmlOntology(data);
	}
	
	this.setClassTreeData = function(/*JSON Array*/ data) { 
		self.currentOntologyData.classTree = data; 
	};
	
	this.setRelationListData = function(/*JSON Array*/ data) { 
		self.currentOntologyData.relationList = data;
	};	
	
	this.setRuleListData = function(/*JSON Array*/ data) {
		self.currentOntologyData.ruleList = data;
	};
	
	this.setFunctionListData = function(/*JSON Array*/ data) {
		self.currentOntologyData.functionList = data;
	};	
	
	this.setInstanceListData = function(/*JSON Array*/ data) {
		self.currentOntologyData.instanceList = data;
	};
	
	this.setWsmoEntitiesTreeData = function(/*JSON Array*/ data) {
		self.currentOntologyData.wsmoEntitiesTree = data;
	};
		
		
	function applyFilters(/*JSON Array*/ data, /*JSON Object*/ ontologyMetadata){
		//summary:	Filter ontology data depending on the configuration of the filter buttons
		//description:	This function has been introduced so that the filtering can now be done
		//		at client-side rather than server-side. The 'ontologyMetadata' parameter contains 
		//		the information about what other ontologies are included by the current ontology.
		//		This 'includes' information is a major part of the filtering.
		
		var filters = self.ontologySelector.filters();
		var filtereddata = new Array();
		
		//XXX Terrible hack: This is so we don't display a class at the top-level
		//when it is also a subclass of another class
		var NON_TOP_LEVEL_ELEMENTS = new Array(); 
		
		
		var imported = filters[0];
		var base = filters[1];
		var wsmo = filters[2];
		
		//Get the array of names of included ontologies
		var importedOntologies = ontologyMetadata.includes; 
		
		//If selected ontology is BASE, then we have to ignore the BASE filter button
		if (ontologyMetadata.name == "BASE-ONTOLOGY") {
			base = false;
			
			//Now uncheck and disable the BASE filter button
			self.renderer.activeWindow.filterButtons[1].set("checked", false);
			self.renderer.activeWindow.filterButtons[1].set("disabled", true);
		}
		
		//Similarly, if selected ontology is WSMO, then we have to ignore the WSMO filter button
		if (ontologyMetadata.name == "WSMO") {
			wsmo = false;
			
			//Now uncheck and disable the WSMO filter button
			self.renderer.activeWindow.filterButtons[2].set("checked", false);
			self.renderer.activeWindow.filterButtons[2].set("disabled", true);
		}
		
		var ontologiesToFilter = new Array();
		
		if (imported) {
			if (base) {
				if (wsmo) {//T T T - Filter out all three
					
					ontologiesToFilter = ontologiesToFilter.concat(importedOntologies, "BASE-ONTOLOGY", "WSMO"); 
					filtereddata = filterData (data[0].name, data, filtereddata, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS);						  
				}
				else {//T T F - Filter out 'imported' and 'base' but keep 'wsmo'
					
					removeFromArray("WSMO", importedOntologies);
					ontologiesToFilter = ontologiesToFilter.concat(importedOntologies, "BASE-ONTOLOGY");
					filtereddata = filterData (data[0].name, data, filtereddata, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS);
					
				};
			}
			else {
				if (wsmo) {//T F T - Filter out 'imported' and 'wsmo' but keep 'base' 
					removeFromArray("BASE-ONTOLOGY", importedOntologies);
					ontologiesToFilter = ontologiesToFilter.concat(importedOntologies, "WSMO");
					filtereddata = filterData (data[0].name, data, filtereddata, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS);
				}
				else {//T F F - Filter out 'imported' but keep 'base' and 'wsmo'
					removeFromArray("BASE-ONTOLOGY", importedOntologies);
					removeFromArray("WSMO", importedOntologies);
					ontologiesToFilter = ontologiesToFilter.concat(importedOntologies);
					filtereddata = filterData (data[0].name, data, filtereddata, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS);
				}
			};
		}
		else {
			if (base) {
				if (wsmo) {//F T T - Keep 'imported' but filter out 'base' and 'wsmo'
					ontologiesToFilter.push("BASE-ONTOLOGY", "WSMO");
					filtereddata = filterData (data[0].name, data, filtereddata, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS);
				}
				else {//F T F - Keep 'imported' and 'wsmo'but filter out 'base' 
					ontologiesToFilter.push("BASE-ONTOLOGY");
					filtereddata = filterData (data[0].name, data, filtereddata, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS);
				};				
			}
			else {
				if (wsmo) {//F F T - Keep 'imported' and 'base' but filter out 'wsmo' 
					ontologiesToFilter.push("WSMO");
					filtereddata = filterData (data[0].name, data, filtereddata, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS);
				}
				else {//F F F - Don't filter out anything 					
					filtereddata = filterData (data[0].name, data, filtereddata, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS);
				};
			};
		};
		
		
		//Final clean-up - removes any top-level elements that shouldn't be there.  This is really an inelegant hack.
		var i = 0;
		while (filtereddata.length > 0 && i < filtereddata.length) {
		
			if (isMemberOf(filtereddata[i], NON_TOP_LEVEL_ELEMENTS)) { 
				filtereddata.splice(i, 1);
			}
			else
				i++;	
		}
		
		
		return filtereddata;
	};
	
	function filterData (cnode, data, filtereddata, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS) {
						
		if (data.length > 0) {
			
			for (var i = 0; i < data.length; i++) {
			
				if (!isMemberOf(data[i].homeOntology, ontologiesToFilter)) { 
				
					if (!isMemberOf(data[i], filtereddata)) {//Check if we haven't already added the element
					
						var dataCopy = new cloneObject(data[i]); /*Best to create a clone rather than use the original data
															       if not weird things start happening with the object referencing
															       (especially when it comes to filtering the children)*/
																   
						var j = filtereddata.push(dataCopy) - 1; //push() returns the new array length so 'j' is the index of current element in 'filtereddata'
					
						//Check if we are dealing with tree data (as is the case when we will be filtering classes
						if (data[i].hasOwnProperty('children')) {
							filtereddata[j].children = new Array(); //This gets rid of the 'children' data that would have been there before
							filtereddata[j].children = filterData(data[i].name, data[i].children, filtereddata[j].children, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS); 
							
							 for (var k = 0; k < filtereddata[j].children.length; k++)		
								NON_TOP_LEVEL_ELEMENTS.push(filtereddata[j].children[k]); 
							
						};
					}	
				}
				else {
					//Even if a node does not belong to one of the ontologies we want, we still have to check its children
					//If it turns out we want the children, then we have to bump the children up to the top-level (because the parent would be discarded)
					//But will have to deal with the problem that children can have multiple parents.
					if (data[i].hasOwnProperty('children')) {
						filtereddata = filterData(data[i].name, data[i].children, filtereddata, ontologiesToFilter, NON_TOP_LEVEL_ELEMENTS);
						
					}					
				}						
							
			}			
			
		}
		
		return filtereddata;
	
	}
	
	
	function isMemberOf (value, someArray) {
		//summary:	Function to see if a given 'value' can be found within 'someArray'.
		//description:	Works for cases where 'value' is a literal and 'someArray' is an array
		//		of literals, as well as cases where 'value' is an ontology element (i.e. an object
		//		with .name and .homeOntology properties) and 'someArray' is an array of ontology
		//		elements.
		var found = false;
		
		for (var i = 0; i < someArray.length; i++) {
		
			if (typeof (value) == 'object' && typeof (someArray[i]) == 'object') {
				if (sameOntologyElement (value, someArray[i])) {
					found = true;
				}
			}
			else //We are dealing with literals
				if (value == someArray[i]) {
					found = true; 
				}	
		}
		
		return found;
	}
	
	function removeFromArray (value, someArray) {
		var i = 0;
		while (someArray.length > 0 && i < someArray.length) {
			if (value == someArray[i]) {
				someArray.splice(i, 1); //Remove that item from the array wherever it is found.
			}
			else
				i++;
		}
	
	}
	
	
	function sameOntologyElement (a, b) {
		//summary:	Check whether two ontology elements are the same thing.
		//description:	This function has been introduced to allow checking whether two ontology
		//		elements (i.e. two classes, two relations, or two instances) are the same thing.
		//		This relies on the fact that two elements in the same ontology can't have the same
		//		identifier
		return (a.name == b.name && a.homeOntology == b.homeOntology)
	}
	
	function cloneObject(what) { 
	//From http://www.irt.org/script/879.htm
	
    	for (i in what) {
        	if (typeof what[i] == 'object') {
            	this[i] = new cloneObject(what[i]);
        	}
        	else
            	this[i] = what[i];
    	}
	}
	
		
	this.selectOntology = function(ontology) {
		//summary:	This overrides the 'selectOntology' method inherited from Browser().
		
		self.selectedOntology = ontology;	
		
		
		//Check whether ontology is already open, in which case don't
		//launch a new ontology window; instead bring the already-opened ontology
		//window into focus
		var ontologyOpen = false;
		for (var i = 0; i < self.ontologyWindows.length; i++) {
			if (self.ontologyWindows[i].windowTitle == ontology){
				ontologyOpen = true;
				self.ontologyWindowsManager.focus(self.ontologyWindows[i].panel);
				
				//Make sure the rendering functions know which is the currently active window
				self.renderer.setActiveWindowID();
			}
		}
		
		if (ontologyOpen == false) {
			//Launch a new ontology window
			self.ontologyWindows.push(new OntologyWindow(self));
			
			var i = self.ontologyWindows.length - 1;
			_GENID++; //Generate a new unique ontology window ID
			//Initialise the ontology window with unique ID and ontology name
			self.ontologyWindows[i].init(_GENID, ontology);
			
			//Register the new ontology window panel with the overlay manager
			self.ontologyWindowsManager.register(self.ontologyWindows[i].panel);
			
			//And give the new ontology window panel the focus
			self.ontologyWindowsManager.focus(self.ontologyWindows[i].panel);
			
			//Make sure the rendering functions know which is the currently active window
			self.renderer.setActiveWindowID();	
			
			self.ontologySelector.drawFilterButtons();		
			
			inspectBypassHistory(ontology, ontology, 'ONTOLOGY');
			
			self.noOntologyData = true;
			
			//There is also a call to 'irs.getOntology()' that takes place within 
			//the 'inspectBypassHistory()' function located in the 'browser.js' file
			//(and overriden in 'editor.renderer.js').
			irs.getOntology(function(data){
				self.setOntologyMetadata(data);
				self.drawPanels(data.name);
				
				//Trialling the full ontology windowing capabilities
				var i = self.ontologyWindows.length - 1;
				self.ontologyWindows[i].setOntologyMetadata(data);
			}, ontology);
						
			
		}
		
			
		//And update the File menu to put a tick next to the currently active window		
		self.menuBar.updateWindowSubmenu(self.ontologyWindows);
		
//[@TODO007] Need to enable some of the menu bar options (e.g. New Goal Description) once an ontology is selected	
    }//END 'this.selectOntology'
	
	/*
		Override some of the current behaviour in the irs-api.  Eventually this will replace the current irs-api.
		For each of the following modified functions, the original arguments were (callback, ontology, filters).
		The 'filters' argument has now been removed because the filtering will be done at client-side.
		
		Note also, I added an 'asynch' parameter to each of the following because there are times when we want
		to make the request to the server in a synchronous rather than asynchronous manner
		(e.g. when receiving the data in bulk for an ontology using the 'getOntologyData' function)
	*/
	//{{{{
	
	irs.getClassTree = function (callback, ontology, asynch) {
	httpget(irs.irsUri("/api-javascript/class-tree?") + "ontology=" + ontology + "&filters=false%2Cfalse%2Cfalse",
		function (response) { callback(eval(response));}, asynch);
    };
	
    irs.getFunctionList = function (callback, ontology, asynch) {
	httpget(irs.irsUri("/api-javascript/function-list?")
		+ "ontology=" + ontology + "&filters=false%2Cfalse%2Cfalse",
	        function (response) {callback(eval(response));}, asynch);
    };
	
	irs.getInstanceList = function (callback,ontology, asynch) {
	httpget(irs.irsUri("/api-javascript/instance-list?")
		+ "ontology=" + ontology + "&filters=false%2Cfalse%2Cfalse",
	        function (response) {callback(eval(response));}, asynch);
    };
	
	irs.getRelationList = function (callback, ontology, asynch) {
	httpget(irs.irsUri("/api-javascript/relation-list?")
		+ "ontology=" + ontology + "&filters=false%2Cfalse%2Cfalse",
		function (response) {callback(eval(response));}, asynch);
    };

    irs.getRuleList = function (callback, ontology, asynch) {
	httpget(irs.irsUri("/api-javascript/rule-list?")
		+ "ontology=" + ontology + "&filters=false%2Cfalse%2Cfalse",
		function (response) {callback(eval(response));}, asynch);
    };
	
	irs.getWsmoEntitiesTree = function (callback, ontology, asynch) {
	httpget(irs.irsUri("/api-javascript/wsmo-entities-tree?")
		+ "ontology=" + ontology + "&filters=false%2Cfalse%2Cfalse",
		function (response) {callback(eval(response)[0]);}, asynch);
    };
	
	

	//}}}
	
	//Now introducing this new irs-api function for retrieving the text files for an ontology
	irs.getOntologyTextFiles = function (callback, ontology) {
		httpget(irs.irsUri("/irs/ontology/ocml/") + ontology + "?format=raw",
			function (response) {callback(response)});
	}
	
	
	//Now the filtering is done just before each element of the panel is drawn
	this.drawPanels = function(ontologyName) {
		
		if (self.noOntologyData == true) {
			
		
			irs.getClassTree(
				function(data) {

					//Trialling the full ontology windowing capabilities
					var i = self.ontologyWindows.length - 1;
					self.ontologyWindows[i].setClassTreeData(data);
					
					drawClassTree(
						applyFilters(
							self.ontologyWindows[i].ontologyData.classTree, 
							self.ontologyWindows[i].ontologyData.metaData
						),
						ontologyName
					);
				
				},
				ontologyName
			);
			
			irs.getRelationList(
				function(data) {

					//Trialling the full ontology windowing capabilities
					var i = self.ontologyWindows.length - 1;
					self.ontologyWindows[i].setRelationListData(data);
					
					drawRelationList(
						applyFilters(
							self.ontologyWindows[i].ontologyData.relationList,
							self.ontologyWindows[i].ontologyData.metaData
						),
						ontologyName
					);
				
				}, 
				ontologyName
			); 
			
			irs.getRuleList(
				function(data) {

					//Trialling the full ontology windowing capabilities
					var i = self.ontologyWindows.length - 1;
					self.ontologyWindows[i].setRuleListData(data);
					
					drawRuleList(
						applyFilters(
							self.ontologyWindows[i].ontologyData.ruleList,
							self.ontologyWindows[i].ontologyData.metaData
						),
						ontologyName
					);
				
				}, 
				ontologyName
			);
			
			irs.getFunctionList(
				function(data) {

					//Trialling the full ontology windowing capabilities
					var i = self.ontologyWindows.length - 1;
					self.ontologyWindows[i].setFunctionListData(data);
					
					drawFunctionList(
						applyFilters(
							self.ontologyWindows[i].ontologyData.functionList,
							self.ontologyWindows[i].ontologyData.metaData
						),
						ontologyName
					);
				
				},
				ontologyName
			);
			
			irs.getInstanceList(
				function(data) {

					//Trialling the full ontology windowing capabilities
					var i = self.ontologyWindows.length - 1;
					self.ontologyWindows[i].setInstanceListData(data);
					
					drawInstanceList(
						applyFilters(
							self.ontologyWindows[i].ontologyData.instanceList,
							self.ontologyWindows[i].ontologyData.metaData
						),
						ontologyName
					);
				
				},
				ontologyName
			);
			
			irs.getWsmoEntitiesTree(
				function(data) {
										
					//Trialling the full ontology windowing capabilities
					var i = self.ontologyWindows.length - 1;
					self.ontologyWindows[i].setWsmoEntitiesTreeData(data);
					
					var wsmoData = deepCloneObject(self.ontologyWindows[i].ontologyData.wsmoEntitiesTree);
										
					wsmoData.goals = applyFilters(wsmoData.goals, self.ontologyWindows[i].ontologyData.metaData); 
					wsmoData.services = applyFilters(wsmoData.services, self.ontologyWindows[i].ontologyData.metaData);
					wsmoData.mediators = applyFilters(wsmoData.mediators, self.ontologyWindows[i].ontologyData.metaData);
					wsmoData.concepts = applyFilters(wsmoData.concepts, self.ontologyWindows[i].ontologyData.metaData); 
					wsmoData.instances = applyFilters(wsmoData.instances, self.ontologyWindows[i].ontologyData.metaData);	
					drawWsmoEntitiesTree(wsmoData, ontologyName);
				
				}, 
				ontologyName
			);
			

			//Load the text files for the ontology into the 'Code Viewer' tab
			irs.getOntologyTextFiles(function(html){
				

				var activeWindowID = self.renderer.activeWindowID;
				
				var thisFrame = frames['codeviewer'+activeWindowID];
				thisFrame.document.write(html);
				thisFrame.document.close();
				
				
			}, ontologyName);
		
			self.noOntologyData = false;	

		}
		else {
			
			//Trialling the full ontology windowing capabilities
			drawClassTree(
				applyFilters(
					self.renderer.activeWindow.ontologyData.classTree,
					self.renderer.activeWindow.ontologyData.metaData
				),
				ontologyName
			);
			
			drawRelationList(
				applyFilters(
					self.renderer.activeWindow.ontologyData.relationList,
					self.renderer.activeWindow.ontologyData.metaData
				),
				ontologyName
			);
			
			drawRuleList(
				applyFilters(
					self.renderer.activeWindow.ontologyData.ruleList,
					self.renderer.activeWindow.ontologyData.metaData
				),
				ontologyName
			);
			
			drawFunctionList(
				applyFilters(
					self.renderer.activeWindow.ontologyData.functionList,
					self.renderer.activeWindow.ontologyData.metaData
				),
				ontologyName
			);
			
			drawInstanceList(
				applyFilters(
					self.renderer.activeWindow.ontologyData.instanceList,
					self.renderer.activeWindow.ontologyData.metaData
				),
				ontologyName
			);
			
			var wsmoData = deepCloneObject(self.renderer.activeWindow.ontologyData.wsmoEntitiesTree);
			wsmoData.goals = applyFilters(wsmoData.goals, self.renderer.activeWindow.ontologyData.metaData); 
			wsmoData.services = applyFilters(wsmoData.services, self.renderer.activeWindow.ontologyData.metaData); 
			wsmoData.mediators = applyFilters(wsmoData.mediators, self.renderer.activeWindow.ontologyData.metaData);
			wsmoData.concepts = applyFilters(wsmoData.concepts, self.renderer.activeWindow.ontologyData.metaData); 
			wsmoData.instances = applyFilters(wsmoData.instances, self.renderer.activeWindow.ontologyData.metaData);	
			drawWsmoEntitiesTree(wsmoData, ontologyName);
		}
	//The way that the WSMO Tree data is structured means the filtering has to be applied here to individual components (Goals, Services, etc.) since that is where the home-ontology information is stored.
/*
		var wsmoData = self.currentOntologyData.wsmoEntitiesTree; 
	
		wsmoData.goals = applyFilters(wsmoData.goals, self.currentOntologyData.metaData); 
		wsmoData.services = applyFilters(wsmoData.services, self.currentOntologyData.metaData);
		wsmoData.mediators = applyFilters(wsmoData.mediators, self.currentOntologyData.metaData);
		wsmoData.concepts = applyFilters(wsmoData.concepts, self.currentOntologyData.metaData); 
		wsmoData.instances = applyFilters(wsmoData.instances, self.currentOntologyData.metaData);	
		drawWsmoEntitiesTree(wsmoData, ontologyName);
*/
	
	}	
	
	
}//Editor
Editor.prototype = new Browser();