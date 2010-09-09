function OntologyWindow(editor){
	
	var self = this;	//According to (http://www.crockford.com/javascript/private.html), we should follow the convention
					    //to add a 'self' (which is not accessible from outiside the Editor object but is global within the object) in order to make the current instance
 					    //available to the inner methods. Apparently, this is a workaround for an error in the ECMAScript Language
					    //Specification which causes this to be set incorrectly for inner functions.
	
	self.windowID;	//Unique ID number for the ontology Window	
	self.windowTitle;//The ontology name - will be displayed in the 'Windows' submenu of the menubar
	self.panel; //Holds the actual Panel instance which IS the window
	self.inspectorHistory = new History();//Each ontology window must maintain its own history
	self.ontologyData = new OntologyDataset();
	self.filterButtons = new Array; //This is to store a set of filter buttons with each window
	
	this.setOntologyMetadata = function (data) {
		self.ontologyData.metaData = new OcmlOntology(data);
	}
	
	
	this.setClassTreeData = function(data) { 
		self.ontologyData.classTree = data; 
	};
	
	this.setRelationListData = function(data) { 
		self.ontologyData.relationList = data;
	};	
	
	this.setRuleListData = function(data) {
		self.ontologyData.ruleList = data;
	};
	
	this.setFunctionListData = function(data) {
		self.ontologyData.functionList = data;
	};	
	
	this.setInstanceListData = function(data) {
		self.ontologyData.instanceList = data;
	};
	
	this.setWsmoEntitiesTreeData = function(data) {
		self.ontologyData.wsmoEntitiesTree = data;
	};
	
	self.init = function (windowID, windowTitle){
		
		self.windowID = windowID;
		self.windowTitle = windowTitle;
		
				
		self.panel = new YAHOO.widget.Panel (("ontology-window"+windowID), 
												{
													context: ["menubarhole","tl", "bl"],
													underlay:"none", 
													close:true, 
													visible:true, 
													draggable:false,
													appendtodocumentbody:true
												}
											)
		self.panel.setHeader(		
			'<div style="display:table-row; width: 100%">' +
			'<div style="display:table-cell; width: 50%; vertical-align: middle; font-size: 100%">Ontology | '+ windowTitle + '</div>' + 
			'<div id = "filter-container' + windowID + '" style="display:table-cell; vertical-align: middle; font-weight: normal"></div>' +
			'</div>'
		);
		
		self.panel.setBody(_drawBrowserTable(windowID));
		self.panel.hideEvent.subscribe(_onWindowClose, self)
		self.panel.render(document.body);
		
		var thisPanel = document.getElementById("ontology-window"+windowID);
		thisPanel.className = "yui-panel ontology-window";
		
		//XXX This is a terrible hack for getting the ontology window
		// to fill the entire width in Firefox 3.
		//(Adding 'width:100%' to the CSS stylesheet alone doesn't seem to work)
		thisPanel.parentNode.style.width = "100%";
		
		
		//XXX Copied and pasted from Browser.init
		//Would much prefer to reuse via inheritance
		{
			leftTab = new YAHOO.widget.TabView('leftTab'+windowID);
			leftTab.set('activeIndex', 0);
			rightTab = new YAHOO.widget.TabView('rightTab'+windowID);
			rightTab.set('activeIndex', 0);
			
			// Capture the OCML listener form's submit event and subvert it to our cause.
			//var form = document.getElementById("repl-form"+windowID);			
			//form.onsubmit = replSubmitHook;//replSubmitHook will also need to be changed (TODO)
		}
		
/*
		//XXX Copied and pasted from <script> element of EDITOR.HTML
		//Would much prefer to reuse via inheritance (TODO)
		{
			(function() {
    			var Dom = YAHOO.util.Dom,
       			Event = YAHOO.util.Event,
        		col1 = null
       			col2 = null;
//Eventually, to ensure that the browser-table is the same width as the menubar
//we will have to attach an event listener to the menubar resize event
//that would then trigger the resize of the browser-table
    			Event.onAvailable('browser-table'+windowID, function() {
        		var size = parseInt(Dom.getStyle('browser-table'+windowID, 'width'), 10);
        		col1 = Dom.get('leftTab'+windowID);
        		col2 = Dom.get('rightTab'+windowID);
        		var max = (size - 150);
        		var resize = new YAHOO.util.Resize('leftTab'+windowID, {
            		handles: ['r'],
            		minWidth: 150,
            		maxWidth: max
        		});
        		resize.on('resize', function(ev) {
            		var w = ev.width;
            		Dom.setStyle(col1, 'height', '');
            		Dom.setStyle(col2, 'width', (size - w - 6) + 'px');
        		});

        		resize.resize(null, null, 300, 0, 0, true);
    			});

			})();
		}
*/			
	}
	
	function _drawBrowserTable(windowID){
		
		var html =
			'<div id= "ontology-info-hole'+windowID+'" style='+
					'"height: 90px;'+
					'padding: 0px 0px 0px 10px;'+
					'overflow: auto; border-bottom: thin double"></div>' +
			'<div id = "browser-table'+windowID+'">' +
				'<table >' +
				'<tr>' +
					'<td style="width: 35%">' +
						'<div class = "yui-navset" id = "leftTab'+windowID+'">' +
							'<ul class = "yui-nav">' +
								'<li><a href = "#classtree'+windowID+'"><em>Classes</em></a></li>' +
								'<li><a href = "#functionlist'+windowID+'"><em>Functions</em></a></li>' +
								'<li><a href = "#instancelist'+windowID+'"><em>Instances</em></a></li>' +
								'<li><a href = "#relationlist'+windowID+'"><em>Relations</em></a></li>' +
								'<li><a href = "#rulelist'+windowID+'"><em>Rules</em></a></li>' +
								'<li><a href = "#classlist'+windowID+'"><em>WSMO View</em></a></li>' +
							'</ul>' +
							'<div class ="yui-content">' +
						
								'<div id ="classtree'+windowID+'" class ="tabbycat">' +
									'<p></p></div>' +
								'<div id ="functionlist'+windowID+'" class ="tabbycat">' +
									'<p></p></div>' +
								'<div id ="instancelist'+windowID+'" class ="tabbycat">' +
									'<p></p></div>' +
								'<div id = "relationlist'+windowID+'" class = "tabbycat">' +
									'<p></p></div>' +
								'<div id = "rulelist'+windowID+'" class = "tabbycat">' +
									'<p></p></div>' +
								'<div id = "classlist'+windowID+'" class = "tabbycat">' +
									'<p></p></div>' +
							'</div>' +
						'</div>' +				
					'</td>' +
        			'<td>' +
						'<div class ="yui-navset" id ="rightTab'+windowID+'">' +
							'<ul class ="yui-nav">' +
								'<li><a href ="#info'+windowID+'"><em>Inspect</em></a></li>' +
								//'<li><a href ="#servervis'+windowID+'"><em>WSMO View</em></a></li>' +
								//'<li><a href ="#repl'+windowID+'"><em>Listener</em></a></li>' +
								'<li><a href ="#codeviewer'+windowID+'"><em>Code</em></a></li>' +
							'</ul>' +
				
							'<div class = "yui-content">' +
								'<div id = "info'+windowID+'" class ="tabbycat">' +
									'<p>Nothing has been clicked yet</p></div>' +
								//'<div id = "servervis'+windowID+'" class ="tabbycat">' +
								//	'<p>Double click on an entity to activate this view</p></div>' +
								//'<div id ="repl-tab'+windowID+'" class ="tabbycat">' +
								//	'<div id ="repl-input-hole'+windowID+'">' +
								//		'<form id ="repl-form'+windowID+'">' +
								//			'"OCML> " ' +
								//			'<input type="text"id ="repl-input-area'+windowID+'" name ="repl-input'+windowID+'" size ="70" class ="codebox"></input><br/>' +
								//			'<textarea id = "repl-output-area'+windowID+'" class="codebox" rows = "30" cols ="80" readonly ="readonly"></textarea>' +
								//		'</form>' +
								//	'</div>' +
								//'</div>' +
								//'<div id ="file-editor'+windowID+'" class = "tabbycat">' +
									'<iframe name ="codeviewer'+windowID+'" width="100%" height="100%" class = "tabbycat" style="background-color: white">' +
									//	'<textarea id ="editortextarea'+windowID+'" name ="editor-textarea'+windowID+'" rows = "70" cols ="70"></textarea>' +
									'</iframe>' +	
								//'</div>' +
							'</div>' +
						'</div>' +
					'</td>' +
				'</tr>' +
				'</table>' +
			'</div><!--browser-table-->';
	
		return html;
	}
	
	//Function that responds to the 'destroyEvent' fired when an ontology window is closed
	function _onWindowClose(evType, evArgs, ontWindow) {
		
		//Look for the ontology window in the 'ontologyWindows' array
		var index;
		for (var i = 0; i < editor.ontologyWindows.length; i++) {
			if (editor.ontologyWindows[i] == ontWindow){
				index = i;
			}
		}		
		//Remove the ontology window from the 'ontologyWindows' array		
		editor.ontologyWindows.splice(index, 1);
		
		//'Unregister' the ontology window with the ontology windows manager
		editor.ontologyWindowsManager.remove(ontWindow.panel);
		
		if (editor.ontologyWindows.length > 0) {
		
			//Go through the remaining overlays/panels/ontology-windows
			//and find the one with the highest z-index
			var highIndex = editor.ontologyWindowsManager.overlays[0].cfg.getProperty("zindex");
			
			var j = 0;
			for (var i = 0; i < editor.ontologyWindowsManager.overlays.length; i++) {
				if (highIndex < editor.ontologyWindowsManager.overlays[i].cfg.getProperty("zindex")) {
					highIndex = editor.ontologyWindowsManager.overlays[i].cfg.getProperty("zindex");
					j = i;
				}
			}
			//When it is found, give it the focus
			editor.ontologyWindowsManager.focus(editor.ontologyWindowsManager.overlays[j]);
			
			//Make sure the rendering functions know which is the currently active window
			editor.renderer.setActiveWindowID();
		}

		//Update the 'Windows' submenu item
		editor.menuBar.updateWindowSubmenu(editor.ontologyWindows); 
	}
}
