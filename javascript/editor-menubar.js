function EditorMenuBar (editor) {
/*
	New Menu bar for the editor.  This takes the place of 'drawMenubar()' function in old browser.js code. 
	The code is largely taken from Application Menubar example from YUI website: http://developer.yahoo.com/yui/examples/menu/applicationmenubar.html
*/

	var self = this;	//According to (http://www.crockford.com/javascript/private.html), we should follow the convention
					    //to add a 'self' (which is not accessible from outiside the Editor object but is global within the object) in order to make the current instance
 					    //available to the inner methods. Apparently, this is a workaround for an error in the ECMAScript Language
					    //Specification which causes this to be set incorrectly for inner functions.
						
	//Define an array of object literals, each containing the data
	//necessary to create the items for a MenuBar.
	var _menuData;
	var WINDOW_MENUITEM_INDEX;

	if (FULL_EDITOR_MODE == true){
		_menuData = 
			[   

    			{ 
        			text: "File", 
        			submenu: {  
            			id: "filemenu", 
            			itemdata: [                
                			{ 								
								text: "Open...", 
								onclick: 
									{
										fn: 	function() {
													editor.ontologySelector.selectorPanel.buttons.getButton(0).set('disabled', true);
													editor.ontologySelector.selectorPanel.buttons.getButton(0).set('checked', false);
													editor.ontologySelector.selectorPanel.buttons.getButton(1).focus();	
													editor.ontologySelector.selectorPanel.buttons.getButton(1).set('checked', false);
													editor.ontologySelector.ontologyTable.unselectAllRows();							
													editor.ontologySelector.selectorPanel.show();								
												}
									}								
							},
                			{ 
								text: "New", 
								submenu: {
									id: "newsubmenu", 
									itemdata: [
										{text: "New Ontology...", onclick: {fn: function() {editor.dialogs.showNewOntologyDialog();}}, disabled: false}, 
										{text: "New Goal Description...", onclick: {fn: function() {editor.dialogs.showNewGoalDialog();}}, disabled: false}, 
										{text: "New Web Service Description...", onclick: {fn: function() {editor.dialogs.showNewWebServiceDescriptionDialog();}}, disabled: false}, 
										{text: "New Mediator Description...", onclick: {fn: function() {editor.dialogs.showNewMediatorDescriptionDialog();}}, disabled: false}
									]
								},
								disabled:false 
							},									
							
                			{ 
								text: "Edit", 
								submenu: {
									id: "editsubmenu",
									itemdata: [
                    					{ text: "Edit Ontology Properties...", disabled: false, onclick: {fn: function() {editor.dialogs.showEditOntologyPropertiesDialog();}}},
                    					{ text: "Edit Goal Description...", onclick: {fn: function() {editor.dialogs.showEditGoalDescriptionDialog();}}, disabled: false },
                    					{ text: "Edit Web Service Description...", onclick: {fn: function() {editor.dialogs.showEditWebServiceDescriptionDialog();}}, disabled: false },
                    					{ text: "Edit Mediator Description...", onclick: {fn: function() {editor.dialogs.showEditMediatorDescriptionDialog();}}, disabled: false }
                					]
								}	
							},
/*
							{ 

                    			text: "Delete", 
                    			submenu: { 
                        			id: "deletesubmenu", 
                        			itemdata: [
                            			{text: "Delete Ontology", disabled: false, onclick: {fn: function() {editor.dialogs.showDeleteOntologyDialog();}}}, 
                            			{text: "Delete Goal Description", disabled: true}, 
                            			{text: "Delete Web Service Description", disabled: true}, 
                            			{text: "Delete Mediator Description", disabled: true}
                        			] 
                    			},
								disabled:false 
                			},
*/

/*
                			{ 
                    			text: "View As", 
                    			submenu: { 
                        			id: "viewsubmenu", 
                        			itemdata: [
                            			{text: "View Ontology as HyperOCML", disabled: true}, 
                            			{text: "View Ontology as OWL/RDF", disabled: true}
                        			] 
                    			} 
                			}
*/
            			],
        			}
    
    			},    
/*
    			{
        			text: "Edit", 
        			submenu: { 
            			id: "editmenu", 
            			itemdata: [
                               
                			[
                    			{ text: "Edit Ontology Properties...", disabled: false, onclick: {fn: function() {editor.dialogs.showEditOntologyPropertiesDialog();}}},
                    			{ text: "Edit Goal Description...", onclick: {fn: function() {editor.dialogs.showEditGoalDescriptionDialog();}}, disabled: false },
                    			{ text: "Edit Web Service Description...", disabled: true },
                    			{ text: "Edit Mediator Description...", disabled: true }
                			],               

                			[
                    			{ text: "Search", disabled: true }
                			],

							[
								{text: "Show Filters", checked: true}
							]

    
						],
					},
					disabled:false
    			},

*/
/*
    			{
					text: "Publish",
					submenu: {
						id: "publishmenu",
						itemdata: [
							{text: "HTTP GET Request URL...", disabled: true},
							{text: "Lisp Function...", disabled: true},
							{text: "Java Class and Method...", disabled: true},
							{text: "Web Service WSDL...", disabled: true}
						],
					},
					disabled:true
				},


    			{
					text: "Run",
					submenu: {
						id: "runmenu",
						itemdata: [
							{text: "IRS Goal...", disabled: true},
							{text: "Knowledge-Base Query...", disabled: true}
						],
					},
					disabled:true
				},

*/
    			{
					text: "Window",
					submenu: {
						id: "windowsmenu",
						itemdata: [
						],
					}
				},
    			{
					text: "Help",
					submenu: {
						id: "helpmenu",
						itemdata: [
							{
								text: "About", 
								disabled: false, 
								onclick: 
									{
										fn: function() {
											editor.dialogs.showHelpAboutDialog();
										}
									}
								},
							//{text: "FAQs", disabled: true}
						],
					}
				}	
			];//_menuData
			
		WINDOW_MENUITEM_INDEX = 1;
	}
	else {
		_menuData = 
			[   

    			{ 
        			text: "File", 
        			submenu: {  
            			id: "filemenu", 
            			itemdata: [                
                			{ 
								text: "Open...", 
								onclick: 
									{
										fn: 	function() {
													editor.ontologySelector.selectorPanel.buttons.getButton(0).set('disabled', true);
													editor.ontologySelector.selectorPanel.buttons.getButton(0).set('checked', false);
													editor.ontologySelector.selectorPanel.buttons.getButton(1).focus();	
													editor.ontologySelector.selectorPanel.buttons.getButton(1).set('checked', false);
													editor.ontologySelector.ontologyTable.unselectAllRows();							
													editor.ontologySelector.selectorPanel.show();								
												}
									}									
							},
            			],
        			}    
    			},    
    			{
					text: "Window",
					submenu: {
						id: "windowsmenu",
						itemdata: [
						],
					}
				},
    			{
					text: "Help",
					submenu: {
						id: "helpmenu",
						itemdata: [
							{
								text: "About", 
								disabled: false, 
								onclick: 
									{
										fn: function() {
											editor.dialogs.showHelpAboutDialog();
										}
									}
								},
							//{text: "FAQs", disabled: true}
						],
					}
				}	
			];//_menuData		
		WINDOW_MENUITEM_INDEX = 1;
	}


	
	
	//Instantiate a MenuBar:  The first argument passed to the constructor is the id of the
	//element to be created; the second is an object literal of configuration properties.
	
     self._menuBar = new YAHOO.widget.MenuBar("editormenubar", { 
                                                lazyload: true, 
                                                itemdata: _menuData,
												showdelay: 10 	//time (in milliseconds) that should expire
																// before a submenu is made visible when the
																//user mouses over the menu's items
                                                }); 


    
    //Since this MenuBar instance is built completely from 
    //script, call the "render" method passing in a node 
    //reference for the DOM element that its should be 
    //appended to.
	
     self._menuBar.render('menubarhole');
    

	// Add a "show" event listener for each submenu.	
	function onSubmenuShow() {

		var oIFrame,
			oElement,
			nOffsetWidth;
			
		this.bringToTop();//Always keep submenus on top of any other element
		
		//Need to make sure that the 'Windows' submenu is showing the right content
		if (this.id == "windowsmenu") {
			
			//XXX Hack. For sum reason changing this.itemData does not
			//always change the items displayed in the submenu
			//So need a workaround that clears the contents currently displayed
			//and then repopulates
			this.clearContent();
			
			for (var i = 0; i < this.itemData.length; i++) {
				this.addItem(this.itemData[i]);
			}
			this.render(this.element);
		}
		
		
		// Keep the left-most submenu against the left edge of the browser viewport

		if (this.id == "filemenu") {
			
			YAHOO.util.Dom.setX(this.element, 0);

			oIFrame = this.iframe;            

			if (oIFrame) {	
				YAHOO.util.Dom.setX(oIFrame, 0);	
			}
			
			this.cfg.setProperty("x", 0, true);
		
		}
 

		
		//Need to set the width for submenus of submenus in IE to prevent the mouseout 
		//event from firing prematurely when the user mouses off of a MenuItem's 
		//text node.		

		 if ((this.id == "filemenu" || this.id == "editmenu") && YAHOO.env.ua.ie) {

			oElement = this.element;
			nOffsetWidth = oElement.offsetWidth; 
	
			
			//Measuring the difference of the offsetWidth before and after
			//setting the "width" style attribute allows us to compute the 
			//about of padding and borders applied to the element, which in 
			//turn allows us to set the "width" property correctly.			
			
			oElement.style.width = nOffsetWidth + "px";
			oElement.style.width = (nOffsetWidth - (oElement.offsetWidth - nOffsetWidth)) + "px";
		
		}		
	}
    

    // Subscribe to the "show" event for each submenu    
    self._menuBar.subscribe("show", onSubmenuShow);
	
	
	self.updateWindowSubmenu = function (ontologyWindows) {
		
		var _onClick = function (p_sType, p_aArgs, index){			
						
				
			//Bring the ontology that the user has clicked into focus
			editor.ontologyWindowsManager.focus(editor.ontologyWindows[index].panel.id)
			
			//Let editor know what the current ontology is
			editor.selectedOntology = editor.ontologyWindows[index].windowTitle;			
			
			//Make sure the rendering functions know which is the currently active window
			editor.renderer.setActiveWindowID();

			//Uncheck the currently checked menu item
			for (var i = 0; i < this.parent.itemData.length; i++) {
				this.parent.itemData[i].checked = false;
			}
						
			//'Check' the currently active menu item
			//XXX Hack. Ideally this code should be 'this.cfg.setProperty("checked", true)'
			//But for some reason setting "checked" through 'this.cfg.setProperty' isn't working	
			this.parent.itemData[index].checked = true; 	

		}			

		
		//Clear previous menu item data by resetting the length of the array to 0)
		self._menuBar.itemData[WINDOW_MENUITEM_INDEX].submenu.itemdata.length = 0;
		
		var i;
		for (i = 0; i < ontologyWindows.length; i++) {
			var windowTitle = ontologyWindows[i].windowTitle
			var windowOnTop = false;
			
			if (editor.ontologyWindowsManager.getActive() == ontologyWindows[i].panel) {
				windowOnTop = true;
			}
		
			self._menuBar.itemData[WINDOW_MENUITEM_INDEX].submenu.itemdata.push(
				{
					text: (i+1)+' '+windowTitle, 
					checked: windowOnTop,
					onclick: {
						fn: _onClick, 
						obj: i //An index to the relevant ontology window in 'ontologyWindows' array						
					}
				}
			);
		
		}				
		
	}

}
 
