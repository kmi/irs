function EditorDialogs (editor) {
	//summary: All the dialog boxes used by the editor
	//description: This defines the various dialogue boxes used by the editor.
	//		These dialog boxes are typically opened from the menu bar


	var self = this;	//According to (http://www.crockford.com/javascript/private.html), we should follow the convention
					    //to add a 'self' (which is not accessible from outside the Editor object but is global within the object) in order to make the current instance
 					    //available to the inner methods. Apparently, this is a workaround for an error in the ECMAScript Language
					    //Specification which causes this to be set incorrectly for inner functions.

	this.newOntologyDialog;
	this.newGoalDescriptionDialog;
	this.newWebServiceDescriptionDialog;
	this.newMediatorDescriptionDialog;
	this.openFromLocationDialog;
	this.openFromListDialog; //This will eventually replace the old way of generating the list of available ontologies defined in 'EditorOntologyControl' (18/08/2008)
	this.deleteOntologyDialog;
	this.deleteGoalDescriptionDialog;
	this.deleteWebServiceDescriptionDialog;
	this.deleteMediatorDescriptionDialog;
	this.editOntologyPropertiesDialog;
	this.editGoalDescriptionDialog;
	this.editWebServiceDescriptionDialog;
	this.editMediatorDescriptionDialog;
	this.publishHTTPGETDialog;
	this.publishLispFunctionDialog;
	this.publishJavaClassDialog;
	this.publishWSDLDialog;
	this.runIRSGoalDialog;
	this.runKnowledgeBaseQueryDialog;
	this.helpAboutDialog;
	
	this.init = function () {
		_generateAllDialogHTML();
		_drawNewOntologyDialog();
		_drawNewGoalDialog();
		_drawNewWebServiceDescriptionDialog();
		_drawNewMediatorDescriptionDialog();
		_drawEditOntologyPropertiesDialog();
		_drawEditGoalDescriptionDialog();
		_drawEditWebServiceDescriptionDialog();
		_drawEditMediatorDescriptionDialog();
		_drawDeleteOntologyDialog();
		//_drawRunKnowledgeBaseQueryDialog();
		_drawHelpAboutDialog();
	}
		
///////////////////////////////////////////////////////////////////	
//{{{ START OF 'New Ontology' dialog					
	function _drawNewOntologyDialog () {
	
		self.newOntologyDialog = new YAHOO.widget.Dialog("new-ontology-dialog", 
									{
										width : "30em",
							  			fixedcenter : true,
							  			visible : false, 
							  			constraintoviewport : true,
										modal:true,
							  			buttons : [ 
											{
												text:"Submit", 
												handler:_handleSubmit, 
												isDefault:true 
											},
								      		{
												text:"Cancel", 
												handler:_handleCancel 
											}
										]	  
									}
								);
							
		self.newOntologyDialog.validate = function () {
			var data = this.getData();
			
			if (data.ontologyname == ""){
				yuiAlert('Please enter an ontology name');
				return false;
			}
			else if(data.ontologyname.search(/\s/) > -1) {
				yuiAlert ('No whitespace allowed in the ontology name');
				return false;
			}
			else {
				return true;
			}
//[@TODO008] Need to check if there are other validation criteria that need to be implemented			
		}		
					
		self.newOntologyDialog.render();		

		function _handleSubmit () {
			
			if (this.validate()) {
			
				var data = this.getData(); //The assumption is that 'this' resolves to the current Dialog instance
									//"...the scope of [the function assigned to the 'handler' property of the dialog buttons] is always its Dialog instance..."
									//(See http://developer.yahoo.com/yui/container/dialog/index.html#button)
				
				//Tips for dealing with SOAP taken from
				//http://www.zachleat.com/web/2007/05/09/wash-your-mouth-out-with-soap-and-the-yui-connection-manager/
				var postData = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<CREATE-ONTOLOGY>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<USER-PASSWORD type="string">wsmo</USER-PASSWORD>' +
							'<ONTOLOGY-NAME type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY-NAME>' +
							'<ONTOLOGY-TYPE type="sexpr">' +
								data.ontologytype +
							'</ONTOLOGY-TYPE>' +
							'<ONTOLOGY-USES type="sexpr">(' +
								data.ontologyparents +
							')</ONTOLOGY-USES>' +
							'<ALLOWED-EDITORS type="string">(' +
								data.allowededitors +
							')</ALLOWED-EDITORS>' +
						'</CREATE-ONTOLOGY>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'
				
				
				var callback = {
					success: _handleSubmitSuccess,
					failure: _handleSubmitFailure,
				}
				var targetURL = '/soap';
				
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', false);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
				
				this.hide();//Close the dialog box
			}
		}
		
		function _handleSubmitSuccess (responseObj) {
			
			 
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
				//yuiAlert(result[0].firstChild.nodeValue);
				//yuiAlert('Success: New ontology has been created!')
				//Refresh the Ontology List held by the editor
				editor.ontologySelector.updateOntologySelectorMenu();
				
				var ontologyName = self.newOntologyDialog.getData().ontologyname;
				editor.selectOntology(ontologyName.toUpperCase()); //The UPPERCASE is important
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//[@TODO009] Need to add code for processing different types of ERROR and WARNING messages from server

			}
			
			
//[@TODO011] Eventually, the success handler needs to update the ontology list (i.e. providing at the server-end an ontology has actually been added)

		}
		
		function _handleSubmitFailure (responseObj) {
			yuiAlert(responseObj.status);
		}
		
		function _handleCancel () {
			this.cancel();	//The assumption is that 'this' resolves to the current Dialog instance
							//By default the Dialog is automatically hidden after cancel
		}		
		
	}//_drawNewOntologyDialog
	
	this.showNewOntologyDialog = function () {
		self.newOntologyDialog.show();
		self.newOntologyDialog.bringToTop();
	}
//}}} END OF 'New Ontology' dialog 
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//{{{ START OF 'New Goal' dialog					
	function _drawNewGoalDialog () {
	
		self.newGoalDialog = new YAHOO.widget.Dialog("new-goal-dialog", 
									{
										width : "60em",
							  			fixedcenter : true,
							  			visible : false, 
							  			constraintoviewport : true,
										modal:true,
							  			buttons : [ 
											{
												text:"Save", 
												handler:_handleSave, 
												isDefault:true 
											},
								      		{
												text:"Cancel", 
												handler:_handleCancel 
											}
										]	  
									}
								);
		var soapTypes = [
			{value: "", text: ""},
			{value: "string", text: "string"},
			{value: "int", text: "int"},
			{value: "float", text: "float"},
			{value: "sexpr", text: "sexpr"},
			{value: "xml", text: "xml"}
		];
		var myColumnDefs = [
			{key:"inputname", label:"Name", editor:"textbox", width: "200px", resizeable: false},
			{key:"inputtype", label:"Type", editor:"textbox", width: "100px", resizeable: false},
			{
				key:"soaptype", 
				label:"SOAP Type", 
				editor:"dropdown", 
				editorOptions: {
					dropdownOptions: soapTypes
				}, 
				width: "100px", 
				resizeable: false
			}
		];
		
		var blankRow = {inputname: "", inputtype: "", soaptype: ""};
		
		//Initialise the datatable with a blank row
		//The data will be obtained from the user		
		var myDataSource = new YAHOO.util.DataSource([blankRow]);
		
		myDataSource.responseType = YAHOO.util.DataSource.TYPE_JSARRAY;
        myDataSource.responseSchema = {
            fields: ["inputname", "inputtype", "soaptype"]
        };
		
		self.newGoalDialog.inputRoleTable = new YAHOO.widget.DataTable(
												"new-goal-input-role-table", 
												myColumnDefs, 
												myDataSource, 
												{
													scrollable:true, 
													height:"20em", 
													selectionMode: "single",
													fixedWidth: true
												}
											)
				
		// Subscribe to events for row selection 
		self.newGoalDialog.inputRoleTable.subscribe("cellClickEvent", self.newGoalDialog.inputRoleTable.onEventShowCellEditor);
		self.newGoalDialog.inputRoleTable.subscribe("cellMouseoverEvent", self.newGoalDialog.inputRoleTable.onEventHighlightCell);
		self.newGoalDialog.inputRoleTable.subscribe("cellMouseoutEvent", self.newGoalDialog.inputRoleTable.onEventUnhighlightCell);
		self.newGoalDialog.inputRoleTable.subscribe("cellDblclickEvent", self.newGoalDialog.inputRoleTable.onEventHighlightRow);
		
		//Buttons for adding and deleting rows in the datatable
		self.newGoalDialog.buttons = new YAHOO.widget.ButtonGroup (
											{
												id: "adddeletebuttons", 
												container: "new-goal-input-role-table-buttons"
											}
									);
		
		//Configure the two buttons							
		self.newGoalDialog.buttons.addButtons ([
			{
				label: "Add Input", 
				onclick: {
					fn: _handleAdd, 
					scope: self.newGoalDialog.inputRoleTable
				}, 
				type: "push"
			},
			{
				label: "Delete Input", 
				onclick: {
					fn: _handleDelete, 
					scope: self.newGoalDialog.inputRoleTable
				},
				type: "push"
			}
		]);
		
		function _handleAdd (){
			var blankRow = {inputname: "", inputtype: "", soaptype: ""};
			
			var newRow = YAHOO.widget.DataTable._cloneObject(blankRow);
			
			this.addRow(newRow);
		}
		
		function _handleDelete (){
			var rowToDelete = this.getSelectedRows()[0];
			
			this.deleteRow(rowToDelete);
		}
		
							
		self.newGoalDialog.validate = function () {
			var data = this.getData();
			
			if (data.goalname == ""){
				yuiAlert('Please enter a goal name');
				return false;
			}
			else if(data.goalname.search(/\s/) > -1) {
				yuiAlert ('No whitespace allowed in the goal name');
				return false;
			}
			else {
				return true;
			}
//[@TODO012] Need to check if there are other validation criteria that need to be implemented
//Definitely need to check that if an input-role or output role name is given, that
//the user has also filled in the Type and the SOAP Type.			
		}	
		//When the 'show' event fires for the dialog box, get the current ontology name	
		self.newGoalDialog.showEvent.subscribe(_getOntologyName, self.newGoalDialog, true);		
			
		self.newGoalDialog.render();
		
		function _getOntologyName(){
		
			var currentOntology;
			
			if (typeof(editor.selectedOntology) == 'undefined') {
				
				this.hide();
				editor.ontologySelector.selectorPanel.show();
				yuiAlert('Please select an ontology first');
			}
			else {
				var newGoalDialogForm = document.forms['new-goal-dialog-form'];
				
				//Reset the form in case there are stray values in the boxes
				newGoalDialogForm.reset();
				
				//Put the name of the current ontology in the form
				currentOntology = editor.selectedOntology;
				newGoalDialogForm.elements['ontologyname'].value = currentOntology;
			}	
		}
		
		function _numberOfInputRoleValuePairs(){
			var recordSet = self.newGoalDialog.inputRoleTable.getRecordSet();
			
			return recordSet.getLength();
		}
		
		function _inputRolesToSOAPTags () {
			var SOAPTags = "";
			
			for (var i = 0; i < _numberOfInputRoleValuePairs(); i++) {
				
				var inputname = self.newGoalDialog.inputRoleTable.getRecord(i).getData("inputname");
				var inputtype = self.newGoalDialog.inputRoleTable.getRecord(i).getData("inputtype");
				var soaptype = self.newGoalDialog.inputRoleTable.getRecord(i).getData("soaptype");
				
				SOAPTags = SOAPTags + 
							'<INPUT-NAME type="sexpr">' +
								inputname +
							'</INPUT-NAME>' +
							'<INPUT-TYPE type="sexpr">' +
								inputtype +
							'</INPUT-TYPE>' +
							'<INPUT-SOAP-TYPE type="sexpr">' +
								soaptype +
							'</INPUT-SOAP-TYPE>';
			}
			
			return SOAPTags;
		}							

		function _handleSave () {
			
			if (this.validate()) {
			
				var data = this.getData(); 
				
				var postData = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<SAVE-GOAL-DESCRIPTION>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<GOAL-NAME type="sexpr">' +
								data.goalname +
							'</GOAL-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<GOAL-PARENT type="sexpr">' +
								data.goalparent +
							'</GOAL-PARENT>' +
							'<NUMBER-OF-INPUT-ROLE-VALUE-PAIRS type="int">' +
								_numberOfInputRoleValuePairs() +
							'</NUMBER-OF-INPUT-ROLE-VALUE-PAIRS>' +
							
							_inputRolesToSOAPTags() + 
							
							'<OUTPUT-NAME type="sexpr">' +
								data.outputrolename +
							'</OUTPUT-NAME>' +
							'<OUTPUT-TYPE type="sexpr">' +
								(data.outputroletype==""? 'nil': data.outputroletype) +
							'</OUTPUT-TYPE>' +
							'<OUTPUT-SOAP-TYPE type="sexpr">' +
								data.outputsoaptype +
							'</OUTPUT-SOAP-TYPE>' +
							'<USED-MEDIATOR type="sexpr">' +
								 'nil' + //data.goalmediator
							'</USED-MEDIATOR>' +
							'<POST-CONDITION type="sexpr">' +
								 'nil' + //data.postcondition
							'</POST-CONDITION>' +
							'<EFFECT type="sexpr">' +
								 'nil' + //data.effect
							'</EFFECT>' +
							'<NON-FUNC-PROPS type="sexpr">' +
								 '(nil nil nil nil)' + //data.nonfunctionalproperties
							'</NON-FUNC-PROPS>' +
						'</SAVE-GOAL-DESCRIPTION>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'
				
				
				var callback = {
					success: _handleSaveSuccess,
					failure: _handleSaveFailure
				}
								
				var targetURL = '/soap';
				
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', false);
				YAHOO.util.Connect.initHeader('Accept', 'text/xml', false);
				YAHOO.util.Connect.initHeader('Cache-Control', 'no-cache', false);
				YAHOO.util.Connect.initHeader('Pragma', 'no-cache', false);
				YAHOO.util.Connect.initHeader('Content-length', postData.length.toString(), false);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
				
				this.hide();//Close the dialog box
			}
		}
		
		function _handleSaveSuccess (responseObj) {
			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
				//yuiAlert(result[0].firstChild.nodeValue);
				yuiAlert('Success: New goal has been created!')
				editor.noOntologyData = true;
				editor.drawPanels(editor.selectedOntology);				
				
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
		}
		
		function _handleSaveFailure (responseObj) {
			yuiAlert(responseObj.status);
		}
		
		function _handleCancel () {
			this.cancel();
		}		
		
	}//_drawNewGoalDialog
	
	this.showNewGoalDialog = function () {
		
		if (typeof(editor.selectedOntology) == 'undefined') {
			editor.ontologySelector.selectorPanel.show();
			yuiAlert('Please select an ontology first');
		}
		else {
			self.newGoalDialog.show();
			self.newGoalDialog.bringToTop();
		}
	}
//}}} END OF 'New Goal' dialog 
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//{{{ START OF 'New Web Service Description' dialog	
	function _drawNewWebServiceDescriptionDialog() {
		self.newWebServiceDescriptionDialog = new YAHOO.widget.Dialog("new-web-service-description-dialog", 
									{
										width: "50em",
							  			fixedcenter : true,
							  			visible : false, 
							  			constraintoviewport : true,
										modal:true,
							  			buttons : [ 
											{
												text:"Save", 
												handler:_handleSave, 
												isDefault:true 
											},
								      		{
												text:"Cancel", 
												handler:_handleCancel 
											}
										]	  
									}
								);
								
		//Need to create a TabView within the New Web Service Description dialog box
		//The TabView will have two tabs - 'Capability' and 'Interface'
		var capabilityInterfaceTabs = new YAHOO.widget.TabView('capability-interface-tabs');
		capabilityInterfaceTabs.set('activeIndex', 0)
		
		self.newWebServiceDescriptionDialog.validate = function () {
			var data = this.getData();
			
			if (data.webservicename == ""){
				yuiAlert('Please enter a web service name');
				return false;
			}
			else if(data.webservicename.search(/\s/) > -1) {
				yuiAlert ('No whitespace allowed in the mediator name');
				return false;
			}
			else if(data.capabilitymediator == ""){
				yuiAlert('Please enter a mediator name');
				return false;
			}
			else {
				return true;
			}
//[@TODO012] Need to check if there are other validation criteria that need to be implemented
//Definitely need to check that if an input-role or output role name is given, that
//the user has also filled in the Type and the SOAP Type.			
		}	
		//When the 'show' event fires for the dialog box, get the current ontology name	
		self.newWebServiceDescriptionDialog.showEvent.subscribe(_getOntologyName, self.newWebServiceDescriptionDialog, true);		
				
		self.newWebServiceDescriptionDialog.render();
		
		function _getOntologyName(){
		
			var currentOntology;
			
			if (typeof(editor.selectedOntology) == 'undefined') {
								
				this.hide();
				editor.ontologySelector.selectorPanel.show();
				yuiAlert('Please select an ontology first');
			}
			else {
				var newWebServiceDescriptionDialogForm = document.forms['new-web-service-description-dialog-form'];
				
				//Reset the form in case there are stray values in the boxes
				newWebServiceDescriptionDialogForm.reset();
				
				//Put the name of the current ontology in the form
				currentOntology = editor.selectedOntology;
				newWebServiceDescriptionDialogForm.elements['ontologyname'].value = currentOntology;
			}	
		}
		
		
		function _handleSave () {
			if (this.validate()) {
			
				var data = this.getData(); 							
				
								
				//First message: SAVE-WEB-SERVICE-DESCRIPTION
				var postDataPart1 = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<SAVE-WEB-SERVICE-DESCRIPTION>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<WEB-SERVICE-NAME type="sexpr">' +
								data.webservicename +
							'</WEB-SERVICE-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<WEB-SERVICE-PARENT type="sexpr">' +
								data.webserviceparent +
							'</WEB-SERVICE-PARENT>' +
							'<NUMBER-OF-INPUT-ROLE-VALUE-PAIRS type="int">' +
								'0' + //removed from the interface but still part of SOAP message
							'</NUMBER-OF-INPUT-ROLE-VALUE-PAIRS>' +							
							'<OUTPUT-NAME type="sexpr">' +
								'nil' + //removed from interface but still part of SOAP message
							'</OUTPUT-NAME>' +
							'<OUTPUT-TYPE type="sexpr">' +
								'nil' + //removed from interface but still part of SOAP message
							'</OUTPUT-TYPE>' +
							'<CAPABILITY type="sexpr">' +
								data.capabilityname +
							'</CAPABILITY>' +
							'<INTERFACE type="sexpr">' +
								data.interfacename +
							'</INTERFACE>' +
							'<USED-MEDIATOR type="sexpr">' +
								 'nil' + //removed from interface but still part of SOAP message
							'</USED-MEDIATOR>' +
							'<NON-FUNC-PROPS type="sexpr">' +
								 '(nil nil nil nil)' + //not part of the interface but still part of SOAP message
							'</NON-FUNC-PROPS>' +
						'</SAVE-WEB-SERVICE-DESCRIPTION>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'				

				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', true);
				YAHOO.util.Connect.initHeader('Accept', 'text/xml', true);
				YAHOO.util.Connect.initHeader('Cache-Control', 'no-cache', true);
				YAHOO.util.Connect.initHeader('Pragma', 'no-cache', true);
				YAHOO.util.Connect.initHeader('Content-length', postDataPart1.length.toString(), false);
									
				//Second message: SAVE-CAPABILITY-DESCRIPTION
				var postDataPart2 = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<SAVE-CAPABILITY-DESCRIPTION>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<CAPABILITY-NAME type="sexpr">' +
								data.capabilityname +
							'</CAPABILITY-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<CAPABILITY-PARENT type="sexpr">' +
								'capability' +
							'</CAPABILITY-PARENT>' +
							'<USED-MEDIATOR type="sexpr">' +
								 (data.capabilitymediator=="" ? 'nil': data.capabilitymediator) + 
							'</USED-MEDIATOR>' +
							'<PRE-CONDITION type="sexpr">' +
								(data.precondition=="" ? 'nil': data.precondition) +
							'</PRE-CONDITION>' +
							'<POST-CONDITION type="sexpr">' +
								(data.postcondition=="" ? 'nil': data.postcondition) +
							'</POST-CONDITION>' +
							'<ASSUMPTION type="sexpr">' +
								(data.assumption=="" ? 'nil': data.assumption) +
							'</ASSUMPTION>' +
							'<EFFECT type="sexpr">' +
								(data.effect=="" ? 'nil': data.effect) +
							'</EFFECT>' +
							'<NON-FUNC-PROPS-NAME type="sexpr">' +
								 '(nil nil nil nil)' + //not part of the interface but still part of SOAP message
							'</NON-FUNC-PROPS-NAME>' +
						'</SAVE-CAPABILITY-DESCRIPTION>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'				

				
				//Third message: SAVE-INTERFACE-DESCRIPTION
				var postDataPart3 = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<SAVE-INTERFACE-DESCRIPTION>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<INTERFACE-NAME type="sexpr">' +
								data.interfacename +
							'</INTERFACE-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<INTERFACE-PARENT type="sexpr">' +
								'interface' +
							'</INTERFACE-PARENT>' +
							'<USED-MEDIATOR type="sexpr">' +
								 'nil' + 
							'</USED-MEDIATOR>' +
							'<CHOREOGRAPHY-GROUNDING type="sexpr">' +
								(data.grounding=="" ? 'nil': data.grounding) +
							'</CHOREOGRAPHY-GROUNDING>' +
							'<CHOREOGRAPHY-BODY type="sexpr">' +
								(data.guardedtransitions=="" ? 'nil': data.guardedtransitions) +
							'</CHOREOGRAPHY-BODY>' +
							'<ORCHESTRATION-BODY type="sexpr">' +
								(data.orchestration=="" ? 'nil': data.orchestration) +
							'</ORCHESTRATION-BODY>' +
							'<NON-FUNC-PROPS type="sexpr">' +
								 '(nil nil nil nil)' + //not part of the interface but still part of SOAP message
							'</NON-FUNC-PROPS>' +
						'</SAVE-INTERFACE-DESCRIPTION>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'		
				
				//Fourth message: PUBLISH-WSMO-HTTP-GET-REQUEST
				var postDataPart4 = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<PUBLISH-WSMO-HTTP-GET-REQUEST>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<PSM-NAME type="sexpr">' +
								data.webservicename +
							'</PSM-NAME>' +
							'<HOST type="string">' +
								data.host + 
							'</HOST>' +
							'<PORT type="string">' +
								data.port +
							'</PORT>' +
							'<PATH type="string">' +
								data.path +
							'</PATH>' +
							'<MESSAGE-EXCHANGE-PATTERN type="string">' +
								'nil' +
							'</MESSAGE-EXCHANGE-PATTERN>' +
							'<VARS type="string">' +
								 ' ' + //Need to include this blank space because of server-side bug
							'</VARS>' +
						'</PUBLISH-WSMO-HTTP-GET-REQUEST>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>';
							
				var targetURL = '/soap';
				
				
				//This only works if the callbacks are declared in reverse order
				var callbackPart4 = {
					success: _handleSaveSuccessPart4,
					failure: _handleSaveFailure
				}		
					
				var callbackPart3 = {
					success: _handleSaveSuccessPart3,
					failure: _handleSaveFailure,
					argument: {url: targetURL, postData: postDataPart4, callback: callbackPart4}
				}		
					
				var callbackPart2 = {
					success: _handleSaveSuccessPart2,
					failure: _handleSaveFailure,
					argument: {url: targetURL, postData: postDataPart3, callback: callbackPart3}
				}
									
				var callbackPart1 = {
					success: _handleSaveSuccessPart1,
					failure: _handleSaveFailure,
					argument: {url: targetURL, postData: postDataPart2, callback: callbackPart2}
				}
	
				
				var postCallPart1 = YAHOO.util.Connect.asyncRequest('POST', targetURL, callbackPart1, postDataPart1)		
								
				this.hide();//Close the dialog box
			}
		}
		
		
		
		function _handleSaveSuccessPart1 (responseObj) {			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			var targetURL = responseObj.argument.url; 
			var postData = responseObj.argument.postData;
			var callback = responseObj.argument.callback;
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
			
				//If part 1 is successful then move on to part two of the message
				YAHOO.util.Connect.initHeader('Content-length', postData.length.toString(), false);
				
				var postCallPart2 = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)						
				
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
		}
		
		function _handleSaveSuccessPart2 (responseObj) {			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');			
			var targetURL = responseObj.argument.url; 
			var postData = responseObj.argument.postData;
			var callback = responseObj.argument.callback;
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
							
				//If part 2 is successful then move on to part three of the message
				YAHOO.util.Connect.initHeader('Content-length', postData.length.toString(), false);
				
				var postCallPart3 = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
								
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
		}
		
		function _handleSaveSuccessPart3 (responseObj) {			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');			
			var targetURL = responseObj.argument.url; 
			var postData = responseObj.argument.postData;
			var callback = responseObj.argument.callback;
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
							
				//If part 3 is successful then move on to part four of the message
				YAHOO.util.Connect.initHeader('Content-length', postData.length.toString(), false);
				
				var postCallPart4 = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
								
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
		}
		
		function _handleSaveSuccessPart4 (responseObj) {			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
				yuiAlert('Success: New web service has been created!');
				editor.noOntologyData = true;
				editor.drawPanels(editor.selectedOntology);				
				
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
		}
		
		function _handleSaveFailure (responseObj) {
			yuiAlert(responseObj.status);
		}
		
		function _handleCancel () {
			this.cancel();
		}
	}
	
	this.showNewWebServiceDescriptionDialog = function () {
		
		if (typeof(editor.selectedOntology) == 'undefined') {
			editor.ontologySelector.selectorPanel.show();
			yuiAlert('Please select an ontology first');
		}
		else {
			self.newWebServiceDescriptionDialog.show();
			self.newWebServiceDescriptionDialog.bringToTop();
		}
	}
//}}} END OF 'New Web Service Description' dialog 
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//{{{ START OF 'New Mediator Description' dialog					
	function _drawNewMediatorDescriptionDialog(){
	
		self.newMediatorDescriptionDialog = new YAHOO.widget.Dialog("new-mediator-description-dialog", 
											{
												width: "45em",
												fixedcenter: true,
												visible: false,
												constraintoviewport: true,
												modal:true,
												buttons: [
													{
														text: "Save",
														handler: _handleSave,
														isDefault: true
													},
													{
														text: "Cancel",
														handler: _handleCancel
													}
												]
											});
		
		self.newMediatorDescriptionDialog.validate = function () {
			var data = this.getData();
			
			if (data.mediatorname == ""){
				yuiAlert('Please enter a mediator name');
				return false;
			}
			else if (data.sourcecomponent == ""){
				yuiAlert('Please enter a Source Component');
				return false;
			}
			else if(data.mediatorname.search(/\s/) > -1) {
				yuiAlert ('No whitespace allowed in the mediator name');
				return false;
			}
			else {
				return true;
			}
//[@TODO012] Need to check if there are other validation criteria that need to be implemented
//Definitely need to check that if an input-role or output role name is given, that
//the user has also filled in the Type and the SOAP Type.			
		}	
		//When the 'show' event fires for the dialog box, get the current ontology name	
		self.newMediatorDescriptionDialog.showEvent.subscribe(_getOntologyName, self.newMediatorDescriptionDialog, true);		
			
		self.newMediatorDescriptionDialog.render();
		
		function _getOntologyName(){
		
			var currentOntology;
			
			if (typeof(editor.selectedOntology) == 'undefined') {
				
				this.hide();
				editor.ontologySelector.selectorPanel.show();
				yuiAlert('Please select an ontology first');
			}
			else {
				var newMediatorDescriptionDialogForm = document.forms['new-mediator-description-dialog-form'];
				
				//Reset the form in case there are stray values in the boxes
				newMediatorDescriptionDialogForm.reset();
				
				//Put the name of the current ontology in the form
				currentOntology = editor.selectedOntology;
				newMediatorDescriptionDialogForm.elements['ontologyname'].value = currentOntology;
			}	
		}
		
		function _handleSave () {
			
			if (this.validate()) {
			
				var data = this.getData(); 
				
				var postData = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<SAVE-MEDIATOR-DESCRIPTION>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<MEDIATOR-NAME type="sexpr">' +
								data.mediatorname +
							'</MEDIATOR-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<MEDIATOR-PARENT type="sexpr">' +
								data.mediatorparent +
							'</MEDIATOR-PARENT>' +
							'<USED-MEDIATOR type="sexpr">' +
								 'nil' + //not part of UI but has to be part of SOAP message
							'</USED-MEDIATOR>' +
							'<SOURCE type="sexpr">' +
								 (data.sourcecomponent=="" ? 'nil': data.sourcecomponent) +
							'</SOURCE>' +
							'<TARGET type="sexpr">' +
								 (data.targetcomponent=="" ? 'nil': data.targetcomponent) + 
							'</TARGET>' +
							'<MEDIATION-SERVICE type="sexpr">' +
								 'nil' + //not part of UI but has to be part of SOAP message
							'</MEDIATION-SERVICE>' +
							'<MAPPING-RULES type="sexpr">' +
								 'nil' + //not part of UI but has to be part of SOAP message
							'</MAPPING-RULES>' +
							'<NON-FUNC-PROPS type="sexpr">' +
								 '(nil nil nil nil)' + //not part of UI but has to be part of SOAP message
							'</NON-FUNC-PROPS>' +
						'</SAVE-MEDIATOR-DESCRIPTION>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'
				
				
				var callback = {
					success: _handleSaveSuccess,
					failure: _handleSaveFailure
				}
								
				var targetURL = '/soap';
				
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', false);
				YAHOO.util.Connect.initHeader('Accept', 'text/xml', false);
				YAHOO.util.Connect.initHeader('Cache-Control', 'no-cache', false);
				YAHOO.util.Connect.initHeader('Pragma', 'no-cache', false);
				YAHOO.util.Connect.initHeader('Content-length', postData.length.toString(), false);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
				
				this.hide();//Close the dialog box
			}
		}
		
		function _handleSaveSuccess (responseObj) {
			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
				//yuiAlert(result[0].firstChild.nodeValue);
				yuiAlert('Success: New Mediator has been created!')
				editor.noOntologyData = true;
				editor.drawPanels(editor.selectedOntology);				
				
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
		}
		
		function _handleSaveFailure (responseObj) {
			yuiAlert(responseObj.status);
		}
		
		function _handleCancel () {
			this.cancel();
		}
											
	}
	
	this.showNewMediatorDescriptionDialog = function () {
		
		if (typeof(editor.selectedOntology) == 'undefined') {
			editor.ontologySelector.selectorPanel.show();
			yuiAlert('Please select an ontology first');
		}
		else {
			self.newMediatorDescriptionDialog.show();
			self.newMediatorDescriptionDialog.bringToTop();
		}
	}
//}}} END OF 'New Mediator Description' dialog 
///////////////////////////////////////////////////////////////////							
///////////////////////////////////////////////////////////////////
//{{{ START OF 'Edit Ontology Properties' dialog	
	function _drawEditOntologyPropertiesDialog () {
		self.editOntologyPropertiesDialog = new YAHOO.widget.Dialog("edit-ontology-dialog", 
												{ 
													width : "30em",
							  						fixedcenter : true,
							  						visible : false, 
							  						constraintoviewport : true,
													modal:true,
							 						buttons : [ 
														{ 
															text:"Update", 
															handler:_handleUpdate, 
															isDefault:true
														},
								      					{ 
															text:"Cancel", 
															handler:_handleCancel 
														} 
													]
												}
											);
							
		self.editOntologyPropertiesDialog.validate = function () {
			var data = this.getData();
			
			if (data.ontologyname == ""){
				yuiAlert('Please enter an ontology name');
				return false;
			}
			else if(data.ontologyname.search(/\s/) > -1) {
				yuiAlert ('No whitespace allowed in the ontology name');
				return false;
			}
			else {
				return true;
			}
//Need to check if there are other validation criteria that need to be implemented			
		}		
		self.editOntologyPropertiesDialog.showEvent.subscribe(_getOntologyProperties, self.editOntologyPropertiesDialog, true);
		self.editOntologyPropertiesDialog.render();	
		
		function _getOntologyProperties(){
			
			var currentOntology;
			
			function _handleGetPropertiesSuccess (responseObj) {
				//Here we populate the fields in the 'Edit Ontology Properties' dialog
				//Based on the response to our GET-ONTOLOGY-PROPERTIES request
				//The SOAP-Body format for the response is as follows:
				//<GET-ONTOLOGY-PROPERTIES-RESPONSEResponse>
				//	<ONTOLOGY-INCLUDES type="string">wsmo</ONTOLOGY-INCLUDES>
				//	<ONTOLOGY-ALLOWED-EDITORS type="string">enrico john</ONTOLOGY-ALLOWED-EDITORS>
				//	<ONTOLOGY-AUTHOR type="string">wsmo</ONTOLOGY-AUTHOR>
				//	<ONTOLOGY-TYPE type="string">domain</ONTOLOGY-TYPE>
				//</GET-ONTOLOGY-PROPERTIES-RESPONSEResponse>
				
				var root = responseObj.responseXML.documentElement;
				var includes = root.getElementsByTagName ('ONTOLOGY-INCLUDES');
				var allowedEditors = root.getElementsByTagName ('ONTOLOGY-ALLOWED-EDITORS');
				var author = root.getElementsByTagName ('ONTOLOGY-AUTHOR');
				var ontologyType = root.getElementsByTagName ('ONTOLOGY-TYPE');
				
				document.forms['edit-ontology-dialog-form'].elements['ontologyname'].value = currentOntology;
				document.forms['edit-ontology-dialog-form'].elements['oldname'].value = currentOntology;
				document.forms['edit-ontology-dialog-form'].elements['ontologyparents'].value = includes[0].firstChild.nodeValue;
				document.forms['edit-ontology-dialog-form'].elements['allowededitors'].value = allowedEditors[0].firstChild.nodeValue;
				document.forms['edit-ontology-dialog-form'].elements['author'].value = author[0].firstChild.nodeValue;
				document.forms['edit-ontology-dialog-form'].elements['ontologytype'].value = ontologyType[0].firstChild.nodeValue;
			}
			
			function _handleGetPropertiesFailure (responseObj) {
				yuiAlert(responseObj.status);
			}				

					
			if (typeof(editor.selectedOntology) == 'undefined') {
				yuiAlert('Please select an ontology first');
				this.hide();
				editor.ontologySelector.selectorPanel.show();
				
			}
			else {
				currentOntology = editor.selectedOntology;
				
				var postData = 
					'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
					'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
					'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
						'<SOAP-ENV:Header/>' +
						'<SOAP-ENV:Body>' +
							'<GET-ONTOLOGY-PROPERTIES>' +
								'<USER-NAME type="string">wsmo</USER-NAME>' +
								'<USER-PASSWORD type="string">wsmo</USER-PASSWORD>' +
								'<ONTOLOGY-NAME type="sexpr">' +
									currentOntology +
								'</ONTOLOGY-NAME>' +
							'</GET-ONTOLOGY-PROPERTIES>' +
						'</SOAP-ENV:Body>' +
					'</SOAP-ENV:Envelope>'
				
				var callback = {
					success: _handleGetPropertiesSuccess,
					failure: _handleGetPropertiesFailure
				}
				var targetURL = '/soap';
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', false);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
			}			
		}	

		function _handleUpdate () {
			
			if (this.validate()) {
			
				var data = this.getData(); 
				
				
				var postData = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<UPDATE-ONTOLOGY-PROPERTIES>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<USER-PASSWORD type="string">wsmo</USER-PASSWORD>' +
							'<OLD-ONTOLOGY-NAME type="sexpr">' +
								data.oldname +
							'</OLD-ONTOLOGY-NAME>' +
							'<ONTOLOGY-NAME type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY-NAME>' +
							'<ONTOLOGY-TYPE type="sexpr">' +
								data.ontologytype +
							'</ONTOLOGY-TYPE>' +
							'<ONTOLOGY-USES type="sexpr">(' +
								data.ontologyparents +
							')</ONTOLOGY-USES>' +
							'<NEW-AUTHOR type="string">' +
								data.author +
							'</NEW-AUTHOR>' +
							'<ALLOWED-EDITORS type="string">(' +
								data.allowededitors +
							')</ALLOWED-EDITORS>' +
						'</UPDATE-ONTOLOGY-PROPERTIES>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'
				
				
				var callback = {
					success: _handleUpdateSuccess,
					failure: _handleUpdateFailure
				}
				var targetURL = '/soap';
				
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', false);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
				
				this.hide();//Close the dialog box
			}
		}
		
		function _handleUpdateSuccess (responseObj) {
			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
				//yuiAlert(result[0].firstChild.nodeValue);
				yuiAlert('Success: Ontology has been updated!')
				editor.ontologySelector.updateOntologySelectorMenu();
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
//Need to add code for processing different types of ERROR and WARNING messages from server
//Need to change from browser alerts to YUI style SimpleDialog boxes				
			}
			
			
//Eventually, the success handler needs to update the ontology list (i.e. providing at the server-end an ontology has actually been added)
		}
		
		function _handleUpdateFailure (responseObj) {
			yuiAlert(responseObj.status);
		}
		
		function _handleCancel () {
			this.cancel();//The assumption is that 'this' resolves to the current Dialog instance
			//By default the Dialog is automatically hidden after cancel
		}
	}
	
	this.showEditOntologyPropertiesDialog = function () {
		
		if (typeof(editor.selectedOntology) == 'undefined') {
			editor.ontologySelector.selectorPanel.show();
			yuiAlert('Please select an ontology first');
		}
		else {
			self.editOntologyPropertiesDialog.show();
			self.editOntologyPropertiesDialog.bringToTop();
		}
	}
	
//}}}END OF 'Edit Ontology Properties' dialog
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//{{{ START OF 'Edit Goal Description' dialog	
	function _drawEditGoalDescriptionDialog () {
		self.editGoalDescriptionDialog = new YAHOO.widget.Dialog("edit-goal-description-dialog", 
												{ 
													width : "60em",
							  						fixedcenter : true,
							  						visible : false, 
							  						constraintoviewport : true,
													modal:true,
							 						buttons : [ 
														{ 
															text:"Update", 
															handler:_handleUpdate, 
															isDefault:true
														},
								      					{ 
															text:"Cancel", 
															handler:_handleCancel 
														} 
													]
												}
											);

		var soapTypes = [
			{value: "", text: ""},
			{value: "string", text: "string"},
			{value: "int", text: "int"},
			{value: "float", text: "float"},
			{value: "sexpr", text: "sexpr"},
			{value: "xml", text: "xml"}
		];
		var myColumnDefs = [
			{key:"inputname", label:"Name", editor:"textbox", width: "200px", resizeable: false},
			{key:"inputtype", label:"Type", editor:"textbox", width: "100px", resizeable: false},
			{
				key:"soaptype", 
				label:"SOAP Type", 
				editor:"dropdown", 
				editorOptions: {
					dropdownOptions: soapTypes
				}, 
				width: "100px", 
				resizeable: false
			}
		];
		
		var blankRow = {inputname: "", inputtype: "", soaptype: ""};
		
		//Initialise the datatable with a blank row
		//The data will be obtained from the user		
		var myDataSource = new YAHOO.util.DataSource([blankRow]);
		
		myDataSource.responseType = YAHOO.util.DataSource.TYPE_JSARRAY;
        myDataSource.responseSchema = {
            fields: ["inputname", "inputtype", "soaptype"]
        };
		
		self.editGoalDescriptionDialog.inputRoleTable = new YAHOO.widget.DataTable(
												"edit-goal-input-role-table", 
												myColumnDefs, 
												myDataSource, 
												{
													scrollable:true, 
													height:"20em", 
													selectionMode: "single",
													fixedWidth: true
												}
											)
				
		// Subscribe to events for row selection 
		self.editGoalDescriptionDialog.inputRoleTable.subscribe("cellClickEvent", self.editGoalDescriptionDialog.inputRoleTable.onEventShowCellEditor);
		self.editGoalDescriptionDialog.inputRoleTable.subscribe("cellMouseoverEvent", self.editGoalDescriptionDialog.inputRoleTable.onEventHighlightCell);
		self.editGoalDescriptionDialog.inputRoleTable.subscribe("cellMouseoutEvent", self.editGoalDescriptionDialog.inputRoleTable.onEventUnhighlightCell);
		self.editGoalDescriptionDialog.inputRoleTable.subscribe("cellDblclickEvent", self.editGoalDescriptionDialog.inputRoleTable.onEventHighlightRow);
		
		//Buttons for adding and deleting rows in the datatable
		self.editGoalDescriptionDialog.buttons = new YAHOO.widget.ButtonGroup (
											{
												id: "editgoaladddeletebuttons", 
												container: "edit-goal-input-role-table-buttons"
											}
									);
		
		//Configure the two buttons							
		self.editGoalDescriptionDialog.buttons.addButtons ([
			{
				label: "Add Input", 
				onclick: {
					fn: _handleAdd, 
					scope: self.editGoalDescriptionDialog.inputRoleTable
				}, 
				type: "push"
			},
			{
				label: "Delete Input", 
				onclick: {
					fn: _handleDelete, 
					scope: self.editGoalDescriptionDialog.inputRoleTable
				},
				type: "push"
			}
		]);
		
		function _handleAdd (){
			var blankRow = {inputname: "", inputtype: "", soaptype: ""};
			
			var newRow = YAHOO.widget.DataTable._cloneObject(blankRow);
			
			this.addRow(newRow);
		}
		
		function _handleDelete (){
			var rowToDelete = this.getSelectedRows()[0];
			
			this.deleteRow(rowToDelete);
		}
							
		self.editGoalDescriptionDialog.validate = function () {			
			
//Need to check what validation criteria need to be implemented
			//var data = this.getData();
			return true;		
		}
		
		self.editGoalDescriptionDialog.showEvent.subscribe(_getGoals, self.editGoalDescriptionDialog, true);
		self.editGoalDescriptionDialog.render();	
		
		function _getGoals(){
			var goals = editor.renderer.activeWindow.ontologyData.wsmoEntitiesTree.goals;
			
			//Need to filter the goals so they only contain names from current ontology
			var filterredGoals = new Array();
			
			if (typeof(goals) != 'undefined' && goals.length != 0) {
				
				for (var i = 0; i < goals.length; i++) { 
					if (goals[i].homeOntology == editor.selectedOntology){
						filterredGoals.push(deepCloneObject(goals[i]));
					}
				}
			}
			
			goals = filterredGoals;
			
			//If there are no goals in the current ontology 
			//the system alerts user and then hides dialogue
			if (typeof(goals) == 'undefined' || goals.length == 0) {
				yuiAlert('There are no goals in this ontology');
				this.hide();
			}
			else {
				
				var editGoalDescriptionDialogForm = document.forms['edit-goal-description-dialog-form'];
				editGoalDescriptionDialogForm.reset();
				
				var goalNameDropList = editGoalDescriptionDialogForm.elements['goalname'];
				
				//Note that Form.reset() doesn't clear the drop down list. So...
				//First clear the previous goal contents of the drop down list
				//Leave the first option which is the "" (blank) option
				for (var i = goalNameDropList.options.length - 1; i > 0 ;i--) {					
					goalNameDropList.remove(i)
				}

				
				for (var i = 0; i < goals.length; i++) {
					var newOption = document.createElement('option');
					newOption.text = goals[i].name;
					newOption.value = goals[i].name;
					
					goalNameDropList.add(newOption, null);
				}
				
				//Add the 'onchange' event listener to the drop down list
				YAHOO.util.Event.addListener(goalNameDropList, "change", _getGoalDescription);
				
				//Check whether the currentInspectionElement is in the goals list.
				//If it is, then the system selects this goal from the drop down list,
				//and automatically gets the goal description
				var currentInspectionElement = editor.renderer.activeWindow.currentInspectionElement
				if (typeof(currentInspectionElement) != 'undefined') {
					for (var i = 0; i < goalNameDropList.options.length; i++) {
						if (goalNameDropList.options[i].value == currentInspectionElement){
							goalNameDropList.options[i].selected = true;
							
							 var changeEvent = new YAHOO.util.CustomEvent("change", goalNameDropList);
							 changeEvent.subscribe(_getGoalDescription, goalNameDropList);
							 changeEvent.fire();
						}
					}
				}
				
			}
		}
		
		function _getGoalDescription(){
			
			var currentOntology;
			
			//'this' resolves to the DropDown List of goals
			var goalname = this.value;
			
			function _handleGetGoalDescriptionSuccess (responseObj) {
				//Here we populate the fields in the 'Edit Goal Description' dialog
				//Based on the response to our GET-GOAL-DESCRIPTION request
				//The SOAP-Body format for the response is as follows:
				//<GET-GOAL-DESCRIPTION-RESPONSEResponse>
				//	<GOAL-PARENT type="string">goal </GOAL-PARENT>
				//	<NUMBER-OF-INPUT-ROLE-VALUE-PAIRS type="string">2</NUMBER-OF-INPUT-ROLE-VALUE-PAIRS>
				//	<INPUT-NAME type="string">dividend</INPUT-NAME>
				//	<INPUT-TYPE type="string">integer</INPUT-TYPE>
				//	<ONTOLOGY-NAME type="string">base-ontology</ONTOLOGY-NAME>
				//	<INPUT-SOAP-TYPE type="string">int</INPUT-SOAP-TYPE>
				//	<INPUT-NAME type="string">divisor</INPUT-NAME>
				//	<INPUT-TYPE type="string">integer</INPUT-TYPE>
				//	<ONTOLOGY-NAME type="string">base-ontology</ONTOLOGY-NAME>
				//	<INPUT-SOAP-TYPE type="string">int</INPUT-SOAP-TYPE>
				//	<OUTPUT-NAME type="string">quotient</OUTPUT-NAME>
				//	<OUTPUT-TYPE type="string">integer</OUTPUT-TYPE>
				//	<ONTOLOGY-NAME type="string">base-ontology</ONTOLOGY-NAME>
				//	<OUTPUT-SOAP-TYPE type="string">int</OUTPUT-SOAP-TYPE>
				//	<USED-MEDIATOR type="string"></USED-MEDIATOR>
				//	<POST-CONDITION type="string"></POST-CONDITION>
				//	<EFFECT type="string"></EFFECT>
				//	<NON-FUNCTIONAL-PROPERTIES type="string"></NON-FUNCTIONAL-PROPERTIES>
				//</GET-GOAL-DESCRIPTION-RESPONSEResponse>
				
				var root = responseObj.responseXML.documentElement;
				var goalParent = root.getElementsByTagName ('GOAL-PARENT');
				var numberOfInputRoles = root.getElementsByTagName('NUMBER-OF-INPUT-ROLE-VALUE-PAIRS');
				var inputName = root.getElementsByTagName ('INPUT-NAME');
				var inputType = root.getElementsByTagName ('INPUT-TYPE');
				var inputSoapType = root.getElementsByTagName ('INPUT-SOAP-TYPE');
				var outputName = root.getElementsByTagName ('OUTPUT-NAME');
				var outputType = root.getElementsByTagName ('OUTPUT-TYPE');
				var outputSoapType = root.getElementsByTagName ('OUTPUT-SOAP-TYPE');
				
				document.forms['edit-goal-description-dialog-form'].elements['goalname'].value = goalname;
				document.forms['edit-goal-description-dialog-form'].elements['ontologyname'].value = currentOntology;
				document.forms['edit-goal-description-dialog-form'].elements['goalparent'].value = (goalParent[0].firstChild == null ? "" : goalParent[0].firstChild.nodeValue);
				document.forms['edit-goal-description-dialog-form'].elements['outputrolename'].value = (outputName[0].firstChild == null ? "" : outputName[0].firstChild.nodeValue);
				document.forms['edit-goal-description-dialog-form'].elements['outputroletype'].value = (outputType[0].firstChild == null ? "" : outputType[0].firstChild.nodeValue);
				document.forms['edit-goal-description-dialog-form'].elements['outputsoaptype'].value = (outputSoapType[0].firstChild == null ? "" : outputSoapType[0].firstChild.nodeValue);
				
				//Now populate the Input Roles table
				numberOfInputRoles = parseInt(numberOfInputRoles[0].firstChild.nodeValue);
				var inputRows;
				
				if (numberOfInputRoles == 0) {
					inputRows = [{inputname: "", inputname: "", soaptype: ""}] //blank row
				}
				else {
					inputRows = new Array();
				
					for (var i = 0; i < numberOfInputRoles; i++) {
						
						var newRow = 
							{
								inputname: (inputName[i].firstChild == null ? "" : inputName[i].firstChild.nodeValue), 
								inputtype: (inputType[i].firstChild == null ? "" : inputType[i].firstChild.nodeValue), 
								soaptype: (inputSoapType[i].firstChild == null ? "" : inputSoapType[i].firstChild.nodeValue)
							}
						inputRows.push(newRow)
					}
				}				
				
				editor.dialogs.editGoalDescriptionDialog.inputRoleTable.getRecordSet().replaceRecords(inputRows);
				editor.dialogs.editGoalDescriptionDialog.inputRoleTable.render();
			}
			
			function _handleGetGoalDescriptionFailure (responseObj) {
				yuiAlert(responseObj.status);
			}				

					
			if (typeof(editor.selectedOntology) == 'undefined') {
				
				this.hide();
				editor.ontologySelector.selectorPanel.show();
				yuiAlert('Please select an ontology first');
			}
			else if (goalname != ""){
				currentOntology = editor.selectedOntology;
				
				var postData = 
					'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
					'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
					'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
						'<SOAP-ENV:Header/>' +
						'<SOAP-ENV:Body>' +
							'<GET-GOAL-DESCRIPTION>' +
								'<GOAL-NAME type="sexpr">' +
									goalname +
								'</GOAL-NAME>' +
								'<ONTOLOGY-NAME type="sexpr">' +
									currentOntology +
								'</ONTOLOGY-NAME>' +
							'</GET-GOAL-DESCRIPTION>' +
						'</SOAP-ENV:Body>' +
					'</SOAP-ENV:Envelope>'
				
				var callback = {
					success: _handleGetGoalDescriptionSuccess,
					failure: _handleGetGoalDescriptionFailure
				}
				var targetURL = '/soap';
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', false);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
			}			
		}//End of _getGoalDescription	

		function _numberOfInputRoleValuePairs(){
			var recordSet = self.editGoalDescriptionDialog.inputRoleTable.getRecordSet();
			
			return recordSet.getLength();
		}
		
		function _inputRolesToSOAPTags () {
			var SOAPTags = "";
			
			for (var i = 0; i < _numberOfInputRoleValuePairs(); i++) {
				
				var inputname = self.editGoalDescriptionDialog.inputRoleTable.getRecord(i).getData("inputname");
				var inputtype = self.editGoalDescriptionDialog.inputRoleTable.getRecord(i).getData("inputtype");
				var soaptype = self.editGoalDescriptionDialog.inputRoleTable.getRecord(i).getData("soaptype");
				
				SOAPTags = SOAPTags + 
							'<INPUT-NAME type="sexpr">' +
								inputname +
							'</INPUT-NAME>' +
							'<INPUT-TYPE type="sexpr">' +
								inputtype +
							'</INPUT-TYPE>' +
							'<INPUT-SOAP-TYPE type="sexpr">' +
								soaptype +
							'</INPUT-SOAP-TYPE>';
			}
			
			return SOAPTags;
		}							

		function _handleUpdate () {
			
			if (this.validate()) {
			
				var data = this.getData(); 
				
				var postData = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<SAVE-GOAL-DESCRIPTION>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<GOAL-NAME type="sexpr">' +
								data.goalname +
							'</GOAL-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<GOAL-PARENT type="sexpr">' +
								data.goalparent +
							'</GOAL-PARENT>' +
							'<NUMBER-OF-INPUT-ROLE-VALUE-PAIRS type="int">' +
								_numberOfInputRoleValuePairs() +
							'</NUMBER-OF-INPUT-ROLE-VALUE-PAIRS>' +
							
							_inputRolesToSOAPTags() + 
							
							'<OUTPUT-NAME type="sexpr">' +
								data.outputrolename +
							'</OUTPUT-NAME>' +
							'<OUTPUT-TYPE type="sexpr">' +
								(data.outputroletype==""? 'nil': data.outputroletype) +
							'</OUTPUT-TYPE>' +
							'<OUTPUT-SOAP-TYPE type="sexpr">' +
								data.outputsoaptype +
							'</OUTPUT-SOAP-TYPE>' +
							'<USED-MEDIATOR type="sexpr">' +
								 'nil' + //data.goalmediator
							'</USED-MEDIATOR>' +
							'<POST-CONDITION type="sexpr">' +
								 'nil' + //data.postcondition
							'</POST-CONDITION>' +
							'<EFFECT type="sexpr">' +
								 'nil' + //data.effect
							'</EFFECT>' +
							'<NON-FUNC-PROPS type="sexpr">' +
								 '(nil nil nil nil)' + //data.nonfunctionalproperties
							'</NON-FUNC-PROPS>' +
						'</SAVE-GOAL-DESCRIPTION>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'
				
				
				var callback = {
					success: _handleUpdateSuccess,
					failure: _handleUpdateFailure
				}
								
				var targetURL = '/soap';
				
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', false);
				YAHOO.util.Connect.initHeader('Accept', 'text/xml', false);
				YAHOO.util.Connect.initHeader('Cache-Control', 'no-cache', false);
				YAHOO.util.Connect.initHeader('Pragma', 'no-cache', false);
				YAHOO.util.Connect.initHeader('Content-length', postData.length.toString(), false);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
				
				this.hide();//Close the dialog box
			}
		}
		
		function _handleUpdateSuccess (responseObj) {
			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
				//yuiAlert(result[0].firstChild.nodeValue);
				yuiAlert('Success: Goal Description has been updated!');
				editor.noOntologyData = true;
				editor.drawPanels(editor.selectedOntology);	
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
			
//Eventually, the success handler needs to update the ontology list (i.e. providing at the server-end an ontology has actually been added)
		}
		
		function _handleUpdateFailure (responseObj) {
			yuiAlert(responseObj.status);
		}
		
		function _handleCancel () {
			this.cancel();//The assumption is that 'this' resolves to the current Dialog instance
			//By default the Dialog is automatically hidden after cancel
		}
	}//_drawEditGoalDescriptionDialog
	
	this.showEditGoalDescriptionDialog = function () {
		
		if (typeof(editor.selectedOntology) == 'undefined') {
			editor.ontologySelector.selectorPanel.show();
			yuiAlert('Please select an ontology first');
		}
		else {
			self.editGoalDescriptionDialog.show();
			self.editGoalDescriptionDialog.bringToTop();
		}
	}
	
//}}}END OF 'Edit Goal Description' dialog
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//{{{ START OF 'Edit Web Service Description' dialog
	function _drawEditWebServiceDescriptionDialog(){
		self.editWebServiceDescriptionDialog = new YAHOO.widget.Dialog("edit-web-service-description-dialog", 
												{ 
													width : "50em",
							  						fixedcenter : true,
							  						visible : false, 
							  						constraintoviewport : true,
													modal:true,
							 						buttons : [ 
														{ 
															text:"Update", 
															handler:_handleUpdate, 
															isDefault:true
														},
								      					{ 
															text:"Cancel", 
															handler:_handleCancel 
														} 
													]
												}
											);
		//Need to create a TabView within the New Web Service Description dialog box
		//The TabView will have two tabs - 'Capability' and 'Interface'
		var capabilityInterfaceTabs = new YAHOO.widget.TabView('capability-interface-tabs-edit-form');
		capabilityInterfaceTabs.set('activeIndex', 0)
												
		self.editWebServiceDescriptionDialog.validate = function () {			
			
//Need to check what validation criteria need to be implemented
			//var data = this.getData();
			return true;		
		}
		
		self.editWebServiceDescriptionDialog.showEvent.subscribe(_getWebServices, self.editWebServiceDescriptionDialog, true);
		self.editWebServiceDescriptionDialog.render();	
		
		function _getWebServices(){
			var services = editor.renderer.activeWindow.ontologyData.wsmoEntitiesTree.services;
			
			//Need to filter the services so they only contain names from current ontology
			var filterredServices = new Array();
			
			if (typeof(services) != 'undefined' && services.length != 0) {
				
				for (var i = 0; i < services.length; i++) { 
					if (services[i].homeOntology == editor.selectedOntology){
						filterredServices.push(deepCloneObject(services[i]));
					}
				}
			}
			
			services = filterredServices;
			
			//If there are no services in the current ontology 
			//the system alerts user and then hides dialogue
			if (typeof(services) == 'undefined' || services.length == 0) {
				yuiAlert('There are no services in this ontology');
				this.hide();
			}
			else {
				var editWebServiceDescriptionDialogForm = document.forms['edit-web-service-description-dialog-form'];
				editWebServiceDescriptionDialogForm.reset();
												
				var serviceNameDropList = editWebServiceDescriptionDialogForm.elements['webservicename'];

				//Note that Form.reset() doesn't clear the drop down list. So...				
				//First clear the previous services contents of the drop down list
				//Leave the first option which is the "" (blank) option
				for (var i = serviceNameDropList.options.length - 1; i > 0 ;i--) {					
					serviceNameDropList.remove(i);
				}
				
				for (var i = 0; i < services.length; i++) {
					var newOption = document.createElement('option');
					newOption.text = services[i].name;
					newOption.value = services[i].name;
					
					serviceNameDropList.add(newOption, null);
				}
				
				//Add the 'onchange' event listener to the drop down list
				YAHOO.util.Event.addListener(serviceNameDropList, "change", _getWebServiceDescription);
				
				//Check whether the currentInspectionElement is in the services list.
				//If it is, then the system selects this service from the drop down list,
				//and automatically gets the service description
				var currentInspectionElement = editor.renderer.activeWindow.currentInspectionElement
				if (typeof(currentInspectionElement) != 'undefined') {
					for (var i = 0; i < serviceNameDropList.options.length; i++) {
						if (serviceNameDropList.options[i].value == currentInspectionElement){
							serviceNameDropList.options[i].selected = true;
							
							 var changeEvent = new YAHOO.util.CustomEvent("change", serviceNameDropList);
							 changeEvent.subscribe(_getWebServiceDescription, serviceNameDropList);
							 changeEvent.fire();
						}
					}
				}
				
			}
		}
		
		function _getWebServiceDescription(){
			
			var currentOntology;
			
			//'this' resolves to the DropDown List of goals
			var webservicename = this.value;
			
			function _handleGetWebServiceDescriptionSuccess (responseObj) {
				//Here we populate the fields in the 'Edit Web Service Description' dialog
				//Based on the response to our GET-WEB-SERVICE-DESCRIPTION request
				//The SOAP-Body format for the response is as follows:
				//<GET-WEB-SERVICE-DESCRIPTION-RESPONSEResponse>
				//	<WEB-SERVICE-PARENT type="string">web-service </WEB-SERVICE-PARENT>
				//	<NUMBER-OF-INPUT-ROLE-VALUE-PAIRS type="string">0</NUMBER-OF-INPUT-ROLE-VALUE-PAIRS>
				//	<OUTPUT-NAME type="string"></OUTPUT-NAME>
				//	<OUTPUT-TYPE type="string"></OUTPUT-TYPE>
				//	<OUTPUT-TYPE-ONTOLOGY type="string"></OUTPUT-TYPE-ONTOLOGY>
				//	<CAPABILITY type="string">bendy-multiply-service-capability</CAPABILITY>
				//	<INTERFACE type="string">bendy-multiply-service-interface</INTERFACE>
				//	<USED-MEDIATOR type="string"></USED-MEDIATOR>
				//	<NON-FUNCTIONAL-PROPERTIES type="string"></NON-FUNCTIONAL-PROPERTIES>
				//</GET-WEB-SERVICE-DESCRIPTION-RESPONSEResponse>
				
				var root = responseObj.responseXML.documentElement;
				var webServiceParent = root.getElementsByTagName ('WEB-SERVICE-PARENT');
				var capability = root.getElementsByTagName('CAPABILITY');
				var interface = root.getElementsByTagName ('INTERFACE');
				
				document.forms['edit-web-service-description-dialog-form'].elements['webservicename'].value = webservicename;
				document.forms['edit-web-service-description-dialog-form'].elements['ontologyname'].value = currentOntology;
				document.forms['edit-web-service-description-dialog-form'].elements['webserviceparent'].value = (webServiceParent[0].firstChild == null ? "" : webServiceParent[0].firstChild.nodeValue);
				document.forms['edit-web-service-description-dialog-form'].elements['capabilityname'].value = (capability[0].firstChild == null ? "" : capability[0].firstChild.nodeValue);
				document.forms['edit-web-service-description-dialog-form'].elements['interfacename'].value = (interface[0].firstChild == null ? "" : interface[0].firstChild.nodeValue);
									
				//Now need to send the post messages to get CAPABILITY and INTERFACE descriptions
				
				var capabilityname = (capability[0].firstChild == null ? "" : capability[0].firstChild.nodeValue);
				var interfacename = (interface[0].firstChild == null ? "" : interface[0].firstChild.nodeValue);
				
				var targetURL = '/soap';
				
				if (capabilityname != "") {
				
					var postDataCAPABILITY = 
					'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
					'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
					'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
						'<SOAP-ENV:Header/>' +
						'<SOAP-ENV:Body>' +
							'<GET-CAPABILITY-DESCRIPTION>' +
								'<CAPABILITY-NAME type="sexpr">' +
									capabilityname +
								'</CAPABILITY-NAME>' +
								'<ONTOLOGY-NAME type="sexpr">' +
									currentOntology +
								'</ONTOLOGY-NAME>' +
							'</GET-CAPABILITY-DESCRIPTION>' +
						'</SOAP-ENV:Body>' +
					'</SOAP-ENV:Envelope>'
					
					var callbackCAPABILITY = {
						success: _handleGetCapabilityDescriptionSuccess,
						failure: _handleGetCapabilityDescriptionFailure
					}
					
					var postCallCAPABILITY = YAHOO.util.Connect.asyncRequest('POST', targetURL, callbackCAPABILITY, postDataCAPABILITY);
				}
				
				if (interfacename != "") {
					var postDataINTERFACE = 
					'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
					'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
					'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
						'<SOAP-ENV:Header/>' +
						'<SOAP-ENV:Body>' +
							'<GET-INTERFACE-DESCRIPTION>' +
								'<INTERFACE-NAME type="sexpr">' +
									interfacename +
								'</INTERFACE-NAME>' +
								'<ONTOLOGY-NAME type="sexpr">' +
									currentOntology +
								'</ONTOLOGY-NAME>' +
							'</GET-INTERFACE-DESCRIPTION>' +
						'</SOAP-ENV:Body>' +
					'</SOAP-ENV:Envelope>'
					
					var callbackINTERFACE = {
						success: _handleGetInterfaceDescriptionSuccess,
						failure: _handleGetInterfaceDescriptionFailure
					}
					
					var postCallINTERFACE = YAHOO.util.Connect.asyncRequest('POST', targetURL, callbackINTERFACE, postDataINTERFACE);
					
				}
				
				var postDataPUBLISHER = 
					'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
					'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
					'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
						'<SOAP-ENV:Header/>' +
						'<SOAP-ENV:Body>' +
							'<GET-PUBLISHER-INFORMATION>' +
								'<WEB-SERVICE type="sexpr">' +
									webservicename +
								'</WEB-SERVICE>' +
								'<ONTOLOGY-NAME type="sexpr">' +
									currentOntology +
								'</ONTOLOGY-NAME>' +
							'</GET-PUBLISHER-INFORMATION>' +
						'</SOAP-ENV:Body>' +
					'</SOAP-ENV:Envelope>'					
				
				var callbackPUBLISHER = {
						success: _handleGetPublisherInformationSuccess,
						failure: _handleGetPublisherInformationFailure
					}					
				
				var postCallPUBLISHER = YAHOO.util.Connect.asyncRequest('POST', targetURL, callbackPUBLISHER, postDataPUBLISHER);
				
			}
			
			function _handleGetWebServiceDescriptionFailure (responseObj) {
				yuiAlert(responseObj.status);
			}				

			function _handleGetCapabilityDescriptionSuccess (responseObj) {
				//Here we populate the fields in the 'Edit Web Service Description' dialog
				//Based on the response to our GET-CAPABILITY-DESCRIPTION request
				//The SOAP-Body format for the response is as follows:
				//<GET-CAPABILITY-DESCRIPTION-RESPONSEResponse>
				//	<CAPABILITY-PARENT type="string">capability </CAPABILITY-PARENT>
				//	<USED-MEDIATOR type="string">bendy-multiple-wg-mediator</USED-MEDIATOR>
				//	<PRE-CONDITION type="string"></PRE-CONDITION>
				//	<POST-CONDITION type="string"></POST-CONDITION>
				//	<ASSUMPTION type="string"></ASSUMPTION>
				//	<EFFECT type="string"></EFFECT>
				//	<NON-FUNCTIONAL-PROPERTIES type="string"></NON-FUNCTIONAL-PROPERTIES>
				//</GET-CAPABILITY-DESCRIPTION-RESPONSEResponse>
				
				var root = responseObj.responseXML.documentElement;
				//var capabilityParent = root.getElementsByTagName ('CAPABILITY-PARENT');
				var usedMediator = root.getElementsByTagName('USED-MEDIATOR');
				var preCondition = root.getElementsByTagName ('PRE-CONDITION');
				var postCondition = root.getElementsByTagName ('POST-CONDITION');
				var assumption = root.getElementsByTagName ('ASSUMPTION');
				var effect = root.getElementsByTagName ('EFFECT');
				
				document.forms['edit-web-service-description-dialog-form'].elements['capabilitymediator'].value = (usedMediator[0].firstChild == null ? "" : usedMediator[0].firstChild.nodeValue);
				document.forms['edit-web-service-description-dialog-form'].elements['precondition'].value = (preCondition[0].firstChild == null ? "" : preCondition[0].firstChild.nodeValue);
				document.forms['edit-web-service-description-dialog-form'].elements['postcondition'].value = (postCondition[0].firstChild == null ? "" : postCondition[0].firstChild.nodeValue);
				document.forms['edit-web-service-description-dialog-form'].elements['assumption'].value = (assumption[0].firstChild == null ? "" : assumption[0].firstChild.nodeValue);
				document.forms['edit-web-service-description-dialog-form'].elements['effect'].value = (effect[0].firstChild == null ? "" : effect[0].firstChild.nodeValue);
				
			}
			
			function _handleGetCapabilityDescriptionFailure (responseObj) {
				yuiAlert(responseObj.status);
			}
			
			function _handleGetInterfaceDescriptionSuccess (responseObj) {
				//Here we populate the fields in the 'Edit Web Service Description' dialog
				//Based on the response to our GET-INTERFACE-DESCRIPTION request
				//The SOAP-Body format for the response is as follows:
				//<GET-INTERFACE-DESCRIPTION-RESPONSEResponse>
				//	<INTERFACE-PARENT type="string">interface </INTERFACE-PARENT>
				//	<USED-MEDIATOR type="string"></USED-MEDIATOR>
				//	<CHOREOGRAPHY type="string">bendy-multiply-service-interface-choreography</CHOREOGRAPHY>
				//	<CHOREOGRAPHY-GROUNDING type="sexpr">((GROUNDED-TO-HTTP (NORMAL (NIL (A B)))))</CHOREOGRAPHY-GROUNDING>
				//	<CHOREOGRAPHY-GUARDED-TRANSITIONS type="string"></CHOREOGRAPHY-GUARDED-TRANSITIONS>
				//	<ORCHESTRATION type="string">bendy-multiply-service-interface-orchestration</ORCHESTRATION>
				//	<ORCHESTRATION-BODY type="string"></ORCHESTRATION-BODY>
				//	<NON-FUNCTIONAL-PROPERTIES type="string"></NON-FUNCTIONAL-PROPERTIES>
				//</GET-INTERFACE-DESCRIPTION-RESPONSEResponse>
				
				var root = responseObj.responseXML.documentElement;
				//var interfaceParent = root.getElementsByTagName ('INTERFACE-PARENT');
				var grounding = root.getElementsByTagName('CHOREOGRAPHY-GROUNDING');
				var guardedTransitions = root.getElementsByTagName ('CHOREOGRAPHY-GUARDED-TRANSITIONS');
				var orchestration = root.getElementsByTagName ('ORCHESTRATION-BODY');
				
				document.forms['edit-web-service-description-dialog-form'].elements['grounding'].value = (grounding[0].firstChild == null ? "" : grounding[0].firstChild.nodeValue);
				document.forms['edit-web-service-description-dialog-form'].elements['guardedtransitions'].value = (guardedTransitions[0].firstChild == null ? "" : guardedTransitions[0].firstChild.nodeValue);
				document.forms['edit-web-service-description-dialog-form'].elements['orchestration'].value = (orchestration[0].firstChild == null ? "" : orchestration[0].firstChild.nodeValue);
				
			}
			
			function _handleGetInterfaceDescriptionFailure (responseObj) {
				yuiAlert(responseObj.status);
			}
			
			function _handleGetPublisherInformationSuccess (responseObj) {
				//Here we populate the fields in the 'Edit Web Service Description' dialog
				//Based on the response to our GET-PUBLISHER-INFORMATION request
				//The SOAP-Body format for the response is as follows:
				//<GET-PUBLISHER-INFORMATION-RESPONSEResponse>
				//	<WEB-SERVICE-HOST type="string">137.108.25.145</WEB-SERVICE-HOST>
				//	<WEB-SERVICE-PORT type="string">8080</WEB-SERVICE-PORT>
				//	<WEB-SERVICE-LOCATION type="string">/benniverse/multiply</WEB-SERVICE-LOCATION>
				//</GET-PUBLISHER-INFORMATION-RESPONSEResponse>
				
				var root = responseObj.responseXML.documentElement;
				var host = root.getElementsByTagName('WEB-SERVICE-HOST');
				var port = root.getElementsByTagName ('WEB-SERVICE-PORT');
				var path = root.getElementsByTagName ('WEB-SERVICE-LOCATION');
				
				document.forms['edit-web-service-description-dialog-form'].elements['host'].value = (host[0].firstChild == null ? "" : host[0].firstChild.nodeValue);
				document.forms['edit-web-service-description-dialog-form'].elements['port'].value = (port[0].firstChild == null ? "" : port[0].firstChild.nodeValue);
				document.forms['edit-web-service-description-dialog-form'].elements['path'].value = (path[0].firstChild == null ? "" : path[0].firstChild.nodeValue);
				
			}
			
			function _handleGetPublisherInformationFailure (responseObj) {
				yuiAlert(responseObj.status);
			}			
					
			if (typeof(editor.selectedOntology) == 'undefined') {
				
				this.hide();
				editor.ontologySelector.selectorPanel.show();
				yuiAlert('Please select an ontology first');
			}
			else if (webservicename != ""){
				currentOntology = editor.selectedOntology;
				
				var postData = 
					'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
					'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
					'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
						'<SOAP-ENV:Header/>' +
						'<SOAP-ENV:Body>' +
							'<GET-WEB-SERVICE-DESCRIPTION>' +
								'<WEB-SERVICE-NAME type="sexpr">' +
									webservicename +
								'</WEB-SERVICE-NAME>' +
								'<ONTOLOGY-NAME type="sexpr">' +
									currentOntology +
								'</ONTOLOGY-NAME>' +
							'</GET-WEB-SERVICE-DESCRIPTION>' +
						'</SOAP-ENV:Body>' +
					'</SOAP-ENV:Envelope>'
				
				var callback = {
					success: _handleGetWebServiceDescriptionSuccess,
					failure: _handleGetWebServiceDescriptionFailure
				}
				var targetURL = '/soap';
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', true);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
			}			
		}//End of _getWebServiceDescription	

		function _handleUpdate () {
			
			if (this.validate()) {
			
				var data = this.getData(); 							
				
								
				//First message: SAVE-WEB-SERVICE-DESCRIPTION
				var postDataPart1 = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<SAVE-WEB-SERVICE-DESCRIPTION>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<WEB-SERVICE-NAME type="sexpr">' +
								data.webservicename +
							'</WEB-SERVICE-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<WEB-SERVICE-PARENT type="sexpr">' +
								data.webserviceparent +
							'</WEB-SERVICE-PARENT>' +
							'<NUMBER-OF-INPUT-ROLE-VALUE-PAIRS type="int">' +
								'0' + //removed from the interface but still part of SOAP message
							'</NUMBER-OF-INPUT-ROLE-VALUE-PAIRS>' +							
							'<OUTPUT-NAME type="sexpr">' +
								'nil' + //removed from interface but still part of SOAP message
							'</OUTPUT-NAME>' +
							'<OUTPUT-TYPE type="sexpr">' +
								'nil' + //removed from interface but still part of SOAP message
							'</OUTPUT-TYPE>' +
							'<CAPABILITY type="sexpr">' +
								data.capabilityname +
							'</CAPABILITY>' +
							'<INTERFACE type="sexpr">' +
								data.interfacename +
							'</INTERFACE>' +
							'<USED-MEDIATOR type="sexpr">' +
								 'nil' + //removed from interface but still part of SOAP message
							'</USED-MEDIATOR>' +
							'<NON-FUNC-PROPS type="sexpr">' +
								 '(nil nil nil nil)' + //not part of the interface but still part of SOAP message
							'</NON-FUNC-PROPS>' +
						'</SAVE-WEB-SERVICE-DESCRIPTION>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'				

				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', true);
				YAHOO.util.Connect.initHeader('Accept', 'text/xml', true);
				YAHOO.util.Connect.initHeader('Cache-Control', 'no-cache', true);
				YAHOO.util.Connect.initHeader('Pragma', 'no-cache', true);
				YAHOO.util.Connect.initHeader('Content-length', postDataPart1.length.toString(), false);
									
				//Second message: SAVE-CAPABILITY-DESCRIPTION
				var postDataPart2 = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<SAVE-CAPABILITY-DESCRIPTION>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<CAPABILITY-NAME type="sexpr">' +
								data.capabilityname +
							'</CAPABILITY-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<CAPABILITY-PARENT type="sexpr">' +
								'capability' +
							'</CAPABILITY-PARENT>' +
							'<USED-MEDIATOR type="sexpr">' +
								 (data.capabilitymediator=="" ? 'nil': data.capabilitymediator) + 
							'</USED-MEDIATOR>' +
							'<PRE-CONDITION type="sexpr">' +
								(data.precondition=="" ? 'nil': data.precondition) +
							'</PRE-CONDITION>' +
							'<POST-CONDITION type="sexpr">' +
								(data.postcondition=="" ? 'nil': data.postcondition) +
							'</POST-CONDITION>' +
							'<ASSUMPTION type="sexpr">' +
								(data.assumption=="" ? 'nil': data.assumption) +
							'</ASSUMPTION>' +
							'<EFFECT type="sexpr">' +
								(data.effect=="" ? 'nil': data.effect) +
							'</EFFECT>' +
							'<NON-FUNC-PROPS-NAME type="sexpr">' +
								 '(nil nil nil nil)' + //not part of the interface but still part of SOAP message
							'</NON-FUNC-PROPS-NAME>' +
						'</SAVE-CAPABILITY-DESCRIPTION>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'				

				
				//Third message: SAVE-INTERFACE-DESCRIPTION
				var postDataPart3 = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<SAVE-INTERFACE-DESCRIPTION>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<INTERFACE-NAME type="sexpr">' +
								data.interfacename +
							'</INTERFACE-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<INTERFACE-PARENT type="sexpr">' +
								'interface' +
							'</INTERFACE-PARENT>' +
							'<USED-MEDIATOR type="sexpr">' +
								 'nil' + 
							'</USED-MEDIATOR>' +
							'<CHOREOGRAPHY-GROUNDING type="sexpr">' +
								(data.grounding=="" ? 'nil': data.grounding) +
							'</CHOREOGRAPHY-GROUNDING>' +
							'<CHOREOGRAPHY-BODY type="sexpr">' +
								(data.guardedtransitions=="" ? 'nil': data.guardedtransitions) +
							'</CHOREOGRAPHY-BODY>' +
							'<ORCHESTRATION-BODY type="sexpr">' +
								(data.orchestration=="" ? 'nil': data.orchestration) +
							'</ORCHESTRATION-BODY>' +
							'<NON-FUNC-PROPS type="sexpr">' +
								 '(nil nil nil nil)' + //not part of the interface but still part of SOAP message
							'</NON-FUNC-PROPS>' +
						'</SAVE-INTERFACE-DESCRIPTION>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'		

				//Fourth message: PUBLISH-WSMO-HTTP-GET-REQUEST
				var postDataPart4 = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<PUBLISH-WSMO-HTTP-GET-REQUEST>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<PSM-NAME type="sexpr">' +
								data.webservicename +
							'</PSM-NAME>' +
							'<HOST type="string">' +
								data.host + 
							'</HOST>' +
							'<PORT type="string">' +
								data.port +
							'</PORT>' +
							'<PATH type="string">' +
								data.path +
							'</PATH>' +
							'<MESSAGE-EXCHANGE-PATTERN type="string">' +
								'nil' +
							'</MESSAGE-EXCHANGE-PATTERN>' +
							'<VARS type="string">' +
								 ' ' + //Need to include this blank space because of server-side bug
							'</VARS>' +
						'</PUBLISH-WSMO-HTTP-GET-REQUEST>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>';
							
				var targetURL = '/soap';
				
				
				//This only works if the callbacks are declared in reverse order
				var callbackPart4 = {
					success: _handleUpdateSuccessPart4,
					failure: _handleUpdateFailure
				}	
				
				var callbackPart3 = {
					success: _handleUpdateSuccessPart3,
					failure: _handleUpdateFailure,
					argument: {url: targetURL, postData: postDataPart4, callback: callbackPart4}
				}		
					
				var callbackPart2 = {
					success: _handleUpdateSuccessPart2,
					failure: _handleUpdateFailure,
					argument: {url: targetURL, postData: postDataPart3, callback: callbackPart3}
				}
									
				var callbackPart1 = {
					success: _handleUpdateSuccessPart1,
					failure: _handleUpdateFailure,
					argument: {url: targetURL, postData: postDataPart2, callback: callbackPart2}
				}
	
				
				var postCallPart1 = YAHOO.util.Connect.asyncRequest('POST', targetURL, callbackPart1, postDataPart1)		
								
				this.hide();//Close the dialog box
			}
		}
		
		function _handleUpdateSuccessPart1 (responseObj) {			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			var targetURL = responseObj.argument.url; 
			var postData = responseObj.argument.postData;
			var callback = responseObj.argument.callback;
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
				
				//If part 1 is successful then move on to part two of the message
				YAHOO.util.Connect.initHeader('Content-length', postData.length.toString(), false);
				
				var postCallPart2 = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)						
				
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
		}
		
		function _handleUpdateSuccessPart2 (responseObj) {			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			var targetURL = responseObj.argument.url; 
			var postData = responseObj.argument.postData;
			var callback = responseObj.argument.callback;
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
							
				//If part 2 is successful then move on to part three of the message
				YAHOO.util.Connect.initHeader('Content-length', postData.length.toString(), false);
				
				var postCallPart3 = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
								
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
		}
		
		function _handleUpdateSuccessPart3 (responseObj) {			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			var targetURL = responseObj.argument.url; 
			var postData = responseObj.argument.postData;
			var callback = responseObj.argument.callback;			
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
							
				//If part 3 is successful then move on to part four of the message
				YAHOO.util.Connect.initHeader('Content-length', postData.length.toString(), false);
				
				var postCallPart4 = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)			
				
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
		}
		
		function _handleUpdateSuccessPart4 (responseObj) {			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
				yuiAlert('Success: Web Service Description has been updated!');
				editor.noOntologyData = true;
				editor.drawPanels(editor.selectedOntology);				
				
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}			
		}
		
		function _handleUpdateFailure (responseObj) {
			yuiAlert(responseObj.status);
		}
		
		function _handleCancel () {
			this.cancel();//The assumption is that 'this' resolves to the current Dialog instance
			//By default the Dialog is automatically hidden after cancel
		}
	}//_drawEditWebServiceDescriptionDialog
	
	this.showEditWebServiceDescriptionDialog = function () {
		
		if (typeof(editor.selectedOntology) == 'undefined') {			
			editor.ontologySelector.selectorPanel.show();
			yuiAlert('Please select an ontology first');
		}
		else {
			self.editWebServiceDescriptionDialog.show();
			self.editWebServiceDescriptionDialog.bringToTop();
		}
	}
//}}}END OF 'Edit Web Service Description' dialog
///////////////////////////////////////////////////////////////////							
///////////////////////////////////////////////////////////////////
//{{{ START OF 'Edit Mediator Description' dialog
	function _drawEditMediatorDescriptionDialog () {
		
		self.editMediatorDescriptionDialog = new YAHOO.widget.Dialog("edit-mediator-description-dialog", 
											{
												width: "45em",
												fixedcenter: true,
												visible: false,
												constraintoviewport: true,
												modal:true,
												buttons: [
													{
														text: "Update",
														handler: _handleUpdate,
														isDefault: true
													},
													{
														text: "Cancel",
														handler: _handleCancel
													}
												]
											});
											
		self.editMediatorDescriptionDialog.validate = function () {			
			
//Need to check what validation criteria need to be implemented
			var data = this.getData();
			
			if (data.sourcecomponent == "") {
				yuiAlert('Please enter a Source Component');
				return false;
			}
			else {
				return true;
			}		
		}
		
		self.editMediatorDescriptionDialog.showEvent.subscribe(_getMediators, self.editMediatorDescriptionDialog, true);
		self.editMediatorDescriptionDialog.render();	
		
		function _getMediators(){
			var mediators = editor.renderer.activeWindow.ontologyData.wsmoEntitiesTree.mediators;
			
			//Need to filter the services so they only contain names from current ontology
			var filterredMediators = new Array();
			
			if (typeof(mediators) != 'undefined' && mediators.length != 0) {
				
				for (var i = 0; i < mediators.length; i++) { 
					if (mediators[i].homeOntology == editor.selectedOntology){
						filterredMediators.push(deepCloneObject(mediators[i]));
					}
				}
			}
			
			mediators = filterredMediators;
			
			//If there are no services in the current ontology 
			//the system alerts user and then hides dialogue
			if (typeof(mediators) == 'undefined' || mediators.length == 0) {
				yuiAlert('There are no mediators in this ontology');
				this.hide();
			}
			else {
				var editMediatorDescriptionDialogForm = document.forms['edit-mediator-description-dialog-form'];
				editMediatorDescriptionDialogForm.reset();						
				
				var mediatorNameDropList = editMediatorDescriptionDialogForm.elements['mediatorname'];

				//Note that Form.reset() doesn't clear the drop down list. So...
				//First clear the previous mediators contents of the drop down list
				//Leave the first option which is the "" (blank) option
				for (var i = mediatorNameDropList.options.length - 1; i > 0 ;i--) {					
					mediatorNameDropList.remove(i);
				}
				
				for (var i = 0; i < mediators.length; i++) {
					var newOption = document.createElement('option');
					newOption.text = mediators[i].name;
					newOption.value = mediators[i].name;
					
					mediatorNameDropList.add(newOption, null);
				}
				
				//Add the 'onchange' event listener to the drop down list
				YAHOO.util.Event.addListener(mediatorNameDropList, "change", _getMediatorDescription);
				
				//Check whether the currentInspectionElement is in the services list.
				//If it is, then the system selects this mediator from the drop down list,
				//and automatically gets the mediator description
				var currentInspectionElement = editor.renderer.activeWindow.currentInspectionElement
				if (typeof(currentInspectionElement) != 'undefined') {
					for (var i = 0; i < mediatorNameDropList.options.length; i++) {
						if (mediatorNameDropList.options[i].value == currentInspectionElement){
							mediatorNameDropList.options[i].selected = true;
							
							 var changeEvent = new YAHOO.util.CustomEvent("change", mediatorNameDropList);
							 changeEvent.subscribe(_getMediatorDescription, mediatorNameDropList);
							 changeEvent.fire();
						}
					}
				}
				
			}
		}
		function _getMediatorDescription(){
			
			var currentOntology;
			
			//'this' resolves to the DropDown List of mediators
			var mediatorname = this.value;
			
			function _handleGetMediatorDescriptionSuccess (responseObj) {
				//Here we populate the fields in the 'Edit Mediator Description' dialog
				//Based on the response to our GET-MEDIATOR-DESCRIPTION request
				//The SOAP-Body format for the response is as follows:
				//<GET-MEDIATOR-DESCRIPTION-RESPONSEResponse>
				//	<MEDIATOR-PARENT type="string">wg-mediator </MEDIATOR-PARENT>
				//	<USED-MEDIATOR type="string"></USED-MEDIATOR>
				//	<SOURCE type="string">bendy-multiply-goal</SOURCE>
				//	<TARGET type="string"></TARGET>
				//	<MEDIATION-SERVICE type="string"></MEDIATION-SERVICE>
				//	<REDUCTION type="string"></REDUCTION>
				//	<NON-FUNCTIONAL-PROPERTIES type="string"></NON-FUNCTIONAL-PROPERTIES>
				//</GET-MEDIATOR-DESCRIPTION-RESPONSEResponse>
				
				var root = responseObj.responseXML.documentElement;
				var mediatorParent = root.getElementsByTagName ('MEDIATOR-PARENT');
				var source = root.getElementsByTagName('SOURCE');
				var target = root.getElementsByTagName ('TARGET');
				
				document.forms['edit-mediator-description-dialog-form'].elements['mediatorname'].value = mediatorname;
				document.forms['edit-mediator-description-dialog-form'].elements['ontologyname'].value = currentOntology;
				document.forms['edit-mediator-description-dialog-form'].elements['mediatorparent'].value = (mediatorParent[0].firstChild == null ? "" : mediatorParent[0].firstChild.nodeValue);
				document.forms['edit-mediator-description-dialog-form'].elements['sourcecomponent'].value = (source[0].firstChild == null ? "" : source[0].firstChild.nodeValue);
				document.forms['edit-mediator-description-dialog-form'].elements['targetcomponent'].value = (target[0].firstChild == null ? "" : target[0].firstChild.nodeValue);
				document.forms['edit-mediator-description-dialog-form'].elements['mediatortype'].value = (mediatorParent[0].firstChild == null ? "" : mediatorParent[0].firstChild.nodeValue);

			}
			
			function _handleGetMediatorDescriptionFailure (responseObj) {
				yuiAlert(responseObj.status);
			}				

					
			if (typeof(editor.selectedOntology) == 'undefined') {
				
				this.hide();
				editor.ontologySelector.selectorPanel.show();
				yuiAlert('Please select an ontology first');
			}
			else if (mediatorname != ""){
				currentOntology = editor.selectedOntology;
				
				var postData = 
					'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
					'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
					'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
						'<SOAP-ENV:Header/>' +
						'<SOAP-ENV:Body>' +
							'<GET-MEDIATOR-DESCRIPTION>' +
								'<MEDIATOR-NAME type="sexpr">' +
									mediatorname +
								'</MEDIATOR-NAME>' +
								'<ONTOLOGY-NAME type="sexpr">' +
									currentOntology +
								'</ONTOLOGY-NAME>' +
							'</GET-MEDIATOR-DESCRIPTION>' +
						'</SOAP-ENV:Body>' +
					'</SOAP-ENV:Envelope>'
				
				var callback = {
					success: _handleGetMediatorDescriptionSuccess,
					failure: _handleGetMediatorDescriptionFailure
				}
				var targetURL = '/soap';
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', false);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
			}			
		}//End of _getMediatorDescription
				
		function _handleUpdate () {
			
			if (this.validate()) {
			
				var data = this.getData(); 
				
				var postData = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<SAVE-MEDIATOR-DESCRIPTION>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<MEDIATOR-NAME type="sexpr">' +
								data.mediatorname +
							'</MEDIATOR-NAME>' +
							'<ONTOLOGY type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY>' +
							'<MEDIATOR-PARENT type="sexpr">' +
								data.mediatorparent +
							'</MEDIATOR-PARENT>' +
							'<USED-MEDIATOR type="sexpr">' +
								 'nil' + //not part of UI but has to be part of SOAP message
							'</USED-MEDIATOR>' +
							'<SOURCE type="sexpr">' +
								 (data.sourcecomponent=="" ? 'nil': data.sourcecomponent) +
							'</SOURCE>' +
							'<TARGET type="sexpr">' +
								 (data.targetcomponent=="" ? 'nil': data.targetcomponent) + 
							'</TARGET>' +
							'<MEDIATION-SERVICE type="sexpr">' +
								 'nil' + //not part of UI but has to be part of SOAP message
							'</MEDIATION-SERVICE>' +
							'<MAPPING-RULES type="sexpr">' +
								 'nil' + //not part of UI but has to be part of SOAP message
							'</MAPPING-RULES>' +
							'<NON-FUNC-PROPS type="sexpr">' +
								 '(nil nil nil nil)' + //not part of UI but has to be part of SOAP message
							'</NON-FUNC-PROPS>' +
						'</SAVE-MEDIATOR-DESCRIPTION>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'
				
				
				var callback = {
					success: _handleUpdateSuccess,
					failure: _handleUpdateFailure
				}
								
				var targetURL = '/soap';
				
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', false);
				YAHOO.util.Connect.initHeader('Accept', 'text/xml', false);
				YAHOO.util.Connect.initHeader('Cache-Control', 'no-cache', false);
				YAHOO.util.Connect.initHeader('Pragma', 'no-cache', false);
				YAHOO.util.Connect.initHeader('Content-length', postData.length.toString(), false);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
				
				this.hide();//Close the dialog box
			}
		}
		
		function _handleUpdateSuccess (responseObj) {
			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
				//yuiAlert(result[0].firstChild.nodeValue);
				yuiAlert('Success: Mediator Description has been updated!');
				editor.noOntologyData = true;
				editor.drawPanels(editor.selectedOntology);				
				
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
		}
		
		function _handleUpdateFailure (responseObj) {
			yuiAlert(responseObj.status);
		}
		
		function _handleCancel () {
			this.cancel();
		}
											
	}
	
	this.showEditMediatorDescriptionDialog = function () {
		
		if (typeof(editor.selectedOntology) == 'undefined') {			
			editor.ontologySelector.selectorPanel.show();
			yuiAlert('Please select an ontology first');
		}
		else {
			self.editMediatorDescriptionDialog.show();
			self.editMediatorDescriptionDialog.bringToTop();
		}
	}
//}}}END OF 'Edit Mediator Description' dialog
///////////////////////////////////////////////////////////////////							
///////////////////////////////////////////////////////////////////
//{{{ START OF 'Delete Ontology' dialog					
	function _drawDeleteOntologyDialog () {
	
		self.deleteOntologyDialog = new YAHOO.widget.Dialog("delete-ontology-dialog", 
									{
										width : "30em",
							  			fixedcenter : true,
							  			visible : false, 
							  			constraintoviewport : true,
										modal:true,
							  			buttons : [ 
											{
												text:"Delete", 
												handler:_handleDelete, 
												isDefault:true 
											},
								      		{
												text:"Cancel", 
												handler:_handleCancel 
											}
										]	  
									}
								);
							
		self.deleteOntologyDialog.validate = function () {
			var data = this.getData();
			
			if (data.ontologyname == ""){
				alert('Please enter an ontology name');
				return false;
			}
			else if(data.ontologyname.search(/\s/) > -1) {
				alert ('No whitespace allowed in the ontology name');
				return false;
			}
			else {
				return true;
			}
//Need to check if there are other validation criteria that need to be implemented			
		}		
		self.deleteOntologyDialog.beforeShowEvent.subscribe(_getOntologyName, self.deleteOntologyDialog, true);			
		self.deleteOntologyDialog.render();
		
		function _getOntologyName(){
		
			var currentOntology;
			
			if (typeof(editor.selectedOntology) == 'undefined') {
				alert('Please select an ontology first');
				this.hide();
				editor.ontologySelector.selectorPanel.show();
				
			}
			else {
				currentOntology = editor.selectedOntology;
				document.forms['delete-ontology-dialog-form'].elements['ontologyname'].value = currentOntology;
			}	
		}			

		function _handleDelete () {
			
			if (this.validate()) {
			
				var data = this.getData(); //The assumption is that 'this' resolves to the current Dialog instance
									//"...the scope of [the function assigned to the 'handler' property of the dialog buttons] is always its Dialog instance..."
									//(See http://developer.yahoo.com/yui/container/dialog/index.html#button)
				
				//Tips for dealing with SOAP taken from
				//http://www.zachleat.com/web/2007/05/09/wash-your-mouth-out-with-soap-and-the-yui-connection-manager/
				var postData = 
				'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' + 
				'xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' + 
				'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
					'<SOAP-ENV:Header/>' +
					'<SOAP-ENV:Body>' +
						'<DELETE-ONTOLOGY>' +
							'<USER-NAME type="string">wsmo</USER-NAME>' +
							'<USER-PASSWORD type="string">wsmo</USER-PASSWORD>' +
							'<ONTOLOGY-NAME type="sexpr">' +
								data.ontologyname +
							'</ONTOLOGY-NAME>' +
						'</DELETE-ONTOLOGY>' +
					'</SOAP-ENV:Body>' +
				'</SOAP-ENV:Envelope>'
				
				
				var callback = {
					success: _handleDeleteSuccess,
					failure: _handleDeleteFailure
				}
				var targetURL = '/soap';
				
				
				// we need to set our own header.
				YAHOO.util.Connect._use_default_post_header = false;
				YAHOO.util.Connect.initHeader('Content-Type', 'text/xml', false);
				
				var postCall = YAHOO.util.Connect.asyncRequest('POST', targetURL, callback, postData)
				
				this.hide();//Close the dialog box
			}
		}
		
		function _handleDeleteSuccess (responseObj) {
			
			
			var root = responseObj.responseXML.documentElement;
			var result = root.getElementsByTagName ('RESULT');
			
						
			if (result.hasOwnProperty(0)){//A <RESULT> tag was included in the SOAP message
				//yuiAlert(result[0].firstChild.nodeValue);
				yuiAlert('Success: Ontology has been deleted!');
				
				//Refresh the Ontology List held by the editor
				editor.ontologySelector.updateOntologySelectorMenu();
//[@TODO014] If the deleted ontology is the current one in view, then will have to clear all classes and relations from view				
				editor.renderer.activeWindow.panel.hide();
			}
			else {//Something didn't happen right on the server side
				yuiAlert(responseObj.responseText); //So print complete SOAP error message from server
				
//Need to add code for processing different types of ERROR and WARNING messages from server
		
			}
			
			
//Eventually, the success handler needs to update the ontology list (i.e. providing at the server-end an ontology has actually been added)
		}
		
		function _handleDeleteFailure (responseObj) {
			yuiAlert(responseObj.status);
		}
		
		function _handleCancel () {
			this.cancel();	//The assumption is that 'this' resolves to the current Dialog instance
							//By default the Dialog is automatically hidden after cancel
		}		
		
	}//_drawDeleteOntologyDialog
	
	this.showDeleteOntologyDialog = function () {
		if (typeof(editor.selectedOntology) == 'undefined') {
			yuiAlert('Please select an ontology first');
			editor.ontologySelector.selectorPanel.show();
		}
		else {
			self.deleteOntologyDialog.show();
			self.deleteOntologyDialog.bringToTop();
		}
	}
//}}} END OF 'Delete Ontology' dialog
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//{{{ START OF 'Run Query' dialog					
	function _drawRunKnowledgeBaseQueryDialog () {
	
		self.runKnowledgeBaseQueryDialog = new YAHOO.widget.Dialog("run-query-dialog", 
									{
										width : "30em",
							  			fixedcenter : true,
							  			visible : false, 
							  			constraintoviewport : true,
										modal:true,
							  			buttons : [ 
											{
												text:"Submit", 
												handler:_handleSubmit, 
												isDefault:true 
											},
								      		{
												text:"Cancel", 
												handler:_handleCancel 
											}
										]	  
									}
								);
							
		self.runKnowledgeBaseQueryDialog.validate = function () {
			//Some validation of the query here	
			return true;	
		}		
		
		self.runKnowledgeBaseQueryDialog.beforeShowEvent.subscribe(_getOntologyName, self.deleteOntologyDialog, true);			
		self.runKnowledgeBaseQueryDialog.render();
		
		function _getOntologyName(){
		
			var currentOntology;
			
			if (typeof(editor.selectedOntology) == 'undefined') {
				yuiAlert('Please select an ontology first');
				this.hide();
				editor.ontologySelector.selectorPanel.show();
				
			}
			else {
				currentOntology = editor.selectedOntology;
				document.forms['run-query-dialog-form'].elements['ontologyname'].value = currentOntology;
			}	
		}			

		function _handleSubmit () {
			
			if (this.validate()) {
			
				var data = this.getData(); //The assumption is that 'this' resolves to the current Dialog instance
									//"...the scope of [the function assigned to the 'handler' property of the dialog buttons] is always its Dialog instance..."
									//(See http://developer.yahoo.com/yui/container/dialog/index.html#button)
				
				//Need to use irs.ocmlQuery(callback, ontology, query)				
				
			}
		}
		
		function _handleSubmitCallback (responseObj) {
			yuiAlert(responseObj);
		}
		
		
		function _handleCancel () {
			this.cancel();	//The assumption is that 'this' resolves to the current Dialog instance
							//By default the Dialog is automatically hidden after cancel
		}		
		
	}//_drawRunKnowledgeBaseQueryDialog
	
	this.showRunKnowledgeBaseQueryDialog = function () {
		if (typeof(editor.selectedOntology) == 'undefined') {
			yuiAlert('Please select an ontology first');
			editor.ontologySelector.selectorPanel.show();
		}
		else {
			self.runKnowledgeBaseQueryDialog.show();
			self.runKnowledgeBaseQueryDialog.bringToTop();
		}
	}
//}}} END OF 'Run Query' dialog
///////////////////////////////////////////////////////
	function _drawHelpAboutDialog() { 
		
		
	
	}

	this.showHelpAboutDialog = function () {
		
		var aboutText = 
			'<p>IRSBrowser version 4(alpha) - Released 08/10/2008</p>' +
			'<p>(c) Copyright <a href = "http://www.open.ac.uk" target = "_blank">The Open University</a> 2008. All rights reserved</p>';
		
		var aboutDialog = 	new YAHOO.widget.SimpleDialog("helpaboutdialog", 
					{ 
						width: "300px",
		   				fixedcenter: true,
		   				visible: true,
		   				draggable: false,
		   				close: true,
						modal:true,
		   				text: aboutText,
		   				icon: null,
		   				constraintoviewport: true,
		   				buttons: [ 
							{ 
								text:"OK",
								handler: {
									fn: function() {
										this.hide();
									}
								},
								isDefault:true
							}
						]
		 			}
				);
		aboutDialog.setHeader('About');
		aboutDialog.render(document.body);		

	}

/////////////////////////////////////////////////////////////////

	function _generateAllDialogHTML() {
	//We define the various dialog boxes here and the YUI takes care of hiding the HTML	
		_generateNewOntologyDialogHTML();
		_generateEditOntologyDialogHTML();
		_generateDeleteOntologyDialogHTML();
		_generateNewGoalDialogHTML();
		_generateNewWebServiceDescriptionDialogHTML();
		_generateNewMediatorDescriptionDialogHTML();
		_generateEditGoalDescriptionDialogHTML();
		_generateEditWebServiceDescriptionDialogHTML();
		_generateEditMediatorDescriptionDialogHTML();
	}

	function _generateNewOntologyDialogHTML() {	
			
		var newOntologyDialogDIV = document.createElement('div')
		newOntologyDialogDIV.setAttribute('id', "new-ontology-dialog");
		newOntologyDialogDIV.innerHTML = 
		
			'<div class="hd">New Ontology</div>' +
			'<div class="bd">' +
				'<form name="new-ontology-dialog-form">' +
				'<div style="display: table">' +	
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<label for="ontologytype">Ontology Type:</label>' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<select name="ontologytype">' +
								'<option value="domain">Domain</option>' +
								'<option value="goal">Goal</option>' +
								'<option value="webservice">Web Service</option>' +
								'<option value="mediator">Mediator</option>' +
								'<option value="application">Application</option>' +
							'</select>'  +
						'</div>' +
					'</div>' +			
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<label for="ontologyname">Ontology Name:</label>' +
						'</div>' +	
						'<div style="display: table-cell">'	 +
							'<input type="textbox" name="ontologyname" />' +
						'</div>' +
					'</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell">'	 +		
							'<label for="ontologyparents">Parent(s):</label>' +
						'</div>' +	
						'<div style="display: table-cell">' +	
							'<input type="textbox" name="ontologyparents" />' +
						'</div>' +	
					'</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<label for="allowededitors">Allowed Editors:</label>' +
						'</div>' +	
						'<div style="display: table-cell">' +	
							'<input type="textbox" name="allowededitors" />' +
						'</div>' +
					'</div>' +		
			 
//<!-- [@TODO001] Need to add hidden fields for username and password which will be set to the user login details for the current editor session.  (Sort out when user-login is eventually added as a feature) 
//			<label for="author">Author:</label><input type="textbox" name="author" value="wsmo" /> -->
			
				'</div>' +
				'</form>' +
			'</div>';
		
		document.body.appendChild(newOntologyDialogDIV);
		
	}
	
	function _generateEditOntologyDialogHTML() {
		var editOntologyDialogDIV = document.createElement('div')
		editOntologyDialogDIV.setAttribute('id', "edit-ontology-dialog");
		editOntologyDialogDIV.innerHTML = 
			
			'<div class="hd">Edit Ontology</div>' +
			'<div class="bd">' +
				'<form name="edit-ontology-dialog-form">' +
				'<div style="display: table">' +	
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<label for="ontologytype">Ontology Type:</label>' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<select name="ontologytype">' +
								'<option value="domain">Domain</option>' +
								'<option value="goal">Goal</option>' +
										'<option value="webservice">Web Service</option>' +
								'<option value="mediator">Mediator</option>' +
								'<option value="application">Application</option>' +
							'</select>' + 
						'</div>' +
					'</div>	' +		
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<label for="ontologyname">Ontology Name:</label>' +
						'</div>	' +
						'<div style="display: table-cell">	' +
							'<input type="textbox" name="ontologyname" />' +
							'<input type="hidden" name="oldname" />' +
						'</div>' +
					'</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +	
							'<label for="ontologyparents">Parent(s):</label>' +
						'</div>	' +
						'<div style="display: table-cell">' +	
							'<input type="textbox" name="ontologyparents" />' +
						'</div>	' +
					'</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<label for="author">Author:</label>' +
						'</div>	' +
						'<div style="display: table-cell">' +	
							'<input type="textbox" name="author" />' +
						'</div>' +
					'</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<label for="allowededitors">Allowed Editors:</label>' +
						'</div>	' +
						'<div style="display: table-cell">	' +
							'<input type="textbox" name="allowededitors" />' +
						'</div>' +
					'</div>' +
			
				'</div>' +
				'</form>' +
			'</div>';
		
		document.body.appendChild(editOntologyDialogDIV);

	}
	
	function _generateDeleteOntologyDialogHTML() {
		var deleteOntologyDialogDIV = document.createElement('div');
		deleteOntologyDialogDIV.setAttribute('id', "delete-ontology-dialog");
		deleteOntologyDialogDIV.innerHTML = 
		
			'<div class="hd">Delete Ontology</div>' +
			'<div class="bd">' +
				'<form name="delete-ontology-dialog-form">' +
				'<div style="display: table">' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<label for="ontologyname">Ontology Name:</label>' +
						'</div>' +	
						'<div style="display: table-cell">' +	
							'<input type="textbox" name="ontologyname" />' +
						'</div>' +
					'</div>	' +				
			 
//<!-- [@TODO002] Need to add hidden fields for username and password which will be set to the user login details for the current editor session.  (Sort out when user-login is eventually added as a feature) 
//			<label for="author">Author:</label><input type="textbox" name="author" value="wsmo" /> -->
			
				'</div>' +
				'</form>' +
			'</div>';
		
		document.body.appendChild(deleteOntologyDialogDIV);
	}
	
	function _generateNewGoalDialogHTML() {
		var newGoalDialogDIV = document.createElement('div')
		newGoalDialogDIV.setAttribute('id', "new-goal-dialog");
		newGoalDialogDIV.innerHTML = 
		
			'<div class="hd">New Goal</div>' +
			'<div class="bd">' +
				'<form name="new-goal-dialog-form">' +
				'<div style="display: table; width: 100%">' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="goalname">Name:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<input type="textbox" name="goalname" />' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="ontologyname">Ontology:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<input type="textbox" name="ontologyname" readonly ="readonly"/>' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="goalparent">Parent:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<input type="textbox" name="goalparent" value="goal"/>' +
						'</div>' +
					'</div>' +
				'</div>' +
				'<div style="display: table; width: 100%">' +
					'<div style="display: table-row">Inputs:</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<div id="new-goal-input-role-table"></div>' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<div id="new-goal-input-role-table-buttons"></div>	' +				
						'</div>	' +	
					'</div>' +
				'</div>' +
				'<div style="display: table; width: 100%">	' +				
					'<div style="display: table-row">Output:</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<label for="outputrolename">Name:</label>' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<input type="textbox" name="outputrolename" />' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<label for="outputroletype">Type:</label>' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<input type="textbox" name="outputroletype" />' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<label for="outputsoaptype">SOAP Type:</label>' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<select name="outputsoaptype">' +
								'<option value=""></option>' +
								'<option value="string">string</option>' +
								'<option value="int">int</option>' +
								'<option value="float">float</option>' +
								'<option value="sexpr">sexpr</option>' +
								'<option value="xml">xml</option>' +
							'</select>' +
						'</div>' +
					'</div>' +	
				'</div>' +
				'</form>' +
			'</div>';
		
		document.body.appendChild(newGoalDialogDIV);
	}
	
	function _generateNewWebServiceDescriptionDialogHTML () {
		var newWebServiceDescriptionDialogDIV = document.createElement('div')
		newWebServiceDescriptionDialogDIV.setAttribute('id', "new-web-service-description-dialog");
		newWebServiceDescriptionDialogDIV.innerHTML = 
		
			'<div class="hd">New Web Service Description</div>' +
			'<div class="bd">' +
				'<form name="new-web-service-description-dialog-form">' +
				'<div style="display: table; width: 100%">' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="webservicename">Name:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<input type="textbox" name="webservicename" />' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="ontologyname">Ontology:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<input type="textbox" name="ontologyname" readonly ="readonly"/>' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="webserviceparent">Parent:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<input type="textbox" name="webserviceparent" value="web-service"/>' +
						'</div>' +
					'</div>' +
				'</div>' +
				
				'<hr/>' +

				'<div class = "yui-navset" id = "capability-interface-tabs">' +
					'<ul class = "yui-nav">' +
						'<li><a href = "#capability"><em>Capability</em></a></li>' +
						'<li><a href = "#interface"><em>Interface</em></a></li>' +
					'</ul>' +
					'<div class ="yui-content">' +
						'<div id ="capability" style="height:25em">' +
							'<div style="display: table;">' +				
								'<div style="display: table-row">' +								
									'<div style="display: table-cell; width: 10%">' +
										'<label for="capabilityname">Name:</label>' +
									'</div>' +	
									'<div style="display: table-cell; width: 90%">' +	
										'<input type="textbox" name="capabilityname"/>' +
									'</div>' +
								'</div>' +				
								'<div style="display: table-row">' +
									'<div style="display: table-cell; width: 10%">' +
										'<label for="capabilitymediator">Mediator:</label>' +
									'</div>' +	
									'<div style="display: table-cell; width: 90%">' +	
										'<input type="textbox" name="capabilitymediator"/>' +
									'</div>' +
								'</div>' +
							'</div>' +
							'<hr/>' +		
							'<label for ="precondition">Pre-condition:</label><br/>' +
							'<textarea name="precondition" rows="2"></textarea><br/>' +							
							'<label for ="assumption">Assumption:</label><br/>' +
							'<textarea name="assumption" rows="2"></textarea><br/>' +
							'<label for ="postcondition">Post-condition:</label><br/>' +
							'<textarea name="postcondition" rows="2"></textarea><br/>' +
							'<label for ="effect">Effect:</label><br/>' +
							'<textarea name="effect" rows="2"></textarea><br/>' +
						'</div>' +
						'<div id ="interface" style="height:25em">' +
							'<div style="display: table-cell; width: 10%">' +
								'<label for="interfacename">Name:</label>' +
							'</div>' +	
							'<div style="display: table-cell; width: 90%">' +	
								'<input type="textbox" name="interfacename"/>' +
							'</div>' +
							'<hr/>' +
							'<label>Choreography</label><br/><br/>' +
							'<table>' +				
								'<tr>' +
									'<td rowspan = "3" style="width: 20%; vertical-align: top">' +
										'<label for ="grounding">Grounding:</label>' +
									'</td>' +
									'<td rowspan = "3" style="width: 40%">' +	
										'<textarea name="grounding" rows="3"></textarea>' +							
									'</td>' +
									'<td>' +
										'<label for ="host">Host:</label>' +
									'</td>' +
									'<td>' +
										'<input type="textbox" name="host"/>' +
									'</td>' +
								'</tr>' +
								'<tr>' +
									'<td>' +
										'<label for ="port">Port:</label>' +
									'</td>' +
									'<td>' +
										'<input type="textbox" name="port"/>' +
									'</td>' +
								'</tr>' +
								'<tr>' +
									'<td>' +
										'<label for ="path">Path:</label>' +
									'</td>' +
									'<td>' +
										'<input type="textbox" name="path"/>' +
									'</td>' +
								'</tr>' +
								'<tr>' +
									'<td style="width: 20%; vertical-align: top">' +
										'<label for ="guardedtransitions">Guarded Transitions:</label>' +
									'</td>' +
									'<td style="width: 40%">' +
										'<textarea name="guardedtransitions" rows="3"></textarea>' +
									'</td>' +
								'</tr>' +
							'</table>' +	
							'<hr />' +
							'<label for ="orchestration">Orchestration:</label><br/>' +
							'<textarea name="orchestration" rows="3"></textarea>' +						
						'</div>' +
					'</div>' +
				'</div>' +				

				'</form>' +
			'</div>';
		
		document.body.appendChild(newWebServiceDescriptionDialogDIV);		
	}
	
	function _generateNewMediatorDescriptionDialogHTML () {
		var newMediatorDescriptionDialogDIV = document.createElement('div')
		newMediatorDescriptionDialogDIV.setAttribute('id', "new-mediator-description-dialog");
		newMediatorDescriptionDialogDIV.innerHTML = 
		
			'<div class="hd">New Mediator Description</div>' +
			'<div class="bd">' +
				'<form name="new-mediator-description-dialog-form">' +
				'<div style="display: table; width: 100%">' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="mediatorname">Name:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 80%">' +	
							'<input type="textbox" name="mediatorname" />' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="ontologyname">Ontology:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 80%">' +	
							'<input type="textbox" name="ontologyname" readonly ="readonly"/>' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="mediatorparent">Parent:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 80%">' +	
							'<input type="textbox" name="mediatorparent" value="mediator"/>' +
						'</div>' +
					'</div>' +			
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="sourcecomponent">Source Component:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 80%">' +	
							'<input type="textbox" name="sourcecomponent"/>' +
						'</div>' +
					'</div>' +			
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="targetcomponent">Target Component:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 80%">' +	
							'<input type="textbox" name="targetcomponent"/>' +
						'</div>' +
					'</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="mediatortype">Mediator Type:</label>' +
						'</div>' +
						'<div style="display: table-cell; width: 80%">' +
							'<select name="mediatortype">' +
								'<option value="wg-mediator">wg-mediator</option>' +
								'<option value="oo-mediator">oo-mediator</option>' +
								'<option value="gg-mediator">gg-mediator</option>' +
								'<option value="ww-mediator">ww-mediator</option>' +
								'<option value="mediator">mediator</option>' +
							'</select>' +
						'</div>' +
				'</div>' +											

				'</form>' +
			'</div>';
		
		document.body.appendChild(newMediatorDescriptionDialogDIV);		
	}
	
	function _generateEditGoalDescriptionDialogHTML() {
		var editGoalDescriptionDialogDIV = document.createElement('div')
		editGoalDescriptionDialogDIV.setAttribute('id', "edit-goal-description-dialog");
		editGoalDescriptionDialogDIV.innerHTML = 
		
			'<div class="hd">Edit Goal Description</div>' +
			'<div class="bd">' +
				'<form name="edit-goal-description-dialog-form">' +
				'<div style="display: table; width: 100%">' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="goalname">Name:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<select name="goalname">' +
								'<option value="" selected="selected"></option>' +
							'</select>' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="ontologyname">Ontology:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<input type="textbox" name="ontologyname" readonly ="readonly"/>' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="goalparent">Parent:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<input type="textbox" name="goalparent" value="goal"/>' +
						'</div>' +
					'</div>' +
				'</div>' +
				'<div style="display: table; width: 100%">' +
					'<div style="display: table-row">Inputs:</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<div id="edit-goal-input-role-table"></div>' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<div id="edit-goal-input-role-table-buttons"></div>	' +				
						'</div>	' +	
					'</div>' +
				'</div>' +
				'<div style="display: table; width: 100%">	' +				
					'<div style="display: table-row">Output:</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell">' +
							'<label for="outputrolename">Name:</label>' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<input type="textbox" name="outputrolename" />' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<label for="outputroletype">Type:</label>' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<input type="textbox" name="outputroletype" />' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<label for="outputsoaptype">SOAP Type:</label>' +
						'</div>' +
						'<div style="display: table-cell">' +
							'<select name="outputsoaptype">' +
								'<option value=""></option>' +
								'<option value="string">string</option>' +
								'<option value="int">int</option>' +
								'<option value="float">float</option>' +
								'<option value="sexpr">sexpr</option>' +
								'<option value="xml">xml</option>' +
							'</select>' +
						'</div>' +
					'</div>' +	
				'</div>' +
				'</form>' +
			'</div>';
		
		document.body.appendChild(editGoalDescriptionDialogDIV);
	}
	
	function _generateEditWebServiceDescriptionDialogHTML() {
		var editWebServiceDescriptionDialogDIV = document.createElement('div')
		editWebServiceDescriptionDialogDIV.setAttribute('id', "edit-web-service-description-dialog");
		editWebServiceDescriptionDialogDIV.innerHTML = 
		
			'<div class="hd">Edit Web Service Description</div>' +
			'<div class="bd">' +
				'<form name="edit-web-service-description-dialog-form">' +
				'<div style="display: table; width: 100%">' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="webservicename">Name:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">'  +	
							'<select name="webservicename">' +
								'<option value="" selected="selected"></option>' +
							'</select>' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="ontologyname">Ontology:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<input type="textbox" name="ontologyname" readonly ="readonly"/>' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 10%">' +
							'<label for="webserviceparent">Parent:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 90%">' +	
							'<input type="textbox" name="webserviceparent" value="web-service"/>' +
						'</div>' +
					'</div>' +
				'</div>' +
				
				'<hr/>' +

				'<div class = "yui-navset" id = "capability-interface-tabs-edit-form">' +
					'<ul class = "yui-nav">' +
						'<li><a href = "#capability-edit-form"><em>Capability</em></a></li>' +
						'<li><a href = "#interface-edit-form"><em>Interface</em></a></li>' +
					'</ul>' +
					'<div class ="yui-content">' +
						'<div id ="capability-edit-form" style="height:25em">' +
							'<div style="display: table;">' +				
								'<div style="display: table-row">' +								
									'<div style="display: table-cell; width: 10%">' +
										'<label for="capabilityname">Name:</label>' +
									'</div>' +	
									'<div style="display: table-cell; width: 90%">' +	
										'<input type="textbox" name="capabilityname"/>' +
									'</div>' +
								'</div>' +				
								'<div style="display: table-row">' +
									'<div style="display: table-cell; width: 10%">' +
										'<label for="capabilitymediator">Mediator:</label>' +
									'</div>' +	
									'<div style="display: table-cell; width: 90%">' +	
										'<input type="textbox" name="capabilitymediator"/>' +
									'</div>' +
								'</div>' +
							'</div>' +	
							'<hr/>' +	
							'<label for ="precondition">Pre-condition:</label><br/>' +
							'<textarea name="precondition" rows="2"></textarea><br/>' +							
							'<label for ="assumption">Assumption:</label><br/>' +
							'<textarea name="assumption" rows="2"></textarea><br/>' +
							'<label for ="postcondition">Post-condition:</label><br/>' +
							'<textarea name="postcondition" rows="2"></textarea><br/>' +
							'<label for ="effect">Effect:</label><br/>' +
							'<textarea name="effect" rows="2"></textarea><br/>' +
						'</div>' +
						'<div id ="interface-edit-form" style="height:25em">' +
							'<div style="display: table-cell; width: 10%">' +
								'<label for="interfacename">Name:</label>' +
							'</div>' +	
							'<div style="display: table-cell; width: 90%">' +	
								'<input type="textbox" name="interfacename"/>' +
							'</div>' +	
							'<hr/>' +
							'<label>Choreography</label><br/><br/>' +
							'<table>' +				
								'<tr>' +
									'<td rowspan = "3" style="width: 20%; vertical-align: top">' +
										'<label for ="grounding">Grounding:</label>' +
									'</td>' +
									'<td rowspan = "3" style="width: 40%">' +	
										'<textarea name="grounding" rows="3"></textarea>' +							
									'</td>' +
									'<td>' +
										'<label for ="host">Host:</label>' +
									'</td>' +
									'<td>' +
										'<input type="textbox" name="host"/>' +
									'</td>' +
								'</tr>' +
								'<tr>' +
									'<td>' +
										'<label for ="port">Port:</label>' +
									'</td>' +
									'<td>' +
										'<input type="textbox" name="port"/>' +
									'</td>' +
								'</tr>' +
								'<tr>' +
									'<td>' +
										'<label for ="path">Path:</label>' +
									'</td>' +
									'<td>' +
										'<input type="textbox" name="path"/>' +
									'</td>' +
								'</tr>' +
								'<tr>' +
									'<td style="width: 20%; vertical-align: top">' +
										'<label for ="guardedtransitions">Guarded Transitions:</label>' +
									'</td>' +
									'<td style="width: 40%">' +
										'<textarea name="guardedtransitions" rows="3"></textarea>' +
									'</td>' +
								'</tr>' +
							'</table>' +	
							'<hr />' +
							'<label for ="orchestration">Orchestration:</label><br/>' +
							'<textarea name="orchestration" rows="3" cols="80"></textarea>' +						
						'</div>' +
					'</div>' +
				'</div>' +				

				'</form>' +
			'</div>';
		
		document.body.appendChild(editWebServiceDescriptionDialogDIV);		
	}
	
	function _generateEditMediatorDescriptionDialogHTML() {
		var editMediatorDescriptionDialogDIV = document.createElement('div')
		editMediatorDescriptionDialogDIV.setAttribute('id', "edit-mediator-description-dialog");
		editMediatorDescriptionDialogDIV.innerHTML = 
		
			'<div class="hd">New Mediator Description</div>' +
			'<div class="bd">' +
				'<form name="edit-mediator-description-dialog-form">' +
				'<div style="display: table; width: 100%">' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="mediatorname">Name:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 80%">' +	
							'<select name="mediatorname">' +
								'<option value="" selected="selected"></option>' +
							'</select>' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="ontologyname">Ontology:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 80%">' +	
							'<input type="textbox" name="ontologyname" readonly ="readonly"/>' +
						'</div>' +
					'</div>' +				
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="mediatorparent">Parent:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 80%">' +	
							'<input type="textbox" name="mediatorparent" value="mediator"/>' +
						'</div>' +
					'</div>' +			
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="sourcecomponent">Source Component:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 80%">' +	
							'<input type="textbox" name="sourcecomponent"/>' +
						'</div>' +
					'</div>' +			
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="targetcomponent">Target Component:</label>' +
						'</div>' +	
						'<div style="display: table-cell; width: 80%">' +	
							'<input type="textbox" name="targetcomponent"/>' +
						'</div>' +
					'</div>' +
					'<div style="display: table-row">' +
						'<div style="display: table-cell; width: 20%">' +
							'<label for="mediatortype">Mediator Type:</label>' +
						'</div>' +
						'<div style="display: table-cell; width: 80%">' +
							'<select name="mediatortype">' +
								'<option value="wg-mediator">wg-mediator</option>' +
								'<option value="oo-mediator">oo-mediator</option>' +
								'<option value="gg-mediator">gg-mediator</option>' +
								'<option value="ww-mediator">ww-mediator</option>' +
								'<option value="mediator">mediator</option>' +
							'</select>' +
						'</div>' +
				'</div>' +											

				'</form>' +
			'</div>';
		
		document.body.appendChild(editMediatorDescriptionDialogDIV);
	}

}
