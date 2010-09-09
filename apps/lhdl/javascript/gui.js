var lastReturnValue = "";

var textFieldWidth = 40;

var rollerImage = irs.irsUri("/irs/assets/images/busy-roller.gif");

function initGUI() {
    document.getElementById("ops-panel").innerHTML = "<table></table>";
    drawMenuBar();
}

// {{{ extract ISO surface
function drawExtractIsoSurface () {
    document.getElementById("ops-panel").innerHTML =
	"<form id='input-panel'><table>"
    // text field for url.  XXX should be a selector, on data from
    // another service
	+ "<tr><td>URL</td><td id='urlSpace'><input type='text' name='url' value='http://lhdl.cis.beds.ac.uk/storage/head.zmsf' size='50'/></td></tr>"
    // Contour slider
    // Slider is disabled at the moment becuse I can't get it to work.
// 	+ "<style type='text/css'>
//     #slider-bg { \
//         position: relative; \
//         background:url(/lhdl/www/slider-bg.png) 5px 0 no-repeat; \
//         height:28px; \
//         width:228px;  \
//     } \
//  \
//     #sliderthumb { \
//         cursor:default; \
//         position: absolute; \
//         top: 4px; \
//     } \
// </style>"

// 	+ "<tr><td>Contour</td><td><div id='slider-bg' tabindex='-1'> \
// <div id='sliderthumb'><img src='/lhdl/www/slider-thumb.png'/></div></div>\
// </td><td><span id='slider-value'>0</span></td></tr>"

    + "<tr><td>Contour</td><td><input type='text' name='contour' value='1.0' size='5'/></td></tr>"
	+ "<tr><td>AutoLod</td><td id='autolodButtonSpace'></td></tr>"
	+ "<tr><td>Optimise</td><td id='optimiseButtonSpace'></td></tr>"
	+ "<tr><td></td><td id='executeButtonSpace'></td></tr>"
	+ "</form></table>";

    // auto-lod, a checkbox
    var autolodButton = new YAHOO.widget.Button({
        id: "autolodButton", type: "checkbox",
	checked: true, container: "autolodButtonSpace"});
    // optimise, a checkbox
    var optimiseButton = new YAHOO.widget.Button({
        id: "optimiseButton", type: "checkbox",
	checked: true, container: "optimiseButtonSpace"});
    // Contour slider
//XXX see above
//     var slider = YAHOO.widget.Slider.getHorizSlider("slider-bg", "sliderthumb", 0, 200);
//     slider.setValue(100);

    // execute button
    var executeButton = new YAHOO.widget.Button({
	label:"extractIsoSurface", container:"executeButtonSpace",
	onclick: {fn: doExtractIsoSurface2(autolodButton, optimiseButton)}});
}

// Default to the example URL.
var extractIsoSurfaceResult = "http://lhdl.cis.beds.ac.uk/storage/head_Isosurface_1.zmsf";

function doExtractIsoSurface2(autolod, optimise) {
    return function (x) {
	setBusy(true);
	form = document.getElementById("input-panel");
	irs.achieveGoal(function (val) {
	    extractIsoSurfaceResult = val;
	    setBusy(false);
	    notifyReturnValue(val);
	},
			"LHDL-APPLICATION", "EXTRACT-ISO-SURFACE-GOAL",
			[{name: "HAS-URL", value: form.url.value},
			 {name: "HAS-CONTOUR", value: form.contour.value},
			 {name: "HAS-AUTO-LOD", value: jsToGoalArg(autolod.get("checked"))},
			 {name: "HAS-OPTIMISE", value: jsToGoalArg(optimise.get("checked"))},
			 {name: "goals:hasAccount", value: form.btaccount.value}]);
    }
}
// }}}
// {{{ render surface
function drawRenderSurface () {
    document.getElementById("ops-panel").innerHTML =
 	"<form id='input-panel'><table>"
	+ "<tr><td>BT account</td><td><input type='text' name='btaccount' value='#_defaultBiomedTownAccount' size='" + textFieldWidth + "'/></td></tr>"
 	+ "<tr><td>URL</td><td><input type='text' name='url' value='"
	+ lastReturnValue
	+ "' size='"+ textFieldWidth +"'/></td></tr>"
 	+ "<tr><td>XML</td><td><input type='text' name='xml' value='" + lastReturnValue	+ ".xml' size='"+ textFieldWidth +"'/></td></tr>"
	+ "<tr><td>Background colour</td><td><input type='text' name='bgColour' value='0/0/0' size='11'/></td></tr>"
	+ "<tr><td>Zoom factor</td><td><input id='zoomFactor' value='1.0'/></td></tr>"
	+ "<tr><td>Camera azimuth</td><td><input id='cameraAzimuth' value='90'/></td></tr>"
	+ "<tr><td>Camera roll</td><td><input id='cameraRoll' value='270'/></td></tr>"
	+ "<tr><td></td><td id='executeButtonSpace'></td></tr>"
 	+ "</form></table>";
    // execute button
    var executeButton = new YAHOO.widget.Button({
	label:"Execute", container:"executeButtonSpace",
	onclick: {fn: doRenderSurface2()}});
}

function doRenderSurface2() {
    return function (x) {
	form = document.getElementById("input-panel");
	setBusy(true);
	irs.achieveGoal(function (val) {
	    setRenderSurfaceResult(val);
	    setBusy(false);
	    notifyReturnValue(val);
	    showImage();
	},
			"LHDL-APPLICATION", "RENDER-SURFACE-GOAL",
			[{name: "HAS-FILE-URL", value: form.url.value},
			 {name: "HAS-XML", value: form.xml.value},
			 {name: "HAS-BACKGROUND-COLOUR", value: form.bgColour.value},
			 {name: "HAS-ZOOM-FACTOR", value: form.zoomFactor.value},
			 {name: "HAS-CAMERA-AZIMUTH", value: form.cameraAzimuth.value},
			 {name: "HAS-CAMERA-ROLL", value: form.cameraRoll.value},
			 {name: "HAS-IMAGE-WIDTH", value: "400"},
			 {name: "HAS-IMAGE-HEIGHT", value: "400"},
			 {name: "goals:hasAccount", value: form.btaccount.value}]);
    };
}
// }}}

// {{{ Import operations
function drawImportVtk () {
    document.getElementById("ops-panel").innerHTML =
 	"<form id='input-panel'><table>"
	+ "<tr><td>BT account</td><td><input type='text' name='btaccount' value='#_defaultBiomedTownAccount' /></td></tr>"
 	+ "<tr><td>File</td><td id='urlSpace'><input type='text' name='url' value='"
    // XXX Hard code this at the moment
	+ "sample/head.vtk" + "' size='50'/></td></tr>"
	+ "<tr><td>Compress</td><td id='compressButtonSpace'></td></tr>"
	+ "<tr><td></td><td id='executeButtonSpace'></td></tr>"
 	+ "</form></table>";

    var compressButton = new YAHOO.widget.Button({
        id: "compressButton", type: "checkbox",
	checked: true, container: "compressButtonSpace"});

    // execute button
    var executeButton = new YAHOO.widget.Button({
	label:"Execute", container:"executeButtonSpace",
	onclick: {fn: doImportVtk(compressButton)}});
}

var importVtkResult;
function doImportVtk(compress) {
    return function (x) {
	form = document.getElementById("input-panel");
	setBusy(true);
	irs.achieveGoal(function (val) {
		importVtkResult = val;
	    setBusy(false);
	    notifyReturnValue(val);
	},
			"LHDL-APPLICATION", "IMPORT-VTK-GOAL",
			[{name: "HAS-FILENAME", value: form.url.value},
			 {name: "HAS-COMPRESS", value: jsToGoalArg(compress.get("checked"))},
			 {name: "goals:hasAccount", value: form.btaccount.value}]);
    };
}

function drawImportStl () {
    document.getElementById("ops-panel").innerHTML =
 	"<form id='input-panel'><table>"
	+ "<tr><td>BT account</td><td><input type='text' name='btaccount' value='#_defaultBiomedTownAccount' /></td></tr>"
 	+ "<tr><td>File</td><td id='urlSpace'><input type='text' name='url' value='"
    // XXX Hard code this at the moment
	+ "sample/1091_Femur_Right.stl" + "' size='50'/></td></tr>"
	+ "<tr><td></td><td id='executeButtonSpace'></td></tr>"
 	+ "</form></table>";

    // execute button
    var executeButton = new YAHOO.widget.Button({
	label:"Execute", container:"executeButtonSpace",
	onclick: {fn: doImportStl()}});
}

var importStlResult;
function doImportStl() {
    return function (x) {
	form = document.getElementById("input-panel");
	setBusy(true);
	irs.achieveGoal(function (val) {
		importStlResult = val;
	    setBusy(false);
	    notifyReturnValue(val);
	},
			"LHDL-APPLICATION", "IMPORT-STL-GOAL",
			[{name: "HAS-FILENAME", value: form.url.value},
			 {name: "goals:hasAccount", value: form.btaccount.value}]);
    };
}

function drawImportVrml () {
    document.getElementById("ops-panel").innerHTML =
 	"<form id='input-panel'><table>"
	+ "<tr><td>BT account</td><td><input type='text' name='btaccount' value='#_defaultBiomedTownAccount' /></td></tr>"
 	+ "<tr><td>File</td><td id='urlSpace'><input type='text' name='url' value='"
    // XXX Hard code this at the moment
	+ "sample/fran.wrl" + "' size='50'/></td></tr>"
	+ "<tr><td></td><td id='executeButtonSpace'></td></tr>"
 	+ "</form></table>";

    // execute button
    var executeButton = new YAHOO.widget.Button({
	label:"Execute", container:"executeButtonSpace",
	onclick: {fn: doImportVrml()}});
}

var importVrmlResult;
function doImportVrml() {
    return function (x) {
	form = document.getElementById("input-panel");
	setBusy(true);
	irs.achieveGoal(function (val) {
		importVrmlResult = val;
	    setBusy(false);
	    notifyReturnValue(val);
	},
			"LHDL-APPLICATION", "IMPORT-VRML-GOAL",
			[{name: "HAS-FILENAME", value: form.url.value},
			 {name: "goals:hasAccount", value: form.btaccount.value}]);
    };
}

// }}}

// {{{ Export operations
function drawExportVtk () {
    document.getElementById("ops-panel").innerHTML =
 	"<form id='input-panel'><table>"
	+ "<tr><td>BT account</td><td><input type='text' name='btaccount' value='#_defaultBiomedTownAccount' /></td></tr>"
 	+ "<tr><td>URL</td><td id='urlSpace'><input type='text' name='url' value='"
	+ lastReturnValue + "' size='50'/></td></tr>"
 	+ "<tr><td>XML</td><td><input type='text' name='xml' value='" + lastReturnValue	+ ".xml' size='"+ textFieldWidth +"'/></td></tr>"
	+ "<tr><td>Absolute matrix</td><td id='absMatrixButtonSpace'></td></tr>"
	+ "<tr><td>VTK file format</td><td id='vtkFileFormatSpace'></td></tr>"
	+ "<tr><td></td><td id='executeButtonSpace'></td></tr>"
 	+ "</form></table>";

    var absMatrixButton = new YAHOO.widget.Button({
        id: "absMatrixButton", type: "checkbox",
	checked: true, container: "absMatrixButtonSpace"});

    var vtkFileFormatButtonGroup = new YAHOO.widget.ButtonGroup({
	    id: "absMatrixButtonGroup",
	    name: "is there a point to this name?",
	    container:  "absMatrixButtonSpace"});

    vtkFileFormatButtonGroup.addButtons([{ label: "ASCII", value: "ascii", checked: true },
					 { label: "Binary", value: "binary" }]);

    // execute button
    var executeButton = new YAHOO.widget.Button({
	label:"Execute", container:"executeButtonSpace",
	onclick: {fn: doExportVtk(absMatrixButton, vtkFileFormatButtonGroup)}});
}



var exportVtkResult;

function doExportVtk(absMatrix, vtkFileFormat) {
    return function (x) {
	form = document.getElementById("input-panel");
	setBusy(true);
	irs.achieveGoal(function (val) {
		importVtkResult = val;
	    setBusy(false);
	    notifyReturnValue(val);
	},
	    "LHDL-APPLICATION", "EXPORT-VTK-GOAL",
	    [{name: "HAS-URL", value: form.url.value},
	     {name: "HAS-ABS-MATRIX", value: jsToGoalArg(absMatrix.get("checked"))},
	     {name: "HAS-VTK-FILE-FORMAT",
	     value: vtkFileFormat.get("checkedButton").get("value")},
	     {name: "goals:hasAccount", value: form.btaccount.value}]);
    };
}
// }}}

// {{{ Decimate operation

function drawDecimateSurface () {
    document.getElementById("ops-panel").innerHTML =
 	"<form id='input-panel'><table>"
	+ "<tr><td>BT account</td><td><input type='text' name='btaccount' value='#_defaultBiomedTownAccount' /></td></tr>"
 	+ "<tr><td>File</td><td id='fileUrlSpace'><input type='text' name='fileUrl' value='"
 	+ lastReturnValue // "http://lhdl.cis.beds.ac.uk/storage/fran_cut.zmsf"
	+ "' size='50'/></td></tr>"
 	+ "<tr><td>XML</td><td><input type='text' name='xml' value='" + lastReturnValue	+ ".xml' size='"+ textFieldWidth +"'/></td></tr>"
	+ "<tr><td>Preserve Topology</td><td id='preserveTopologyButtonSpace'></td></tr>"
 	+ "<tr><td>Target reduction</td><td><input type='text' name='targetReduction' value='1'"
	+ "<tr><td></td><td id='executeButtonSpace'></td></tr>"
 	+ "</form></table>";
    var preserveTopologyButton = new YAHOO.widget.Button({
        id: "preserveTopologyButton", type: "checkbox",
	checked: true, container: "preserveTopologyButtonSpace"});
    // execute button
    var executeButton = new YAHOO.widget.Button({
	label:"Execute", container:"executeButtonSpace",
	onclick: {fn: doDecimateSurface(preserveTopologyButton)}
	});
}

var decimateSurfaceResult;
function doDecimateSurface(preserveTopologyButton) {
    return function (x) {
	form = document.getElementById("input-panel");
	setBusy(true);
	irs.achieveGoal(function (val) {
		decimateSurfaceResult = val;
	    setBusy(false);
	    notifyReturnValue(val);
	},
			"LHDL-APPLICATION", "DECIMATE-SURFACE-GOAL",
			[{name: "HAS-FILE-URL", value: form.fileUrl.value},
			 {name: "HAS-PRESERVE-TOPOLOGY", value: jsToGoalArg(preserveTopologyButton.get("checked"))},
			 {name: "HAS-TARGET-REDUCTION", value: form.targetReduction.value},
			 {name: "goals:hasAccount", value: form.btaccount.value}]);
    };
}

// }}}

// {{{ import decimate render surface
function drawImportDecimateRender () {
  document.getElementById("ops-panel").innerHTML =
    "<form id='input-panel'><table>"
    + "<tr><td>BT account</td><td><input type='text' name='btaccount' value='#_defaultBiomedTownAccount' /></td></tr>"
    + "<tr><td>Filename</td><td id='urlSpace'><input type='text' name='filename' value='"
    + "sample/1091_Femur_Right.stl" + "' size='50'/></td></tr>"
    + "<tr><td>Preserve Topology</td><td id='preserveTopologyButtonSpace'></td></tr>"
    + "<tr><td>Target reduction</td><td><input type='text' name='targetReduction' value='1'"
    + "<tr><td>Background colour</td><td><input type='text' name='bgColour' value='0/0/0' size='11'/></td></tr>"
    + "<tr><td>Zoom factor</td><td><input id='zoomFactor' value='2.0'/></td></tr>"
    + "<tr><td>Camera azimuth</td><td><input id='cameraAzimuth' value='90'/></td></tr>"
    + "<tr><td>Camera roll</td><td><input id='cameraRoll' value='225'/></td></tr>"
    + "<tr><td></td><td id='executeButtonSpace'></td></tr>"
    + "</form></table>";

  var preserveTopologyButton = new YAHOO.widget.Button({
    id: "preserveTopologyButton", type: "checkbox",
    checked: true, container: "preserveTopologyButtonSpace"});

  // execute button
  var executeButton = new YAHOO.widget.Button({
    label:"Execute", container:"executeButtonSpace",
    onclick: {fn: doImportDecimateRender2(preserveTopologyButton)}});
}

function doImportDecimateRender2(preserveTopologyButton) {
    return function (x) {
	form = document.getElementById("input-panel");
	setBusy(true);
	irs.achieveGoal(function (val) {
	    setRenderSurfaceResult(val);
	    setBusy(false);
	    notifyReturnValue(val);
	    showImage();
	},
			"LHDL-APPLICATION", "IMPORT-DECIMATE-RENDER-GOAL",
			[{name: "HAS-FILENAME", value: form.filename.value},
			 {name: "HAS-PRESERVE-TOPOLOGY", value: jsToGoalArg(preserveTopologyButton.get("checked"))},
			 {name: "HAS-TARGET-REDUCTION", value: form.targetReduction.value},
			 {name: "HAS-BACKGROUND-COLOUR", value: form.bgColour.value},
			 {name: "HAS-ZOOM-FACTOR", value: form.zoomFactor.value},
			 {name: "HAS-CAMERA-AZIMUTH", value: form.cameraAzimuth.value},
			 {name: "HAS-CAMERA-ROLL", value: form.cameraRoll.value},
			 {name: "HAS-IMAGE-WIDTH", value: "400"},
			 {name: "HAS-IMAGE-HEIGHT", value: "400"},
			 {name: "goals:hasAccount", value: form.btaccount.value}]);
    };
}
// }}}


// {{{ Manage VME surface

function drawManageVMEsurface () {
  document.getElementById("ops-panel").innerHTML =
	"<h3>Define composition</h3>"
    + "<form id='input-panel'><table>"
    + "<tr><td valign='right'>Source Type: </td><td> <select name='SourceClass'><option value='VTKsource'>Import VTK</option><option value='STLsource'>Import STL</option><option value='CreateCone'>Create Cone Surface</option><option value='CreateCube'>Create Cube Surface</option></select></td></tr>"
	+ "<tr><td valign='right'><br/>Operation Type: </td><td><br/> <select name='OperationClass'><option value='SmoothSurface'>Smooth Surface</option><option value='StripSurface'>Strip Surface</option><option value='TriangulateSurface'>Triangulate Surface</option></select></td></tr>"
	+ "<tr><td valign='right'><br/>Destination Type: </td><td><br/> <select name='DestinationClass'><option value='ExportVTK'>Export to VTK</option><option value='ExportSTL'>Export to STL</option><option value='RenderSurface'>Render Surface</option></select></td></tr>"
    + "<tr><td></td><td id='executeButtonSpace'><br/><br/></td></tr>"
    + "</form></table>";

//  var preserveTopologyButton = new YAHOO.widget.Button({
//    id: "preserveTopologyButton", type: "checkbox",
//    checked: true, container: "preserveTopologyButtonSpace"});

  // execute button
  var executeButton = new YAHOO.widget.Button({
    label:"Next", container:"executeButtonSpace",
    onclick: {fn: drawManageVMEsurface2}});
}

function drawManageVMEsurface2 () {
	form = document.getElementById("input-panel");

	// source inputs
	if (form.SourceClass.value == "VTKsource")
		{sCode = "<tr><td>Source Filename: </td><td id='urlSpace'><input type='text' name='filename' value='sample/head.vtk' size='50'/></td></tr>";}
	if (form.SourceClass.value == "STLsource")
		{sCode = "<tr><td>Source Filename: </td><td id='urlSpace'><input type='text' name='filename' value='sample/1091_Femur_Right.stl' size='50'/></td></tr>";}
	if (form.SourceClass.value == "CreateCube")
		{sCode = "<tr><td>Cube-X: </td><td id='XSpace'><input type='text' name='x' value='2' size='50'/></td></tr>"
		+ "<tr><td>Cube-Y: </td><td id='YSpace'><input type='text' name='y' value='2' size='50'/></td></tr>"
		+ "<tr><td>Cube-Z: </td><td id='ZSpace'><input type='text' name='z' value='2' size='50'/></td></tr>";}
	if (form.SourceClass.value == "CreateCone")
		{sCode = "<tr><td>Cone Height: </td><td id='HSpace'><input type='text' name='height' value='5' size='50'/></td></tr>"
		+ "<tr><td>Cone Radius: </td><td id='RaSpace'><input type='text' name='radius' value='5' size='50'/></td></tr>"
		+ "<tr><td>Cone Resolution: </td><td id='ReSpace'><input type='text' name='resolution' value='20' size='50'/></td></tr>"
		+ "<tr><td>Cone Cap: </td><td id='CapSpace'><input type='text' name='cap' value='true' size='50'/></td></tr>";}

	// operation inputs
	if (form.OperationClass.value == "SmoothSurface")
		{oCode = "<tr><td>Smooth Iterations: </td><td id='IterationSpace'><input type='text' name='iterations' value='30' size='50'/></td></tr>";}
		else {oCode = "";}


	// destination inputs
	if ((form.DestinationClass.value == "ExportVTK") || (form.DestinationClass.value == "ExportSTL"))
		{dCode = "<tr><td>Destination ABSMatrixFlag: </td><td id='ABSSpace'><input type='text' name='abs' value='true' size='50'/></td></tr>"
				 + "<tr><td>Destination Format: </td><td id='FormatSpace'><input type='text' name='format' value='ascii' size='50'/></td></tr>";}
	if (form.DestinationClass.value == "RenderSurface")
		{dCode = "<tr><td>Destination bgColour: </td><td id='bgSpace'><input type='text' name='bg' value='0/0/0' size='50'/></td></tr>"
				 + "<tr><td>Destination Zoom: </td><td id='zoomSpace'><input type='text' name='zoom' value='1' size='50'/></td></tr>"
				 + "<tr><td>Destination camAzimuth: </td><td id='aziSpace'><input type='text' name='azimuth' value='0' size='50'/></td></tr>"
				 + "<tr><td>Destination camRoll: </td><td id='rollSpace'><input type='text' name='roll' value='0' size='50'/></td></tr>"
				 + "<tr><td>Destination Width: </td><td id='widthSpace'><input type='text' name='iwidth' value='300' size='50'/></td></tr>"
				 + "<tr><td>Destination Height: </td><td id='heightSpace'><input type='text' name='iheight' value='300' size='50'/></td></tr>";}

	document.getElementById("ops-panel").innerHTML = "<br/> <h3>Process Inputs: </h3>"
    + "<form id='input-panel'><table>"
	+ sCode
	+ "<br/><br/>"
	+ oCode
	+ "<br/><br/>"
	+ dCode
	+ "<br/><br/>"
	+ "<input type='hidden' name='SourceClass' value=" + form.SourceClass.value + ">"
	+ "<input type='hidden' name='OperationClass' value="+ form.OperationClass.value + ">"
	+ "<input type='hidden' name='DestinationClass' value="+ form.DestinationClass.value + ">"
	+ "<tr><td></td><td id='executeButtonSpace2'><br/><br/></td></tr>"
    + "</form></table>";


  // execute button
  var executeButton = new YAHOO.widget.Button({
    label:"Execute", container:"executeButtonSpace2",
    onclick: {fn: doManageVMEsurface}});
}

function doManageVMEsurface() {
  form = document.getElementById("input-panel");

  // source goal input
  if ((form.SourceClass.value == "VTKsource") || (form.SourceClass.value == "STLsource"))
	 {sValue = "(" + form.SourceClass.value + " " + form.filename.value + ")";}
  if (form.SourceClass.value == "CreateCube")
	 {sValue = "(" + form.SourceClass.value + " " + form.x.value + " " + form.y.value + " " + form.z.value + ")";}
  if (form.SourceClass.value == "CreateCone")
	 {sValue = "(" + form.SourceClass.value + " " + form.height.value + " " + form.radius.value + " " + form.resolution.value + " " + form.cap.value + ")";}

  // operation goal input
  if (form.OperationClass.value == "SmoothSurface")
	 {oValue = "(" + form.OperationClass.value + " " + form.iterations.value + ")";}
  if ((form.OperationClass.value == "StripSurface") || (form.OperationClass.value == "TriangulateSurface"))
	 {oValue = "(" + form.OperationClass.value + ")";}

  // sink goal input
  if ((form.DestinationClass.value == "ExportVTK") || (form.DestinationClass.value == "ExportSTL"))
	 {dValue = "(" + form.DestinationClass.value + " " + form.abs.value + " " + form.format.value + ")";}
  if (form.SourceClass.value == "RenderSurface")
	 {dValue = "(" + form.DestinationClass.value + " " + form.bg.value + " " + form.zoom.value + " " + form.azimuth.value + " " + form.roll.value + " " + form.iwidth.value + " " + form.iheight.value + ")";}



  setBusy(true);
  irs.achieveGoal(function (val) {
		    setBusy(false);
		  },
		  "LHDL-APPLICATION", "MANAGE-VMESURFACE-GOAL",
		  [{name: "HAS-SOURCE", value:sValue},
		   {name: "HAS-OPERATION", value:oValue},
		   {name: "HAS-SINK", value:dValue},
		   {name: "goals:hasAccount", value: form.btaccount.value}]);

}

// }}}

// {{{ amazon s3

function drawBTtoS3 () {
  document.getElementById("ops-panel").innerHTML =
    "<h2>Copy from BT to S3</h2><form id='input-panel'><table>"
    + "<tr><td>BT account</td><td><input type='text' name='btaccount' value='#_defaultBiomedTownAccount' /></td></tr>"
    + "<tr><td>BT filename</td><td><input type='text' name='btfilename' value='' /></td></tr>"
    + "<tr><td>S3 account</td><td><input type='text' name='s3account' value='lhdl-amazons3-account'"
    + "<tr><td>S3 bucket</td><td><input type='text' name='s3bucket' value='lhdl'"
    + "<tr><td>S3 filename</td><td><input type='text' name='s3filename' value=''"
    + "<tr><td></td><td id='executeButtonSpace'></td></tr>"
    + "</form></table>";

  // execute button
  var executeButton = new YAHOO.widget.Button({
    label:"Execute", container:"executeButtonSpace",
    onclick: {fn: doBTtoS3}});
}

function doBTtoS3() {
  form = document.getElementById("input-panel");
  setBusy(true);
  irs.achieveGoal(function (val) {
		    setBusy(false);
		  },
		  "LHDL-APPLICATION", "MOVE-FILE-BT-TO-S3-GOAL",
		  [{name: "HAS-FILENAME", value: form.btfilename.value},
		   {name: "goals:hasBiomedTownAccount", value: form.btaccount.value},
		   {name: "HAS-AMAZON-ACCOUNT", value: form.s3account.value},
		   {name: "HAS-AMAZON-BUCKET", value: form.s3bucket.value},
		   {name: "HAS-AMAZON-KEY", value: form.s3filename.value}]);
}

function drawS3toBT () {
  document.getElementById("ops-panel").innerHTML =
    "<h2>Copy from S3 to BT</h2><form id='input-panel'><table>"
    + "<tr><td>S3 account</td><td><input type='text' name='s3account' value='lhdl-amazons3-account'"
    + "<tr><td>S3 bucket</td><td><input type='text' name='s3bucket' value='lhdl'"
    + "<tr><td>S3 filename</td><td><input type='text' name='s3filename' value=''"
    + "<tr><td>BT account</td><td><input type='text' name='btaccount' value='#_defaultBiomedTownAccount' /></td></tr>"
    + "<tr><td>BT filename</td><td id='urlSpace'><input type='text' name='btfilename' value='' size='50'/></td></tr>"
    + "<tr><td></td><td id='executeButtonSpace'></td></tr>"
    + "</form></table>";

  // execute button
  var executeButton = new YAHOO.widget.Button({
    label:"Execute", container:"executeButtonSpace",
    onclick: {fn: doS3toBT}});
}

function doS3toBT() {
  form = document.getElementById("input-panel");
  setBusy(true);
  irs.achieveGoal(function (val) {
		    setBusy(false);
		  },
		  "LHDL-APPLICATION", "MOVE-FILE-S3-TO-BT-GOAL",
		  [{name: "HAS-AMAZON-ACCOUNT", value: form.s3account.value},
		   {name: "goals:hasBiomedTownAccount", value: form.btaccount.value},
		   {name: "HAS-AMAZON-BUCKET", value: form.s3bucket.value},
		   {name: "HAS-AMAZON-KEY", value: form.s3filename.value},
		   {name: "HAS-FILENAME", value: form.btfilename.value}
		  ]);
}

// }}}

// {{{ Menubar

var menuData =
    [{text: "File",
      submenu: {id: "fileMenu", itemdata:
		[{text: "Import STL",
		  onclick: {fn: drawImportStl}},
		 {text: "Import VRML",
		  onclick: {fn: drawImportVrml}},
		 {text: "Import VTK",
		  onclick: {fn: drawImportVtk}},
		 {text: "Export VTK",
		  onclick: {fn: drawExportVtk}}
		]}},
     {text: "Operations",
      submenu: {id: "opsMenu", itemdata:
		[{text: "Extract Iso Surface",
		  onclick: {fn: drawExtractIsoSurface}},
		 {text: "Decimate Surface",
		  onclick: {fn: drawDecimateSurface}}
		]}},
     {text: "View",
      submenu: {id: "viewMenu", itemdata:
		[{text: "Render Surface",
		  onclick: {fn: drawRenderSurface}}]}},
     {text: "Compositions",
      submenu: {id: "compositionsMenu", itemdata:
		[{text: "Import, decimate, render",
		  onclick: {fn: drawImportDecimateRender}},
		 {text: "Manage VME Surface",
		  onclick: {fn: drawManageVMEsurface}}]}},
     {text: "Amazon S3",
      submenu: {id: "amazons3Menu", itemdata:
		[{text: "BT->S3", onclick: {fn: drawBTtoS3}},
		{text: "S3->BT", onclick: {fn: drawS3toBT}}
		]}}
    ];

function drawMenuBar () {
    /*
         Instantiate a MenuBar:  The first argument passed to the
         constructor is the id of the element to be created; the
         second is an object literal of configuration properties.
    */

    var oMenuBar = new YAHOO.widget.MenuBar("mymenubar",
	{lazyload: true, itemdata: menuData});
    /*
         Since this MenuBar instance is built completely from
         script, call the "render" method passing in a node
         reference for the DOM element that its should be
         appended to.
    */
    oMenuBar.render(document.getElementById("menubarContainer"));
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
// {{{ Utilities
// Just for booleans at the moment.
function jsToGoalArg (x) {
    if (x) {
	return "true";
    } else {
	return "false";
    }
}

var renderSurfaceResult;

function setRenderSurfaceResult(val) {
    renderSurfaceResult = val.substring(1,val.length-1);
}

function showImage () {
    // XXX HACK!  To force the image to reload, we attach a query
    // parameter.  The server ignores it, but because the URL is different
    // each time, the browser will reload it.
    var now=new Date();
    document.getElementById("image-panel").innerHTML = "<img src='" + renderSurfaceResult + "?time=" + now.getTime() + "'/>";
}

function notifyReturnValue (str) {
    var maxLength = 50;
    var line;

    // XXX The IRS returns the value as a string containing a string.
    // Until we fix that, just strip it out.
    lastReturnValue = str.substring(1,str.length -1 );
    if (str.length > maxLength) {
	line = str.substring(0,50) + "...";
    } else {
	line = str;
    }
    // document.getElementById("return-value-panel").innerHTML =
    // 	"<verbatim>" + xmlEscape(line) + "</verbatim>";
}

// Busy indicator

function setBusy(busy) {
    var el = document.getElementById("busyBox");
    if (busy) {
	el.innerHTML = "<img src='" + rollerImage + "'/>";
    } else {
	el.innerHTML = "";
    }
}
// }}}
