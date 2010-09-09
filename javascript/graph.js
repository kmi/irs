function drawGraph () {
    irs.getConceptGraph(function(nodes) {var graph = new Graph(nodes);},
		     document.graphform.ontology.value,
		     document.graphform.concept.value);
}

function makeCanvas(id,width,height) {
    var canvas = document.createElement("canvas");
    canvas.id = id;
    canvas.width = width;
    canvas.height = height;
    return canvas;
}

// XXX Need to zero all the bits when we redraw :(

function Graph(nodes) {
    this.nodes = nodes;
    this.canvas = makeCanvas("graph", 800, 800);
    this.scale = 1.2;
    document.body.appendChild(this.canvas);
    drawEdges(this.canvas, this.nodes);
    drawNodes(this.canvas, this.nodes);
}

function drawEdges(canvas, nodes) {
    var g = canvas.getContext("2d");
    g.lineWidth = 1;
    g.strokeStyle = "red";
    for (var n=0; n<nodes.length; n++) {
	var node = nodes[n];
	for (var s=0; s<node.superclasses.length; s++) {
	    var sooper = findByName(node.superclasses[s], nodes);
	    g.beginPath();
	    g.moveTo(node.x, node.y);
	    g.lineTo(sooper.x, sooper.y);
	    g.closePath();
	    g.fillStyle = "red";
	    g.fill();
	    g.stroke();
	}
    }
}

function findByName(name, nodes) {
    for (var n=0; n<nodes.length; n++) {
 	if (name == nodes[n].name) {
	    return nodes[n];
 	}
    }
}

function drawNodes(canvas, nodes) {
    var g = canvas.getContext("2d");
    g.lineWidth = 2;
    g.strokeStyle = "black";

    for (var i=0; i<nodes.length; i++) {
	var node = nodes[i];

	// Draw the circle
	g.beginPath();
	g.moveTo(node.x, node.y);
	g.arc(node.x, node.y, 5, 0, 360, false);
	g.closePath();
	g.fillStyle = "black";
	g.fill();
	g.stroke();
	// Add the text label
	var label = document.createElement("div");
	node.label = label;
	label.style.position = "absolute";
	label.style.left = (canvas.offsetLeft + node.x + 10)+"px";
	label.style.top = (canvas.offsetTop + node.y - 5)+"px";
	label.style.fontFamily = "sans-serif";
	label.style.fontSize = "10px";
	label.appendChild(document.createTextNode(node.name));
	document.body.appendChild(label);
    }
}
