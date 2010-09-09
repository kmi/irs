// We mostly create these from JSON data received from the IRS.
function OcmlClass(data) {
  this._data = data;

  // There is probably an easier way to do this.
  this.name = data.name;
  this.superclasses = data.superclasses;
  this.subClasses = data.subClasses;
  this.documentation = data.documentation;
  this.slots = data.slots;

  this.getSlotClassValues = function(slotname) {
    // XXX Only deal with single values at this point.
    return findSlot(slotname, this.slots).values;
  }

  this.getSlotType = function(slotname) {
    return findSlot(slotname, this.slots).type;
  }
}

function findSlot(object, array) {
  for (var i=0; i < array.length; i++) {
    if (array[i].name == object) {
      return array[i];
    }
  }
  return null;
}

function OcmlInstance(data) {
    this._data = data;

    this.name = data.name;
    this.documentation = data.documentation;
    this.homeOntology = data.homeOntology;
    this.class = data.class;
    this.slots = data.slots;
}

function OcmlOntology(data) {
    this._data = data;

    this.name = data.name;
    this.documentation = data.documentation;
    this.namespaceUri = data.namespaceUri;
    this.includes = data.includes;
    this.includedBy = data.includedBy;
    this.namespacePrefixes = data.namespacePrefixes;
    this.type = data.type;
    this.author = data.author;
    this.editors = data.editors;
    this.files = data.files;
}

function OcmlRelation(data) {
    this._data = data;

    this.name = data.name;
    this.documentation = data.documentation;
    this.homeOntology = data.homeOntology;
    this.schema = data.schema;
    this.localSlotOf = data.localSlotOf;
    this.slotOf = data.slotOf;
    this.constraint = data.constraint;
    this.iffDef = data.iffDef;
    this.sufficient = data.sufficient;
    this.associatedRules = data.associatedRules;
    this.proveBy = data.proveBy;
    this.lispFun = data.lispFun;
    this.instances = data.instances;
}

function OcmlFunction(data) {
    this._data = data;

    this.name = data.name;
    this.documentation = data.documentation;
    this.homeOntology = data.homeOntology;
    this.schema = data.schema;

    this.constraint = data.constraint;
    this.definition = data.definition;
    this.body = data.body;
    this.lispFun = data.lispFun;
}

function OcmlRule(data) {
    this._data = data;
    this.name = data.name;
    this.homeOntology = data.homeOntology;
    this.direction = data.direction;
    this.documentation = data.documentation;

    if (this.direction == 'BACKWARD') {
	this.definesRelation = data.definesRelation;
    }
}
