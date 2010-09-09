;;; Mode: Lisp; Package: ocml

(in-package "OCML")

(in-ontology RDFS-ONTOLOGY)

;;;Automatically translated from RDF file "freaky:rdf-files;rdfs.rdf"
;;;at 19:52:46, on 29/10/2003

(def-instance RESOURCE resource
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#" )
(Has-Pretty-Name "Resource" )
))

(def-relation TYPE (?x ?y)
"Indicates membership of a class "
:pretty-name "type"
:constraint (and (class ?y))
)

(def-instance TYPE binary-relation
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
))

(def-instance CLASS resource
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#" )
(Has-Pretty-Name "Class" )
(Subclass-Of RESOURCE )
))

(def-relation SUB-CLASS-OF (?x ?y)
"Indicates membership of a class "
:pretty-name "subClassOf"
:constraint (and (class ?y))
)

(def-instance SUB-CLASS-OF binary-relation
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
))

(def-relation SUB-PROPERTY-OF (?x ?y)
"Indicates specialization of properties "
:pretty-name "subPropertyOf"
:constraint (and (property ?y))
)

(def-instance SUB-PROPERTY-OF binary-relation
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
))

(def-instance PROPERTY resource
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#" )
(Has-Pretty-Name "Property" )
(Subclass-Of RESOURCE )
))

(def-relation COMMENT (?x ?y)
"Use this for descriptions "
:pretty-name "comment"
:constraint (and (literal ?y))
)

(def-instance COMMENT binary-relation
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
))

(def-relation LABEL (?x ?y)
"Provides a human-readable version of a resource name. "
:pretty-name "label"
:constraint (and (literal ?y))
)

(def-instance LABEL binary-relation
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
))

(def-relation DOMAIN (?x ?y)
"A domain class for a property type "
:pretty-name "domain"
:constraint (and (class ?y))
)

(def-instance DOMAIN binary-relation
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
))

(def-relation RANGE (?x ?y)
"A range class for a property type "
:pretty-name "range"
:constraint (and (class ?y))
)

(def-instance RANGE binary-relation
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
))

(def-relation SEE-ALSO (?x ?y)
"A resource that provides information about the subject resource "
:pretty-name "seeAlso"
:constraint (and (resource ?y))
)

(def-instance SEE-ALSO binary-relation
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
))

(def-relation IS-DEFINED-BY (?x ?y)
"Indicates the namespace of a resource "
:pretty-name "isDefinedBy"
:constraint (and (resource ?y))
)

(def-instance IS-DEFINED-BY binary-relation
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Sub-Property-Of SEE-ALSO)
))

(def-rule IS-DEFINED-BY-IS-SUB-PROPERTY-OF-SEE-ALSO 
((see-also ?x ?y) if 
(is-defined-by ?x ?y)))


(def-instance LITERAL resource
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#" )
(Has-Pretty-Name "Literal" )
))

(def-instance STATEMENT resource
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#" )
(Has-Pretty-Name "Statement" )
(Subclass-Of RESOURCE )
))

(def-relation SUBJECT (?x ?y)
"The subject of an RDF statement. "
:pretty-name "subject"
:constraint (and (resource ?y))
)

(def-instance SUBJECT binary-relation
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
))

(def-relation PREDICATE (?x ?y)
"the predicate of an RDF statement. "
:pretty-name "predicate"
:constraint (and (property ?y))
)

(def-instance PREDICATE binary-relation
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
))

(def-relation OBJECT (?x ?y)
"The object of an RDF statement. "
:pretty-name "object"
)

(def-instance OBJECT binary-relation
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
))

(def-instance CONTAINER resource
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#" )
(Has-Pretty-Name "Container" )
(Subclass-Of RESOURCE )
))

(def-instance BAG resource
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#" )
(Has-Pretty-Name "Bag" )
(Subclass-Of CONTAINER )
))

(def-instance SEQ resource
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#" )
(Has-Pretty-Name "Seq" )
(Subclass-Of CONTAINER )
))

(def-instance ALT resource
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#" )
(Has-Pretty-Name "Alt" )
(Subclass-Of CONTAINER )
))

(def-instance CONTAINER-MEMBERSHIP-PROPERTY resource
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#" )
(Has-Pretty-Name "ContainerMembershipProperty" )
(Subclass-Of PROPERTY )
))

(def-relation MEMBER (?x ?y)
"a member of a container "
:pretty-name "member"
)

(def-instance MEMBER binary-relation
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
))

(def-relation VALUE (?x ?y)
"Identifies the principal value (usually a string) of a property when the property value is a structured resource "
:pretty-name "value"
)

(def-instance VALUE binary-relation
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
))

(def-instance "HTTP://WWW.W3.ORG/2000/01/RDF-SCHEMA#" resource
((See-Also "http://www.w3.org/2000/01/rdf-schema-more" )
))