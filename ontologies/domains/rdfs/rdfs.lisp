;;; Mode: Lisp; Package: ocml

(in-package "OCML")

(in-ontology RDFS)

;;;Automatically translated from RDF file "FREAKY:RDF-FILES;rdfs.rdf"
;;;at 19:13:5, on 6/6/2003

(def-class RESOURCE ()
((Value )
(Is-Defined-By 
:type resource)
(See-Also 
:type resource)
(Label 
:type literal)
(Comment 
:type literal)
))


(def-class LITERAL ()
())


(def-class CONTAINER (resource)
((Member )
))


(def-class STATEMENT (resource)
((Object )
(Predicate 
:type property)
(Subject 
:type resource)
))


(def-class PROPERTY (resource)
((Range 
:type class)
(Domain 
:type class)
(Sub-Property-Of 
:type property)
))


(def-class CLASS (resource)
())


(def-class BAG (container resource)
())


(def-class SEQ (container resource)
())


(def-class ALT (container resource)
())


(def-class CONTAINER-MEMBERSHIP-PROPERTY (property resource)
())

(def-instance CONTAINER-MEMBERSHIP-PROPERTY CLASS
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))
(def-instance ALT CLASS
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(Label NIL)
(Comment NIL)
))
(def-instance SEQ CLASS
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(Label NIL)
(Comment NIL)
))
(def-instance BAG CLASS
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(Label NIL)
(Comment NIL)
))
(def-instance CLASS CLASS
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))
(def-instance PROPERTY CLASS
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(Label NIL)
(Comment NIL)
))
(def-instance STATEMENT CLASS
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(Label NIL)
(Comment NIL)
))
(def-instance CONTAINER CLASS
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))
(def-instance LITERAL CLASS
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))
(def-instance RESOURCE CLASS
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))



(def-relation SUB-PROPERTY-OF (?x ?y)
:constraint (and (property ?y) (property ?x))
)

(def-instance SUB-PROPERTY-OF property
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))

(def-relation COMMENT (?x ?y)
:constraint (and (resource ?x) (literal ?y))
)

(def-instance COMMENT property
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))

(def-relation LABEL (?x ?y)
:constraint (and (resource ?x) (literal ?y))
)

(def-instance LABEL property
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))

(def-relation DOMAIN (?x ?y)
:constraint (and (class ?y) (property ?x))
)

(def-instance DOMAIN property
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))

(def-relation RANGE (?x ?y)
:constraint (and (class ?y) (property ?x))
)

(def-instance RANGE property
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))

(def-relation SEE-ALSO (?x ?y)
:constraint (and (resource ?y) (resource ?x))
)

(def-instance SEE-ALSO property
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))

(def-relation IS-DEFINED-BY (?x ?y)
:constraint (and (resource ?y) (resource ?x))
)

(def-instance IS-DEFINED-BY property
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Sub-Property-Of SEE-ALSO)
(Label NIL)
(Comment NIL)
))

(def-rule IS-DEFINED-BY-IS-SUB-PROPERTY-OF-SEE-ALSO 
((see-also ?x ?y) if 
(is-defined-by ?x ?y)))


(def-relation SUBJECT (?x ?y)
:constraint (and (statement ?x) (resource ?y))
)

(def-instance SUBJECT property
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(Label NIL)
(Comment NIL)
))

(def-relation PREDICATE (?x ?y)
:constraint (and (statement ?x) (property ?y))
)

(def-instance PREDICATE property
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(Label NIL)
(Comment NIL)
))

(def-relation OBJECT (?x ?y)
:constraint (and (statement ?x))
)

(def-instance OBJECT property
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(Label NIL)
(Comment NIL)
))

(def-relation MEMBER (?x ?y)
:constraint (and (container ?x))
)

(def-instance MEMBER property
((Is-Defined-By "http://www.w3.org/2000/01/rdf-schema#")
(Label NIL)
(Comment NIL)
))

(def-relation VALUE (?x ?y)
:constraint (and (resource ?x))
)

(def-instance VALUE property
((Is-Defined-By "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(Label NIL)
(Comment NIL)
))

(def-instance "HTTP://WWW.W3.ORG/2000/01/RDF-SCHEMA#" resource
((See-Also //WWW.W3.ORG/2000/01/RDF-SCHEMA-MORE)
))