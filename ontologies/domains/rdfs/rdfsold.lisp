;;; Mode: Lisp; Package: ocml

(in-package "OCML")

(in-ontology rdfs)

;;;Automatically translated from RDF file "freaky:rdf-files;rdfs.rdf"
;;;at 19:0:24, on 21/3/2003

(def-class RESOURCE ()
  ((Value )
   (Isdefinedby 
    :type resource)
   (Seealso 
    :type resource)
   (Label 
    :type literal)
   (Comment 
    :type literal)
   ))


(def-class LITERAL ()
  ())


(def-class CLASS ()
  ())


(def-class PROPERTY ()
  ((Range 
    :type class)
   (Domain 
    :type class)
   (Subpropertyof 
    :type property)
   ))


(def-class STATEMENT ()
  ((Object )
   (Predicate 
    :type property)
   (Subject 
    :type resource)
   ))


(def-class CONTAINER ()
  ((Member )
   ))


(def-class BAG ()
  ())


(def-class SEQ ()
  ())


(def-class ALT ()
  ())


(def-class CONTAINERMEMBERSHIPPROPERTY ()
  ())

(def-instance CLASS-CONTAINERMEMBERSHIPPROPERTY-AS-INSTANCE CLASS
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "containermembershipproperty")
   (Comment "the container membership properties, rdf:1, rdf:2, ..., all of which are sub-properties of 'member'.")
   (Subclassof property)
   (Subclassof resource)
   ))
(def-instance CLASS-ALT-AS-INSTANCE CLASS
  ((Isdefinedby "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
   (Label "alt")
   (Comment "a collection of alternatives.")
   (Subclassof container)
   (Subclassof resource)
   ))
(def-instance CLASS-SEQ-AS-INSTANCE CLASS
  ((Isdefinedby "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
   (Label "seq")
   (Comment "an ordered collection.")
   (Subclassof container)
   (Subclassof resource)
   ))
(def-instance CLASS-BAG-AS-INSTANCE CLASS
  ((Isdefinedby "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
   (Label "bag")
   (Comment "an unordered collection.")
   (Subclassof container)
   (Subclassof resource)
   ))
(def-instance CLASS-CONTAINER-AS-INSTANCE CLASS
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "container")
   (Subclassof resource)
   (Comment "this represents the set containers.")
   ))
(def-instance CLASS-STATEMENT-AS-INSTANCE CLASS
  ((Isdefinedby "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
   (Label "statement")
   (Subclassof resource)
   (Comment "the class of rdf statements.")
   ))
(def-instance CLASS-PROPERTY-AS-INSTANCE CLASS
  ((Isdefinedby "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
   (Label "property")
   (Comment "the concept of a property.")
   (Subclassof resource)
   ))
(def-instance CLASS-CLASS-AS-INSTANCE CLASS
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "class")
   (Comment "the concept of class")
   (Subclassof resource)
   ))
(def-instance CLASS-LITERAL-AS-INSTANCE CLASS
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "literal")
   (Comment "this represents the set of atomic values, eg. textual strings.")
   ))
(def-instance CLASS-RESOURCE-AS-INSTANCE CLASS
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "resource")
   (Comment "the class resource, everything.")
   ))

(def-relation TYPE (?x ?y)
  )

(def-instance TYPE property
  ((Isdefinedby "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
   (Label "type")
   (Comment "indicates membership of a class")
   (Range class)
   (Domain resource)
   ))

(def-relation SUBCLASSOF (?x ?y)
  )

(def-instance SUBCLASSOF property
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "subclassof")
   (Comment "indicates membership of a class")
   (Range class)
   (Domain class)
   ))

(def-relation SUBPROPERTYOF (?x ?y)
  )

(def-instance SUBPROPERTYOF property
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "subpropertyof")
   (Comment "indicates specialization of properties")
   (Range property)
   (Domain property)
   ))

(def-relation COMMENT (?x ?y)
  )

(def-instance COMMENT property
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "comment")
   (Comment "use this for descriptions")
   (Domain resource)
   (Range literal)
   ))

(def-relation LABEL (?x ?y)
  )

(def-instance LABEL property
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "label")
   (Comment "provides a human-readable version of a resource name.")
   (Domain resource)
   (Range literal)
   ))

(def-relation DOMAIN (?x ?y)
  )

(def-instance DOMAIN property
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "domain")
   (Comment "a domain class for a property type")
   (Range class)
   (Domain property)
   ))

(def-relation RANGE (?x ?y)
  )

(def-instance RANGE property
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "range")
   (Comment "a range class for a property type")
   (Range class)
   (Domain property)
   ))

(def-relation SEEALSO (?x ?y)
  )

(def-instance SEEALSO property
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "seealso")
   (Comment "a resource that provides information about the subject resource")
   (Range resource)
   (Domain resource)
   ))

(def-relation ISDEFINEDBY (?x ?y)
  )

(def-instance ISDEFINEDBY property
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Subpropertyof seealso)
   (Label "isdefinedby")
   (Comment "indicates the namespace of a resource")
   (Range resource)
   (Domain resource)
   ))

(def-relation SUBJECT (?x ?y)
  )

(def-instance SUBJECT property
  ((Isdefinedby "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
   (Label "subject")
   (Comment "the subject of an rdf statement.")
   (Domain statement)
   (Range resource)
   ))

(def-relation PREDICATE (?x ?y)
  )

(def-instance PREDICATE property
  ((Isdefinedby "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
   (Label "predicate")
   (Comment "the predicate of an rdf statement.")
   (Domain statement)
   (Range property)
   ))

(def-relation OBJECT (?x ?y)
  )

(def-instance OBJECT property
  ((Isdefinedby "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
   (Label "object")
   (Comment "the object of an rdf statement.")
   (Domain statement)
   ))

(def-relation MEMBER (?x ?y)
  )

(def-instance MEMBER property
  ((Isdefinedby "http://www.w3.org/2000/01/rdf-schema#")
   (Label "member")
   (Comment "a member of a container")
   (Domain container)
   ))

(def-relation VALUE (?x ?y)
  )

(def-instance VALUE property
  ((Isdefinedby "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
   (Label "value")
   (Comment "identifies the principal value (usually a string) of a property when the property value is a structured resource")
   (Domain resource)
   ))

(def-instance "HTTP://WWW.W3.ORG/2000/01/RDF-SCHEMA#" resource
  ((Seealso //www.w3.org/2000/01/rdf-schema-more)
   ))