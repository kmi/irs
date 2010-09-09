;;; Copyright © 2008 The Open University

(in-package #:ocml)

(def-ontology wsml
    :type :domain
    :includes (xsd-types)
    :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/xsd-types#"
    :namespaces (("wsml" wsml-types)
                 ("xsd" xsd-types))
    :files ("time")
    :author "dave")
