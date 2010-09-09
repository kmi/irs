;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology standard-prefixes
  :includes (physical-quantities)
  :type :domain 
  :namespace-uri "http://kmi.open.ac.uk/ontologies/standard-prefixes#"
  :namespaces (("phys-q" physical-quantities))
  :files ("standard-prefixes")
  :author "carlos" :allowed-editors ("nil"))