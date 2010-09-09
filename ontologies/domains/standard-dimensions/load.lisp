;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology standard-dimensions
  :includes (physical-quantities)
  :type :domain 
  :namespace-uri "http://kmi.open.ac.uk/ontologies/standard-dimensions#"
  :namespaces (("phys-q" physical-quantities))
  :files ("standard-dimensions")
  :author "carlos" :allowed-editors ("nil"))