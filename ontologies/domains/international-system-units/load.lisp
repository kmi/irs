;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology international-system-units
  :includes (physical-quantities standard-dimensions standard-prefixes)
  :type :domain 
  :namespace-uri "http://kmi.open.ac.uk/ontologies/international-system-units#"
  :namespaces (("phys-q" physical-quantities)
               ("si-dim" standard-dimensions)
               ("si-prefixes" standard-prefixes))
  :files ("international-system-units")
  :author "carlos" :allowed-editors ("nil"))