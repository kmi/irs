;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology generic-business-queries
  :includes (cobra evo)
  :type :domain 
  :namespace-uri "http://kmi.open.ac.uk/ontologies/generic-business-queries#"
  :namespaces (("cobra" cobra)
               ("evo" evo))               
;;               ("units" units-manipulation)
;;               ("si" international-system-units)
;;               ("phys-q" physical-quantities))
  :files ("generic-business-queries")
  :author "carlos" :allowed-editors ("nil"))