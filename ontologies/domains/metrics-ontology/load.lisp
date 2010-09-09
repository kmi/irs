;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology metrics-ontology
  :includes (units-manipulation cobra)
  :type :domain 
  :namespace-uri "http://kmi.open.ac.uk/ontologies/metrics-ontology#"
  :namespaces (("cobra" cobra)
               ("units" units-manipulation)
               ("si" international-system-units)
               ("phys-q" physical-quantities))
  :files ("metrics-ontology" "tests")
  :author "carlos" :allowed-editors ("nil"))