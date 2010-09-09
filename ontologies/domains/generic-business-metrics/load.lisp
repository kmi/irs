;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology generic-business-metrics
  :includes (generic-business-queries metrics-ontology evo)
  :type :domain 
  :namespace-uri "http://kmi.open.ac.uk/ontologies/generic-business-metrics#"
  :namespaces (("cobra" cobra)
               ("metrics" metrics-ontology)
               ("gbq" generic-business-queries)
               ("evo" evo))
;;               ("units" units-manipulation)
;;               ("si" international-system-units)
;;               ("phys-q" physical-quantities))
  :files ("generic-business-metrics")
  :author "carlos" :allowed-editors ("nil"))