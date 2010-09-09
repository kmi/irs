;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology execution-history 
  :includes (evo events-analysis-ontology see-processes metrics-ontology fulfillment)
  :namespace-uri "http://www.ip-super.org/ontologies/execution-history#"
  :namespaces (("EVO" evo)
               ("COBRA" cobra)
               ("TIME" time-ontology)
               ("PHYS-Q" physical-quantities)
               ("SEE" see-processes))
  :type :domain 
  :files ("execution-history" "review")
  :author "carlos" )


