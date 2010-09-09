;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology evo :includes (cobra)
  ;;:namespace-uri "http://www.ip-super.org/ontologies/EVO/20071215#"
  :namespace-uri "http://ip-super.org/ontologies/process/evo/v1.0.0#"
  :namespaces (("cobra" cobra)
               ("time" time-ontology)
               ("phys-q" physical-quantities))
  :type :domain 
  :files ("evo")
  :author "carlos" )


