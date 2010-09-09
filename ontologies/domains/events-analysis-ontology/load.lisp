;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology events-analysis-ontology 
  :includes (evo)
  :namespace-uri "http://kmi.open.ac.uk/ontologies/eao#"
  :namespaces (("cobra" cobra)
               ("evo" evo)
               ("time" time-ontology))        
  :type :domain 
  :author "wsmo" :allowed-editors ("nil"))
