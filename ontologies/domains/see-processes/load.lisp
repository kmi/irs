;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology see-processes
  :includes (cobra)
  :type :domain 
  :namespace-uri "http://www.ip-super.org/ontologies/SEEProcesses/20080710#"
  :namespaces (("COBRA" cobra))
  :files ("see-processes" 
          "new")
  :author "carlos" :allowed-editors ("nil"))