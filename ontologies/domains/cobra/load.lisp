;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology cobra
              :type :domain 
              :includes (time-ontology physical-quantities)
              ;;:namespace-uri "http://www.ip-super.org/ontologies/COBRA/20071215#"
              :namespace-uri "http://ip-super.org/ontologies/process/cobra/v1.1.1#"
              :Namespaces (("time" time-ontology)
                           ("phys-q" physical-quantities))
              :files ("cobra" 
                      "new")
              :author "carlos" :allowed-editors ("nil"))