;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" )
  (ensure-ontology d-s domain "ocml:library;domains;d-s;load.lisp" ))


(def-ontology conceptual-situation-spaces :includes (wsmo d-s)
              :type :domain 
              :author "stefan" :allowed-editors ("alessio" "john")
              :namespace-uri "http://www.kmi.open.ac.uk/css#"
              :namespaces (("css" conceptual-situation-spaces)))
