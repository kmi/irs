;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" ))

(def-ontology d-s :includes (wsmo)
              :namespace-uri "http://www.kmi.open.ac.uk/d-s#"
              :type :domain 
              :author "alessio" :allowed-editors ("stefan" "john")
              :namespaces (("d-s" d-s)))

