;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology egovernment-change-of-circumstances-ontology domain "ocml:library;domains;egovernment-change-of-circumstances-ontology;load.lisp" ))

(def-ontology elms-ontology :includes (egovernment-change-of-circumstances-ontology)
              :type :domain 
              :author "john" :allowed-editors ("liliana" "leticia"))

