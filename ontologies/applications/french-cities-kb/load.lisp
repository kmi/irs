;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology country-ontology domain "ocml:library;domains;country-ontology;load.lisp" ))

(def-ontology french-cities-kb :includes (country-ontology)
              :type :application 
              :author "john" :allowed-editors ("liliana" "farshad" "wsmo"))