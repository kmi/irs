;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology common-concepts domain "ocml:library;domains;common-concepts;load.lisp" ))

(def-ontology british-ethnic-groups-ontology :includes (common-concepts)
              :type :domain 
              :author "john" :allowed-editors ("enrico"))