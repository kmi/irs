;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology common-concepts domain "ocml:library;domains;common-concepts;load.lisp" ))

(def-ontology uk-county-knowledge-base :includes (common-concepts)
              :type :application 
              :author "john" :allowed-editors ("tim"))