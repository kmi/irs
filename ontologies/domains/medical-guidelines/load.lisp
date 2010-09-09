;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology medical-ontology domain "ocml:library;domains;medical-ontology;load.lisp" )
  (ensure-ontology organization-ontology domain "ocml:library;domains;organization-ontology;load.lisp" ))

(def-ontology medical-guidelines :includes (medical-ontology organization-ontology)
              :type :domain 
              :author "enrico" :allowed-editors ("silvana"))