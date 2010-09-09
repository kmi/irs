;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology common-concepts domain "ocml:library;domains;common-concepts;load.lisp" ))

(def-ontology uk-location-ontology :includes (common-concepts)
              :type :domain
              :files ("uk-location-ontology" "new" "relations")
              :author "john" :allowed-editors ("enrico"))