;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology base-ontology basic "ocml:library;basic;load.lisp" ))

(def-ontology softskills-ontology :includes (base-ontology)
              :type :domain 
              :author "b_de_vree" :allowed-editors ("b_de_vree" "Katholieke"))