;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology economic-sector-ontology domain "ocml:library;domains;economic-sector-ontology;load.lisp" ))

(def-ontology occupation-ontology :includes (economic-sector-ontology)
              :type :domain 
              :author "tim" :allowed-editors ("john"))