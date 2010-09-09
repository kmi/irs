;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology sgis-spatial domain "ocml:library;domains;sgis-spatial;load.lisp" )
  (ensure-ontology emergency-gis-situations domain "ocml:library;domains;emergency-gis-situations;load.lisp" ))

(def-ontology met-office-domain :includes (sgis-spatial emergency-gis-situations)
              :type :domain 
              :author "alessio" :allowed-editors ("john" "vlad")
              :files ("met-office-domain" "new" "lilo"))