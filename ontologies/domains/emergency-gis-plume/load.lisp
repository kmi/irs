;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" )
  (ensure-ontology emergency-gis-situations domain "ocml:library;domains;emergency-gis-situations;load.lisp")
  (ensure-ontology sgis-spatial domain "ocml:library;domains;sgis-spatial;load.lisp" ))

(def-ontology emergency-gis-plume :includes (wsmo sgis-spatial emergency-gis-situations)
              :type :domain 
              :author "wsmo" :allowed-editors ("vlad" "john" "carlos" "alessio")
              :files ("emergency-gis-plume" "new" "lilo"))

