;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" )
  (ensure-ontology sgis-spatial domain "ocml:library;domains;sgis-spatial;load.lisp" ))

(def-ontology emergency-gis-domain :includes (wsmo sgis-spatial)
              :type :domain 
              :author "alessio" :allowed-editors ("john" "vlad" "wsmo")
              :files ("emergency-gis-domain" "new" "lilo"))