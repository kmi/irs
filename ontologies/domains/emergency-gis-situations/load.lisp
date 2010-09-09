;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" )
  (ensure-ontology classificationv2-class-map task "ocml:library;tasks;classificationv2-class-map;load.lisp" )
  (ensure-ontology heuristic-classification method "ocml:library;methods;heuristic-classification;load.lisp" )
  (ensure-ontology emergency-gis-domain domain "ocml:library;domains;emergency-gis-domain;load.lisp" ))

(def-ontology emergency-gis-situations :includes (wsmo classificationv2-class-map heuristic-classification emergency-gis-domain)
              :type :domain 
              :author "alessio" :allowed-editors ("vlad" "john"))