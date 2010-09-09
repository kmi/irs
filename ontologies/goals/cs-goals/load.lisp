;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" )
  (ensure-ontology conceptual-spaces domain "ocml:library;domains;conceptual-spaces;load.lisp" ))


(def-ontology cs-goals :includes (wsmo conceptual-spaces)
              :type :goal 
              :author "alessio" :allowed-editors ("stefan"))