;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" ))

(def-ontology luisa-first-prototype-ontology :includes (wsmo)
              :type :domain 
              :author "stefan" :allowed-editors ("john" "alessio" "enrico"))
