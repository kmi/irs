;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" ))

(def-ontology asm-ontology :includes (wsmo)
              :type :domain 
              :author "simon" :allowed-editors ("john" "barry"))