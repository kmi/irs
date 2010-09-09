;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology medical-guidelines domain "ocml:library;domains;medical-guidelines;load.lisp" ))

(def-ontology pressure-ulcer :includes (medical-guidelines)
              :type :domain 
              :author "enrico" :allowed-editors ("john"))