;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology medical-guidelines domain "ocml:library;domains;medical-guidelines;load.lisp" ))

(def-ontology post-stroke-rehabilitation :includes (medical-guidelines)
              :type :domain 
              :author "john" :allowed-editors ("enrico" "silvana"))