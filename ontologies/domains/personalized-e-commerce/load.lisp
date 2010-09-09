;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology common-concepts domain "ocml:library;domains;common-concepts;load.lisp" ))

(def-ontology personalized-e-commerce :includes (common-concepts)
              :type :domain 
              :author "helgi" :allowed-editors ("helgi" "maria_c"))
