;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology simple-time domain "ocml:library;domains;simple-time;load.lisp" ))

(def-ontology common-concepts :includes (simple-time)
              :type :domain 
              :author "enrico" :allowed-editors ("john"))