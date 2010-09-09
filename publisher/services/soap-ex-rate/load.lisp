;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology money-ontology  domain 
                   "OCML:LIBRARY;domains;money-ontology;load.lisp"))

(def-ontology exchange-rate-provider-ontology
  :includes (money-ontology)
  :author "enrico"
  :allowed-editors ("john")
  :type :method)

