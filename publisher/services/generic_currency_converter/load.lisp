;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology exchange-rate-provider-ontology method
                   "OCML:LIBRARY;methods;exchange-rate-provider-ontology;load.lisp"))

(def-ontology generic-currency-converter
  :includes (exchange-rate-provider-ontology)
  :author "enrico"
  :allowed-editors ("john")
  :type :method)

