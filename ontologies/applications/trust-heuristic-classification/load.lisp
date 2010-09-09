;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology trust-heuristic-classification
    :includes (trust-profile classificationv2-class-map country-ontology cryptography)
    :type :application
    :namespaces (("crypt" cryptography))
    :author "stefania" :allowed-editors ("john" "alessio"))
