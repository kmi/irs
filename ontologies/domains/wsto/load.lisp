;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology wsto
    :includes (cryptography heuristic-classification wsmo)
    :type :domain
    :namespaces (("crypt" cryptography))
    :author "stefania"
    :allowed-editors ("john" "alessio"))
