;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology monitoring-ontology)

(def-class monitoring-observer ()
    ((has-session-id :type string)
     (has-callback :type function)))
