;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" )
  (ensure-ontology monitoring-events domain "ocml:library;domains;monitoring-events;load.lisp" ))

(def-ontology see-events :includes (wsmo monitoring-events)
              :type :domain 
              :author "carlos" )