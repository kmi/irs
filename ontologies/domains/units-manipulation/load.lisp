;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

;;; The ontology depends on some statistics functions
(require :cl-statistics)

(def-ontology units-manipulation
  :includes (international-system-units)
  :type :domain 
  :namespace-uri "http://kmi.open.ac.uk/ontologies/units-manipulation#"
  :namespaces (("si" international-system-units)              
               ("phys-q" physical-quantities))
  :files ("units-manipulation")
  :author "carlos" :allowed-editors ("nil"))