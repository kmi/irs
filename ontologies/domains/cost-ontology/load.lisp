;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology medical-ontology domain 
                   "OCML:LIBRARY;domains;medical-ontology;load"))

(def-ontology cost-ontology
              :author "enrico"
              :allowed-editors ("kmi" "hcrema")
              :includes (medical-ontology))


;(eval-when (eval load)
;  (load-ontology-files 'cost-ontology 'application
;                         '("hc-rema-cost-ocml"
;                           "new"
;                           )
;                         "cost-ontology")
;    (select-ontology 'cost-ontology))

