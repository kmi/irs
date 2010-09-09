;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology cost-ontology domain 
                   "OCML:LIBRARY;domains;cost-ontology;load"))

(def-ontology health-rel-payoff
              :author "enrico"
              :allowed-editors ("kmi" "hcrema")
              :includes (cost-ontology))


;(eval-when (eval load)
;  (load-ontology-files 'health-rel-payoff 'application
;                         '("hc-rema-payoff-ocml"
;                           "new"
;                           )
;                         "health-rel-payoff")
;    (select-ontology 'health-rel-payoff))



