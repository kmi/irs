;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology medical-ontology)

(def-class generic-care-giver(person))

(def-class generic-care-giver-type ()?X
  :iff-def (subclass-of ?X generic-care-giver))

(def-class medical-condition (medical-ontology-object)
  ((has-risk-factor :type medical-variable)))

(def-class patient (person)
  ((has-medical-condition :type medical-condition)))


(def-class healthcare-instrument (medical-ontology-object)
 ((associated-medical-condition-class :type medical-condition-type)))

(def-class clinical-instrument (healthcare-instrument)
  "These are instruments that can only be used in a clinical setting")


(def-class generalized-healthcare-technique (medical-ontology-object))

(def-class generalized-clinical-technique (generalized-healthcare-technique)
  "These are techniques that can only be applied in a clinical setting")

(def-class non-professional-care-giver (generic-care-giver)
  ((cares-for :type person)))

(def-class health-care-professional (generic-care-giver working-person))

(def-class lesion (medical-condition))

(def-class paramedic (health-care-professional))

(def-class health-care-technician (health-care-professional))

(def-class physician (health-care-professional))

(def-class nurse (health-care-professional))

(def-relation chair-bound (?x)
 :constraint (person ?x))

(def-relation bed-ridden (?x)
 :constraint (person ?x))
