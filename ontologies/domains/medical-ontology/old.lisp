;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology medical-ontology)

(def-class clinical-instrument (medical-ontology-object)
 ((associated-medical-condition :type medical-condition)))

(def-class patient (person)
  ((has-medical-condition :type medical-condition)))

(def-class medical-condition (medical-ontology-object)
  ((has-risk-factor :type medical-variable)))

(def-class generic-care-giver(person))

(def-class non-professional-care-giver (generic-care-giver)
	((cares-for :type person)))

(def-class health-care-professional (generic-care-giver working-person))

(def-class lesion (medical-condition))

(def-class nurse (health-care-professional))

(def-class physician (health-care-professional))

(def-class health-care-technician (health-care-professional))

(def-class paramedic (health-care-professional))



(def-relation chair-bound (?x)
 :constraint (person ?x))

(def-relation bed-ridden (?x)
 :constraint (person ?x))