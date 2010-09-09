;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology softskills-ontology)

(def-class leidinggevende_eigenschappen ())

(def-class soft_skills (leidinggevende_eigenschappen ))

(def-class soft_skills ())

(def-class diploma ())

(def-class communicatieve_vaardigheden (soft_skills))

(def-class communicatieve_vaardigheden (soft_skills))

(def-class mondeling (communicatieve_vaardigheden ))

(def-class schriftelijk(communicatieve_vaardigheden))



(def-class leidinggevende_eigenschappen (soft_skills))

(def-class leidinggevende_eigenschappen ())