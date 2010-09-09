;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology cost-ontology)

(def-class productivity-loss (indirect-cost))

(def-class productivity-loss-of-carers (productivity-loss))

(def-class staff (direct-cost))

(def-class patient-productivity-loss (productivity-loss))

(def-class instrument-purchase (direct-cost))

(def-class drugs (direct-cost))

(def-class side-effect (direct-cost))

(def-class ecological-cost (indirect-cost))

