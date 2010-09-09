;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology lhdl-goals)

(def-class lhdl-goal (goal))

(def-class execution-goal (lhdl-goal))
(def-class storage-goal (lhdl-goal))
(def-class community-goal (lhdl-goal))
