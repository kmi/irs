;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology generic-events)

(def-class event-involving-project (event)
	"An event which involves some kind of project: launching it, reviewing it, new funds for it"
	((project-involved :type project)) 
	;;;;;:slot-renaming ((project-involved instrument-used))
)

(def-class event-project-launch (event-involving-project))
