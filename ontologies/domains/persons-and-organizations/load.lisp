;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-



(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology event-ontology domain
                   "OCML:LIBRARY;domains;event-ontology;load.lisp")
  )

(def-ontology persons-and-organizations :includes (event-ontology)
 :author "enrico"
  :allowed-editors ("john" )
  :files ("persons-and-organizations"
                                                            "new"))