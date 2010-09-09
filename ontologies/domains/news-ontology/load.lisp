;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology generic-events domain "OCML:library;domains;generic-events;load.lisp" )
  (ensure-ontology generic-technologies domain "OCML:library;domains;generic-technologies;load.lisp" ))

(def-ontology news-ontology :includes (generic-events generic-technologies) 
  :author "enrico"
  :allowed-editors ("john" )
  :files ("news-ontology"
          "new"
          ))
