;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology bibliographic-data domain 
                   "OCML:LIBRARY;domains;bibliographic-data;load.lisp" )
  (ensure-ontology generic-events domain "OCML:LIBRARY;domains;generic-events;load.lisp" )
  (ensure-ontology generic-technologies domain 
                   "OCML:LIBRARY;domains;generic-technologies;load.lisp" )
  (ensure-ontology occupation-ontology domain
                   "OCML:LIBRARY;domains;occupation-ontology;load.lisp" ))

(def-ontology organization-ontology 
  :includes
  (bibliographic-data generic-events generic-technologies occupation-ontology)
  :type :domain
  :author "enrico"
  :allowed-editors ("john" )
  :files ("organization-ontology"
          "new"
          ))