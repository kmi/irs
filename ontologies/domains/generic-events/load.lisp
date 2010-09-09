;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology common-concepts domain 
                   "OCML:LIBRARY;domains;common-concepts;load.lisp"
                    ))

(def-ontology generic-events :includes (common-concepts) :type :domain
   :author "enrico"
  :allowed-editors ("john" )
  :files ("generic-events"
          "new"
          ))