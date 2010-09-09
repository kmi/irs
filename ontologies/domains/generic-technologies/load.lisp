;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  
  (ensure-ontology common-concepts domain "OCML:library;domains;common-concepts;load.lisp" )
  )

(def-ontology generic-technologies :includes (common-concepts )
  :type :domain
    :author "enrico"
  :allowed-editors ("john" )
  :files ("generic-technologies"
                           "new"
                           ))