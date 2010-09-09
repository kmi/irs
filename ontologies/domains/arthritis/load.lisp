;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology medical-guidelines domain 
                    ))

(def-ontology arthritis 
  :includes (medical-guidelines)
  :author "enrico"
 )
