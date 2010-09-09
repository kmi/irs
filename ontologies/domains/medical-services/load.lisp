;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
   (ensure-ontology
    medical-guidelines
    domain
     ))

(def-ontology medical-services :includes (medical-guidelines)
               :type :domain
               :author "enrico" :allowed-editors ("john"))