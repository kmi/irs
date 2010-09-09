;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology 
   organization-ontology 
   domain 
   "ocml:library;domains;organization-ontology;load.lisp" ))

(def-ontology medical-ontology :includes (organization-ontology)
              :type :domain 
              :author "enrico" :allowed-editors ("silvana" "john"))