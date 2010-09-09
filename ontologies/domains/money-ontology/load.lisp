;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology organization-ontology  domain 
                   ))

(def-ontology money-ontology
  :includes (organization-ontology)
  :author "enrico"
  :allowed-editors ("john")
  :files ("money-ontology" "new"))
