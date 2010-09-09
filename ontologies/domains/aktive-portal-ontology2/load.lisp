;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")



(ensure-ontology akt-support-ontology2 domain
                 "OCML:LIBRARY;domains;akt-support-ontology2;load.lisp")

(def-ontology aktive-portal-ontology2
  :author "enrico"
  :includes (akt-support-ontology2)
  :allowed-editors ("john")
  :do-not-include-base-ontology? t
  :files ("docs"  "organizations" 
           "events" "research-areas" "techs" "projects"    
           "new")
  :version 2.0)