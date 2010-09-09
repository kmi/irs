;;; Mode: Lisp; Package: ocml 

;;; Plans ontology.
;;; Author Aldo Gangemi

 ;;; Translated from owl with OWL2OCML Translator (Author: Baldassarre Claudio)

(in-package "OCML")

(eval-when (eval load)

(ensure-ontology FunctionalParticipation  domain
                  "ocml:library;domains;FunctionalParticipation;load.lisp")

)

(def-ontology Plans 

:includes (FunctionalParticipation)
:files ("Plans"
        "new"))