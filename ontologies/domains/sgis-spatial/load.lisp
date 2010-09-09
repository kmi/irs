;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" )
  (ensure-ontology archetypes domain "ocml:library;domains;archetypes;load.lisp" ))

(def-ontology sgis-spatial :includes (wsmo archetypes)
              :type :domain 
              :author "wsmo" :allowed-editors ("john" "wsmo" "vlad")
              :files ("sgis-spatial" "new" "lilo"))
