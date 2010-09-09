;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" ))

(def-ontology top-level-ontology :includes (wsmo)
              :type :domain 
              :author "wsmo" :allowed-editors ("nil"))