;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" ))

(def-ontology luisa-domain :includes (wsmo)
              :type :domain 
              :author "wsmo" :allowed-editors ("nil"))