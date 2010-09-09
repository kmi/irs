;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology luisa-lom2-wsml domain "ocml:library;domains;luisa-lom2-wsml;load.lisp" ))

(def-ontology luisa-wsmo-descriptions :includes (luisa-lom2-wsml)
              :type :domain 
              :author "wsmo" :allowed-editors ("nil"))