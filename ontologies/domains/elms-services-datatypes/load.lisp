;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology elms-kb application "ocml:library;applications;elms-kb;load.lisp" ))

(def-ontology elms-services-datatypes :includes (elms-kb)
              :type :domain 
              :author "john" :allowed-editors ("liliana" "leticia"))