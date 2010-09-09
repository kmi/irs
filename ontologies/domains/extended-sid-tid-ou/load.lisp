;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology sid-ou domain "ocml:library;domains;sid-ou;load.lisp" ))

(def-ontology extended-sid-tid-ou :includes (sid-ou)
              :type :domain 
              :author "wsmo" )