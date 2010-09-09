;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology swift-services-ontology domain "ocml:library;domains;swift-services-ontology;load.lisp" ))

(def-ontology swift-services-datatypes  :includes (swift-services-ontology)
              :type :domain 
              :author "john" :allowed-editors ("liliana" "leticia"))