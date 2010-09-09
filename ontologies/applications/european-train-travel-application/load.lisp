;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  ;;(ensure-ontology austrian-cities-kb application "ocml:library;applications;austrian-cities-kb;load.lisp" )
  (ensure-ontology german-cities-kb application "ocml:library;applications;german-cities-kb;load.lisp" )
  (ensure-ontology french-cities-kb application "ocml:library;applications;french-cities-kb;load.lisp" )
  (ensure-ontology uk-county-knowledge-base application "ocml:library;applications;uk-county-knowledge-base;load.lisp" ))

(def-ontology european-train-travel-application :includes (;;austrian-cities-kb 
                                                           german-cities-kb 
                                                           french-cities-kb
                                                           uk-county-knowledge-base)
              :type :application 
              :author "john" :allowed-editors ("liliana" "farshad" "wsmo"))