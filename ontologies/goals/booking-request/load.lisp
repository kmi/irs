;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology wsmo domain "ocml:library;domains;wsmo;load.lisp" )
  (ensure-ontology conceptual-spaces domain "ocml:library;domains;conceptual-spaces;load.lisp" ))


(def-ontology booking-request :includes (wsmo conceptual-spaces)
              :type :goal 
              :files ("new" "booking-request" "booking-request2")
              :author "stefan" :allowed-editors ("nil"))