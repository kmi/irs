;;; Mode: Lisp; Package: ocml

(in-package "OCML")


(eval-when (eval load)
  (ensure-ontology akt-support-ontology  domain
                   "OCML:LIBRARY;domains;rdfs;load.lisp"))

;;;File generated at 14:55:54, on 5/11/2003

(def-ontology ontoweb-ontology
 :includes (rdfs)
 :do-not-include-base-ontology? t)