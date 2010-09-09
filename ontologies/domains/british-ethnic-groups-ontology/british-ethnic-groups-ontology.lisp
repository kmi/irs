;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology british-ethnic-groups-ontology)

(def-class british-ethnic-group (ethnic-group ))

(def-class british-minority-ethnic-group (british-ethnic-group ))

(def-class asian-ethnic-group (british-minority-ethnic-group))

(def-class black-ethnic-group (british-minority-ethnic-group))

(def-class british-ethnic-group-type () ?x
  :iff-def (british-ethnic-group ?x))

















