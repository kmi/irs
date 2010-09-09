;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology british-ethnic-groups-ontology)

(def-class asian-other (asian-ethnic-group))

(def-class bangladeshi (asian-ethnic-group))

(def-class pakistani (asian-ethnic-group))

(def-class indian (asian-ethnic-group))

(def-class black-african (black-ethnic-group))

(def-class black-caribbean (black-ethnic-group))

(def-class black-other (black-ethnic-group))

(def-class other-ethnic-group (british-minority-ethnic-group))

(def-class white-ethnic-group (british-ethnic-group))

