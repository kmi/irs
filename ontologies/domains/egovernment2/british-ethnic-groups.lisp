;;; Mode: Lisp; Package: ocml

;;;Authors Liliana Cabral, John Domingue

;;; File created in WebOnto

(in-package "OCML")

(in-ontology egovernment2)

(def-class ethnic-group ())


(def-class british-ethnic-group (ethnic-group ))


(def-class british-ethnic-group-type () ?x
  :iff-def (or (= british-ethnic-group ?x)
               (subclass-of ?x british-ethnic-group)))

(def-class british-minority-ethnic-group (british-ethnic-group ))

(def-class british-minority-ethnic-group-type () ?x
  :iff-def (or (= ?x british-minority-ethnic-group)
               (subclass-of ?x british-minority-ethnic-group)))

(def-class asian-ethnic-group (british-minority-ethnic-group))

(def-class black-ethnic-group (british-minority-ethnic-group))


(def-class asian-other (asian-ethnic-group))

(def-class bangladeshi (asian-ethnic-group))

(def-class pakistani (asian-ethnic-group))

(def-class indian (asian-ethnic-group))

(def-class black-african (black-ethnic-group))

(def-class black-caribbean (black-ethnic-group))

(def-class black-other (black-ethnic-group))

(def-class other-ethnic-group (british-minority-ethnic-group))

(def-class white-ethnic-group (british-ethnic-group))




