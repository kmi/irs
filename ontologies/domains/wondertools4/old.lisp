;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology wondertools4)

(def-class universitair_persoon ()
((naam :type string)
(adres :type string))
)

(def-class medewerker (universitair_persoon )
((salaris :type integer))
)

(def-class student (universitair_persoon )
((studiep :type integer))
)

(def-class prop_student (student ))

(def-class doct_student (student ))

(def-class prom_student (student ))

(def-class ondersteunend_personeel (medewerker ))

(def-class wetenschappelijk_personeel (medewerker ))

(def-class aio (ondersteunend_personeel prom_student ))

(def-class oio (ondersteunend_personeel prom_student ))

(def-rule prop (prop_student ?x)if
(student ?x)
(studiepunt ?x <42))

(def-rule prop (prop_student ?x)if
(student ?x)
(studiepunt ?x <42))

(def-instance Bob universitair_persoon
  ((naam Bob Wielinga)
   (adres Bobstraat 1)))

(def-instance Arthur doct_student
  ((studiepunten 21)
   (adres Jan van Kuikweg 83)
   (naam Arhtur Duineveld)))

(def-instance piet medewerker
  ((naam Piet)
   (adres Pietstraat 1)
   (salaris 1000)))

(def-instance Kees ondersteunend_personeel
  ((salaris 100)
   (adres Keestraat 2)
   (naam Kees)))

(def-instance marcel student
  ((naam Marcel Weiden)
   (adres Reigerstraat 7)
   (studiep 21)))



(def-instance john student
  ((naam "john domingue")
   (adres )
   (studiep )))
