;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology persons-and-organizations)

(def-class course ()
 ((course-name) (host-educational-organization) (host-department)))

(def-class presentation ()
  ((presentation-title :type string)
   (presentation-topic)
   (speaker)
   (presentation-abstract :type string)))

(def-class minister (politician)
  ((portfolio) (areas-of-interest)))

(def-class drawing-technology (multimedia-technology))

(def-class higher-educational-course (course)
  ((host-faculty)))

(def-class open-university-course (higher-educational-course)
  ((host-educational-organization :default-value open-university)))

(def-instance course-t171 open-university-course
  ((course-name "you, your computer and the net")
   (host-department department-of-telematics)
   (host-faculty faculty-of-technology)))

(def-instance department-of-telematics academic-unit
  ((part-of faculty-of-technology)))

(def-instance howells minister
  ((portfolio lifelong-learning)
   (constituency pontypridd)
   (areas-of-interest learning-age university-for-industry)
   (biography "Dr Howells entered parliament in 1989 and is MP for Pontypridd.  In
opposition, he has spoken for Development and Cooperation, Foreign and
Commonwealth Affairs, Home Affairs and Trade and Industry. His current
government position in Lifelong Learning at the DfEE has seen the
publication of a Green Paper on the Learning Age and the planning
prospectus for the University for Industry.")))
