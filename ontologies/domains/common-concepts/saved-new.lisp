;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology common-concepts)

(def-class ethnic-group-type () ?x
  :iff-def (or (subclass-of ?x ethnic-group)
               (= ?x ethnic-group)))

(def-class capital-city (city)
  ((is-capital-of :type country)))

(def-class computer-mediated-conferencing (internet-technology))

(def-class ethnic-group ())

(def-class web-page ()
"Generalizes from URL to give more information about web sites"
((has-title :type string)
(has-URL :type string))
)

(def-class software-agent (generic-agent))

(def-class organization-unit (temporal-thing)
  ((has-full-name :type string)
   (has-web-address :type URL)
   (has-address :type postal-address) 
   (affiliated-people :type affiliated-person) 
   (organization-unit-part-of :type organization)
   (has-sub-unit :type organization-unit)
   (headed-by :type affiliated-person)))

(def-class race ())

(def-class age-range (pair) ?x
  :iff-def (and (pair ?X) (positive-real-number (first ?x))
                 (positive-real-number (second ?X)))
:lisp-fun '(lambda (x env)
(Let ((instantiated-x (instantiate x env)))
     (if (and (listp instantiated-x)(= (length instantiated-x)2)
          (holds? 'positive-real-number (car instantiated-x))
(holds? 'positive-real-number (second instantiated-x)))
(List env)
:fail))))

(def-class population-specification ()?x
  "Members of this class do not violate any of the associated constraints"
  ((has-gender-constraint :type gender)
    (has-age-constraint :type age-range)
    (has-race-constraint :type race)
    (has-geographical-constraint :type geographical-region)
    (has-generic-constraint :type unary-kappa-expression
                            :documentation "A kappa expression which is true for each member of this class")))

(def-class software-system (software-technology))

(def-class group-of-people (temporal-thing)
	"It represents a group of people which are together in a project or some kind of
meeting. The related event is information about the reason why they meet together."
	((full-name-group :type string :min-cardinality 1)
	(people-who-belong-to :type person)
	(related-events :type event)
	(has-web-address :type web-page))
)

(def-class Information-Bearing-Object (temporal-thing);;;;(knowledge-asset)
   "A collection of objects which are both tangible and intangible. 
   (See also the comment for its superset Composite-Tangible-And-Intangible-Object.) 
   Each element of Information-Bearing-Object is an object that can be interpreted, 
   by an interpreter understanding its conventions, to yield a chunk or chunks
   of information. Information-Bearing-Object includes all of the following: 
    (1) artifacts made solely for the purpose of conveying information (e.g., a
     newspaper, or a children's science video); 
    (2) artifacts that convey information in addition to their intended function (e.g., Neolithic pottery)
    (3) non-artifacts, such as a person's fingerprints, gestures, and utterances, which may be interpreted 
        to yield information. Note: `an IBO' abbreviates `an information bearing object'."
  ((has-author :type generic-agent)
   (owned-by :type (or organization person))))

(def-class information-stuff (temporal-thing)
	"It can be any piece of information which can be presented in a talk.
	In Cyc it is defined as an information-bearing-object, being a subclass
	of a composite-tangible-and-intangible-object and an information-bearing-thing
	It should be placed in the common-concepts ontology
	Definition in Ontolingua: A collection of objects which are both tangible and
 intangible. (See also the comment for its superset Composite-Tangible-And-Intangible-
Object.) Each element of Information-Bearing-Object is an object that can be 
interpreted, by an interpreter understanding its conventions, to yield a chunk or
chunks of information. Information-Bearing-Object includes all of the following: (1) 
artifacts made solely for the purpose of conveying information
(e.g., a newspaper, or a children's science video); (2) artifacts that convey information in 
addition to their intended function (e.g., Neolithic pottery); and (3) non-artifacts, such as 
a person's fingerprints, gestures, and utterances, which may be interpreted to yield 
information. Note: `an IBO' abbreviates `an information bearing object'."
	((info-about :type temporal-thing :documentation
	 "Added by myself so that the kind of information can be stated"))
)

(def-class person-type ()?x
  :iff-def (subclass-of ?X person))

(def-class organization-size ())

(def-class learning-support-technology (software-technology))

(def-class telephone-technology (hardware-technology))

(def-class town (geopolitical-entity))

(def-class economic-sector ())

(def-class economic-sector-type () ?x
  :iff-def (or (= ?x economic-sector) (subclass-of ?x economic-sector)))

(def-class organization-size-type () ?x
  :iff-def (or (= organization-size ?x) (subclass-of ?x organization-size)))

(def-class large-sized-organization (organization-size))

(def-class micro-sized-organization (organization-size))

(def-class computer-based-training-technology (learning-support-technology))

(def-class computer-assisted-learning-technology (learning-support-technology))

(def-class minitel-technology (telephone-technology))

(def-class small-or-medium-sized-organization (organization-size ))

(def-class medium-sized-organization (small-or-medium-sized-organization))

(def-class small-sized-organization (small-or-medium-sized-organization))

(def-instance male-gender gender)

(def-instance female-gender gender)

