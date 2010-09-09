;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology akt-support-ontology2)

;;;A very simple top-level. We define something called THING, which 
;;;is the top-level concept in the ontology.  We then distinguish two basic 
;;;types of 'things': TANGIBLE-THING, something that has some physicality,
;;;and INTANGIBLE-THING, something which has not. We use a very open definition 
;;;of being tangible: obviusly a physical object is tangible, but also a sub-atomic
;;;particle is tangible, even if some of them are very tricky (you do not see them)
;;;you only see the trace they leave behind.  Also a piece of software will be
;;;considered a tangible thing, it is something that you can see on a floppy disk.
;;;In contrast an algorithm will be an intangible, although the file that contains
;;;its implementation will be a tangible thing.

;;;This is a stop-gap top level. Over the course of teh project we may want to extend this 
;;;and link to existing top levels (SUO, HPKB, CYC, Sowa, Guarino, etc.)


(def-class THING ()
  "This is the top-level concept in the AKT reference ontology"
  ((has-pretty-name :type string
                    :max-cardinality 1
                    :documentation "A human readable name")
   (has-variant-name :type string
                     :documentation "Other variants of the human readable name")))


(def-class INTANGIBLE-THING (thing)
  "This comes from HPKB upper level.
   The collection of things that are not physical -- are not made 
   of, or encoded in, matter. Every Collection is an Intangible (even if its
   instances are tangible), and so are some Individuals. 
   Caution: do not confuse `tangibility' with `perceivability' -- humans can perceive
   light even though it's intangible--at least in a sense.")

(def-class TEMPORAL-THING (thing)
  "Like in Cyc, this is something which has a temporal extent."
  ((has-time-interval :type time-interval)))

(def-class TANGIBLE-THING (temporal-thing)
  "Something which is not intangible, something which is physical, made of matter.
   It does not matter whether things are real of imaginary.  Therefore we consider
   Mickey Mouse's car and a hippogriff as  tangible things")

(def-axiom TANGIBLE-AND-INTANGIBLE-THINGS-ARE-DISJOINT
  (subclass-partition thing (set-of Tangible-Thing  Intangible-thing)))

(def-class INDIVIDUAL (Thing) ?x
  "Something which is not a set.  For instance an instance of a class."
  :iff-def (not (set ?x))

  ;;;the definitions below are effective ways to prove whether 
  ;;;somebody is an individual in OCML
  :prove-by (or 
             (and (variable-bound ?x)
                  (not (set ?x)))
             (= ?x nil))
  :no-proofs-by (:iff-def))

(def-class QUANTITY (Individual Intangible-thing)
  "From SUO: Any specification of how many or how much of something there is. 
   Accordingly, there are two subclasses of Quantity: Number (how many) 
   and Physical-Quantity (how much).")

(def-class PHYSICAL-QUANTITY (quantity)
 "SUO: Physical Quantities are distinguished from Numbers by the
  fact that the former are associated with a dimension of measurement."
  ((has-unit-of-measure :type unit-of-measure)
   (has-magnitude :type number)))


(def-class UNIT-OF-MEASURE (Intangible-thing)
  "Any kind of unit of measure, metre, dollar, kilogram, etc..")


