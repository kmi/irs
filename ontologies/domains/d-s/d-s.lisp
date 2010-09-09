;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology d-s)


;;; dolce top clasess and relations

(def-class #_entity ()
  "top level class in DOLCE"
  ((has-documentation :type string)
   (has-title :type string)
   (#_has-quality :type #_quality :min-cardinality 1)))

;;

(def-class #_endurant (#_entity)
  ((#_part-of :type #_endurant)))

(def-class #_perdurant (#_entity)
  ((#_part-of :type #_perdurant)))

(def-class #_abstract (#_entity))

;;

(def-class #_quality-type ()
  ((#_qt-location :type #_region :min-cardinality 1)))

(def-class #_region (#_abstract)
  ((#_part-of :type #_region)
   (#_qt-location-of :type #_quality-type :min-cardinality 1) 
   (#_q-location-of :type #_quality :min-cardinality 1))
:avoid-infinite-loop t)


(def-class #_quale (#_region)
  "It's an atomic region (member)")

(def-class #_quality-space (#_region)
  "It's a maximal topological region (conceptual space)")

(def-class #_quality (#_endurant)
  "Each quality has a type (quality is measured, type not)"
  ((#_q-belongs-to :type #_quality-type :min-cardinality 1)
   (#_inhere-in :type #_entity :cardinality 1)
   (#_q-location :type #_region :min-cardinality 1))
:avoid-infinite-loop t)


;;

(def-relation #_part-of (?x ?z)
  :sufficient (and (#_part-of ?x ?y)
                   (#_part-of ?y ?z))
:avoid-infinite-loop t)

(def-relation #_participant (?e ?p) 
  :constraint (and (#_endurant ?e)
                   (#_perdurant ?p))
:avoid-infinite-loop t)


(def-relation #_inhere-in (?q ?e)
  :iff-def (#_has-quality ?e ?q)
  :constraint (and (#_quality ?q) 
                   (#_entity ?e))
:avoid-infinite-loop t)


(def-relation #_has-quality (?e ?q)
  :iff-def (#_inhere-in ?q ?e)
  :constraint (and (#_quality ?q) 
                   (#_entity ?e))
:avoid-infinite-loop t)



(def-relation #_q-location-of (?r ?q)
  :constraint (and (#_quality ?q)
                   (#_region ?r))
  :iff-def (#_q-location ?q ?r))

(def-relation #_q-location (?q ?r)
  :constraint (and (#_quality ?q)
                   (#_region ?r))
  :iff-def (#_q-location-of ?r ?q))


(def-relation #_qt-location-of (?r ?qt)
  :constraint (and (#_quality-type ?qt)
                   (#_region ?r))
  :iff-def (#_qt-location ?qt ?r))

(def-relation #_q-location (?qt ?r)
  :constraint (and (#_quality-type ?qt)
                   (#_region ?r))
  :iff-def (#_qt-location-of ?r ?qt))

(def-relation #_q-belongs-to (?q ?qt)
  :constraint (and (#_quality-type ?qt)
                   (#_quality ?q)))

(def-rule quality-belongs-to 
  ((#_q-belongs-to ?q ?qt)
   if (#_q-location ?q ?r)
      (#_qt-location-of ?r ?qt)))

;;; actual d-s

(def-class #_description (#_endurant)
  ((#_d-uses :type #_concept :min-cardinality 2)))

(def-class #_situation (#_entity)
  ((#_setting-for :type #_entity :min-cardinality 1)
   (#_part-of :type #_situation)
   (#_satisfies :type #_description :min-cardinality 1)))


(def-class #_concept (#_endurant)
  ((#_classifies :type #_entity :min-cardinality 1)))

(def-class  #_course-of-event (#_concept)
  ((#_sequences :type #_perdurant :min-cardinality 1)))

(def-class #_functional-role (#_concept)
  ((#_played-by :type #_endurant :min-cardinality 1)))

(def-class #_parameter (#_concept)
  ((#_valued-by :type #_region :min-cardinality 1)))


(def-relation #_d-uses (?d ?c)
  :constraint (and (#_description ?d)
                   (#_concept ?c)))

(def-relation #_setting-for (?s ?e)
  :constraint (and (#_situation ?s)
                   (#_entity ?e)))

(def-relation #_satisfies (?s ?d)
  :constraint (and (#_situation ?s)
                   (#_description ?d)))
  ;;;define here what does it mean

(def-relation #_classifies (?c ?e)
  :constraint (and (#_concept ?c)
                   (#_entity ?e)))
  ;;;define here what does it mean

(def-relation #_sequences (?cof ?p)
  :constraint (and (#_course-of-event ?cof)
                   (#_perdurant ?p)))

(def-relation #_played-by (?fr ?e)
  :constraint (and (#_functional-role ?fr)
                   (#_endurant ?e)))

(def-relation #_valued-by (?p ?r)
  :constraint (and (#_parameter ?p)
                   (#_region ?r)))



;;; add relations between concepts of a description!


