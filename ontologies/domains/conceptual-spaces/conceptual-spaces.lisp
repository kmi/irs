;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology conceptual-spaces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; TOP LEVEL CONCEPTS ;;;;;;;;;;;;;
;;;;;;;; partially derived from d-s  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-class #_entity (#_d-s:entity)
((has-description :type string)))

(def-class #_region (#_d-s:region #_entity))

(def-class #_quality (#_d-s:quality #_entity)
"probably cs will refer to quality types (spaces and members) and quales (dimensions) exclusively in order to facilitate reusability of instances"
)

(def-class #_quale (#_d-s:quale #_entity))

(def-class #_quality-type (#_d-s:quality-type #_entity))

(def-class #_endurant (#_d-s:endurant #_entity))

(def-class #_perdurant (#_d-s:perdurant #_entity))

(def-class #_representation (#_d-s:description #_entity)
((represents :type (#_d-s:entity #_quality-dimension))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Top Level Relations ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-relation #_represented-by (?e ?r)
  :sufficient (and (#_representation ?r) 
                   ( represents ?r ?e)
                   (#_d-s:entity ?e)))

(def-relation #_valued-by (?qt ?q)
  :sufficient (and (#_quale ?q) 
                   ( values ?q ?qt)
                   (#_quality-type ?qt)))

(def-relation #_subspace-of (?ss ?s)
 "cs ss refining a quality dimension of another cs s is subspace of s"
  :sufficient (exists (?qd) (and (#_conceptual-space ?s) 
                                 (has-dimension ?s ?qd)
                                 (#_quality-dimension ?qd)
                                 (represents ?ss ?qd)
                                 (#_conceptual-space ?ss))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPACE AND MEMBER CONCEPTUALISATION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-class #_conceptual-space (#_representation)
((has-dimension :type #_quality-dimension :min-cardinality 1)))

#|
(def-class #_conceptual-space (#_conceptual-space) ?css
"a cs representing either a d-s situation or parameter"
:constraint (for-all (?e) (and (represents ?css ?e)
                                    (or (#_d-s:situation ?e)
                                    (#_d-s:concept ?e)))))
|#

(def-class #_member (#_representation #_quale) ?m
  ((member-in :type #_conceptual-space)
   (has-valued-dimension :type #_valued-dimension-vector :min-cardinality 1))
  :constraint (for-all (?cs ?vd ?d) (and (member-in ?m ?cs)
                                      (#_conceptual-space ?cs)
                                      (has-valued-dimension ?m ?vd)
                                      (#_valued-dimension-vector ?vd)
                                      ( values ?vd ?d)
                                      (#_quality-dimension ?d)
                                      (has-dimension ?cs ?d))))

(def-class #_prototypical-member (#_member))

(def-class #_quality-dimension (#_quality-type)
((has-metric-scale :type #_metric-scale-type :min-cardinality 1 :max-cardinality 1)
(has-id :type string)
(has-prominence :type float)))

(def-class #_valued-dimension-vector (#_quale)
(( values :type #_quality-dimension )))

(def-class #_metric-scale-type (#_entity)
((has-data-type :type string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SPACE AND MEMBER SPECIALIZATIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;metric types

(def-instance #_nominal-scale #_metric-scale-type 
"possible operations are equality/inequality. least likely to be used in a cs"
((has-data-type "String")
(has-description "Nominal-value based metric.")))


(def-instance #_ordinal-scale #_metric-scale-type
"ordered (ranked) sets. greater/less comparisons are valid" 
((has-data-type "String")
(has-description "Ordinal-value based metric.")))

(def-instance #_interval-scale #_metric-scale-type
"features of ordinal scale. but average and substraction meanigful as well."
((has-data-type "String")
(has-description "Interval based metric.")))

(def-instance #_ratio-scale #_metric-scale-type
"all features of interval scale, but non-arbitrary zero-point. addition, multiplication etc meaningful."
((has-data-type "Integer")
(has-description "Ratio (percentage) based metric.")))

;;;quality dimensions for certain metrics
(def-class #_ratio-quality-dimension (#_quality-dimension) ?rd
((has-metric-scale #_ratio-scale))
  :constraint (for-all (?vd) (and (#_valued-dimension-vector ?vd)
                                      ( values ?vd ?rd)
                                      (#_valued-ratio-dimension-vector ?vd))))

(def-class #_ordinal-quality-dimension (#_quality-dimension) ?od
((has-metric-scale #_ordinal-scale))
  :constraint (for-all (?vd) (and (#_valued-dimension-vector ?vd)
                                      ( values ?vd ?od)
                                      (#_valued-ordinal-dimension-vector ?vd))))

(def-class #_nominal-quality-dimension (#_quality-dimension) ?nd
((has-metric-scale #_nominal-scale))
  :constraint (for-all (?vd) (and (#_valued-dimension-vector ?vd)
                                      ( values ?vd ?nd)
                                      (#_valued-nominal-dimension-vector ?vd))))

(def-class #_interval-quality-dimension (#_quality-dimension) ?id
((has-metric-scale #_interval-scale))
  :constraint (for-all (?vd) (and (#_valued-dimension-vector ?vd)
                                      ( values ?vd ?id)
                                      (#_valued-interval-dimension-vector ?vd))))

;;; valued dimension vectors

(def-class #_valued-ratio-dimension-vector (#_valued-dimension-vector)
(( values :type #_ratio-quality-dimension)
(has-value :type float)))

(def-class #_valued-ordinal-dimension-vector (#_valued-dimension-vector)
((values :type #_ordinal-quality-dimension)
(has-value :type float)))

(def-class #_valued-nominal-dimension-vector (#_valued-dimension-vector) ?nv
(( values :type #_nominal-quality-dimension)
(has-value :type string)))

(def-class #_valued-interval-dimension-vector (#_valued-dimension-vector)
(( values :type #_interval-quality-dimension)
(has-value :type string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Example spaces/members: instances & subclasses ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; following subclasses/instances are still valid but not updated with respect to the recent ontological modifications.

;;; FSLSM instances/subclasses

(def-class #_fslsm-space (#_conceptual-space)
((has-title "Felder-Silverman-Learning-Style-Model-Space")
(has-description "Conceptual Space described on dimensions following the Felder Silverman Learning Style Model")
(has-dimension :type #_fslsm-quality-dimension)))

(def-class #_fslsm-member (#_member)
((member-in #_fslsm-space)
(has-valued-dimension :type #_fslsm-valued-dimension-vector)))

(def-class #_fslsm-prototypical-member (#_fslsm-member #_prototypical-member)
())

(def-class #_fslsm-quality-dimension (#_quality-dimension)
())

(def-class #_fslsm-valued-dimension-vector (#_valued-ordinal-dimension-vector)
(( values :type #_fslsm-quality-dimension)))

(def-instance #_fslsm-active-reflective-dimension #_fslsm-quality-dimension 
((has-title "Active-Reflective-Dimension")
(has-metric-scale #_interval-scale)
(has-id "a")
(has-prominence 1.5)))

(def-instance #_fslsm-sensing-intuitive-dimension #_fslsm-quality-dimension
((has-title "Sensing-Intuitive-Dimension")
(has-metric-scale #_interval-scale)
(has-id "b")
(has-prominence 1)))

(def-instance #_fslsm-sequential-global-dimension #_fslsm-quality-dimension 
((has-title "Sequential-Global-Dimension")
(has-metric-scale #_interval-scale)
(has-id "c")
(has-prominence 1)))

(def-instance #_fslsm-visual-verbal-dimension #_fslsm-quality-dimension
((has-title "Visual-Verbal-Dimension")
(has-metric-scale #_interval-scale)
(has-id "d")
(has-prominence 1.5)))

(def-instance #_fslsm-p1-active-visual-style #_fslsm-prototypical-member
((has-title "Active-Visual-Style")
(has-description "Prototype for rather active and visual learning styles.")
(member-in #_fslsm-space)
(has-valued-dimension (#_fslsm-p1-valued-ar-vector #_fslsm-p1-valued-si-vector #_fslsm-p1-valued-gs-vector #_fslsm-p1-valued-vv-vector))))

(def-instance #_fslsm-p1-valued-ar-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-active-reflective-dimension) 
(has-value -11)))

(def-instance #_fslsm-p1-valued-si-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sensing-intuitive-dimension) 
(has-value -11)))

(def-instance #_fslsm-p1-valued-gs-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sequential-global-dimension) 
(has-value 11)))

(def-instance #_fslsm-p1-valued-vv-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-visual-verbal-dimension) 
(has-value -11)))

(def-instance #_fslsm-p2-reflective-style #_fslsm-prototypical-member
((has-title "Reflective-Style")
(has-description "Prototype for rather reflective learning styles.")
(member-in #_fslsm-space)
(has-valued-dimension (#_fslsm-p2-valued-ar-vector #_fslsm-p2-valued-si-vector #_fslsm-p2-valued-gs-vector #_fslsm-p2-valued-vv-vector))))

(def-instance #_fslsm-p2-valued-ar-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-active-reflective-dimension) 
(has-value 11)))

(def-instance #_fslsm-p2-valued-si-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sensing-intuitive-dimension) 
(has-value -11)))

(def-instance #_fslsm-p2-valued-gs-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sequential-global-dimension) 
(has-value 0)))

(def-instance #_fslsm-p2-valued-vv-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-visual-verbal-dimension) 
(has-value -11)))

(def-instance #_fslsm-p3-sensing-sequential-style #_fslsm-prototypical-member
((has-title "Sensing-Sequential-Style")
(has-description "Prototype for rather sensing and sequential learning styles.")
(member-in #_fslsm-space)
(has-valued-dimension (#_fslsm-p3-valued-ar-vector #_fslsm-p3-valued-si-vector #_fslsm-p3-valued-gs-vector #_fslsm-p3-valued-vv-vector))))

(def-instance #_fslsm-p3-valued-ar-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-active-reflective-dimension) 
(has-value -11)))

(def-instance #_fslsm-p3-valued-si-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sensing-intuitive-dimension) 
(has-value -11)))

(def-instance #_fslsm-p3-valued-vv-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-visual-verbal-dimension) 
(has-value -11)))

(def-instance #_fslsm-p3-valued-gs-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sequential-global-dimension) 
(has-value -11)))

(def-instance #_fslsm-p4-intuitive-global-style #_fslsm-prototypical-member
((has-title "Intuitive-Global-Style")
(has-description "Prototype for rather intuitive and global learning styles.")
(member-in #_fslsm-space)
(has-valued-dimension (#_fslsm-p4-valued-ar-vector #_fslsm-p4-valued-si-vector #_fslsm-p4-valued-gs-vector #_fslsm-p4-valued-vv-vector))))

(def-instance #_fslsm-p4-valued-ar-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-active-reflective-dimension) 
(has-value -11)))

(def-instance #_fslsm-p4-valued-si-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sensing-intuitive-dimension) 
(has-value 11)))

(def-instance #_fslsm-p4-valued-vv-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-visual-verbal-dimension) 
(has-value -11)))

(def-instance #_fslsm-p4-valued-gs-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sequential-global-dimension) 
(has-value 11)))

(def-instance #_fslsm-p5-verbal-style #_fslsm-prototypical-member
((has-title "Verbal-Style")
(has-description "Prototype for rather verbal learning styles.")
(member-in #_fslsm-space)
(has-valued-dimension (#_fslsm-p5-valued-ar-vector #_fslsm-p5-valued-si-vector #_fslsm-p5-valued-gs-vector #_fslsm-p5-valued-vv-vector))))

(def-instance #_fslsm-p5-valued-ar-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-active-reflective-dimension) 
(has-value -11)))

(def-instance #_fslsm-p5-valued-si-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sensing-intuitive-dimension) 
(has-value 11)))

(def-instance #_fslsm-p5-valued-vv-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-visual-verbal-dimension) 
(has-value 11)))

(def-instance #_fslsm-p5-valued-gs-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sequential-global-dimension) 
(has-value 11)))

(def-instance #_fslsm-brad-p-style #_fslsm-member
((has-title "Brad P. FSLSM Vectors")
(has-description "FSLSM member describing Brad P. invidual Learning Style.")
(member-in #_fslsm-space)
(has-valued-dimension (#_fslsm-brad-valued-ar-vector #_fslsm-brad-valued-si-vector #_fslsm-brad-valued-gs-vector #_fslsm-brad-valued-vv-vector))))

(def-instance #_fslsm-brad-valued-ar-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-active-reflective-dimension) 
(has-value -5)))

(def-instance #_fslsm-brad-valued-si-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sensing-intuitive-dimension) 
(has-value -5)))

(def-instance #_fslsm-brad-valued-gs-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sequential-global-dimension) 
(has-value 3)))

(def-instance #_fslsm-brad-valued-vv-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-visual-verbal-dimension) 
(has-value -9)))

(def-instance #_fslsm-angie-j-style #_fslsm-member
((has-title "Angie J. FSLSM Vectors")
(has-description "FSLSM member describing Angie J. invidual Learning Style.")
(member-in #_fslsm-space)
(has-valued-dimension (#_fslsm-angie-valued-ar-vector #_fslsm-angie-valued-si-vector #_fslsm-angie-valued-gs-vector #_fslsm-angie-valued-vv-vector))))

(def-instance #_fslsm-angie-valued-ar-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-active-reflective-dimension) 
(has-value 5)))

(def-instance #_fslsm-angie-valued-si-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sensing-intuitive-dimension) 
(has-value -2)))

(def-instance #_fslsm-angie-valued-gs-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-sequential-global-dimension) 
(has-value -3)))

(def-instance #_fslsm-angie-valued-vv-vector #_fslsm-valued-dimension-vector
(( values #_fslsm-visual-verbal-dimension) 
(has-value 8)))

;;;; location space

(def-class #_location-space (#_conceptual-space)
((has-title "Location Space")
(has-description "Conceptual Space representing spatial location.")
(has-dimension :type #_location-quality-dimension)))

(def-class #_location-member (#_member)
((member-in #_location-space)
(has-valued-dimension :type #_location-valued-dimension-vector)))

(def-class #_location-prototypical-member (#_location-member #_prototypical-member)
())

(def-class #_location-quality-dimension (#_quality-dimension)
())

(def-class #_location-valued-dimension-vector (#_valued-ordinal-dimension-vector)
(( values :type #_location-quality-dimension)))

(def-instance #_latitude-dimension #_location-quality-dimension 
((has-title "Latitude-Dimension")
(has-metric-scale #_interval-scale)
(has-id "a")
(has-prominence 1)))

(def-instance #_longitude-dimension #_location-quality-dimension
((has-title "Longitude Dimension")
(has-metric-scale #_interval-scale)
(has-id "b")
(has-prominence 1)))

(def-instance #_p1-location-london #_location-prototypical-member
((has-title "Location-London")
(has-description "Prototype describing London")
(member-in #_location-space)
(has-valued-dimension (london-valued-lat-vector london-valued-long-vector))))

(def-instance #_london-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 51.500152)))

(def-instance #_london-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value -0.126236)))

(def-instance #_p2-location-brighton #_location-prototypical-member
((has-title "Location-Brighton")
(has-description "Prototype describing London")
(member-in #_location-space)
(has-valued-dimension (#_brighton-valued-lat-vector #_brighton-valued-long-vector))))

(def-instance #_brighton-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 50.820931)))

(def-instance #_brighton-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value -0.139846)))

(def-instance #_p3-location-milton-keynes #_location-prototypical-member
((has-title "Location-MK")
(has-description "Prototype describing Milton Keynes")
(member-in #_location-space)
(has-valued-dimension (#_milton-keynes-valued-lat-vector #_milton-keynes-valued-long-vector))))

(def-instance #_milton-keynes-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 52.044041)))

(def-instance #_milton-keynes-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value -0.699569)))

(def-instance #_p4-location-paris #_location-prototypical-member
((has-title "Location-Paris")
(has-description "Prototype describing Paris")
(member-in #_location-space)
(has-valued-dimension (#_paris-valued-lat-vector #_paris-valued-long-vector))))

(def-instance #_paris-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 48.856667)))

(def-instance #_paris-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value 2.350987)))

(def-instance #_p5-location-toulouse #_location-prototypical-member
((has-title "Location-Toulouse")
(has-description "Prototype describing Toulouse")
(member-in #_location-space)
(has-valued-dimension (#_toulouse-valued-lat-vector #_toulouse-valued-long-vector))))

(def-instance #_toulouse-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 43.604363)))

(def-instance #_toulouse-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value 1.442951)))

(def-instance #_p6-location-uk #_location-prototypical-member
((has-title "Location-UK")
(has-description "Prototype describing UK")
(member-in #_location-space)
(has-valued-dimension (#_uk-valued-lat-vector #_uk-valued-long-vector))))

(def-instance #_uk-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 55.378051)))

(def-instance #_uk-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value -3.435973)))

(def-instance #_p6-1-location-uk #_location-prototypical-member
((has-title "Location-UK")
(has-description "Prototype describing UK")
(member-in #_location-space)
(has-valued-dimension (#_1-uk-valued-lat-vector #_1-uk-valued-long-vector))))

(def-instance #_1-uk-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 55.378048)))

(def-instance #_1-uk-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value -3.435963)))

(def-instance #_p7-location-france #_location-prototypical-member
((has-title "Location-France")
(has-description "Prototype describing France")
(member-in #_location-space)
(has-valued-dimension (#_france-valued-lat-vector #_france-valued-long-vector))))

(def-instance #_france-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 46.227638)))

(def-instance #_france-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value 2.213749)))

(def-instance #_p7-1-location-france #_location-prototypical-member
((has-title "Location-France")
(has-description "Prototype describing France")
(member-in #_location-space)
(has-valued-dimension (#_1-france-valued-lat-vector #_1-france-valued-long-vector))))

(def-instance #_1-france-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 46.227644)))

(def-instance #_1-france-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value 2.213755)))

(def-instance #_p8-location-germany #_location-prototypical-member
((has-title "Location-Germany")
(has-description "Prototype describing Germany")
(member-in #_location-space)
(has-valued-dimension (#_germany-valued-lat-vector #_germany-valued-long-vector))))

(def-instance #_germany-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 51.165691)))

(def-instance #_germany-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value 10.451526)))

(def-instance #_p8-1-location-germany #_location-prototypical-member
((has-title "Location-Germany")
(has-description "Prototype describing Germany")
(member-in #_location-space)
(has-valued-dimension (#_1-germany-valued-lat-vector #_1-germany-valued-long-vector))))

(def-instance #_1-germany-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 51.165687)))

(def-instance #_1-germany-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value 10.451531)))

(def-instance #_p9-location-spain #_location-prototypical-member
((has-title "Location-Spain")
(has-description "Prototype describing Spain")
(member-in #_location-space)
(has-valued-dimension (#_spain-valued-lat-vector #_spain-valued-long-vector))))

(def-instance #_spain-valued-lat-vector #_location-valued-dimension-vector
(( values #_latitude-dimension) 
(has-value 40.463667)))

(def-instance #_spain-valued-long-vector #_location-valued-dimension-vector
(( values #_longitude-dimension) 
(has-value -3.74922)))

;;; price space

(def-class #_price-space (#_conceptual-space)
((has-title "Price Space")
(has-description "Conceptual Space representing price in terms of contrast currency exchange rates.")
(has-dimension :type #_price-quality-dimension)))

(def-class #_price-member (#_member)
((member-in #_price-space)
(has-valued-dimension :type #_price-valued-dimension-vector)))

(def-class #_price-prototypical-member (#_price-member #_prototypical-member)
())

(def-class #_price-quality-dimension (#_quality-dimension)
())

(def-class #_price-valued-dimension-vector (#_valued-interval-dimension-vector)
(( values :type #_price-quality-dimension)))

(def-instance #_euro-rate-dimension #_price-quality-dimension 
((has-title "Euro-Rate-Dimension")
(has-metric-scale #_interval-scale)
(has-id "a")
(has-prominence 1)))

(def-instance #_dollar-rate-dimension #_price-quality-dimension
((has-title "Dollar Rate Dimension")
(has-metric-scale #_interval-scale)
(has-id "b")
(has-prominence 1)))

(def-instance #_gbp-rate-dimension #_price-quality-dimension
((has-title "GBP Rate Dimension")
(has-metric-scale #_interval-scale)
(has-id "c")
(has-prominence 1)))

(def-instance #_c1-1-euro-price #_price-prototypical-member
((has-title "1 Euro Price")
(has-description "1 Euro Price")
(member-in #_price-space)
(has-valued-dimension (#_euro-valued-euro-vector #_euro-valued-dollar-vector #_euro-valued-gbp-vector))))

(def-instance #_euro-valued-euro-vector #_price-valued-dimension-vector
(( values #_euro-rate-dimension) 
(has-value 1)))

(def-instance #_euro-valued-dollar-vector #_price-valued-dimension-vector
(( values #_dollar-rate-dimension) 
(has-value 1.34259)))

(def-instance #_euro-valued-gbp-vector #_price-valued-dimension-vector
(( values #_gbp-rate-dimension) 
(has-value 0.77694)))

(def-instance #_c2-1-dollar-price #_price-prototypical-member
((has-title "1 Dollar Price")
(has-description "1 Dollar Price")
(member-in #_price-space)
(has-valued-dimension (#_dollar-valued-euro-vector #_dollar-valued-dollar-vector #_dollar-valued-gbp-vector))))

(def-instance #_dollar-valued-euro-vector #_price-valued-dimension-vector
(( values #_euro-rate-dimension) 
(has-value 0.74306)))

(def-instance #_dollar-valued-dollar-vector #_price-valued-dimension-vector
(( values #_dollar-rate-dimension) 
(has-value 1)))

(def-instance #_dollar-valued-gbp-vector #_price-valued-dimension-vector
(( values #_gbp-rate-dimension) 
(has-value 0.57782)))

(def-instance #_c3-1-gbp-price #_price-prototypical-member
((has-title "1 GBP Price")
(has-description "1 GBP Price")
(member-in #_price-space)
(has-valued-dimension (#_gbp-valued-euro-vector #_gbp-valued-dollar-vector #_gbp-valued-gbp-vector))))

(def-instance #_gbp-valued-euro-vector #_price-valued-dimension-vector
(( values #_euro-rate-dimension) 
(has-value 1.28529)))

(def-instance #_gbp-valued-dollar-vector #_price-valued-dimension-vector
(( values #_dollar-rate-dimension) 
(has-value 1.73107)))

(def-instance #_gbp-valued-gbp-vector #_price-valued-dimension-vector
(( values #_gbp-rate-dimension) 
(has-value 1)))


;;; temporal quality space
;;; space reprsenting temporal qualities (dates, times...)

(def-class #_temporal-quality-space (#_conceptual-space)
((has-title "Temporal Quality Space")
(has-description "Conceptual Space representing temporal qualities, i.e. dates, times...")
(has-dimension :type #_temporal-quality-quality-dimension)))

(def-class #_temporal-quality-member (#_member)
((member-in #_temporal-quality-space)
(has-valued-dimension :type #_temporal-quality-valued-dimension-vector)))

(def-class #_temporal-quality-prototypical-member (#_temporal-quality-member #_prototypical-member)
())

(def-class #_temporal-quality-quality-dimension (#_quality-dimension)
())

(def-class #_temporal-quality-valued-dimension-vector (#_valued-interval-dimension-vector)
(( values :type #_temporal-quality-quality-dimension)))

(def-instance #_tq-year-dimension #_temporal-quality-quality-dimension 
((has-title "Year-Dimension")
(has-metric-scale #_interval-scale)
(has-id "a")
(has-prominence 1)))

(def-instance #_tq-month-dimension #_temporal-quality-quality-dimension 
((has-title "Month-Dimension")
(has-metric-scale #_interval-scale)
(has-id "b")
(has-prominence 1)))

(def-instance #_tq-day-dimension #_temporal-quality-quality-dimension 
((has-title "Day-Dimension")
(has-metric-scale #_interval-scale)
(has-id "c")
(has-prominence 1)))

(def-instance #_tq-hour-dimension #_temporal-quality-quality-dimension 
((has-title "Hours-Dimension")
(has-metric-scale #_interval-scale)
(has-id "d")
(has-prominence 1)))

(def-instance #_tq-minute-dimension #_temporal-quality-quality-dimension 
((has-title "Minutes-Dimension")
(has-metric-scale #_interval-scale)
(has-id "e")
(has-prominence 1)))

(def-instance #_tq-second-dimension #_temporal-quality-quality-dimension 
((has-title "Seconds-Dimension")
(has-metric-scale #_interval-scale)
(has-id "f")
(has-prominence 1)))

(def-instance #_tq-1-example-date #_temporal-quality-member
((has-title "a-particular-date")
(has-description "A particular date.")
(member-in #_temporal-quality-space)
(has-valued-dimension (tq-1-year-vector tq-1-month-vector tq-1-day-vector tq-1-hour-vector tq-1-minute-vector tq-1-second-vector))))

(def-instance #_tq-1-year-vector #_temporal-quality-valued-dimension-vector
(( values #_tq-year-dimension) 
(has-value 2008)))

(def-instance #_tq-1-month-vector #_temporal-quality-valued-dimension-vector
(( values #_tq-month-dimension) 
(has-value 6)))

(def-instance #_tq-1-day-vector #_temporal-quality-valued-dimension-vector
(( values #_tq-day-dimension) 
(has-value 25)))

(def-instance #_tq-1-hour-vector #_temporal-quality-valued-dimension-vector
(( values #_tq-hour-dimension) 
(has-value 20)))

(def-instance #_tq-1-minute-vector #_temporal-quality-valued-dimension-vector
(( values #_tq-minute-dimension) 
(has-value 47)))

(def-instance #_tq-1-second-vector #_temporal-quality-valued-dimension-vector
(( values #_tq-second-dimension) 
(has-value 28)))

;;; representation space, possibly also "format space"
;;; is supposed to represent the format of a certain representation/concept value, for instance a date format

(def-class #_format-space (#_conceptual-space)
((has-title "Input/Output Representation Format Space")
(has-description "Conceptual Space representing I/O message part formats in terms of stakeholders, positions, separators.")
(has-dimension :type #_format-quality-dimension)
(formats :type #_conceptual-space)))
;;; should be constrained stating that a format space can not format format-spaces (just CS)

(def-class #_format-member (#_member)
((member-in #_format-space)
(has-valued-dimension :type (or (#_format-valued-dimension-vector #_format-valued-nominal-dimension-vector)))))

(def-class #_format-prototypical-member (#_format-member #_prototypical-member)
())

(def-class #_format-quality-dimension (#_quality-dimension)
())

(def-class #_format-valued-dimension-vector (#_valued-dimension-vector)
())

(def-class #_format-valued-interval-dimension-vector (#_valued-interval-dimension-vector #_format-valued-dimension-vector)
(( values :type #_format-quality-dimension)))

(def-class #_format-valued-nominal-dimension-vector (#_valued-nominal-dimension-vector #_format-valued-dimension-vector)
(( values :type #_format-quality-dimension)))

;;; price-format-space
(def-class #_price-format-space (#_format-space)
((has-title "Price Format Space")
(has-description "Conceptual Space representing price formats.")
(has-dimension :type #_price-format-quality-dimension)
(formats :type #_price-space)))

(def-class #_price-format-member (#_member)
((member-in #_price-format-space)
(has-valued-dimension :type #_price-format-valued-dimension-vector)))

(def-class #_price-format-prototypical-member (#_price-format-member #_format-prototypical-member)
())

(def-class #_price-format-quality-dimension (#_format-quality-dimension)
())

(def-class #_price-format-valued-dimension-vector (#_format-valued-dimension-vector)
())

(def-class #_price-format-valued-interval-dimension-vector (#_format-valued-interval-dimension-vector)
(( values :type #_price-format-quality-dimension)))

(def-class #_price-format-valued-nominal-dimension-vector (#_format-valued-nominal-dimension-vector)
(( values :type #_price-format-quality-dimension)))

;;; following position dimensions could reflect the element number (i.e. first, second...) or the actual position in the string
;;; the latter would enable that the position also reflects the length of the entire element.
;;; undefined dimension or value of "0" means it does not occur at all
;;; in principle there could be several dimensions for each element (e.g. year) reflecting the length, the separator, ... but for simplification I go for the solution as followed

(def-instance #_price-format-euro-position-dimension #_price-format-quality-dimension 
((has-title "Euro-Position-Dimension")
(has-metric-scale #_interval-scale)
(has-id "a")
(has-prominence 1)))

(def-instance #_price-format-dollar-position-dimension #_price-format-quality-dimension 
((has-title "Dollar-Position-Dimension")
(has-metric-scale #_interval-scale)
(has-id "b")
(has-prominence 1)))

(def-instance #_price-format-gbp-position-dimension #_price-format-quality-dimension 
((has-title "GBP-Position-Dimension")
(has-metric-scale #_interval-scale)
(has-id "c")
(has-prominence 1)))


;;; instance defines the format of a euro-representation

(def-instance #_pr1-euro-price-format #_price-format-member
((has-title "Euro Format")
(has-description "Euro Format")
(member-in #_price-format-space)
(has-valued-dimension (#_pr1-euro-position-vector #_pr1-dollar-position-vector #_pr1-gbp-position-vector))))

(def-instance #_pr1-euro-position-vector #_price-format-valued-interval-dimension-vector
(( values #_price-format-euro-position-dimension) 
(has-value 1)))

(def-instance #_pr1-dollar-position-vector #_price-format-valued-interval-dimension-vector
(( values #_price-format-dollar-position-dimension) 
(has-value 0)))

(def-instance #_pr1-gbp-position-vector #_price-format-valued-interval-dimension-vector
(( values #_price-format-gbp-position-dimension) 
(has-value 0)))

(def-instance #_pr2-gbp-price-format #_price-format-member
((has-title "GBP Format")
(has-description "GBP Format")
(member-in #_price-format-space)
(has-valued-dimension (#_pr2-euro-position-vector #_pr2-dollar-position-vector #_pr2-gbp-position-vector))))

(def-instance #_pr2-euro-position-vector #_price-format-valued-interval-dimension-vector
(( values #_price-format-euro-position-dimension) 
(has-value 0)))

(def-instance #_pr2-dollar-position-vector #_price-format-valued-interval-dimension-vector
(( values #_price-format-dollar-position-dimension) 
(has-value 0)))

(def-instance #_pr2-gbp-position-vector #_price-format-valued-interval-dimension-vector
(( values #_price-format-gbp-position-dimension) 
(has-value 1)))

(def-instance #_pr3-dollar-price-format #_price-format-member
((has-title "Dollar Format")
(has-description "Dollar Format")
(member-in #_price-format-space)
(has-valued-dimension (#_pr3-euro-position-vector #_pr3-dollar-position-vector #_pr3-gbp-position-vector))))

(def-instance #_pr3-euro-position-vector #_price-format-valued-interval-dimension-vector
(( values #_price-format-euro-position-dimension) 
(has-value 0)))

(def-instance #_pr3-dollar-position-vector #_price-format-valued-interval-dimension-vector
(( values #_price-format-dollar-position-dimension) 
(has-value 1)))

(def-instance #_pr3-gbp-position-vector #_price-format-valued-interval-dimension-vector
(( values #_price-format-euro-position-dimension) 
(has-value 0)))

;;;; date-format-space

(def-class #_date-format-space (#_format-space)
((has-title "Date Format Space")
(has-description "Conceptual Space representing date formats.")
(has-dimension :type #_date-format-quality-dimension)
(formats :type #_temporal-quality-space)))

(def-class #_date-format-member (#_member)
((member-in #_date-format-space)
(has-valued-dimension :type #_date-format-valued-dimension-vector)))

(def-class #_date-format-prototypical-member (#_date-format-member #_format-prototypical-member)
())

(def-class #_date-format-quality-dimension (#_format-quality-dimension)
())

(def-class #_date-format-valued-dimension-vector (#_format-valued-dimension-vector)
())

(def-class #_date-format-valued-interval-dimension-vector (#_format-valued-interval-dimension-vector)
(( values :type #_date-format-quality-dimension)))

(def-class #_date-format-valued-nominal-dimension-vector (#_format-valued-nominal-dimension-vector)
(( values :type #_date-format-quality-dimension)))

;;; following position dimensions could reflect the element number (i.e. first, second...) or the actual position in the string
;;; the latter would enable that the position also reflects the length of the entire element.
;;; undefined dimension or value of "0" means it does not occur at all
;;; in principle there could be several dimensions for each element (e.g. year) reflecting the length, the separator, ... but for simplification I go for the solution as followed

(def-instance #_date-format-year-position-dimension #_date-format-quality-dimension 
((has-title "Year-Position-Dimension")
(has-metric-scale #_interval-scale)
(has-id "a")
(has-prominence 1)))

(def-instance #_date-format-month-position-dimension #_date-format-quality-dimension 
((has-title "Month-Position-Dimension")
(has-metric-scale #_interval-scale)
(has-id "b")
(has-prominence 1)))

(def-instance #_date-format-day-position-dimension #_date-format-quality-dimension 
((has-title "Day-Position-Dimension")
(has-metric-scale #_interval-scale)
(has-id "c")
(has-prominence 1)))

(def-instance #_date-format-hour-position-dimension #_date-format-quality-dimension 
((has-title "Hour-Position-Dimension")
(has-metric-scale #_interval-scale)
(has-id "d")
(has-prominence 1)))

(def-instance #_date-format-minute-position-dimension #_date-format-quality-dimension 
((has-title "Minute-Position-Dimension")
(has-metric-scale #_interval-scale)
(has-id "e")
(has-prominence 1)))

(def-instance #_date-format-second-position-dimension #_date-format-quality-dimension 
((has-title "Second-Position-Dimension")
(has-metric-scale #_interval-scale)
(has-id "f")
(has-prominence 1)))

(def-instance #_date-format-length-dimension #_date-format-quality-dimension 
((has-title "Length-Dimension")
(has-metric-scale #_interval-scale)
(has-id "h")
(has-prominence 1)))

(def-instance #_date-format-separator-dimension #_date-format-quality-dimension 
((has-title "Second-Position-Dimension")
(has-metric-scale #_nominal-scale)
(has-id "g")
(has-prominence 1)))

;;; following instance defines the date format "dd/mm/yyyy" 

(def-instance #_df1-date-format #_date-format-member
((has-title "dd/mm/yyyy")
(has-description "dd/mm/yyyy-format")
(member-in #_date-format-space)
(has-valued-dimension (#_df1-year-position-vector #_df1-month-position-vector #_df1-day-position-vector #_df1-separator-vector #_df1-length-vector))))

(def-instance #_df1-year-position-vector #_date-format-valued-interval-dimension-vector
(( values #_date-format-year-position-dimension) 
(has-value 7)))

(def-instance #_df1-month-position-vector #_date-format-valued-interval-dimension-vector
(( values #_date-format-month-position-dimension) 
(has-value 4)))

(def-instance #_df1-day-position-vector #_date-format-valued-interval-dimension-vector
(( values #_date-format-day-position-dimension) 
(has-value 1)))

;;; probably the other positions ("0") need to be assigned as well in order to produce meaningful vectors

(def-instance #_df1-separator-vector #_date-format-valued-nominal-dimension-vector
(( values #_date-format-separator-dimension) 
(has-value "/")))

(def-instance #_df1-length-vector #_date-format-valued-interval-dimension-vector
(( values #_date-format-length-dimension) 
(has-value 10)))

;;;; aim space

(def-class #_aim-space (#_conceptual-space)
((has-title "Aim Space")
(has-description "Conceptual Space representing Mobile Aims.")
(has-dimension :type #_aim-quality-dimension)))

(def-class #_aim-member (#_member)
((member-in #_aim-space)
(has-valued-dimension :type #_aim-valued-dimension-vector)))

(def-class #_aim-prototypical-member (#_aim-member #_prototypical-member)
())

(def-class #_aim-quality-dimension (#_quality-dimension)
())

(def-class #_aim-valued-dimension-vector (#_valued-ratio-dimension-vector)
(( values :type #_aim-quality-dimension)))

(def-instance #_culture-dimension #_aim-quality-dimension 
((has-title "Culture Dimension")
(has-metric-scale #_ratio-scale)
(has-id "a")
(has-prominence 1)))

(def-instance #_languages-dimension #_aim-quality-dimension
((has-title "Languages & Linguistics Dimension")
(has-metric-scale #_ratio-scale)
(has-id "b")
(has-prominence 1)))

(def-instance #_geography-dimension #_aim-quality-dimension 
((has-title "Geography Dimension")
(has-metric-scale #_ratio-scale)
(has-id "c")
(has-prominence 1)))

(def-instance #_history-dimension #_aim-quality-dimension
((has-title "History Dimension")
(has-metric-scale #_ratio-scale)
(has-id "d")
(has-prominence 1)))

(def-instance #_p1-language-aim #_aim-prototypical-member
((has-title "Languages-and-Linguistics-Aim")
(has-description "Prototypical member describing the languages aim.")
(member-in #_aim-space)
(has-valued-dimension (#_language-valued-history-vector #_language-valued-culture-vector #_language-valued-language-vector #_language-valued-geography-vector))))

(def-instance #_language-valued-history-vector #_aim-valued-dimension-vector
(( values #_history-dimension) 
(has-value 0)))

(def-instance #_language-valued-culture-vector #_aim-valued-dimension-vector
(( values #_culture-dimension) 
(has-value 0)))

(def-instance #_language-valued-language-vector #_aim-valued-dimension-vector
(( values #_languages-dimension) 
(has-value 100)))

(def-instance #_language-valued-geography-vector #_aim-valued-dimension-vector
(( values #_geography-dimension) 
(has-value 0)))

(def-instance #_p2-history-aim #_aim-prototypical-member
((has-title "History-Aim")
(has-description "Prototypical member describing the history aim.")
(member-in #_aim-space)
(has-valued-dimension (#_history-valued-history-vector #_history-valued-culture-vector #_history-valued-language-vector #_history-valued-geography-vector))))

(def-instance #_history-valued-history-vector #_aim-valued-dimension-vector
(( values #_history-dimension) 
(has-value 100)))

(def-instance #_history-valued-culture-vector #_aim-valued-dimension-vector
(( values #_culture-dimension) 
(has-value 0)))

(def-instance #_history-valued-language-vector #_aim-valued-dimension-vector
(( values #_languages-dimension) 
(has-value 0)))

(def-instance #_history-valued-geography-vector #_aim-valued-dimension-vector
(( values #_geography-dimension) 
(has-value 0)))

(def-instance #_p3-geography-aim #_aim-prototypical-member
((has-title "Geography-Aim")
(has-description "Prototypical member describing the geography aim.")
(member-in #_aim-space)
(has-valued-dimension (#_geography-valued-history-vector #_geography-valued-culture-vector #_geography-valued-language-vector #_geography-valued-geography-vector))))

(def-instance #_geography-valued-history-vector #_aim-valued-dimension-vector
(( values #_history-dimension) 
(has-value 0)))

(def-instance #_geography-valued-culture-vector #_aim-valued-dimension-vector
(( values #_culture-dimension) 
(has-value 0)))

(def-instance #_geography-valued-language-vector #_aim-valued-dimension-vector
(( values #_languages-dimension) 
(has-value 0)))

(def-instance #_geography-valued-geography-vector #_aim-valued-dimension-vector
(( values #_geography-dimension) 
(has-value 100)))

(def-instance #_p4-culture-aim #_aim-prototypical-member
((has-title "Culture-Aim")
(has-description "Prototypical member describing the culture aim.")
(member-in #_aim-space)
(has-valued-dimension (#_culture-valued-history-vector #_culture-valued-culture-vector #_culture-valued-language-vector #_culture-valued-geography-vector))))

(def-instance #_culture-valued-history-vector #_aim-valued-dimension-vector
(( values #_history-dimension) 
(has-value 0)))

(def-instance #_culture-valued-culture-vector #_aim-valued-dimension-vector
(( values #_culture-dimension) 
(has-value 100)))

(def-instance #_culture-valued-language-vector #_aim-valued-dimension-vector
(( values #_languages-dimension) 
(has-value 0)))

(def-instance #_culture-valued-geography-vector #_aim-valued-dimension-vector
(( values #_geography-dimension) 
(has-value 0)))

(def-instance #_a1-language-culture-aim #_aim-member
((has-title "Language/Culture-Aim")
(has-description "Member describing the language/culture aim.")
(member-in #_aim-space)
(has-valued-dimension (#_a1-valued-history-vector #_a1-valued-culture-vector #_a1-valued-language-vector #_a1-valued-geography-vector))))

(def-instance #_a1-valued-history-vector #_aim-valued-dimension-vector
(( values #_history-dimension) 
(has-value 0)))

(def-instance #_a1-valued-culture-vector #_aim-valued-dimension-vector
(( values #_culture-dimension) 
(has-value 30)))

(def-instance #_a1-valued-language-vector #_aim-valued-dimension-vector
(( values #_languages-dimension) 
(has-value 70)))

(def-instance #_a1-valued-geography-vector #_aim-valued-dimension-vector
(( values #_geography-dimension) 
(has-value 0)))

(def-instance #_a2-history-culture-aim #_aim-member
((has-title "History/Culture-Aim")
(has-description "Member describing the history/culture aim.")
(member-in #_aim-space)
(has-valued-dimension (#_a2-valued-history-vector #_a2-valued-culture-vector #_a2-valued-language-vector #_a2-valued-geography-vector))))

(def-instance #_a2-valued-history-vector #_aim-valued-dimension-vector
(( values #_history-dimension) 
(has-value 70)))

(def-instance #_a2-valued-culture-vector #_aim-valued-dimension-vector
(( values #_culture-dimension) 
(has-value 30)))

(def-instance #_a2-valued-language-vector #_aim-valued-dimension-vector
(( values #_languages-dimension) 
(has-value 0)))

(def-instance #_a2-valued-geography-vector #_aim-valued-dimension-vector
(( values #_geography-dimension) 
(has-value 0)))

(def-instance #_a3-geography-history-aim #_aim-member
((has-title "Geography/History-Aim")
(has-description "Member describing the geography/history aim.")
(member-in #_aim-space)
(has-valued-dimension (#_a3-valued-history-vector #_a3-valued-culture-vector #_a3-valued-language-vector #_a3-valued-geography-vector))))

(def-instance #_a3-valued-history-vector #_aim-valued-dimension-vector
(( values #_history-dimension) 
(has-value 30)))

(def-instance #_a3-valued-culture-vector #_aim-valued-dimension-vector
(( values #_culture-dimension) 
(has-value 0)))

(def-instance #_a3-valued-language-vector #_aim-valued-dimension-vector
(( values #_languages-dimension) 
(has-value 0)))

(def-instance #_a3-valued-geography-vector #_aim-valued-dimension-vector
(( values #_geography-dimension) 
(has-value 70)))

;;;; interaction space

(def-class #_interaction-space (#_conceptual-space)
((has-title "Interaction Space")
(has-description "Conceptual Space representing interactivity types.")
(has-dimension :type #_interaction-quality-dimension)))

(def-class #_interaction-member (#_member)
((member-in #_interaction-space)
(has-valued-dimension :type #_interaction-valued-dimension-vector)))

(def-class #_interaction-prototypical-member (#_interaction-member #_prototypical-member)
())

(def-class #_interaction-quality-dimension (#_quality-dimension)
())

(def-class #_interaction-valued-dimension-vector (#_valued-ordinal-dimension-vector)
(( values :type #_interaction-quality-dimension)))

(def-instance #_non-interactivity-dimension #_interaction-quality-dimension 
((has-title "Non-Interactivity Dimension")
(has-metric-scale #_ratio-scale)
(has-id "a")
(has-prominence 1)))

(def-instance #_interactivity-dimension #_interaction-quality-dimension
((has-title "Interactivity Dimension")
(has-metric-scale #_ratio-scale)
(has-id "b")
(has-prominence 1)))

(def-instance #_p1-interactive-prototype #_interaction-prototypical-member
((has-title "Interactive")
(has-description "Prototype describing non-interactive participation.")
(member-in #_interaction-space)
(has-valued-dimension (#_p1-interactive-vector #_p1-non-interactive-vector))))

(def-instance #_p1-interactive-vector #_interaction-valued-dimension-vector
(( values #_interactivity-dimension) 
(has-value 100)))

(def-instance #_p1-non-interactive-vector #_interaction-valued-dimension-vector
(( values #_non-interactivity-dimension) 
(has-value 0)))

(def-instance #_p2-non-interactive-prototype #_interaction-prototypical-member
((has-title "Non-Interactive")
(has-description "Prototype describing non-interactive participation.")
(member-in #_interaction-space)
(has-valued-dimension (#_p2-interactive-vector #_p2-non-interactive-vector))))

(def-instance #_p2-interactive-vector #_interaction-valued-dimension-vector
(( values #_interactivity-dimension) 
(has-value 0)))

(def-instance #_p2-non-interactive-vector #_interaction-valued-dimension-vector
(( values #_non-interactivity-dimension) 
(has-value 100)))

(def-instance #_i1-brad-p-interaction-member #_interaction-member
((has-title "BradInteractivity")
(has-description "Member describing Brad P.'s interactivity")
(member-in #_interaction-space)
(has-valued-dimension (#_i1-interactive-vector #_i1-non-interactive-vector))))

(def-instance #_i1-interactive-vector #_interaction-valued-dimension-vector
(( values #_interactivity-dimension) 
(has-value 80)))

(def-instance #_i1-non-interactive-vector #_interaction-valued-dimension-vector
(( values #_non-interactivity-dimension) 
(has-value 20)))

(def-instance #_i2-angie-interaction-member #_interaction-member
((has-title "AngieInteractivity")
(has-description "Member describing Angie J.'s interactivity")
(member-in #_interaction-space)
(has-valued-dimension (#_i2-interactive-vector #_i2-non-interactive-vector))))

(def-instance #_i2-interactive-vector #_interaction-valued-dimension-vector
(( values #_interactivity-dimension) 
(has-value 35)))

(def-instance #_i2-non-interactive-vector #_interaction-valued-dimension-vector
(( values #_non-interactivity-dimension) 
(has-value 65)))

#|
(deflower lower-css-dimension css-quality-dimension
'(("Dimension"
   (("Title" "has-title")
    ("Id" "has-id")
    ("Prominence" "has-prominence")
    ("MetricScale" "has-metric-scale")))))

(def-class css-quality-dimension-list (list))

(deflower lower-css-dimensions css-quality-dimension-list
  (lambda (qd)
    (let ((res ""))
     (loop for x in qd do         
        (setf l (lower-css-dimension x))
        (setf res (concatenate 'string res l)))
     res)))

(deflower lower-css css-conceptual-space 
'(("ConceptualSpace"
   ((lower-css-dimensions "has-dimension")))))



(def-class css-prototypical-member-list (list))

(deflower lower-css-prototypes css-prototypical-member-list
  (lambda (pml)
    (let ((res "<Prototypes>")
          (l nil))
     (loop for x in pml do         
        (setf l (lower-css-member x))
        (setf res (concatenate 'string res l)))
     (setf res (concatenate 'string res "</Prototypes>"))))) 

(deflower lower-css-member css-member
'(("Member"
   (("Title" "has-title")
    (lower-css-vector-list "has-valued-dimension")))))

(def-class css-vector-list (list))

(deflower lower-css-vector-list css-vector-list
  (lambda (vl)
    (let ((res ""))
     (loop for x in vl do         
        (setf l (lower-css-vector x))
        (setf res (concatenate 'string res l)))
     res)))


(deflower lower-css-vector css-valued-dimension-vector
'(("Vector"
   ((lower-css-dimension-id "values") 
    ("Value" "has-value")))))

(deflower lower-css-dimension-id css-quality-dimension ;;; alternative to lower-css-dimension in the previous lowering
'(("Dimension"
   (("Id" "has-id")))))


(def-class distance-result ()
  ((has-string :type string)))

(deflift lift-distance-result distance-result ()
  (("has-string" "getDistancesResponse/getDistancesReturn/text()")))

(deflower lower-distance-result distance-result 
  (lambda (dr)
    (ocml::the-slot-value dr 'has-string)))

;(lower-css-dimension-id 'languages-dimension)
;(describe-instance 'ocml::fslsm-brad-p-style)
;(lower-css-member 'ocml::fslsm-brad-p-style)

;(lower-css-vector-list '(FSLSM-BRAD-VALUED-AR-VECTOR FSLSM-BRAD-VALUED-SI-VECTOR FSLSM-BRAD-VALUED-GS-VECTOR FSLSM-BRAD-VALUED-VV-VECTOR))
;(lower-css-vector 'FSLSM-BRAD-VALUED-gs-VECTOR)
;(describe-instance 'FSLSM-SEQUENTIAL-GLOBAL-DIMENSION) 
;(lower-css-dimension-id 'FSLSM-SEQUENTIAL-GLOBAL-DIMENSION)

|#






;;;;;;;;;;;;;;;;;;


;;; maybe creating subclasses of a member and quality dimension which can be constrained.
#|
;;; following is a try to subclass a member for a specific space to constrain it with correlation functions such as the one shown below.
;;; could be used in case we would like to go for corellated quality dimensions

(def-class fslsm-member (css-member) ?fm
((member-in fslsm-space))
:constraint (exists (?r ?s ?t ?u ?x ?y)
                    (and (has-valued-dimension ?fm ?r)
                         (css-valued-ratio-dimension-vector ?r)
                         (values ?r ?s)
                         (fslsm-active-dimension ?s)
                         (has-value ?r ?x  )
                         (has-valued-dimension ?fm ?t)
                         (css-valued-ratio-dimension-vector ?t)
                         (values ?t ?u)
                         (fslsm-reflective-dimension ?u)
                         (has-value ?t ?y)
                         ((== 100 (+ ?x ?y))))))

(def-class fslsm-prototypical-member (fslsm-member)
())

(def-instance fslsm-active-dimension css-quality-dimension 
((has-title "Active Dimension")
(has-metric-scale css-ratio-scale)))

(def-instance fslsm-reflective-dimension css-quality-dimension
((has-title "Reflective Dimension")
(has-metric-scale css-ratio-scale)))
|#
