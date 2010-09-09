;; Mode: Lisp; Package: ocml

;;; File created in WebOnto
 
(in-package "OCML")

(in-ontology sgis-spatial)

(def-class input-roles-adapter ()
  ((has-mapping :TYPE STRING )))

(def-class spatial-affordance ()
  ((has-goal :type goal-meta-class)
   (has-max-range-km :type float :default-value 0)
   (has-input-roles-adapter :type input-roles-adapater)))


(def-class affordance-source ()
  ((has-affordances :type spatial-affordance)))

(def-class space (set) ?s
  ((membership-test :value (kappa (?p) 
                                  :constraint (and (point ?p)
                                                   (has-space ?p ?s))))))
#|

?doesn't work, ask John

(def-instance my-space space 
  ())

(def-instance pt1 point
  ((has-space my-space)))

(findall '?x '(element-of ?x my-space))
(findany '?x '(membership-test my-space ?x))
(ask (element-of ?x my-space))

|#

(def-class point ()
  ((has-space :type space)))

(def-class two-dimensional-point (point)
  ((has-x-coordinate :type real-number)
   (has-y-coordinate :type real-number)))

(def-class longitudinal-point (two-dimensional-Point)
  ((has-latitude :type real-number)
   (has-longitude :type real-number))
 :slot-renaming ((has-latitude has-x-coordinate)
                (has-longitude has-y-coordinate)))

(def-class three-dimensional-point (two-dimensional-Point)
  ((has-z-coordinate :type real-number)))

(def-class location (set) ?l
  "A Location is a subset of a space"
  ((has-space :type Space)
   (has-points :type Point))
  :constraint (subset ?l (has-space ?l))) ;???

(def-class point-location (location) ?l0
  "Loc0 contains only points of the space"
  :constraint (= (length (has-points ?l0)) 1))

(def-class linear-location (location) ?l1
  "Loc 1 represents a linear location")
  
(def-class area-location (location) ?l2
  "Areal location")

(def-class monotone-polygon (area-location) ?pol
  "A polygon without holes"
  :constraint (= (first (has-points ?pol?))
                 (first (last (has-points ?pol)))))

(def-class region (monotone-polygon)
  "A region is constituted of one or more Monotone Polygons"
  ((has-parts :type monotone-polygon)))

(def-class simple-region (region) ?sr
  "A simple region is constituted of a single Monotone Polygons"
  :constraint (=(length (has-parts ?sr)) 1))

(def-class object-field (affordance-source)
  ((has-id :type string)
   (has-objects :type spatial-object)))

(def-class spatial-object (affordance-source)
  ((has-id :type string)
   (has-type :type string)
   (has-location :type Location)
   ;;john d 1/3/2006(has-attributes :type Spatial-object-attribute)
   ;;vlad 20/9/06, replaced by affordance-source (has-wsmo-goal :type goal-meta-class)))
   ))

(def-class spatial-object-meta-class () ?x
  :iff-def (or (= ?x spatial-object) (subclass-of ?x spatial-object)))

#|

(def-class neighbour () ())

(def-class test-spatial-object (spatial-object)
  ((has-name :type string)
   (has-latitude :type number)
   (has-longitude :type number)
   (has-neighbours :type neighbour)
   (has-address :type string)))

(def-instance foo test-spatial-object
  ((has-name "foo99")
   (has-latitude 0.084)
   (has-longitude 51.957)
   (has-neighbours fred bill tom)))

|#

(def-relation spatial-object-attributes (?spatial-object ?attributes)
  :iff-def
  (= ?attributes (get-spatial-object-attributes ?spatial-object)))

;;(findany '?atts '(spatial-object-attributes foo ?atts))
;;(ocml-eval (get-spatial-object-attributes 'foo))


(def-class area-location-marker () ()) ;;inherit from this to say it's an area, to be replaced by more subtle means when API allows
(def-class point-location-marker () ()) ;;inherit from this to say it's an area, to be replaced by more subtle means when API allows