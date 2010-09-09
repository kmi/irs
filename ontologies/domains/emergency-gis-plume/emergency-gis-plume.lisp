;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology emergency-gis-plume)

(def-instance GET-PLUME-DATA-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!Plume!Polygon!In!Radius\"")))

(def-instance GET-PLUME-DATA-AFFORDANCE spatial-affordance
  ((has-goal GET-PLUME-DATA-GOAL)
   (has-max-range-km 0.3)
   (has-input-roles-adapter GET-PLUME-DATA-ADAPTER)))

(def-class chemical-plume-emergency (area-location-marker)
  ((has-affordances 
    :DEFAULT-VALUE GET-PLUME-DATA-AFFORDANCE)))


;;;
(def-class plume-results (object-field)
  ((has-caller-situation :default-value chemical-plume-investigation-situation) 
   (producing-observable :default-value ((plume-level get-average-plume-level)))
   (has-objects :type plume-result)))

(def-class plume-result (spatial-object field-value)
  ((has-conc :type float)
   (has-pretty-name :type string)
   (has-location :type  plume-location)))

(def-class plume-location (monotone-polygon)
  ())

(def-class plume-method ()
  ((has-method :type string)))

(def-instance getPlume plume-method
  ((has-method "getPlume")))

(def-instance getPlumePolygonInRadius plume-method
  ((has-method "getPlumePolygonInRadius")))

(def-instance getPlumePolygonWithinPoligon plume-method
  ((has-method "getPlumePolygonWithinPoligon")))


(defun get-average-plume-level (inst)
(let ((obj (ocml::setofall '?x `(ocml::has-slot-value ,inst 'ocml::has-objects ?x)))
      (plume-level nil)
      (sum 0)
      (count 0))
     (loop for o in obj
           do (setf plume-level (ocml::THE-SLOT-VALUE o 'ocml::has-conc))
              (setf sum (+ sum plume-level))
              (setf count (+ count 1)))
     (if (> count 0) (/ sum count) 0)))


;;level1 (first situation)

(def-class chemical-plume-investigation-situation (localized-emergency-type area-location-marker classifiable-situation)
((has-requirements :default-value plume-hazard-level)
 (plume-hazard-level :default-value zero)
 (has-observables :default-value plume-level)
 (plume-level :type float :default-value 0)
 (has-affordances :default-value GET-PLUME-DATA-AFFORDANCE)
 (has-following-situation 
         :default-value chemical-plume-hazard-situation
         :default-value chemical-plume-investigation-situation))) 


;; level2 

(def-class chemical-plume-hazard-situation (localized-emergency-type classifiable-situation)
((has-requirements :default-value plume-hazard-level)
 (plume-hazard-level :type feature-level)
 ;;(has-observables :default-value go-to-entry)
 ;;(go-to-entry :type boolean)
 (has-abstractor :default-value plume-hazard-abstractor)))

;;1E-14,1E-11

(def-instance plume-hazard-abstractor abstractor
  ((has-body '(lambda (?obs)  
                (in-environment 
                 ((?v . (observables-feature-value ?obs 'plume-level)))
                 (cond ((> ?v 1E-12) 
                        (list-of 'plume-hazard-level 'high 
                                 (list-of (list-of 'plume-level ?v))))
                       ((and (<= ?v 1E-12) (> ?v 1E-14))
                        (list-of 'plume-hazard-level 'low 
                                 (list-of (list-of 'plume-level ?v))))
                       ((<= ?v 1E-14) 
                        (list-of 'plume-hazard-level 'zero 
                                 (list-of (list-of 'plume-level ?v))))))))   
   (applicability-check (kappa (?obs)
                                   (member 'plume-level (all-features-in-observables ?obs))))))


(def-class chemical-plume-low-hazard-situation (chemical-plume-hazard-situation)
((plume-hazard-level :default-value low)
 (has-affordances :DEFAULT-VALUE LOGIN-AFFORDANCE)))




(def-class chemical-plume-high-hazard-situation (chemical-plume-hazard-situation)
((plume-hazard-level :default-value high) 
 (has-affordances 
    :DEFAULT-VALUE LOGIN-AFFORDANCE
    :DEFAULT-VALUE GET-HOSPITALS-AFFORDANCE
    :DEFAULT-VALUE GET-REST-CENTRES-AFFORDANCE
    :DEFAULT-VALUE GET-TEMP-REST-CENTRES-AFFORDANCE
    :DEFAULT-VALUE GET-HOTELS-AFFORDANCE
    :DEFAULT-VALUE GET-INNS-AFFORDANCE
    :DEFAULT-VALUE GET-SUPERMARKETS-AFFORDANCE)))
 
