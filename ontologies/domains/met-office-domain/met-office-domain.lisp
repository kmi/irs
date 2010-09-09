;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology met-office-domain)

(def-class met-office-method ())

(def-instance getSnowPolygon met-office-method)

(def-instance getSnowPolygonInRadius met-office-method)

(def-instance getSnowPolygonWithinPoligon met-office-method)

(def-instance GET-SNOW-DATA-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!Snow!Polygon!In!Radius\"" "HAS-SNOW-LEVEL 1")))

(def-instance GET-SNOW-DATA-AFFORDANCE spatial-affordance
  ((has-goal GET-SNOW-DATA-GOAL)
   (has-max-range-km 4)
   (has-input-roles-adapter GET-SNOW-DATA-ADAPTER)))

(def-instance GET-POLYGON-SNOW-DATA-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!Snow!Polygon!Within!Polygon\"" "HAS-SNOW-LEVEL 1" "HAS-OPTION true" "HAS-RADIUS 0")))

(def-instance GET-POLYGON-SNOW-DATA-AFFORDANCE spatial-affordance
  ((has-goal GET-POLYGON-SNOW-DATA-GOAL)
   ;;(has-max-range-km 4)
   (has-input-roles-adapter GET-POLYGON-SNOW-DATA-ADAPTER)))
#|

(def-instance LOGIN-GOAL-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-COMMAND connect" "has-user-name \"irs-iii\"" "has-password \"irs-iii\"")))

(def-instance LOGIN-AFFORDANCE spatial-affordance
  ((has-goal LOGIN-GOAL)
   (has-input-roles-adapter LOGIN-GOAL-ADAPTER)))


(def-instance get-snow-storm-resources-data-goal-adapter input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!All!In!Radius\"" )))

(def-instance GET-RESOURCES-AFFORDANCE spatial-affordance
  ((has-goal get-snow-storm-resources-data-goal)
   (has-input-roles-adapter get-snow-storm-resources-data-goal-adapter)))

(def-instance get-snow-storm-resources-data-goal-adapter input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!All!In!Radius\"" )))

(def-instance GET-SNOW-STORM-RESOURCES-AFFORDANCE spatial-affordance
  ((has-goal get-snow-storm-resources-data-goal)
   (has-input-roles-adapter get-snow-storm-resources-data-goal-adapter)))
|#
#|
(def-class SNOW-HAZARD (area-location-marker)
  ((has-affordances 
    :DEFAULT-VALUE GET-SNOW-DATA-AFFORDANCE
    :DEFAULT-VALUE LOGIN-AFFORDANCE)))
|#
;;; 0) domain ontology has to load emergency-gis-situation ontology (domain) 
;;; 1) adding caller and obsvervable maker

(def-class snow-results (object-field)
  ((has-caller-situation :default-value weather-investigation-area) ;; needed for retrieving the following sit
   (producing-observable :default-value ((snow-level get-average-snow-level))) ;;pair <feature function> where the function get an obsvervable of feature f from the instance of the class
   (has-objects :type snow-result)))

(def-class snow-result (spatial-object field-value)
  ((has-snow-level :type float)
   (has-location :type snow-location)
   (has-pretty-name :type string)))

(def-class snow-location (monotone-polygon)
  ())

;;(def-class snow-polygon-results (snow-results))


;;(def-class snow-polygon-result(snow-result))


;;; 2) defining obsvervable maker

(defun get-average-snow-level (inst)
(let ((obj (ocml::setofall '?x `(ocml::has-slot-value ,inst 'ocml::has-objects ?x)))
      (snow-level nil)
      (sum 0)
      (count 0))
     (loop for o in obj
           do (setf snow-level (ocml::THE-SLOT-VALUE o 'ocml::has-snow-level))
              (setf sum (+ sum snow-level))
              (setf count (+ count 1)))
     (if (> count 0) (/ sum count) 0)))


;;(get-average-snow-level 'instance3214)




;;;; 3) defining situations

;(def-instance met-office-domain application-domain)



;;level1 (first situation)

(def-class weather-investigation-area (localized-emergency-type area-location-marker classifiable-situation)
((has-requirements :default-value snow-hazard-level)
 (snow-hazard-level :default-value zero)
 (has-observables :default-value snow-level)
 (snow-level :type float :default-value 0)
 (has-affordances 
  :default-value GET-SNOW-DATA-AFFORDANCE
  :default-value GET-POLYGON-SNOW-DATA-AFFORDANCE
  :default-value LOGIN-AFFORDANCE)
 (has-following-situation 
         :default-value snow-storm-hazard-situation
         :default-value weather-investigation-area))) 


;; level2 

(def-class snow-storm-hazard-situation (localized-emergency-type classifiable-situation) ;; snow-storm-emergency)
((has-requirements :default-value snow-hazard-level)
 (snow-hazard-level :type feature-level)
 ;;(has-observables :default-value go-to-entry)
 ;;(go-to-entry :type boolean)
 (has-abstractor :default-value snow-hazard-abstractor)))

(def-instance snow-hazard-abstractor abstractor
  ((has-body '(lambda (?obs)  
                (in-environment 
                 ((?v . (observables-feature-value ?obs 'snow-level)))
                 (cond ((> ?v 2) 
                        (list-of 'snow-hazard-level 'high 
                                 (list-of (list-of 'snow-level ?v))))
                       ((and (<= ?v 2) (> ?v 0))
                        (list-of 'snow-hazard-level 'low 
                                 (list-of (list-of 'snow-level ?v))))
                       ((= ?v 0) 
                        (list-of 'snow-hazard-level 'zero 
                                 (list-of (list-of 'snow-level ?v))))))))   
   (applicability-check (kappa (?obs)
                                   (member 'snow-level (all-features-in-observables ?obs))))))


(def-class snow-storm-low-hazard-situation (snow-storm-hazard-situation)
((snow-hazard-level :default-value low)
 ;;(go-to-entry :default-value t)
 (has-affordances :DEFAULT-VALUE LOGIN-AFFORDANCE 
                  :DEFAULT-VALUE GET-RESOURCES-AFFORDANCE)))
 ;;(has-following-situation :default-value weather-investigation-area)))



(def-class snow-storm-high-hazard-situation (snow-storm-hazard-situation)
((snow-hazard-level :default-value high)
 ;;(go-to-entry :default-value t)
 (has-affordances 
    :DEFAULT-VALUE LOGIN-AFFORDANCE
    :DEFAULT-VALUE GET-HOSPITALS-AFFORDANCE
    :DEFAULT-VALUE GET-REST-CENTRES-AFFORDANCE
    :DEFAULT-VALUE GET-POLYGON-REST-CENTRES-AFFORDANCE
    :DEFAULT-VALUE GET-TEMP-REST-CENTRES-AFFORDANCE
    :DEFAULT-VALUE GET-HOTELS-AFFORDANCE
    :DEFAULT-VALUE GET-INNS-AFFORDANCE
    :DEFAULT-VALUE GET-SUPERMARKETS-AFFORDANCE
    :DEFAULT-VALUE GET-SNOW-STORM-RESOURCES-AFFORDANCE)))
 ;;(has-following-situation :default-value weather-investigation-area)))




