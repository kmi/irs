;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")
 
(in-ontology emergency-gis-domain)

;; spatial object query

(def-class emergency-type (spatial-object)
  ())

(def-class localized-emergency-type (emergency-type)
  ((has-location :TYPE AREA-LOCATION)));;should be has-location AREA-LOCATION

(def-instance GET-GIS-DATA-GOAL-HOSPITALS-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!Hospitals!In!Radius\"" "HAS-SPATIAL-OBJECT-QUERY hospitalsquery")))

(def-instance GET-GIS-DATA-GOAL-REST-CENTRES-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!Rest!Centres!In!Radius\"" "HAS-SPATIAL-OBJECT-QUERY RestCentresQuery")))

(def-instance  GET-POLYGON-REST-CENTRES-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!Rest!Centres!Polygon\"" "HAS-OPTION true" "HAS-BORDER true" "HAS-RADIUS 0")))

(def-instance GET-GIS-DATA-GOAL-TEMP-REST-CENTRES-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!Rest!Centres!In!Radius\"" "HAS-SPATIAL-OBJECT-QUERY TempCentresQuery")))

(def-instance GET-GIS-DATA-GOAL-HOTELS-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!Hotels!In!Radius\"" "HAS-SPATIAL-OBJECT-QUERY HotelsQuery")))

(def-instance GET-GIS-DATA-GOAL-INNS-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!Inns!In!Radius\"" "HAS-SPATIAL-OBJECT-QUERY InnsQuery")))

(def-instance GET-GIS-DATA-GOAL-SUPERMARKETS-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!Supermarkets!In!Radius\"" "HAS-SPATIAL-OBJECT-QUERY SupermarketsQuery")))

;(all-instances 'input-roles-adapter)

;(setofall '?x `(instance-of ?x 'spatial-affordance)) 

(def-instance GET-HOSPITALS-AFFORDANCE spatial-affordance
  ((has-goal GET-GIS-DATA-GOAL)
   (has-max-range-km 15)
   (has-input-roles-adapter GET-GIS-DATA-GOAL-HOSPITALS-ADAPTER)))

(def-instance GET-REST-CENTRES-AFFORDANCE spatial-affordance
  ((has-goal GET-GIS-DATA-GOAL)
   (has-max-range-km 15)
   (has-input-roles-adapter GET-GIS-DATA-GOAL-REST-CENTRES-ADAPTER)))

(def-instance GET-POLYGON-REST-CENTRES-AFFORDANCE spatial-affordance
  ((has-goal GET-POLYGON-ECC-REST-CENTRES-GOAL)
   ;;(has-max-range-km 15)
   (has-input-roles-adapter GET-POLYGON-REST-CENTRES-ADAPTER)))

(def-instance GET-FILTERED-REST-CENTRES-AFFORDANCE spatial-affordance
  ((has-goal GET-GIS-DATA-WITH-FILTER-GOAL)
   (has-max-range-km 15)
   (has-input-roles-adapter GET-GIS-DATA-GOAL-REST-CENTRES-ADAPTER)))

(def-instance GET-FILTERED-REST-CENTRES-IN-POLYGON-AFFORDANCE spatial-affordance
  ((has-goal GET-POLYGON-GIS-DATA-WITH-FILTER-GOAL)
   (has-max-range-km 15)
   (has-input-roles-adapter GET-GIS-DATA-GOAL-REST-CENTRES-ADAPTER)))

(def-instance GET-TEMP-REST-CENTRES-AFFORDANCE spatial-affordance
  ((has-goal GET-GIS-DATA-GOAL)
   (has-max-range-km 15)
   (has-input-roles-adapter GET-GIS-DATA-GOAL-TEMP-REST-CENTRES-ADAPTER)))

(def-instance GET-HOTELS-AFFORDANCE spatial-affordance
  ((has-goal GET-GIS-DATA-GOAL)
   (has-max-range-km 15)
   (has-input-roles-adapter GET-GIS-DATA-GOAL-HOTELS-ADAPTER)))

(def-instance GET-INNS-AFFORDANCE spatial-affordance
  ((has-goal GET-GIS-DATA-GOAL)
   (has-max-range-km 15)
   (has-input-roles-adapter GET-GIS-DATA-GOAL-INNS-ADAPTER)))

(def-instance GET-SUPERMARKETS-AFFORDANCE spatial-affordance
  ((has-goal GET-GIS-DATA-GOAL)
   (has-max-range-km 15)
   (has-input-roles-adapter GET-GIS-DATA-GOAL-SUPERMARKETS-ADAPTER)))

(def-instance LOGIN-GOAL-ADAPTER input-roles-adapter
  ((HAS-MAPPING "HAS-COMMAND connect" "has-user-name \"irs-iii@msg.open.ac.uk\"" "has-password \"irs-iii\"")))

(def-instance LOGIN-AFFORDANCE spatial-affordance
  ((has-goal LOGIN-GOAL)
   (has-input-roles-adapter LOGIN-GOAL-ADAPTER)))

(def-instance get-resources-data-goal-adapter input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!All!In!Radius\"" )))

(def-instance GET-RESOURCES-AFFORDANCE spatial-affordance
  ((has-goal get-resources-data-goal)
   (has-input-roles-adapter get-resources-data-goal-adapter)))

(def-instance get-snow-storm-resources-data-goal-adapter input-roles-adapter
  ((HAS-MAPPING "HAS-METHOD \"get!All!In!Radius\"" )))

(def-instance GET-SNOW-STORM-RESOURCES-AFFORDANCE spatial-affordance
  ((has-goal get-snow-storm-resources-data-goal)
   (has-input-roles-adapter get-snow-storm-resources-data-goal-adapter)))

;;default values do not inherit, is that normal?

(def-class snow-storm-emergency (localized-emergency-type area-location-marker)
  ((has-affordances 
    :DEFAULT-VALUE GET-HOSPITALS-AFFORDANCE
    :DEFAULT-VALUE GET-REST-CENTRES-AFFORDANCE
    :DEFAULT-VALUE GET-POLYGON-REST-CENTRES-AFFORDANCE
    :DEFAULT-VALUE GET-TEMP-REST-CENTRES-AFFORDANCE
    :DEFAULT-VALUE GET-HOTELS-AFFORDANCE
    :DEFAULT-VALUE GET-INNS-AFFORDANCE
    :DEFAULT-VALUE GET-SUPERMARKETS-AFFORDANCE
    :DEFAULT-VALUE LOGIN-AFFORDANCE
    :DEFAULT-VALUE GET-SNOW-STORM-RESOURCES-AFFORDANCE
    :DEFAULT-VALUE GET-FILTERED-REST-CENTRES-AFFORDANCE
    ;;:DEFAULT-VALUE GET-FILTERED-REST-CENTRES-IN-POLYGON-AFFORDANCE
    )))

(def-class proximity-so-affordance-source (affordance-source)
  ((has-affordances
    :DEFAULT-VALUE GET-HOSPITALS-AFFORDANCE
    :DEFAULT-VALUE GET-REST-CENTRES-AFFORDANCE
    :DEFAULT-VALUE GET-TEMP-REST-CENTRES-AFFORDANCE
    :DEFAULT-VALUE GET-HOTELS-AFFORDANCE
    :DEFAULT-VALUE GET-INNS-AFFORDANCE
    :DEFAULT-VALUE GET-SUPERMARKETS-AFFORDANCE
    :DEFAULT-VALUE GET-RESOURCES-AFFORDANCE)))


(def-class spatial-object-query ()
((has-method :type string)))

(def-instance RestCentresQuery spatial-object-query
((has-method "getRestCentresInRadius")
 (has-method "getRestCentresPolygonRequest"))) ;; not implemented in wsmo

(def-instance TempCentresQuery spatial-object-query
((has-method "getRestCentresInRadius")))

(def-instance HotelsQuery spatial-object-query
((has-method "getHotelsInRadius")))

(def-instance InnsQuery spatial-object-query
((has-method "getInnsInRadius")))

(def-instance HospitalsQuery spatial-object-query
((has-method "getHospitalsInRadius")))

(def-instance SupermarketsQuery spatial-object-query
((has-method "getSupermarketsInRadius")))

(def-class method (string))

(def-class has-gis-result-data (object-field)
  ())

(def-class rest-centres-results (has-gis-result-data)
  ((has-objects :type rest-centre)))

(def-class temp-centres-results (has-gis-result-data)
  ((has-objects :type rest-centre)))

(def-class hospitals-results (has-gis-result-data)
  ((has-objects :type hospital)))

(def-class hotels-results (has-gis-result-data)
  ((has-objects :type hotel)))

(def-class inns-results (has-gis-result-data)
  ((has-objects :type inn)))

(def-class supermarkets-results (has-gis-result-data)
  ((has-objects :type supermarket)))

(def-class oracle-object () ())

;; results' elements
;;spatial-object
(def-class rest-centre (spatial-object point-location-marker house-archetype proximity-so-affordance-source)
  ((has-latitude :type float)
   (has-longitude :type float)
   (has-address :type string)
   (has-capacity :type integer)
   (has-cooking :type string)
   (has-GMS :type string)
   (has-heating :type string)
   (has-key-holder :type string)
   (has-key-holder2 :type string)
   (has-key-holder2-telephone :type string)
   (has-key-holder-telephone :type string)
   (has-meals :type integer)
   (has-name :type string)
   (has-provision :type string)
   (has-remarks :type string)
   (has-telephone :type string)
   ))

(def-class temp-centre (rest-centre))

(def-class accommodation (spatial-object point-location-marker house-archetype proximity-so-affordance-source)
((has-latitude :type float)
 (has-longitude :type float)
 (has-address1 :type string)
 (has-address2 :type string)
 (has-postcode :type string)
 (has-rooms :type string)
 (has-telephone :type string)))

(def-class hotel (accommodation))

(def-class inn (accommodation))

(def-class hospital (spatial-object point-location-marker house-archetype google-maps-hci proximity-so-affordance-source) ?h
((has-latitude :type float)
 (has-longitude :type float)
 (has-address :type string)
 (has-beds :type integer)
 (has-text-location :type string)
 (has-name :type string)
 (has-postcode :type string)
 (has-telephone :type string)))

(def-rule hover-content-rule
  ((has-hover-content ?item ?content)
   if 
   (hospital ?item)
   (has-name ?item ?content))
  ((has-hover-content ?item ?content)
   if 
   (supermarket ?item)
   (has-chain ?item ?content))
  ((has-hover-content ?item ?content)
   if 
   (rest-centre ?item)
   (has-address ?item ?content)))

(def-class supermarket (spatial-object point-location-marker house-archetype proximity-so-affordance-source)
  ((has-latitude :type float)
   (has-longitude :type float)
   (has-address :type string)
   (has-alcohol :type boolean)
   (has-chain :type string)
   (has-clothing :type boolean)
   (has-fri-close :type string)
   (has-fri-open :type string)
   (has-mon-close :type string)
   (has-mon-open :type string)
   (has-petrol :type boolean)
   (has-pharmacy :type boolean)
   (has-postcode :type string)
   (has-sat-close :type string)
   (has-sat-open :type string)
   (has-store :type string)
   (has-sun-close :type string)
   (has-sun-open :type string)
   (has-telephone :type string)
   (has-thu-close :type string)
   (has-thu-open :type string)
   (has-town :type string)
   (has-tue-close :type string)
   (has-tue-open :type string)
   (has-wed-close :type string)
   (has-wed-open :type string)))



;; resources


(def-class emergency-resources-results (has-gis-result-data)
((has-objects :type emergency-resource)))


(def-class emergency-resource (spatial-object point-location-marker house-archetype proximity-so-affordance-source) 
((has-latitude :type float)
 (has-longitude :type float)
 (has-res24hrNo :type string) 
 (has-res2nd24hrNo :type string) 
 (has-res2ndContact :type string) 
 (has-resAccessEquip :type boolean) 
 (has-resAddress1 :type string) 
 (has-resAddress2 :type string) 
 (has-resAerialPhotography :type boolean) 
 (has-resBarrierAndControlSystems :type boolean) 
 (has-resBrochureAvailable :type boolean) 
 (has-resChemAndBiodetections :type boolean) 
 (has-resCleaningAndRecoverySystem :type boolean) 
 (has-resCompanyName :type string) 
 (has-resConferenceSystems :type boolean) 
 (has-resContactName :type string) 
 (has-resCounty :type string) 
 (has-resDecontaminationEquipment :type boolean) 
 (has-resDecontaminationKitsAndProducts :type boolean) 
 (has-resDriersAndDehumid :type boolean) 
 (has-resDrinkingWater :type boolean) 
 (has-resEarthMovingEquip :type boolean) 
 (has-resEmergencyBedding :type boolean) 
 (has-resEmergencyBeds :type boolean) 
 (has-resEmergencyBlanketsAndCapes :type boolean) 
 (has-resEmergencyClothingAndFootwear :type boolean) 
 (has-resEmergencyFood :type boolean) 
 (has-resFax :type string) 
 (has-resGenerators :type boolean) 
 (has-resHeatersAndCoolers :type boolean) 
 (has-resHotWashFacilities :type boolean) 
 (has-resHygieneProducts :type boolean) 
 (has-resId :type integer) 
 (has-resLighting :type boolean) 
 (has-resMethod :type string) 
 (has-resMisc :type boolean) 
 (has-resOilWaterAbsorbing :type boolean) 
 (has-resPortableToilets :type boolean) 
 (has-resPortacabinsAndPortableShelter :type boolean) 
 (has-resPosition :type string) 
 (has-resPostcode :type string) 
 (has-resPressureWashers :type boolean) 
 (has-resProtectiveClothing :type boolean) 
 (has-resReply :type boolean) 
 (has-resRescueCraft :type boolean) 
 (has-resSandbaggingMachines :type boolean) 
 (has-resSandbags :type boolean) 
 (has-resSiteClearenceDemolition :type boolean) 
 (has-resSiteClearencePlant :type boolean) 
 (has-resTelNo :type string) 
 (has-resTempMortuaryEquip :type boolean) 
 (has-resTemporaryCateringFacilities :type boolean) 
 (has-resTown :type string) 
 (has-resTranslators :type boolean) 
 (has-resTwoWayRadios :type boolean) 
 (has-resType :type string) 
 (has-resType1 :type string) 
 (has-resUniqueNo :type string) 
 (has-resWaterpumps :type boolean) 
 (has-resXRayAndMedical :type boolean))) 


(def-instance err1 emergency-resources-results 
((has-objects er1)))


(def-instance er1 emergency-resource
((has-resDriersAndDehumid true)))

(def-instance er2 emergency-resource
((has-resEmergencyBlanketsAndCapes true)))

(def-instance er3 emergency-resource
((has-resProtectiveClothing true)))



;;#|

;;(def-class emergency-resource-method (spatial-object-query))

;;(def-instance getAllInRadius emergency-resource-method
;;  ((has-method "getAllInRadius")))

;; introduced for resources filtering

(def-class supplying-company (emergency-resource)
  ((is-suitable-for-emergency :type emergency-type)))


(def-instance driersDehumidifiersSupplier supplying-company 
((is-suitable-for-emergency snow-storm-emergency)
 (has-resDriersAndDehumid true)))


(def-instance generatorsSupplier  supplying-company 
((is-suitable-for-emergency snow-storm-emergency)
 (has-resGenerators true)))

(def-instance waterPumpsSupplier  supplying-company 
((is-suitable-for-emergency snow-storm-emergency)
 (has-resWaterPumps true)))


(def-instance emergencyBlanketsCapesSupplier supplying-company 
((is-suitable-for-emergency snow-storm-emergency)
 (has-resEmergencyBlanketsAndCapes true)))


(def-instance emergencyClothingFootwearSupplier supplying-company 
((is-suitable-for-emergency chemical-plume-emergency)
 (has-resProtectiveClothing true)))
;;|#

;(describe-instance 'instance2103)