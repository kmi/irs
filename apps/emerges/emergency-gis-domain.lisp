;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology emergency-gis-domain)

;; method

(def-class method ())

(def-instance getRestCentresInRadius method)

(def-instance getHotelsInRadius method)

(def-instance getInnsInRadius method)

(def-instance getHospitalsInRadius method)

(def-instance getSupermarketsInRadius method)


;; results

(def-class rest-centres-list (list))

(def-class accommodations-list (list))

(def-class hospitals-list (list))

(def-class supermarkets-list (list))




;; results' elements

(def-class rest-centre ()
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
 (has-telephone :type string)))

(def-class accommodation ()
((has-latitude :type float)
 (has-longitude :type float)
 (has-address1 :type string)
 (has-address2 :type string)
 (has-postcode :type string)
 (has-rooms :type string)
 (has-telephone :type string)))

(def-class hospital ()
((has-latitude :type float)
 (has-longitude :type float)
 (has-address :type string)
 (has-beds :type integer)
 (has-location :type string)
 (has-name :type string)
 (has-postcode :type string)
 (has-telephone :type string)))

(def-class supermarket ()
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
 
 