
;; Mode: Lisp; Package: ocml

;;; Vlad
;;; this file contains all lifting and lowering functions
 
(in-package "OCML")

(in-ontology emergency-gis-domain)

(deflift lift-rest-centres-results rest-centres-results (:web-services '(GET-ECC-REST-CENTRES-WEB-SERVICE))
  (("has-objects" (lift-rest-centre '("getRestCentresInRadiusResponse/getRestCentresInRadiusReturn/getRestCentresInRadiusReturn") :AS-VALUES))))

(deflift lift-polygon-rest-centres-results rest-centres-results (:web-services '(GET-POLYGON-ECC-REST-CENTRES-WEB-SERVICE))
  (("has-objects" (lift-rest-centre '("getRestCentresPolygonResponse/getRestCentresPolygonReturn/getRestCentresPolygonReturn") :AS-VALUES))))

(deflift lift-temp-centres-results temp-centres-results ()
  (("has-objects" (lift-rest-centre '("getRestCentresInRadiusResponse/getRestCentresInRadiusReturn/getRestCentresInRadiusReturn") :AS-VALUES))))

(deflift lift-rest-centre rest-centre ()
(;;("has-latitude" "getRestCentresInRadiusReturn/latitude/text()")
 ("has-latitude" "*/latitude/text()")
 ("has-longitude" "*/longitude/text()")
 ("has-address" "*/rcAddress/text()")
 ("has-capacity" "*/rcCapacity/text()")
 ("has-cooking" "*/rcCooking/text()")
 ("has-GMS" "*/rcGMS/text()")
 ("has-heating" "*/rcHeating/text()")
 ("has-key-holder" "*/rcKeyHolder/text()")
 ("has-key-holder2" "*/rcKeyHolder2/text()")
 ("has-key-holder2-telephone" "*/rcKeyHolder2Telephone/text()")
 ("has-key-holder-telephone" "*/rcKeyHolderTelephone/text()")
 ("has-meals" "*/rcMeals/text()")
 ("has-name" "*/rcName/text()")
 ("has-provision" "*/rcProvision/text()")
 ("has-remarks" "*/rcRemarks/text()")
 ("has-telephone" "*/rcTelephone/text()")
 ("has-location" (eval (sgis-lift-point-location 
                          :lat-xpath "*/latitude/text()"
                          :long-xpath "*/longitude/text()")))))

(deflift lift-hotels-results hotels-results ()
  (("has-objects" (lift-hotel '("getHotelsInRadiusResponse/getHotelsInRadiusReturn/getHotelsInRadiusReturn") :AS-VALUES))))

(deflift lift-hotel hotel ()
(("has-latitude" "getHotelsInRadiusReturn/latitude/text()")
 ("has-longitude" "getHotelsInRadiusReturn/longitude/text()")
 ("has-address1" "getHotelsInRadiusReturn/aAddress1/text()")
 ("has-address2" "getHotelsInRadiusReturn/aAddress2/text()")
 ;;("has-name" "getHotelsInRadiusReturn/aName/text()")
 ("has-postcode" "getHotelsInRadiusReturn/aPostCode/text()")
 ("has-rooms" "getHotelsInRadiusReturn/aRooms/text()")
 ("has-telephone" "getHotelsInRadiusReturn/aTelephone/text()")
 ("has-location" (eval (sgis-lift-point-location 
                          :lat-xpath "getHotelsInRadiusReturn/latitude/text()"
                          :long-xpath "getHotelsInRadiusReturn/longitude/text()")))))

(deflift lift-inns-results inns-results ()
  (("has-objects" (lift-inn '("getInnsInRadiusResponse/getInnsInRadiusReturn/getInnsInRadiusReturn") :AS-VALUES))))

(deflift lift-inn inn ()
(("has-latitude" "getInnsInRadiusReturn/latitude/text()")
 ("has-longitude" "getInnsInRadiusReturn/longitude/text()")
 ("has-address1" "getInnsInRadiusReturn/aAddress1/text()")
 ("has-address2" "getInnsInRadiusReturn/aAddress2/text()")
 ;;("has-name" "getInnsInRadiusReturn/aName/text()")
 ("has-postcode" "getInnsInRadiusReturn/aPostCode/text()")
 ("has-rooms" "getInnsInRadiusReturn/aRooms/text()")
 ("has-telephone" "getInnsInRadiusReturn/aTelephone/text()")
 ("has-location" (eval (sgis-lift-point-location 
                          :lat-xpath "getInnsInRadiusReturn/latitude/text()"
                          :long-xpath "getInnsInRadiusReturn/longitude/text()")))))

(deflift lift-hospitals-results hospitals-results ()
  (("has-objects" (lift-hospital '("getHospitalsInRadiusResponse/getHospitalsInRadiusReturn/getHospitalsInRadiusReturn") :AS-VALUES))))

(deflift lift-hospital hospital ()
  (("has-latitude" "getHospitalsInRadiusReturn/latitude/text()")
   ("has-longitude" "getHospitalsInRadiusReturn/longitude/text()")
   ("has-address" "getHospitalsInRadiusReturn/hAddress/text()")
   ("has-beds" "getHospitalsInRadiusReturn/hBeds/text()" )
   ("has-text-location" "getHospitalsInRadiusReturn/hLocation/text()")
   ("has-name" "getHospitalsInRadiusReturn/hName/text()")
   ("has-postcode" "getHospitalsInRadiusReturn/hPostCode/text()")
   ("has-telephone" "getHospitalsInRadiusReturn/hTelephone/text()")
   ("has-location" (eval (sgis-lift-point-location 
                          :lat-xpath "getHospitalsInRadiusReturn/latitude/text()"
                          :long-xpath "getHospitalsInRadiusReturn/longitude/text()")))))


#|

(defparameter *rest-centres-string* "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soapenv:Body><getRestCentresInRadiusResponse soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><getRestCentresInRadiusReturn soapenc:arrayType=\"ns1:RestCentreResult[6]\" xsi:type=\"soapenc:Array\" xmlns:ns1=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><getRestCentresInRadiusReturn href=\"#id0\"/><getRestCentresInRadiusReturn href=\"#id1\"/><getRestCentresInRadiusReturn href=\"#id2\"/><getRestCentresInRadiusReturn href=\"#id3\"/><getRestCentresInRadiusReturn href=\"#id4\"/><getRestCentresInRadiusReturn href=\"#id5\"/></getRestCentresInRadiusReturn></getRestCentresInRadiusResponse><multiRef id=\"id5\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns2:RestCentreResult\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:ns2=\"http://www.bt.com\"><latitude xsi:type=\"xsd:float\">51.745605</latitude><longitude xsi:type=\"xsd:float\">0.45462352</longitude><rcAddress xsi:type=\"xsd:string\">Fox Crescent</rcAddress><rcCapacity xsi:type=\"xsd:int\">300</rcCapacity><rcCooking xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcGMS xsi:type=\"xsd:string\">F</rcGMS><rcHeating xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcKeyHolder xsi:type=\"xsd:string\">C.Palmer</rcKeyHolder><rcKeyHolder2 xsi:type=\"xsd:string\">V.Finch</rcKeyHolder2><rcKeyHolder2Telephone xsi:type=\"xsd:string\">01245 352466</rcKeyHolder2Telephone><rcKeyHolderTelephone xsi:type=\"xsd:string\">01245 601228</rcKeyHolderTelephone><rcMeals xsi:type=\"xsd:int\">0</rcMeals><rcName xsi:type=\"xsd:string\">Rainsford High Sch</rcName><rcProvision xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcRemarks xsi:type=\"xsd:string\"></rcRemarks><rcTelephone xsi:type=\"xsd:string\">01245 265511</rcTelephone></multiRef><multiRef id=\"id0\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns3:RestCentreResult\" xmlns:ns3=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><latitude xsi:type=\"xsd:float\">51.74668</latitude><longitude xsi:type=\"xsd:float\">0.44258353</longitude><rcAddress xsi:type=\"xsd:string\">Trent Road</rcAddress><rcCapacity xsi:type=\"xsd:int\">200</rcCapacity><rcCooking xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcGMS xsi:type=\"xsd:string\">F</rcGMS><rcHeating xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcKeyHolder xsi:type=\"xsd:string\">D.Springett</rcKeyHolder><rcKeyHolder2 xsi:type=\"xsd:string\">M.Sullivan</rcKeyHolder2><rcKeyHolder2Telephone xsi:type=\"xsd:string\">01245 269515</rcKeyHolder2Telephone><rcKeyHolderTelephone xsi:type=\"xsd:string\">01245 442857</rcKeyHolderTelephone><rcMeals xsi:type=\"xsd:int\">0</rcMeals><rcName xsi:type=\"xsd:string\">Lawford Mead Cnty Junior Sch</rcName><rcProvision xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcRemarks xsi:type=\"xsd:string\"></rcRemarks><rcTelephone xsi:type=\"xsd:string\">01245 354134</rcTelephone></multiRef><multiRef id=\"id4\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns4:RestCentreResult\" xmlns:ns4=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><latitude xsi:type=\"xsd:float\">51.75177</latitude><longitude xsi:type=\"xsd:float\">0.4522192</longitude><rcAddress xsi:type=\"xsd:string\">Salerno Way</rcAddress><rcCapacity xsi:type=\"xsd:int\">250</rcCapacity><rcCooking xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcGMS xsi:type=\"xsd:string\">F</rcGMS><rcHeating xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcKeyHolder xsi:type=\"xsd:string\">B.Pepper</rcKeyHolder><rcKeyHolder2 xsi:type=\"xsd:string\">David Tebby</rcKeyHolder2><rcKeyHolder2Telephone xsi:type=\"xsd:string\">01245 441405</rcKeyHolder2Telephone><rcKeyHolderTelephone xsi:type=\"xsd:string\">01245 467139</rcKeyHolderTelephone><rcMeals xsi:type=\"xsd:int\">0</rcMeals><rcName xsi:type=\"xsd:string\">Melbourne Park Pavilion</rcName><rcProvision xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcRemarks xsi:type=\"xsd:string\"></rcRemarks><rcTelephone xsi:type=\"xsd:string\">01245 250355</rcTelephone></multiRef><multiRef id=\"id3\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns5:RestCentreResult\" xmlns:ns5=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><latitude xsi:type=\"xsd:float\">51.748695</latitude><longitude xsi:type=\"xsd:float\">0.44650304</longitude><rcAddress xsi:type=\"xsd:string\">Melbourne Avenue</rcAddress><rcCapacity xsi:type=\"xsd:int\">100</rcCapacity><rcCooking xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcGMS xsi:type=\"xsd:string\">F</rcGMS><rcHeating xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcKeyHolder xsi:type=\"xsd:string\">Rev. M.Gordon</rcKeyHolder><rcKeyHolder2 xsi:type=\"xsd:string\">Mrs M.Haggar</rcKeyHolder2><rcKeyHolder2Telephone xsi:type=\"xsd:string\">01245 265033</rcKeyHolder2Telephone><rcKeyHolderTelephone xsi:type=\"xsd:string\">01245 354256</rcKeyHolderTelephone><rcMeals xsi:type=\"xsd:int\">0</rcMeals><rcName xsi:type=\"xsd:string\">Blessed Sacrement Church Hall</rcName><rcProvision xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcRemarks xsi:type=\"xsd:string\"></rcRemarks><rcTelephone xsi:type=\"xsd:string\">-</rcTelephone></multiRef><multiRef id=\"id1\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns6:RestCentreResult\" xmlns:ns6=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><latitude xsi:type=\"xsd:float\">51.74699</latitude><longitude xsi:type=\"xsd:float\">0.44333917</longitude><rcAddress xsi:type=\"xsd:string\">Trent Road</rcAddress><rcCapacity xsi:type=\"xsd:int\">200</rcCapacity><rcCooking xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcGMS xsi:type=\"xsd:string\">F</rcGMS><rcHeating xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcKeyHolder xsi:type=\"xsd:string\">D.Springett</rcKeyHolder><rcKeyHolder2 xsi:type=\"xsd:string\">Mrs K.Wright</rcKeyHolder2><rcKeyHolder2Telephone xsi:type=\"xsd:string\">01376 584959</rcKeyHolder2Telephone><rcKeyHolderTelephone xsi:type=\"xsd:string\">01245 442857</rcKeyHolderTelephone><rcMeals xsi:type=\"xsd:int\">0</rcMeals><rcName xsi:type=\"xsd:string\">Lawford Mead Cnty Infant Sch</rcName><rcProvision xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcRemarks xsi:type=\"xsd:string\"></rcRemarks><rcTelephone xsi:type=\"xsd:string\">01245 256086</rcTelephone></multiRef><multiRef id=\"id2\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns7:RestCentreResult\" xmlns:ns7=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><latitude xsi:type=\"xsd:float\">51.748066</latitude><longitude xsi:type=\"xsd:float\">0.44528082</longitude><rcAddress xsi:type=\"xsd:string\">Melbourne Avenue</rcAddress><rcCapacity xsi:type=\"xsd:int\">100</rcCapacity><rcCooking xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcGMS xsi:type=\"xsd:string\">F</rcGMS><rcHeating xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcKeyHolder xsi:type=\"xsd:string\">Rev. A.Cozens</rcKeyHolder><rcKeyHolder2 xsi:type=\"xsd:string\">J.Sharp</rcKeyHolder2><rcKeyHolder2Telephone xsi:type=\"xsd:string\">01245 256114</rcKeyHolder2Telephone><rcKeyHolderTelephone xsi:type=\"xsd:string\">01245 496722</rcKeyHolderTelephone><rcMeals xsi:type=\"xsd:int\">0</rcMeals><rcName xsi:type=\"xsd:string\">St Andrews Church Hall</rcName><rcProvision xsi:type=\"xsd:string\" xsi:nil=\"true\"/><rcRemarks xsi:type=\"xsd:string\"></rcRemarks><rcTelephone xsi:type=\"xsd:string\">-</rcTelephone></multiRef></soapenv:Body></soapenv:Envelope>")


(lift-rest-centres-results *rest-centres-string*)
(describe-instance 'INSTANCE2711)
(describe-instance 'INSTANCE2743)
(describe-instance 'INSTANCE2745)
(sgis-lower-spatial-object-location 'INSTANCE2743)
(findall '?x '(= ?x (the-parent-new INSTANCE2745 nil)))
(findany '?x `(and
               (= ?x (the-parent-new INSTANCE2745 nil))
               ))
(xpm::o-xp "has-location" 'INSTANCE2743)
(describe-instance 'INSTANCE2747)
(lower-object-field 'INSTANCE2479)

(defparameter *supermarkets-string* "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soapenv:Body><getSupermarketsInRadiusResponse soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><getSupermarketsInRadiusReturn soapenc:arrayType=\"ns1:SupermarketResult[3]\" xsi:type=\"soapenc:Array\" xmlns:ns1=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><getSupermarketsInRadiusReturn href=\"#id0\"/><getSupermarketsInRadiusReturn href=\"#id1\"/><getSupermarketsInRadiusReturn href=\"#id2\"/></getSupermarketsInRadiusReturn></getSupermarketsInRadiusResponse><multiRef id=\"id0\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns2:SupermarketResult\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:ns2=\"http://www.bt.com\"><latitude xsi:type=\"xsd:float\">51.733887</latitude><longitude xsi:type=\"xsd:float\">0.47929397</longitude><smAddress xsi:type=\"xsd:string\">47-53 Springfield Road</smAddress><smAlcohol xsi:type=\"xsd:boolean\">false</smAlcohol><smChain xsi:type=\"xsd:string\">Tesco</smChain><smClothing xsi:type=\"xsd:boolean\">false</smClothing><smFriClose xsi:type=\"xsd:string\">2200</smFriClose><smFriOpen xsi:type=\"xsd:string\">0700</smFriOpen><smMonClose xsi:type=\"xsd:string\">2200</smMonClose><smMonOpen xsi:type=\"xsd:string\">0700</smMonOpen><smPetrol xsi:type=\"xsd:boolean\">false</smPetrol><smPharmacy xsi:type=\"xsd:boolean\">false</smPharmacy><smPostCode xsi:type=\"xsd:string\">CM2 6QT</smPostCode><smSatClose xsi:type=\"xsd:string\">2200</smSatClose><smSatOpen xsi:type=\"xsd:string\">0700</smSatOpen><smStore xsi:type=\"xsd:string\">CHELMSFORD SPRINGFIELD ROAD</smStore><smSunClose xsi:type=\"xsd:string\">1700</smSunClose><smSunOpen xsi:type=\"xsd:string\">1100</smSunOpen><smTelephone xsi:type=\"xsd:string\"></smTelephone><smThuClose xsi:type=\"xsd:string\">2200</smThuClose><smThuOpen xsi:type=\"xsd:string\">0700</smThuOpen><smTown xsi:type=\"xsd:string\">CHELMSFORD</smTown><smTueClose xsi:type=\"xsd:string\">2200</smTueClose><smTueOpen xsi:type=\"xsd:string\">0700</smTueOpen><smWedClose xsi:type=\"xsd:string\">2200</smWedClose><smWedOpen xsi:type=\"xsd:string\">0700</smWedOpen></multiRef><multiRef id=\"id2\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns3:SupermarketResult\" xmlns:ns3=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><latitude xsi:type=\"xsd:float\">51.721</latitude><longitude xsi:type=\"xsd:float\">0.4639337</longitude><smAddress xsi:type=\"xsd:string\">Princes Road</smAddress><smAlcohol xsi:type=\"xsd:boolean\">false</smAlcohol><smChain xsi:type=\"xsd:string\">Tesco</smChain><smClothing xsi:type=\"xsd:boolean\">false</smClothing><smFriClose xsi:type=\"xsd:string\">2359</smFriClose><smFriOpen xsi:type=\"xsd:string\">0000</smFriOpen><smMonClose xsi:type=\"xsd:string\">2359</smMonClose><smMonOpen xsi:type=\"xsd:string\">0800</smMonOpen><smPetrol xsi:type=\"xsd:boolean\">false</smPetrol><smPharmacy xsi:type=\"xsd:boolean\">false</smPharmacy><smPostCode xsi:type=\"xsd:string\">CM2 9XW</smPostCode><smSatClose xsi:type=\"xsd:string\">2200</smSatClose><smSatOpen xsi:type=\"xsd:string\">0000</smSatOpen><smStore xsi:type=\"xsd:string\">CHELMSFORD MIAMI - PRINCES ROAD</smStore><smSunClose xsi:type=\"xsd:string\">1600</smSunClose><smSunOpen xsi:type=\"xsd:string\">1000</smSunOpen><smTelephone xsi:type=\"xsd:string\"></smTelephone><smThuClose xsi:type=\"xsd:string\">2359</smThuClose><smThuOpen xsi:type=\"xsd:string\">0000</smThuOpen><smTown xsi:type=\"xsd:string\">CHELMSFORD</smTown><smTueClose xsi:type=\"xsd:string\">2359</smTueClose><smTueOpen xsi:type=\"xsd:string\">0000</smTueOpen><smWedClose xsi:type=\"xsd:string\">2359</smWedClose><smWedOpen xsi:type=\"xsd:string\">0000</smWedOpen></multiRef><multiRef id=\"id1\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns4:SupermarketResult\" xmlns:ns4=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><latitude xsi:type=\"xsd:float\">51.733482</latitude><longitude xsi:type=\"xsd:float\">0.48015535</longitude><smAddress xsi:type=\"xsd:string\">Springfield Road</smAddress><smAlcohol xsi:type=\"xsd:boolean\">false</smAlcohol><smChain xsi:type=\"xsd:string\">Iceland</smChain><smClothing xsi:type=\"xsd:boolean\">false</smClothing><smFriClose xsi:type=\"xsd:string\">0</smFriClose><smFriOpen xsi:type=\"xsd:string\" xsi:nil=\"true\"/><smMonClose xsi:type=\"xsd:string\">0</smMonClose><smMonOpen xsi:type=\"xsd:string\" xsi:nil=\"true\"/><smPetrol xsi:type=\"xsd:boolean\">false</smPetrol><smPharmacy xsi:type=\"xsd:boolean\">false</smPharmacy><smPostCode xsi:type=\"xsd:string\">CM2 6JY</smPostCode><smSatClose xsi:type=\"xsd:string\">0</smSatClose><smSatOpen xsi:type=\"xsd:string\" xsi:nil=\"true\"/><smStore xsi:type=\"xsd:string\">CHELMSFORD</smStore><smSunClose xsi:type=\"xsd:string\">0</smSunClose><smSunOpen xsi:type=\"xsd:string\">0</smSunOpen><smTelephone xsi:type=\"xsd:string\"></smTelephone><smThuClose xsi:type=\"xsd:string\">0</smThuClose><smThuOpen xsi:type=\"xsd:string\" xsi:nil=\"true\"/><smTown xsi:type=\"xsd:string\">Chelmsford</smTown><smTueClose xsi:type=\"xsd:string\">0</smTueClose><smTueOpen xsi:type=\"xsd:string\" xsi:nil=\"true\"/><smWedClose xsi:type=\"xsd:string\">0</smWedClose><smWedOpen xsi:type=\"xsd:string\" xsi:nil=\"true\"/></multiRef></soapenv:Body></soapenv:Envelope>")

(lift-supermarkets-results *supermarkets-string*)

(lower-object-field 'INSTANCE6548)

(describe-instance 'instance6566)
(describe-instance 'instance2416)

(lower-supermarket 'instance2368)
(lower-spatial-object 'instance2416)
(lower-supermarket-results 'instance7166)
(lower-object-field 'instance2414)
(lower-supermarket 'instance7845)
(setofall '?x '(= ?x (all-slot-values 'instance7845 'has-affordances)))

(xpm::get-lower-function 'supermarkets-results)
(xpm::get-lower-function 'rest-centres-results)

;;1)
;;(ip::internal-solve-goal 'emergency-gis-goals2 'get-polygon-gis-data-with-filter-goal '((has-method "getHotelsInRadius") (has-polygon-points ((0.54160 61.7479)(0.34160 41.7479)(0.54160 41.7479)(0.34160 61.7479))) (has-spatial-object-query HotelsQuery)))


;;2)
(ip::internal-solve-goal 'emergency-gis-goals2 'get-resources-data-goal '((has-method "getAllInRadius") (has-latitude 51.8582) (has-longitude 0.5643)(radius 10)))

(ip::internal-solve-goal 'emergency-gis-goals2 'resource-filter-goal '((has-resource-data 'instance2477) (has-caller "snow-storm-emergency")))

(select-ontology 'emergency-gis-goals2)
(describe-instance 'instance7413)

(ocml::generate-ocml-instances-source '(err1))

|#

(deflift lift-supermarkets-results supermarkets-results ()
  (("has-objects" (lift-supermarket '("getSupermarketsInRadiusResponse/getSupermarketsInRadiusReturn/getSupermarketsInRadiusReturn")  :AS-VALUES))))

(deflift lift-supermarket supermarket ()
(("has-latitude" "getSupermarketsInRadiusReturn/latitude/text()")
 ("has-longitude" "getSupermarketsInRadiusReturn/longitude/text()")
 ("has-address" "getSupermarketsInRadiusReturn/smAddress/text()")
 ("has-alcohol" "getSupermarketsInRadiusReturn/smAlcohol/text()")
 ("has-chain" "getSupermarketsInRadiusReturn/smChain/text()")
 ("has-clothing" "getSupermarketsInRadiusReturn/smClothing/text()")
 ("has-fri-close" "getSupermarketsInRadiusReturn/smFriClose/text()")
 ("has-fri-open" "getSupermarketsInRadiusReturn/smFriOpen/text()")
 ("has-mon-close" "getSupermarketsInRadiusReturn/smMonClose/text()")
 ("has-mon-open" "getSupermarketsInRadiusReturn/smMonOpen/text()")
 ("has-petrol" "getSupermarketsInRadiusReturn/smPetrol/text()")
 ("has-pharmacy" "getSupermarketsInRadiusReturn/smPharmacy/text()")
 ("has-postcode" "getSupermarketsInRadiusReturn/smPostcode/text()")
 ("has-sat-close" "getSupermarketsInRadiusReturn/smSatClose/text()")
 ("has-sat-open" "getSupermarketsInRadiusReturn/smSatOpen/text()")
 ("has-store" "getSupermarketsInRadiusReturn/smStore/text()")
 ("has-sun-close" "getSupermarketsInRadiusReturn/smSunClose/text()")
 ("has-sun-open" "getSupermarketsInRadiusReturn/smSunOpen/text()")
 ("has-telephone" "getSupermarketsInRadiusReturn/smTelephone/text()")
 ("has-thu-close" "getSupermarketsInRadiusReturn/smThuClose/text()")
 ("has-thu-open" "getSupermarketsInRadiusReturn/smThuOpen/text()")
 ("has-town" "getSupermarketsInRadiusReturn/smTown/text()")
 ("has-tue-close" "getSupermarketsInRadiusReturn/smTueClose/text()")
 ("has-tue-open" "getSupermarketsInRadiusReturn/smTueOpen/text()")
 ("has-wed-close" "getSupermarketsInRadiusReturn/smWedClose/text()")
 ("has-wed-open" "getSupermarketsInRadiusReturn/smWedOpen/text()")
 ("has-location" (eval (sgis-lift-point-location 
                          :lat-xpath "getSupermarketsInRadiusReturn/latitude/text()"
                          :long-xpath "getSupermarketsInRadiusReturn/longitude/text()")))))

(deflift lift-emergency-resources-results emergency-resources-results ()
(("has-objects" (lift-emergency-resource '("getAllInRadiusResponse/getAllInRadiusReturn/getAllInRadiusReturn")  :AS-VALUES))))

;(deflift lift-snow-storm-emergency-resources-results emergency-resources-results 
;    (:web-services '(get-snow-storm-resources-data-web-service))
;(("has-objects" (lift-emergency-resource '("getAllInRadiusResponse/getAllInRadiusReturn/getAllInRadiusReturn")  :AS-VALUES))))




#|
(deflift lift-snow-storm-emergency-resources-results emergency-resources-results 
    (:web-services '(get-snow-storm-resources-data-web-service))
(("has-objects" (eval (select-resource (lift-emergency-resource '("getAllInRadiusResponse/getAllInRadiusReturn/getAllInRadiusReturn")  :AS-VALUES) 'snow-storm-emergency)))))

(lift-snow-storm-emergency-resources-results *resstring*)

(lift-emergency-resources-results *resstring*)

(describe-instance 'instance2308)

(select-resource 'instance2310 'snow-storm-emergency)

(find-true-slots 'instance2310)

(find-needed-slots 'snow-storm-emergency)

(defun select-resource (resource emergency)
 (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology 'ocml::emergency-gis-domain)
    (let ((result nil) 
          (retrieved-slots nil)
          (expected-slots nil))
         (setf retrieved-slots (find-true-slots resource))
         (setf expected-slots (find-needed-slots emergency))
         (if (intersection retrieved-slots expected-slots) (list resource) nil))))


(defun find-true-slots (resource)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology 'ocml::emergency-gis-domain)
    (let ((result nil))
         (setf result (ocml::findall '?x `(ocml::HAS-SLOT-VALUE ,resource ?x 'ocml::true)))
         (first (list result)))))

(defun find-needed-slots (emergency)
  (ocml::with-ocml-thread-safety ()
   (ocml::select-ontology 'ocml::emergency-gis-domain)
   (let ((result nil)
         (true-slots nil))
        (setf result (ocml::setofall '?x `(ocml::has-slot-value ?x 'ocml::is-suitable-for-emergency ,emergency)))
        (loop for i in result
              do (setf true-slots (append true-slots (find-true-slots i))))
        (first (list true-slots)))))
|#

;;(select-ontology 'emergency-gis-goals2)
;;(describe-instance 'instance2322)

(deflift lift-emergency-resource emergency-resource ()
(("has-latitude" "getAllInRadiusReturn/latitude/text()")
 ("has-longitude" "getAllInRadiusReturn/longitude/text()")
 ("has-res24hrNo" "getAllInRadiusReturn/res24hrNo/text()") 
 ("has-res2nd24hrNo" "getAllInRadiusReturn/res2nd24hrNo/text()") 
 ("has-res2ndContact" "getAllInRadiusReturn/res2ndContact/text()") 
 ("has-resAccessEquip" "getAllInRadiusReturn/resAccessEquip/text()") 
 ("has-resAddress1" "getAllInRadiusReturn/resAddress1/text()") 
 ("has-resAddress2" "getAllInRadiusReturn/resAddress2/text()") 
 ("has-resAerialPhotography" "getAllInRadiusReturn/resAerialPhotography/text()") 
 ("has-resBarrierAndControlSystems" "getAllInRadiusReturn/resBarrierAndControlSystems/text()") 
 ("has-resBrochureAvailable" "getAllInRadiusReturn/resBrochureAvailable/text()") 
 ("has-resChemAndBiodetections" "getAllInRadiusReturn/resChemAndBiodetections/text()") 
 ("has-resCleaningAndRecoverySystem" "getAllInRadiusReturn/resCleaningAndRecoverySystem/text()") 
 ("has-resCompanyName" "getAllInRadiusReturn/resCompanyName/text()") 
 ("has-resConferenceSystems" "getAllInRadiusReturn/resConferenceSystems/text()") 
 ("has-resContactName" "getAllInRadiusReturn/resContactName/text()") 
 ("has-resCounty" "getAllInRadiusReturn/resCounty/text()") 
 ("has-resDecontaminationEquipment" "getAllInRadiusReturn/resDecontaminationEquipment/text()") 
 ("has-resDecontaminationKitsAndProducts" "getAllInRadiusReturn/resDecontaminationKitsAndProducts/text()") 
 ("has-resDriersAndDehumid" "getAllInRadiusReturn/resDriersAndDehumid/text()") 
 ("has-resDrinkingWater" "getAllInRadiusReturn/resDrinkingWater/text()") 
 ("has-resEarthMovingEquip" "getAllInRadiusReturn/resEarthMovingEquip/text()") 
 ("has-resEmergencyBedding" "getAllInRadiusReturn/resEmergencyBedding/text()") 
 ("has-resEmergencyBeds" "getAllInRadiusReturn/resEmergencyBeds/text()") 
 ("has-resEmergencyBlanketsAndCapes" "getAllInRadiusReturn/resEmergencyBlanketsAndCapes/text()") 
 ("has-resEmergencyClothingAndFootwear" "getAllInRadiusReturn/resClothingAndFootwear/text()") 
 ("has-resEmergencyFood" "getAllInRadiusReturn/resEmergencyFood/text()") 
 ("has-resFax" "getAllInRadiusReturn/resFax/text()") 
 ("has-resGenerators" "getAllInRadiusReturn/resGenerators/text()") 
 ("has-resHeatersAndCoolers" "getAllInRadiusReturn/resHeatersAndCoolers/text()") 
 ("has-resHotWashFacilities" "getAllInRadiusReturn/resHotWashFacilities/text()") 
 ("has-resHygieneProducts" "getAllInRadiusReturn/resHygieneProducts/text()") 
 ("has-resId" "getAllInRadiusReturn/resId/text()") 
 ("has-resLighting" "getAllInRadiusReturn/resLighting/text()") 
 ("has-resMethod" "getAllInRadiusReturn/resMethod/text()") 
 ("has-resMisc" "getAllInRadiusReturn/resMisc/text()") 
 ("has-resOilWaterAbsorbing" "getAllInRadiusReturn/resOilWaterAbsorbing/text()") 
 ("has-resPortableToilets" "getAllInRadiusReturn/resPortableToilets/text()") 
 ("has-resPortacabinsAndPortableShelter" "getAllInRadiusReturn/resPortacabinsAndPortableShelter/text()") 
 ("has-resPosition" "getAllInRadiusReturn/resPosition/text()") 
 ("has-resPostcode" "getAllInRadiusReturn/resPostcode/text()") 
 ("has-resPressureWashers" "getAllInRadiusReturn/resPressureWashers/text()") 
 ("has-resProtectiveClothing" "getAllInRadiusReturn/resProtectiveClothing/text()") 
 ("has-resReply" "getAllInRadiusReturn/resReply/text()") 
 ("has-resRescueCraft" "getAllInRadiusReturn/resRescueCraft/text()") 
 ("has-resSandbaggingMachines" "getAllInRadiusReturn/resSandbaggingMachines/text()") 
 ("has-resSandbags" "getAllInRadiusReturn/resSandbags/text()") 
 ("has-resSiteClearenceDemolition" "getAllInRadiusReturn/resSiteClearenceDemolition/text()") 
 ("has-resSiteClearencePlant" "getAllInRadiusReturn/resClearencePlant/text()") 
 ("has-resTelNo" "getAllInRadiusReturn/resTelNo/text()") 
 ("has-resTempMortuaryEquip" "getAllInRadiusReturn/resTempMortuaryEquip/text()") 
 ("has-resTemporaryCateringFacilities" "getAllInRadiusReturn/resTemporaryCateringFacilities/text()") 
 ("has-resTown" "getAllInRadiusReturn/resTown/text()") 
 ("has-resTranslators" "getAllInRadiusReturn/resTranslators/text()") 
 ("has-resTwoWayRadios" "getAllInRadiusReturn/resTwoWayRadios/text()") 
 ("has-resType" "getAllInRadiusReturn/resType/text()") 
 ("has-resType1" "getAllInRadiusReturn/resType1/text()") 
 ("has-resUniqueNo" "getAllInRadiusReturn/resUniqueNo/text()") 
 ("has-resWaterpumps" "getAllInRadiusReturn/resWaterpumps/text()") 
 ("has-resXRayAndMedical" "getAllInRadiusReturn/resXRayAndMedical/text()")
 ("has-location" (eval (sgis-lift-point-location 
                          :lat-xpath "getAllInRadiusReturn/latitude/text()"
                          :long-xpath "getAllInRadiusReturn/longitude/text()")))))

#|
(def-instance myHospital hospital
((has-latitude 0.0638580322265625)
 (has-longitude 52.01193653675363)
 (has-address "Great Chishill")
 (has-beds 40)
 (has-text-location "Essex")
 (has-name "Chelmsford & Essex Hospital")
 (has-postcode "CM1 7LF")
 (has-telephone "01245 91149")))


(defparameter *hospital-string*
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soapenv:Body><getHospitalsInRadiusResponse soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><getHospitalsInRadiusReturn soapenc:arrayType=\"ns1:HospitalResult[1]\" xsi:type=\"soapenc:Array\" xmlns:ns1=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><getHospitalsInRadiusReturn href=\"#id0\"/></getHospitalsInRadiusReturn></getHospitalsInRadiusResponse><multiRef id=\"id0\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns2:HospitalResult\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:ns2=\"http://www.bt.com\"><hAddress xsi:type=\"xsd:string\">New London Road,</hAddress><hBeds xsi:type=\"xsd:int\">0</hBeds><hLocation xsi:type=\"xsd:string\">Chelmsford</hLocation><hName xsi:type=\"xsd:string\">Chelmsford &amp; Essex Hospital</hName><hPostCode xsi:type=\"xsd:string\">CM1 7LF</hPostCode><hTelephone xsi:type=\"xsd:string\">01245 91149</hTelephone><latitude xsi:type=\"xsd:float\">51.73113</latitude><longitude xsi:type=\"xsd:float\">0.47122172</longitude></multiRef></soapenv:Body></soapenv:Envelope>")
|#

#|
(lift-hospitals-results *hospital-string*)

(describe-instance 'INSTANCE2567)

(lower-hospital 'INSTANCE9660)

(lower-object-field '(list 'INSTANCE2567))

(lower-hospital 'myHospital)
(lower-spatial-object 'INSTANCE9654)

(setofall '?x '(has-hover-content instance5707 ?x))


(eval '(ocml-eval (all-slot-values 'INSTANCE5602 'has-name)))
(eval '(ocml-eval (all-slot-values 'INSTANCE5602 'has-hover-content)))
(ask (has-hover-content 'INSTANCE5471 ?val))
(eval '(ocml-eval (findany '?x (spatial-object-attributes 'INSTANCE5471 '?x))))

(ask (spatial-object-attributes myHospital ?attrs))
;;((HAS-LATITUDE 0.0638580322265625) (HAS-LONGITUDE 52.01193653675363) (HAS-ADDRESS "Great Chishill") (HAS-BEDS 40) (HAS-NAME "Chelmsford & Essex Hospital") (HAS-POSTCODE "CM1 7LF") (HAS-TELEPHONE "01245 91149"))
|#

;;#|

(defparameter *resstring* "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soapenv:Body><getAllInRadiusResponse soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><getAllInRadiusReturn soapenc:arrayType=\"ns1:ResourcesResult[1]\" xsi:type=\"soapenc:Array\" xmlns:ns1=\"http://www.bt.com\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"><getAllInRadiusReturn href=\"#id0\"/><getAllInRadiusReturn href=\"#id1\"/></getAllInRadiusReturn></getAllInRadiusResponse><multiRef id=\"id0\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns2:ResourcesResult\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:ns2=\"http://www.bt.com\"><latitude xsi:type=\"xsd:float\">51.879303</latitude><longitude xsi:type=\"xsd:float\">0.550707</longitude><res24hrNo xsi:type=\"xsd:string\">0870 774 3020</res24hrNo><res2nd24hrNo xsi:type=\"xsd:string\" xsi:nil=\"true\"/><res2ndContact xsi:type=\"xsd:string\" xsi:nil=\"true\"/><resAccessEquip xsi:type=\"xsd:boolean\">false</resAccessEquip><resAddress1 xsi:type=\"xsd:string\">The Pines, Lynderswood Farm</resAddress1><resAddress2 xsi:type=\"xsd:string\">London Road</resAddress2><resAerialPhotography xsi:type=\"xsd:boolean\">false</resAerialPhotography><resBarrierAndControlSystems xsi:type=\"xsd:boolean\">false</resBarrierAndControlSystems><resBrochureAvailable xsi:type=\"xsd:boolean\">false</resBrochureAvailable><resChemAndBiodetections xsi:type=\"xsd:boolean\">false</resChemAndBiodetections><resCleaningAndRecoverySystem xsi:type=\"xsd:boolean\">false</resCleaningAndRecoverySystem><resCompanyName xsi:type=\"xsd:string\">RAD Interpreting Service</resCompanyName><resConferenceSystems xsi:type=\"xsd:boolean\">false</resConferenceSystems><resContactName xsi:type=\"xsd:string\">Leanne Dovey</resContactName><resCounty xsi:type=\"xsd:string\">Essex</resCounty><resDecontaminationEquipment xsi:type=\"xsd:boolean\">false</resDecontaminationEquipment><resDecontaminationKitsAndProducts xsi:type=\"xsd:boolean\">false</resDecontaminationKitsAndProducts><resDriersAndDehumid xsi:type=\"xsd:boolean\">true</resDriersAndDehumid><resDrinkingWater xsi:type=\"xsd:boolean\">false</resDrinkingWater><resEarthMovingEquip xsi:type=\"xsd:boolean\">false</resEarthMovingEquip><resEmergencyBedding xsi:type=\"xsd:boolean\">false</resEmergencyBedding><resEmergencyBeds xsi:type=\"xsd:boolean\">false</resEmergencyBeds><resEmergencyBlanketsAndCapes xsi:type=\"xsd:boolean\">true</resEmergencyBlanketsAndCapes><resEmergencyClothingAndFootwear xsi:type=\"xsd:boolean\">false</resEmergencyClothingAndFootwear><resEmergencyFood xsi:type=\"xsd:boolean\">false</resEmergencyFood><resFax xsi:type=\"xsd:string\">0970 774 3539</resFax><resGenerators xsi:type=\"xsd:boolean\">false</resGenerators><resHeatersAndCoolers xsi:type=\"xsd:boolean\">false</resHeatersAndCoolers><resHotWashFacilities xsi:type=\"xsd:boolean\">false</resHotWashFacilities><resHygieneProducts xsi:type=\"xsd:boolean\">false</resHygieneProducts><resId xsi:type=\"xsd:int\">90</resId><resLighting xsi:type=\"xsd:boolean\">false</resLighting><resMethod xsi:type=\"xsd:string\">Form returned</resMethod><resMisc xsi:type=\"xsd:boolean\">false</resMisc><resOilWaterAbsorbing xsi:type=\"xsd:boolean\">false</resOilWaterAbsorbing><resPortableToilets xsi:type=\"xsd:boolean\">false</resPortableToilets><resPortacabinsAndPortableShelter xsi:type=\"xsd:boolean\">false</resPortacabinsAndPortableShelter><resPosition xsi:type=\"xsd:string\">Co-ordinating Manager</resPosition><resPostcode xsi:type=\"xsd:string\">CM77 8QN</resPostcode><resPressureWashers xsi:type=\"xsd:boolean\">false</resPressureWashers><resProtectiveClothing xsi:type=\"xsd:boolean\">false</resProtectiveClothing><resReply xsi:type=\"xsd:boolean\">false</resReply><resRescueCraft xsi:type=\"xsd:boolean\">false</resRescueCraft><resSandbaggingMachines xsi:type=\"xsd:boolean\">false</resSandbaggingMachines><resSandbags xsi:type=\"xsd:boolean\">false</resSandbags><resSiteClearenceDemolition xsi:type=\"xsd:boolean\">false</resSiteClearenceDemolition><resSiteClearencePlant xsi:type=\"xsd:boolean\">false</resSiteClearencePlant><resTelNo xsi:type=\"xsd:string\">0800 004 3030</resTelNo><resTempMortuaryEquip xsi:type=\"xsd:boolean\">false</resTempMortuaryEquip><resTemporaryCateringFacilities xsi:type=\"xsd:boolean\">false</resTemporaryCateringFacilities><resTown xsi:type=\"xsd:string\">Braintree</resTown><resTranslators xsi:type=\"xsd:boolean\">false</resTranslators><resTwoWayRadios xsi:type=\"xsd:boolean\">false</resTwoWayRadios><resType xsi:type=\"xsd:string\" xsi:nil=\"true\"/><resType1 xsi:type=\"xsd:string\" xsi:nil=\"true\"/><resUniqueNo xsi:type=\"xsd:string\">036</resUniqueNo><resWaterpumps xsi:type=\"xsd:boolean\">false</resWaterpumps><resXRayAndMedical xsi:type=\"xsd:boolean\">false</resXRayAndMedical></multiRef><multiRef id=\"id1\" soapenc:root=\"0\" soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xsi:type=\"ns2:ResourcesResult\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:ns2=\"http://www.bt.com\"><latitude xsi:type=\"xsd:float\">51.879303</latitude><longitude xsi:type=\"xsd:float\">0.550707</longitude><res24hrNo xsi:type=\"xsd:string\">0870 774 3020</res24hrNo><res2nd24hrNo xsi:type=\"xsd:string\" xsi:nil=\"true\"/><res2ndContact xsi:type=\"xsd:string\" xsi:nil=\"true\"/><resAccessEquip xsi:type=\"xsd:boolean\">false</resAccessEquip><resAddress1 xsi:type=\"xsd:string\">The Pines, Lynderswood Farm</resAddress1><resAddress2 xsi:type=\"xsd:string\">London Road</resAddress2><resAerialPhotography xsi:type=\"xsd:boolean\">false</resAerialPhotography><resBarrierAndControlSystems xsi:type=\"xsd:boolean\">false</resBarrierAndControlSystems><resBrochureAvailable xsi:type=\"xsd:boolean\">false</resBrochureAvailable><resChemAndBiodetections xsi:type=\"xsd:boolean\">false</resChemAndBiodetections><resCleaningAndRecoverySystem xsi:type=\"xsd:boolean\">false</resCleaningAndRecoverySystem><resCompanyName xsi:type=\"xsd:string\">RAD Interpreting Service</resCompanyName><resConferenceSystems xsi:type=\"xsd:boolean\">false</resConferenceSystems><resContactName xsi:type=\"xsd:string\">Leanne Dovey</resContactName><resCounty xsi:type=\"xsd:string\">Essex</resCounty><resDecontaminationEquipment xsi:type=\"xsd:boolean\">false</resDecontaminationEquipment><resDecontaminationKitsAndProducts xsi:type=\"xsd:boolean\">false</resDecontaminationKitsAndProducts><resDriersAndDehumid xsi:type=\"xsd:boolean\">false</resDriersAndDehumid><resDrinkingWater xsi:type=\"xsd:boolean\">false</resDrinkingWater><resEarthMovingEquip xsi:type=\"xsd:boolean\">false</resEarthMovingEquip><resEmergencyBedding xsi:type=\"xsd:boolean\">false</resEmergencyBedding><resEmergencyBeds xsi:type=\"xsd:boolean\">false</resEmergencyBeds><resEmergencyBlanketsAndCapes xsi:type=\"xsd:boolean\">false</resEmergencyBlanketsAndCapes><resEmergencyClothingAndFootwear xsi:type=\"xsd:boolean\">false</resEmergencyClothingAndFootwear><resEmergencyFood xsi:type=\"xsd:boolean\">false</resEmergencyFood><resFax xsi:type=\"xsd:string\">0970 774 3539</resFax><resGenerators xsi:type=\"xsd:boolean\">false</resGenerators><resHeatersAndCoolers xsi:type=\"xsd:boolean\">false</resHeatersAndCoolers><resHotWashFacilities xsi:type=\"xsd:boolean\">false</resHotWashFacilities><resHygieneProducts xsi:type=\"xsd:boolean\">false</resHygieneProducts><resId xsi:type=\"xsd:int\">90</resId><resLighting xsi:type=\"xsd:boolean\">false</resLighting><resMethod xsi:type=\"xsd:string\">Form returned</resMethod><resMisc xsi:type=\"xsd:boolean\">false</resMisc><resOilWaterAbsorbing xsi:type=\"xsd:boolean\">false</resOilWaterAbsorbing><resPortableToilets xsi:type=\"xsd:boolean\">false</resPortableToilets><resPortacabinsAndPortableShelter xsi:type=\"xsd:boolean\">false</resPortacabinsAndPortableShelter><resPosition xsi:type=\"xsd:string\">Co-ordinating Manager</resPosition><resPostcode xsi:type=\"xsd:string\">CM77 8QN</resPostcode><resPressureWashers xsi:type=\"xsd:boolean\">false</resPressureWashers><resProtectiveClothing xsi:type=\"xsd:boolean\">false</resProtectiveClothing><resReply xsi:type=\"xsd:boolean\">false</resReply><resRescueCraft xsi:type=\"xsd:boolean\">false</resRescueCraft><resSandbaggingMachines xsi:type=\"xsd:boolean\">false</resSandbaggingMachines><resSandbags xsi:type=\"xsd:boolean\">false</resSandbags><resSiteClearenceDemolition xsi:type=\"xsd:boolean\">false</resSiteClearenceDemolition><resSiteClearencePlant xsi:type=\"xsd:boolean\">false</resSiteClearencePlant><resTelNo xsi:type=\"xsd:string\">0800 004 3030</resTelNo><resTempMortuaryEquip xsi:type=\"xsd:boolean\">false</resTempMortuaryEquip><resTemporaryCateringFacilities xsi:type=\"xsd:boolean\">false</resTemporaryCateringFacilities><resTown xsi:type=\"xsd:string\">Braintree</resTown><resTranslators xsi:type=\"xsd:boolean\">false</resTranslators><resTwoWayRadios xsi:type=\"xsd:boolean\">false</resTwoWayRadios><resType xsi:type=\"xsd:string\" xsi:nil=\"true\"/><resType1 xsi:type=\"xsd:string\" xsi:nil=\"true\"/><resUniqueNo xsi:type=\"xsd:string\">036</resUniqueNo><resWaterpumps xsi:type=\"xsd:boolean\">false</resWaterpumps><resXRayAndMedical xsi:type=\"xsd:boolean\">false</resXRayAndMedical></multiRef></soapenv:Body></soapenv:Envelope>")

;;(lift-emergency-resources-results *resstring*)

;;(describe-instance 'instance1867)
;;(describe-instance 'instance2581)
;;(describe-instance 'instance2583)
;;(describe-instance 'instance2585)

;;(lower-object-field 'instance2573)

;;|#