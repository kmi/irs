
(in-package "OCML")

(in-ontology swift-services-datatypes)

;;(import 'xpm::deflift)
;;(import 'xpm::internal-lift)

#|
(def-class change-details-of-citizen-response-type ()
  ((has-response :type string)))
|#

(deflift lift-change-details-of-citizen-response-type change-details-of-citizen-response-type
    (:ontology 'swift-services-datatypes)
  (("has-response" "ChangeDetailsOfCitizenResponseMessage/response/text()")))

(lift-change-details-of-citizen-response-type "
<ChangeDetailsOfCitizenResponseMessage>
  <response>100092904</response>
</ChangeDetailsOfCitizenResponseMessage>")


#|
(def-class citizen-address-response-type ()
  ((has-citizen-address :type citizen-address)))

(def-class citizen-address ()
  ((has-address-key :type integer)
   (has-postcode :type post-code-string)
   (has-premise-number :type integer)  ;;; type positive-integer ??
   (has-premise-name :type string)
   (has-street :type string)
   (has-locality :type string)
   (has-town :type string)))

(lift-citizen-address "
  <CitizenAddress>
    <AddressKey>12</AddressKey>
    <Postcode>mk62pl</Postcode>
    <PremiseNumber>35</PremiseNumber>
    <PremiseName>Richardson Palace</PremiseName>
    <Street>Boycott</Street>
    <town>MK</town>
  </CitizenAddress>")


(deflift lift-citizen-address-response-type citizen-address-response-type
  ()
  (("has-citizen-address" (lift-citizen-address '("CitizenAddressResponseMessage")))))

(lift-citizen-address-response-type "
<CitizenAddressResponseMessage>
  <CitizenAddress>
    <AddressKey>12</AddressKey>
    <Postcode>mk62pl</Postcode>
    <PremiseNumber>35</PremiseNumber>
    <PremiseName>Richardson Palace</PremiseName>
    <Street>Boycott</Street>
    <BDate>12-12-2006</BDate>
    <town>MK</town>
  </CitizenAddress>
</CitizenAddressResponseMessage>")
|#







;(describe-instance 'instance2159)

;(deflift lift-BDate-date
;  (:ontology 'swift-services-datatypes :class 'date)
;  (("has-date" "BDate/text()")))

(deflift lift-date date
  (:ontology 'swift-services-datatypes)
  (("has-date" "text()")))