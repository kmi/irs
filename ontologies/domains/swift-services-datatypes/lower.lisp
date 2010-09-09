
(in-package "OCML")

(in-ontology swift-services-datatypes)

;;(import 'xpm::deflower)
;;(import 'xpm::lower)


(deflower lower-citizen-address change-details-of-citizen-response-type
  '(("CitizenAddress"
     (("AddressKey" "has-address-key")
      ("Postcode" "has-postcode")
      ("PremiseNumber" "has-premise-number")))))


;(lower-citizen-address 'instance2159)


;(lower (:instance 'instance2203)
;    '(("CitizenAddress"
;       (lower (:instance 'instance2203)
;         '(("AddressKey" "has-address-key")
;           ("Postcode" "has-postcode")
;           ("PremiseNumber" "has-premise-number"))))))


;;<CitizenAddress><AddressKey>12</AddressKey><Postcode>mk62pl</Postcode><PremiseNumber>35</PremiseNumber></CitizenAddress>




