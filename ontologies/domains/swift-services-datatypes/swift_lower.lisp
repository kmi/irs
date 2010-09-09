
(in-package "OCML")

(in-ontology swift-services-datatypes)

(import 'xpm::deflower)
(import 'xpm::lower)


;;; 2) ChangeDetailsOfCitizenInterfaceOut -- SWIFTDB


(deflower lower-change-details-of-citizen-request-type
'(("ChangeDetailsOfCitizenRequestMessage"
   (("AddressKey" "has-address-key")
    (lower-new-address "has-new-address")
    ("PrevNewAddress" 
     (("EventuallyMyAddress"
       ((lower-new-address "has-new-address")))))))))

(deflower lower-new-address
'(("toto" "has-postcode")
  ("NewAddress"
   (("Postcode" "has-postcode")
    ("PremiseNumber" "has-premise-number")
    ("PremiseName" "has-premise-name")
    ("Street" "has-street")
    ("Locally" "has-locally")
    ("Town" "has-town")))))

#|
(def-instance my-change-details-of-citizen-request change-details-of-citizen-request-type
  ((has-address-key 12345)
   (has-new-address my-new-address)))

(def-instance my-new-address new-address
  ((has-postcode "mk6 2pl")
   (has-premise-number 123)
   (has-premise-name "Richardson Place")
   (has-street "Richardson")
   (has-town "MK")))

(describe-instance 'my-change-details-of-citizen-request) 
(describe-instance 'my-new-address)
  

(lower-new-address 'my-new-address)
(lower-change-details-of-citizen-request-type 'my-change-details-of-citizen-request)
|#

(deflower lower-change-details-of-citizen-response-type
'(("ChangeDetailsOfCitizenResponseMessage"
   (("response" "has-response")))))
 
;;; 3) CitizenAddressByCodeInterfaceOut -- SWIFTDB

(deflower lower-citizen-address-by-citizen-code-request-type
'(("CitizenAddressByCitizenCodeRequestMessage"
   (("CitizenKey" "has-citizen-key")))))

(deflower lower-citizen-address-response-type
'(("CitizenAddressResponseMessage"
  lowe-citizen-address '
;;;   (("CitizenAddress" "has-citizen-address")))))


;;   (( (lower-catalogue-data 'instance??)  "has-catalogue-data"))))) 

;; example of lifting
;;(("has-catalogue-data" (lift-catalogue-data '("CatalogueEntryResponseMessage"))))

;; possible lowering 
;;((lower-catalogue-data '("has-catalogue-data")))




(deflower lower-citizen-address
'(("CitizenAddress"
   (("AddressKey" "has-address-key")
    ("Postcode" "has-postcode")
    ("PremiseNumber" "has-premise-number")
    ("PremiseName" "has-premise-name")
    ("Street" "has-street")
    ("Locality" "has-locality")
    ("Town" "has-town")))))


        
;;; 4) CitizenAddressByNameInterfaceOut -- SWIFTDB

(deflower lower-citizen-address-by-name-request-type
'(("CitizenAddressByNameRequestMessage"
   (("FamilyName" "has-family-name")
    ("FirstNames" "has-first-names")))))

;;;(def-class citizen-address-response-type () ;;; already defined in 3)



;;; 5) CitizenDataByCitizenCodeInterfaceOut -- SWIFTDB

(deflower lower-citizen-data-by-citizen-code-request-type
'(("CitizenDataByCitizenCodeRequestMessage"
   (("CitizenKey" "has-citizen-key")))))



(deflower lower-citizen-data-response-type
'(("CitizenDataResponseMessage"
;;;   (("CitizenData" "has-citizen-adata")))))


(deflower lower-citizen-data
'(("CitizenData"
   (("CitizenKey" "has-citizen-key")
    ("GenderCode" "has-gender-code")
    ("MaritalStatusCode" "has-marital-status-code")
    ("TitleCode" "has-title-code")
    ("EthnicityCode" "has-ethnicity-code")
    ("FamilyName" "has-family-name")
    ("SpeechImpairment" "has-speech-impairment")
    ("HearingImpairment" "has-hearing-impairment")
    ("Initials" "has-initials")
    ("DateOfBirth" "has-date-of-birth")
;;    ("DateOfDeath" "has-date-of-death")
;;    ("ApproxDateOfBirth" "has-approx-date-of-birth")
    ("Age" "has-age")
;;    ("ExpectedDateOfBirth" "has-expected-date-of-birth")))))


;;defined before
;;(def-class date ()
;;  ((has-date :type string))
;;)



;;; 6) CitizenDataByNameInterfaceOut -- SWIFTDB

(deflower lower-citizen-data-by-name-request-type
'(("CitizenDataByNameRequestMessage"
   (("FamilyName" "has-family-name")
    ("FirstNames" "has-first-names")))))

;;; response already defined in 5)


;;; 7) CreateCitizenRecordInterfaceOut -- SWIFTDB 

(deflower lower-create-citizen-record-response-type
'(("CreateCitizenRecordResponseMessage"
   (("response" "has-response")))))


(deflower lower-create-citizen-record-request-type
'(("CreateCitizenRecordRequestMessage"
;;;   (("CitizenData" "has-citizen-adata")))))

;;; 8) CreateNewAssessmentForClientInterfaceOut -- SWIFTDB

(deflower lower-create-new-assessment-for-client-request-out
'(("CreateNewAssessmentForClientRequestMessage"
   (("CitizenKey" "has-citizen-key")))))

(deflower lower-create-new-assessment-for-client-response-out
'(("CreateNewAssessmentForClientResponseMessage"
   (("AssessmentKey" "has-assessment-key")))))

;;; 9) EthnicityByCodeInterfaceOut -- SWIFTDB 

(deflower lower-ethnicity-response-type
'(("EthnicityResponseMessage"
;;;   (("Ethnicity" "has-ethnicity")))))


(deflower lower-ethnicity
'(("Ethnicity"
   (("EthnicityCode" "has-ethnicity-code")
    ("EthnicityDisplay" "has-ethnicity-display")
    ("EthnicityDescription" "has-ethnicity-description")))))


(deflower lower-ethnicity-by-code-request-type
'(("EthnicityByCodeRequestMessage"
   (("EthnicityCode" "has-ethnicity-code")))))

;;; 10) FinalizeServiceInterfaceOut -- SWIFTDB




(deflower lower-finalize-service-response-type 
'(("FinalizeServiceResponseMessage"
   (("response" "has-response")))))

(deflower lower-finalize-service-request-type
'(("FinalizeServiceRequestMessage"
   (("CitizenKey" "has-citizen-key")
    ("CareItemCode" "has-care-item-code")))))



;;; 11) GenderByCodeInterfaceOut -- SWIFTDB




(deflower lower-gender-response-type
'(("GenderResponseMessage"
;;;   (("Ethnicity" "has-ethnicity")))))


(deflower lower-gender
'(("Gender"
   (("GenderCode" "has-gender-code")
    ("GenderDisplay" "has-gender-display")
    ("GenderDescription" "has-gender-description")))))


(deflower lower-gender-by-code-request-type
'(("GenderByCodeRequestMessage"
   (("GenderCode" "has-gender-code")))))


;;; 12) NotifyCitizenDeceasedInterfaceOut -- SWIFT


(deflower lower-notify-citizen-deceased-response-type
'(("NotifyCitizenDeceasedResponseMessage"
   (("response" "has-response")))))

(deflower lower-notify-citizen-deceased-request-type
'(("NotifyCitizenDeceasedRequestMessage"
   (("CitizenKey" "has-citizen-key")
;;    ("DateOfDeath" "Date")))))



;;; 13) OrderServiceForAssessmentInterfaceOut -- SWIFT



(deflower lower-order-service-for-assessment-request-type
'(("OrderServiceForAssessmentRequestMessage"
   (("ReferralKey" "has-referral-key")
    ("CareItemCode" "has-care-item-code")))))


(deflower lower-order-service-for-assessment-response-type
'(("OrderServiceForAssessmentResponseMessage"
   (("response" "has-response")))))


;;; 16) TitleByCodeInterfaceOut -- SWIFT

(deflower lower-title-response-type
'(("TitleResponseMessage"
;;;   (("Title" "has-title")))))


(deflower lower-title
'(("Title"
   (("TitleCode" "has-title-code")
    ("TitleDisplay" "has-title-display")
    ("TitleDescription" "has-title-description")))))


(deflower lower-title-by-code-request-type
'(("TitleByCodeRequestMessage"
   (("TitleCode" "has-title-code")))))
