;;; 2) ChangeDetailsOfCitizenInterfaceOut -- SWIFTDB

(in-package "OCML")

(in-ontology swift-services-datatypes)

(import 'xpm::deflift)
(import 'xpm::lift)

(deflift lift-new-address
         (:ontology 'swift-services-datatype :class 'new-address)
(("has-postcode" "NewAddress/PostCode/text()")
 ("has-premise-number" "NewAddress/PremiseNumber/text()")
 ("has-premise-name" "NewAddress/PremiseName/text()")
 ("has-street" "NewAddress/Street/text()")
 ("has-locality" "NewAddress/Locality/text()")
 ("has-town" "NewAddress/Town/text()")))

(deflift lift-change-details-of-citizen-request-type
         (:ontology 'swift-services-datatype :class 'change-details-of-citizen-request-type)
(("has-address-key" "ChangeDetailsOfCitizenRequestMessage/AddressKey/text()")
 ("has-new-address" (lift-new-address '("ChangeDetailsOfCitizenRequestMessage")))))


(deflift lift-change-details-of-citizen-response-type
         (:ontology 'swift-services-datatype :class 'change-details-of-citizen-response-type)
(("has-response" "ChangeDetailsOfCitizenRequestMessage/response/text()")))


;;; 3) CitizenAddressByCodeInterfaceOut -- SWIFTDB


(deflift lift-citizen-address-by-citizen-code-request-type
         (:ontology 'swift-services-datatype :class 'citizen-address-by-citizen-code-request-type)
(("has-citizen-key" "CitizenAddressByCitizenCodeRequestMessage/CitizenKey/text()")))


;; !!! Attention .. may be problem with restriction on post code???
;;(def-class post-code-string (string) ?x ;;; postcode constraint: its length is 8 characters maximum
;;  :constraint (< (length ?x) 9))   

(deflift lift-citizen-address
    (:ontology 'swift-services-datatypes :class 'citizen-address)
  (("has-address-key" "CitizenAddress/AddressKey/text()")
   ("has-postcode" "CitizenAddress/Postcode/text()")
   ("has-premise-number" "CitizenAddress/PremiseNumber/text()")
   ("has-premise-name" "CitizenAddress/PremiseName/text()")
   ("has-street" "CitizenAddress/Street/text()")
   ("has-locality" "CitizenAddress/Locality/text()")
   ("has-town" "CitizenAddress/Town/text()")))

(deflift lift-citizen-address-response-type
  (:ontology 'swift-services-datatypes :class 'citizen-address-response-type)
  (("has-citizen-address" (lift-citizen-address '("CitizenAddressResponseMessage")))))


;;; 4) CitizenAddressByNameInterfaceOut -- SWIFTDB



(deflift lift-citizen-address-by-name-request-type
    (:ontology 'swift-services-datatypes :class 'citizen-address-by-name-request-type)
  (("has-family-name" "CitizenAddressByNameRequestMessage/FamilyName/text()")
   ("has-first-names" "CitizenAddressByNameRequestMessage/FirstNames/text()")))



;;; 5) CitizenDataByCitizenCodeInterfaceOut -- SWIFTDB

(deflift lift-citizen-data-by-citizen-code-request-type
         (:ontology 'swift-services-datatype :class 'citizen-data-by-citizen-code-request-type)
(("has-citizen-key" "CitizenDataByCitizenCodeRequestMessage/CitizenKey/text()")))


;;; !!!! this is a proposal for solving the problem of date
(deflift lift-date-of-birth-date (:ontology 'swift-services-datatype :class 'date)
(("has-date" "DateOfBirth/text()")))

(deflift lift-date-of-death-date (:ontology 'swift-services-datatype :class 'date)
(("has-date" "DateOfDeath/text()")))

(deflift lift-approx-date-of-birth-date (:ontology 'swift-services-datatype :class 'date)
(("has-date" "ApproxDateOfBirth/text()")))

(deflift lift-expected-date-of-birth-date (:ontology 'swift-services-datatype :class 'date)
(("has-date" "ExpectedDateOfBirth/text()")))


(deflift lift-citizen-data
    (:ontology 'swift-services-datatypes :class 'citizen-data)
(("has-citizen-key" "CitizenData/CitizenKey/text()")
 ("has-gender-code" "CitizenData/GenderCode/text()")
 ("has-marital-status-code" "CitizenData/MaritalStatusCode/text()")
 ("has-title-code" "CitizenData/TitleCode/text()")
 ("has-ethnicity-code" "CitizenData/EthnicityCode/text()")
 ("has-family-name" "CitizenData/FamilyName/text()")
 ("has-speech-impairment" "CitizenData/SpeechImpairment/text()")
 ("has-hearing-impairment" "CitizenData/HearingImpairment/text()")
 ("has-first-names" "CitizenData/FirstNames/text()")
 ("has-initials" "CitizenData/Initials/text()")
 ("has-date-of-birth" (lift-date-of-birth-date '("CitizenData")))
 ("has-date-of-death" (lift-date-of-death-date '("CitizenData")))
 ("has-approx-date-of-birth" (lift-approx-date-of-birth-date '("CitizenData")))
 ("has-age" "CitizenData/Age/text()")
 ("has-expected-date-of-birth" (lift-expected-date-of-birth-date '("CitizenData")))))


(deflift lift-citizen-data-response-type
  (:ontology 'swift-services-datatypes :class 'citizen-data-response-type)
  (("has-citizen-data" (lift-citizen-data '("CitizenDataResponseMessage")))))

 
;;; 6) CitizenDataByNameInterfaceOut -- SWIFTDB

(deflift lift-citizen-data-by-name-request-type 
    (:ontology 'swift-services-datatype :class 'citizen-data-by-name-request-type)
(("has-family-name" "CitizenDataByNameRequestMessage/FamilyName/text()")
 ("has-first-names" "CitizenDataByNameRequestMessage/FirstNames/text()")))


;;; 7) CreateCitizenRecordInterfaceOut -- SWIFTDB 

(deflift lift-create-citizen-record-response-type 
    (:ontology 'swift-services-datatype :class 'create-citizen-record-response-type)
(("has-response" "CreateCitizenRecordResponseMessage/response/text()")))


(deflift lift-create-citizen-record-request-type
  (:ontology 'swift-services-datatypes :class 'create-citizen-record-request-type)
  (("has-citizen-data" (lift-citizen-data '("CreateCitizenRecordRequestMessage")))))


;;; 8) CreateNewAssessmentForClientInterfaceOut -- SWIFTDB

(deflift lift-create-new-assessment-for-client-request-out 
    (:ontology 'swift-services-datatype :class 'create-new-assessment-for-client-request-out)
(("has-citizen-key" "CreateNewAssessmentForClientRequestMessage/CitizenKey/text()")))


(deflift lift-create-new-assessment-for-client-response-type 
    (:ontology 'swift-services-datatype :class 'create-new-assessment-for-client-response-type)
(("has-assessment-key" "CreateNewAssessmentForClientResponseMessage/AssessmentKey/text()")))

;;; ("has-??" "CreateNewAssessmentForClientResponseMessage/ReferralKey/text()") this is created in WSDL but not in the create-new-assessment-for-client-response-type class


;;; 9) EthnicityByCodeInterfaceOut -- SWIFTDB 


(deflift lift-ethnicity 
    (:ontology 'swift-services-datatype :class 'ethnicity)
(("has-ethnicity-code" "Ethnicity/EthnicityCode/text()")
 ("has-ethnicity-display" "Ethnicity/EthnicityDisplay/text()")
 ("has-ethnicity-description" "Ethnicity/EthnicityDescription/text()")))

(deflift lift-ethnicity-response-type
  (:ontology 'swift-services-datatypes :class 'ethnicity-response-type)
  (("has-ethnicity" (lift-ethnicity '("EthnicityResponseMessage")))))


(deflift lift-ethnicity-by-code-request-type
    (:ontology 'swift-services-datatype :class 'ethnicity-by-code-request-type)
(("has-ethnicity-code" "EthnicityByCodeRequestMessage/EthnicityCode/text()")))

;;; 10) FinalizeServiceInterfaceOut -- SWIFTDB

(deflift lift-finalize-service-response-type
    (:ontology 'swift-services-datatype :class 'finalize-service-response-type)
(("has-response" "FinalizeServiceResponseMessage/response/text()")))



(deflift lift-finalize-service-request-type
    (:ontology 'swift-services-datatype :class 'finalize-service-request-type)
(("has-citizen-key" "FinalizeServiceRequestMessage/CitizenKey/text()")
 ("has-care-item-code" "FinalizeServiceRequestMessage/CareItemCode/text()")))


;;; 11) GenderByCodeInterfaceOut -- SWIFTDB



(deflift lift-gender 
    (:ontology 'swift-services-datatype :class 'gender)
(("has-gender-code" "Gender/GenderCode/text()")
 ("has-gender-display" "Gender/GenderDisplay/text()")
 ("has-gender-description" "Gender/GenderDescription/text()")))


(deflift lift-gender-response-type
  (:ontology 'swift-services-datatypes :class 'gender-response-type)
  (("has-gender" (lift-gender '("GenderResponseMessage")))))


(deflift lift-gender-by-code-request-type
    (:ontology 'swift-services-datatype :class 'gender-by-code-request-type)
(("has-gender-code" "GenderByCodeRequestMessage/GenderCode/text()")))



;;; 12) NotifyCitizenDeceasedInterfaceOut -- SWIFT



(deflift lift-notify-citizen-deceased-response-type
    (:ontology 'swift-services-datatype :class 'notify-citizen-deceased-response-type)
(("has-response" "NotifyCitizenDeceasedResponseMessage/response/text()")))


(deflift lift-notify-citizen-deceased-request-type
    (:ontology 'swift-services-datatype :class 'notify-citizen-deceased-request-type)
(("has-citizen-key" "NotifyCitizenDeceasedRequestMessage/CitizenKey/text()")
 ("has-date-of-death" (lift-date-of-death-date '("NotifyCitizenDeceasedRequestMessage")))))


;;; 13) OrderServiceForAssessmentInterfaceOut -- SWIFT



(deflift lift-order-service-for-assessment-request-type
    (:ontology 'swift-services-datatype :class 'order-service-for-assessment-request-type)
(("has-referral-key" "OrderServiceForAssessmentRequestMessage/ReferralKey/text()")
 ("has-care-item-code" "OrderServiceForAssessmentRequestMessage/CareItemCode/text()")))



(deflift lift-order-service-for-assessment-response-type
    (:ontology 'swift-services-datatype :class 'notify-order-service-for-assessment-response-type)
(("has-response" "OrderServiceForAssessmentResponseMessage/response/text()")))


;;; 16) TitleByCodeInterfaceOut -- SWIFT

(deflift lift-title 
    (:ontology 'swift-services-datatype :class 'title)
(("has-title-code" "Title/TitleCode/text()")
 ("has-title-display" "Title/TitleDisplay/text()")
 ("has-title-description" "Title/TitleDescription/text()")))

;(lift-title "<Title><TitleCode>123</TitleCode><TitleDisplay>blabla</TitleDisplay><TitleDescription>blibli</TitleDescription></Title>")
;(describe-instance 'instance2074)

(deflift lift-title-response-type
  (:ontology 'swift-services-datatypes :class 'title-response-type)
  (("has-title" (lift-title '("TitleResponseMessage")))))


(deflift lift-title-by-code-request-type
    (:ontology 'swift-services-datatype :class 'titke-by-code-request-type)
(("has-title-code" "TitleByCodeRequestMessage/TitleCode/text()")))
