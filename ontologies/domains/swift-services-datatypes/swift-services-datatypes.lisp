;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology swift-services-datatypes)


;;; 2) ChangeDetailsOfCitizenInterfaceOut -- SWIFTDB

(def-class change-details-of-citizen-request-type ()
  ((has-address-key :type integer)
   (has-new-address :type new-address)))

(def-class new-address ()
  ((has-postcode :type string)
   (has-premise-number :type string)
   (has-premise-name :type string)
   (has-street :type string)
   (has-locality :type string)
   (has-town :type string)))

(def-class change-details-of-citizen-response-type ()
  ((has-response :type string)))
 
;;; 3) CitizenAddressByCodeInterfaceOut -- SWIFTDB

(def-class citizen-address-by-citizen-code-request-type ()
  ((has-citizen-key :type integer)))

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

(def-class post-code-string (string) ?x ;;; postcode constraint: its length is 8 characters maximum
  :constraint (< (length ?x) 9))             
 
;;; 4) CitizenAddressByNameInterfaceOut -- SWIFTDB

(def-class citizen-address-by-name-request-type ()
  ((has-family-name :type string)
   (has-first-names :type string)))

(def-class citizen-address-response-type ()
  ((has-citizen-address :type citizen-address))) ;;; already defined in 3)

;;; 5) CitizenDataByCitizenCodeInterfaceOut -- SWIFTDB

(def-class citizen-data-by-citizen-code-request-type ()
  ((has-citizen-key :type integer)))




(def-class citizen-data-response-type ()
  ((has-citizen-data :type citizen-data)))

(def-class citizen-data ()
  ((has-citizen-key :type integer)
   (has-gender-code :type integer)
   (has-marital-status-code :type integer)
   (has-title-code :type integer)
   (has-ethnicity-code :type integer)
   (has-family-name :type string)
   (has-speech-impairment :type boolean) ;;; value defined as boolean in the DB, and as a string with length=1 and Y or N values in WSDL description
   (has-hearing-impairment :type boolean) ;;; value defined as boolean in the DB, and as a string with length=1 and Y or N values in WSDL description
   (has-first-names :type string)
   (has-initials :type string)
   (has-date-of-birth :type date) ;;; class date defined below 
   (has-date-of-death :type date) 
   (has-approx-date-of-birth :type date) 
   (has-age :type integer)
   (has-expected-date-of-birth :type date)
   ))

(def-class date ()
  ((has-date :type string))
)


;;; 6) CitizenDataByNameInterfaceOut -- SWIFTDB

(def-class citizen-data-by-name-request-type ()
  ((has-family-name :type string)
   (has-first-names :type string)))

(def-class citizen-data-response-type ()
  ((has-citizen-data :type citizen-data)))  ;;; already defined in 5)


;;; 7) CreateCitizenRecordInterfaceOut -- SWIFTDB 

(def-class create-citizen-record-response-type ()
  ((has-response :type string)))

(def-class create-citizen-record-request-type ()
  ((has-citizen-data :type citizen-data))) ;;; already defined in 5)


;;; 8) CreateNewAssessmentForClientInterfaceOut -- SWIFTDB

(def-class create-new-assessment-for-client-request-out ()
  ((has-citizen-key :type integer)))

(def-class create-new-assessment-for-client-response-type ()
  ((has-assessment-key :type integer)))

;;; 9) EthnicityByCodeInterfaceOut -- SWIFTDB 

(def-class ethnicity-response-type ()
  ((has-ethnicity :type ethnicity)))

(def-class ethnicity ()
  ((has-ethnicity-code :type integer)
   (has-ethnicity-display :type string)
   (has-ethnicity-description :type string)))

(def-class ethnicity-by-code-request-type ()
  ((has-ethnicity-code :type integer)))

;;; 10) FinalizeServiceInterfaceOut -- SWIFTDB


(def-class finalize-service-response-type ()
  ((has-response :type string)))


(def-class finalize-service-request-type ()
  ((has-citizen-key :type integer)
   (has-care-item-code :type integer)))

;;; 11) GenderByCodeInterfaceOut -- SWIFTDB

(def-class gender-response-type ()
  ((has-gender :type gender)))

(def-class gender ()
  ((has-gender-code :type integer)
   (has-gender-description :type string)
   (has-gender-display :type string)))

(def-class gender-by-code-request-type ()
   ((has-gender-code :type integer)))

;;; 12) NotifyCitizenDeceasedInterfaceOut -- SWIFT

(def-class notify-citizen-deceased-response-type ()
  ((has-response :type string))) 

(def-class notify-citizen-deceased-request-type ()
  ((has-citizen-key :type integer)
   (has-date-of-death :type date))) ;;; type date already defined as string))


;;; 13) OrderServiceForAssessmentInterfaceOut -- SWIFT

(def-class order-service-for-assessment-request-type ()
  ((has-referral-key :type integer)
   (has-care-item-code :type integer)))

(def-class order-service-for-assessment-response-type ()
  ((has-response :type string)))


;;; 16) TitleByCodeInterfaceOut -- SWIFT

(def-class title-response-type ()
  ((has-title :type title)))


(def-class title ()
  ((has-title-code :type integer)
   (has-title-display :type string)
   (has-title-description :type string)))


(def-class title-by-code-request-type ()
  ((has-title-code :type integer)))

