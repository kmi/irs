;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology swift-services-ontology)

(def-class citizen-address ()
  ((has-address-key :type integer)
   (has-postcode :type post-code-string)
   (has-premise-number :type integer)  ;;; type positive-integer ??
   (has-premise-name :type string)
   (has-street :type string)
   (has-locality :type string)
   (has-town :type string)))
   
(def-class citizen-data ()
  ((has-CitizenKey :type integer)
   (has-GenderCode :type integer)
   (has-MaritalStatusCode :type integer) 
   (has-TitleCode :type integer)
   (has-EthnicityCode :type integer)
   (has-FamilyName :type string)
   (has-SpeechImpairment :type string)
   (has-HearingImpairment :type string)
   (has-FirstNames :type string)
   (has-Initials :type string)
   (has-DateOfBirth :type date)
   (has-DateOfDeath :type date)
   (has-ApproxDateOfBirth :type date)
   (has-Age :type integer)
   (has-ExpectedDateOfBirth :type date))) 
   
          