;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology profile)

(def-class profile ()
"Basic profile for somebody"
((has-msg-montacts :type msg-contact)
 (has-employment-identity :type employment-identity)
 (has-common-name :type common-name :cardinality 1)
 (has-address-cards :type address-card)
 (has-facade :type facade)
 (has-legal-identity :type legal-identity)))

(def-class common-name ()
"The way the user likes to be called in every day situations"
((has-analyzed-name :type analyzed-name :cardinality 1)
 (has-CN :type string)
 (has-alt-AN :type string)))

(def-class msg-type ()
"Usage role of the messaging contact"
)

(def-class gender ()
"Gender of the Principal"
)

(def-class msg-contact ()
"generic phone, email, or instant messaging contact"
((has-msg-method :type msg-method)
 (has-msg-technology :type msg-technology)
 (has-msg-type :type msg-type)
 (has-msg-provider :type string)
 (has-msg-account :type string)
 (has-nickname :type string)
 (has-msg-subaccount :type string)))

(def-class legal-identity ()
"official legal identification of the principal"
((has-ids :type id)
 (has-gender :type gender)
 (has-marital-status :type marital-status)
 (has-legal-name :type string)
 (has-date-of-birth :type date)))

(def-class address-card ()
"representation of an address"
((has-address-type :type domicile)
 (has-address :type address)))

(def-class address ()
"commonly used group of postal address fields"
((has-county :type county)
 (has-postal-code :type string)
 (has-locality :type string)
 (has-postal-address :type string)
 (has-street-number :type integer :cardinality 1) ;seems that cardinality is not enforced
 (has-street :type string)
 (has-country :type country)))

(def-instance my-domicile address)
(ask (has-street-number my-domicile ?s))

(def-class analyzed-name ()
"name analyzed into bits and pieces"
(;(haspersonaltitle :type appelation)
 (has-middle-name :type string)
 (has-surname :type string)
 (has-firstname :type string)))

(def-class appelation ()
"title given to a person"
((has-appellation-value :type string))
)

(def-class employment-identity ()
"minimal employer and employment details"
((has-alternate-organization :type string)
 (has-job-title :type string)
 (has-organization :type string)))

(def-class any-uri (string))

(def-class facade ()
"profiles look and sound facade"
((has-name-pronounced :type any-uri)
 (has-website :type any-uri)
 (has-mugshot :type any-uri)))

(def-class domicile ()
"defines the role of an address-card"
)

(def-class id ()
"an identifier"
((has-name :type string)
 (has-value :type string)))

(def-class county ()
  ((has-pretty-name :type string)
   (has-test-slot :type integer)))

(def-class country ()
  ((has-pretty-name :type string))) 


(def-class marital-status ()
"Marital status, such as single or married"
)

(def-class msg-method ()
"Messaging method associated with this contact or device"
)

(def-class msg-technology ()
"messaging technology or protocol associated with this contact or device"
)

(def-class msg-type ()
)

;(def-class date (string))

(def-instance gender-f gender
  )

(def-instance msg-technology-mms msg-technology
)

(def-instance msg-technology-email msg-technology
)

(def-instance marital-status-commonlawmarriage marital-status
)

(def-instance msg-type-mobile msg-type
)

(def-instance domicile-vacation domicile ;"holiday address"
  )

(def-instance msg-type-personal msg-type
)

(def-instance msg-technology-fax msg-technology
)

(def-instance msg-technology-mim msg-technology
)

(def-instance msg-technology-voip msg-technology
)

(def-instance marital-status-notapplicable marital-status
)

(def-instance marital-status-single marital-status
)

(def-instance domicile-home domicile ;"everyday home"
)

(def-instance msg-technology-pager msg-technology
)

(def-instance marital-status-widowed marital-status
)

(def-instance msg-technology-sms msg-technology
)

(def-instance msg-method-voice msg-method
)

(def-instance msg-technology-pots msg-technology
)

(def-instance gender-m gender ;"male"
)

(def-instance domicile-emergency domicile ;"structured emergency contact"
)

(def-instance msg-technology-irc msg-technology
)

(def-instance msg-method-im msg-method
)

(def-instance msg-technology-msn msg-technology
)

(def-instance marital-status-divorced marital-status
)

(def-instance msg-method-fax msg-method
)

(def-instance msg-method-pager msg-method
)

(def-instance msg-type-work msg-type
)

(def-instance marital-status-married marital-status
)

(def-instance msg-type-vacation msg-type
)

(def-instance domicile-domicile domicile ;"legal residence"
)

(def-instance msg-method-email msg-method
)

(def-instance domicile-work domicile ;"work address, the office where the person works"
)

(def-instance msg-technology-icq msg-technology
)

(def-instance msg-type-emergency msg-type
)

(def-instance msg-technology-aol msg-technology
)

(def-instance marital-status-separated marital-status
)

(def-instance marital-status-dead marital-status
)

(def-instance msg-technology-yahoo msg-technology
)

(def-instance county-essex county)
(def-instance county-bedfordshire county)

;(def-class exclusive-pair (list))


;WAS BEFORE ADMG BECAME WSMO GOAL
;(def-class administrative-goal ()
;  ((has-base-top-classes :type string)
;   (has-removed-slots :type string)
;   (has-optional-slots :type string)
;   (has-exclusive-slots :type list)))
;
;(def-class administrative-goal ()
;  ((has-update)))
;
;(def-relation is-part-of-admg (?c ?s ?admg)
;  :constraint (and (class ?c)
;                (slot ?s)
;                (slot-of ?s ?c)
;                (subclass-of ?admg administrative-goal)))
;(def-function build-pair-class-slot-for-slot (?s) -> ?p
;  :body (list-of (first (classes-with-slot ?s)) ?s))
;(def-function get-admg-elements (?admg) -> ?l
;  :body (map build-pair-class-slot-for-slot (all-class-slot-values ?admg has-update)))

;(ocml-eval (get-admg-elements2 life-event-change-address ))
;(util-filter #'(lambda (tup) (NOT (or (member 'ADMG tup ) (member 'TRANSACTION-ID tup ))) ) (ocml-eval (get-admg-elements2 life-event-change-address )))
;(member tup 'TRANSACTION-ID)  (member tup 'ADMG)

;(def-class admg-life-event-change-address (administrative-goal)
;  ((has-update :value HAS-ADDRESS-TYPE 
;               :value HAS-STREET-NUMBER 
;               :value HAS-STREET 
;               :value HAS-LOCALITY)))

(def-function build-pair-class-slot-for-slot2 (?s) -> ?p
  "since wsmo goals the first element is always the goal, we want the profile, which is the last element"
  :body (list-of (last (classes-with-slot ?s)) ?s))

(def-function get-admg-elements2 (?admg) -> ?l
  "...2 to tackle wsmo goals"
  :body (map build-pair-class-slot-for-slot2 (all-class-slot-values ?admg HAS-INPUT-ROLE)))

;(ocml-eval (get-admg-elements2 'life-event-change-address ))

(def-relation not-nil (?x)
  :iff-def (not (null ?x)))

(def-function admg-solve-general (?transaction-id ?admg ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9) -> ?res
  "returns all elements if not null"
  ;:body (filter (list-of ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9) not-nil))
  :body (list-of (list-of "life-event-change-address-profile-ms" "profile-ms" (list-of "has-address" ?3))))

;(ocml-eval (admg-solve-general 1234 life-event-change-address 1234 5678 9101112))


;(def-function achieve-mapping 




;;;TO ADD A NEW ADM CASE
;
; 1. CREATE GOAL, mediator and ws
; 2. add metadata for slots eventually
; 4. publish function for new ws
; (has-marital-status :type marital-status)
; (has-legal-name :type string)


(setf (logical-pathname-translations "transform")
       '(("ROOT;*"        "C:\\users\\jbd2\\code\\ocml\\library\\v5-0\\domains\\profile\\transform\\")))

;;;; problem to load in 1.4.5!!!
;;(load "transform:root;transform-profilepart-cformdef.lisp")

(defparameter *md-db*
;;;;   """
;;;;   class proprieties (props):
;;;;    label: pretty-name
;;;;   slot proprieties (props):
;;;;    label: pretty-name
;;;;    optional: if not mandatory
;;;;   """
(second `'((cls-path ((transform ocml classes)))
           (typ-path ((transform ocml types)))
           (transform ((ocml ((classes ((ADDRESS-CARD
                                         ((has-address-type 
                                           ((enumeration( ,(ocml::ocml-eval (ocml::all-instances domicile))))
                                            (label ( ("Address Type") ))
                                            (optional ( (t) ))))))
                                        ;(APPELATION
                                       ; ((has-appellation-value 
                                        ;   ((optional ( (t) ))
                                        ;    (label ( ("Appelation") ))))))
                                        (ADDRESS
                                         ((props 
                                           ((label ( ("Address")  ))))
                                          (has-county 
                                           ((optional ( (t) ))
                                            (enumeration ( ,(ocml::ocml-eval (ocml::all-instances county)) ))
                                            (help ( ("Choose your County or it will be deduced from the address if possible") ))))
                                          (has-country 
                                           ((optional ( (t) ))))
                                          (has-street-number 
                                           ((optional ( (t) )))
                                           ((value ( (12) ))))
                                          (has-street 
                                           ((optional ( (t) ))))
                                          (has-locality 
                                           ((optional ( (t) ))))
                                          (has-postal-code 
                                           ((help ( ("If your postal code uniquely identifies your address other fields are optional") ))))
                                          ))))
                              (types 
                               ((string (("string")))
                                (county (("string")))
                                (integer (("integer")))
                                (domicile(("string"))))))))))))


(def-instance country-UK country)

(def-instance my-domicile address
	((has-county county-buckinghamshire)
	 (has-postal-code "MK6 3PA")
	 (has-locality "Milton Keynes")
	 (has-postal-address "7 Woodley")
	 (has-country country-UK)))




;;;;IMPORTANT, JOHN'S SOLUTION FOR MAPPING
;(def-rule family-name-mapping-rule 
;  ((has-family-name ?inst ?x) if 
;   (haslastname ?inst ?x)))



;;;; (def-rule full-name-mapping-rule 
;;;;   ((has-full-name ?inst ?full-name) if 
;;;;    (haslastname ?inst ?x)
;;;;    (hasfirstname ?inst ?y)
;;;;    (= ?full-name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 ;;(irs-wsmo-web-service-registration egov ;;this is the ontology name
  ;;                         get-form-definition-w  ;;this is the method name
  ;;                         xs-cform-def-for-adm-goal ;;this is the function name
  ;;                         )

   ;;(irs-wsmo-web-service-registration egov ;;this is the ontology name
   ;;                        admg-change-address  ;;this is the method name
   ;;                         admg-solve-general ;;this is the function name
   ;;                         )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(in-ontology egov)

;(ocml-eval (ALL-CLASS-SLOTS admg-change-address))
;(ocml-eval (ALL-CLASS-SLOTS get-form-definition-w))

;(irs-wsmo-web-service-registration egov ;;this is the ontology name
;                           admg-change-address  ;;this is the method name
;                            admg-solve-general ;;this is the function name
;                            )

;(ocml-eval (all-class-slot-values life-event-change-address  has-update))
;(ocml-eval (get-admg-elements life-event-change-address))





;;;; ;;;;this is needed for a new administrative goal






;;;; (DEF-CLASS ADMG-CHANGE-ADDRESS-W-CAPABILITY
;;;;            (CAPABILITY)
;;;;            ?CAPABILITY
;;;;            ((USED-MEDIATOR :VALUE ADMG-CHANGE-ADDRESS-M)
;;;;             (HAS-NON-FUNCTIONAL-PROPERTIES
;;;;              :VALUE
;;;;              ADMG-CHANGE-ADDRESS-W-NON-FUNC-PROPS)))

;;;; (def-class admg-change-address-w-interface (interface)
;;;;   ((has-orchestration :value admg-change-address-w-orchestration)
;;;;    (has-choreography :value admg-change-address-w-choreography)))

;;;; (DEF-CLASS ADMG-CHANGE-ADDRESS-W-CHOREOGRAPHY
;;;;            (CHOREOGRAPHY)
;;;;            ((HAS-WEB-SERVICE-HOST :VALUE "137.108.25.26")
;;;;             (HAS-WEB-SERVICE-PORT :VALUE 3001)
;;;;             (HAS-WEB-SERVICE-LOCATION :VALUE "/soap")
;;;;             (HAS-LISP-FUNCTION :VALUE ADMG-SOLVE-GENERAL)))
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








;;;; (DEF-CLASS ADMINISTRATIVE-GOAL-CHANGE-ADDRESS-W-CAPABILITY
;;;;            (CAPABILITY)
;;;;            ?CAPABILITY
;;;;            ((USED-MEDIATOR :VALUE ADMINISTRATIVE-GOAL-CHANGE-ADDRESS-M)
;;;;             (HAS-NON-FUNCTIONAL-PROPERTIES
;;;;              :VALUE
;;;;              ADMG-CHANGE-ADDRESS-W-NON-FUNC-PROPS)))

;;;; (def-class administrative-goal-change-address-w-interface (interface)
;;;;   ((has-choreography :value admg-change-address-w-choreography40081)))

;;;; (DEF-CLASS ADMINISTRATIVE-GOAL-CHANGE-ADDRESS-W-CHOREOGRAPHY
;;;;            (CHOREOGRAPHY)
;;;;            ((HAS-WEB-SERVICE-HOST :VALUE "137.108.25.26")
;;;;             (HAS-WEB-SERVICE-PORT :VALUE 3001)
;;;;             (HAS-WEB-SERVICE-LOCATION :VALUE "/soap")
;;;;             (HAS-LISP-FUNCTION :VALUE ADMG-SOLVE-GENERAL)))

;;;; (DEF-CLASS ADMG-CHANGE-ADDRESS
;;;;            (GOAL)
;;;;            ?GOAL
;;;;            ((HAS-INPUT-ROLE
;;;;              :VALUE
;;;;              TRANSACTION-ID
;;;;              :VALUE
;;;;              ADMG
;;;;              :VALUE
;;;;              HAS-ADDRESS-TYPE
;;;;              :VALUE
;;;;              HAS-STREET-NUMBER
;;;;              :VALUE
;;;;              HAS-STREET
;;;;              :VALUE
;;;;              HAS-LOCALITY)
;;;;             (HAS-INPUT-SOAP-BINDING
;;;;              :VALUE
;;;;              (TRANSACTION-ID "sexpr")
;;;;              :VALUE
;;;;              (ADMG "sexpr")
;;;;              :VALUE
;;;;              (HAS-ADDRESS-TYPE "sexpr")
;;;;              :VALUE
;;;;              (HAS-STREET-NUMBER "sexpr")
;;;;              :VALUE
;;;;              (HAS-STREET "sexpr")
;;;;              :VALUE
;;;;              (HAS-LOCALITY "sexpr"))
;;;;             (HAS-OUTPUT-ROLE :VALUE HAS-RESULT)
;;;;             (HAS-OUTPUT-SOAP-BINDING :VALUE (HAS-RESULT "sexpr"))
;;;;             (TRANSACTION-ID :TYPE STRING)
;;;;             (ADMG :TYPE STRING)
;;;;             (HAS-ADDRESS-TYPE :TYPE STRING)
;;;;             (HAS-STREET-NUMBER :TYPE STRING)
;;;;             (HAS-STREET :TYPE STRING)
;;;;             (HAS-LOCALITY :TYPE STRING)
;;;;             (HAS-RESULT :TYPE STRING)))

;;;; (DEF-CLASS ADMG-CHANGE-ADDRESS-W
;;;;            (WEB-SERVICE)
;;;;            ?WEB-SERVICE
;;;;            ((HAS-CAPABILITY :VALUE ADMG-CHANGE-ADDRESS-W-CAPABILITY)
;;;;             (HAS-INTERFACE
;;;;              :VALUE
;;;;              ADMINISTRATIVE-GOAL-CHANGE-ADDRESS-W-INTERFACE)
;;;;             (HAS-NON-FUNCTIONAL-PROPERTIES
;;;;              :VALUE
;;;;              ADMG-CHANGE-ADDRESS-W-NON-FUNC-PROPS)))

;;;; (DEF-CLASS ADMG-CHANGE-ADDRESS-M
;;;;            (WG-MEDIATOR)
;;;;            ?MEDIATOR
;;;;            ((HAS-SOURCE-COMPONENT :VALUE ADMG-CHANGE-ADDRESS)))

;;;; (DEF-CLASS ADMG-CHANGE-ADDRESS-W-CHOREOGRAPHY40081
;;;;            (CHOREOGRAPHY)
;;;;            ((HAS-WEB-SERVICE-HOST :VALUE "137.108.25.26")
;;;;             (HAS-WEB-SERVICE-PORT :VALUE 3001)
;;;;             (HAS-WEB-SERVICE-LOCATION :VALUE "/soap")
;;;;             (HAS-LISP-FUNCTION :VALUE ADMG-SOLVE-GENERAL)))

