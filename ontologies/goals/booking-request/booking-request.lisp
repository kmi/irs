;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology booking-request)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; locations members ;;;;;;;

;;; format spaces need only to be assigned for rather "complex" types (e.g. dates)

;; date
(def-class br-date ()
((has-format :type #_cs:format-member)
 (refined-by :type #_cs:temporal-quality-member)
 (has-value :type string)))

;; country
(def-class br-country ()
((has-country-title :type string)
 (refined-by :type #_cs:location-member)
))

(def-instance br-uk-1 br-country
((has-country-title "UK1")
 (refined-by #_cs:p6-1-location-uk)))

(def-instance br-france-1 br-country
((has-country-title "France1")
 (refined-by #_cs:p7-1-location-france)))

(def-instance br-germany-1 br-country
((has-country-title "Germany1")
 (refined-by #_cs:p8-1-location-germany)))

(def-instance br-uk br-country
((has-country-title "UK")
 (refined-by #_cs:p6-location-uk)))

(def-instance br-france br-country
((has-country-title "France")
 (refined-by #_cs:p7-location-france)))

(def-instance br-germany br-country
((has-country-title "Germany")
 (refined-by #_cs:p8-location-germany)))

(def-instance br-spain br-country
((has-country-title "Spain")
 (refined-by #_cs:p9-location-spain)))

#|
(def-class br-country-member (#_cs:location-member)
((has-br-title :type string))
)
|#

;; price

(def-class br-price ()
((has-currency :type String)
 (has-value :type float)
 (has-format :type #_cs:price-format-member)
 (refined-by :type #_cs:price-member)))

#|
(def-class br-currency ()
((has-currency-title :type string)
(refined-by :type #_cs:location-member)
))
|#

(def-instance br-1-euro br-price
((has-currency "Euro")
 (has-value 1.0)
 (has-format #_cs:pr1-euro-price-format)
(refined-by #_cs:c1-1-euro-price)
))

(def-instance br-1-gbp br-price
((has-currency "Gbp")
 (has-value 1.0)
 (has-format #_cs:pr2-gbp-price-format)
 (refined-by #_cs:c3-1-gbp-price)
))

(def-instance br-1-dollar br-price
((has-currency "Dollar")
 (has-value 1.0)
 (has-format #_cs:pr3-dollar-price-format)
 (refined-by #_cs:c2-1-dollar-price)
))

(def-class br-currency ()
((has-title :type String)
 (refined-by :type #_cs:price-member)))

(def-instance br-euro br-currency
((has-title "Euro")
(refined-by #_cs:c1-1-euro-price)))
 
(def-instance br-dollar br-currency
((has-title "Dollar")
(refined-by #_cs:c2-1-dollar-price)))

(def-instance br-gbp br-currency
((has-title "GBP")
(refined-by #_cs:c3-1-gbp-price)))
 
;;; in principle, to perform mediation, just the input value (i.e. the concept) plus the format space is required. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; OLD BOOKING REQUEST SWS DESCRIPTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; get-booking goal and med

(DEF-CLASS GET-Booking-Request-GOAL-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get-Booking-request-GOAL
           (GOAL)
           ?GOAL
           ((HAS-INPUT-ROLE :VALUE has-method :VALUE has-departure-city :VALUE has-departure-country :VALUE has-arrival-city :VALUE has-arrival-country :VALUE has-outbound-date :VALUE has-return-date)
            (HAS-INPUT-SOAP-BINDING
             :VALUE
             (has-method "string")
             :VALUE
             (has-departure-city "string")
             :VALUE
             (has-departure-country "string")
             :VALUE
             (has-arrival-city "string")
             :VALUE
             (has-arrival-country "string")
             :VALUE
             (has-outbound-date "string")
             :VALUE
             (has-return-date "string"))
            (HAS-OUTPUT-ROLE :VALUE has-proposal)
            (HAS-OUTPUT-SOAP-BINDING
             :VALUE
             (has-proposal "xml"))
            (has-method :TYPE STRING)
            (has-departure-city :TYPE String)
            (has-departure-country :TYPE String)
         (has-arrival-city :TYPE String)
            (has-arrival-country :TYPE String)
         (has-outbound-date :TYPE community-date)
            (has-return-date :TYPE community-date)
            (has-proposal :TYPE String)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get-booking-request-GOAL-NON-FUNCTIONAL-PROPERTIES)))


#|
;;; new goal with mediation (defined as part of the lowering/lifting
(DEF-CLASS Get-Booking-request-GOAL
           (GOAL)
           ?GOAL
           ((HAS-INPUT-ROLE :VALUE has-method :VALUE has-departure-city :VALUE has-departure-country :VALUE has-arrival-city :VALUE has-arrival-country :VALUE has-outbound-date :VALUE has-return-date)
            (HAS-INPUT-SOAP-BINDING
             :VALUE
             (has-method "string")
             :VALUE
             (has-departure-city "string")
             :VALUE
             (has-departure-country "string")
             :VALUE
             (has-arrival-city "string")
             :VALUE
             (has-arrival-country "string")
             :VALUE
             (has-outbound-date "string")
             :VALUE
             (has-return-date "string"))
            (HAS-OUTPUT-ROLE :VALUE has-proposal)
            (HAS-OUTPUT-SOAP-BINDING
             :VALUE
             (has-proposal "xml"))
            (has-method :TYPE STRING)
            (has-departure-city :TYPE String)
            (has-departure-country :TYPE String)
         (has-arrival-city :TYPE String)
            (has-arrival-country :TYPE String)
         (has-outbound-date :TYPE community-date)
            (has-return-date :TYPE community-date)
            (has-proposal :TYPE travel-proposal)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get-booking-request-GOAL-NON-FUNCTIONAL-PROPERTIES)))
|#


(def-class community-date ()
((has-date :type string))
)

;;(def-instance date-x community-date
;;((has-date "05:02:2008")))

;;(lower-datex "05:02:2008")

#|
(deflower lower-datex community-date 
  (lambda (string)
   (let ()
   (setf dd (subseq string 0 2))
   (setf mm (subseq string 3 5))
   (replace string mm :end1 2)
   (replace string dd :start1 3)
   )
   (let ()
   (setf string string)
   ))   
   )
|#

;;; the following lowering was used in the www book chapter example.

#|
(deflower lower-date community-date 
  (lambda (string)
      (setf current-goal (ocml::findany '?x `(and (or (get-booking-request-goal ?x)
                                                      (get2-booking-request-goal ?x)
                                                      (get3-booking-request-goal ?x))
                                                  (not (exists ?y (has-effect ?x ?y))))))
      (if (not (equalp current-goal
                       :nothing))
          (let ()
            (set-slot-value current-goal 'has-effect string)
            (setf country (the-slot-value current-goal 'HAS-departure-country))
            ))
    (if (string-equal country "Uk")
   (let ()
   (setf dd (subseq string 0 2))
   (setf mm (subseq string 3 5))
   (replace string mm :end1 2)
   (replace string dd :start1 3)
   )
   (let ()
   (setf string string)
   ))   
   ))
|#


#|
(deflift lift-lo-uhp-metadata-string lo-uhp-metadata-string ()
  (lambda (instance)
    (let ((lo-instance nil)
          (lo-list nil)
          (string-list)
          (value-list nil))
      (setf lo-instance (lift-lo-metadata-list (replace-null (filter-french instance))))
      (setf string-list (setofall '?x `(has-lo-metadata ,lo-instance ?x)))
      ;; reversing the string list to have the same order of the input
      (loop for x in string-list
            do
            (setf value-list (append (list x) value-list)))
      ;; get inputs
    
      (setf current-goal (ocml::findany '?x `(and (get-uhp-lo-metadata-goal ?x)
                                                  (not (exists ?y (has-effect ?x ?y))))))
      (if (not (equalp current-goal
                       :nothing))
          (let ()
            (set-slot-value current-goal 'has-effect lo-instance)
            (setf lo-list (get-list-from-string (the-slot-value current-goal 'HAS-LO-REFS)))
            (setf attribute-list (get-list-from-string (the-slot-value current-goal 'HAS-ATTRIBUTES)))
            ))
   
   (if (not value-list)
          (print "<LOs></LOs>")
        (lower-lo-uhp-metadata-string attribute-list lo-list value-list))))) 
|#


(DEF-CLASS Get-booking-request-MED-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get-booking-request-MED
           (WG-MEDIATOR)
           ?MEDIATOR
           ((HAS-SOURCE-COMPONENT :VALUE Get-booking-request-GOAL)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get-booking-request-MED-NON-FUNCTIONAL-PROPERTIES)))

;;; get france booking WS

(DEF-CLASS Get-FRANCE-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get-FRANCE-BOOKING-REQUEST-WS
           (WEB-SERVICE)
           ?WEB-SERVICE
             ((HAS-CAPABILITY :VALUE Get-FRANCE-BOOKING-REQUEST-WS-CAPABILITY)
            (HAS-INTERFACE :VALUE Get-FRANCE-BOOKING-REQUEST-INTERFACE)
      ;;;      (USED-MEDIATOR :VALUE Get-FRANCE-BOOKING-REQUEST-MED)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get-FRANCE-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get-FRANCE-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get-FRANCE-BOOKING-REQUEST-WS-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
      ((USED-MEDIATOR :VALUE Get-booking-request-MED)
            (HAS-ASSUMPTION
             :VALUE
             (KAPPA
              (?WEB-SERVICE)
             (NOT (= (WSMO-ROLE-VALUE ?WEB-SERVICE 'HAS-DEPARTURE-COUNTRY) 
                 "Uk"))))
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get-FRANCE-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get-FRANCE-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get-FRANCE-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING
             :VALUE
             ((GROUNDED-TO-HTTP
               (NORMAL
                (NIL (METHOD departurecity departurecountry arrivalcity arrivalcountry outbounddate returndate)))))))) 

(DEF-CLASS Get-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
           (PROBLEM-SOLVING-PATTERN)
           NIL)

(DEF-CLASS Get-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             Get-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS Get-FRANCE-BOOKING-REQUEST-INTERFACE
           (INTERFACE)
           ?INTERFACE
           ((HAS-CHOREOGRAPHY
             :VALUE
             Get-FRANCE-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY)
            (HAS-ORCHESTRATION
             :VALUE
             Get-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get-FRANCE-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get-FRANCE-BOOKING-REQUEST-WS-PUBLISHER-INFORMATION
           (PUBLISHER-INFORMATION)
           ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE
             :VALUE
             Get-FRANCE-BOOKING-REQUEST-INTERFACE)
            (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
            (HAS-WEB-SERVICE-PORT :VALUE 8080)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/getFranceRequest.jws")))

;;; get UK booking WS

(DEF-CLASS Get-UK-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

#|
(DEF-CLASS Get-UK-BOOKING-REQUEST-WS
           (WEB-SERVICE)
           ?WEB-SERVICE
             ((HAS-CAPABILITY :VALUE Get-UK-BOOKING-REQUEST-WS-CAPABILITY)
            (HAS-INTERFACE :VALUE Get-UK-BOOKING-REQUEST-INTERFACE)
      ;;;      (USED-MEDIATOR :VALUE Get-UK-BOOKING-REQUEST-MED)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get-UK-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES)))
|#


(def-class travel-proposal ()
((has-proposal :type string))
)

(deflift lift-proposal travel-proposal ()
  (lambda (string)
   ;; function which transforms the currency in case UK is selected (replace-all (replace-all string "#" "%23") ":/" "://" )
   (setf pos1 (position #\[ string :test #'equal))
   (setf pos2 (position #\] string :test #'equal))
   (setf price (subseq string (+ pos1 1) pos2))
   (setf string1 (subseq string 0 (+ pos1 1)))
          (setf intx (parse-integer price)) ;;  alternative would be (read-from-string "4.5")
          (setf floatx (float intx))
          (setf result (* floatx 1.2456506)) 
          (setf resultstr (write-to-string result))
;;(setf resultstr (concatenate 'string "TEST" price "TEST"))
    (setf string (concatenate 'string string1 resultstr "]"))
))

;;; below an attempt to refine the goal in order to apply the lowering/lifting just to this WS. Works but just for the lifting, not the lowering
(DEF-CLASS Get-UK-Booking-request-WS
           (WEB-SERVICE)
           ?WEB-SERVICE
           ((HAS-INPUT-ROLE :VALUE has-method :VALUE has-departure-city :VALUE has-departure-country :VALUE has-arrival-city :VALUE has-arrival-country :VALUE has-outbound-date :VALUE has-return-date)
            (HAS-INPUT-SOAP-BINDING
             :VALUE
             (has-method "string")
             :VALUE
             (has-departure-city "string")
             :VALUE
             (has-departure-country "string")
             :VALUE
             (has-arrival-city "string")
             :VALUE
             (has-arrival-country "string")
             :VALUE
             (has-outbound-date "string")
             :VALUE
             (has-return-date "string"))
            (HAS-OUTPUT-ROLE :VALUE has-proposal)
            (HAS-OUTPUT-SOAP-BINDING
             :VALUE
             (has-proposal "xml"))
            (has-method :TYPE STRING)
            (has-departure-city :TYPE String)
            (has-departure-country :TYPE String)
         (has-arrival-city :TYPE String)
            (has-arrival-country :TYPE String)
         (has-outbound-date :TYPE community-date)
            (has-return-date :TYPE community-date)
            (has-proposal :TYPE travel-proposal)
            (HAS-CAPABILITY :VALUE Get-UK-BOOKING-REQUEST-WS-CAPABILITY)
            (HAS-INTERFACE :VALUE Get-UK-BOOKING-REQUEST-INTERFACE)
      ;;;      (USED-MEDIATOR :VALUE Get-UK-BOOKING-REQUEST-MED)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get-UK-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES)))



(DEF-CLASS Get-UK-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get-UK-BOOKING-REQUEST-WS-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE Get-booking-request-MED)
            (HAS-ASSUMPTION
             :VALUE
             (KAPPA
              (?WEB-SERVICE)
              (= (WSMO-ROLE-VALUE ?WEB-SERVICE 'HAS-DEPARTURE-COUNTRY) 
                 "Uk")))
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get-UK-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get-UK-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get-UK-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING
             :VALUE
             ((GROUNDED-TO-HTTP
               (NORMAL
                (NIL (METHOD departurecity departurecountry arrivalcity arrivalcountry outbounddate returndate)))))))) 

(DEF-CLASS Get-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
           (PROBLEM-SOLVING-PATTERN)
           NIL)

(DEF-CLASS Get-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             Get-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS Get-UK-BOOKING-REQUEST-INTERFACE
           (INTERFACE)
           ?INTERFACE
           ((HAS-CHOREOGRAPHY
             :VALUE
             Get-UK-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY)
            (HAS-ORCHESTRATION
             :VALUE
             Get-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get-UK-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get-UK-BOOKING-REQUEST-WS-PUBLISHER-INFORMATION
           (PUBLISHER-INFORMATION)
           ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE
             :VALUE
             Get-UK-BOOKING-REQUEST-INTERFACE)
            (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
            (HAS-WEB-SERVICE-PORT :VALUE 8080)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/getUkRequest.jws")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; NEW BOOKING REQUEST SWS DESCRIPTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(DEF-CLASS GET2-Booking-Request-GOAL-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get2-Booking-request-GOAL
           (GOAL)
           ?GOAL
           ((HAS-INPUT-ROLE :VALUE has-method :VALUE has-departure-city :VALUE has-departure-country :VALUE has-arrival-city :VALUE has-arrival-country :VALUE has-outbound-date :VALUE has-return-date)
            (HAS-INPUT-SOAP-BINDING
             :VALUE
             (has-method "string")
             :VALUE
             (has-departure-city "string")
             :VALUE
             (has-departure-country "string")
             :VALUE
             (has-arrival-city "string")
             :VALUE
             (has-arrival-country "string")
             :VALUE
             (has-outbound-date "string")
             :VALUE
             (has-return-date "string"))
            (HAS-OUTPUT-ROLE :VALUE has-proposal)
            (HAS-OUTPUT-SOAP-BINDING
             :VALUE
             (has-proposal "xml"))
            (has-method :TYPE STRING)
            (has-departure-city :TYPE String)
            (has-departure-country :TYPE br-country) ;;string
            (has-arrival-city :TYPE String)
            (has-arrival-country :TYPE br-country) ;;string
            (has-outbound-date :TYPE community-date)
            (has-return-date :TYPE community-date)
            (has-proposal :TYPE String)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get2-booking-request-GOAL-NON-FUNCTIONAL-PROPERTIES)))


(DEF-CLASS Get2-booking-request-MED-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get2-booking-request-MED
           (WG-MEDIATOR)
           ?MEDIATOR
           ((HAS-SOURCE-COMPONENT :VALUE Get2-booking-request-GOAL)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get2-booking-request-MED-NON-FUNCTIONAL-PROPERTIES)))

;;; get france booking WS

(DEF-CLASS Get2-FRANCE-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get2-FRANCE-BOOKING-REQUEST-WS
           (WEB-SERVICE)
           ?WEB-SERVICE
             ((HAS-CAPABILITY :VALUE Get2-FRANCE-BOOKING-REQUEST-WS-CAPABILITY)
            (HAS-INTERFACE :VALUE Get2-FRANCE-BOOKING-REQUEST-INTERFACE)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get2-FRANCE-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get2-FRANCE-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get2-FRANCE-BOOKING-REQUEST-WS-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
      ((USED-MEDIATOR :VALUE Get2-booking-request-MED)
       (HAS-ASSUMPTION
        :VALUE (KAPPA (?WEB-SERVICE) (OR (= 1 1)
                                         (OR (br-france br-germany)))))
             ;(OR (br-france br-germany)))
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get2-FRANCE-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

;;; assumption should also state that it targets euro currency

(DEF-CLASS Get2-FRANCE-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get2-FRANCE-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING
             :VALUE
             ((GROUNDED-TO-HTTP
               (NORMAL
                (NIL (METHOD departurecity departurecountry arrivalcity arrivalcountry outbounddate returndate)))))))) 

(DEF-CLASS Get2-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
           (PROBLEM-SOLVING-PATTERN)
           NIL)

(DEF-CLASS Get2-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             Get2-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS Get2-FRANCE-BOOKING-REQUEST-INTERFACE
           (INTERFACE)
           ?INTERFACE
           ((HAS-CHOREOGRAPHY
             :VALUE
             Get2-FRANCE-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY)
            (HAS-ORCHESTRATION
             :VALUE
             Get2-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get2-FRANCE-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get2-FRANCE-BOOKING-REQUEST-WS-PUBLISHER-INFORMATION
           (PUBLISHER-INFORMATION)
           ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE
             :VALUE
             Get2-FRANCE-BOOKING-REQUEST-INTERFACE)
            (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
            (HAS-WEB-SERVICE-PORT :VALUE 8080)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/getFranceRequest.jws")))

;;; get UK booking WS

(DEF-CLASS Get2-UK-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get2-UK-Booking-request-WS
           (WEB-SERVICE)
           ?WEB-SERVICE
           ((HAS-INPUT-ROLE :VALUE has-method :VALUE has-departure-city :VALUE has-departure-country :VALUE has-arrival-city :VALUE has-arrival-country :VALUE has-outbound-date :VALUE has-return-date)
            (HAS-INPUT-SOAP-BINDING
             :VALUE
             (has-method "string")
             :VALUE
             (has-departure-city "string")
             :VALUE
             (has-departure-country "string")
             :VALUE
             (has-arrival-city "string")
             :VALUE
             (has-arrival-country "string")
             :VALUE
             (has-outbound-date "string")
             :VALUE
             (has-return-date "string"))
            (HAS-OUTPUT-ROLE :VALUE has-proposal)
            (HAS-OUTPUT-SOAP-BINDING
             :VALUE
             (has-proposal "xml"))
            (has-method :TYPE STRING)
            (has-departure-city :TYPE String)
            (has-departure-country :TYPE String)
         (has-arrival-city :TYPE String)
            (has-arrival-country :TYPE String)
         (has-outbound-date :TYPE community-date)
            (has-return-date :TYPE community-date)
            (has-proposal :TYPE travel-proposal)
            (HAS-CAPABILITY :VALUE Get2-UK-BOOKING-REQUEST-WS-CAPABILITY)
            (HAS-INTERFACE :VALUE Get2-UK-BOOKING-REQUEST-INTERFACE)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get2-UK-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES)))



(DEF-CLASS Get2-UK-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get2-UK-BOOKING-REQUEST-WS-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE Get2-booking-request-MED)
            (HAS-ASSUMPTION
             :VALUE (KAPPA (?WEB-SERVICE) (OR (= 1 1)
                                              (OR (br-uk)))))
             ;;(OR (br-uk)))
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get2-UK-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

;;; assumption should also refer to gbp-currency.

(DEF-CLASS Get2-UK-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get2-UK-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING
             :VALUE
             ((GROUNDED-TO-HTTP
               (NORMAL
                (NIL (METHOD departurecity departurecountry arrivalcity arrivalcountry outbounddate returndate)))))))) 

(DEF-CLASS Get2-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
           (PROBLEM-SOLVING-PATTERN)
           NIL)

(DEF-CLASS Get2-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             Get2-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS Get2-UK-BOOKING-REQUEST-INTERFACE
           (INTERFACE)
           ?INTERFACE
           ((HAS-CHOREOGRAPHY
             :VALUE
             Get2-UK-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY)
            (HAS-ORCHESTRATION
             :VALUE
             Get2-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get2-UK-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get2-UK-BOOKING-REQUEST-WS-PUBLISHER-INFORMATION
           (PUBLISHER-INFORMATION)
           ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE
             :VALUE
             Get2-UK-BOOKING-REQUEST-INTERFACE)
            (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
            (HAS-WEB-SERVICE-PORT :VALUE 8080)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/getUkRequest.jws")))

;;; assumptions above should also refer to the respective currencies.
;;; a third WS should be introduced which targest UK but accepts gbp and euros.

;;; for the goal requests, we would need to define additional concepts (the ones used by the requester) which define the instances used by the assumptions in a slightly different way than the services and which 