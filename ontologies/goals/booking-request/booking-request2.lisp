(in-package "OCML")

(in-ontology booking-request)

;; general relation for instance names 
(def-relation has-instance-name (?x ?y)
  :iff-def (or (and (departure-country ?x)
                    (has-country-title ?x ?y))
               (and (arrival-country ?x)
                    (has-country-title ?x ?y))  
               (and (br-currency ?x)
                    (has-title ?x ?y))
               (and (departure-city ?x)
                    (has-city-title ?x ?y))
               
               ))   


(def-class departure-country ()
  ((has-country-title :type string)
   (refined-by :type #_cs:location-member)))

(def-class departure-city ()
  ((has-city-title :type string)
   (refined-by :type #_cs:location-member)))

(def-instance departing-toulouse departure-city
((has-city-title "departing-toulouse")
 (refined-by #_cs:p5-location-toulouse)))

(def-instance departing-germany-1 departure-country
((has-country-title "departing-germany-1")
 (refined-by #_cs:p8-1-location-germany)))

(def-instance departing-germany departure-country
((has-country-title "departing-germany")
 (refined-by #_cs:p8-location-germany)))


(def-instance departing-france-1 departure-country
((has-country-title "departing-france-1")
 (refined-by #_cs:p7-1-location-france)))

(def-instance departing-france departure-country
((has-country-title "departing-france")
 (refined-by #_cs:p7-location-france)))


(def-instance departing-uk-1 departure-country
((has-country-title "departing-uk-1")
 (refined-by #_cs:p6-1-location-uk)))

(def-instance departing-uk departure-country
((has-country-title "departing-uk")
 (refined-by #_cs:p6-location-uk)))

(def-instance departing-spain departure-country
((has-country-title "departing-spain")
 (refined-by #_cs:p9-location-spain)))


(def-class arrival-country ()
  ((has-country-title :type string)
   (refined-by :type #_cs:location-member)))

(def-instance arriving-germany-1 arrival-country
((has-country-title "arriving-germany-1")
 (refined-by #_cs:p8-1-location-germany)))

(def-instance arriving-germany arrival-country
((has-country-title "arriving-germany")
 (refined-by #_cs:p8-location-germany)))


(def-instance arriving-france-1 arrival-country
((has-country-title "arriving-france-1")
 (refined-by #_cs:p7-1-location-france)))

(def-instance arriving-france arrival-country
((has-country-title "arriving-france")
 (refined-by #_cs:p7-location-france)))


(def-instance arriving-uk-1 arrival-country
((has-country-title "arriving-uk-1")
 (refined-by #_cs:p6-1-location-uk)))

(def-instance arriving-uk arrival-country
((has-country-title "arriving-uk")
 (refined-by #_cs:p6-location-uk)))


;;; SWS descriptions

(def-class result-string (string))


(DEF-CLASS GET3-Booking-Request-GOAL-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get3-Booking-request-GOAL
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
            (has-departure-city :TYPE departure-city)
            (has-departure-country :TYPE departure-country) ;;string
            (has-arrival-city :TYPE String)
            (has-arrival-country :TYPE arrival-country) ;;string
            (has-outbound-date :TYPE community-date)
            (has-return-date :TYPE community-date)
            (has-proposal :TYPE result-string)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get3-booking-request-GOAL-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get3-booking-request-MED-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get3-booking-request-MED
           (WG-MEDIATOR)
           ?MEDIATOR
           ((HAS-SOURCE-COMPONENT :VALUE Get3-booking-request-GOAL)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get3-booking-request-MED-NON-FUNCTIONAL-PROPERTIES)))

;;; get france booking WS

(DEF-CLASS Get3-FRANCE-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get3-FRANCE-BOOKING-REQUEST-WS
           (WEB-SERVICE)
           ?WEB-SERVICE
             ((HAS-CAPABILITY :VALUE Get3-FRANCE-BOOKING-REQUEST-WS-CAPABILITY)
            (HAS-INTERFACE :VALUE Get3-FRANCE-BOOKING-REQUEST-INTERFACE)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get3-FRANCE-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get3-FRANCE-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get3-FRANCE-BOOKING-REQUEST-WS-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
      ((USED-MEDIATOR :VALUE Get3-booking-request-MED)
       (HAS-ASSUMPTION
        :VALUE (KAPPA (?WEB-SERVICE) (OR (= 1 1)
                                         (OR (arriving-france arriving-germany)))))
             ;(OR (br-france br-germany)))
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get3-FRANCE-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

;;; assumption should also state that it targets euro currency

(DEF-CLASS Get3-FRANCE-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get3-FRANCE-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING
             :VALUE
             ((GROUNDED-TO-HTTP
               (NORMAL
                (NIL (METHOD departurecity departurecountry arrivalcity arrivalcountry outbounddate returndate)))))))) 

(DEF-CLASS Get3-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
           (PROBLEM-SOLVING-PATTERN)
           NIL)

(DEF-CLASS Get3-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             Get3-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS Get3-FRANCE-BOOKING-REQUEST-INTERFACE
           (INTERFACE)
           ?INTERFACE
           ((HAS-CHOREOGRAPHY
             :VALUE
             Get3-FRANCE-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY)
            (HAS-ORCHESTRATION
             :VALUE
             Get3-FRANCE-BOOKING-REQUEST-INTERFACE-ORCHESTRATION)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get3-FRANCE-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get3-FRANCE-BOOKING-REQUEST-WS-PUBLISHER-INFORMATION
           (PUBLISHER-INFORMATION)
           ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE
             :VALUE
             Get3-FRANCE-BOOKING-REQUEST-INTERFACE)
            (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
            (HAS-WEB-SERVICE-PORT :VALUE 8080)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/getFranceRequest.jws")))

;;; get UK booking WS

(DEF-CLASS Get3-UK-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get3-UK-Booking-request-WS
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
            (has-departure-city :TYPE departure-city)
            (has-departure-country :TYPE departure-country)
            (has-arrival-city :TYPE String)
            (has-arrival-country :TYPE arrival-country)
            (has-outbound-date :TYPE community-date)
            (has-return-date :TYPE community-date)
            (has-proposal :TYPE travel-proposal)
            (HAS-CAPABILITY :VALUE Get3-UK-BOOKING-REQUEST-WS-CAPABILITY)
            (HAS-INTERFACE :VALUE Get3-UK-BOOKING-REQUEST-INTERFACE)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get3-UK-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES)))



(DEF-CLASS Get3-UK-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get3-UK-BOOKING-REQUEST-WS-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE Get3-booking-request-MED)
            (HAS-ASSUMPTION
             :VALUE (KAPPA (?WEB-SERVICE) (OR (= 1 1)
                                              (OR (arriving-uk)))))
             ;;(OR (br-uk)))
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get3-UK-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

;;; assumption should also refer to gbp-currency.

(DEF-CLASS Get3-UK-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get3-UK-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING
             :VALUE
             ((GROUNDED-TO-HTTP
               (NORMAL
                (NIL (METHOD departurecity departurecountry arrivalcity arrivalcountry outbounddate returndate)))))))) 

(DEF-CLASS Get3-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
           (PROBLEM-SOLVING-PATTERN)
           NIL)

(DEF-CLASS Get3-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             Get3-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS Get3-UK-BOOKING-REQUEST-INTERFACE
           (INTERFACE)
           ?INTERFACE
           ((HAS-CHOREOGRAPHY
             :VALUE
             Get3-UK-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY)
            (HAS-ORCHESTRATION
             :VALUE
             Get3-UK-BOOKING-REQUEST-INTERFACE-ORCHESTRATION)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get3-UK-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get3-UK-BOOKING-REQUEST-WS-PUBLISHER-INFORMATION
           (PUBLISHER-INFORMATION)
           ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE
             :VALUE
             Get3-UK-BOOKING-REQUEST-INTERFACE)
            (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
            (HAS-WEB-SERVICE-PORT :VALUE 8080)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/getUkRequest.jws")))

;;; assumptions above should also refer to the respective currencies.
;;; a third WS should be introduced which targest UK but accepts gbp and euros.

;;; for the goal requests, we would need to define additional concepts (the ones used by the requester) which define the instances used by the assumptions in a slightly different way than the services and which 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWS descriptions GET 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEF-CLASS GET4-Booking-Request-GOAL-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-Booking-request-GOAL
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
            (has-departure-city :TYPE departure-city)
            (has-departure-country :TYPE departure-country) ;;string
            (has-arrival-city :TYPE String)
            (has-arrival-country :TYPE arrival-country) ;;string
            (has-outbound-date :TYPE community-date)
            (has-return-date :TYPE community-date)
            (has-proposal :TYPE result-string)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-booking-request-GOAL-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get4-booking-request-MED-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-booking-request-MED
           (WG-MEDIATOR)
           ?MEDIATOR
           ((HAS-SOURCE-COMPONENT :VALUE Get4-booking-request-GOAL)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-booking-request-MED-NON-FUNCTIONAL-PROPERTIES)))

;;; get france spain booking WS

(DEF-CLASS Get4-France-Spain-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-France-Spain-BOOKING-REQUEST-WS
           (WEB-SERVICE)
           ?WEB-SERVICE
             ((HAS-CAPABILITY :VALUE Get4-France-Spain-BOOKING-REQUEST-WS-CAPABILITY)
            (HAS-INTERFACE :VALUE Get4-France-Spain-BOOKING-REQUEST-INTERFACE)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-France-Spain-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get4-France-Spain-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-France-Spain-BOOKING-REQUEST-WS-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
      ((USED-MEDIATOR :VALUE Get4-booking-request-MED)
       (HAS-ASSUMPTION
        :VALUE (KAPPA (?WEB-SERVICE) (OR (= 1 1)
                                         (OR (departing-france departing-spain br-euro)))))
             ;(OR (br-france br-germany)))
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-France-Spain-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

;;; assumption should also state that it targets euro currency

(DEF-CLASS Get4-France-Spain-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-France-Spain-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING
             :VALUE
             ((GROUNDED-TO-HTTP
               (NORMAL
                (NIL (METHOD departurecity departurecountry arrivalcity arrivalcountry outbounddate returndate)))))))) 

(DEF-CLASS Get4-France-Spain-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
           (PROBLEM-SOLVING-PATTERN)
           NIL)

(DEF-CLASS Get4-France-Spain-BOOKING-REQUEST-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             Get4-France-Spain-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS Get4-France-Spain-BOOKING-REQUEST-INTERFACE
           (INTERFACE)
           ?INTERFACE
           ((HAS-CHOREOGRAPHY
             :VALUE
             Get4-France-Spain-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY)
            (HAS-ORCHESTRATION
             :VALUE
             Get4-France-Spain-BOOKING-REQUEST-INTERFACE-ORCHESTRATION)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-France-Spain-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get4-France-Spain-BOOKING-REQUEST-WS-PUBLISHER-INFORMATION
           (PUBLISHER-INFORMATION)
           ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE
             :VALUE
             Get4-France-Spain-BOOKING-REQUEST-INTERFACE)
            (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
            (HAS-WEB-SERVICE-PORT :VALUE 8080)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/getFranceRequest.jws")))

;;; get UK GBP booking WS

(DEF-CLASS Get4-Uk-Gbp-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-Uk-Gbp-Booking-request-WS
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
            (has-departure-city :TYPE departure-city)
            (has-departure-country :TYPE departure-country)
            (has-arrival-city :TYPE String)
            (has-arrival-country :TYPE arrival-country)
            (has-outbound-date :TYPE community-date)
            (has-return-date :TYPE community-date)
            (has-proposal :TYPE travel-proposal)
            (HAS-CAPABILITY :VALUE Get4-Uk-Gbp-BOOKING-REQUEST-WS-CAPABILITY)
            (HAS-INTERFACE :VALUE Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-Uk-Gbp-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES)))



(DEF-CLASS Get4-Uk-Gbp-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-Uk-Gbp-BOOKING-REQUEST-WS-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE Get4-booking-request-MED)
            (HAS-ASSUMPTION
             :VALUE (KAPPA (?WEB-SERVICE) (OR (= 1 1)
                                              (OR (departing-uk br-gbp)))))
             ;;(OR (br-uk)))
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-Uk-Gbp-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

;;; assumption should also refer to gbp-currency.

(DEF-CLASS Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING
             :VALUE
             ((GROUNDED-TO-HTTP
               (NORMAL
                (NIL (METHOD departurecity departurecountry arrivalcity arrivalcountry outbounddate returndate)))))))) 

(DEF-CLASS Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
           (PROBLEM-SOLVING-PATTERN)
           NIL)

(DEF-CLASS Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE
           (INTERFACE)
           ?INTERFACE
           ((HAS-CHOREOGRAPHY
             :VALUE
             Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY)
            (HAS-ORCHESTRATION
             :VALUE
             Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE-ORCHESTRATION)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get4-Uk-Gbp-BOOKING-REQUEST-WS-PUBLISHER-INFORMATION
           (PUBLISHER-INFORMATION)
           ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE
             :VALUE
             Get4-Uk-Gbp-BOOKING-REQUEST-INTERFACE)
            (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
            (HAS-WEB-SERVICE-PORT :VALUE 8080)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/getUkRequest.jws")))


;;; get UK GBP Euro booking WS

(DEF-CLASS Get4-Uk-Gbp-Euro-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-Uk-Gbp-Euro-Booking-request-WS
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
            (has-departure-city :TYPE departure-city)
            (has-departure-country :TYPE departure-country)
            (has-arrival-city :TYPE String)
            (has-arrival-country :TYPE arrival-country)
            (has-outbound-date :TYPE community-date)
            (has-return-date :TYPE community-date)
            (has-proposal :TYPE travel-proposal)
            (HAS-CAPABILITY :VALUE Get4-Uk-Gbp-Euro-BOOKING-REQUEST-WS-CAPABILITY)
            (HAS-INTERFACE :VALUE Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-Uk-Gbp-Euro-BOOKING-REQUEST-WS-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get4-Uk-Gbp-Euro-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-Uk-Gbp-Euro-BOOKING-REQUEST-WS-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE Get4-booking-request-MED)
            (HAS-ASSUMPTION
             :VALUE (KAPPA (?WEB-SERVICE) (OR (= 1 1)
                                              (OR (departing-uk-1 br-euro br-gbp)))))
             ;;(OR (br-uk)))
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-Uk-Gbp-Euro-BOOKING-REQUEST-WS-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

;;; assumption should also refer to gbp-currency.

(DEF-CLASS Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING
             :VALUE
             ((GROUNDED-TO-HTTP
               (NORMAL
                (NIL (METHOD departurecity departurecountry arrivalcity arrivalcountry outbounddate returndate)))))))) 

(DEF-CLASS Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
           (PROBLEM-SOLVING-PATTERN)
           NIL)

(DEF-CLASS Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE
           (INTERFACE)
           ?INTERFACE
           ((HAS-CHOREOGRAPHY
             :VALUE
             Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE-CHOREOGRAPHY)
            (HAS-ORCHESTRATION
             :VALUE
             Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE-ORCHESTRATION)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS Get4-Uk-Gbp-Euro-BOOKING-REQUEST-WS-PUBLISHER-INFORMATION
           (PUBLISHER-INFORMATION)
           ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE
             :VALUE
             Get4-Uk-Gbp-Euro-BOOKING-REQUEST-INTERFACE)
            (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
            (HAS-WEB-SERVICE-PORT :VALUE 8080)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/getUkRequest.jws")))

;;; assumptions above should also refer to the respective currencies.
;;; a third WS should be introduced which targest UK but accepts gbp and euros.

;;; for the goal requests, we would need to define additional concepts (the ones used by the requester) which define the instances used by the assumptions in a slightly different way than the services and which 
