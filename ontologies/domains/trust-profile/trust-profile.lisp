;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology trust-profile)



;;; user trust requirements

(def-class feature-level ()
"example of qualitative features. We abstract to it from the WS observables")

(def-instance high feature-level)

(def-instance medium feature-level)

(def-instance low feature-level)




;#|
; QoS-requirements are requirements related with quality of service
(def-class QoS-requirement-type () ?x
  :iff-def (or (= ?x  QoS-requirement)
               (subclass-of ?x  QoS-requirement)))

(def-class QoS-requirement (trust-requirement)
  ((datafreshness :type feature-level)
   (availability :type feature-level)
   (execution-time :type feature-level)))


;USER1 trust Requirements
(def-class USER1 (trust-user)
((has-trust-profile :type trust-profile-USER1)))

(def-class trust-profile-USER1 (trust-profile)
 ((has-trust-guarantee :type guarantee-USER1)
  (has-trust-requirement :type requirement-USER1)))

(def-class requirement-USER1 (QoS-requirement)
 ((datafreshness :value low)
   (availability :value medium)
   (execution-time :value low)))

;USER2 trust Requirements
(def-class USER2 (trust-user)
((has-trust-profile :type trust-profile-USER2)))

(def-class trust-profile-USER2 (trust-profile)
 ((has-trust-guarantee :type guarantee-USER2)
  (has-trust-requirement :type requirement-USER2)))

(def-class requirement-USER2 (QoS-requirement)
 ((datafreshness :value high)
   (availability :value medium)
   (execution-time :value low)))

;USER3 trust Requirements
(def-class USER3 (trust-user)
((has-trust-profile :type trust-profile-USER3)))

(def-class trust-profile-USER3 (trust-profile)
 ((has-trust-guarantee :type guarantee-USER3)
  (has-trust-requirement :type requirement-USER3)))

(def-class requirement-USER3 (QoS-requirement)
 ((datafreshness :value low)
   (availability :value low)
   (execution-time :value high)))

;|#

;security-requirements are related with securirt aspects

(def-class security-requirement-type () ?x
  :iff-def (or (= ?x  security-requirement)
               (subclass-of ?x  security-requirement)))

(def-class security-requirement (trust-requirement)
  ((encryption-algorithm :type feature-level)
   (certification-authority :type feature-level)
   (certification-authority-country :type feature-level)))


;USER4 trust Requirements
(def-class USER4 (trust-user)
((has-trust-profile :type trust-profile-USER4)))

(def-class trust-profile-USER4 (trust-profile)
 ((has-trust-guarantee :type guarantee-USER4)
  (has-trust-requirement :type requirement-USER4)))

(def-class requirement-USER4 (security-requirement)
  ((encryption-algorithm :value high)
   (certification-authority :value medium)
   (certification-authority-country :value medium)))
 


;USER5 trust Requirements
(def-class USER5 (trust-user)
((has-trust-profile :type trust-profile-USER5)))

(def-class trust-profile-USER5 (trust-profile)
 ((has-trust-guarantee :type guarantee-USER5)
  (has-trust-requirement :type requirement-USER5)))

(def-class requirement-USER5 (security-requirement)
 ((encryption-algorithm :value medium)
  (certification-authority :value low)
  (certification-authority-country :value low)))



;USER6 trust Requirements
(def-class USER6 (trust-user)
((has-trust-profile :type trust-profile-USER6)))

(def-class trust-profile-USER6 (trust-profile)
 ((has-trust-guarantee :type guarantee-USER6)
  (has-trust-requirement :type requirement-USER6)))

(def-class requirement-USER6 (security-requirement)
  
 ((encryption-algorithm :value low)
  (certification-authority :value high)
  (certification-authority-country :value high)))
 




;;;;;;;;;;;;Trusted-Wsmo-Use-case


(def-class get-train-timetable-trusted-goal
  (goal)
  ?web-service
  ((HAS-POST-CONDITION
    :VALUE
    (KAPPA
     (?GOAL)
     (HAS-TRAIN-time-table
      (WSMO-ROLE-VALUE ?GOAL 'HAS-DEPARTURE-STATION)
      (WSMO-ROLE-VALUE ?GOAL 'HAS-DESTINATION-STATION)
      (WSMO-ROLE-VALUE ?GOAL 'HAS-DATE-AND-TIME)
      (WSMO-ROLE-VALUE ?GOAL 'has-train-timetable))))
   (has-input-role
    :value
    has-departure-station
    :value
    has-destination-station
    :value
    has-date-and-time)
   (has-input-soap-binding
    :value
    (has-departure-station "sexpr")
    :value
    (has-destination-station "sexpr")
    :value
    (has-date-and-time "sexpr"))
   (has-output-role :value has-train-timetable)
   (has-output-soap-binding
    :value
    (has-train-timetable "xml"))
   (has-departure-station :type city)
   (has-destination-station :type city)
   (has-date-and-time :type date-and-time)
   (has-train-timetable :type string)))




(def-class get-train-timetable-trusted-mediator (wg-mediator)
  ((has-source-component :value get-train-timetable-trusted-goal)))


;#|

;;trusted ws1

(def-class get-train-timetable-service-T1 (trust-web-service)
  ((has-capability :value get-train-timetable-capability-T1)
   (has-interface :value get-train-timetable-service-interface-T1)
;;this slot specifies the ws trust profile 
   (has-trust-profile :type get-train-timetable-service-trust-profile-T1)))

(def-class get-train-timetable-service-trust-profile-T1 (trust-profile)
 ((has-trust-guarantee :type get-train-timetable-service-guarantee-T1)
  (has-trust-requirement :type get-train-timetable-service-requirement-T1)))

(def-class get-train-timetable-service-guarantee-T1 (Trust-non-functional-properties)
 ((encryption-algorithm :type #_crypt:3DES)
   (certification-authority :value verisign)
   (certification-authority-country :value north-american-country))) 






(def-class get-train-timetable-capability-T1 (capability)
  ((used-mediator :value get-train-timetable-trusted-mediator)
   (HAS-PRE-CONDITION :VALUE 
                      (KAPPA
                       (?web-service)
                       (AND (WSMO-ROLE-VALUE ?web-service 'HAS-DEPARTURE-STATION)
                            (WSMO-ROLE-VALUE ?web-service 'HAS-DESTINATION-STATION)
                            (WSMO-ROLE-VALUE ?web-service 'HAS-DATE-AND-TIME))))
   (HAS-POST-CONDITION
    :VALUE
    (KAPPA
     (?web-service)
     (HAS-TRAIN-time-table
      (WSMO-ROLE-VALUE ?web-service 'HAS-DEPARTURE-STATION)
      (WSMO-ROLE-VALUE ?web-service 'HAS-DESTINATION-STATION)
      (WSMO-ROLE-VALUE ?web-service 'HAS-DATE-AND-TIME)
      (WSMO-ROLE-VALUE ?web-service 'has-train-timetable))))))

(def-class get-train-timetable-service-interface-T1 (interface)
    ((has-choreography :value get-train-timetable-service-choreography-T1)))

(DEF-CLASS GET-TRAIN-TIMETABLE-SERVICE-CHOREOGRAPHY-T1
           (CHOREOGRAPHY)
    ((has-earthing :value #_generic-grounding)))

;;trusted ws2

(def-class get-train-timetable-service-T2 (trust-web-service)
  ((has-capability :value get-train-timetable-capability-T2)
   (has-interface :value get-train-timetable-service-interface-T2)
;;this slot specifies the ws trust profile 
   (has-trust-profile :type get-train-timetable-service-trust-profile-T2)))

(def-class get-train-timetable-service-trust-profile-T2 (trust-profile)
 ((has-trust-guarantee :type get-train-timetable-service-guarantee-T2)
  (has-trust-requirement :type get-train-timetable-service-requirement-T2)))

(def-class get-train-timetable-service-guarantee-T2 (Trust-non-functional-properties)
((encryption-algorithm :type #_crypt:RSA)
   (certification-authority :value globalsign-austria)
   (certification-authority-country :value austria))) 





(def-class get-train-timetable-capability-T2 (capability)
  ((used-mediator :value get-train-timetable-trusted-mediator)
   (HAS-PRE-CONDITION :VALUE 
                      (KAPPA
                       (?web-service)
                       (AND (WSMO-ROLE-VALUE ?web-service 'HAS-DEPARTURE-STATION)
                            (WSMO-ROLE-VALUE ?web-service 'HAS-DESTINATION-STATION)
                            (WSMO-ROLE-VALUE ?web-service 'HAS-DATE-AND-TIME))))
   (HAS-POST-CONDITION
    :VALUE
    (KAPPA
     (?web-service)
     (HAS-TRAIN-time-table
      (WSMO-ROLE-VALUE ?web-service 'HAS-DEPARTURE-STATION)
      (WSMO-ROLE-VALUE ?web-service 'HAS-DESTINATION-STATION)
      (WSMO-ROLE-VALUE ?web-service 'HAS-DATE-AND-TIME)
      (WSMO-ROLE-VALUE ?web-service 'has-train-timetable))))))



(def-class get-train-timetable-service-interface-T2 (interface)
  ((has-choreography :value get-train-timetable-service-choreography-T2)))

(DEF-CLASS GET-TRAIN-TIMETABLE-SERVICE-CHOREOGRAPHY-T2
    (CHOREOGRAPHY)
    ((has-earthing :value #_generic-grounding)))

;;trusted ws3

(def-class get-train-timetable-service-T3 (trust-web-service)
  ((has-capability :value get-train-timetable-capability-T3)
   (has-interface :value get-train-timetable-service-interface-T3)
;;this slot specifies the ws trust profile  
   (has-trust-profile :type get-train-timetable-service-trust-profile-T3)))

(def-class get-train-timetable-service-trust-profile-T3 (trust-profile)
 ((has-trust-guarantee :type get-train-timetable-service-guarantee-T3)
  (has-trust-requirement :type get-train-timetable-service-requirement-T3)))

(def-class get-train-timetable-service-guarantee-T3 (Trust-non-functional-properties)
((encryption-algorithm :type #_crypt:AES)
   (certification-authority :value tc-trust-center)
   (certification-authority-country :value germany)))

(def-class get-train-timetable-capability-T3 (capability)
  ((used-mediator :value get-train-timetable-trusted-mediator)
   (HAS-PRE-CONDITION :VALUE 
                      (KAPPA
                       (?web-service)
                       (AND (WSMO-ROLE-VALUE ?web-service 'HAS-DEPARTURE-STATION)
                            (WSMO-ROLE-VALUE ?web-service 'HAS-DESTINATION-STATION)
                            (WSMO-ROLE-VALUE ?web-service 'HAS-DATE-AND-TIME))))
   (HAS-POST-CONDITION
    :VALUE
    (KAPPA
     (?web-service)
     (HAS-TRAIN-time-table
      (WSMO-ROLE-VALUE ?web-service 'HAS-DEPARTURE-STATION)
      (WSMO-ROLE-VALUE ?web-service 'HAS-DESTINATION-STATION)
      (WSMO-ROLE-VALUE ?web-service 'HAS-DATE-AND-TIME)
      (WSMO-ROLE-VALUE ?web-service 'has-train-timetable))))))



(def-class get-train-timetable-service-interface-T3 (interface)
  ((has-choreography :value get-train-timetable-service-choreography-T3)))

(DEF-CLASS GET-TRAIN-TIMETABLE-SERVICE-CHOREOGRAPHY-T3
    (CHOREOGRAPHY)
    ((has-earthing :value #_generic-grounding)))

(def-rule #_generic-lower-get-timetable
    ((#_generic-lower-get-timetable ?invocation ?http-request)
     if
     (= ?depart (wsmo-role-value ?invocation 'has-departure-station))
     (= ?arrive (wsmo-role-value ?invocation 'has-destination-station))
     (= ?date (wsmo-role-value ?invocation 'has-date-and-time))
     (= ?date (?day ?month ?year))
     (= ?url (make-string "http://localhost:8080/travel/get-timetable?depart=~A&arrive=~A&day=~A&month=~A&year=~A"
                          ?depart ?arrive ?day ?month ?year))
     (#_rfc2616:set-url ?http-request ?url)
     (#_rfc2616:set-method ?http-request "GET")))

(def-rule #_generic-lift-get-timetable
    ((#_generic-lift-get-timetable ?http-response ?invocation)
     if
     (#_rfc2616:get-content ?http-response ?http-content)
     (set-goal-slot-value ?invocation has-train-timetable ?http-content)))

(def-instance #_generic-grounding rest-grounding
  ((lower-rule #_generic-lower-get-timetable)
   (lift-rule #_generic-lift-get-timetable)))
