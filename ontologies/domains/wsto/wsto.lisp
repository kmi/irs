;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology wsto)

(def-class Trust-Non-Functional-Properties (Non-Functional-Properties)
"these properties are more related with provider and then represent the WS quality of service"
  ((datafreshness :type real-number)
   (availability :type real-number)
   (execution-time :type real-number)
   (encryption-algorithm :type #_crypt:CryptographicAlgorithm)
   (certification-authority :type ca)
   (certification-authority-country :type country)))

(def-class ca-type () ?x
  :iff-def (or (= ?x ca)
               (subclass-of ?x ca)))

(def-class ca ())
"list of certification authorities"
(def-instance verisign ca)
(def-instance globalsign-austria ca)
(def-instance tc-trust-center ca)
 
;|#

;(describe-class 'participant)


(def-class participant-type () ?x
  :iff-def (or (= ?x participant)
               (subclass-of ?x participant)))

(def-class participant () ?x
" A participant is a trust-user or a trust-web-service It is useful to define relations and slots that involve both user and web service"
((has-trust-profile :type trust-profile :cardinality 1))
  :iff-def (or (trust-web-service ?x)
               (trust-user ?x)))

(def-class trust-profile () ?x
"user and ws are associated with own profile, that includes guarantees and requirementes. The guarantees are expressed in terms of 
non functional properties." 
((has-trust-guarantee :type Trust-Non-Functional-Properties)
  (has-trust-requirement :type trust-requirement)))


(def-class trust-requirement ())
"trust requirements allows both web services and user to declare their requirements in matter of trust"

  
(def-class trust-web-service-type () ?x
  :iff-def (or (= ?x trust-web-service)
               (subclass-of ?x trust-web-service)))

(def-class trust-web-service (web-service participant)
"trust  web service is subclass of web service as defined in wsmo domain
and subclass of participant")
 
(def-class trust-user-type () ?x
  :iff-def (or (= ?x trust-user)
               (subclass-of ?x trust-user)))

(def-class trust-user (wsmo-entity participant)
"The user concept is not in WSMO, but we define it, as a wsmo-entity because it is nesessary during the trust-based invocation process"
 ((has-current-goal :type goal)))
