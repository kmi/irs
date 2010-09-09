;;; Mode: Lisp; Package: ocml

;;; Author: Liliana Cabral

;;; The Open University

(in-package "OCML")

(in-ontology egovernment)

;;;comments from john
;;;1. string type in several cases should be a class
;;;2. seperate out agencies, benefits and clients
;;;3 link2 from agency to benefits and services
;;;4 case-worker social-worker subclass of professionals linked to person
;;;5 citizen is a subclass of person
;;;6 client can be kept separate
;;;7 case assessment and decision can be linked

(def-class E-GOV-TOP ())

(def-class ORGANIZATION (E-GOV-TOP))

(def-class AGENCY (ORGANIZATION))

(def-class CLIENT (E-GOV-TOP)
 "Clients are identified with unique id"
 ((has-name :type string :cardinality 1)
  (has-age :type integer :cardinality 1)
  (has-swift-id :type string :cardinality 1)
  (has-legal-representant :type string :cardinality 1)
  (involved-in-cases :type list :min-cardinality 1)
 ))
  
(def-class ENTITLEMENT (E-GOV-TOP ))

(def-class CITIZEN (CLIENT )
 ((has-social-security-num :type string :cardinality 1)
 ))

(def-class SERVICE (ENTITLEMENT )
 ((has-name :type string :cardinality 1)
 (has-service-provider :type service-provider :cardinality 1)
 (has-payment-status :type string :cardinality 1)
 ))

(def-class SERVICE-PROVIDER (E-GOV-TOP )
 ((has-name :type string :cardinality 1)
 (represents-agency :type agency :cardinality 1)
 ))

(def-class BENEFIT-PROVIDER (E-GOV-TOP )
 ((has-name :type string :cardinality 1)
 (represents-agency :type agency :cardinality 1)
 ))


(def-class BENEFIT (ENTITLEMENT )
 ((has-name :type string :cardinality 1)
  (has-calculation-statement :type string :cardinality 1)
  (has-benefit-provider :type benefit-provider :cardinality 1)
  (has-payment-status :type string :cardinality 1)
 ))
 
 
(def-class CASE-WORKER (E-GOV-TOP )
 ((works-for-agency :type agency :cardinality 1)
  (has-contact :type string :cardinality 1)
 ))

(def-class SOCIAL-WORKER (CASE-WORKER ))


(def-class CASE (E-GOV-TOP)
 ((has-originator :type string :cardinality 1)
  (has-case-worker :type case-worker :cardinality 1)
  (has-client :type client :cardinality 1)
  (has-assessment :type assessment :cardinality 1)
  (has-decision :type decision :cardinality 1)
 ))

(def-class ASSESSEMENT (E-GOV-TOP)
 ((applies-to-case :type case :cardinality 1)
  (has-decision :type decision :cardinality 1)
 ))


(def-class DECISION (E-GOV-TOP)
 ((applies-to-assessment :type assessment :cardinality 1)
  (has-entitlements :type list :cardinality 1)
 ))


(def-instance 00001_CLIENT client
 ((has-name mother)
  (has-age 65)
  (has-swift-id 001)
  (has-legal-representant daughter)
  (involved-in-cases 00001_CASE)
 ))

(def-instance 00001_CASE case
 ((has-originator mother's GP)
  (has-case-worker community_care_worker)
  (has-client 00001_CLIENT)
  (has-assessment  00001_assessment )
  (has-decision  00001_decision )
 ))
 
 (def-instance community_care_worker CASE-WORKER 
 ((works-for-agency essex_community_care )
  (has-contact my@email)
 ))

 (def-instance essex_community_care agency)
 
 (def-instance Housing agency)
 
 (def-instance Pensions-service agency)
 
 (def-instance disability-and-carers agency)
 
 (def-instance jobcentre-plus agency)
 
 
 (def-instance mothers-benefit1 BENEFIT 
  ((has-name Attendance_allowance)
   (has-calculation-statement some_calculation-st)
   (has-benefit-provider jobcentre_plus_provider)
   (has-payment-status pending)
  ))

 (def-instance daughters-benefit1 BENEFIT 
  ((has-name housing_benefit)
   (has-calculation-statement some_calculation-st)
   (has-benefit-provider housing_provider)
   (has-payment-status pending)
  ))
  
 (def-instance daughters-benefit2 BENEFIT 
  ((has-name council_tax_benefit)
   (has-calculation-statement some_calculation-st)
   (has-benefit-provider housing_provider)
   (has-payment-status pending)
  ))
  

 
