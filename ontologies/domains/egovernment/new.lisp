;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology egovernment)

(def-class E-GOV-TOP ())

(def-class ORGANIZATION (E-GOV-TOP))

(def-class CLIENT (tangible-thing) ;;john d 3/12/04 (E-GOV-TOP)
 "Clients are identified with unique id"
 ((has-name :type string :cardinality 1)
  (has-age :type integer :cardinality 1)
  (has-swift-id :type string :cardinality 1)
  (has-legal-representant :type string :cardinality 1)
  (involved-in-cases :type list :min-cardinality 1)
 ))



(def-class agency () ?x
  :iff-def (or (organization ?x) (organization-unit ?x) (person ?x)))

(def-class ENTITLEMENT (intangible-thing) ;;john d 3/12/04 (E-GOV-TOP )
  )

(def-class SERVICE-PROVIDER (E-GOV-TOP )
 ((has-name :type string :cardinality 1)
 (represents-agency :type agency :cardinality 1)
 ))

(def-class BENEFIT-PROVIDER (E-GOV-TOP )
 ((has-name :type string :cardinality 1)
 (represents-agency :type agency :cardinality 1)
 ))

(def-class CASE-WORKER (E-GOV-TOP )
 ((works-for-agency :type agency :cardinality 1)
  (has-contact :type string :cardinality 1)
 ))

(def-class CASE (intangible-thing) ;;john d 3/12/04 (E-GOV-TOP)
 ((has-originator :type (or person organization) :cardinality 1)
  (has-case-worker :type case-worker :cardinality 1)
  (has-client :type client :cardinality 1)
  (has-assessment :type assessment :cardinality 1)
  (has-decision :type decision :cardinality 1)
 ))

(def-class ASSESSEMENT (E-GOV-TOP)
 ((applies-to-case :type case :cardinality 1)
  (has-decision :type decision :cardinality 1)
 ))

(def-class DECISION (intangible-thing) ;;john d 3/12/04 (E-GOV-TOP)
 ((applies-to-assessment :type assessment :cardinality 1)
  (has-entitlements :type list :cardinality 1)
 ))

(def-class SOCIAL-WORKER (CASE-WORKER ))

(def-class BENEFIT (ENTITLEMENT )
 ((has-name :type string :cardinality 1)
  (has-calculation-statement :type string :cardinality 1)
  (has-benefit-provider :type benefit-provider :cardinality 1)
  (has-payment-status :type string :cardinality 1)
 ))

(def-class SERVICE (ENTITLEMENT )
 ((has-name :type string :cardinality 1)
 (has-service-provider :type service-provider :cardinality 1)
 (has-payment-status :type string :cardinality 1)
 ))

(def-class CITIZEN (CLIENT )
 ((has-social-security-num :type string :cardinality 1)
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
