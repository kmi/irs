;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology medical-guidelines)

(def-class guideline-user-type () ?x 
  :iff-def (or (subclass-of ?x generic-care-giver )
                (member ?x '(software-agent patient ))))

(def-class utility-centred-decision-support-system (software-system ))

(def-class payoff-type () ?x
:iff-def (member ?x '(health-payoff economic-payoff composite-economic-health-payoff)))

(def-class consensus-conference (conference))

(def-class guideline-basis ())

(def-class generic-planning-entity (temporal-thing)
  ((has-author :type (or person organization))
   (has-subcomponent :type generic-planning-entity)
   (has-goals :type string)))

  
(def-class plan-activity (generic-planning-entity)
  ((has-precondition :type kappa-expression)
   (starts-during-activity :type plan-activity)
   (responsible-agent-type :type generic-agent-type)
   (activity-of :type plan)
   (is-performed-at-periodical-intervals? :type boolean :default-value false)
   (has-min-periodicity :type interval)))

(def-class PLAN (generic-planning-entity)
  ((additional-decision-support-model :type decision-support-model)
   (has-plan-specification :type plan-specification)))

(def-class guideline-application-location (location))

(def-class plan-specification () ?x
 "a plan specification can be either a single plan activity (do this) or a complex specification using 
plan operators, or can be a (sub-)plan.  The latter scenario is one in which we have a non-primitive-activity
which opens up a new plan (i.e. a plan specification with additional information)"
 :iff-def (or (plan ?x)(plan-activity ?x) (composite-planning-expression ?x)))

(def-class plan-operator ())

(def-class composite-planning-expression () ?x
 :iff-def (or (and (== ?x (?op . ?l))
                    (member ?op '(one-of one-or-more-of sequential-all-of non-ordered-all-of))
                    (every ?l plan-specification))
              (and (== ?x (conditional-choice . ?l))
                    (every ?l (kappa (?pair) (and (pair ?pair)(ground-thing (first ?pair)) (sentence (first ?pair))
                                                                (plan-specification (second ?pair))))))))

(def-class decision-support-model ()
  ((has-computational-form :type utility-centred-decision-support-system)
   (maximizes-payoff-type :type payoff-type)))

(def-class medical-guideline (plan)
  "Each guideline is associated with a medical condition.
 It also targets a particular population"
  ((outcome-measure :type medical-variable)
   (has-main-goal :type string)
   (target-population :type population-specification)
   (full-name :type string)
   (associated-medical-condition-class :type medical-condition-type)
   (temporal-constraints :type string)
   (location-constraints :type guideline-application-location)
   (associated-documents :type document-reference)
   (has-guideline-user-type :type  guideline-user-type)))

(def-class evidence-centred-basis (guideline-basis)
((associated-publications :type publication-reference :min-cardinality 1)))

(def-class practice-policy-basis (guideline-basis)
((associated-institutions :type (or health-care-organization health-care-organization-unit))))

(def-class consensus-basis (guideline-basis)((associated-conference :type consensus-conference)
(associated-publications :type publication-reference)))


(def-class generalized-health-care-procedure (plan-activity )
 "This includes both administrative and 'proper' healthcare procedures"
 ((responsible-agent-type :type (or generic-care-giver-type
                                    non-clinical-employee-of-health-care-organization))))


(def-class health-care-procedure (generalized-health-care-procedure)
  ((responsible-agent-type :type generic-care-giver-type)
   (requires-healthcare-instrument :type healthcare-instrument)
   (uses-healthcare-technique :type generalized-healthcare-technique)))

(def-class monitoring-activity (plan-activity)
 ((exit-condition  :type (and (ground-thing ?x)(sentence ?x)))
  (max-monitoring-period :type duration)
 (min-monitoring-period :type duration)
 (measurement-interval :type duration )
  (condition-satisfied-plan :type plan-specification)
   (condition-violated-plan :type plan-specification)))

(def-class influence-diagram (utility-centred-decision-support-system))

(def-class decision-tree (utility-centred-decision-support-system))

(def-class waiting-activity(plan-activity)
((waiting-time :type duration)))

(def-class preventive-guideline (medical-guideline))

(def-class diagnostic-guideline (medical-guideline))

(def-class therapeutic-guideline (medical-guideline)
"This is a class of guidelines which specify therapeutic care")

(def-class administrative-procedure (generalized-health-care-procedure)
  ((responsible-agent-type :type non-clinical-employee-of-health-care-organization)))

(def-class clinical-procedure (health-care-procedure)
  ((requires-clinical-instrument :type clinical-instrument)
   (uses-clinical-technique :type generalized-clinical-technique)))

(def-class medical-procedure (clinical-procedure)
((responsible-agent-type :value physician)))

(def-class nurse-procedure (clinical-procedure)
((responsible-agent-type :value nurse)))

(def-class laboratory-procedure (clinical-procedure)
((responsible-agent-type :value health-care-technician)))

(def-class procedure-carried-out-by-paramedics (clinical-procedure)
((responsible-agent-type :value paramedic)))

(def-class clinical-monitoring-activity (monitoring-activity clinical-procedure))

(def-class medical-monitoring-activity (medical-procedure clinical-monitoring-activity))

(def-instance at-home guideline-application-location)

(def-instance at-hospital guideline-application-location)

(def-instance at-gp-surgery guideline-application-location)

(def-instance at-day-hospital guideline-application-location)

(def-instance sequential-all-of plan-operator)

(def-instance non-ordered-all-of plan-operator)

(def-instance one-or-more-of plan-operator)

(def-instance one-of plan-operator)

(def-instance conditional-choice plan-operator)

(def-instance AHCPR health-care-organization
  ((has-duration )
   (start-time )
   (end-time )
   (has-web-address )
   (has-address )
   (affiliated-people )
   (organization-part-of )
   (headed-by )
   (has-organization-unit )))
