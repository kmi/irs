;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology medical-guidelines)

(def-class decision-support-model ()
  ((has-computational-form :type utility-centred-decision-support-system)
   (maximizes-payoff-type :type payoff-type)))

(def-class composite-planning-expression () ?x
 :iff-def (or (and (== ?x (?op . ?l))
                    (member ?op '(one-of one-or-more-of sequential-all-of non-ordered-all-of))
                    (every ?l plan-specification))
              (and (== ?x (conditional-choice . ?l))
                    (every ?l (kappa (?pair) (and (pair ?pair)(ground-thing (first ?pair)) (sentence (first ?pair))
                                                                (plan-specification (second ?pair))))))))

(def-class plan-operator ())

(def-class plan-specification () ?x
 "a plan specification can be either a single plan activity (do this) or a complex specification using 
plan operators, or can be a (sub-)plan.  The latter scenario is one in which we have a non-primitive-activity
which opens up a new plan (i.e. a plan specification with additional information)"
 :iff-def (or (plan ?x)(plan-activity ?x) (composite-planning-expression ?x)))

(def-class guideline-application-location (location))

(def-class PLAN (temporal-thing)
	((has-author :type (or person organization))
         (has-activity :type plan-activity)
   	(has-goals :type string)
	(additional-decision-support-model :type decision-support-model)
        (has-plan-specification :type plan-specification)))

(def-class plan-activity ()
  ((has-precondition :type kappa-expression)
   (starts-during-activity :type plan-activity)
   (has-goals :type string)
    (responsible-agent :type generic-agent)
   (activity-of :type plan)
   (is-performed-at-periodical-intervals? :type boolean :default-value false)
   (has-periodicity :type interval)))

(def-class guideline-basis ())

(def-class consensus-conference (conference))

(def-class payoff-type () ?x
:iff-def (member ?x '(health-payoff economic-payoff composite-economic-health-payoff)))

(def-class utility-centred-decision-support-system (software-system ))

(def-class guideline-user-type () ?x 
  :iff-def (or (subclass-of ?x generic-care-giver )
                (member ?x '(software-agent patient ))))

(def-class medical-guideline (plan)
  "Each guideline is associated with a medical condition.
 It also targets a particular population"
((outcome-measure :type string)
 (has-main-goal :type string)
(target-population :type population-specification)
(full-name :type string)
(associated-medical-condition :type medical-condition)
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

(def-class health-care-procedure (plan-activity)
	((responsible-agent :type (or non-professional-care-giver health-care-professional))))

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

(def-class clinical-procedure (health-care-procedure)
  ((requires-clinical-instrument :type clinical-instrument)))

(def-class administrative-procedure (health-care-procedure))

(def-class therapeutic-guideline (medical-guideline)
"This is a class of guidelines which specify therapeutic care")

(def-class diagnostic-guideline (medical-guideline))

(def-class preventive-guideline (medical-guideline))

(def-class medical-procedure (clinical-procedure)
((responsible-agent :type physician)))

(def-class nurse-procedure (clinical-procedure)
((responsible-agent :type nurse)))

(def-class laboratory-procedure (clinical-procedure)
((responsible-agent :type health-care-technician)))

(def-class procedure-carried-out-by-paramedics (clinical-procedure)
((responsible-agent :type paramedic)))

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
