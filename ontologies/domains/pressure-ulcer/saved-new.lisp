;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology pressure-ulcer)

(def-class pressure-ulcer (lesion)
  "This is a type of lesion of the skin"
  ((at-stage :type pressure-ulcer-stage)
   (has-risk-factor :value immobility 
			:value incontinence :value inadequate-dietary-intake :value impaired-nutritional-status
                    :value altered-consciousness)))

(def-class pressure-ulcer-stage ( ))

(def-class pressure-ulcer-risk-assessment-clinical-instrument (clinical-instrument)
  ((associated-medical-condition :value pressure-ulcer)))

(def-class people-at-risk-from-pressure-ulcer (population-specification)
  ((has-generic-constraint (Kappa (?x)(or (chair-bound ?x)(bed-ridden ?x))))))

(def-instance prevention-of-pressure-ulcer preventive-guideline
  ((has-duration )
   (start-time )
   (end-time )
   (has-author ahcpr)
   (has-activity pressure-ulcer-risk-assessment provision-of-mechanical-loading-and-support-surfaces pressure-ulcer-skin-care-and-early-treatment)
   (has-goals "identifying at-risk individuals who need preventive intervention and the specific factors placing them at risk" "maintaining and improving tissue tolerance to pressure in order to prevent injury" "protecting against the adverse effects of external mechanical forces (pressure, friction, and shear)" "reducing the incidence of pressure ulcers through educational programs")
   (additional-decision-support-model )
   (has-plan-specification )
   (outcome-measure "whether or not pressure ulcers have arisen and their severity (Stage)")
   (has-main-goal "predicting and preventing pressure ulcers")
   (target-population people-at-risk-from-pressure-ulcer)
   (full-name "pressure ulcer in adults: prediction and prevention")
   (associated-medical-condition pressure-ulcer)
   (temporal-constraints )
   (location-constraints )
   (associated-documents ahcprpub92-0047)
   (has-guideline-user-type generic-care-giver)))

(def-instance may-92 calendar-date
  ((minute-of )
   (second-of )
   (hour-of )
   (day-of )
   (month-of 5)
   (year-of 1992)))

(def-instance AHCPRPub92-0047 document-reference
  ((has-author ahcpr)
   (has-title "pressure ulcers in adults: prediction and prevention")
   (has-url "www.ahcpr.gov")
   (published-date may-1992)))

(def-instance pressure-ulcer medical-condition)

(def-instance pressure-ulcer-stage-1 pressure-ulcer-stage)

(def-instance pressure-ulcer-stage-2 pressure-ulcer-stage)

(def-instance pressure-ulcer-stage-3 pressure-ulcer-stage)

(def-instance pressure-ulcer-stage-4 pressure-ulcer-stage)

(def-instance pressure-ulcer-risk-assessment medical-monitoring-activity
  ((has-precondition (kappa (?patient) (or (chair-bound ?patient) (bed-ridden ?patient))))
   (starts-during-activity patient-admission)
   (has-goals "to identify at-risk individuals needing prevention")
   (activity-of prevention-of-pressure-ulcer)
   (requires-clinical-instrument pressure-ulcer-risk-assessment-clinical-instrument)
   (responsible-agent health-care-professional)))

(def-instance pressure-ulcer-skin-care-and-early-treatment medical-procedure
  ((has-precondition )
   (has-goals )
   (activity-of )
   (responsible-agent )))

(def-instance provision-of-mechanical-loading-and-support-surfaces medical-procedure
  ((has-precondition )
   (has-goals "protecting against the adverse effects of external mechanical forces" )
   (activity-of prevention-of-pressure-ulcer )
   (responsible-agent generic-care-giver )))

(def-instance braden-scale pressure-ulcer-risk-assessment-clinical-instrument)

(def-instance norton-scale pressure-ulcer-risk-assessment-clinical-instrument)

(def-instance patient-admission plan-activity
  ((has-precondition )
   (starts-during-activity )
   (has-goals "to admit the patient in a care institute")
   (responsible-agent nurse)
   (activity-of )))

