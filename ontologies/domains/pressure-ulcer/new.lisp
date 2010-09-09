;;; Mode: Lisp; Package: ocml



;;; Author: John Domingue



;;; The Open University



(in-package "OCML")



(in-ontology pressure-ulcer)



(def-class pressure-ulcer (lesion)

  "This is a type of lesion of the skin"

  ((at-stage :type pressure-ulcer-stage)

   (has-risk-factor :value immobility 
                    :value incontinence 
                    :value inadequate-dietary-intake 
                    :value impaired-nutritional-status
                    :value altered-consciousness)))



(def-class pressure-ulcer-stage ( ))



(def-class pressure-ulcer-risk-assessment-clinical-instrument (clinical-instrument)

  ((associated-medical-condition-class :value pressure-ulcer)))



(def-class people-at-risk-from-pressure-ulcer (population-specification)

  ((has-generic-constraint (Kappa (?x)(or (chair-bound ?x)(bed-ridden ?x))))))



(def-instance prevention-of-pressure-ulcer preventive-guideline

  ((has-duration )

   (start-time )

   (end-time )

   (has-author ahcpr)

   (has-subcomponent pressure-ulcer-risk-assessment provision-of-mechanical-loading-and-support-surfaces pressure-ulcer-skin-care-and-early-treatment)

   (has-goals "identifying at-risk individuals who need preventive intervention and the specific factors placing them at risk" "maintaining and improving tissue tolerance to pressure in order to prevent injury" "protecting against the adverse effects of external mechanical forces (pressure, friction, and shear)" 
              "reducing the incidence of pressure ulcers through educational programs")

   (additional-decision-support-model )

   (has-plan-specification )

   (outcome-measure pressure-ulcer-incidence)

   (has-main-goal "predicting and preventing pressure ulcers")

   (target-population people-at-risk-from-pressure-ulcer)

   (full-name "pressure ulcer in adults: prediction and prevention")

   (associated-medical-condition-class pressure-ulcer)

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






(def-instance pressure-ulcer-stage-1 pressure-ulcer-stage)



(def-instance pressure-ulcer-stage-2 pressure-ulcer-stage)



(def-instance pressure-ulcer-stage-3 pressure-ulcer-stage)



(def-instance pressure-ulcer-stage-4 pressure-ulcer-stage)



(def-instance pressure-ulcer-risk-assessment medical-procedure

  ((has-precondition (kappa (?patient) (or (chair-bound ?patient) (bed-ridden ?patient))))

   (starts-during-activity patient-admission)

   (has-goals "to identify at-risk individuals needing prevention")

   (activity-of prevention-of-pressure-ulcer)

   (is-performed-at-periodical-intervals? true)

   (requires-clinical-instrument pressure-ulcer-risk-assessment-clinical-instrument)

   (responsible-agent-type physician)))



(def-instance pressure-ulcer-skin-care-and-early-treatment clinical-procedure

  ((has-subcomponent skin-inspection skin-cleansing friction-minimization 
                     ensure-adequate-dietary-intake)
   (has-goals "Maintain and improve tissue tolerance to pressure in order to prevent injury.")
   (activity-of prevention-of-pressure-ulcer)))

(def-instance skin-inspection medical-procedure
  ((IS-PERFORMED-AT-PERIODICAL-INTERVALS? true)
   (HAS-MIN-PERIODICITY 24-hours)
   (activity-of pressure-ulcer-skin-care-and-early-treatment)
   (responsible-agent-type PHYSICIAN)))

(def-instance skin-cleansing nurse-procedure
  ((IS-PERFORMED-AT-PERIODICAL-INTERVALS? true)
   (activity-of pressure-ulcer-skin-care-and-early-treatment)
   (responsible-agent-type nurse)))

(def-instance friction-minimization nurse-procedure
  ((IS-PERFORMED-AT-PERIODICAL-INTERVALS? true)
   (activity-of pressure-ulcer-skin-care-and-early-treatment)
   (responsible-agent-type nurse)
   (USES-CLINICAL-TECHNIQUE regular-turning-of-patient-in-bed application-of-lubricant
                            application-of-protective-dressing
                            application-of-protective-film)))

(def-instance ensure-adequate-dietary-intake nurse-procedure
  (
   (activity-of pressure-ulcer-skin-care-and-early-treatment)
   (responsible-agent-type nurse)
   ))







(def-instance provision-of-mechanical-loading-and-support-surfaces health-care-procedure

  ((has-goals "protecting against the adverse effects of external mechanical forces" )

   (activity-of prevention-of-pressure-ulcer )

   (responsible-agent-type generic-care-giver )
   (has-subcomponent patient-repositioning)))

(def-instance patient-repositioning  health-care-procedure
  ((is-performed-at-periodical-intervals? true)
   (has-min-periodicity 2-hours)
   (activity-of provision-of-mechanical-loading-and-support-surfaces )
   (responsible-agent-type generic-care-giver )
   (uses-healthcare-technique side-lying-position)
   (requires-healthcare-instrument lifting-device 
                                   pressure-reducing-device-for-beds 
                                   pressure-reducing-device-for-heels
                                   positioning-device)))

(def-class pressure-ulcer-related-healthcare-instrument (healthcare-instrument)
  ((associated-medical-condition-class :value pressure-ulcer)))


(def-class pressure-reducing-device (pressure-ulcer-related-healthcare-instrument ))

(def-instance lifting-device pressure-ulcer-related-healthcare-instrument)
(def-instance pressure-reducing-device-for-beds  pressure-reducing-device)

(def-instance  pressure-reducing-device-for-heels pressure-ulcer-related-healthcare-instrument)
(def-instance  positioning-device pressure-ulcer-related-healthcare-instrument)





(def-instance braden-scale pressure-ulcer-risk-assessment-clinical-instrument)



(def-instance norton-scale pressure-ulcer-risk-assessment-clinical-instrument)



(def-instance patient-admission plan-activity

  ((has-precondition )

   (starts-during-activity )

   (has-goals "to admit the patient in a care institute")

   (responsible-agent-type nurse)

   (activity-of )))




(def-relation associated-to-d3e-url (?x ?C)
  :constraint (url ?c))

(def-rule associated-to-d3e-url-rule

((associated-to-d3e-url prevention-of-pressure-ulcer 
 "http://enrich.open.ac.uk/patman/docs/pressure-ulcer/pressure-ulcer-1.html"))

((associated-to-d3e-url pressure-ulcer-risk-assessment 
 "http://enrich.open.ac.uk/patman/docs/pressure-ulcer/pressure-ulcer-12.html"))

((associated-to-d3e-url pressure-ulcer-skin-care-and-early-treatment
 "http://enrich.open.ac.uk/patman/docs/pressure-ulcer/pressure-ulcer-13.html"))

((associated-to-d3e-url provision-of-mechanical-loading-and-support-surfaces 
 "http://enrich.open.ac.uk/patman/docs/pressure-ulcer/pressure-ulcer-14.html"))

((associated-to-d3e-url friction-minimization 
 "http://enrich.open.ac.uk/patman/docs/pressure-ulcer/pressure-ulcer-23.html"))

((associated-to-d3e-url patient-repositioning
  "http://enrich.open.ac.uk/patman/docs/pressure-ulcer/pressure-ulcer-23.html"))

((associated-to-d3e-url ensure-adequate-dietary-intake 
 "http://enrich.open.ac.uk/patman/docs/pressure-ulcer/pressure-ulcer-23.html"))

((associated-to-d3e-url skin-inspection 
 "http://enrich.open.ac.uk/patman/docs/pressure-ulcer/pressure-ulcer-23.html"))

((associated-to-d3e-url skin-cleansing 
 "http://enrich.open.ac.uk/patman/docs/pressure-ulcer/pressure-ulcer-23.html" )))




