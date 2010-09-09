;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology organization-ontology)

(def-class network-based-organization (organization))

(def-class voluntary-organization (non-profit-organization))

(def-class government-organization (organization))

(def-class employer-lead-organization (profit-organization))

(def-class trade-union (non-profit-organization))

(def-class exam-taking-place (information-transfer-event)
	"The event when an exam takes place"
	((main-agent 	:type (or person organization organization-unit)
			:documentation "Body who prepares the exam") 
	 (other-agents-involved :type (or person organization organization-unit))
	 (recipient-agents :type (or person group-of-people) :documentation "Who makes the exam"))
)

(def-class research-team (group-of-people)
	"A research team is a group of researchers who are making research in some area"
	((projects-in-research :type project)
	(research-area-of-interest :type research-area)
	(leader-of-team :type person)))

(def-class governing-body (non-profit-organization)
	"Every elected or non-elected governing body. It could have as subclasses national-governing-gody, regional-governing-body, local-governing-body& I have decided not to make any distinction."
)

(def-class health-care-organization-unit (organization-unit)
((has-sub-unit :type health-care-organization-unit )
(organization-unit-part-of :type health-care-organization)))

(def-class non-clinical-employee-of-health-care-organization (employee)
  ((works-at :type health-care-organization)))

(def-class health-care-organization(organization)((has-organization-unit :type health-care-organization-unit)))

(def-class non-profit-educational-organization (educational-organization non-profit-organization))

(def-class lord (affiliated-person)
	"A member of the House of Lords"
((has-affiliation :type organization :value org-house-of-lords))
)

(def-class chairman (employee)
"maybe we should call this 'chair-person'?")

(def-class visiting-a-place-or-people (social-gathering)
	"Used when a person is visiting a place or a group of people"
	((visitor :type (or person group-of-people))
         (people-or-organization-being-visited :type (or person organization)))
        :slot-renaming ((visitor main-agent)
                        (people-or-organization-being-visited  OTHER-AGENTS-INVOLVED)))

(def-class research-programme (temporal-thing)
	"A research programme is a temporal thing which is supported by an organization, which funds all the research projects which this programme has"
	((supported-by :type organization)
	(has-projects :type research-project)
	(has-web-pages :type web-page)))

(def-class further-educational-organization (educational-organization))

(def-class senior-manager (manager))

(def-class hospitality-company (corporation))

(def-class leisure-company (corporation))

(def-class manufacturing-company (corporation))

(def-class construction-company (corporation))

(def-class armed-forces (organization))

(def-class training-organization (learning-centred-organization))

(def-class library-organization (organization))

(def-class advisory-organization (organization))

(def-class clinical-unit (health-care-organization-unit))

(def-class health-care-service-unit (health-care-organization-unit))

(def-class health-care-administration-unit (health-care-organization-unit))

(def-class health-care-research-unit (health-care-organization-unit))

(def-class hospital (health-care-organization))

(def-class geographically-distributed-health-care-service (health-care-organization)
"This covers the health care services distributed over the territory - e.g. the emergency services")

(def-class trade-association (employer-lead-organization))

(def-class local-authority (government-organization))

(def-class college (further-educational-organization))

(def-class chamber-of-commerce (network-based-organization))

(def-class vocational-guidance-organization (advisory-organization))

(def-class private-hospital (hospital))

(def-class public-hospital (hospital))

(def-class general-medicine-unit (clinical-unit))

(def-class stroke-unit (clinical-unit))

(def-class ematology (clinical-unit))

(def-class clinical-laboratory (health-care-service-unit))

(def-class medical-imaging-unit (health-care-service-unit))

(def-relation application-domain (?X)
 :sufficient (exists ?y (project-application-domain ?y ?X)))




(def-class wsmo-entity ())
