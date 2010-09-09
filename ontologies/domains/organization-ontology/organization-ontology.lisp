;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology organization-ontology)

;;;;;;;


(def-class non-profit-organization (organization))

(def-class profit-organization (organization))

(def-class industrial-organization (profit-organization ))

(def-class corporation (industrial-organization))

(def-class charitable-organization (non-profit-organization))

(def-class learning-centred-organization (organization))

(def-class educational-organization (learning-centred-organization ))

(def-class awarding-body (organization ))


(def-class dedicated-funding-body (awarding-body))

(def-class higher-educational-organization (educational-organization)
  ((has-academic-unit :type academic-unit) 
   (has-support-unit :type academic-support-unit)))

(def-rule has-academic-unit-implies-HAS-ORGANIZATION-UNIT
  ((HAS-ORGANIZATION-UNIT ?x ?y)
   if
   (has-academic-unit ?x ?y)))

(def-rule has-support-unit-implies-HAS-ORGANIZATION-UNIT
  ((HAS-ORGANIZATION-UNIT ?x ?y)
   if
   (has-support-unit ?x ?y)))




(def-class university (higher-educational-organization)
  ((has-faculty :type university-faculty)
   (has-vice-chancellor :type educational-employee))
  :slot-renaming ((has-vice-chancellor headed-by)))

(def-class distance-teaching-university (university))

(def-class school (educational-organization))

(def-class civil-service (non-profit-organization))

(def-class academic-unit (organization-unit))

(def-class academic-support-unit (organization-unit))

(def-class educational-research-institute (academic-unit))

;;;;;;;;;

(def-class project (temporal-thing) 
  ((leading-organization :type organization)
   (organizations-involved :type organization :min-cardinality 1)
   (has-goals :type string)
   (has-project-leader :type person)
   (has-project-member :type person :min-cardinality 1)
   (funding-source :type organization)
   (has-web-address :type URL)
   (uses-technology :type technology)
   (associated-documents :type document-reference)
   (associated-products :type technology)
   (project-generic-area :type  generic-area-of-commercial-or-scholarly-interest)
   (project-application-domain :type application-domain)))

(def-relation organizations-involved (?project ?org)
  :sufficient (and (has-organization-unit ?org ?sub-org)
                    (organizations-involved ?project ?sub-org)))
                   
   
(def-class industrial-project (project))

(def-class academic-project (project)
  ((associated-publications :type publication-reference)))


(def-class research-project (project)
  ((has-research-area :type research-area))
  :slot-renaming ((has-research-area project-generic-area)))

(def-class development-project (project)
 )

(def-class externally-funded-project (project)
  ((project-funding-source :type funding-source)))

(def-class research-and-development-project (research-project development-project))

(def-class academic-research-project (academic-project research-project))

(def-class phd-project (academic-research-project))

(def-class academic-r&d-project (academic-project research-and-development-project))

(def-class industrial-r&d-project (industrial-project research-and-development-project))

(def-class mixed-academic-and-industrial-r&d-project (academic-r&d-project industrial-r&d-project))

;;;;;;;

(def-class generic-area-of-commercial-or-scholarly-interest ()
 "A generic class to specify generic areas for research or businees initiatives.
  For instance, the area in which a project is situated")

(def-class research-area (generic-area-of-commercial-or-scholarly-interest )
  )

(def-relation has-awarding-body (?award-giving-event ?body)
  :sufficient (and (organization-part-of ?body ?org)
                   (has-awarding-body ?award-giving-event ?org)))
;;;;;;;;;;;;;
(def-relation has-occupation (?x ?C)
  :sufficient (and (occupation ?C)
                   (instance-of ?X ?c)))


(def-class working-person (person)
  ((has-occupation :type occupation :min-cardinality 1)))

(def-class unemployed-person (person))

(def-class self-employed-person (working-person))

(def-class consultant (self-employed-person)
  ((consults-for :type (or organization person))))

(def-relation involved-in-projects (?x ?project)
  :constraint (and (person ?x)
                   (project ?project))
  :sufficient (or (has-project-member ?project ?x)
                  (has-project-leader ?project ?X)))

(def-class employee (affiliated-person working-person)
  ((works-at :type organization)
   (has-job-title :type string)
   (involved-in-projects :type project))
  :slot-renaming ((works-at has-affiliation)))

(def-rule employee-address-rule
  ((has-address ?X ?address)
   if
   (employee ?x)
   (works-at ?x ?org)
   (has-address ?org ?address)))


(def-class clerical-employee (employee))

(def-class educational-employee (employee)
  ((works-at :type educational-organization)))

(def-class school-teacher (educational-employee)
  ((works-at :type school)))

(def-class academic (educational-employee person-who-publishes) 
  ((works-at :type higher-educational-organization)
   (has-research-area :type research-area)))

(def-class politician (employee)
  ((has-constituency :type geographical-region)))


(def-class industry-employee (employee)
  ((works-at :type industrial-organization)))

(def-class professor (academic)
  ((has-occupation :default-value professor)))

(def-class manager (employee))

(def-class business-manager (manager))

(def-class system-manager (manager))

(def-class student (affiliated-person))

(def-class phd-student (student))

(def-class researcher (academic))

(def-class research-assistant (researcher))

(def-class research-fellow (researcher))

(def-class senior-research-fellow (researcher))


(def-class senior-lecturer (academic))
(def-class lecturer (academic))

(def-class reader (academic))

(def-class secretary (employee))

(def-class project-officer (employee)
  )

(def-class assistant-project-officer (employee)
  )
  
(def-class multimedia-designer (employee)
  )

;;;;;;;;;;

(def-class giving-a-tutorial(information-transfer-event))

(def-class giving-a-lecture (information-transfer-event))

(def-class scientific-demonstration (information-transfer-event)
  ((subject-of-the-demo )))
   ;;;;;:slot-renaming ((subject-of-the-demo object-acted-on)))


(def-class demonstration-of-technology (scientific-demonstration)
 ((technology-being-demoed :type technology))
  :slot-renaming ((technology-being-demoed subject-of-the-demo)))

(def-class conferring-an-award(composite-abstract-and-physical-transfer)
  ((has-awarding-body :type awarding-body)
   (has-award-rationale)
   (object-acted-on :type award))
  :slot-renaming ((has-awarding-body main-agent)))

(def-class broadcasting-event (information-transfer-event)
 ((medium-used :type broadcasting-medium)))
 ;;;:slot-renaming ((medium-used instrument-used)))

(def-class publication-type-event (event-involving-production)
  ((event-product :type publication-reference)
   (medium-used :type publishing-medium)))

(def-class conferring-a-monetary-award (conferring-an-award)
((monetary-award :type sum-of-money))
:slot-renaming ((monetary-award object-acted-on)))

(def-class web-broadcasting-event (broadcasting-event)
((medium-used web)))

(def-class paper-publishing (publication-type-event)
  ((published-paper :type paper-publication))
 :slot-renaming ((published-paper event-product)))

(def-class book-publishing (publication-type-event)
  ((published-book :type book))
 :slot-renaming ((published-book event-product)))

(def-class attending-an-event (taking-a-trip)
  ((event-attended :type event)))

(def-class attending-an-industrial-event(attending-an-event))

(def-class attending-an-academic-event (attending-an-event))

(def-class conference-trip (attending-an-academic-event )
 ((event-attended :type academic-conference)))

(def-class workshop-trip (attending-an-academic-event )
  ((event-attended :type academic-workshop)))

(def-class industrial-exhibition-trip (attending-an-industrial-event)
 ((event-attended :type industrial-exhibition)))



(def-class conference (meeting-taking-place))

(def-class academic-conference (conference)
  ((has-papers :type proceedings-paper-reference)
   (has-invited-talks :type giving-a-lecture)
   (has-demonstrations :type scientific-demonstration)))

(def-class academic-workshop (academic-conference))

(def-class industrial-conference (conference))

(def-class industrial-exhibition (meeting-taking-place))


