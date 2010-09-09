;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology persons-and-organizations)

(def-class research-project ()
  ((has-research-area)
   (tackles-domain)
   (responsible-scientist)
   (project-members)
   (funding-source)
   (goals :type string)
   (organizations-involved :type organization)
   (products)
   (uses-technology :type technology)
   (publications)
   (web-address)))

(def-rule uses-technology-1
  ((uses-technology ?p ?t)
   if
   (asserted (uses-technology ?p ?t2))
   (technology-builds-on ?t2 ?t)))

(def-class phd-project (research-project))

(def-class publication ()
  ((title)
   (author)
   (appears-in)
   (topic)))
   

(def-class person ()
  ((full-name :type string)
   (biography :type string)
   (email-address)
   (web-address)))

(def-class affiliated-person (person)
  ((has-affiliation :type organization)))

(def-class student (affiliated-person) 
  ((studies-in :type educational-organization)))

(def-rule student-affiliation 
  "A student is affiliated to the place where he/she studies"
  ((has-affiliation ?X ?org)
   if
   (studies-in ?x ?org)))


(def-class working-person (affiliated-person))
(def-class unemployed-person (person))

(def-class self-employed-person (working-person))

(def-class consultant (self-employed-person))

(def-class employee (working-person)
  ((works-at :type organization)))

(def-rule employee-affiliation 
  "An employee is affiliated to teh place where he/she studies"
  ((has-affiliation ?X ?org)
   if
   (works-at ?x ?org)))



(def-class clerical-employee (employee))

(def-class educational-employee (employee)
  ((works-at :type EDUCATIONAL-ORGANIZATION)))

(def-class school-teacher (educational-employee)
  ((works-at :type school)))

(def-class academic (educational-employee) 
  ((works-at :type higher-educational-organization)))

(def-class politician (employee)
  ((constituency)))

(def-class industry-employee (employee)
  ((works-at :type industrial-organization)))

(def-class professor (academic))

(def-class manager (employee))

(def-class business-manager (manager))


(def-class system-manager (manager))
(def-class phd-student (student))

(def-class researcher (academic))


(def-class lecturer (academic))

(def-class secretary (employee))

(def-class project-officer (employee)
  )

(def-class assistant-project-officer (employee)
  )
  
(def-class mm-designer (employee)
  )

;;;;;;;;;;

(def-class web-address ()
  ((address :type string)))

(def-class organization ()
  ((location :type location) 
   (affiliated-people :type affiliated-person) 
   (alternate-names :type string) 
   (part-of :type organization)
   (web-address :type web-address)))

(def-class non-profit-organization (organization))

(def-class profit-organization (organization))

(def-class industrial-organization (profit-organization ))

(def-class corporation (industrial-organization))

(def-class charitable-organization (non-profit-organization))

(def-class educational-organization (non-profit-organization ))

(def-class higher-educational-organization (educational-organization)
  ((academic-units) (support-units)))

(def-class school (educational-organization))

(def-class civil-service (non-profit-organization))

(def-class academic-unit (educational-organization))

(def-class support-unit (educational-organization))

(def-class educational-research-institute (academic-unit))

(def-class meeting ()
  ((has-location :type location)
   (attendees)
   (has-organizer)))

(def-class industrial-exhibition (meeting)
  ((has-demonstrations)))

(def-class scientific-conference (meeting)
  ((has-papers :type conference-paper)
   (has-invited-talks :type lecture)
   (has-demonstrations)))

(def-class scientific-workshop (scientific-conference)
  )

(def-class meeting-attendance ()
  ((meeting-attended :type meeting)))
  
(def-class conference-trip (meeting-attendance)
  ((meeting-attended :type scientific-conference)))

(def-class workshop-trip (meeting-attendance)
  ((meeting-attended :type scientific-workshop)))

(def-class industrial-exhibition-trip (meeting-attendance)
  ((meeting-attended :type industrial-exhibition)))

;;;;;;;
(def-relation technology-builds-on (?x ?y)
  :constraint (and (technology ?x)
                   (or (technology ?y)
                       (technology-type ?y))))
  
                  

(def-rule technology-builds-on-1
  ((technology-builds-on ?x ?z)
   if
   ;;;(variable-bound ?x)
   (asserted (technology-builds-on ?x ?y))
   (technology-builds-on ?y ?z)))
  ;;((technology-builds-on ?x ?z)
  ;; if
  ;; (instance-of ?x ?z)))
  
  
 

(def-class technology ()
  ((has-author :type person)
   (made-by :type organisation)
   (technology-builds-on :type technology)
   (associated-web-site :type string)
   (has-features)))

(def-class computing-technology (technology)
  ((number-of-user-sites :type number)))

(def-class software-technology (computing-technology)
  ((system-requirements) (hardware-platforms) (operating-systems) 
   (status :type software-status :documentation "Whether the software is finished, alpha or beta")))

(def-class hardware-technology (computing-technology))

(def-class internet-technology (software-technology hardware-technology))

(def-instance web internet-technology)

(def-class web-technology (internet-technology )
  ((technology-builds-on :value web)))

(def-class multimedia-technology (software-technology))

(def-class agent-technology (software-technology))

(def-class internet-agent-technology (internet-technology agent-technology))

(def-class web-agent-technology (web-technology internet-agent-technology)
  ())

(def-class modelling-technology (software-technology))

(def-class software-visualization-technology (software-technology))

(def-class genetic-algorithm-software-visualization-technology (software-visualization-technology))

(def-class programming-language (software-technology))

(def-class genetic-algorithms  (software-technology))

(def-class modelling-language (programming-language modelling-technology))

(def-instance java programming-language)
(def-instance lisp programming-language)
(def-instance prolog programming-language)
(def-instance c++ programming-language)
(def-instance smalltalk programming-language)
(def-instance perl programming-language)
(def-instance hypernews computing-technology)
(def-instance cgi programming-language)

(def-instance frame-logic modelling-technology)


(def-class location ())


;;;;;;;;;;;;;;;;;;;;;;


(def-instance sun corporation)

(def-instance british-telecom corporation)

(def-instance british-aerospace corporation
  ((affiliated-people murray millican)))

(def-instance andersen corporation)

(def-instance british-petroleum corporation)

(def-instance baa corporation)

(def-instance ibm corporation)

(def-instance apple corporation)

(def-instance sgi corporation)

(def-instance open-university higher-educational-organization
  ((academic-units  Faculty-of-Arts
                    Faculty-of-Mathematics-and-Computing
                    Faculty-of-Science
                    Faculty-of-Social-Sciences
                    Faculty-of-Technology
                    School-of-Education
                    School-of-Health-and-Social-Welfare
                    The-Open-University-Business-School
                    Centre-for-Modern-Languages
                    Institute-of-Educational-Technology
                    Knowledge-Media-Institute)
   (support-units
    Academic-Administration-Division
    Academic-Computing-Service
    BBC-Open-University-Production-Centre
    Customer-Relations-Centre
    Library
    Office-for-Technology-Development
    Open-University-Conference-Centre
    Open-University-Validation-Services
    Operations 
    Quality-Support-Centre
    Regional-Academic-Services
    Regional-Centres
    Services-To-Disabled-Students
    Vice-Chancellors-Office)))

(def-instance Knowledge-Media-Institute educational-research-institute
  ((part-of open-university)))

(def-instance Institute-of-Educational-Technology educational-research-institute
  ((part-of open-university)))

(def-instance lms charitable-organization)

(def-instance Faculty-of-Arts academic-unit
  ((part-of open-university)))

(def-instance Faculty-of-Mathematics-and-Computing academic-unit
  ((part-of open-university)))

(def-instance Faculty-of-Science academic-unit
  ((part-of open-university)))

(def-instance Faculty-of-Social-Sciences academic-unit
  ((part-of open-university)))

(def-instance Faculty-of-Technology academic-unit
  ((part-of open-university)))

(def-instance School-of-Education academic-unit
  ((part-of open-university)))

(def-instance School-of-Health-and-Social-Welfare academic-unit
  ((part-of open-university)))

(def-instance The-Open-University-Business-School academic-unit
  ((part-of open-university)))

(def-instance Centre-for-Modern-Languages academic-unit
  ((part-of open-university)))

(def-instance Institute-of-Educational-Technology academic-unit
  ((part-of open-university)))

(def-instance Academic-Administration-Division support-unit
  ((part-of open-university)))

(def-instance Academic-Computing-Service support-unit
  ((part-of open-university)))

(def-instance BBC-Open-University-Production-Centre support-unit
  ((part-of open-university)))

(def-instance Customer-Relations-Centre support-unit
  ((part-of open-university)))

(def-instance Library support-unit
  ((part-of open-university)))

(def-instance Office-for-Technology-Development support-unit
  ((part-of open-university)))

(def-instance Open-University-Conference-Centre support-unit
  ((part-of open-university)))

(def-instance Open-University-Validation-Services support-unit
  ((part-of open-university)))

(def-instance Operations support-unit
  ((part-of open-university)))

(def-instance Quality-Support-Centre support-unit
  ((part-of open-university)))

(def-instance Regional-Academic-Services support-unit
  ((part-of open-university)))

(def-instance Regional-Centres support-unit
  ((part-of open-university)))

(def-instance Services-To-Disabled-Students support-unit
  ((part-of open-university)))

(def-instance Vice-Chancellors-Office support-unit
  ((part-of open-university)))



