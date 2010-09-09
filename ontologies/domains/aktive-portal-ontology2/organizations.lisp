;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology aktive-portal-ontology2)


(def-class GENERIC-AGENT (temporal-thing)
  "This is a generic notion, an agent can be an organization, a person
   an animal, a software agent, etc"
  ((has-web-address :type URL)
   (has-email-address :type email-address)))



(def-class LEGAL-AGENT (generic-agent)
  "Some agents have legal status: definitely organizations and people,
   anybody else?"
  ((has-telephone-number :type string)
   (has-fax-number :type string)
   (has-postal-address :type postal-address)))




(def-class ORGANIZATION (legal-agent)
  "An organization is a type of legal agent"
  (
   (affiliated-people :type affiliated-person) 
   (organization-part-of :type organization)
   (has-sub-unit :type organization-unit)
   (headed-by :type affiliated-person)
   (has-size :cardinality 1 :type organization-size)
   ))


(def-class POLITICAL-ORGANIZATION (organization)
  "An organization which has a political connotation")


(def-class ORGANIZATION-SIZE (intangible-thing)
  "We use EU guidelines to distinguish between different organization sizes")


(def-instance VERY-LARGE-SIZE organization-size
  "An organization with over 10000 employees")

(def-instance LARGE-SIZE organization-size
  "An organization with more than 250 employees")

(def-instance MEDIUM-SIZE organization-size
  "An organization with no more than 250 employees.  It also has to be independent,
   i.e., less than 25% owned by one enterprise (or jointly by several enterprises) 
   falling outside the definition of medium-sized enterprise.
   Finally, either the turnover total must be less than 40M Euros or the balance sheet 
   total must be less than 27M Euros.")

(def-instance SMALL-SIZE organization-size
  "An organization with no more than 50 employees.  It also has to be independent,
   i.e., less than 25% owned by one enterprise (or jointly by several enterprises) 
   falling outside the definition of small-sized enterprise.
   Finally, either the turnover total must be less than 7M Euros or the balance sheet 
   total must be less than 5M Euros.")

(def-instance MICRO-SIZE organization-size
  "An organization with no more than 10 employees.  It also has to be independent,
   i.e., less than 25% owned by one enterprise (or jointly by several enterprises) 
   falling outside the definition of micro-sized enterprise.")


(def-class SMALL-OR-MEDIUM-SIZED-ORGANIZATION (organization) ?x
  "SME are important, so we define a class to represent them explicitly.
   In some case we might not know or we do not want to bother specifying 
   excatly whether something is a small-organization or a medium-organization.
   Hence, we can just say 'x is a SME' without going into further detail."
  :iff-def (and (organization ?x)
                (has-size ?x  ?size)
                (member ?size '(micro-size small-size medium-size)))
   :avoid-infinite-loop t)


(def-class ORGANIZATION-UNIT (generic-agent)
  "An organization may have a number of units. Units may themselves have sub-units"
  (
   (has-telephone-number :type string)
   (has-fax-number :type string)
   (has-postal-address :type postal-address) 
   (has-size :type  organization-size)
   (affiliated-people :type affiliated-person) 
   (unit-of-organization :type organization)
   (sub-unit-of-organization-unit :type organization-unit)
   (has-sub-unit :type organization-unit)
   (headed-by :type affiliated-person)))


(def-rule UNIT-OF-ORGANIZATION-IS-TRANSITIVE
  ((unit-of-organization ?u ?o)
   if
   (sub-unit-of-organization-unit ?u ?u-super)
   (unit-of-organization ?u-super ?o)))
   


(def-class NON-PROFIT-ORGANIZATION (organization))

(def-class PROFIT-ORGANIZATION (organization)
  ((subsidiary-of :type profit-organization)))

(def-class PARTNERSHIP (profit-organization)
  "A partnership is not necessarily a company, e.g. a consultancy firm is 
   not a company")

(def-class COMPANY (profit-organization))

(def-class PRIVATE-COMPANY (company))

(def-class PUBLIC-COMPANY (company))

(def-class INDUSTRIAL-ORGANIZATION (profit-organization ))

(def-class GOVERNMENT-ORGANIZATION (non-profit-organization))

(def-class CIVIL-SERVICE (GOVERNMENT-ORGANIZATION))

(def-class GOVERNMENT (GOVERNMENT-ORGANIZATION)
  ((government-of-country :type country)))


(def-class CHARITABLE-ORGANIZATION (non-profit-organization))

(def-class LEARNING-CENTRED-ORGANIZATION (organization))

(def-class R&D-INSTITUTE (learning-centred-organization ))

(def-class R&D-INSTITUTE-WITHIN-LARGER-ORGANIZATION (r&d-institute organization-unit))

(def-class EDUCATIONAL-ORGANIZATION (learning-centred-organization ))

(def-class HIGHER-EDUCATIONAL-ORGANIZATION (educational-organization)
  ((has-academic-unit :type academic-unit) 
   (has-support-unit :type academic-support-unit)))


(def-rule HAS-ACADEMIC-UNIT-IMPLIES-HAS-ORGANIZATION-UNIT
  ((has-sub-unit ?x ?y)
   if
   (has-academic-unit ?x ?y)))

(def-rule HAS-SUPPORT-UNIT-IMPLIES-HAS-ORGANIZATION-UNIT
  ((has-sub-unit ?x ?y)
   if
   (has-support-unit ?x ?y)))


(def-class UNIVERSITY (higher-educational-organization)
  ((has-faculty :type university-faculty)
   (has-vice-chancellor :type educational-employee))
  :slot-renaming ((has-vice-chancellor headed-by)))



(def-class DISTANCE-TEACHING-UNIVERSITY (university))

(def-class SCHOOL (educational-organization))


(def-class EDUCATIONAL-ORGANIZATION-UNIT (organization-unit)
  ((unit-of-organization :type educational-organization)
   ))

(def-class ACADEMIC-UNIT (educational-organization-unit)
  ((unit-of-organization :type university)
   ))

(def-class UNIVERSITY-FACULTY (academic-unit))


(def-class ACADEMIC-SUPPORT-UNIT (educational-organization-unit))

(def-class PUBLISHING-HOUSE (organization))




(def-class LOCATION (tangible-thing)
  "A generic class for locations.  It includes both real and 
   fantastic places")


(def-class GEOGRAPHICAL-REGION (tangible-thing location)
  "'Real' geographical regions")



(def-class GEOPOLITICAL-ENTITY (Geographical-Region Generic-Agent)
  "A geopolitical entity is a geographical area which is associated with some sort of 
   political structure. For instance, Russia, Italy, The-city-of-Messina, etc..
   A geopolitical entity can be also seen as an agent - e.g., France declared 
   war to Spain")

(def-class COUNTRY (Geopolitical-Entity)
  ((has-capital :type capital-city)
   (has-currency :type currency)
   (has-government :type government)))

(def-class CURRENCY (information-bearing-object)
   ((issued-by :type government)))

(def-class AMOUNT-OF-MONEY (Physical-Quantity)
  ((has-unit-of-measure :type currency)
   (has-amount :type number))
  :slot-renaming ((has-amount has-magnitude)))
  
(def-class MUNICIPAL-UNIT (geopolitical-entity))

(def-class CITY (municipal-unit))

(def-class TOWN (municipal-unit))

(def-class CAPITAL-CITY (city)
  ((is-capital-of :type country)))

(def-axiom CONSISTENCY-BETWEEN-COUNTRIES-AND-CAPITAL-CITIES
  (<=> (is-capital-of ?city ?country) 
       (has-capital ?country ?city)))


(def-class VILLAGE (municipal-unit))

(def-class LOCAL-DISTRICT (geopolitical-entity))



(def-class ADDRESS (abstract-information)
  "A generic class for addresses, whether email or postal.
   We see an address as abstract information and therefore it is an intangible thing")




(def-class POSTAL-ADDRESS (address)
  "Modified to allow addresses to be given as strings, with no structure"
 ((address-street :type string)
   (address-area :type local-district)
   (address-number :type integer)
   (address-building :type string)
   (address-city-or-village :type municipal-unit)
   (address-postcode :type string)
   (address-region :type geographical-region)
   (address-country :type country)
   (address-pretty-label :type string))
 :slot-renaming ((address-pretty-label has-pretty-name )))


(def-class EMAIL-ADDRESS (string address) ?x
 :sufficient-for-type-checking (string ?x))


(def-class PERSON (legal-agent tangible-thing)
  ((full-name :type string)
   (family-name :type string)
   (given-name :type string)
   (has-gender :type gender)
   (has-academic-degree :type academic-degree)
   (has-appellation :type appellation))
  :slot-renaming ((full-name has-pretty-name)
                  ))


(def-class APPELLATION (Intangible-thing))

(def-instance DR appellation)

(def-instance MR appellation)

(def-instance MS appellation)

(def-instance MISS appellation)

(def-instance PROF appellation)

(def-instance MRS appellation)

(def-instance SIR appellation)

(def-instance LADY appellation)


(def-class GENDER (Intangible-thing) ?x
  "HPKB says that genders are intangible..Uhm..."
  :iff-def (element-of  ?x (set-of male-gender female-gender)))

(def-instance MALE-GENDER gender)

(def-instance FEMALE-GENDER gender)

(def-class AFFILIATED-PERSON (person)
  "A person which has an affiliation with some organization.
   For instance employees are affiliated to the organization they work for,
   students to the institution where they are studying, etc..
   A person can have multiple affiliations, which means that there is no 
   constraint relating the values of slot has-affiliation-to-unit to the values 
   of slot has-affiliation"
  ((has-affiliation-to-unit :type organization-unit)
   (has-affiliation :type organization :min-cardinality 1)))

(def-class WORKING-PERSON (person)
  ((has-work-status :type work-status)))


(def-class WORK-STATUS (Intangible-thing))

(def-instance FULL-TIME work-status)

(def-instance PART-TIME  work-status)


(def-class SELF-EMPLOYED-PERSON (working-person))



                        
(def-class EMPLOYEE (affiliated-person working-person) ?x
  ((works-for :type organization)
   (works-in-unit :type organization-unit)
   (has-job-title :type string)
   (has-contract-type :type employment-contract-type)
   )
  :slot-renaming ((works-for has-affiliation)
                  (works-in-unit has-affiliation-to-unit)))


(def-class EMPLOYMENT-CONTRACT-TYPE (Intangible-thing))


(def-instance PERMANENT-CONTRACT employment-contract-type)

(def-instance TEMPORARY-CONTRACT  employment-contract-type)


(def-class SYSTEM-ADMINISTRATOR (employee)) 

(def-class MULTIMEDIA-DESIGNER (employee)) 

(def-class GRAPHIC-DESIGNER (employee)) 

(def-class SECRETARY (employee))


(def-class EDUCATIONAL-EMPLOYEE (employee) ?x
  ((works-for :min-cardinality 1 :type educational-organization))
  :iff-def (and  (works-for ?x ?y)
                 (educational-organization ?y))
  
  :no-proofs-by (:iff-def)) ;;to avoid endless loops with the renaming in 
                            ;;the definition of Employee

(def-class ACADEMIC-SUPPORT-STAFF (educational-employee)
  ((works-for :type higher-educational-organization)
   ))

(def-class PROJECT-OFFICER-IN-ACADEMIA (academic-support-staff))

(def-relation HAS-RESEARCH-INTEREST (?x ?r)
  "People and in general even organizations and organization units
   may have research interests.  This relation shoudl be used to 
   specify them"
  :constraint (and (or (organization-unit ?x)
                       (organization ?x)
                       (person ?x))
                   (research-area ?r)))

(def-class STUDENT (affiliated-person)
  ((studies-at :type educational-organization :min-cardinality 1)
   (studies-in-unit :type educational-organization-unit :min-cardinality 1))
   :slot-renaming ((studies-at has-affiliation)
                   (studies-in-unit has-affiliation-to-unit)))

(def-class PHD-STUDENT (student)
  ((has-research-interest :type research-area)
   (studies-at :type higher-educational-organization :min-cardinality 1)
   (has-supervisor :type person)))

(def-class ACADEMIC (educational-employee)
  ((works-for :type higher-educational-organization)
   (has-research-interest :type research-area)))

(def-class RESEARCHER (working-person)
  ((has-research-interest :type research-area)))

(def-class VISITING-RESEARCHER (researcher affiliated-person)
  ((organization-being-visited :type organization)
   (organization-unit-being-visited :type organization-unit)
   (person-being-visited :type employee)))

(def-rule VISITING-RESEARCHER-GETS-AFFILIATION-TO-VISITED-ORGANIZATION
  ((has-affiliation ?x ?y)
   if
   (visiting-researcher ?x)
   (organization-being-visited ?x ?y)))

(def-rule VISITING-RESEARCHER-GETS-AFFILIATION-TO-VISITED-ORGANIZATION-UNIT
  ((has-affiliation-to-unit ?x ?y)
   if
   (visiting-researcher ?x)
   (organization-unit-being-visited ?x ?y)))


(def-class RESEARCHER-IN-ACADEMIA (academic researcher))

(def-class RESEARCH-FELLOW-IN-ACADEMIA  (researcher-in-academia))

(def-class SENIOR-RESEARCH-FELLOW-IN-ACADEMIA  (researcher-in-academia))

(def-class RESEARCH-ASSISTANT-IN-ACADEMIA  (researcher-in-academia))

(def-class LECTURER-IN-ACADEMIA  (academic))

(def-class SENIOR-LECTURER-IN-ACADEMIA  (academic))

(def-class PROFESSOR-IN-ACADEMIA  (academic))

(def-class READER-IN-ACADEMIA  (academic))

;;;Some sample instances below.....



(def-instance THE-OPEN-UNIVERSITY distance-teaching-university
  ((has-web-address "http://www.open.ac.uk")
   (has-sub-unit knowledge-media-institute-at-the-open-university)
   (has-size large-size)))


(def-instance UNIVERSITY-OF-SOUTHAMPTON university
  ((has-web-address "http://www.ecs.soton.ac.uk")
   (has-academic-unit dept-of-electronics-and-computer-science-at-southampton)
   (has-size large-size)))

(def-instance UNIVERSITY-OF-EDINBURGH university
  ((has-size large-size)
   (has-web-address "http://www.ed.ac.uk/")
   (has-academic-unit division-of-informatics-at-edinburgh)))

(def-instance UNIVERSITY-OF-SHEFFIELD university
  ((has-web-address "http://www.shef.ac.uk")
   (has-academic-unit dept-of-computer-science-at-sheffield)
   (has-size large-size)))

(def-instance UNIVERSITY-OF-ABERDEEN university
  ((has-size large-size)
   (has-web-address "http://www.abdn.ac.uk/")
   (has-academic-unit dept-of-computing-science-at-aberdeen)))

(def-instance EPSRC non-profit-organization
  ((has-size small-size)))


(def-instance DEPT-OF-COMPUTING-SCIENCE-AT-ABERDEEN academic-unit
  ((has-web-address "http://www.csd.abdn.ac.uk/")
   (unit-of-organization  university-of-aberdeen)))

(def-instance DEPT-OF-COMPUTER-SCIENCE-AT-SHEFFIELD academic-unit
  ((has-web-address "http://www.dcs.shef.ac.uk")
   (unit-of-organization university-of-sheffield)))
  


(def-instance KNOWLEDGE-MEDIA-INSTITUTE-AT-THE-OPEN-UNIVERSITY  r&d-institute-within-larger-organization
  ((has-pretty-name "Knowledge Media Institute")
   (has-web-address "http://kmi.open.ac.uk")
   (has-telephone-number "+44 1908 653800")
   (has-size medium-size)
   (unit-of-organization the-open-university)))


(def-instance FACULTY-OF-SCIENCE-AND-ENGINEERING-AT-EDINBURGH university-faculty
  ((unit-of-organization university-of-edinburgh)))

(def-instance DIVISION-OF-INFORMATICS-AT-EDINBURGH academic-unit
  ((has-web-address "http://www.informatics.ed.ac.uk/")
   (sub-unit-of-organization-unit faculty-of-science-and-engineering-at-edinburgh)
   (unit-of-organization university-of-edinburgh)))

(def-instance DEPT-OF-ELECTRONICS-AND-COMPUTER-SCIENCE-AT-SOUTHAMPTON academic-unit
  ((unit-of-organization university-of-southampton)))
  



(def-instance JOHN-DOMINGUE senior-research-fellow-in-academia 
  ((has-research-interest e-commerce-research-area
                          software-visualization
                          knowledge-management
                          organizational-learning)
   (works-for the-open-university)
   (works-in-unit knowledge-media-institute-at-the-open-university )
   (has-job-title "Deputy Director, Knowledge Media Institute")
   (has-work-status full-time)
   (has-appellation dr)
   (has-email-address "j.b.domingue@open.ac.uk")
   (has-web-address "http://kmi.open.ac.uk/people/john")
   (has-gender  male-gender)
   (full-name "john domingue")))

(def-instance NIGEL-SHADBOLT professor-in-academia
  "What does 'R.' stand for in 'Nigel R. Shadbolt'?"
  ((has-research-interest knowledge-acquisition
                          knowledge-management)
   (works-for University-of-southampton)
   (works-in-unit dept-of-electronics-and-computer-science-at-southampton)
   (has-job-title "Professor of Knowledge Something")
   (has-work-status full-time)
   (has-appellation prof)
   (has-email-address "nrs@ecs.soton.ac.uk")
   (has-web-address "http://www.ecs.soton.ac.uk/~nrs/")
   (has-gender  male-gender)
   (full-name "Nigel Rodriguez Shadbolt")))

(def-instance NATO political-organization
  "nato is both a political organization and a geopolitical entity")

(def-instance NATO geopolitical-entity
  "nato is both a political organization and a geopolitical entity")
 



