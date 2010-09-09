;;; Mode: Lisp; Package: ocml

;;; Author: Liliana Cabral, John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology egovernment2)

(def-class e-gov-top ())

(def-class marital-status (E-GOV-TOP))

(def-class time-point (E-GOV-TOP)
 "Something which is conceptually a point in time, even if partially specified"
	((minute-of :type minute-in-time :max-cardinality 1)
	(second-of :type second-in-time :max-cardinality 1)
	(hour-of :type hour-in-time :max-cardinality 1)
	(day-of :type day-in-time :max-cardinality 1)
	(month-of :type month-in-time :max-cardinality 1)
	(year-of :type year-in-time :max-cardinality 1)))

(def-class calendar-date (time-point)
 "A calendar date is a time point in which month, day and year have 
  been specified"
  ((day-of :type day-in-time :cardinality 1)
   (month-of :type month-in-time :cardinality 1)
   (year-of :type year-in-time :cardinality 1)))

(def-class year-in-time ()?x
	"A year-in-time must be an integer and integer can be a year-in-time"
	:iff-def (integer ?x))

(def-class month-in-time ()?x
	"A month-in-time is an integer in the interval 0-12"
	:iff-def (and (integer ?x)(< ?x 13) (or (= ?X 0)(> ?x 0))))

(def-class day-in-time ()?x
	"A day-in-time is an integer in the interval 0-31"
	:iff-def (and (integer ?x)(< ?x 32) (or (= ?X 0)(> ?x 0))))

(def-class hour-in-time ()?x
	"A hour-in-time is an integer in the interval 0-23"
	:iff-def (and (integer ?x)(< ?x 24) (or (= ?X 0)(> ?x 0))))

(def-class second-in-time ()?x
	"A second-in-time is an integer in the interval 0-59"
	:iff-def (and (integer ?x)(< ?x 60) (or (= ?X 0)(> ?x 0))))

(def-class minute-in-time ()?x
	"A minute-in-time is an integer in the interval 0-59"
	:iff-def (and (integer ?x)(< ?x 60) (or (= ?X 0)(> ?x 0))))

(def-class thing (E-GOV-TOP))

(def-class GENERIC-AGENT (E-GOV-TOP)         ;;(temporal-thing)
  "This is a generic notion, an agent can be an organization, a person
   an animal, a software agent, etc"
  ((has-web-address :type URL)
   (has-email-address :type email-address)))

(def-class CLIENT (E-GOV-TOP)
 "A client is a generic notion. It can be a citizen, a couple or an asset"
 ((has-swift-id :type string :cardinality 1)
  (has-legal-representative :type citizen :cardinality 1)
  (involved-in-case :type case :min-cardinality 1)
 ))

(def-class ENTITLEMENT (E-GOV-TOP )
 ((has-entitlement-type :type entitlement-type :cardinality 1)
  (has-entitlement-provider :type entitlement-provider :cardinality 1)
  (has-payment :type payment :cardinality 1)
 ))

(def-class ENTITLEMENT-TYPE (intangible-thing) ;;john d 3/12/04 (E-GOV-TOP )
 ((has-eligibility-criteria :type eligibility-criteria)))

(def-class ELIGIBILITY-CRITERIA (E-GOV-TOP )
 ((has-description :type string)
  (has-eligibility-criteria-function :type kappa-expression)
))

(def-class PAYMENT (intangible-thing) ;;john d 3/12/04 (E-GOV-TOP )
 ((for-entitlement :type entitlement :cardinality 1)
  (payment-status :type string :cardinality 1)))

(def-class CASE (E-GOV-TOP)
 ((has-status :type string )
  (has-originator :type string )
  (has-case-worker :type case-worker :cardinality 1)
  (has-client :type client :cardinality 1)
  (has-assessment :type assessment :cardinality 1)
  (has-decision :type decision :cardinality 1)
 ))

(def-class assessment (intangible-thing) ;;john d 3/12/04 (E-GOV-TOP)
 ((applies-to-case :type case :cardinality 1)
  (has-facts :type string )
  (has-perceptions :type string )
  (has-decision :type decision :cardinality 1)
 ))

(def-class DECISION (E-GOV-TOP)
 ((applies-to-assessment :type assessment :cardinality 1)
  (has-entitlement :type entitlement )
 ))

(def-class couple (intangible-thing) ;;3/12/04 (e-gov-top)
 ((has-citizen1 :type citizen)
  (has-citizen2 :type citizen)))

(def-class asset (thing) ;;john d 3/12/04 (e-gov-top)
 ((has-asset-owner :type citizen)))

(def-class acommodation-type (intangible-thing) ;;john d 3/12/04 (e-gov-top)
   )

(def-class tangible-thing (thing))

(def-class intangible-thing (thing))

(def-class LEGAL-AGENT (generic-agent)
  "Some agents have legal status: definitely organizations and people,
   anybody else?"
  ((has-telephone-number :type string)
   (has-fax-number :type string)
   (has-postal-address :type postal-address)))

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

(def-class CURRENCY (thing) ;;(information-bearing-object)
   ((issued-by :type government)))

(def-class AMOUNT-OF-MONEY (thing) ;; (Physical-Quantity)
  ((has-unit-of-measure :type currency)
   (has-amount :type number))
  :slot-renaming ((has-amount has-magnitude)))

(def-class couple-client (couple CLIENT))

(def-class asset-client (asset CLIENT))

(def-class SERVICE (ENTITLEMENT ))

(def-class BENEFIT (ENTITLEMENT )
 ((has-calculation-statement :type string :cardinality 1)
 ))

(def-class ORGANIZATION (legal-agent)
  "An organization is a type of legal agent"
  (
   (affiliated-people :type affiliated-person) 
   (organization-part-of :type organization)
   (has-sub-unit :type organization-unit)
   (headed-by :type affiliated-person)
   (has-size :cardinality 1 :type organization-size)
  ))

(def-class ORGANIZATION-SIZE  (intangible-thing)
  "We use EU guidelines to distinguish between different organization sizes")

(def-class EDUCATIONAL-ORGANIZATION-UNIT (organization-unit)
  ((unit-of-organization :type educational-organization)
   ))

(def-class LOCATION (tangible-thing)
  "A generic class for locations.  It includes both real and 
   fantastic places")

(def-class PERSON ( tangible-thing)
  ((full-name :type string)
   (family-name :type string)
   (given-name :type string)
   (has-gender :type gender)
   (has-academic-degree :type academic-degree)
   (has-appellation :type appellation))
  :slot-renaming ((full-name has-pretty-name)))

(def-class APPELLATION (Intangible-thing))

(def-class GENDER (Intangible-thing) ?x
  "HPKB says that genders are intangible..Uhm..."
  :iff-def (element-of  ?x (set-of male-gender female-gender)))

(def-class WORK-STATUS (Intangible-thing))

(def-class EMPLOYMENT-CONTRACT-TYPE (Intangible-thing))

(def-class AGENCY (E-GOV-TOP ORGANIZATION-UNIT ))

(def-class POLITICAL-ORGANIZATION (organization)
  "An organization which has a political connotation")

(def-class SMALL-OR-MEDIUM-SIZED-ORGANIZATION (organization) ?x
  "SME are important, so we define a class to represent them explicitly.
   In some case we might not know or we do not want to bother specifying 
   excatly whether something is a small-organization or a medium-organization.
   Hence, we can just say 'x is a SME' without going into further detail."
  :iff-def (and (organization ?x)
                (has-size ?x  ?size)
                (member ?size '(micro-size small-size medium-size)))
   :avoid-infinite-loop t)

(def-class NON-PROFIT-ORGANIZATION (organization))

(def-class PROFIT-ORGANIZATION (organization)
  ((subsidiary-of :type profit-organization)))

(def-class LEARNING-CENTRED-ORGANIZATION (organization))

(def-class ACADEMIC-UNIT (educational-organization-unit)
  ((unit-of-organization :type university)
   ))

(def-class ACADEMIC-SUPPORT-UNIT (educational-organization-unit))

(def-class PUBLISHING-HOUSE (organization))

(def-class GEOGRAPHICAL-REGION (tangible-thing location)
  "'Real' geographical regions")

(def-class ENTITLEMENT-PROVIDER (agency )
 ((has-contact-person :type string :cardinality 1)
  (provides-entitlement :type entitlement-type :min-cardinality 1)
 ))

(def-class citizen-client (person CLIENT)
 ((full-name :type string)
  (family-name :type string)
  (given-name :type string)
  (has-gender :type gender)
  (has-address :type address)
  (has-marital-status :type marital-status)
  (has-religion :type religious-faith)
  (has-ethnicity :type british-ethnic-group-type)
  (has-type-of-accomodation :type accomodation-type)
  (has-date-of-birth :type date)))

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

(def-class EMPLOYEE (affiliated-person working-person) ?x
  ((works-for :type organization)
   (works-in-unit :type organization-unit)
   (has-job-title :type string)
   (has-contract-type :type employment-contract-type)
   )
  :slot-renaming ((works-for has-affiliation)
                  (works-in-unit has-affiliation-to-unit)))

(def-class CASE-WORKER (AFFILIATED-PERSON ))

(def-class SELF-EMPLOYED-PERSON (working-person))

(def-class PARTNERSHIP (profit-organization)
  "A partnership is not necessarily a company, e.g. a consultancy firm is 
   not a company")

(def-class COMPANY (profit-organization))

(def-class INDUSTRIAL-ORGANIZATION (profit-organization ))

(def-class GOVERNMENT-ORGANIZATION (non-profit-organization))

(def-class CHARITABLE-ORGANIZATION (non-profit-organization))

(def-class R&D-INSTITUTE (learning-centred-organization ))

(def-class EDUCATIONAL-ORGANIZATION (learning-centred-organization ))

(def-class UNIVERSITY-FACULTY (academic-unit))

(def-class GEOPOLITICAL-ENTITY (Geographical-Region Generic-Agent)
  "A geopolitical entity is a geographical area which is associated with some sort of 
   political structure. For instance, Russia, Italy, The-city-of-Messina, etc..
   A geopolitical entity can be also seen as an agent - e.g., France declared 
   war to Spain")

(def-class SERVICE-PROVIDER (ENTITLEMENT-PROVIDER ))

(def-class BENEFIT-PROVIDER (ENTITLEMENT-PROVIDER ))

(def-class health-care-organization (non-profit-organization))

(def-class PRIVATE-COMPANY (company))

(def-class PUBLIC-COMPANY (company))

(def-class CIVIL-SERVICE (GOVERNMENT-ORGANIZATION))

(def-class GOVERNMENT (GOVERNMENT-ORGANIZATION)
  ((government-of-country :type country)))

(def-class R&D-INSTITUTE-WITHIN-LARGER-ORGANIZATION (r&d-institute organization-unit))

(def-class HIGHER-EDUCATIONAL-ORGANIZATION (educational-organization)
  ((has-academic-unit :type academic-unit) 
   (has-support-unit :type academic-support-unit)))

(def-class SCHOOL (educational-organization))

(def-class COUNTRY (Geopolitical-Entity)
  ((has-capital :type capital-city)
   (has-currency :type currency)
   (has-government :type government)))

(def-class MUNICIPAL-UNIT (geopolitical-entity))

(def-class LOCAL-DISTRICT (geopolitical-entity))

(def-class national-government-organization (government-organization ))

(def-class local-council-organization (government-organization ))

(def-class SOCIAL-WORKER (CASE-WORKER ))

(def-class UNIVERSITY (higher-educational-organization)
  ((has-faculty :type university-faculty)
   (has-vice-chancellor :type educational-employee))
  :slot-renaming ((has-vice-chancellor headed-by)))

(def-class CITY (municipal-unit))

(def-class TOWN (municipal-unit))

(def-class VILLAGE (municipal-unit))

(def-class county-council-organization (local-council-organization ))

(def-class district-council-organization (local-council-organization ))

(def-class borough-council-organization (local-council-organization ))

(def-class parish-council-organization (local-council-organization ))

(def-class national-health-care-organization (national-government-organization health-care-organization))

(def-class DISTANCE-TEACHING-UNIVERSITY (university))

(def-class CAPITAL-CITY (city)
  ((is-capital-of :type country)))

(def-axiom CONSISTENCY-BETWEEN-COUNTRIES-AND-CAPITAL-CITIES
  (<=> (is-capital-of ?city ?country) 
       (has-capital ?country ?city)))

(def-rule UNIT-OF-ORGANIZATION-IS-TRANSITIVE
  ((unit-of-organization ?u ?o)
   if
   (sub-unit-of-organization-unit ?u ?u-super)
   (unit-of-organization ?u-super ?o)))

(def-rule HAS-ACADEMIC-UNIT-IMPLIES-HAS-ORGANIZATION-UNIT
  ((has-sub-unit ?x ?y)
   if
   (has-academic-unit ?x ?y)))

(def-rule HAS-SUPPORT-UNIT-IMPLIES-HAS-ORGANIZATION-UNIT
  ((has-sub-unit ?x ?y)
   if
   (has-support-unit ?x ?y)))

(def-instance single-marital-status marital-status)

(def-instance married-marital-status marital-status)

(def-instance divorced-marital-status marital-status)

(def-instance cohabiting-marital-status marital-status)

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

(def-instance DR appellation)

(def-instance MR appellation)

(def-instance MS appellation)

(def-instance MISS appellation)

(def-instance PROF appellation)

(def-instance MRS appellation)

(def-instance SIR appellation)

(def-instance LADY appellation)

(def-instance MALE-GENDER gender)

(def-instance FEMALE-GENDER gender)

(def-instance FULL-TIME work-status)

(def-instance PART-TIME  work-status)

(def-instance essex-county-council GOVERNMENT-ORGANIZATION )

(def-instance a-private-company PRIVATE-COMPANY )

(def-instance community-care service-provider
  ((has-contact-person "someone")
  (provides-entitlement meals-on-wheels nursing-support)
  (unit-of-organization a-private-company)
 ))

(def-instance housing-department benefit-provider
  ((has-contact-person "someone")
  (provides-entitlement housing-benefit council-tax-benefit)
  (unit-of-organization essex-county-council)
 ))

(def-instance disability-and-carers service-provider
  ((has-contact-person "someone")
  (provides-entitlement carers-allowance)
  (unit-of-organization essex-county-council)
 ))

(def-instance planning-department entitlement-provider
  ((has-contact-person "someone")
  (provides-entitlement building-regulations-permission)
  (unit-of-organization essex-county-council)
 ))

(def-instance jobCentre-plus service-provider
  ((has-contact-person "someone")
  (provides-entitlement attendance-allowance pension-credit)
  (unit-of-organization some-private-organization)
 ))

(def-instance housing-benefit ENTITLEMENT-TYPE 
 ((has-eligibility-criteria eligibility-criteria1)
))

(def-instance council-tax-benefit ENTITLEMENT-TYPE )

(def-instance attendance-allowance ENTITLEMENT-TYPE )

(def-instance pension-credit ENTITLEMENT-TYPE )

(def-instance meals-on-wheels ENTITLEMENT-TYPE )

(def-instance nursing-support ENTITLEMENT-TYPE )

(def-instance carers-allowance ENTITLEMENT-TYPE )

(def-instance building-regulations-permission ENTITLEMENT-TYPE )

(def-instance mother CITIZEN-client
 ((has-swift-id "001")
  (has-legal-representative daughter)
  (involved-in-case mother-case)
  (full-name "mother Scott")
  (family-name "Scott")
  (has-gender female)
  (has-address mother-address)
  (has-marital-status "widow")
 ))

(def-instance daughter CITIZEN-client 
 ((has-swift-id "002")
  (involved-in-case daughter-case)
  (full-name "daughter Scott")
  (family-name "Scott")
  (has-gender female)
  (has-address daughter-address)
  (has-marital-status "single")
 ))

(def-instance mother-case-worker CASE-WORKER
  ((has-affiliation-to-unit :type housing-department)
   (full-name "worker full name")
   (has-gender female)
   (has-academic-degree social-sciences)
   (has-appellation MISS))
 )

(def-instance mother-case  CASE
 ((has-status "active" )
  (has-originator "Mother's GP")
  (has-case-worker mother-case-worker)
  (has-client mother)
  (has-assessment mother-case-assessment)
  (has-decision mother-case-decision)
 ))

(def-instance daughter-case  CASE
 ((has-status "active" )
  (has-originator "herself")
  (has-case-worker daughter-case-worker)
  (has-client daughter)
  (has-assessment daughter-case-assessment)
  (has-decision daughter-case-decision)
 ))

(def-instance mother-case-assessment ASSESSMENT
 ((applies-to-case mother-case)
  (has-facts "change of circumstance: moves to daughter's house" "is disabled") 
  (has-perceptions "eligible for benefits " )
  (has-decision mother-case-decision)
 ))

(def-instance daughter-case-assessment ASSESSMENT
 ((applies-to-case daughter-case)
  (has-facts "disabled mother moves in") 
  (has-perceptions "eligible for services" "permission to adapt the house " )
  (has-decision daughter-case-decision)
 ))

(def-instance mother-case-decision DECISION
 ((applies-to-assessment mother-case-assessment)
  (has-entitlement mother-housing-benefit 
                   mother-council-tax-benefit 
                   mother-pension-credit 
                   mother-meals-on-wheels 
                   mother-nursing-support )
 ))

(def-instance daughter-case-decision DECISION
 ((applies-to-assessment mother-case-assessment)
  (has-entitlement daughter-attendance-service 
                   daughter-carer-service 
                   daughter-building-permission)
 ))

(def-instance daughter-attendance-service SERVICE 
 ((has-entitlement-type attendance-allowance)
  (has-entitlement-provider job-Centre-plus)
  (has-payment daughter-payment-for-attendance-service)
 ))

(def-instance daughter-carer-service SERVICE 
 ((has-entitlement-type carers-allowance)
  (has-entitlement-provider disabilty-unit)
  (has-payment daughter-payment-for-carers-allowance)
 ))

(def-instance daughter-payment-for-carers-allowance PAYMENT
 ((for-entitlement daughter-carer-service)
  (payment-status "exempt")
 ))

(def-instance mother-housing-benefit BENEFIT 
 ((has-entitlement-type housing-benefit)
  (has-entitlement-provider housing-department)
  (has-payment mother-payment-for-housing-benefit)
 ))

(def-instance mother-council-tax-benefit BENEFIT 
 ((has-entitlement-type council-tax-benefit)
  (has-entitlement-provider housing-department)
  (has-payment mother-payment-for-council-tax-benefit)
 ))

(def-instance nhs-mental-health-unit organization-unit
  ((has-web-address )
   (has-email-address )
   (has-telephone-number )
   (has-fax-number )
   (has-postal-address )
   (has-size )
   (affiliated-people )
   (unit-of-organization  national-health-service)
   (sub-unit-of-organization-unit )
   (has-sub-unit )
   (headed-by )))

(def-instance national-health-service national-health-care-organization
  ((has-web-address )
   (has-email-address )
   (has-telephone-number )
   (has-fax-number )
   (has-postal-address )
   (affiliated-people )
   (organization-part-of )
   (has-sub-unit  nhs-mental-health-unit)
   (headed-by )
   (has-size  large-size)))

(def-instance department-of-work-and-pensions national-government-organization
  ((has-web-address "http://www.dwp.gov.uk/")
   (has-size large-size)
   (has-sub-unit the-pensions-service job-centre-plus 
                 disabilty-unit)))

(def-instance essex-county-council county-council-organization
  ((has-web-address )
   (has-email-address )
   (has-telephone-number )
   (has-fax-number )
   (has-postal-address )
   (affiliated-people )
   (organization-part-of )
   (has-sub-unit )
   (headed-by )
   (has-size large-size)))

(def-instance meals-on-wheels service-provider
  ((has-web-address )
   (has-email-address )
   (has-telephone-number "01245 434692")
   (has-fax-number )
   (has-postal-address )
   (has-size )
   (affiliated-people )
   (unit-of-organization  essex-county-council)
   (sub-unit-of-organization-unit )
   (has-sub-unit )
   (headed-by )
   (has-contact-person )
   (provides-entitlement )))

(def-instance essex-community-care-agency service-provider
  ((unit-of-organization  essex-county-council)))

(def-instance the-pensions-service service-provider
  ((unit-of-organization  department-of-work-and-pensions)
   (has-web-address "http://www.thepensionservice.gov.uk/")))

(def-instance job-centre-plus service-provider
  ((unit-of-organization  department-of-work-and-pensions)
   (has-web-address "http://www.jobcentreplus.gov.uk/")))

(def-instance disabilty-unit service-provider
  ((unit-of-organization  department-of-work-and-pensions)
   (has-web-address "http://www.disability.gov.uk/")))

(def-instance fred-bloggs citizen-client
  ((has-academic-degree )
   (has-appellation mr)
   (has-swift-id 2)
   (has-legal-representative )
   (involved-in-case )
   (full-name "Fred Bloggs")
   (family-name "Bloggs")
   (given-name "Fred")
   (has-gender male-gender)
   (has-address )
   (has-marital-status  single-marital-status)
   (has-religion  )
   (has-ethnicity white-ethnic-group)
   (has-type-of-accomodation )
   (has-date-of-birth )))

(def-instance jane-doe citizen-client
  ((has-academic-degree )
   (has-appellation ms)
   (has-swift-id 1)
   (has-legal-representative )
   (involved-in-case )
   (full-name "Jane Doe")
   (family-name "Doe")
   (given-name "Jane")
   (has-gender female-gender)
   (has-address )
   (has-marital-status  divorced-marital-status)
   (has-religion  hinduism)
   (has-ethnicity bangladeshi)
   (has-type-of-accomodation )
   (has-date-of-birth )))



(def-class calculate-automatically-class (intangible-thing) ;;john d 3/12/04 (e-gov-top)
  )

(def-instance calculate-automatically calculate-automatically-class)

(def-instance tracy-smith citizen-client
  ((has-academic-degree )
   (has-appellation  miss)
   (has-swift-id 3)
   (has-legal-representative )
   (involved-in-case tracy-smith-mother-moves-in-case)
   (full-name "Tracy Smith")
   (family-name "Smith")
   (given-name "Tracy")
   (has-gender  female-gender)
   (has-address )
   (has-marital-status )
   (has-religion )
   (has-ethnicity )
   (has-type-of-accomodation )
   (has-date-of-birth )))

(def-instance tracy-smith-mother-moves-in-case case
  ((has-status )
   (has-originator )
   (has-case-worker )
   (has-client  tracy-smith)
   (has-assessment  daughter-case-assessment)
   (has-decision )))
