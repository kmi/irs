;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology egovernment-upper-level-ontology)

(def-class thing ())


(def-class temporal-thing ()
  "Like in Cyc, this is something which has a temporal extent."
  ((has-duration :type duration)
   (has-start-time :type time-point)
   (has-end-time :type time-point)
  ))

(def-class tangible-thing (temporal-thing))

(def-class intangible-thing (temporal-thing))

(def-class year-in-time ()?x
"A year-in-time must be an integer and integer can be a year-in-time"
:iff-def (integer ?x))

(def-class month-in-time ()?x
"A month-in-time is an integer in the interval 0-12"
:iff-def (and (integer ?x)(< ?x 13) (or (= ?X 0)(> ?x 0))))

(def-class week-in-time () ?x
  "A week must be an integer in the interval of 0-7"
        :iff-def (and (integer ?x) (or (< ?x 7) (= ?x 7)) (> ?x 0)))

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


(def-class time-entity () ?te
 ((second-of :type second-in-time :default-value 0 :max-cardinality 1)
  (minute-of :type minute-in-time :default-value 0 :max-cardinality 1)
  (hour-of :type hour-in-time  :default-value 0 :max-cardinality 1)
  (day-of :type day-in-time :default-value 0 :max-cardinality 1)
  (month-of :type month-in-time :default-value 0 :max-cardinality 1)
  (year-of :type year-in-time :default-value 0 :max-cardinality 1))
 :constraint (and (not (and (month-of ?x 2)
                            (> (the ?day (day-of ?x ?day))
                               29)))
                  (not (and (member-of ?x (4 6 9 11))
                            (> (the ?day (day-of ?x ?day))
                               30))))) 

(def-class duration (time-entity) ?d
 )

(def-class time-point(time-entity) ?tp
 )

(def-class unit-of-time ()
 "This can be a second, a month, a year, a day, etc..")


(def-class calendar-date (time-point)
 "A calendar date is a time point in which month, day and year have 
  been specified"
  ((day-of :type day-in-time :cardinality 1)
   (month-of :type month-in-time :cardinality 1)
   (year-of :type year-in-time :cardinality 1)))



(def-class activity (intangible-thing))

(def-class assistance (activity))

(def-class service (assistance))

(def-class public-service (service)
  ((need-to-pay-for-service :type boolean :value true))
)

(def-class private-service (service)
  ((need-to-pay-for-service :type boolean :value true))
)

(def-class county-council-service (public-service)
  ((provided-by :type county-council))
)

(def-class borough-council-service (public-service)
  ((provided-by :type borough-council))
)

(def-class central-government-service (public-service)
  ((provided-by :type national-government))
)

(def-class parish-service (public-service)
  ((provided-by :type parish-council))
)

(def-class county-registration-service (county-council-service)
)

(def-class county-council-birth-registration-service (county-registration-service)
)

(def-class county-council-marriage-registration-service (county-registration-service)
)

(def-class county-council-death-registration-service (county-registration-service)
)

(def-class county-council-planning-service (county-council-service)
)

(def-class county-council-highways-and-transportation-service (county-council-planning-service)
)

(def-class county-council-learning-service (county-council-service)
)

(def-class county-council-environment-service (county-council-service)
)

(def-class county-council-waste-and-recycling-service (county-council-environment-service)
)

(def-class county-council-culture-art-heritage-service (county-council-service)
)

(def-class county-council-administration-service (county-council-service)
)

(def-class county-council-health-and-caring-service (county-council-service)
)

(def-class county-council-health-service (county-council-health-and-caring-service)
)

(def-class county-council-care-service (county-council-health-and-caring-service)
)


(def-class GENERIC-AGENT (thing)
  "This is a generic notion, an agent can be an organization, a person
   an animal, a software agent, etc"
  ((has-name :type string :min-cardinality 0) 
   (has-acronym :type string :min-cardinality 0)
   (has-web-address :type string :min-cardinality 0)
   (has-email-address :type string :min-cardinality 0)))

(def-class LEGAL-AGENT (generic-agent)
  "Some agents have legal status: definitely organizations and people,
   anybody else?"
  ((has-telephone-number :type string :min-cardinality 0)
   (has-fax-number :type string :min-cardinality 0)
   (has-postal-address :type string :min-cardinality 0)))

(def-class ORGANIZATION (generic-agent)
  "An organization may have a number of units. Units may themselves have sub-units"
  (
   (has-telephone-number :type string :min-cardinality 0)
   (has-fax-number :type string :min-cardinality 0)
   (has-postal-address :type postal-address :min-cardinality 0) 
   (has-size :type  organization-size :min-cardinality 0)
   (headed-by :type affiliated-person :min-cardinality 0)))

(def-class ORGANIZATION-SIZE  (intangible-thing)
  "We use EU guidelines to distinguish between different organization sizes"
  ((min-number-employees :type number)
  (max-number-employees :type number :min-cardinality 0))

)


(def-instance VERY-LARGE-SIZE organization-size
  "An organization with over 10000 employees"
  ((min-number-employees 10001))
)

(def-instance LARGE-SIZE organization-size
  "An organization with more than 250 employees"
  ((min-number-employees 250)
  (max-number-employees 10000))
)

(def-instance MEDIUM-SIZE organization-size
  "An organization with no more than 250 employees.  It also has to be independent,
   i.e., less than 25% owned by one enterprise (or jointly by several enterprises) 
   falling outside the definition of medium-sized enterprise.
   Finally, either the turnover total must be less than 40M Euros or the balance sheet 
   total must be less than 27M Euros."
  ((min-number-employees 51)
   (max-number-employees 250))


)

(def-instance SMALL-SIZE organization-size
  "An organization with no more than 50 employees.  It also has to be independent,
   i.e., less than 25% owned by one enterprise (or jointly by several enterprises) 
   falling outside the definition of small-sized enterprise.
   Finally, either the turnover total must be less than 7M Euros or the balance sheet 
   total must be less than 5M Euros."
  ((min-number-employees 11)
  (max-number-employees 50))

)

(def-instance MICRO-SIZE organization-size
  "An organization with no more than 10 employees.  It also has to be independent,
   i.e., less than 25% owned by one enterprise (or jointly by several enterprises) 
   falling outside the definition of micro-sized enterprise."
   ((min-number-employees 1)
   (max-number-employees 10))
)



(def-class EDUCATIONAL-ORGANIZATION (organization)
  ((unit-of-organization :type educational-organization)
   ))

(def-class POLITICAL-ORGANIZATION (organization)
  "An organization which has a political connotation")

(def-class NON-PROFIT-ORGANIZATION (organization)
  "An organization which has a political connotation")

(def-class CENTRAL-GOVERNMENT-ORGANIZATION (political-organization)
  "An organization which has a political connotation")

(def-class LOCAL-GOVERNMENT-ORGANIZATION (political-organization)
  "An organization which has a political connotation")

(def-class COUNTY-COUNCIL (local-government-organization))


(def-class NATIONAL-GOVERNMENT (central-government-organization))


(def-class BOROUGH-COUNCIL (local-government-organization))


(def-class PARISH-COUNCIL (local-government-organization))


(def-instance ESSEX-COUNTY-COUNCIL county-council
  ((has-name "Essex County Council") 
   (has-acronym "ECC")
   (has-web-address "http://www.essexcc.gov.uk/")
   (has-postal-address "PO Box 11")
   (has-size large-size)))

