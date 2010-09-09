;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology egovernment-change-of-circumstances-ontology)

(def-class egovernment-change-of-circumstances-ontology-top-class ())

(def-class thing (egovernment-change-of-circumstances-ontology-top-class))

(def-class tangible-thing (thing))

(def-class intangible-thing (thing))

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

