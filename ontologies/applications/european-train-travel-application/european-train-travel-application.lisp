;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology european-train-travel-application)

(def-rule county-is-in-country-rule 
  ((county-is-in-country ?county england) if
   (english-county ?county))
  ((county-is-in-country ?county wales) if
   (welsh-county ?county))
  ((county-is-in-country ?county scotland) if
   (scottish-county ?county))
  ((county-is-in-country ?county ireland) if
   (irish-county ?county)))
  
(def-rule country-is-in-continent-rule 
  ((country-is-in-continent ?country europe) if
   (european-country ?country))

  ((country-is-in-continent ?country asian) if
   (asian-country ?country))

  ((country-is-in-continent ?country north-america) if
   (north-american-country ?country))

  ((country-is-in-continent ?country south-america) if
   (south-american-country ?country))

  ((country-is-in-continent ?country oceana) if
   (oceanian-country ?country))

  ((country-is-in-continent ?country africa) if
   (african-country ?country)))
  
(def-relation is-in-country (?x ?country)
  :sufficient
  (or (located-in-country ?x ?country)
      (and (has-county ?x ?county)
           (county-is-in-country ?county ?country))
      (is-capital-of ?x ?country)))

(def-relation is-in-geographical-region (?x ?region)
  :sufficient
  (or (is-in-country ?x ?region)
      (has-county ?x ?region)
      (and (is-in-country ?x ?country)
           (country-is-in-continent ?country ?region))))

(def-class person ()
  ((has-name :type string)
   (gold-card-customer :type boolean)
   (accept-upgrade :type boolean)))

(def-class business-person (person))

(def-class academic (person))

(def-class professor (academic))

(def-class student (person))

(def-class professor2 ()
  ((has-full-name :type string)))

(def-rule mapping-rule
  ((name-mapping ?professor-1 ?name) if
   (has-name ?professor-1 ?name)))

(def-rule mapping-rule2
  ((name-mapping2 ?professor-2 ?name) if
   (has-full-name ?professor-2 ?name)))



(def-instance john student
  ((has-name "John")))

(def-instance matt student
  ((has-name "Matt")))

(def-instance Michal student
  ((has-name "Michal")))

(def-instance liliana business-person
  ((has-name "Liliana")))

(def-instance michael business-person
  ((has-name "Michael")))

(def-instance christoph professor
  ((has-name "Christoph")
   (gold-card-customer :true)
   (accept-upgrade :true)))

(def-instance alistair academic
  ((has-name "alistair")
   (gold-card-customer :true)))

(def-instance Sinuhe academic
  ((has-name "Sinuhe")))

(def-instance barry academic
  ((has-name "Barry")))

(def-instance carlos academic
  ((has-name "Carlos")))

