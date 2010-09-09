;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology common-concepts)

(def-relation has-address (?x ?c)
  :constraint (and (or (organization ?x)
                       (person ?x))
                   (postal-address ?c))
  )

(def-relation HAS-WEB-ADDRESS (?x ?C)
  :constraint (URL ?c))

(def-relation has-author (?x ?C)
  "?C has been produced by ?x")

(def-class generic-agent ())

(def-class generic-agent-type () ?x
  :iff-def (subclass-of ?X generic-agent))

(def-class person (generic-agent temporal-thing)
  ((full-name :type string)
   (has-gender :type gender)
   (has-address :type postal-address)
   (has-web-address :type web-page)
   (has-email-address :type email-address)))

(def-class URL (string))

(def-class email-address ())

(def-class man (person)
  ((has-gender :value male)))

(def-class woman (person)
  ((has-gender :value female)))

(def-class child (person))

(def-class male-child (child)
  ((has-gender :value male)))

(def-class female-child (child)
  ((has-gender :value female)))

(def-class gender () ?x
  ()
  :iff-def (member ?x (male-gender female-gender)))

(def-instance male gender)

(def-instance female gender)


(def-class technology (temporal-thing)
  ((has-full-name :type string)
   (has-author :type person)
   (made-by :type organization)
   (technology-builds-on :type technology)))

(def-class uk-location ())

(def-class uk-county (uk-location)
  ((has-name :type string)
   (has-alternative-name :type string)
   (has-town :type uk-town)))

(def-class uk-town (uk-location)
  ((has-name :type string)
   (has-county :type uk-county)))

(def-class organization (temporal-thing)
  ((has-full-name :type string)
   (has-web-address :type URL)
   (has-address :type (or postal-address uk-address)) 
   (affiliated-people :type affiliated-person) 
   (organization-part-of :type organization)
   (has-organization-unit :type organization-unit)
   (headed-by :type affiliated-person)
   (has-organization-size :type organization-size-type)
   (in-economic-sector :type economic-sector-type)))

(def-class affiliated-person (person)
  ((has-affiliation :type organization :min-cardinality 1)))


(def-class postal-address ()
  ((address-street :type string)
   (address-area :type local-district)
   (address-number :type integer)
   (address-building :type string)
   (address-city-or-village :type municipal-unit)
   (address-postcode :type string)
   (address-region :type geographical-region)
   (address-country :type country)))

(def-class uk-address (postal-address)
  ((address-country :value united-kingdom)
   (address-county :type uk-county)
   (address-city-or-village :type uk-town)))

(def-class human-settlement ())

(def-class geo-political-region (human-settlement )
  ((has-name :type string)))




(def-class location ()
  ((has-name :type string)))


(def-class geographical-region (location ))

(def-class Geopolitical-Entity (Geographical-Region Generic-Agent))


(def-class city (geopolitical-entity)
  ((located-in-country :type country)))

(def-class village (geopolitical-entity))

(def-class local-district (geopolitical-entity))



(def-class country (Geopolitical-Entity)
  ((has-capital :type capital-city)))
  


(def-class document ()
  ((has-author :type person)))

(def-class lecture ()
  ((has-title :type string)
   (has-author :type person)))

(def-class demonstration ()
  ((has-title :type string)
   (has-author :type person)
   (thing-demoed)))

(def-class degree ())

(def-class academic-degree (degree))

(def-instance phd academic-degree)

(def-instance msc academic-degree)

(def-instance ba academic-degree)


;;;;;;;;;;
(def-class communication-technology ())

(def-class communication-medium (communication-technology))


(def-class computing-technology (technology)
  ())

(def-class hardware-technology (computing-technology))

(def-class hardware-platform (hardware-technology))

(def-class software-technology (computing-technology)
  ( 
   (hardware-platforms :type hardware-platform) 
   (runs-on-operating-system :type operating-system) 
   (software-requirements :type software-technology)
   (status :type software-status 
           :documentation "Whether the software is finished, alpha or beta")))

(def-class internet-technology (software-technology hardware-technology))



(def-class web-technology (internet-technology )
  ((technology-builds-on :value web)))

(def-instance web internet-technology)

;;;;;;;;;;;;;;;;;;;;


(def-class publishing-medium (communication-medium))
(def-class electronic-publishing-medium (publishing-medium ))
(def-class paper-based-publishing-medium (publishing-medium ))



(def-class news ()
  "News is a collection of news-item"
  ((has-news-items :type news-item
                   :min-cardinality 1)))

(def-relation has-contents (?x ?c)
  :constraint (and (news-item ?X)
                   (string ?c)))


(def-class news-item (temporal-thing)
  ((has-author :type person)
   (has-headline :type string)
   (has-contents :type string)
   (expressed-in-medium :type publishing-medium)
   (published-date :type calendar-date)
   (relates-events :min-cardinality 1 :type event))
  :slot-renaming ((published-date start-time)))
