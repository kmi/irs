


;;;This file lists the changes from version 1 to version 2 of the AKT reference Ontology
;;;When a new definition replaces an existing one both definitions are given,
;;;the second one being the new one.  
;;;;;;;;;;;;;;


(def-class THING ()
  "This is the top-level concept in the AKT reference ontology"
  )


(def-class THING ()
  "This is the top-level concept in the AKT reference ontology"
  ((has-pretty-name :type string
                    :max-cardinality 1
                    :documentation "A human readable name")
   (has-variant-name :type string
                     :documentation "Other variants of the human readable name")))


;;;;;;;;;;;;;;
(def-class AWARDING-BODY (legal-agent)
  "Typically this is an individual or an organization")


(def-class AWARDING-BODY (legal-agent)
  "Legal agents can be either organizations or people.
   An awarding body is normally an organization, an individual, or a bunch of people")


;;;;;;;;;;;;;;

(def-class FINANCIAL-AWARD (Intangible-thing)
  "An award is an intangible thing, even if the 
   piece of paper which is often associated with an award is 
   tangible.  What about the virtual piece of paper in the 
   virtual degree ceremony?  I guess that ought to be an intangible"
  ((has-amount :type amount-of-money)))


(def-class FINANCIAL-AWARD (Award)
  ((has-amount :type amount-of-money)))

;;;;;;;;;;;;;;
(def-class GENERIC-AGENT (temporal-thing)
  "This is a generic notion, an agent can be an organization, a person
   an animal, a software agent, etc"
  )


(def-class GENERIC-AGENT (temporal-thing)
  "This is a generic notion, an agent can be an organization, a person
   an animal, a software agent, etc"
  ((has-web-address :type URL)
   (has-email-address :type email-address)))

;;;;;;;;;;;;;;
(def-class LEGAL-AGENT (generic-agent)
  "Some agents have legal status: definitely organizations and people,
   anybody else?")


(def-class LEGAL-AGENT (generic-agent)
  "Some agents have legal status: definitely organizations and people,
   anybody else?"
  ((has-telephone-number :type string)
   (has-fax-number :type string)
   (has-postal-address :type postal-address)))
   

;;;;;;;;;;;;;;
(def-class ORGANIZATION (legal-agent)
  "An organization is a type of legal agent"
  ((has-web-address :type URL)
   (full-name :type string)
   (has-address :type postal-address) 
   (affiliated-people :type affiliated-person) 
   (organization-part-of :type organization)
   (has-sub-unit :type organization-unit)
   (headed-by :type affiliated-person)
   (has-size :cardinality 1 :type organization-size)
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

;;;;;;;;;;;;;;

(def-class POLITICAL-ORGANIZATION (organization)
  "An organization which has a political connotation")

(def-instance NATO political-organization
  "nato is both a political organization and a geopolitical entity.
   Actually we should specify its size, because slot has-size has min-cardinality 1....")

(def-instance NATO geopolitical-entity
  "nato is both a political organization and a geopolitical entity"))

;;;;;;;;;;;;;;
(def-class ORGANIZATION-UNIT (generic-agent)
  "An organization may have a number of units. Units may themselves have sub-units"
  ((has-web-address :type URL)
   (full-name :type string)
   (has-address :type postal-address) 
   (has-size :type  organization-size)
   (affiliated-people :type affiliated-person) 
   (unit-of-organization :type organization)
   (sub-unit-of-organization-unit :type organization-unit)
   (has-sub-unit :type organization-unit)
   (headed-by :type affiliated-person)))

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

;;;;;;;;;;;;;;
(def-class PERSON (legal-agent)
  ((full-name :type string)
   (Family-Name :type string)
   (Given-Name :type string)
   (has-gender :type gender)
   (has-telephone-number :type string)
   (has-postal-address :type postal-address)
   (has-web-address :type URL)
   (has-email-address :type email-address)
   (has-appellation :type appellation)))


;;;The slot has-telephone-number is now inherited from legal agent
(def-class PERSON (legal-agent tangible-thing)
  ((full-name :type string)
   (family-name :type string)
   (given-name :type string)
   (has-gender :type gender)
   (has-academic-degree :type academic-degree)
   (has-appellation :type appellation))
  :slot-renaming ((full-name has-pretty-name)
                  ))

;;;;;;;;;;;;;;
(def-class TOWN (municipal-unit))

;;;;;;;;;;;;;;
(def-class GEOPOLITICAL-ENTITY (Geographical-Region Generic-Agent)
  "For instance, Russia, Italy, The-city-of-Messina, etc..")


(def-class GEOPOLITICAL-ENTITY (Geographical-Region Generic-Agent)
  "A geopolitical entity is a geographical area which is associated with some sort of 
   political structure. For instance, Russia, Italy, The-city-of-Messina, etc..
   A geopolitical entity can be also seen as an agent - e.g., France declared 
   war to Spain")

;;;;;;;;;;;;;;

(def-class LOCATION (thing)
  "A generic class for locations.  It includes both real and 
   fantastic places")



(def-class LOCATION (tangible-thing)
  "A generic class for locations.  It includes both real and 
   fantastic places")

;;;;;;;;;;;;;;
(def-class ADDRESS (thing)
  "A generic class for addresses, whether email or postal.
   Is an address a tangible thing or an intangible thing?")


(def-class ADDRESS (abstract-information)
  "A generic class for addresses, whether email or postal.
   We see an address as abstract information and therefore it is an intangible thing")

;;;;;;;;;;;;;;
(def-class TANGIBLE-THING (thing)
  "Something which is not intangible")


(def-class TANGIBLE-THING (temporal-thing)
  "Something which is not intangible, something which is physical, made of matter.
   It does not matter whether things are real of imaginary.  Therefore we consider
   Mickey Mouse's car and a hippogriff as  tangible things")


;;;;;;;;;;;;;;

(def-class INFORMATION-BEARING-OBJECT (tangible-thing temporal-thing)
  "This notion comes from Cyc.  It is useful to group together all
   information bearing entities, including video, audio and documents.
   An information bearing object may have an author (a generic agent)
   and may be owned by a legal agent. It is a tangible object"
  ((has-author :type generic-agent)
   (owned-by :type legal-agent)))


(def-class INFORMATION-BEARING-OBJECT (tangible-thing)
  "This notion comes from Cyc.  It is useful to group together all
   information bearing entities, including video, audio and documents.
   An information bearing object may have an author (a generic agent)
   and may be owned by a legal agent. It is a tangible object"
  ((has-author :type generic-agent)
   (owned-by :type legal-agent)))



;;;;;;;;;;;;;;
(def-class PUBLICATION-REFERENCE (thing)
  "Not sure whether a reference is a tangible or intangible.
   Open to suggestions"
  ((has-title :type string)
   (has-author :type generic-agent)
   (has-date :type calendar-date)
   (has-place-of-publication :type location)))


(def-class PUBLICATION-REFERENCE (abstract-information)
  "we have decided that a publication reference is an intangible, abstract information"
  ((has-title :type string)
   (has-author :type generic-agent)
   (has-date :type calendar-date)
   (has-place-of-publication :type location)))


;;;;;;;;;;;;;;
(def-class PUBLICATION (Information-Bearing-Object)
  "A publication is something which has one or more publication references.
   A publication can be both an article in a journal or a journal itself"
  ((has-publication-reference :min-cardinality 1
                              :type publication-reference)
   ))

(def-class PUBLICATION (Information-Bearing-Object)
  "A publication is something which has one or more publication references.
   A publication can be both an article in a journal or a journal itself.
   The distinction between publication and publication-reference makes it possible
   to distinguish between multiple occurrences of the sam publication, for instance in
   different media"
  ((has-publication-reference :min-cardinality 1
                              :type publication-reference)
   (cites-publication-reference :type publication-reference)))

;;;;;;;;;;;;;;


(def-class COMPOSITE-PUBLICATION (publication)
  "A publication which contains items which cane be themselves referenced through a 
   publication reference.  Composite publications include newspapers, magazines and journals.
   A book which is a collection of articles is a composite publication, a monograph is not"
  ((contains-publication :min-cardinality 1
                         :type publication)
   ))

;;;;;;;;;;;;;;

(def-class SERIAL-PUBLICATION (Publication) ?x
  "This used to be called periodical publication.  However, many periodicals
do not appear at fixed intervals, which is why librarians refer to them as serials.
So, we now use the concept of serial publication and the has-periodicity slot has been 
removed"
  )

;;;;;;;;;;;;;;
(def-class PERIODICAL-PUBLICATION (publication) ?x 
  "This comes from the ontolingua library.
   A periodical-publication is published regularly, such as once
   every week.  Strictly speaking, the noun 'periodical' is used
   by librarians to refer to things published at intervals of greater
   than a day.  We use the phase periodical-publication to include
   newspapers and other daily publications, since they share many
   bibliographic features."
  ((has-periodicity :cardinality 1 :type time-interval)))


(def-class PERIODICAL-PUBLICATION (serial-publication) ?x 
  "This comes from the ontolingua library.
   A periodical-publication is published regularly, such as once
   every week.  Strictly speaking, the noun 'periodical' is used
   by librarians to refer to things published at intervals of greater
   than a day.  We use the phase periodical-publication to include
   newspapers and other daily publications, since they share many
   bibliographic features.
   The periodicity indicates how often the publication comes out. Note that this is
   a duration, rather than a time interval. A time interval indicates a specific time interval
   on the time continuum, so we need to model periodicity as a time quantity"
  ((has-periodicity :cardinality 1 :type duration)))
;;;;;;;;;;;;;;

(def-class ARTICLE-IN-A-COMPOSITE-PUBLICATION (publication)
  ((included-in-publication :type composite-publication)))

(def-axiom CONSISTENCY-BETWEEN-COMPOSITE-PUBLICATIONS-AND-THEIR-CONTENTS
  (<=> (included-in-publication ?a ?p)
       (contains-publication ?p ?a)))
  
;;;;;;;;;;;;;;

(def-class JOURNAL (Periodical-Publication))


(def-class JOURNAL (serial-publication composite-publication)
   ((contains-article :type publication))
   :slot-renaming ((contains-article contains-publication)
                  ))
;;;;;;;;;;;;;;
(def-class MAGAZINE (Periodical-Publication))

(def-class MAGAZINE (serial-publication composite-publication)
  ((contains-article :type publication))
  :slot-renaming ((contains-article contains-publication)
                  ))
;;;;;;;;;;;;;;
(def-class NEWSPAPER (Periodical-Publication))

(def-class NEWSPAPER (periodical-publication composite-publication)
  ((contains-news-item :type news-item))
  :slot-renaming ((contains-news-item contains-publication)
                  ))
;;;;;;;;;;;;;;
(def-class DAILY-NEWSPAPER (newspaper)
  ((has-periodicity :value 24-hour-duration)))

(def-class NEWS-ITEM (article-in-a-composite-publication))

;;;;;;;;;;;;;;
;;The axiom MUTUALLY-EXCLUSIVE-SERIAL-PUBLICATIONS replaces MUTUALLY-EXCLUSIVE-PERIODICAL-PUBLICATIONS
(def-axiom MUTUALLY-EXCLUSIVE-PERIODICAL-PUBLICATIONS
  (subclass-partition  Periodical-Publication 
                                  (Setof Journal Magazine Newspaper)))


(def-axiom MUTUALLY-EXCLUSIVE-SERIAL-PUBLICATIONS
  (subclass-partition  Serial-Publication 
                                  (Setof Journal Magazine Newspaper)))

;;;;;;;;;;;;;;
(def-class EMPLOYEE (affiliated-person working-person) ?x
  ((works-for :type organization)
   (works-in-unit :type organization-unit)
   (has-job-title :type string)
   )
  :slot-renaming ((works-for has-affiliation))
  :constraint (forall (?u ?o)
                      (=> (and  (works-in-unit ?x ?u)
                                (works-for ?x ?o))
                          (unit-of-organization ?u ?o))))

(def-class EMPLOYEE (affiliated-person working-person) ?x
  ((works-for :type organization)
   (works-in-unit :type organization-unit)
   (has-job-title :type string)
   (has-contract-type :type employment-contract-type)
   )
  :slot-renaming ((works-for has-affiliation)
                  (works-in-unit has-affiliation-to-unit)))

;;;;;;;;;;;;;;

(def-class EMPLOYMENT-CONTRACT-TYPE (Intangible-thing))


(def-instance PERMANENT-CONTRACT employment-contract-type)

(def-instance TEMPORARY-CONTRACT  employment-contract-type)

;;;;;;;;;;;;;;


;;;We cluster a bunch of related definitions here;;;;

;;;OLD
(def-class ORGANIZATION-SIZE (intangible-thing))

(def-instance VERY-LARGE-SIZE organization-size)

(def-instance LARGE-SIZE organization-size)

(def-instance MEDIUM-SIZE organization-size)

(def-instance SMALL-SIZE organization-size)

(def-instance MICRO-SIZE organization-size)

(def-class SMALL-OR-MEDIUM-SIZED-ORGANIZATION (organization) ?x
  "SME are important, so we define a class to represent them explicitly.
   In some case we might not know or we do not want to bother specifying 
   excatly whether something is a small-organization or a medium-organization.
   Hence, we can just say 'x is a SME' without going into further detail"
  :iff-def (and (organization ?x)
                (has-size ?x  ?size)
                (member ?size '(micro-size small-size medium-size)))
   :avoid-infinite-loop t)
                  

;;;NEW

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
                  
;;;;;;;;;;;;;;
;;;time-entity now replaced by the notion of time position
(def-class TIME-ENTITY (intangible-thing)
  "WARNING: The constraint below ought to be extended to handle leap years.
   We use this generic notion of time entity which subsumes both time intervals
   and time points"
  ((minute-of :type minute-in-time :max-cardinality 1 )
   (second-of :type second-in-time :max-cardinality 1 )
   (hour-of :type hour-in-time :max-cardinality 1 )
   (day-of :type day-in-time :max-cardinality 1)
   (month-of :type month-in-time :max-cardinality 1)
   (year-of :type year-in-time :max-cardinality 1 ))
  :constraint  (and (not (and (month-of ?x 2)
                             (> (the ?day (day-of ?x ?day))
                                29)))
                    (not (and (member-of ?x (4 6 9 11))
                              (> (the ?day (day-of ?x ?day))
                                30)))))



(def-class TIME-POSITION (intangible-thing)
  "A time position is either a time interval or a time point.
   Any time position is relative to a time zone"
  ((in-timezone :default-value "+00:00" :type timezone)))

;;;;;;;;;;;;;;

(def-class TIMEZONE (string)
  "We represent a time zone as a string with the format 
   {-/+}hh:mm ")
  
;;;;;;;;;;;;;; 
  
(def-class TIME-POINT (time-entity)
  "A point in time")

(def-class TIME-POINT (time-position)
  "A point in time"
  ((second-of :type second-in-time :max-cardinality 1 )
   (minute-of :type minute-in-time :max-cardinality 1 )
   (hour-of :type hour-in-time :max-cardinality 1 )
   (day-of :type day-in-time :max-cardinality 1)
   (month-of :type month-in-time :max-cardinality 1)
   (year-of :type year-in-time :max-cardinality 1 ))
  :constraint  (and (not (and (month-of ?x 2)
                             (> (the ?day (day-of ?x ?day))
                                29)))
                    (not (and (member-of ?x (4 6 9 11))
                              (> (the ?day (day-of ?x ?day))
                                30)))))
;;;;;;;;;;;;;; 
(def-class CALENDAR-DATE (time-point)
 "A calendar date is a time point in which month, day and year have 
  been specified"
  ((day-of :type day-in-time :cardinality 1)
   (month-of :type month-in-time :cardinality 1)
   (year-of :type year-in-time :cardinality 1)))

(def-class CALENDAR-DATE (time-point)
 "A calendar date is a time point in which month, day and year have 
  been specified but hour, minute and second have not"
  ((minute-of :type minute-in-time :max-cardinality 0 )
   (second-of :type second-in-time :max-cardinality 0 )
   (hour-of :type hour-in-time :max-cardinality 0 )
   (day-of :type day-in-time :cardinality 1)
   (month-of :type month-in-time :cardinality 1)
   (year-of :type year-in-time :cardinality 1)))

;;;;;;;;;;;;;; 
(def-class TIME-INTERVAL (time-entity)
  "An interval of time - e.g., 6 hours, 5 days, or '6 hours and 5 minutes'.  
   We use the same structure as a time point to express intervals")

(def-class TIME-INTERVAL (time-position)
  "An interval is defined by two time points or a duration.  
   Classes of intervals, e.g., a day, can be defined by specifying only
   a duration.  A time interval has no gaps"

  ((begins-at-time-point :type time-point :max-cardinality 1)
   (ends-at-time-point :type time-point :max-cardinality 1)
   (has-duration :type duration :max-cardinality 1)))

;;;;;;;;;;;;;; 

;;;Axiom DURATION-IS-START-TIME-MINUS-END-TIME replaced by DURATION-IS-BEGIN-TIME-MINUS-END-TIME
(def-axiom DURATION-IS-START-TIME-MINUS-END-TIME
  "This axiom states the relation between duration, start time 
   and end time in a temporal thing"
  (=> (temporal-thing ?x)
      (= (the ?duration (has-duration ?x ?duration))
         (time-difference (the ?et (has-end-time ?x ?et))
                          (the ?st (has-start-time ?x ?st))))))


(def-axiom DURATION-IS-BEGIN-TIME-MINUS-END-TIME
  "This axiom states the relation between duration, begin time 
   and end time in an interval"
  (=> (and (time-interval ?x)
           (begins-at-time-point ?x ?tp1)
           (ends-at-time-point ?x ?tp2))
      (= (has-duration ?x (time-difference 
                           (the ?tp1 (begins-at-time-point ?x ?tp1))
                           (the ?tp2 (ends-at-time-point ?x ?tp2)))))))
;;;;;;;;;;;;;; 

;;;A bunch of new definitions

(def-class DAY (time-interval)
  ((has-duration :value 24-hour-duration)))

(def-class WEEK (time-interval)
  ((has-duration :value 7-day-duration)))

(def-class MONTH (time-interval))

(def-class JANUARY (month)
  ((has-duration :value 31-day-duration)))

(def-class FEBRUARY (month)
  ((has-duration :default-value 28-day-duration)))

(def-class FEBRUARY-IN-LEAP-YEARS (february)
  ((has-duration :value 29-day-duration))

(def-class MARCH (month)
  ((has-duration :value 31-day-duration)))

(def-class APRIL (month)
  ((has-duration :value 30-day-duration)))

(def-class MAY (month)
  ((has-duration :value 31-day-duration)))

(def-class JUNE (month)
  ((has-duration :value 30-day-duration)))

(def-class JULY (month)
  ((has-duration :value 31-day-duration)))

(def-class AUGUST (month)
  ((has-duration :value 31-day-duration)))

(def-class SEPTEMBER (month)
  ((has-duration :value 30-day-duration)))

(def-class OCTOBER (month)
  ((has-duration :value 31-day-duration)))

(def-class NOVEMBER (month)
  ((has-duration :value 30-day-duration)))

(def-class DECEMBER (month)
  ((has-duration :value 31-day-duration)))

(def-class YEAR (time-interval)
  ((has-duration :value 12-month-duration)))

;;;;;;;;;;;;;; 

;;;Another bunch of new definitions

(def-class DURATION (physical-quantity)
  "A measure of time, e.g., 5 hours"
  ((has-unit-of-measure :type time-measure)
   ))

(def-instance 24-HOUR-DURATION duration
  ((has-unit-of-measure time-measure-hour)
   (has-magnitude 24)))

(def-instance 7-DAY-DURATION duration
  ((has-unit-of-measure time-measure-day)
   (has-magnitude 7)))

(def-instance 28-DAY-DURATION duration
  ((has-unit-of-measure time-measure-day)
   (has-magnitude 28)))

(def-instance 29-DAY-DURATION duration
  ((has-unit-of-measure time-measure-day)
   (has-magnitude 29)))

(def-instance 30-DAY-DURATION duration
  ((has-unit-of-measure time-measure-day)
   (has-magnitude 30)))

(def-instance 31-DAY-DURATION duration
  ((has-unit-of-measure time-measure-day)
   (has-magnitude 31)))

(def-instance 12-MONTH-DURATION duration
  ((has-unit-of-measure time-measure-year)
   (has-magnitude 12)))
  
;;;;;;;;;;;;;; 

  
(def-class TIME-MEASURE (unit-of-measure)
  "The class of all unit of measures used to measure time,
   e.g., minute, second, hour, etc...")

(def-instance TIME-MEASURE-SECOND time-measure)

(def-instance TIME-MEASURE-MINUTE time-measure)

(def-instance TIME-MEASURE-HOUR time-measure)

(def-instance TIME-MEASURE-DAY time-measure)

(def-instance TIME-MEASURE-MONTH time-measure)

(def-instance TIME-MEASURE-YEAR time-measure)

(def-instance TIME-MEASURE-CENTURY time-measure)

;;;;;;;;;;;;;; 
(def-class TEMPORAL-THING (thing)
  "Like in Cyc, this is something which has a temporal extent."
  ((has-duration :type time-interval)
   (has-start-time :type time-point)
   (has-end-time :type time-point)))

(def-class TEMPORAL-THING (thing)
  "Like in Cyc, this is something which has a temporal extent."
  ((has-time-interval :type time-interval)))

;;;;;;;;;;;;;; 

(def-function TIME-DIFFERENCE (?te1 ?te2) -> ?ti
  "The interval between two time entities."
  :def (and (time-entity ?te1)
            (time-entity ?te2)
            (time-interval ?ti)))

(def-function TIME-DIFFERENCE (?tp1 ?tp2) -> ?d
  "The duration between two time points.
   No operational definition is given here, only a spec"
  :def (and (time-point ?tp1)
            (time-point ?tp2)
            (duration ?d)))


;;;;;;;;;;;;;;
(def-class QUANTITY (Intangible-thing)
  ((has-unit-of-measure :type unit-of-measure)
   (has-magnitude :type number)))

(def-class QUANTITY (Individual Intangible-thing)
  "From SUO: Any specification of how many or how much of something there is. 
   Accordingly, there are two subclasses of Quantity: Number (how many) 
   and Physical-Quantity (how much).")

;;;;;;;;;;;;;;
(def-class NUMBER (individual Intangible-thing)
  "The class of all numbers"
  :lisp-fun  #'(lambda (x env)
                 (let ((y (unbound-variable? x env)))
                   (if y ;;if y is unbound we return a 'sample' number
                     (list (cons (cons y 0) env))
                     (if (numberp (instantiate x env)) ;;;make sure to instantiate x
                       (list env)
                       :fail)))))


(def-class NUMBER (Quantity)
  "The class of all numbers"
  :lisp-fun  #'(lambda (x env)
                 (let ((y (unbound-variable? x env)))
                   (if y ;;if y is unbound we return a 'sample' number
                     (list (cons (cons y 0) env))
                     (if (numberp (instantiate x env)) ;;;make sure to instantiate x
                       (list env)
                       :fail)))))
;;;;;;;;;;;;;;

(def-class PHYSICAL-QUANTITY (quantity)
 "SUO: Physical Quantities are distinguished from Numbers by the
  fact that the former are associated with a dimension of measurement."
  ((has-unit-of-measure :type unit-of-measure)
   (has-magnitude :type number)))

;;;;;;;;;;;;;;

(def-class AMOUNT-OF-MONEY (Quantity)
  ((has-unit-of-measure :type currency)
   (has-amount :type number))
  :slot-renaming ((has-amount has-magnitude)))

(def-class AMOUNT-OF-MONEY (Physical-Quantity)
  ((has-unit-of-measure :type currency)
   (has-amount :type number))
  :slot-renaming ((has-amount has-magnitude)))


;;;;;;;;;;;;;;
(def-class TECHNOLOGY (thing) 
  "By technology we mean engineered applications of science.
   I guess we are probably confining ourselves to tangible things, but 
   as I am not sure I will use thing as the direct superclass"
  ((has-author :type person)
   (owned-by :type organization)
   (technology-builds-on :type technology)
   (supports-method :type method)
   (addresses-generic-area-of-interest :type generic-area-of-interest)))


(def-class TECHNOLOGY (thing) 
  "By technology we mean engineered applications of science.
   I guess we are probably confining ourselves to tangible things
   but as I am not sure I will use thing as the direct superclass - 
   e.g., an algorithm is an intangible thing, but it could be seen as 
   a technology, if we give a broad interpretation of the term"
  ((has-author :type generic-agent)
   (owned-by :type legal-agent)
   (technology-builds-on :type technology)
   (supports-method :type method)
   (addresses-generic-area-of-interest :type generic-area-of-interest)))

;;;;;;;;;;;;;;
(def-class MEETING-TAKING-PLACE (social-gathering)
  ((meeting-attendees :type person)
   (meeting-organizer :type person)
   )
  :slot-renaming ((meeting-organizer has-main-agent)
                  (meeting-attendees  has-other-agents-involved)))


(def-class MEETING-TAKING-PLACE (social-gathering)
  "A meeting type of event. Note that both attendee and organizer have
   multiple cardinality"
  ((meeting-attendee :type person)
   (meeting-organizer :type person)
   )
  :slot-renaming ((meeting-organizer has-main-agent)
                  (meeting-attendee  has-other-agents-involved)))

;;;;;;;;;;;;;;
(def-class POSTAL-ADDRESS (address)
 ((address-street :type string)
   (address-area :type local-district)
   (address-number :type integer)
   (address-building :type string)
   (address-city-or-village :type municipal-unit)
   (address-postcode :type string)
   (address-region :type geographical-region)
   (address-country :type country)))


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

;;;;;;;;;;;;;;

;;;Now we provide a global definition of relation HAS-RESEARCH-INTEREST

(def-relation HAS-RESEARCH-INTEREST (?x ?r)
  "People and in general even organizations and organization units
   may have research interests.  This relation shoudl be used to 
   specify them"
  :constraint (and (or (organization-unit ?x)
                       (organization ?x)
                       (person ?x))
                   (research-area ?r)))

;;;;;;;;;;;;;;

(def-class AFFILIATED-PERSON (person)
  ((has-affiliation-to-unit :type organization-unit)
   (has-affiliation :type organization :min-cardinality 1)))

(def-class AFFILIATED-PERSON (person)
  "A person which has an affiliation with some organization.
   For instance employees are affiliated to the organization they work for,
   students to the institution where they are studying, etc..
   A person can have multiple affiliations, which means that there is no 
   constraint relating the values of slot has-affiliation-to-unit to the values 
   of slot has-affiliation"
  ((has-affiliation-to-unit :type organization-unit)
   (has-affiliation :type organization :min-cardinality 1)))

;;;;;;;;;;;;;;

(def-class STUDENT (affiliated-person)
  ((has-affiliation :type educational-organization :min-cardinality 1)))


(def-class STUDENT (affiliated-person)
  ((studies-at :type educational-organization :min-cardinality 1)
   (studies-in-unit :type educational-organization-unit :min-cardinality 1))
   :slot-renaming ((studies-at has-affiliation)
                   (studies-in-unit has-affiliation-to-unit)))
                 
;;;;;;;;;;;;;;
(def-class EDUCATIONAL-ORGANIZATION-UNIT (organization-unit)
  ((unit-of-organization :type educational-organization)
   ))

;;;;;;;;;;;;;;
(def-class ACADEMIC-UNIT (organization-unit)
  ((unit-of-organization :type university)
   (has-size :default-value small-size 
             :documentation "By default academic units are small organizations")))

(def-class ACADEMIC-UNIT (educational-organization-unit)
  ((unit-of-organization :type university)
   ))

;;;;;;;;;;;;;;
(def-class ACADEMIC-SUPPORT-UNIT (organization-unit))

(def-class ACADEMIC-SUPPORT-UNIT (educational-organization-unit))
;;;;;;;;;;;;;;

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
   
   
;;;;;;;;;;;;;;

(def-class REAL-NUMBER (number))

;;;;;;;;;;;;;;
(def-class INTEGER (number)
   "The class of all integers"
   :lisp-fun  #'(lambda (x env)
                 (let ((y (unbound-variable? x env)))
                   (if y ;;if y is unbound we return a 'sample' integer
                     (list (cons (cons y 0) env))
                     (if (integerp (instantiate x env)) ;;;make sure to instantiate x
                       (list env)
                       :fail)))))

(def-class INTEGER (real-number))

;;;;;;;;;;;;;;
(def-class SECOND-IN-TIME (non-negative-integer)?x
	"A second-in-time is an integer in the interval 0-59"
	:iff-def (and (non-negative-integer ?x)(< ?x 60) )
        :avoid-infinite-loop t)


(def-class SECOND-IN-TIME (real-number)?x
  "A second-in-time is a real number greater or equal to 0, less than 60"
  :iff-def (and (real-number ?x)(not (< ?x 0))(< ?x 60))
  :avoid-infinite-loop t)



;;;;;;;;;;;;;;

;;;A number of new types of employees

(def-class SYSTEM-ADMINISTRATOR (employee)) 

(def-class MULTIMEDIA-DESIGNER (employee)) 

(def-class GRAPHIC-DESIGNER (employee)) 

(def-class SECRETARY (employee))

(def-class ACADEMIC-SUPPORT-STAFF (educational-employee)
  ((works-for :type higher-educational-organization)
   ))

(def-class PROJECT-OFFICER-IN-ACADEMIA (academic-support-staff)) 

;;;;;;;;;;;;;;

(def-class PHD-STUDENT (student)
  ((has-research-interest :type research-area)
   (studies-at :type higher-educational-organization :min-cardinality 1)
   (has-supervisor :type person)))

;;;;;;;;;;;;;;

(def-instance KNOWLEDGE-MEDIA-INSTITUTE  r&d-institute-within-larger-organization
  ((has-web-address "http://kmi.open.ac.uk")
   (has-size small-size)
   (unit-of-organization the-open-university)))

(def-instance KNOWLEDGE-MEDIA-INSTITUTE-AT-THE-OPEN-UNIVERSITY  r&d-institute-within-larger-organization
  ((has-pretty-name "Knowledge Media Institute")
   (has-web-address "http://kmi.open.ac.uk")
   (has-telephone-number "+44 1908 653800")
   (has-size medium-size)
   (unit-of-organization the-open-university)))

;;;;;;;;;;;;;;
(def-instance THE-OPEN-UNIVERSITY distance-teaching-university
  ((has-web-address "http://www.open.ac.uk")
   (has-sub-unit knowledge-media-institute)
   (has-size large-size)))


(def-instance THE-OPEN-UNIVERSITY distance-teaching-university
  ((has-web-address "http://www.open.ac.uk")
   (has-sub-unit knowledge-media-institute-at-the-open-university)
   (has-size large-size)))

;;;;;;;;;;;;;;

(def-instance JOHN-DOMINGUE senior-research-fellow-in-academia 
  ((has-research-interest e-commerce-research-area
                          software-visualization
                          knowledge-management
                          organizational-learning)
   (works-for the-open-university)
   (works-in-unit knowledge-media-institute)
   (has-job-title "Deputy Director, Knowledge Media Institute")
   (has-work-status full-time)
   (has-appellation dr)
   (has-email-address "j.b.domingue@open.ac.uk")
   (has-web-address "http://kmi.open.ac.uk/people/john")
   (has-gender  male-gender)
   (full-name "john domingue")))

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

;;;;;;;;;;;;;;

;;;Change to subclass-of definition only concerns OCML implementations.
;;;The definition stays the same, but I have added an efficient implementation
;;;mechanism in OCML. The change won't be seen by those who browse the ontology
;;;in ontolingua
(def-relation SUBCLASS-OF (?sub ?c)
  "?sub is a subclass of ?c if every instance of ?sub is also an instance of 
   ?c.  Note that according to this definition every class is a subclass of itself"

  :constraint (and (class ?sub)(class ?c))
  :iff-def (and (class ?sub) (class ?c)
                (forall ?inst
                        (=> (instance-of ?inst ?sub)
                            (instance-of ?inst ?c)))))


(def-relation SUBCLASS-OF (?sub ?c)
  "?sub is a subclass of ?c if every instance of ?sub is also an instance of 
   ?c.  Note that according to this definition every class is a subclass of itself"

  :constraint (and (class ?sub)(class ?c))
  
  :prove-by (or (and (variable-bound ?sub)
                      (member ?c (all-superclasses ?sub)))
                 (and (variable-bound ?c)
                      (member ?sub (all-subclasses ?c)))
                 (and 
                      (not (variable-bound ?c))
                      (not (variable-bound ?sub))
                      (class ?sub1)(class ?super1)
                      (subclass-of ?sub1 ?super1)))

  :iff-def (and (class ?sub) (class ?c)
                (forall ?inst
                        (=> (instance-of ?inst ?sub)
                            (instance-of ?inst ?c))))
  :no-proofs-by (:iff-def))

;;;;;;;;;;;;;;
;;;Also added the following two utility functions

(def-function ALL-SUPERCLASSES (?class) -> ?supers
  "returns all superclasses of a class"
  :constraint (class ?class)
  :def (forall ?super (<=> (member ?super ?supers)
                           (subclass-of ?class ?super)))
  :lisp-fun  #'(lambda (class)
                 (let ((class-s (get-ocml-class class)))
                   (if class-s
                     (mapcar #'name (domain-superclasses class-s))))))

(def-function ALL-SUBCLASSES (?class) -> ?subs
    "returns all subclasses of a class"
  :constraint (class ?class)
   :def (forall ?sub (<=> (member ?sub ?subs)
                           (subclass-of ?sub ?class)))
   :lisp-fun  #'(lambda (class)
                  (let ((class-s (get-ocml-class class)))
                   (if class-s
                     (mapcar #'name (current-subclasses class-s))))))




                

