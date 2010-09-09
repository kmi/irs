;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology aktive-portal-ontology2)

;;;A simple, initial ontology for events.  We use some of Roger Schank's categories
;;;to give some kind of top-level structuring

(def-class EVENT (temporal-thing) ?x
  "This is a minimalist definition of class event.  We start with the very basic
   and we will then add slots as we specialise this definition for specific classes of events.
   The fillers of slots has-other-agents-involved and has-main-agent should not 
   intersect"
  ((has-main-agent 
        :type generic-agent 
	:documentation "The agents causing the event to happen, if they are known.")
   (has-other-agents-involved 
   	:type generic-agent
	:documentation "Other agents involved in the event")
   (has-sub-event :type event)
   (has-location 
	:type location
	:documentation "The location at which an event takes place"))
  :constraint (not (exists ?a
                           (and (has-main-agent ?x ?a)
                                (has-other-agents-involved ?x ?a)))))


(def-class EVENT-INVOLVING-PRODUCTION (event)
  "When something is produced"
  ((Event-product :type individual)))

(def-class EVENT-INVOLVING-MOVEMENT (event)
  "This is an event in which the main agent (plus maybe others)
   goes from some place to another"
  ((Location-at-start :type location :max-cardinality 1)
   (Location-at-end :type location :max-cardinality 1)
   (means-of-transport-used :type generalised-means-of-transport))
 :slot-renaming ((location-at-start has-location)))

(def-class GENERALIZED-TRANSFER (event-involving-movement)
  "An event in which the main agent transfers something (the thing-acted-on, tangible or intangible) 
   to one or more recipient agents.
   Note that we do not say anything about whether the original agent still retain
   the thing-acted-on.  In some cases this is clearly true ('I pass my wisdom on to my daughter'),
   in other cases it is not (I give you my wallet)."

   ((thing-acted-on :type individual 
                     :documentation "What is being transferred")
    (recipient-agents :type generic-agent 
                      :documentation "The agents which receive the thing-acted-on"))
   :slot-renaming ((recipient-agents has-other-agents-involved)))


(def-class INFORMATION-TRANSFER-EVENT (generalized-transfer)
  "A generalized transfer in which information is passed from main agent to one or more
   recipient agents.  Examples include giving a tutorial."
  ((sender-of-information :type generic-agent)
   (information-being-transferred  :type abstract-information)
   (information-object-being-transferred  :type information-bearing-object)
   (information-transfer-medium-used :type information-transfer-medium))
  :slot-renaming ((sender-of-information has-main-agent)
                  (information-being-transferred thing-acted-on)
                  (information-transfer-medium-used means-of-transport-used)))

 

(def-class GENERALISED-MEANS-OF-TRANSPORT (thing) 
  "This is a generic class to catch all sorts of borderline and metaphorical 
   ways to carry things from A to B")

(def-class TRANSPORTATION-DEVICE (generalised-means-of-transport tangible-thing)
  "Something tangible designed to transport people, animals, objects from A to B. 
   For instance a bycicle, a car, a boat, etc.")

(def-class INFORMATION-TRANSFER-MEDIUM (generalised-means-of-transport))

(def-instance EMAIL-MEDIUM INFORMATION-TRANSFER-MEDIUM)

(def-class MESSAGE (information-bearing-object)
  ((sender-of-message :type generic-agent)
   (recipient-of-message :type generic-agent)
   (time-of-message :type time-point)))

(def-class LETTER (message))

(def-class EMAIL-MESSAGE (message))

(def-class SENDING-AN-EMAIL (information-transfer-event )
  "a generalized transfer in which information is passed from main agent to one or more
   recipient agents.  examples include giving a tutorial."
  ((sender-of-information :type generic-agent)
   (information-object-being-transferred  :type email-message)
   (information-transfer-medium-used :value email-medium )))
  

(def-class SOCIAL-GATHERING (event))


(def-class MEETING-TAKING-PLACE (social-gathering)
  "A meeting type of event. Note that both attendee and organizer have
   multiple cardinality"
  ((meeting-attendee :type person)
   (meeting-organizer :type person)
   )
  :slot-renaming ((meeting-organizer has-main-agent)
                  (meeting-attendee  has-other-agents-involved)))



(def-class PUBLICATION-TYPE-EVENT (event-involving-production)
  ((event-product :type publication)
   ))

(def-class CONFERENCE (meeting-taking-place publication-type-event)
  ((published-proceedings :type conference-Proceedings-Reference))
  :slot-renaming ((published-proceedings event-product)))

(def-class WORKSHOP (meeting-taking-place publication-type-event)
  ((published-proceedings :type workshop-Proceedings-Reference))
  :slot-renaming ((published-proceedings event-product)))

(def-class BOOK-PUBLISHING (publication-type-event)
  ((published-book :type book))
 :slot-renaming ((published-book event-product)))

(def-class CONFERRING-AN-AWARD (generalized-transfer )
  ((has-awarding-body :type awarding-body)
   (has-award-rationale :type string)
   (confers-award :type award))
  :slot-renaming ((has-awarding-body has-main-agent)
                  (confers-award thing-acted-on)))

(def-class AWARDING-BODY (legal-agent)
  "Legal agents can be either organizations or people.
   An awarding body is normally an organization, an individual, or a bunch of people")


(def-class AWARD (Intangible-thing)
  "An award is an intangible thing, even if the 
   piece of paper which is often associated with an award is 
   tangible.  What about the virtual piece of paper in the 
   virtual degree ceremony?  I guess that ought to be an intangible")

(def-class FINANCIAL-AWARD (Award)
  ((has-amount :type amount-of-money)))

(def-class DEGREE (award)
  "A degree is type of award"
)


(def-class ACADEMIC-DEGREE (degree))

(def-instance PHD academic-degree)

(def-instance DENG academic-degree)

(def-instance MSC academic-degree)

(def-instance MA academic-degree)

(def-instance BA academic-degree)

(def-instance BSC academic-degree)




(def-class GIVING-A-TALK (information-transfer-event)
  ((has-speaker :type person)
   )
  :slot-renaming ((has-speaker sender-of-information)))

(def-class ATTENDING-AN-EVENT (event)
  ((event-attended :type event)))



(def-class ATTENDING-A-CONFERENCE (attending-an-event)
  ((event-attended :type conference)))





  



