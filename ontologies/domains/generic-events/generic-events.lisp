;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology generic-events)


(def-class event (temporal-thing)
  "This is a minimalist definition of class event.  Our previous definition had too many
   slots which were not relevant in a lot of scenarios.  Here we start with the very basic
   and we will then add slots as we specialise this definition for specific classes of events"
  ((main-agent 
        :type generic-agent 
	:documentation "The agents causing the event to happen, if they are known.")
   (other-agents-involved 
   	:type generic-agent
	:documentation "Other agents involved in the event")
  ;;; (instrument-used 
  ;;;  	:documentation "This is the instrument(s) used to carry out the event - e.g. going somewhere by car 
  ;;;  			or breaking an egg with a hammer")
   (has-location 
	:type location
	:documentation "The location at which an event takes place")))

(def-class event-involving-movement (event)
  "This is an event in which the main agent goes from some place to another"
  ((Location-at-start :type location :max-cardinality 1)
   (Location-at-end :type location :max-cardinality 1)
   (means-of-transport-used :type generalised-means-of-transport))
 :slot-renaming (;;;;;(means-of-transport-used instrument-used)
                 (location-at-start has-location)))

(def-class event-involving-production (event)
  ((Event-product)))

(def-class Generalized-Transfer (event-involving-movement)
  "An event in which the main agent transfers something (the Object-Acted-On, tangible or intangible) 
   from one place to another"
   ((Object-Acted-On :documentation "What is being transferred")
    (recipient-agents :documentation "The agents which possibly receive the object-acted-on")))

(def-class generalized-movement (event-involving-movement)
 "This is a generic class for all events in which something goes from
  one place to another.  It is different from generalised-transfer because
  in the latter something is transferred from one place to another.  
  For instance, john goes to work is a generalised movement.  John is moved from
  KMI to Biology is a generalised transfer.  Hence, the difference is conceptual:
  whether or not we want to emphasise that agent A transfers object B from C to D
  (Generalized-Transfer), or simply A goes from B to C (Generalized-movement)"
)

(def-class information-transfer-event (Generalized-Transfer)
  "A generalized transfer in which information is passed from main agent to one or more
   recipient agents.  Examples include giving a tutorial"
  ((information-being-transferred  :type information-stuff)
   (medium-used :type information-transfer-medium))
  :slot-renaming ((information-being-transferred object-acted-on)))

(def-class transfer-of-physical-possession (generalized-transfer)
  "A generalized transfer in which a physical object goes from being in possession of
   main agent to being in possession of a recipient agent"
  ((Object-Acted-On :type tangible-object)))

(def-class transfer-of-abstract-item (generalized-transfer)
  "A generalized transfer in which an abstract object goes is given to the 
   recipient agent - i.e. an award."
  ((Object-Acted-On :type intangible-object)))

(def-class composite-abstract-and-physical-transfer (transfer-of-abstract-item transfer-of-physical-possession )
 "A class for tranfer events which have both an abstract and a physical connotation, such as
  conferring a honour."
  ((object-acted-on :type composite-tangible-and-intangible-object)))

(def-class taking-a-trip (generalized-movement))

(def-class social-gathering (event))

(def-class meeting-taking-place (social-gathering)
  ((meeting-attendees :type person)
   (meeting-organizer :type person)
   ;;;;;(rationale :type temporal-thing)
   )
  :slot-renaming ((meeting-organizer main-agent)
                  (meeting-attendees  OTHER-AGENTS-INVOLVED)))






















