;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology wsmo)

(def-class invokable-entity ()
  "Captures the input and output roles used in the UPML framework. The OCML basic library contains a relations has-input-role and has-output-role which work on classes. "
  ((has-input-role :type role)
   (has-output-role :type role)))

(def-class core-non-functional-properties ()
  ((Title
    :documentation 
    "A name given to an element. Typically, title will be a name by which the element
is formally known.")
   (Creator
    :documentation
    "An entity primarily responsible for creating the content of the element. Examples
of creator include a person, an organization, or a service. Typically, the name of
a creator should be used to indicate the entity.")
   (Subject
    :documentation
    "A topic of the content of the element. Typically, subject will be expressed as
keywords, key phrases or classification codes that describe a topic of the
element. Recommended best practice is to select a value from a controlled
vocabulary or formal classification scheme.")
   (Description
    :documentation
    "An account of the content of the element. Examples of description include, but
are not limited to: an abstract, table of contents, reference to a graphical
representation of content or a free-text account of the content.")
   (Publisher
    :documentation 
    "An entity responsible for making the element available. Examples of publisher
include a person, an organization, or a service. Typically, the name of a
publisher should be used to indicate the entity.")
   (Contributor
    :documentation 
    "An entity responsible for making contributions to the content of the element.
Ex`amples of contributor include a person, an organization, or a service.
Typically, the name of a contributor should be used to indicate the entity.")
   (Date
    :documentation
    "A date of an event in the lifecycle of the element. Typically, date will be
associated with the creation or availability of the element.")
   (Type
    :documentation
    "The nature or genre of the content of the element. The Type includes terms
describing general categories, functions, genres, or aggregation levels for
content.")
   (Format
    :documentation
    "A physical or digital manifestation of the element. Typically, format may include
4 of 19
the media-type or dimensions of the element. Format may be used to identify the
software, hardware, or other equipment needed to display or operate the
element. Examples of dimensions include size and duration.")
   (Identifier
    :documentation
    "An unambiguous reference to the element within a given context. Recommended
best practice is to identify the element by means of a string or number conforming
to a formal identification system. Formal identification systems include but are not
limited to the Uniform element Identifier (URI) (including the Uniform element
Locator (URL)), the Digital Object Identifier (DOI) and the International Standard
Book Number (ISBN).")
   (Source
    :documentation
    "A reference to an element from which the present element is derived. The present
element may be derived from the source element in whole or in part.
Recommended best practice is to identify the referenced element by means of a
string or number conforming to a formal identification system.")
   (Language
    :documentation
    "A language of the intellectual content of the element.")
   ;;relation clashes with existing ocml relation so changed to has-relation
   (has-Relation
    :documentation
    "A reference to a related element. Recommended best practice is to identify the
referenced element by means of a string or number conforming to a formal
identification system.")
   (Coverage
    :documentation
    "The extent or scope of the content of the element. Typically, coverage will include
spatial location (a place name or geographic coordinates), temporal period (a
period label, date, or date range) or jurisdiction (such as a named administrative
entity).")
   (Rights
    :documentation
    "Information about rights held in and over the element. Typically, rights will contain
a rights management statement for the element, or reference a service providing
such information. Rights information often encompasses Intellectual Property
Rights (IPR), Copyright, and various Property Rights. If the Rights element is
absent, no assumptions may be made about any rights held in or over the
element.")
   (Version
    :documentation
    "As many properties of an element might change in time, an identifier of the
element at a certain moment in time is needed.")))

(def-class effect ())

(def-class pre-condition (unary-kappa-expression))

(def-class post-condition ())

(def-class assumption (unary-kappa-expression))

(def-class wsmo-entity ()
  ((has-non-functional-properties :type core-non-functional-properties)))

(def-class wsmo-web-service-entity (wsmo-entity)
  ((has-non-functional-properties :type web-service-non-functional-properties)))

(def-class web-service-non-functional-properties (core-non-functional-properties)
  ((Performance
    :documentation
    "It represents how fast a service request can be completed. According to [Rajesh
& Arulazi, 2003] performance can be measured in terms of throughput, latency,
execution time, and transaction time. The response time of a service can also be
a measure of the performance. High quality web services should provide higher
throughput, lower latency, lower execution time, faster transaction time and faster
response time.")
   (Reliability
    :documentation
    "It represents the ability of a web service to perform its functions (to maintain its
service quality). It can be measured by the number of failures of the service in a
certain time internal.")
   (Security
    :documentation
    "It represents the ability of a service to provide authentication (entities - users or
other services - who can access service and data should be authenticated),
authorization (entities should be authorized so that they only can access the
protected services), confidentiality (data should be treated properly so that only
authorized entities can access or modify the data), traceability/auditability (it
should be possible to trace the history of a service when a request was serviced),
data encryption (data should be encrypted), and non-repudiation (an entity cannot
deny requesting a service or data after the fact).")
   (Scalability
    :documentation
    "It represents the ability of the service to process more requests in a certain time
interval. It can be measured by the number of solved requests in a certain time
interval.")
   (Robustness
    :documentation
    "It represents the ability of the service to function correctly in the presence of
incomplete or invalid inputs. It can be measured by the number of incomplete or
invalid inputs for which the service still function correctly.")
   (Accuracy
    :documentation
    "It represents the error rate generated by the web service. It can be measured by
the numbers of errors generated in a certain time interval.")
   (Transactional
    :documentation
    "It represents the transactional properties of the web service.")
   (Trust
    :documentation
    "It represents the trust worthiness of the service.")
   (Financial
    :documentation
    "It represents the cost-related properties of a web service.")
   (Network-related-quality-of-service
    :documentation
    "They represent the QoS mechanisms operating in the transport network which are
independent of the web services. They can be measured by network delay, delay
variation and/or message loss.")))

(def-class goal (wsmo-entity invokable-entity)
  ((used-mediator :type mediator)
   (has-post-condition :type post-condition)
   (has-effect :type effect)))

(def-class mediator (invokable-entity wsmo-web-service-entity)
  ((has-source-component :type wsmo-entity)
   (has-target-component :type wsmo-entity)
   (has-mediation-service :type mediation-service)))

(def-class web-service (invokable-entity  wsmo-web-service-entity )
  ((has-capability :type capability)
   (has-interface :type interface)
   (used-mediator :type oo-mediator)))

(def-class capability (wsmo-web-service-entity)
  ((used-mediator :type oo-mediator)
   (has-pre-condition :type pre-condition)
   (has-post-condition :type post-condition)
   (has-assumption :type assumption)
   (has-effect :type effect)))

(def-class interface (wsmo-web-service-entity) 
  ((has-choreography :type choreography)
   (has-orchestration :type orchestration)
   (used-mediator :type oo-mediator)))

(def-class refiner (mediator))

(def-class bridge (mediator))

(def-class wg-mediator (bridge)
  ((has-source-component :type (or web-service wg-mediator))
   (has-target-component :type (or goal wg-mediator))
   (used-mediator :type oo-mediator)
   (has-reduction :type axiom-definition)))

(def-class ww-mediator (bridge)
  ((has-source-component :type (or web-service ww-mediator))
   (has-target-component :type (or web-service ww-mediator))
   (used-mediator :type oo-mediator)))

(def-class gg-mediator (refiner)
  ((used-mediator :type oo-mediator)
   (has-source-component :type (or goal gg-mediator))
   (has-target-component :type (or goal gg-mediator))
   (has-reduction :type axiom-definition)))

(def-class oo-mediator (refiner)
  ((has-source-component :type oo-mediator)))

(def-relation HAS-INPUT-ROLE (?thing ?role)
  "This definition generalises the notion of 
   'having an input role' to classes as well 
    as tasks instances.  If ?class is a method, then
    it also 'inherits the input roles from the task type
    to which it is applicable"
  :sufficient (or (and (instance ?thing)
                       (has-input-role (the-parent ?thing) ?role))
                  (and (class ?thing)
                       (subclass-of ?thing task)
                       (or ;;;;;;;(and (slot-of has-input-role ?thing)
                        (member ?role (all-class-slot-values 
                                       ?thing has-input-role))
                        (and (subclass-of ?thing problem-solving-method)
                             (tackles-task-type ?thing ?task-type)
                             ;; (member ?task-type (all-class-slot-values 
                             ;;                     ?thing tackles-task-type))
                             (has-input-role ?task-type ?role))))))

(def-relation HAS-OUTPUT-ROLE (?thing ?role)
  "This definition generalises the notion of 
   'having an output role' to classes as well 
    as tasks instances.  If ?thing is a method, then
    it also 'inherits the output role from the task type
    to which it is applicable"
  :sufficient  (or (and (instance ?thing)
                        (has-output-role (the-parent ?thing) ?role))
                   (and (class ?thing)
                        (subclass-of ?thing task)
                        (or ;;;;;;(and (slot-of has-output-role ?thing)
                         (member ?role (all-class-slot-values 
                                        ?thing has-output-role))
                         (and (subclass-of ?thing problem-solving-method)
                              (tackles-task-type ?thing ?task-type)
                              ;;(member ?task-type (all-class-slot-values 
                              ;;                   ?thing tackles-task-type))
                              (has-output-role ?task-type ?role))))))

