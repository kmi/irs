;;; Mode: Lisp; Package: ocml

;; Core Ontology for Business pRocess Analysis
;; Inspired by DOLCE, Enterprise Ontology, MXML, TOVE
;;
;; Explained in detail in:
;; C. Pedrinaci, J. Domingue, and A. K. Alves de Medeiros. A Core Ontology for Business 
;; Process Analysis. In 5th European Semantic Web Conference, 2008.
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 2.0-Snapshot

(in-package "OCML")

(in-ontology cobra)

(def-class #_Thing ()
  "Top Level Entity in COBRA.") 

(def-class #_PersistentEntity ()
  "Entity that exists independently from the time. Corresponds to Endurant in DOLCE. Disjoint with Temporal Entities defined in Time Ontology. Persistent Entity Types have a set of Role Types they can play."
  ((#_canPlayRole :type #_RoleType)
   (#_hasAnalysisResult :type #_AnalysisResult)))

(def-relation #_canBePlayedBy (?rt ?pe)
  "True if the ?r is a Role Type that can be played by the ?pe Persistent Entity"
  :constraint (and (#_PersistentEntity ?pe)
                   (#_RoleType ?rt))
  
  :iff-def (has-slot-value ?pe #_canPlayRole ?rt))

(def-class #_PersistentEntityType () ?pt
  "Meta-Class that represents the Types of Persistent Entities."
 :iff-def (subclass-of ?pt #_PersistentEntity))

(def-axiom #_PersistentAndTemporalEntitiesDisjoint
  (subclass-partition #_Thing (set-of #_PersistentEntity #_time:TemporalEntity)))

(def-class #_PhysicalEntity (#_PersistentEntity)
  "Physical Entities are Persistent Entities that have a mass (cf DOLCE)."
  ((#_canPlayRole :type #_RoleType
                  :default-value #_Resource)))

(def-class #_NonPhysicalEntity (#_PersistentEntity)
  "Non-Physical Entities are Persistent Entities that have no mass (cf DOLCE).")

(def-axiom #_PhysicalAndNonPhysicalEntitiesDisjoint
  (subclass-partition #_PersistentEntity (set-of #_PhysicalEntity #_NonPhysicalEntity)))

(def-class #_AgentivePhysicalEntity (#_PhysicalEntity)
  "Agentive Physical Entities are Physical Entities that can take active part in a Business Activity."
  ((#_canPlayRole :type #_RoleType
                  :default-value #_Actor
                  :inheritance :merge)))

(def-class #_NonAgentivePhysicalEntity (#_PhysicalEntity)
  "Non Agentive Physical Entities are Physical Entities that cannot take active part in a Business Activity.")

(def-axiom #_AgentiveAndNonAgentivePhysicalEntitiesDisjoint
  (subclass-partition #_PhysicalEntity (set-of #_AgentivePhysicalEntity #_NonAgentivePhysicalEntity)))

(def-class #_NonPhysicalEntity (#_PersistentEntity)
  "Non-Physical Entities are Persistent Entities that have no mass (cf DOLCE).")

(def-class #_AgentiveNonPhysicalEntity (#_NonPhysicalEntity)
  "Agentive Non Physical Entities are Non Physical Entities that can take active part in a Business Activity."
  ((#_canPlayRole :type #_RoleType
                  :default-value #_Actor
                  :inheritance :merge)))


(def-class #_NonAgentiveNonPhysicalEntity (#_NonPhysicalEntity)
  "Non Agentive Non Physical Entities are Non Physical Entities that cannot take active part in a Business Activity.")

(def-axiom #_AgentiveAndNonAgentiveNonPhysicalEntitiesDisjoint
  (subclass-partition #_NonPhysicalEntity (set-of #_AgentiveNonPhysicalEntity #_NonAgentiveNonPhysicalEntity)))

(def-class #_Agent (#_PersistentEntity) ?a
  "Agents is the union of both Physical and Non Physical Agentive Entities."
  :iff-def (or (instance-of ?a #_AgentivePhysicalEntity) 
               (instance-of ?a #_AgentiveNonPhysicalEntity))
  :avoid-infinite-loop t)

(def-class #_NonAgentiveNonPhysicalEntity (#_NonPhysicalEntity)
  "Non Agentive Non Physical Entities are Non Physical Entities that cannot take active part in a Business Activity.")

(def-class #_Role (#_NonAgentiveNonPhysicalEntity)
  "Roles (e.g., supervisor of something) are Non Agentive Physical Entities. Roles are the actual Roles played within some Business Activity, not the types of Roles (see Role Type)"
)

(def-class #_RoleType (#_NonAgentiveNonPhysicalEntity) ?rt
  "Meta-Class that represents the Types of Roles. Every sub-class of Role is a Role Types."
 :iff-def (subclass-of ?rt #_Role))

(def-class #_AgentiveRole (#_Role)
  "Agentive Roles are Roles that can only be played by Agents.")

(def-class #_NonAgentiveRole (#_Role)
  "Non Agentive Roles are Roles that can be played by any Persistent Entity.")

(def-axiom #_AgentiveRolesAndNonAgentiveRolesDisjoint
  (subclass-partition #_Role (set-of #_AgentiveRole #_NonAgentiveRole)))

;;
;; End Upper-Level Categorization
;;

;;
;; BPA-specific part
;; 

;;Include some default typically useful concepts for self-containment
(def-class #_Object (#_NonAgentivePhysicalEntity)
  "Objects (e.g., a hammer) are Non Agentive Physical Entities.")

(def-class #_Person (#_AgentivePhysicalEntity)
  "Persons are Agentive Physical Entities.")

(def-class #_Organisation (#_AgentiveNonPhysicalEntity)
  "Organisations (e.g., the Open University) are Agentive Non Physical Entities since they are essentially conceptual."
  ((#_canPlayRole :type #_RoleType
                  :default-value #_Resource
                  :inheritance :merge)))

(def-class #_SoftwareAgent (#_AgentiveNonPhysicalEntity)
  "Software Agents are Agentive Non Physical Entities. This is referring to the actual software not to the support that holds it. Thus a computer is not considered as an Agentive Physical Entity since it is not really the computer itself that acts but rather the software it runs.")

;; Include typical Roles for self-containment
(def-class #_Actor (#_AgentiveRole)
  "Actor is the Agentive Role per excellence. Note that Actor is the actual Role playedBy an Agent, not the Agent itself.")

(def-class #_Resource (#_NonAgentiveRole)
  "A Resource is a Non Agentive Role played by Persistent Entities in some Business Activities. Being a Resource is contextual, reason why this is a Role and not a kind of Persistent Entity.")

;;
;; Business Activities
;;

(def-class #_BusinessActivity (#_NonAgentiveNonPhysicalEntity) ?i
  "Concept representing any Business Activity specification. This spans both Processes and Activities.")

(def-class #_Process (#_BusinessActivity) ?i
  "Concept representing a Process specification. A Process is a non-atomic Business Activity. A Process is essentially composedOf Activities which are its atomic constituents. In COBRA we are not concerned with detailed descriptions about Processes, such as the data-flow or the control-flow between activities. These are aspects that should be captured in modelling-specific ontologies. This concept should be mapped to general purpose modelling ontologies and domain-specific ontologies."
  ((#_composedOf :type #_Activity :min-cardinality 2)))

(def-class #_Activity (#_BusinessActivity) ?i
  "Concept representing an Activity specification. An Activity is the atomic component of a Process Process. Although this definition might seem to restrict the possibility for having sub-processes, the actual execution of Processes captures this possibility by means of the relation realises (see ProcessInstance)."
)

(def-relation #_activityBelongsToProcess (?a ?p)
  "True if the Process is composedOf this Activity among others"
  :constraint (and (#_Process ?p)
                   (#_Activity ?a))
  
  :iff-def (has-slot-value ?p #_composedOf ?a))

;; Business Activity relations
(def-relation #_businessActivityRequiresRoleType (?ba ?rt ?quantity)
  "Business Activities require Role Types (engineers, providers, etc). "
  :constraint (and (#_BusinessActivity ?ba)
                   (#_RoleType ?rt))
)

(def-relation #_businessActivityRequiresPersistentEntity (?ba ?pe ?quantity)
  "Business Activities require Persistent Entities (requires Carlos Pedrinaci, requires a particular License). "
  :constraint (and (#_BusinessActivity ?ba)
                   (#_PersistentEntity ?pe))
)

(def-relation #_businessActivityRequiresPersistentEntityType (?ba ?pet ?quantity)
  "Business Activities require Persistent Entity Types (requires a license). "
  :constraint (and (#_BusinessActivity ?ba)
                   (#_PersistentEntityType ?pet))
)

(def-relation #_businessActivityUsesPersistentEntity (?ba ?pe)
  "Business Activities use Persistent Entities. "
  :constraint (and (#_BusinessActivity ?ba)
                   (#_PersistentEntity ?pe))
)

(def-relation #_businessActivityUsesPersistentEntityType (?ba ?pet ?quantity)
  "Business Activities use Persistent Entity Types. "
  :constraint (and (#_BusinessActivity ?ba)
                   (#_PersistentEntityType ?pet))
)

;; Relationship consumes -> to Resources which are not Agents
(def-relation #_businessActivityConsumesPersistentEntityType (?ba ?pet ?quantity)
  "Business Activities consume Persistent Entity Types. Agents are not consumed."
  :constraint (and (#_BusinessActivity ?ba)
                   (#_PersistentEntityType ?pet)
                   (not (or (subclass-of ?pet #_AgentivePhysicalEntity)
                            (subclass-of ?pet #_AgentiveNonPhysicalEntity))))
)

;; Relationship produces to everything but Non Agentive Non Physical ENtities (Robot as a Physical Agentive)
(def-relation #_businessActivityProducesPersistentEntityType (?ba ?pet ?quantity)
  "Business Activities produce Persistent Entity Types. They can even produce agentive Physical Entities, e.g., Robot, but not Non Agentive Non Physical Entities, e.g., Information. For latter the relationship to be used is Provides."
  :constraint (and (#_BusinessActivity ?ba)
                   (#_PersistentEntityType ?pet)
                   (not (subclass-of ?pet #_NonAgentiveNonPhysicalEntity)))                
)

;; Relationship provides -> Non-Agentive Non-Physical Entities
(def-relation #_businessActivityProvidesPersistentEntityType (?ba ?pet ?quantity)
  "Business Activities provide Persistent Entity Types. Providing is restricted to Non Agentive Non Physical Entities, e.g., Information."
  :constraint (and (#_BusinessActivity ?ba)
                   (#_PersistentEntityType ?pet)
                   (subclass-of ?pet #_NonAgentiveNonPhysicalEntity))                
)

;;
;; Business Activity Realisations
;;
(def-class #_BusinessActivityRealisation (#_time:TimeSpanningEntity) ?i
  "Concept representing the actual realisation/execution of a Business Activity specification. It therefore spans both Processes and Activities executions."
  ((#_hasAnalysisResult :type #_AnalysisResult)
   (#_hasCurrentState :type #_BusinessActivityState
                      :default-value #_BusinessActivityInitialState)
   (#_hasLifeCycle :type #_LifeCyclePeriod)))

;;
;; Implemented as a Relation to avoid heavy updating of slots and manipulation
;; This is restricted to Business Activity Monitoring Events
;; Message Events should be given another relation
;;
(def-relation #_hasExecutionHistory (?bar ?event)
  "Business Activity Realisations have an Execution History which is basically the Monitoring Events that concern them."
  :constraint (and (#_BusinessActivityRealisation ?bar)
                   (#_BusinessActivityMonitoringEvent ?event))

  :iff-def (and (instance-of ?event #_BusinessActivityMonitoringEvent)
                (or (and (instance-of ?bar #_ProcessInstance)  
                         (= (the-slot-value ?event #_concernsProcessInstance) ?bar))
                    (and (instance-of ?bar #_ActivityInstance) 
                         (= (the-slot-value ?event #_concernsActivityInstance) ?bar)))))

(def-relation #_playsRoleIn (?pe ?role ?bar)
  "Persistent Entities play Roles in Business Activity Realisations. This relation has no predefined definition. Actual relation instances should be asserted manually."
  :constraint (and (#_PersistentEntity ?pe)
                  (#_Role ?role)
                  (#_BusinessActivityRealisation ?bar)))

(def-axiom #_noRolePlayedByEntitiyThatCannotPlayTheRole
  "Axiom that ensures that no Role is played by a Persistent Entity that cannot play the given Role Type"
  (forall (?role)
          (=> (#_Role ?role)
              (not (exists (?pe ?bar)
                           (and (#_PersistentEntity ?pe)
                                (#_BusinessActivityRealisation ?bar)
                                (#_playsRoleIn ?pe ?role ?bar)
                                (not (has-slot-value ?pe #_canPlayRole ?role))))))))

(def-relation #_isFulfilledBy (?role ?pe ?bar)
  "Roles are fulfilled by Persistent Entities in Business Activity Realisations. This relation is pretty much the same as playRoleIn. It is defined in terms of playsRoleIn"
  :constraint (and (#_PersistentEntity ?pe)
                  (#_Role ?role)
                  (#_BusinessActivityRealisation ?bar))
  :iff-def (#_playsRoleIn ?pe ?role ?bar))

(def-class #_ProcessInstance (#_BusinessActivityRealisation) ?i
  "Concept representing an actual execution of a Process specification. Each Process Instance performs a Process (i.e. executes). Every Process Instance"
  ((#_performs :type #_Process :cardinality 1)
   (#_composedOf :type #_ActivityInstance :min-cardinality 2)
   (#_realises :type #_ActivityInstance :max-cardinality 1)))

(def-relation #_processPerformedByProcessInstance (?p ?pi)
  "True if the Process is performed by the given Process Instance"
  :constraint (and (#_Process ?p)
                   (#_ProcessInstance ?pi))
  
  :iff-def (= ?p (the-slot-value ?pi #_performs)))

(def-class #_ActivityInstance (#_BusinessActivityRealisation) ?i
  "Concept representing an Activity specification. A Process is a non-atomic Business Activity. A Process is essentially composedOf Activities which are its atomic constituents. In COBRA we are not concerned with detailed descriptions about Processes, such as the data-flow or the control-flow between activities. These are aspects that should be captured in modelling-specific ontologies. This concept should be mapped to general purpose modelling ontologies and domain-specific ontologies."
 ((#_performs :type #_Activity :cardinality 1)))

(def-relation #_activityPerformedByActivityInstance (?a ?ai)
  "True if the Activity is performed by the given Activity Instance"
  :constraint (and (#_Activity ?a)
                   (#_ActivityInstance ?ai))
  
  :iff-def (= ?a (the-slot-value ?ai #_performs)))

(def-relation #_businessActivityRealisationPerforms (?bar ?ba)
  "True if the Business Activity Realisation performs the Business Activity."
  :constraint (and (#_BusinessActivity ?ba)
                   (#_BusinessActivityRealisation ?bar))
  
  :iff-def (has-slot-value ?bar #_performs ?ba))

(def-relation #_activityInstanceBelongsToProcessInstance (?ai ?pi)
  "True if the Process Instance is composedOf this Activity Instance among others"
  :constraint (and (#_ProcessInstance ?pi)
                   (#_ActivityInstance ?ai))
  
  :iff-def (has-slot-value ?pi #_composedOf ?ai))

;; TODO: Implement the iff-def
(def-relation #_activityInstanceRealisedByProcessInstance (?ai ?pi)
  "True if the Activity Instance is actually realised by the Process Instance. By means of this relation we are able to capture the Process-SubProcess relationship at the execution level."
  :constraint (and (#_ProcessInstance ?pi)
                   (#_ActivityInstance ?ai))
  
  :iff-def (= ?ai (the-slot-value ?pi #_realises)))

(def-relation #_businessActivityRealisationInvolvesAgent (?bar ?agent)
  "Business Activities Realisations involve Agents in their enactment."
  :constraint (and (#_BusinessActivityRealisation ?bar)
                   (#_Agent ?agent))

  :iff-def (or (and (instance-of ?bar #_ActivityInstance)
                    (exists ?bame (and (#_BusinessActivityMonitoringEvent ?bame)
                                       (= ?bar 
                                          (the-slot-value ?bame #_concernsActivityInstance))
                                       (= ?agent 
                                          (the-slot-value ?bame #_generatedBy)))))

               (and (instance-of ?bar #_ProcessInstance)
                    (exists ?bame (and (#_BusinessActivityMonitoringEvent ?bame)
                                       (or (= ?bar 
                                              (the-slot-value ?bame #_concernsActivityInstance))
                                           (= ?bar 
                                              (the-slot-value ?bame #_concernsProcessInstance)))
                                       (= ?agent 
                                          (the-slot-value ?bame #_generatedBy))))))
)

(def-relation #_businessActivityRealisationUsesPersistentEntity (?bar ?pet)
   "Business Activities Realisations use Persistent Entities in their enactment."
  :constraint (and (#_BusinessActivityRealisation ?bar)
                   (#_PersistentEntityType ?pet))
)

(def-relation #_businessActivityRealisationUsesPersistentEntityType (?bar ?pet ?quantity)
   "Business Activities Realisations use a certain amount of Persistent Entities of some type in their enactment."
  :constraint (and (#_BusinessActivityRealisation ?bar)
                   (#_PersistentEntityType ?pet))
)

(def-relation #_businessActivityRealisationConsumesPersistentEntity (?bar ?pe ?quantity)
   "Business Activities Realisations consume a certain amount of a Persistent Entity (e.g., electricity) in their enactment."
  :constraint (and (#_BusinessActivityRealisation ?bar)
                   (#_PersistentEntity ?pe)
                   (not (instance-of #_Agent)))
)

(def-relation #_businessActivityRealisationConsumesPersistentEntityType (?bar ?pet ?quantity)
   "Business Activities Realisations consume a certain amount of a Persistent Entities of some type (e.g., screws) in their enactment."
  :constraint (and (#_BusinessActivityRealisation ?bar)
                   (#_PersistentEntityType ?pet)
                   (not (or (subclass-of ?pet #_AgentivePhysicalEntity)
                            (subclass-of ?pet #_AgentiveNonPhysicalEntity))))
)

(def-relation #_businessActivityRealisationProducesPersistentEntity (?bar ?pe ?quantity)
   "Business Activities Realisations produce a certain amount of a Persistent Entity (e.g., electricity) in their enactment."
  :constraint (and (#_BusinessActivityRealisation ?bar)
                   (#_PersistentEntity ?pe)
                   (not (instance-of ?pe #_NonAgentiveNonPhysicalEntity)))
)

(def-relation #_businessActivityRealisationProducesPersistentEntityType (?bar ?pet ?quantity)
   "Business Activities Realisations produce a certain amount of a Persistent Entities of some type (e.g., screws) in their enactment."
  :constraint (and (#_BusinessActivityRealisation ?bar)
                   (#_PersistentEntityType ?pet)
                   (not (subclass-of ?pet #_NonAgentiveNonPhysicalEntity)))
)

(def-relation #_businessActivityRealisationProvidesPersistentEntity (?bar ?pe ?quantity)
   "Business Activities Realisations provide a certain amount of a Non-Agentive Non-Physical Persistent Entity (e.g., electricity) in their enactment."
  :constraint (and (#_BusinessActivityRealisation ?bar)
                   (#_NonAgentiveNonPhysicalEntity ?pe))
)

(def-relation #_businessActivityRealisationProvidesPersistentEntityType (?bar ?pet ?quantity)
   "Business Activities Realisations provide a certain amount of a Persistent Entities of some type (e.g., screws) in their enactment."
  :constraint (and (#_BusinessActivityRealisation ?bar)
                   (#_PersistentEntityType ?pet)
                   (subclass-of ?pet #_NonAgentiveNonPhysicalEntity))
)

(def-relation #_wasBusinessActivityRealisationInState (?bar ?instant ?state) 
  "True if the state of the Business Activity Realisation at ?instant is the one given"
 :constraint (and (#_BusinessActivityRealisation ?bar)
                  (#_XSD:DateTime ?instant)
                  (#_BusinessActivityState ?state))

 :iff-def (exists ?lcp (and (instance-of ?lcp #_LifeCyclePeriod)
                            (#_time:instantDuring ?instant ?bar)
                            (= ?state (the-slot-value ?bar #_hasState)))))


(def-relation #_isBusinessActivityRealisationInState (?bar ?state) 
  "True if the current state of the Business Activity Realisation is the one given."
 :constraint (and (#_BusinessActivityRealisation ?bar)                
                  (#_BusinessActivityState ?state))

 :iff-def (= (the-slot-value ?bar #_hasCurrentState) ?state))

;;
;; Monitoring Events
;;

(def-class #_MonitoringEvent ( #_time:InstantaneousEntity )
  ((#_hasData :type #_DataValue)
   (#_receivedAt :type #_XSD:DateTime)
   (#_causedBy :type #_MonitoringEvent)
   (#_generatedBy :type #_Agent :max-cardinality 1)))

(def-class #_DataValue ( #_time:InstantaneousEntity )
  ((#_hasParameter :type url)
   (#_hasValue :type string)))

(def-class #_BusinessActivityMonitoringEvent ( #_MonitoringEvent )
  ((#_concernsProcessInstance :type #_ProcessInstance :cardinality 1)
   (#_concernsActivityInstance :type #_ActivityInstance :max-cardinality 1)
   (#_generatedBy :type #_Agent :cardinality 1)
   (#_canOccurInState :type #_BusinessActivityState :min-cardinality 1)
   (#_leadsToState :type #_BusinessActivityState :cardinality 1)))

(def-class #_MessageEvent ( #_MonitoringEvent )
  ((#_processedBy :type #_Agent)
   (#_timeToLive :type #_XSD:DateTime :max-cardinality 1)))

(def-class #_BusinessActivityState (#_NonAgentiveNonPhysicalEntity)
  "Every Business Activity Realisation follows a state model. This state model informs us about the possible transitions and provides at the same time the semantics for the states. Indirectly, since Business Activity Monitoring Events are based on states, their semantics are defined according to these."
  ((#_possibleNextState :type #_BusinessActivityState)))

(def-class #_BusinessActivityStateType (#_NonAgentiveNonPhysicalEntity) ?bat
  "Meta-Class that represents the Types of Business Activity States. Every sub-class of Business Activity State is a Business Activity State Type. This allows further specifying Business Activity Monitoring Events and given that it has an intensional definition it is independent from the actual set of events adopted."
 :iff-def (subclass-of ?bat #_BusinessActivityState))

;; Include the default initial state in COBRA
(def-instance #_BusinessActivityInitialState #_BusinessActivityState
  "Initial state for every Business Activity Realisation when nothing else is known.")

(def-class #_LifeCyclePeriod (#_time:TimeSpanningEntity)
  "Every Business Activity Realisation is characterised by its Life Cycle. The Life Cycle is composed of Life Cycle Periods which are basically intervals during which the Business Activity Realisation was in a specific state. Every event will mark the transition between two states."
  ((#_hasState :type #_BusinessActivityState)))

;;
;; Analysis
;;
(def-class #_Analysis (#_NonAgentiveNonPhysicalEntity) 
  "An Analysis is a Non Agentive Non Physical Entity. The Oxford English Dictionnary defines it as: The resolution or breaking up of anything complex into its various simple elements, the opposite process to synthesis. An Analysis can be either Quantitative or Qualitative."
)

(def-class #_QuantitativeAnalysis (#_Analysis) 
  "Concept representing Quantitative Analysis. Quantitative Analysis is a kind of Analysis that produces a Quantitative Result, i.e., a Constant Quantity (see Physical Quantities ontology). Counting the number of instances is a kind of Quantitative Analysis."
)

(def-class #_QualitativeAnalysis (#_Analysis) 
  "Concept representing Qualitative Analysis. Qualitative Analysis is a kind of Analysis that produces a Qualitative Result, i.e., a class or an instance. Diagnosis is for example a Qualitative Analysis."
)

(def-axiom #_QualitativeAndQuantitativeAnalysisDisjoint
  (subclass-partition #_Analysis (set-of #_QuantitativeAnalysis #_QualitativeAnalysis)))

(def-class #_Query (#_QualitativeAnalysis)
  "A Query is a prototypical kind of Qualitative Analysis that simply captures ontological queries. It has a set of input roles, a list of output types and a body. At runtime the body which is a procedure will be evaluated by binding the internal variables of the body to the given input roles. The output types provide a typing mechanism for queries so that results can be properly manipulated. "
  ((has-input-role :type role)
   (#_hasOutputType :type list :cardinality 1)
   (#_hasBody :type unary-procedure :cardinality 1)))

;;
;; Analysis Results bit
;;

(def-class #_AnalysisResult (#_time:InstantaneousEntity)
  "Analysis Results are Instantaneous Entities that are the outcome of performing some Analysis. Anything, Persistent and Temporal Entities can have related Analysis Results. Analysis Results can be either Qualitative or Quantitative."
  ((#_calculatedAt :type #_XSD:DateTime)
   (#_hasAnalysis :type #_Analysis)))

;; TODO : Reset the slot renaming which does not seem to work with namespaces?
;;    :slot-renaming ((#_calculatedAt #_time:occursAt)))

(def-class #_QualitativeAnalysisResult (#_AnalysisResult) 
  "Concept representing Qualitative Analysis Results. Qualitative Analysis Results are Analysis Results where the actual result are instances or classes different from Physical Quantities. An example of this would be a diagnosis result. This is mainly a placeholder for including domain-specific refinements."
;;  ((#_hasValue :type (or class instance))
;; Commented out until 'or' type definitions are supported
   ((#_hasValue :type class)
   (#_hasAnalysis :type #_QualitativeAnalysis)))

(def-class #_QuantitativeAnalysisResult (#_AnalysisResult)
  "Quantitative Analysis Results are Analysis Results whereby the actual result is a Constant Quantity. An example is a Metric. This is mainly a placeholder for including domain-specific refinements."
  ((#_hasValue :type #_phys-q:ConstantQuantity :cardinality 1)
   (#_hasAnalysis :type #_QuantitativeAnalysis)))

(def-axiom #_QualitativeAndQuantitativeAnalysisResultDisjoint
  (subclass-partition #_Analysis (set-of #_QuantitativeAnalysisResult #_QualitativeAnalysisResult)))

(def-relation #_analysisHasResult (?analysis ?result)
  "True if the Analysis has the given result. Implemented as a relation to avoid redundancy and simplify maintenance."
  :constraint (and (#_Analysis ?analysis)
                   (#_AnalysisResult ?result))
  
  :iff-def (= ?analysis (the-slot-value ?result #_hasAnalysis)))

