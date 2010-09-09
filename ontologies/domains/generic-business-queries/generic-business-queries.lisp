;;; Mode: Lisp; Package: ocml

;; Generic Business Queries Ontology
;; This ontology captures a set of general purpose domain-independent business queries
;; that are generally relevant for the analysis of business processes independently
;; from the kind of activity they perform. These queries can be applied during the
;; computation of aggregation metrics (see Generic Business Metrics Ontology) or in 
;; more general terms during the analysis of processes, filtering of information, etc.
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 1.0

(in-package "OCML")

(in-ontology generic-business-queries)

;;;
;; General Purpose Queries
;;;
(def-class #_GetInstancesOfType (#_cobra:Query)
  "This query obtains all the instances of some kind."
  ((has-input-role :value #_ofType)
   (#_ofType :type class :cardinality 1)              
   (#_cobra:hasBody :default-value 
              '(lambda (?q)
                 (in-environment 
                  ((?type . (the-slot-value ?q #_ofType)))
                  (setofall ?x (instance-of ?x ?type)))))))


(def-class #_GetInstancesOfTypeByRelation (#_cobra:Query)
  "This query obtains all the instances of some kind, that honour some unary relation."
  ((has-input-role :value #_ofType 
                   :value #_relationHolds)
   (#_ofType :type class :cardinality 1)              
   (#_relationHolds :type unary-relation :cardinality 1)
   (#_cobra:hasBody :default-value 
              '(lambda (?q)
                 (in-environment 
                  ((?type . (the-slot-value ?q #_ofType))
                   (?rel . (the-slot-value ?q #_relationHolds)))
                  (setofall ?x (and (instance-of ?x ?type)
                                    (?rel ?x))))))))

;; Modelled like this until we define a more flexible way. Probably using input roles.
(def-class #_GetInstancesOfTypeByBinaryRelation (#_cobra:Query)
  "This query obtains all the instances of some kind, that honour some binary relation. The query constructed depends on the bindings given to the subject and object roles, knowing that binary relations are in the form: (binary-relation ?subject ?object). 
This provides a simple, flexible, yet powerful way of querying. For more complex relations a more complex definition should be required. "
  ((has-input-role :value #_ofType 
                   :value #_relationHolds
                   :value #_hasSubject
                   :value #_hasObject)
   (#_ofType :type class :cardinality 1)              
   (#_relationHolds :type binary-relation :cardinality 1)
   (#_hasSubject :type class)              
   (#_hasObject :type class)              
   (#_cobra:hasBody :default-value 
              '(lambda (?q)
                 (in-environment 
                  ((?type . (the-slot-value ?q #_ofType))
                   (?rel . (the-slot-value ?q #_relationHolds)))
                  (if (is-role-value-set ?q '#_hasSubject)
                      (setofall ?x (and (instance-of ?x ?type)
                                        (?rel (the-slot-value ?q #_hasSubject) ?x)))

                    (if (is-role-value-set ?q '#_hasObject)
                        (setofall ?x (and (instance-of ?x ?type)
                                          (?rel ?x (the-slot-value ?q #_hasObject))))
                      ;; The query wasn't built properly
                      nil)))))))


;;
;; Prototypical Queries for listing instances of some kind 
;;

(def-instance #_whichAgents #_GetInstancesOfType
  ((#_ofType #_cobra:Agent)
   (#_cobra:hasOutputType (#_cobra:Agent))))

(def-instance #_whichProcesses #_GetInstancesOfType
  ((#_ofType #_cobra:Process)
   (#_cobra:hasOutputType (#_cobra:Process))))

(def-instance #_whichActivities #_GetInstancesOfType
  ((#_ofType #_cobra:Activity)
   (#_cobra:hasOutputType (#_cobra:Activity))))

(def-instance #_whichBusinessActivities #_GetInstancesOfType
  ((#_ofType #_cobra:BusinessActivity)
   (#_cobra:hasOutputType (#_cobra:BusinessActivity))))

(def-instance #_whichPIs #_GetInstancesOfType
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))))

(def-instance #_whichAIs #_GetInstancesOfType
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))))

(def-instance #_whichBARs #_GetInstancesOfType
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))))

(def-instance #_whichAnalysisResults #_GetInstancesOfType
  ((#_ofType #_cobra:AnalysisResult)
   (#_cobra:hasOutputType (#_cobra:AnalysisResult))))

(def-instance #_whichMetrics #_GetInstancesOfType
  ((#_ofType #_cobra:Metric)
   (#_cobra:hasOutputType (#_cobra:Metric))))

(def-instance #_whichObjects #_GetInstancesOfType
  ((#_ofType #_cobra:Object)
   (#_cobra:hasOutputType (#_cobra:Object))))

(def-instance #_whichPersons #_GetInstancesOfType
  ((#_ofType #_cobra:Person)
   (#_cobra:hasOutputType (#_cobra:Person))))

(def-instance #_whichOrganisations #_GetInstancesOfType
  ((#_ofType #_cobra:Organisation)
   (#_cobra:hasOutputType (#_cobra:Organisation))))

(def-instance #_whichSoftwareAgents #_GetInstancesOfType
  ((#_ofType #_cobra:SoftwareAgent)
   (#_cobra:hasOutputType (#_cobra:SoftwareAgent))))

;;
;; Prototypical Query Templates for obtaining instances filtered by Binary Relation
;;
(def-class #_whichPIsPerformProcess (#_GetInstancesOfTypeByBinaryRelation)
  ((#_ofType :value #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds :value #_cobra:performs)))

(def-class #_whichProcessPerformedByPI (#_GetInstancesOfTypeByBinaryRelation)
  ((#_ofType :value #_cobra:Process)
   (#_cobra:hasOutputType (#_cobra:Process))
   (#_relationHolds :value #_cobra:performedBy)))

(def-class #_whichAIsPerformActivity (#_GetInstancesOfTypeByBinaryRelation)
  ((#_ofType :value #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds :value #_cobra:performs)))

(def-class #_whichActivityPerformedByAI (#_GetInstancesOfTypeByBinaryRelation)
  ((#_ofType :value #_cobra:Activity)
   (#_cobra:hasOutputType (#_cobra:Activity))
   (#_relationHolds :value #_cobra:performedBy)))

(def-class #_whichBusinessActivitiesBelongToProcess (#_GetInstancesOfTypeByBinaryRelation)
  ((#_ofType :value #_cobra:BusinessActivity)
   (#_cobra:hasOutputType (#_cobra:BusinessActivity))
   (#_relationHolds :value #_cobra:belongsTo)))

(def-class #_whichPIRealisesAI (#_GetInstancesOfTypeByBinaryRelation)
  ((#_ofType :value #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds :value #_cobra:realises)))

;;
;; Prototypical Query Template for obtaining instances filtered by Binary Relation
;;
(def-class #_whichBARsPerformTypeOfBusinessActivity (#_cobra:Query)
  "This query obtains all the Business Activity Realisations that perform some kind of Business Activity."
  ((has-input-role :value #_ofType)
   (#_ofType :type class :cardinality 1)     
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))         
   (#_cobra:hasBody :default-value 
              '(lambda (?q)
                 (in-environment 
                  ((?type . (the-slot-value ?q #_ofType)))
                  (setofall ?x (and (performs ?x ?y)
                                    (instance-of ?y ?type))))))))

;;;;;
;; State Based Generic Queries
;;;;;

;;
;; Query Business Activity Realisations by Current State
;;
(def-instance #_whichBARsOpen #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isOpen)))

(def-instance #_whichAIsOpen #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isOpen)))

(def-instance #_whichPIsOpen #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isOpen)))

(def-instance #_whichBARsNotStarted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isNotStarted)))

(def-instance #_whichAIsNotStarted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isNotStarted)))

(def-instance #_whichPIsNotStarted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isNotStarted)))

(def-instance #_whichAIsScheduled #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isScheduled)))

(def-instance #_whichAIsAssigned #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isAssigned)))

(def-instance #_whichPIsReady #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isReady)))

(def-instance #_whichBARsStarted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isStarted)))

(def-instance #_whichAIsStarted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isStarted)))

(def-instance #_whichPIsStarted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isStarted)))

(def-instance #_whichBARsRunning #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isRunning)))

(def-instance #_whichAIsRunning #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isRunning)))

(def-instance #_whichPIsRunning #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isRunning)))

(def-instance #_whichBARsSuspended #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isSuspended)))

(def-instance #_whichAIsSuspended #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isSuspended)))

(def-instance #_whichPIsSuspended #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isSuspended)))

(def-instance #_whichBARsClosed #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isClosed)))

(def-instance #_whichAIsClosed #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isClosed)))

(def-instance #_whichPIsClosed #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isClosed)))

(def-instance #_whichBARsUnsuccessfullyFinished #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isUnsuccessfullyFinished)))

(def-instance #_whichAIsUnsuccessfullyFinished #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isUnsuccessfullyFinished)))

(def-instance #_whichPIsUnsuccessfullyFinished #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isUnsuccessfullyFinished)))

(def-instance #_whichBARsAborted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isAborted)))

(def-instance #_whichAIsAborted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isAborted)))

(def-instance #_whichPIsAborted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isAborted)))

(def-instance #_whichBARsTerminated #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isTerminated)))

(def-instance #_whichAIsTerminated #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isTerminated)))

(def-instance #_whichPIsTerminated #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isTerminated)))

(def-instance #_whichBARsSuccessfullyFinished #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isSuccessfullyFinished)))

(def-instance #_whichAIsSuccessfullyFinished #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isSuccessfullyFinished)))

(def-instance #_whichPIsSuccessfullyFinished #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isSuccessfullyFinished)))

(def-instance #_whichBARsCompleted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:BusinessActivityRealisation)
   (#_cobra:hasOutputType (#_cobra:BusinessActivityRealisation))
   (#_relationHolds #_evo:isCompleted)))

(def-instance #_whichAIsCompleted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ActivityInstance)
   (#_cobra:hasOutputType (#_cobra:ActivityInstance))
   (#_relationHolds #_evo:isCompleted)))

(def-instance #_whichPIsCompleted #_GetInstancesOfTypeByRelation
  ((#_ofType #_cobra:ProcessInstance)
   (#_cobra:hasOutputType (#_cobra:ProcessInstance))
   (#_relationHolds #_evo:isCompleted)))

;;
;; Query based on past state
;;

(def-class #_getBARsInStateAtInstant (#_cobra:Query)
 "This query obtains all the Business Activity Realisations of some kind, that were in a particular execution state at a given time instant."
  ((has-input-role :value #_inState 
                   :value #_ofType 
                   :value #_atTimeInstant)
   (#_cobra:hasOutputType :default-value (#_cobra:BusinessActivityRealisation))
   (#_ofType :type class
              :default-value #_cobra:BusinessActivityRealisation)
   (#_inState :type #_cobra:BusinessActivityState)
   (#_atTimeInstant :type #_time:TimeInstant)
   (#_cobra:hasBody :value '(lambda (?q)
                        (in-environment 
                         ((?type . (the-slot-value ?q #_ofType))
                          (?state . (the-slot-value ?q #_inState))
                          (?instant . (the-slot-value ?q #_atTimeInstant)))
                         (setofall ?x (and (instance-of ?x ?type)
                                           (#_cobra:wasBusinessActivityRealisationInState ?x ?instant ?state))))))))

