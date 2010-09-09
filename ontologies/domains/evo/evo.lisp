;;; Mode: Lisp; Package: ocml

;; Events Ontology
;; The Events Ontology provides definitions for the monitoring events that can be generated
;; during the execution of Business Processes.
;; This ontology in based on MXML and the Audit Trail format from the WFMC in order to 
;; accommodate a wide range of BPM systems.
;;
;; Explained in some detail in:
;; C. Pedrinaci, J. Domingue, and A. K. Alves de Medeiros. A Core Ontology for Business 
;; Process Analysis. In 5th European Semantic Web Conference, 2008.
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 1.0

(in-package "OCML")

(in-ontology evo)

(def-class #_ProcessMonitoringEvent ( #_cobra:BusinessActivityMonitoringEvent )
  "An Event that concerns the execution of a Process, i.e. a Process Instance")

(def-class #_ActivityMonitoringEvent ( #_cobra:BusinessActivityMonitoringEvent )
  "An Event that concerns the execution of an Activity, i.e. an Activity Instance"
  ((#_cobra:concernsActivityInstance :min-cardinality 1)))

;; Process Monitoring Events
(def-class #_ProcessStarted ( #_ProcessMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityReady)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityRunning)))

(def-class #_ProcessSuspended ( #_ProcessMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityRunning)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivitySuspended)))

(def-class #_ProcessResumed ( #_ProcessMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivitySuspended)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityRunning)))

(def-class #_ProcessInstantiated ( #_ProcessMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_cobra:BusinessActivityInitialState)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityReady)))

(def-class #_ProcessCompleted ( #_ProcessMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityRunning)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityCompleted)))

(def-class #_ProcessAborted ( #_ProcessMonitoringEvent )
  ((#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityAborted)))

(def-class #_ProcessTerminated ( #_ProcessMonitoringEvent )
  ((#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityTerminated)))

;; Activity Monitoring Events
(def-class #_ActivityAssigned ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityScheduled)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityAssigned)))

(def-class #_ActivityStarted ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityAssigned)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityRunning)))

(def-class #_ActivityReassigned ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_cobra:BusinessActivityAssigned)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_cobra:BusinessActivityAssigned)))

(def-class #_ActivityRelieved ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityAssigned)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityScheduled)))

(def-class #_ActivityManuallySkipped ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityScheduled
                            :value #_BusinessActivityAssigned)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityCompleted)))

(def-class #_ActivityScheduled ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_cobra:BusinessActivityInitialState)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityScheduled)))

(def-class #_ActivityAborted ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityRunning
                            :value #_BusinessActivitySuspended)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityAborted)))

(def-class #_ActivityCompleted ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityRunning)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityCompleted)))

(def-class #_ActivityResumed ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivitySuspended)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityRunning)))

(def-class #_ActivityWithdrawn ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityScheduled
                            :value #_BusinessActivityAssigned)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityAborted)))

(def-class #_ActivitySuspended ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_BusinessActivityRunning)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivitySuspended)))

(def-class #_ActivityAutomaticallySkipped ( #_ActivityMonitoringEvent )
  ((#_cobra:canOccurInState :type #_cobra:BusinessActivityState
                            :value #_cobra:BusinessActivityInitialState)
   (#_cobra:leadsToState :type #_cobra:BusinessActivityState
                         :value #_BusinessActivityCompleted)))


;; States 

(def-class #_BusinessActivityOpen (#_cobra:BusinessActivityState)
  "The Business Activity Realisation's life-cycle has initiated and has not finished yet. In other words the Business Activity Realisation is open.")

(def-class #_BusinessActivityStarted (#_BusinessActivityOpen)
  "The Business Activity Realisation is open. Its execution has started and has not finished yet.")

(def-instance #_BusinessActivityRunning #_BusinessActivityStarted
  "The Business Activity Realisation is running")

(def-instance #_BusinessActivitySuspended #_BusinessActivityStarted
  "The Business Activity Realisation is suspend, i.e., not under execution currently.")

(def-class #_BusinessActivityNotStarted (#_BusinessActivityOpen)
  "The Business Activity Realisation is open. Its execution has not started yet.")

(def-instance #_BusinessActivityAssigned #_BusinessActivityNotStarted
  "The Business Activity Realisation has been assigned to an Agent.")

(def-instance #_BusinessActivityScheduled #_BusinessActivityNotStarted
  "The Business Activity Realisation has been scheduled for its execution. No Agent has been assigned to it yet.")

(def-instance #_BusinessActivityReady #_BusinessActivityNotStarted)

(def-class #_BusinessActivityClosed (#_cobra:BusinessActivityState)
  "The Business Activity Realisation's life-cycle has finished. In other words, the Business Activity Realisation is closed.")

(def-class #_BusinessActivityUnsuccessfullyFinished (#_BusinessActivityClosed)
  "The Business Activity Realisation is closed. Its execution was unsuccessful.")

(def-instance #_BusinessActivityAborted #_BusinessActivityUnsuccessfullyFinished
  "The Business Activity Realisation has been aborted. All the depending previously launched Business Activity Realisations are allowed to finish.")

(def-instance #_BusinessActivityTerminated #_BusinessActivityUnsuccessfullyFinished
  "The Business Activity Realisation has been aborted. The depending previously launched Business Activity Realisations are also Terminated (i.e. they are not allowed to finish).")

(def-class #_BusinessActivitySuccessfullyFinished (#_BusinessActivityClosed)
  "The Business Activity Realisation is closed. Its execution was successful.")

(def-instance #_BusinessActivityCompleted #_BusinessActivitySuccessfullyFinished
  "The Business Activity Realisation has been successfully completed.")

;;
;; Useful Relations
;;

(def-relation #_isOpen (?bar)
  "This relation returns True if the Activity Realisation is Open"
  :constraint (#_cobra:BusinessActivityRealisation ?bar)
  :iff-def (or (#_isNotStarted ?bar) (#_isStarted ?bar)))

(def-relation #_isNotStarted (?bar)
  "This relation returns True if the Activity Realisation has Not Started"
  :constraint (#_cobra:BusinessActivityRealisation ?bar)
  :iff-def (and (#_cobra:isBusinessActivityRealisationInState ?bar ?x)
                (instance-of ?x #_BusinessActivityNotStarted)))

(def-relation #_isStarted (?bar)
  "This relation returns True if the Activity Realisation has Started"
  :constraint (#_cobra:BusinessActivityRealisation ?bar)
  :iff-def (or (#_isRunning ?bar) (#_isSuspended ?bar)))

(def-relation #_isScheduled (?bar)
  "This relation holds if the Business Activity Realisation is Scheduled"
  :constraint (#_cobra:ActivityInstance ?bar)
  :iff-def (#_cobra:isBusinessActivityRealisationInState ?bar #_BusinessActivityScheduled))

(def-relation #_isAssigned (?bar)
  "This relation holds if the Business Activity Realisation is Assigned"
  :constraint (#_cobra:ActivityInstance ?bar)
  :iff-def (#_cobra:isBusinessActivityRealisationInState ?bar #_BusinessActivityAssigned))

(def-relation #_isReady (?bar)
  "This relation holds if the Business Activity Realisation is Ready"
  :constraint (#_cobra:ProcessInstance ?bar)
  :iff-def (#_cobra:isBusinessActivityRealisationInState ?bar #_BusinessActivityReady))

(def-relation #_isRunning (?bar)
  "This relation holds if the Business Activity Realisation is Running"
  :constraint (#_cobra:BusinessActivityRealisation ?bar)
  :iff-def (#_cobra:isBusinessActivityRealisationInState ?bar #_BusinessActivityRunning))

(def-relation #_isActive (?bar)
  "This relation holds if the Business Activity Realisation is Active, i.e., Running"
  :constraint (#_cobra:BusinessActivityRealisation ?bar)
  :iff-def (#_isRunning ?bar))

(def-relation #_isSuspended (?bar)
  "This relation holds if the Business Activity Realisation is Suspended"
  :constraint (#_cobra:BusinessActivityRealisation ?bar)
  :iff-def (#_cobra:isBusinessActivityRealisationInState ?bar #_BusinessActivitySuspended))

(def-relation #_isInactive (?bar)
  "This relation holds if the Business Activity Realisation is Inactive, i.e., Open but not Running"
  :constraint (#_cobra:BusinessActivityRealisation ?bar)
  :iff-def (and (#_isOpen ?bar)
                (not (#_isActive ?bar))))
                     
(def-relation #_isAllocated (?ai)
  "This relation holds if the Activity Instance has been allocated Resources for its execution."

  :constraint (#_cobra:ActivityInstance ?ai)
  :iff-def (or (#_isRunning ?ai)
               (#_isSuspended ?ai)
               (#_isAssigned ?ai)))

(def-relation #_isClosed (?bar)
  "This relation returns True if the Activity Realisation has been closed"
  :iff-def (and (#_cobra:isBusinessActivityRealisationInState ?bar ?x)
                (instance-of ?x #_BusinessActivityClosed)))

(def-relation #_isUnsuccessfullyFinished (?bar)
  "This relation returns True if the Activity Realisation was unsuccessfully finished"
  :iff-def (and (#_cobra:isBusinessActivityRealisationInState ?bar ?x)
                (instance-of ?x #_BusinessActivityUnsuccessfullyFinished)))

(def-relation #_isAborted (?bar)
  "This relation returns True if the Activity Realisation was aborted"
  :iff-def (#_cobra:isBusinessActivityRealisationInState ?bar #_BusinessActivityAborted))

(def-relation #_isTerminated (?bar)
  "This relation returns True if the Activity Realisation was terminated"
  :iff-def (#_cobra:isBusinessActivityRealisationInState ?bar #_BusinessActivityTerminated))

(def-relation #_isSuccessfullyFinished (?bar)
  "This relation returns True if the Activity Realisation was successfully finished"
  :iff-def (and (#_cobra:isBusinessActivityRealisationInState ?bar ?x)
                (instance-of ?x #_BusinessActivitySuccessfullyFinished)))

(def-relation #_isCompleted (?bar)
  "This relation returns True if the Activity Realisation was completed"
  :iff-def (#_cobra:isBusinessActivityRealisationInState ?bar #_BusinessActivityCompleted))





