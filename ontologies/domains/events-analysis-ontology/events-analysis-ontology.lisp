;;; Mode: Lisp; Package: ocml

;; Events Analysis Ontology
;; The Events Analysis Ontology provides definitions for the analysis of monitoring events
;; It brings Events Manipulation to the level of Processes Analysis
;;
;; Explained in some detail in:
;; C. Pedrinaci, J. Domingue, and A. K. Alves de Medeiros. A Core Ontology for Business 
;; Process Analysis. In 5th European Semantic Web Conference, 2008.
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 1.1


(in-package "OCML")

(in-ontology events-analysis-ontology)

(def-function #_currentLifeCyclePeriod (?bar) -> ?lcp
  "Returns the current Life Cycle period for the given Business Activity Realisation"
  :constraint (#_cobra:BusinessActvityRealisation ?bar)
  :body (setofall ?x (and (has-slot-value ?bar #_cobra:hasLifeCycle ?x)
                          (#_time:instantDuring (#_time:now) ?x))))

(def-relation #_isNormalTransition (?fromState ?toState)
  "Given the current state and a new state, this relation holds if it is one of the expected transitions."
  :constraint (and (#_cobra:BusinessActivityState ?fromState)
                   (#_cobra:BusinessActivityState ?toState))
                  
  :iff-def (has-slot-value ?fromState #_cobra:possibleNextState ?toState))
                            
;;;
;; Events Process Forward-Chaining Rules
;;;                         

;;
;; States and Life Cycle
;;

(def-procedure #_addSlotValue (?instance ?slot ?value)
  :lisp-fun #'(lambda (i s v)
                (add-slot-value i s v)))

;; Update Process Composition
(def-rule #_updateComposedOfProcInstance
    (#_cobra:BusinessActivityMonitoringEvent ?bame)
    (#_cobra:ProcessInstance ?pi)
    (#_cobra:ActivityInstance ?ai)
    (#_cobra:concernsActivityInstance ?bame ?ai)
    (#_cobra:concernsProcessInstance ?bame ?pi)
    
    then

    (exec (#_addSlotValue ?pi #_cobra:composedOf ?ai))
    (exec (output "Added ~S to composedOf slot of ~S~%" ?ai ?pi)))

;; Update Execution State
(def-rule #_updateStateActivityInstance
    (#_cobra:BusinessActivityMonitoringEvent ?bame)
    (#_cobra:ActivityInstance ?ai)
    (#_cobra:concernsActivityInstance ?bame ?ai)
    
    then

    ;; Update the Current State
    (exec (set-slot-value ?ai 
                          #_cobra:hasCurrentState 
                          (the-slot-value ?bame #_cobra:leadsToState)))

    (exec (output "Updated the state of ~S to ~S~%" ?ai 
                  (the-slot-value ?bame #_cobra:leadsToState))))

(def-rule #_updateStateProcessInstance
    (#_cobra:BusinessActivityMonitoringEvent ?bame)
    (#_cobra:ProcessInstance ?pi)
    (#_cobra:concernsProcessInstance ?bame ?pi)
    (not (#_cobra:concernsActivityInstance ?bame ?ai))
    
    then

    ;; Update the Current State
    (exec (set-slot-value ?pi 
                          #_cobra:hasCurrentState 
                          (the-slot-value ?bame #_cobra:leadsToState)))

    (exec (output "Updated the state of ~S to ~S~%" ?pi 
                  (the-slot-value ?bame #_cobra:leadsToState))))


;; Create an Interval with a starting time
(def-function #_createTimeInterval (?st) -> ?ti
  :lisp-fun #'(lambda (st)
                (let ((instance-structure
                       (new-instance '#_time:TimeInterval 
                                     `((#_time:hasStartTime ,st)))))

                  (ocml::name instance-structure))))

;;
;; Update the Intervals Spanned by Activities and Processes
;;

;; Track Start
(def-rule #_trackActivityInstanceStart
    (#_evo:ActivityStarted ?bame)
    (#_cobra:ActivityInstance ?ai)
    (#_cobra:concernsActivityInstance ?bame ?ai)
    (#_time:occursAt ?bame ?ti)
        
    then

    ;; Create the Time Interval for the Activity
    (exec (set-slot-value ?ai #_time:spansInterval 
                          (#_createTimeInterval ?ti)))

    (exec (output "Initialized the interval spanned by ~S~%" ?ai)))

(def-rule #_trackProcessInstanceStart
    (#_evo:ProcessStarted ?bame)
    (#_cobra:ProcessInstance ?pi)
    (#_cobra:concernsProcessInstance ?bame ?pi)
    (#_time:occursAt ?bame ?ti)
        
    then

    ;; Create the Time Interval for the Process
    (exec (set-slot-value ?pi #_time:spansInterval 
                          (#_createTimeInterval ?ti)))

    (exec (output "Initialized the interval spanned by ~S~%" ?pi)))


;; Track End
(def-rule #_trackProcessInstanceCompleted
    (#_evo:ProcessCompleted ?bame)
    (#_cobra:ProcessInstance ?pi)
    (#_cobra:concernsProcessInstance ?bame ?pi)
    (#_time:occursAt ?bame ?ti)
        
    then

    ;; Close the Time Interval for the Process
    (exec (set-slot-value (the-slot-value ?pi #_time:spansInterval)
                          #_time:hasEndTime ?ti))

    (exec (output "Closed the interval spanned by ~S~%" ?pi)))

(def-rule #_trackProcessInstanceAborted
    (#_evo:ProcessAborted ?bame)
    (#_cobra:ProcessInstance ?pi)
    (#_cobra:concernsProcessInstance ?bame ?pi)
    (#_time:occursAt ?bame ?ti)
        
    then

    ;; Close the Time Interval for the Process
    (exec (set-slot-value (the-slot-value ?pi #_time:spansInterval)
                          #_time:hasEndTime ?ti))

    (exec (output "Closed the interval spanned by ~S~%" ?pi)))

;; TODO: We should also terminate all the Activities running as well
(def-rule #_trackProcessInstanceTerminated
    (#_evo:ProcessTerminated ?bame)
    (#_cobra:ProcessInstance ?pi)
    (#_cobra:concernsProcessInstance ?bame ?pi)
    (#_time:occursAt ?bame ?ti)
        
    then

    ;; Close the Time Interval for the Process
    (exec (set-slot-value (the-slot-value ?pi #_time:spansInterval)
                          #_time:hasEndTime ?ti))

    (exec (output "Closed the interval spanned by ~S~%" ?pi)))

;; Track the end of Activities
(def-rule #_trackActivityInstanceManuallySkipped
    (#_evo:ActivityManuallySkipped ?bame)
    (#_cobra:ActivityInstance ?ai)
    (#_cobra:concernsActivityInstance ?bame ?ai)
    (#_time:occursAt ?bame ?ti)
        
    then

    ;; Close the Time Interval for the Activity
    (exec (set-slot-value (the-slot-value ?ai #_time:spansInterval) 
                          #_time:hasEndTime ?ti))

    (exec (output "Closed the interval spanned by ~S~%" ?ai)))

(def-rule #_trackActivityInstanceCompleted
    (#_evo:ActivityCompleted ?bame)
    (#_cobra:ActivityInstance ?ai)
    (#_cobra:concernsActivityInstance ?bame ?ai)
    (#_time:occursAt ?bame ?ti)
        
    then

    ;; Close the Time Interval for the Activity
    (exec (set-slot-value (the-slot-value ?ai #_time:spansInterval) 
                          #_time:hasEndTime ?ti))

    (exec (output "Closed the interval spanned by ~S~%" ?ai)))

(def-rule #_trackActivityInstanceAutomaticallySkipped
    (#_evo:ActivityAutomaticallySkipped ?bame)
    (#_cobra:ActivityInstance ?ai)
    (#_cobra:concernsActivityInstance ?bame ?ai)
    (#_time:occursAt ?bame ?ti)
        
    then

    ;; Close the Time Interval for the Activity
    (exec (set-slot-value (the-slot-value ?ai #_time:spansInterval) 
                          #_time:hasEndTime ?ti))

    (exec (output "Closed the interval spanned by ~S~%" ?ai)))

(def-rule #_trackActivityInstanceWithdrawn
    (#_evo:ActivityWithdrawn ?bame)
    (#_cobra:ActivityInstance ?ai)
    (#_cobra:concernsActivityInstance ?bame ?ai)
    (#_time:occursAt ?bame ?ti)
        
    then

    ;; Close the Time Interval for the Activity
    (exec (set-slot-value (the-slot-value ?ai #_time:spansInterval) 
                          #_time:hasEndTime ?ti))

    (exec (output "Closed the interval spanned by ~S~%" ?ai)))


(def-rule #_changeBusinessActivityRealisationState
    "Whenever a Business Activity Monitoring Event is received update the state."
    (#_cobra:BusinessActivityRealisation ?bar)
    (#_cobra:BusinessActivityMonitoringEvent ?bame)
    (or (= ?bar (the-slot-value ?bame #_cobra:concernsProcessInstance))
        (= ?bar (the-slot-value ?bame #_cobra:concernsActivityInstance)))

    then
    ;;Modify the hasCurrentState slot
    (exec (set-slot-value ?bar 
                          #_cobra:hasCurrentState 
                          (the-slot-value ?bame #_cobra:leadsToState)))

    ;; Print information
    (exec (output "Updated Business Activity Realisation ~S to State ~S."
                  ?bar (the-slot-value ?bame #_cobra:leadsToState))))

(def-rule #_validateStatesTransition
    "Whenever a Business Activity Monitoring Event is received, this rule validated the transition and eventually triggers a notification. These transition anomalies will be stored for further reference."
    (#_cobra:BusinessActivityRealisation ?bar)
    (#_cobra:BusinessActivityMonitoringEvent ?bame)
    (or (= ?bar (the-slot-value ?bame #_cobra:concernsProcessInstance))
        (= ?bar (the-slot-value ?bame #_cobra:concernsActivityInstance)))

    (= ?cs (the-slot-value ?bar #_cobra:hasCurrentState))
    (= ?ns (the-slot-value ?bame #_cobra:leadsToState))
    (not (#_isNormalTransition ?cs ?ns))

    then

    ;;Trigger notification
    (exec (output "Unexpected state transition from State: ~S - to State:~S for Business Activity Realisation ~S." ?cs ?ns ?bar))
)

;;
;; Business Activity Realisations information updates
;;

(def-rule #_processComposedOfActivities
    "Whenever a Business Activity Monitoring Event concerns both a Process Instance and an Activity Instance, we can infer that the Process Instance is composedOf the Activity Instance."
    (#_cobra:ProcessInstance ?pi)
    (#_cobra:ActivityInstance ?ai)
    (#_cobra:BusinessActivityMonitoringEvent ?bame)
    (= ?pi (the-slot-value ?bame #_cobra:concernsProcessInstance))
    (= ?ai (the-slot-value ?bame #_cobra:concernsActivityInstance))

    then

    ;;Update the composedOf information
    (exec (add-slot-value ?pi #_cobra:composedOf ?ai))
    ;;Trigger a message notification
    (exec (output "Activity Instance ~S is executed as part of Process Instance ~S" 
                  ?ai ?pi)))

#|
(def-rule #_agentPlaysRoleInBusinessActivityRealisation
    "Whenever a Business Activity requires a particular Agentive Role Type and only one of the involved Agents canPlay the given Role, then the Role has been played by that Actor"

    (#_cobra:BusinessActivity ?ba)
    (#_cobra:BusinessActivityRealisation ?bar)
    (#_cobra:RoleType ?roleType)
    (subclass-of ?roleType #_cobra:AgentiveRole)

    (= ?ba (the-sot-value ?bar #_cobra:performs))
    (exists ?agent (and (#_cobra:Agent ?agent)
                        (has-slot-value ?agent #_cobra:canPlayRole ?roleType)
                        (#_cobra:businessActivityRealisationInvolvesAgent ?bar ?agent)

                        (not exists ?another (and (<> ?another ?agent)
                                                  (#_cobra:Agent ?another)
                                                  (has-slot-value ?another #_cobra:canPlayRole ?roleType)
                                                  (#_cobra:businessActivityRealisationInvolvesAgent ?bar ?another)))))

    then
    ;; Assert the fact
    (exec (tell (#_cobra:playsRoleIn ?agent 
                                     (new-instance ?roleType) 
                                     ?bar)))

    ;; Trigger notification
    (exec (output "Agent ~S plays role of kind ~S in Business Activity Realisation ~S"
                  ?agent ?roleType ?bar)))
|#






