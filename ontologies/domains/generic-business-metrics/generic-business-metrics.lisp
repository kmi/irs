;;; Mode: Lisp; Package: ocml

;; Generic Business Metrics Ontology
;; This ontology captures a set of general purpose domain-independent business metrics
;; that are generally relevant for the analysis of business processes independently
;; from the kind of activity they perform
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 1.0

(in-package "OCML")

(in-ontology generic-business-metrics)
;;;;;;;;
;; COUNT
;;;;;;;;
;;  State Based Metrics
;; Open
(def-instance #_NumberOfAIsOpen #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsOpen)))

(def-instance #_NumberOfPIsOpen #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsOpen)))

(def-instance #_NumberOfBARsOpen #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsOpen)))

;; Not Started
(def-instance #_NumberOfAIsNotStarted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsNotStarted)))

(def-instance #_NumberOfPIsNotStarted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsNotStarted)))

(def-instance #_NumberOfBARsNotStarted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsNotStarted)))

;; Scheduled
(def-instance #_NumberOfAIsScheduled #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsScheduled)))

;; Assigned
(def-instance #_NumberOfAIsAssigned #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsAssigned)))

;; Ready
(def-instance #_NumberOfPIsReady #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsReady)))

;; Started
(def-instance #_NumberOfAIsStarted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsStarted)))

(def-instance #_NumberOfPIsStarted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsStarted)))

(def-instance #_NumberOfBARsStarted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsStarted)))

;; Running
(def-instance #_NumberOfAIsRunning #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsRunning)))

(def-instance #_NumberOfPIsRunning #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsRunning)))

(def-instance #_NumberOfBARsRunning #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsRunning)))

;; Suspended
(def-instance #_NumberOfAIsSuspended #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsSuspended)))

(def-instance #_NumberOfPIsSuspended #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsSuspended)))

(def-instance #_NumberOfBARsSuspended #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsSuspended)))

;; Closed
(def-instance #_NumberOfAIsClosed #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsClosed)))

(def-instance #_NumberOfPIsClosed #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsClosed)))

(def-instance #_NumberOfBARsClosed #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsClosed)))

;; Unsuccessfully Finished
(def-instance #_NumberOfAIsUnsuccessfullyFinished #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsUnsuccessfullyFinished)))

(def-instance #_NumberOfPIsUnsuccessfullyFinished #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsUnsuccessfullyFinished)))

(def-instance #_NumberOfBARsUnsuccessfullyFinished #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsUnsuccessfullyFinished)))

;; 
(def-instance #_NumberOfAIsAborted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsAborted)))

(def-instance #_NumberOfPIsAborted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsAborted)))

(def-instance #_NumberOfBARsAborted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsAborted)))

;; Terminated
(def-instance #_NumberOfAIsTerminated #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsTerminated)))

(def-instance #_NumberOfPIsTerminated #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsTerminated)))

(def-instance #_NumberOfBARsTerminated #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsTerminated)))

;; Successfully Finished
(def-instance #_NumberOfAIsSuccessfullyFinished #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsSuccessfullyFinished)))

(def-instance #_NumberOfPIsSuccessfullyFinished #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsSuccessfullyFinished)))

(def-instance #_NumberOfBARsSuccessfullyFinished #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsSuccessfullyFinished)))

;; Completed
(def-instance #_NumberOfAIsCompleted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichAIsCompleted)))

(def-instance #_NumberOfPIsCompleted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichPIsCompleted)))

(def-instance #_NumberOfBARsCompleted #_metrics:Count
  ((#_metrics:hasPopulationFilter #_gbq:whichBARsCompleted)))

;;;;;;;;;
;; Ratios
;;;;;;;;;
(def-instance #_OverallPIsSuccessRatio #_metrics:Ratio
  ((#_metrics:hasDividend #_NumberOfPIsSuccessfullyFinished)
   (#_metrics:hasDivisor #_NumberOfPIsUnsuccessfullyFinished)))

(def-instance #_OverallAIsSuccessRatio #_metrics:Ratio
  ((#_metrics:hasDividend #_NumberOfAIsSuccessfullyFinished)
   (#_metrics:hasDivisor #_NumberOfAIsUnsuccessfullyFinished)))

(def-instance #_OverallBARsInstancesSuccessRatio #_metrics:Ratio
  ((#_metrics:hasDividend #_NumberOfBARsSuccessfullyFinished)
   (#_metrics:hasDivisor #_NumberOfBARsUnsuccessfullyFinished)))

;;;;;;;;;;;;;;;;;;
;; Execution Time
;;;;;;;;;;;;;;;;;;
(def-class #_BARExecutionTime (#_metrics:FunctionMetric)
  ((has-input-role :value #_hasBusinessActivityRealisation)
   (#_hasBusinessActivityRealisation :type #_cobra:BusinessActivityRealisation)
   (#_metrics:hasComputationExpression :value
                                       '(lambda (?fm)
                                          (in-environment 
                                           ((?bar . (the-slot-value ?fm #_hasBusinessActivityRealisation))
                                            (?result . (#_time:getDuration ?bar)))
                                           ?result)))))

;;
;; Unspecified Business Activity Execution Time instance for using it within Aggregations 
;;
(def-instance #_DefaultBARExecutionTimeInstance #_BARExecutionTime)

;;
;; Some Execution Time Aggregation Metrics
;;
(def-instance #_AverageClosedBARsExecutionTime #_metrics:Average
"Overall Average Execution Time for Business Activity Realisations"
  ((#_metrics:hasFunctionMetric #_DefaultBARExecutionTimeInstance)
   (#_metrics:hasUnboundRole #_hasBusinessActivityRealisation)
   (#_metrics:hasPopulationFilter #_gbq:whichBARsClosed)))

(def-instance #_MaximumClosedBARsExecutionTime #_metrics:Maximum
  "Overall Maximum Execution Time for Business Activity Realisations"
  ((#_metrics:hasFunctionMetric #_DefaultBARExecutionTimeInstance)
   (#_metrics:hasUnboundRole #_hasBusinessActivityRealisation)
   (#_metrics:hasPopulationFilter #_gbq:whichBARsClosed)))

(def-instance #_MinimumClosedBARsExecutionTime #_metrics:Minimum
  "Overall Minimum Execution Time for Business Activity Realisations"
  ((#_metrics:hasFunctionMetric #_DefaultBARExecutionTimeInstance)
   (#_metrics:hasUnboundRole #_hasBusinessActivityRealisation)
   (#_metrics:hasPopulationFilter #_gbq:whichBARsClosed)))