;;; Mode: Lisp; Package: ocml

;; Created by Carlos Pedrinaci 
;; Core Ontology for Business pRocess Analysis
;; Inspired by DOLCE, Enterprise Ontology, MXML, TOVE

;;
;; TESTS FILE
;; 

(in-package "OCML")

(in-ontology cobra)

(def-instance #_TimeInstant-t0 #_time:TimeInstant
  ((#_time:microsecondOf 10))
)

(def-instance #_Carlos #_Person)

(def-instance #_proc1 #_Process)

(def-instance #_procInst1-1 #_ProcessInstance
  ((#_performs #_proc1)))

(def-instance #_procInst1-2 #_ProcessInstance
  ((#_performs #_proc1)))

(def-instance #_procInst1-3 #_ProcessInstance
  ((#_performs #_proc1)))

(def-instance #_procInst1-4 #_ProcessInstance
  ((#_performs #_proc1)))

(def-instance #_procInst1-5 #_ProcessInstance
  ((#_performs #_proc1)))

(def-instance #_proc2 #_Process)

(def-instance #_procInst2-1 #_ProcessInstance
  ((#_performs #_proc2)))

;;(holds? '#_followed-by '#_TimeInterval-I '#_TimeInterval-F '#_TimeInterval)

;; (setofall '?x '(ocml::instance-of ?x #_PersistentEntityType))

;; (setofall '?x '(ocml::instance-of ?x #_RoleType))

;;(holds? '#_businessActivityRealisationPerforms '#_procInst1-2 '#_proc1)
