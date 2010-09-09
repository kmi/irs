;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")
(in-ontology execution-history)

;; Ontology Classes


;; Ontology Relations


;; Ontology Functions


;; Ontology Instances

(def-instance #_instant658160348 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 4  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 20  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 3  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 39  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 336  )
))

(def-instance #_event1f886559b63b2 #_EVO:ActivityCompleted
(( #_COBRA:concernsProcessInstance |http://www.ip-super.org/ontologies/unknown#FulfilmentprocessInstance0|  )
( #_COBRA:hasData |http://www.ip-super.org/ontologies/unknown#inputDataValue1f88655b19b66|  )
( #_COBRA:concernsActivityInstance |http://www.ip-super.org/ontologies/unknown#ReceiveAccountRequestactivityInstance4|  )
( #_TIME:occursAt #_instant1026563844  )
( #_COBRA:generatedBy |http://www.ip-super.org/ontologies/unknown#SBPELEE|  )
))

(def-instance #_instant1651672899 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 4  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 20  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 3  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 42  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 345  )
))

(def-instance |http://www.ip-super.org/ontologies/unknown#sequence-activity-line-54sequence-activity-line-54activityInstance2| #_COBRA:ActivityInstance
(( #_COBRA:performs |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#sequence-activity-line-54|  )
))

(def-instance #_event1f886503a3fcf #_EVO:ActivityStarted
(( #_COBRA:concernsProcessInstance |http://www.ip-super.org/ontologies/unknown#FulfilmentprocessInstance0|  )
( #_COBRA:concernsActivityInstance |http://www.ip-super.org/ontologies/unknown#sequence-activity-line-54sequence-activity-line-54activityInstance2|  )
( #_TIME:occursAt #_instant660544576  )
( #_COBRA:generatedBy |http://www.ip-super.org/ontologies/unknown#SBPELEE|  )
))

(def-instance |http://www.ip-super.org/ontologies/unknown#inputDataValue1f88655b19b66| #_COBRA:DataValue
(( #_COBRA:hasParameter |http://www.ip-super.org/ontologies/unknown#SendAccountInfoIn|  )
( #_COBRA:hasValue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<message><Account><sal:Account xmlns:sal=\"http://logos.intern.etel.at/schema/salesDepartment\" xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\">
         <sal:data>test</sal:data>
         <sal:id>test</sal:id>
      </sal:Account></Account></message>"  )
))

(def-instance #_event1f8866853bfa0 #_EVO:ActivityCompleted
(( #_COBRA:concernsProcessInstance |http://www.ip-super.org/ontologies/unknown#FulfilmentprocessInstance0|  )
( #_COBRA:concernsActivityInstance |http://www.ip-super.org/ontologies/unknown#InitactivityInstance8|  )
( #_TIME:occursAt #_instant1651672899  )
( #_COBRA:generatedBy |http://www.ip-super.org/ontologies/unknown#SBPELEE|  )
))

(def-instance |http://www.ip-super.org/ontologies/unknown#ReceiveAccountRequestactivityInstance4| #_COBRA:ActivityInstance
(( #_COBRA:performs |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#ReceiveAccountRequest|  )
))

(def-instance #_event1f8865264cd51 #_EVO:ActivityStarted
(( #_COBRA:concernsProcessInstance |http://www.ip-super.org/ontologies/unknown#FulfilmentprocessInstance0|  )
( #_COBRA:hasData |http://www.ip-super.org/ontologies/unknown#inputDataValue1f886527222d0|  )
( #_COBRA:concernsActivityInstance |http://www.ip-super.org/ontologies/unknown#ReceiveAccountRequestactivityInstance4|  )
( #_TIME:occursAt #_instant1943291419  )
( #_COBRA:generatedBy |http://www.ip-super.org/ontologies/unknown#SBPELEE|  )
))

(def-instance #_instant660544576 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 4  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 20  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 3  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 40  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 339  )
))

(def-instance |http://www.ip-super.org/ontologies/unknown#InitactivityInstance8| #_COBRA:ActivityInstance
(( #_COBRA:performs |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Init|  )
))

(def-instance #_event1f886577e2093 #_EVO:ActivityStarted
(( #_COBRA:concernsProcessInstance |http://www.ip-super.org/ontologies/unknown#FulfilmentprocessInstance0|  )
( #_COBRA:concernsActivityInstance |http://www.ip-super.org/ontologies/unknown#InitactivityInstance8|  )
( #_TIME:occursAt #_instant1350215963  )
( #_COBRA:generatedBy |http://www.ip-super.org/ontologies/unknown#SBPELEE|  )
))

(def-instance #_instant1026563844 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 4  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 20  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 3  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 40  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 343  )
))

(def-instance |http://www.ip-super.org/ontologies/unknown#inputDataValue1f886527222d0| #_COBRA:DataValue
(( #_COBRA:hasParameter |http://www.ip-super.org/ontologies/unknown#SendAccountInfoIn|  )
))

(def-instance #_event1f88640e4fd4f #_EVO:ProcessStarted
(( #_COBRA:concernsProcessInstance |http://www.ip-super.org/ontologies/unknown#FulfilmentprocessInstance0|  )
( #_TIME:occursAt #_instant658160348  )
( #_COBRA:generatedBy |http://www.ip-super.org/ontologies/unknown#SBPELEE|  )
))

(def-instance |http://www.ip-super.org/ontologies/unknown#FulfilmentprocessInstance0| #_COBRA:ProcessInstance
(( #_COBRA:performs |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Fulfilment|  )
))

(def-instance #_instant1350215963 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 4  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 20  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 3  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 41  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 344  )
))

(def-instance #_instant1943291419 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 4  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 20  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 3  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 40  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 341  )
))






