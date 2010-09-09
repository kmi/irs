;;
;; Last tests
;;

(in-package "OCML")
(in-ontology execution-history)

(def-instance #_inputDataValue31013eb4d713 #_COBRA:DataValue
(( #_COBRA:hasParameter |http://www.ip-super.org/processes/sbpel/sahelloworld#myVar|  )
( #_COBRA:hasValue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<message><TestPart>Hello World</TestPart></message>"  )
))

(def-instance #_startactivityInstance4 #_COBRA:ActivityInstance
(( #_COBRA:performs |http://www.ip-super.org/processes/sbpel/sahelloworld#receive|  )
))

(def-instance #_event3101258de636 #_ActivityStarted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:hasData #_inputDataValue31012592311f  )
( #_COBRA:concernsActivityInstance #_startactivityInstance4  )
( |http://www.ip-super.org/ontologies/time-ontology/20080612#occursAt| #_instant1563660  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_event31012be24eb5 #_ActivityStarted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:concernsActivityInstance #_assign1activityInstance8  )
( |http://www.ip-super.org/ontologies/time-ontology/20080612#occursAt| #_instant15497902  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant9813965 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 12  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 9  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 52  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 34  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 313  )
))

(def-instance #_event31012248c00b #_ActivityStarted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:concernsActivityInstance |http://www.example.org/anOntology#sequence-activity-line-46sequence-activity-line-46activityInstance2|  )
( |http://www.ip-super.org/ontologies/time-ontology/20080612#occursAt| #_instant15458568  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant1173485 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 12  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 9  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 52  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 34  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 318  )
))

(def-instance #_instant15440755 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 12  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 9  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 52  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 34  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 319  )
))

(def-instance #_event31013c9ffe48 #_ActivityCompleted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:concernsActivityInstance #_assign1activityInstance8  )
( |http://www.ip-super.org/ontologies/time-ontology/20080612#occursAt| #_instant7767573  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant10755279 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 12  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 9  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 52  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 34  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 319  )
))

(def-instance |http://www.example.org/anOntology#sequence-activity-line-46sequence-activity-line-46activityInstance2| #_COBRA:ActivityInstance
(( #_COBRA:performs |http://ode/bpel/unit-test#sequence-activity-line-46|  )
))

(def-instance #_event3101154aa72b #_ProcessStarted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( |http://www.ip-super.org/ontologies/time-ontology/20080612#occursAt| #_instant12911683  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_assign1activityInstance8 #_COBRA:ActivityInstance
(( #_COBRA:performs |http://www.ip-super.org/processes/sbpel/sahelloworld#composeHelloWorldMessage|  )
))

(def-instance #_inputDataValue31013d002ed1 #_COBRA:DataValue
(( #_COBRA:hasParameter |http://www.ip-super.org/processes/sbpel/sahelloworld#myVar|  )
( #_COBRA:hasValue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<message><TestPart>Hello World</TestPart></message>"  )
))

(def-instance #_instant15497902 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 12  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 9  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 52  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 34  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 314  )
))

(def-instance #_endactivityInstance10 #_COBRA:ActivityInstance
(( #_COBRA:performs |http://www.ip-super.org/processes/sbpel/sahelloworld#reply|  )
))

(def-instance #_inputDataValue31012592311f #_COBRA:DataValue
(( #_COBRA:hasParameter |http://www.ip-super.org/processes/sbpel/sahelloworld#myVar|  )
))

(def-instance #_event31014122a6a9 #_ProcessCompleted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( |http://www.ip-super.org/ontologies/time-ontology/20080612#occursAt| #_instant10755279  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant12911683 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 12  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 9  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 52  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 33  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 307  )
))

(def-instance #_instant15458568 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 12  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 9  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 52  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 34  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 309  )
))

(def-instance #_event31013eb181b2 #_ActivityCompleted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:hasData #_inputDataValue31013eb4d713  )
( #_COBRA:concernsActivityInstance #_endactivityInstance10  )
( |http://www.ip-super.org/ontologies/time-ontology/20080612#occursAt| #_instant1173485  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant4433536 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 12  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 9  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 52  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 34  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 317  )
))

(def-instance #_SAHelloWorldprocessInstance0 #_COBRA:ProcessInstance
(( #_COBRA:performs |http://www.ip-super.org/processes/sbpel/sahelloworld#process|  )
))

(def-instance #_event31013cda89cd #_ActivityStarted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:hasData #_inputDataValue31013d002ed1  )
( #_COBRA:concernsActivityInstance #_endactivityInstance10  )
( |http://www.ip-super.org/ontologies/time-ontology/20080612#occursAt| #_instant4433536  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_event31012af9763f #_ActivityCompleted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:hasData #_inputDataValue31012b4fd590  )
( #_COBRA:concernsActivityInstance #_startactivityInstance4  )
( |http://www.ip-super.org/ontologies/time-ontology/20080612#occursAt| #_instant9813965  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_event31013fc088a9 #_ActivityCompleted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:concernsActivityInstance |http://www.example.org/anOntology#sequence-activity-line-46sequence-activity-line-46activityInstance2|  )
( |http://www.ip-super.org/ontologies/time-ontology/20080612#occursAt| #_instant15440755  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant7767573 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 12  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 9  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 52  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 34  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 315  )
))

(def-instance #_inputDataValue31012b4fd590 #_COBRA:DataValue
(( #_COBRA:hasParameter |http://www.ip-super.org/processes/sbpel/sahelloworld#myVar|  )
( #_COBRA:hasValue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<message><TestPart>Hello</TestPart></message>"  )
))

(def-instance #_instant1563660 |http://kmi.open.ac.uk/ontologies/time-ontology#TimeInstant|
(( |http://kmi.open.ac.uk/ontologies/time-ontology#yearOf| 2008  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#monthOf| 5  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#dayOf| 12  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#hourOf| 9  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#minuteOf| 52  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#secondOf| 34  )
( |http://kmi.open.ac.uk/ontologies/time-ontology#millisecondOf| 311  )
))

