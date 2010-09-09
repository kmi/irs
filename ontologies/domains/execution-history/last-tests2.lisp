

(in-package "OCML")
(in-ontology execution-history)

(def-instance #_inputDataValue33f8e6b429a8 #_COBRA:DataValue
(( #_COBRA:hasParameter |http://www.ip-super.org/processes/sbpel/sahelloworld#myVar|  )
( #_COBRA:hasValue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<message><TestPart>Hello World</TestPart></message>"  )
))

(def-instance #_sequence-activity-line-46sequence-activity-line-46activityInstance2 #_COBRA:ActivityInstance
(( #_COBRA:performs |http://ode/bpel/unit-test#sequence-activity-line-46|  )
))

(def-instance #_startactivityInstance4 #_COBRA:ActivityInstance
(( #_COBRA:performs |http://www.ip-super.org/processes/sbpel/sahelloworld#receive|  )
))

(def-instance #_event33f8de603347 #_EVO:ActivityCompleted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:hasData #_inputDataValue33f8de90bfc2  )
( #_COBRA:concernsActivityInstance #_startactivityInstance4  )
( #_TIME:occursAt #_instant15497902  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_event33f8e6aac366 #_EVO:ActivityStarted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:hasData #_inputDataValue33f8e6b429a8  )
( #_COBRA:concernsActivityInstance #_endactivityInstance10  )
( #_TIME:occursAt #_instant1932245  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant9813965 #_TIME:TimeInstant
(( #_TIME:yearOf 2008  )
( #_TIME:monthOf 5  )
( #_TIME:dayOf 12  )
( #_TIME:hourOf 10  )
( #_TIME:minuteOf 46  )
( #_TIME:secondOf 57  )
( #_TIME:millisecondOf 953  )
))

(def-instance #_event33f8c80687eb #_EVO:ProcessStarted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_TIME:occursAt #_instant9871902  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_event33f8e846258a #_EVO:ActivityCompleted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:concernsActivityInstance #_sequence-activity-line-46sequence-activity-line-46activityInstance2  )
( #_TIME:occursAt #_instant6253254  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant1932245 #_TIME:TimeInstant
(( #_TIME:yearOf 2008  )
( #_TIME:monthOf 5  )
( #_TIME:dayOf 12  )
( #_TIME:hourOf 10  )
( #_TIME:minuteOf 46  )
( #_TIME:secondOf 57  )
( #_TIME:millisecondOf 960  )
))

(def-instance #_instant6088209 #_TIME:TimeInstant
(( #_TIME:yearOf 2008  )
( #_TIME:monthOf 5  )
( #_TIME:dayOf 12  )
( #_TIME:hourOf 10  )
( #_TIME:minuteOf 46  )
( #_TIME:secondOf 57  )
( #_TIME:millisecondOf 957  )
))

(def-instance #_event33f8e7efbc63 #_EVO:ActivityCompleted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:hasData #_inputDataValue33f8e7f2f9c2  )
( #_COBRA:concernsActivityInstance #_endactivityInstance10  )
( #_TIME:occursAt #_instant2799670  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant9871902 #_TIME:TimeInstant
(( #_TIME:yearOf 2008  )
( #_TIME:monthOf 5  )
( #_TIME:dayOf 12  )
( #_TIME:hourOf 10  )
( #_TIME:minuteOf 46  )
( #_TIME:secondOf 56  )
( #_TIME:millisecondOf 950  )
))

(def-instance #_inputDataValue33f8de90bfc2 #_COBRA:DataValue
(( #_COBRA:hasParameter |http://www.ip-super.org/processes/sbpel/sahelloworld#myVar|  )
( #_COBRA:hasValue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<message><TestPart>Hello</TestPart></message>"  )
))

(def-instance #_instant15770267 #_TIME:TimeInstant
(( #_TIME:yearOf 2008  )
( #_TIME:monthOf 5  )
( #_TIME:dayOf 12  )
( #_TIME:hourOf 10  )
( #_TIME:minuteOf 46  )
( #_TIME:secondOf 57  )
( #_TIME:millisecondOf 958  )
))

(def-instance #_instant15497902 #_TIME:TimeInstant
(( #_TIME:yearOf 2008  )
( #_TIME:monthOf 5  )
( #_TIME:dayOf 12  )
( #_TIME:hourOf 10  )
( #_TIME:minuteOf 46  )
( #_TIME:secondOf 57  )
( #_TIME:millisecondOf 955  )
))

(def-instance #_event33f8df2cf216 #_EVO:ActivityStarted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:concernsActivityInstance #_assign1activityInstance8  )
( #_TIME:occursAt #_instant6088209  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant1731527 #_TIME:TimeInstant
(( #_TIME:yearOf 2008  )
( #_TIME:monthOf 5  )
( #_TIME:dayOf 12  )
( #_TIME:hourOf 10  )
( #_TIME:minuteOf 46  )
( #_TIME:secondOf 57  )
( #_TIME:millisecondOf 962  )
))

(def-instance #_instant5953524 #_TIME:TimeInstant
(( #_TIME:yearOf 2008  )
( #_TIME:monthOf 5  )
( #_TIME:dayOf 12  )
( #_TIME:hourOf 10  )
( #_TIME:minuteOf 46  )
( #_TIME:secondOf 56  )
( #_TIME:millisecondOf 951  )
))

(def-instance #_instant6253254 #_TIME:TimeInstant
(( #_TIME:yearOf 2008  )
( #_TIME:monthOf 5  )
( #_TIME:dayOf 12  )
( #_TIME:hourOf 10  )
( #_TIME:minuteOf 46  )
( #_TIME:secondOf 57  )
( #_TIME:millisecondOf 961  )
))

(def-instance #_SAHelloWorldprocessInstance0 #_COBRA:ProcessInstance
(( #_COBRA:performs |http://www.ip-super.org/processes/sbpel/sahelloworld#process|  )
))

(def-instance #_inputDataValue33f8e7f2f9c2 #_COBRA:DataValue
(( #_COBRA:hasParameter |http://www.ip-super.org/processes/sbpel/sahelloworld#myVar|  )
( #_COBRA:hasValue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<message><TestPart>Hello World</TestPart></message>"  )
))

(def-instance #_event33f8e64f2059 #_EVO:ActivityCompleted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:concernsActivityInstance #_assign1activityInstance8  )
( #_TIME:occursAt #_instant15770267  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_endactivityInstance10 #_COBRA:ActivityInstance
(( #_COBRA:performs |http://www.ip-super.org/processes/sbpel/sahelloworld#reply|  )
))

(def-instance #_event33f8d783cc04 #_EVO:ActivityStarted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:hasData #_inputDataValue33f8d788297b  )
( #_COBRA:concernsActivityInstance #_startactivityInstance4  )
( #_TIME:occursAt #_instant9813965  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_instant2799670 #_TIME:TimeInstant
(( #_TIME:yearOf 2008  )
( #_TIME:monthOf 5  )
( #_TIME:dayOf 12  )
( #_TIME:hourOf 10  )
( #_TIME:minuteOf 46  )
( #_TIME:secondOf 57  )
( #_TIME:millisecondOf 961  )
))

(def-instance #_event33f8ea90b9c3 #_EVO:ProcessCompleted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_TIME:occursAt #_instant1731527  )
( #_COBRA:generatedBy #_SBPELEE  )
))

(def-instance #_assign1activityInstance8 #_COBRA:ActivityInstance
(( #_COBRA:performs |http://www.ip-super.org/processes/sbpel/sahelloworld#composeHelloWorldMessage|  )
))

(def-instance #_inputDataValue33f8d788297b #_COBRA:DataValue
(( #_COBRA:hasParameter |http://www.ip-super.org/processes/sbpel/sahelloworld#myVar|  )
))

(def-instance #_event33f8d5695a69 #_EVO:ActivityStarted
(( #_COBRA:concernsProcessInstance #_SAHelloWorldprocessInstance0  )
( #_COBRA:concernsActivityInstance #_sequence-activity-line-46sequence-activity-line-46activityInstance2  )
( #_TIME:occursAt #_instant5953524  )
( #_COBRA:generatedBy #_SBPELEE  )
))

