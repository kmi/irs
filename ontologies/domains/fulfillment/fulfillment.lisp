;;
;; Additional instances for the review
;;

(in-package "OCML")
(in-ontology fulfillment)
	
(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#sequence-activity-line-63| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#ReceiveAccountRequest| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Init| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Flow1| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Sequence1| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Sequence2| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#InvokeUsernameCheck| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#If1| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#InvokeDNSService| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#InvokeADSLRouterProcurement| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#InvokeSIPService| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#If2| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#InvokeGetParameters| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#PrepareResponse| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#ReplyAccountInfo| #_COBRA:Activity
())

(def-instance |http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Fulfilment| #_COBRA:Process
	((#_COBRA:composedOf
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#sequence-activity-line-63|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#ReceiveAccountRequest|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Init|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#sequence-activity-line-63|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#ReceiveAccountRequest|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Init|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Flow1|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Sequence1|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#Sequence2|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#InvokeUsernameCheck|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#If1|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#InvokeDNSService|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#InvokeADSLRouterProcurement|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#InvokeSIPService|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#If2|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#InvokeGetParameters|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#PrepareResponse|
	|http://ip-super.org/etel/bpel/Fulfilment/Fulfilment#ReplyAccountInfo|))
)