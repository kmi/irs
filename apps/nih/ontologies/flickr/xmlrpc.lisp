;;; Copyright © 2010 The Open University

(in-package #:ocml)

(in-ontology flickr)

;;; {{{ #_photosGetSizesXmlrpcService

;; Return a list sizes a photo is available in.

;; api_key (Required)
;;     Your API application key. See here for more details.
;; photo_id (Required)

(def-class #_photosGetSizesXmlrpcGoal (goal) ?goal
    ((has-input-role :value #_hasAccount
		     :value #_hasPhoto
		     :value #_hasToken)
     (has-output-role :value #_hasPhotoSizes)
     (#_hasAccount :type #_Account)
     (#_hasToken :type #_Token)
     (#_hasPhoto :type #_Photo)
     (#_hasPhotoSizes :type #_PhotoSizeList)))

(def-class #_photosGetSizesXmlrpc-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class #_photosGetSizesXmlrpc-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_photosGetSizesXmlrpcGoal)
     (has-non-functional-properties
      :value #_photosGetSizesXmlrpc-mediator-non-functional-properties)))

(def-class #_photosGetSizesXmlrpcService (web-service) ?web-service
    ((has-capability :value #_photosGetSizesXmlrpcService-capability)
     (has-interface :value #_photosGetSizesXmlrpcService-interface)
     (has-non-functional-properties
      :value #_photosGetSizesXmlrpcService-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class #_photosGetSizesXmlrpcService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_photosGetSizesXmlrpcService-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_photosGetSizesXmlrpcService-capability
           (capability)
           ?capability
           ((used-mediator :value #_photosGetSizesXmlrpc-mediator)
            (has-non-functional-properties
             :value
             #_photosGetSizesXmlrpcService-capability-non-functional-properties)))

(def-class #_photosGetSizesXmlrpcService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_photosGetSizesXmlrpcService-interface-choreography
           (choreography)
           ((has-grounding :value ((grounded-to-rest)))
            (has-earthing :value #_photosGetSizesXmlrpcService-grounding)))

(def-instance #_photosGetSizesXmlrpcService-grounding rest-grounding
  ())

(def-class #_photosGetSizesXmlrpcService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_photosGetSizesXmlrpcService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_photosGetSizesXmlrpcService-interface-orchestration-problem-solving-pattern)))

(def-class #_photosGetSizesXmlrpcService-interface (interface) ?interface
    ((has-choreography :value #_photosGetSizesXmlrpcService-interface-choreography)
     (has-orchestration :value #_photosGetSizesXmlrpcService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_photosGetSizesXmlrpcService-interface-non-functional-properties)))

(def-class #_photosGetSizesXmlrpcService-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value #_photosGetSizesXmlrpcService-interface)
     (has-web-service-host :value "yahoo.com")
     (has-web-service-port :value 80)))

;;; {{{ Lifting and lowering
(def-rule #_lower-for-photosGetSizesXmlrpcService
    ((#_grnd:lower #_photosGetSizesXmlrpcService ?invocation ?http-request) if
     (#_argsForPhotosGetSizes ?invocation ?args)
     (= ?account (wsmo-role-value ?invocation #_hasAccount))
     (#_signArguments #_xmlrpc ?args ?account)
     (#_argsToXmlrpcRequest ?args ?http-request)))

(def-rule #_lift-for-photosGetSizesXmlrpcService
    ((#_grnd:lift #_photosGetSizesXmlrpcService ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (#_xml:serialiseXml ?xml ?http-content)
     (#_getTheXmlrpcValue ?xml ?value)
     (#_xmllyPhotoSizeList ?value ?photosizelist)
     (set-goal-slot-value ?invocation #_hasPhotoSizes ?photosizelist)))

;;; }}}

;;; }}}

;;; {{{ General support for how Flickr XML-RPC
(def-rule #_argsToXmlrpcRequest
    ((#_argsToXmlrpcRequest ?args ?http-request) if
     (#_getArgument ?args "method" ?method)
     (= ?nonmethodargs
	(setofall ?member
		  (and (#_hasArgument ?args ?arg)
		       (#_hasName ?arg ?name)
		       (not (= ?name "method"))
		       (#_hasValue ?arg ?value)
		       (= ?member (#_xmlrpc:Member ?name (#_xmlrpc:String ?value))))))
     (= ?xmlrpc
        (#_xmlrpc:MethodCall ?method (#_xmlrpc:Param (#_xmlrpc:Struct ?nonmethodargs))))
     (#_xmlrpc:mapToXml ?xmlrpc ?xmlmodel)
     (#_xml:serialiseXml ?xmlmodel ?xmlstring)
     (#_rfc2616:set-content ?http-request ?xmlstring)
     (#_rfc2616:set-method ?http-request "POST")
     (#_rfc2616:set-url ?http-request "http://api.flickr.com/services/xmlrpc/")))

(def-rule #_getTheXmlrpcValue
    "Extract the first <value><string>...</></> thing.
The processing after that point can be shared with the RESTful
version."
  ((#_getTheXmlrpcValue ?xml-model ?sizes) if
   (#_xml:rootElement ?xml-model ?root)
   (#_xml:elementByName ?root ?string "string")
   (#_xml:contents ?string ?contents)
   (member ?content ?contents)
   (#_xml:value ?content ?val)
   (#_xml:serialiseXml ?inner ?val)
   (#_xml:rootElement ?inner ?inner-root)
   (#_xml:elementByName ?inner-root ?sizes "sizes")))

;;; }}}
