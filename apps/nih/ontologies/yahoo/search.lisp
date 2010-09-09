;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs

(in-package #:ocml)

(in-ontology yahoo)

;;; {{{ Goal definitions
(def-class #_webSearchGoal (goal) ?goal
    ((has-input-role :value #_hasQuery)
     (has-output-role :value #_hasResultSet)
     (has-input-soap-binding :value (#_hasQuery "string"))
     (has-output-soap-binding :value (#_hasResultSet "string"))
     (#_hasQuery :type string)
     (#_hasResultSet :type string)))
;;; }}}

;;; {{{ Grounding
(def-relation application-id (?id)
  :iff-def (= ?id "9_cFTSPV34FNhnTqZZw4_nAuoZ2qvHYW0OSaGMc208MeUVFeUQDTh.R3dd.nfG_3QQ--"))

(def-rule #_lower-for-webSearchService
    ((#_hg:lower #_webSearchService ?invocation ?http-request)
     if
     (= ?rawquery (wsmo-role-value ?invocation #_hasQuery))
     (#_rfc2616:url-encoding ?rawquery ?query)
     (application-id ?app-id)
     (= ?base-url "http://search.yahooapis.com/WebSearchService/V1/webSearch")
     (= ?url (make-string "~A?appid=~A&query=~A" ?base-url ?app-id ?query))
     (#_rfc2616:set-url ?http-request ?url)
     (#_rfc2616:set-method ?http-request "GET")))

(def-rule #_lift-for-webSearchService
    ((#_hg:lift #_webSearchService ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (set-goal-slot-value ?invocation #_hasResultSet ?http-content)
     (#_rfc2616:header-value ?http-request "Content-Type" ?content-type)
     (set-goal-slot-value ?invocation #_hasContentType ?content-type)))
;;; }}}

;;; {{{ #_webSearchService
(def-class #_webSearch-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class #_webSearch-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_webSearchGoal)
     (has-non-functional-properties
      :value #_webSearch-mediator-non-functional-properties)))

(def-class #_webSearchService (web-service) ?web-service
    ((has-capability :value #_webSearchService-capability)
     (has-interface :value #_webSearchService-interface)
     (has-non-functional-properties
      :value #_webSearchService-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class #_webSearchService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_webSearchService-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_webSearchService-capability
           (capability)
           ?capability
           ((used-mediator :value #_webSearch-mediator)
            (has-non-functional-properties
             :value
             #_webSearchService-capability-non-functional-properties)))

(def-class #_webSearchService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_webSearchService-interface-choreography
           (choreography)
           ((has-grounding :value ((grounded-to-rest)))
            (has-earthing :value #_webSearchService-grounding)))

(def-instance #_webSearchService-grounding rest-grounding
  ())

(def-class #_webSearchService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_webSearchService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_webSearchService-interface-orchestration-problem-solving-pattern)))

(def-class #_webSearchService-interface (interface) ?interface
    ((has-choreography :value #_webSearchService-interface-choreography)
     (has-orchestration :value #_webSearchService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_webSearchService-interface-non-functional-properties)))

(def-class #_webSearchService-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value #_webSearchService-interface)
     (has-web-service-host :value "yahoo.com")
     (has-web-service-port :value 80)))

;;; }}}
