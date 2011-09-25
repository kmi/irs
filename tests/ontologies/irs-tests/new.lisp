;;; Copyright © 2008 The Open University

(in-package #:ocml)

(in-ontology irs-tests)

(def-class ocml-query-goal (goal) ?goal
    ((has-input-role :value has-ontology
                     :value has-query
                     :value has-format)
     (has-input-soap-binding :value (has-ontology "string")
                             :value (has-query "string")
                             :value (has-format "string"))
     (has-output-role :value has-query-result)
     (has-output-soap-binding :value (has-query-result "xml"))
     (has-ontology :type string)
     (has-query :type string)
     (has-query-result :type string)
     (has-format :type string)
     (has-non-functional-properties :value invoke-http-service-goal-non-functional-properties)))

(def-class ocml-query-goal-non-functional-properties (non-functional-properties)
    ((has-description "Testing HTTP grounding.")))
;;; }}}
;;; {{{ ocml-query-web-service
(def-class ocml-query-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class ocml-query-mediator (wg-mediator) ?mediator
    ((has-source-component :value ocml-query-goal)
     (has-non-functional-properties
      :value ocml-query-mediator-non-functional-properties)))

(def-class ocml-query-web-service (web-service) ?web-service
    ((has-capability :value ocml-query-web-service-capability)
     (has-interface :value ocml-query-web-service-interface)
     (has-non-functional-properties
      :value ocml-query-web-service-non-functional-properties)))

(def-class ocml-query-web-service-non-functional-properties
    (non-functional-properties)
    nil)

(def-class ocml-query-web-service-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class ocml-query-web-service-capability
           (capability)
           ?capability
           ((used-mediator :value ocml-query-mediator)
            (has-non-functional-properties
             :value
             ocml-query-web-service-capability-non-functional-properties)))

(def-class ocml-query-web-service-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class ocml-query-web-service-interface-choreography
           (choreography)
    ((has-earthing :value ocml-query-web-service-interface-grounding)))

(def-instance ocml-query-web-service-interface-grounding rest-grounding
  ((lower-rule #_lower-for-ocml-query-web-service)
   (lift-rule #_lift-for-ocml-query-web-service)))

(def-rule #_lower-for-ocml-query-web-service
    ((#_lower-for-ocml-query-web-service ?invocation ?http-request) if
     (= ?query (wsmo-role-value ?invocation has-query))
     (= ?ontology (wsmo-role-value ?invocation has-ontology))
     (= ?format (wsmo-role-value ?invocation has-format))
     (= ?url (make-string "http://localhost:8080/api-rest/query?q=~A&o=~A&f=~A"
			  (#_rfc2616:url-encoding ?query)
			  (#_rfc2616:url-encoding ?ontology)
			  (#_rfc2616:url-encoding ?format)))
     (#_rfc2616:set-url ?http-request ?url)
     (#_rfc2616:set-method ?http-request "GET")))

(def-rule #_lift-for-ocml-query-web-service
    ((#_lift-for-ocml-query-web-service ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (set-goal-slot-value ?invocation has-query-result ?http-content)))

(def-class ocml-query-web-service-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class ocml-query-web-service-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             ocml-query-web-service-interface-orchestration-problem-solving-pattern)))

(def-class ocml-query-web-service-interface (interface) ?interface
    ((has-choreography :value ocml-query-web-service-interface-choreography)
     (has-orchestration :value ocml-query-web-service-interface-orchestration)
     (has-non-functional-properties
      :value
      ocml-query-web-service-interface-non-functional-properties)))

(def-class ocml-query-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value ocml-query-web-service-interface)
     (has-web-service-host :value "localhost")
     (has-web-service-port :value 8080)
     (has-web-service-location :value "/api-rest/query")))

;;; }}}
