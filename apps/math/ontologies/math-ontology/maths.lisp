;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs

(in-package #:ocml)

(in-ontology math-ontology)

;;; {{{ Goal definitions
(def-class #_add-goal (goal) ?goal
    ((has-input-role :value #_hasA
                     :value has-b)
     (has-input-soap-binding :value (#_hasA "string")
                             :value (has-b "string"))
     (has-output-role :value has-sum)
     (has-output-soap-binding :value (has-sum "xml"))
     (#_hasA :type number)
     (has-b :type number)
     (has-sum :type number)
     (has-non-functional-properties
      :value #_add-goal-non-functional-properties)))
;;; }}}

;;; {{{ add-web-service
(def-class #_add-mediator-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_add-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_add-goal)
     (has-non-functional-properties
      :value #_add-mediator-non-functional-properties)))

(def-class #_add-web-service (web-service) ?web-service
    ((has-capability :value #_add-web-service-capability)
     (has-interface :value #_add-web-service-interface)
     (has-non-functional-properties
      :value #_add-web-service-non-functional-properties)))

(def-class #_add-web-service-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_add-web-service-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_add-web-service-capability
           (capability)
           ?capability
           ((used-mediator :value #_add-mediator)
            (has-non-functional-properties
             :value
             #_add-web-service-capability-non-functional-properties)))

(def-class #_add-web-service-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_add-web-service-interface-choreography
           (choreography)
           ((has-grounding :value ((grounded-to-rest)))
            (has-earthing :value #_add-web-service-grounding)))

(def-instance #_add-web-service-grounding rest-grounding
  ((lower-rule #_lower-for-add-service)
   (lift-rule #_lift-for-add-service)))

(DEF-CLASS #_add-web-service-interface-orchestration-problem-solving-pattern
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS #_add-web-service-interface-orchestration
           (orchestration)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             #_add-web-service-interface-orchestration-problem-solving-pattern)))

(DEF-CLASS #_add-web-service-interface (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE #_add-web-service-interface-choreography)
     (HAS-ORCHESTRATION :VALUE #_add-web-service-interface-orchestration)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      #_add-web-service-interface-non-functional-properties)))
;;; }}}
;;; {{{ add http grounding

;;; The skyhooks we wrote earlier are complete useless to us here.
;;; They are applied, and the results stored elsewhere.  The
;;; wsmo-role-values are the original inputs.  We *should* apply the
;;; skyhooks, store those values against the service instance, and
;;; work from there.  but that's a whole other layer of crud I don't
;;; have time for.  Instead, we'll just take the goal args and run.
(def-rule #_lower-for-add-service
    ((#_lower-for-add-service ?invocation ?http-request)
     if
     (= ?a (wsmo-role-value ?invocation '#_hasA))
     (= ?b (wsmo-role-value ?invocation 'has-b))
     ;; Set the URL...
     (= ?url (make-string "http://localhost:8080/math/add?a=~A&b=~A"
                          ?a ?b))
     (#_rfc2616:set-url ?http-request ?url)
     (#_rfc2616:set-method ?http-request "GET")))

(def-rule #_lift-for-add
    ((#_lift-for-add-service ?http-response ?invocation)
     if
     (#_rfc2616:get-content ?http-response ?http-content)
     (set-goal-slot-value ?invocation has-sum ?http-content)))

;;; }}}
