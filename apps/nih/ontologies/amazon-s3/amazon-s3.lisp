;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs

(in-package #:ocml)

(in-ontology amazon-s3)

;;; {{{ Goal definitions
(def-class put-object-goal (goal) ?goal
    ((has-input-role :value has-account
                     :value has-bucket
                     :value has-key
                     :value has-data)
     (has-account :type #_amazon-account)
     (has-bucket :type #_amazon-bucket)
     (has-key :type #_amazon-object-key)
     (has-data :type string)
     (has-non-functional-properties
      :value put-object-goal-non-functional-properties)))

(def-class get-object-goal (goal) ?goal
    ((has-input-role :value has-account
                     :value has-bucket
                     :value has-key)
     (has-output-role :value #_hasContentType
                      :value #_hasContent)
     (has-account :type #_amazon-account)
     (has-bucket :type #_amazon-bucket)
     (has-key :type #_amazon-object-key)
     (#_hasContent :type string)
     (#_hasContentType :type string)
     (has-non-functional-properties
      :value get-object-goal-non-functional-properties)))

(def-class get-bucket-goal (goal) ?goal
    ((has-input-role :value has-account
                     :value has-bucket)
     (has-output-role :value #_hasContent
                      :value #_hasContentType)
     (has-account :type #_amazon-account)
     (has-bucket :type #_amazon-bucket)
     (has-key :type #_amazon-object-key)
     (#_hasContent :type string)
     (#_hasContentType :type string)
     (has-non-functional-properties
      :value get-object-goal-non-functional-properties)))
;;; }}}

;;; {{{ put-object-web-service
(def-class put-object-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class put-object-mediator (wg-mediator) ?mediator
    ((has-source-component :value put-object-goal)
     (has-non-functional-properties
      :value put-object-mediator-non-functional-properties)))

(def-class put-object-web-service (web-service) ?web-service
    ((has-capability :value put-object-web-service-capability)
     (has-interface :value put-object-web-service-interface)
     (has-non-functional-properties
      :value put-object-web-service-non-functional-properties)))

(def-class put-object-web-service-non-functional-properties
    (non-functional-properties)
    nil)

(def-class put-object-web-service-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class put-object-web-service-capability
           (capability)
           ?capability
           ((used-mediator :value put-object-mediator)
            (has-non-functional-properties
             :value
             put-object-web-service-capability-non-functional-properties)))

(def-class put-object-web-service-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class put-object-web-service-interface-choreography
           (choreography)
           ((has-grounding :value ((grounded-to-rest)))
            (has-earthing :value put-object-web-service-grounding)))

(def-instance put-object-web-service-grounding rest-grounding
  ())

(DEF-CLASS PUT-OBJECT-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS PUT-OBJECT-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             PUT-OBJECT-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS PUT-OBJECT-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE PUT-OBJECT-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE PUT-OBJECT-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      PUT-OBJECT-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class put-object-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value put-object-web-service-interface)
     (has-web-service-host :value "s3.amazonaws.com")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ put-object http grounding

(def-rule #_lower-for-put-object
    ((#_hg:lower put-object-web-service ?invocation ?http-request)
     if
     ;; Set the content
     (= ?content (wsmo-role-value ?invocation has-data))
     (#_rfc2616:set-content ?http-request ?content)
     ;; Set the date
     (= ?date (#_rfc2616:format-http-time (#_hg:current-time)))
     (#_rfc2616:set-header ?http-request "Date" ?date)
     ;; what IS the URL?!
     (= ?host "s3.amazonaws.com")
     (#_has-value (wsmo-role-value ?invocation 'has-bucket) ?bucket)
     (= ?key (wsmo-role-value ?invocation 'has-key))
     (= ?url (make-string "http://~A/~A/~A" ?host ?bucket ?key))
     (= ?canonical-url (make-string "/~A/~A" ?bucket ?key))
     ;; Set the URL...
     (#_rfc2616:set-url ?http-request ?url)
     (#_rfc2616:set-method ?http-request "PUT")
     (#_rfc2616:set-header ?http-request "Content-Type" "text/plain")
     ;; Dig out the access and secret keys...
     (= ?account (wsmo-role-value ?invocation has-account))
     (#_sign-amazon ?http-request ?canonical-url ?account)))

(def-rule #_lift-for-put-object
    ;; This is a no-op, since there's no meaningful content in the
    ;; response.
    ((#_hg:lift put-object-web-service ?http-response ?invocation)))
;;; }}}

;;; {{{ get-object-web-service
(def-class get-object-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class get-object-mediator (wg-mediator) ?mediator
    ((has-source-component :value get-object-goal)
     (has-non-functional-properties
      :value get-object-mediator-non-functional-properties)))

(def-class get-object-web-service (web-service) ?web-service
    ((has-capability :value get-object-web-service-capability)
     (has-interface :value get-object-web-service-interface)
     (has-non-functional-properties
      :value get-object-web-service-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class get-object-web-service-non-functional-properties
    (non-functional-properties)
    nil)

(def-class get-object-web-service-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class get-object-web-service-capability
           (capability)
           ?capability
           ((used-mediator :value get-object-mediator)
            (has-non-functional-properties
             :value
             get-object-web-service-capability-non-functional-properties)))

(def-class get-object-web-service-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class get-object-web-service-interface-choreography
           (choreography)
           ((has-grounding :value ((grounded-to-rest)))
            (has-earthing :value get-object-web-service-grounding)))

(def-instance get-object-web-service-grounding rest-grounding
  ())

(def-class get-object-web-service-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class get-object-web-service-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             get-object-web-service-interface-orchestration-problem-solving-pattern)))

(def-class get-object-web-service-interface (interface) ?interface
    ((has-choreography :value get-object-web-service-interface-choreography)
     (has-orchestration :value get-object-web-service-interface-orchestration)
     (has-non-functional-properties
      :value
      get-object-web-service-interface-non-functional-properties)))

(def-class get-object-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value get-object-web-service-interface)
     (has-web-service-host :value "s3.amazonaws.com")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ get-object http grounding

(def-rule #_lower-for-get-object
    ((#_hg:lower get-object-web-service ?invocation ?http-request)
     if
     ;; Set the date
     (= ?date (#_rfc2616:format-http-time (#_hg:current-time)))
     (#_rfc2616:set-header ?http-request "Date" ?date)
     ;; what IS the URL?!
     (= ?host "s3.amazonaws.com")
     (#_has-value (wsmo-role-value ?invocation 'has-bucket) ?bucket)
     (= ?key (wsmo-role-value ?invocation 'has-key))
     (= ?url (make-string "http://~A/~A/~A" ?host ?bucket ?key))
     (= ?canonical-url (make-string "/~A/~A" ?bucket ?key))
     ;; Set the URL...
     (#_rfc2616:set-url ?http-request ?url)
     (#_rfc2616:set-method ?http-request "GET")
     ;; Dig out the access and secret keys...
     (= ?account (wsmo-role-value ?invocation has-account))
     (#_sign-amazon ?http-request ?canonical-url ?account)))

(def-rule #_lift-for-get-object
    ((#_hg:lift get-object-web-service ?http-response ?invocation)
     if
     (#_rfc2616:get-content ?http-response ?http-content)
     (set-goal-slot-value ?invocation #_hasContent ?http-content)
     (#_rfc2616:header-value ?http-request "Content-Type" ?content-type)
     (set-goal-slot-value ?invocation #_hasContentType ?content-type)))
;;; }}}
;;; {{{ get-bucket http grounding

(def-rule #_lower-for-get-bucket
    ((#_hg:lower get-bucket-web-service ?invocation ?http-request)
     if
     (= ?date (#_rfc2616:format-http-time (#_hg:current-time)))
     (#_rfc2616:set-header ?http-request "Date" ?date)
     (= ?host "s3.amazonaws.com")
     (= ?bucketi (wsmo-role-value ?invocation 'has-bucket))
     (#_has-value ?bucketi ?bucket)
     (= ?url (make-string "http://~A/~A" ?host ?bucket))
     (= ?canonical-url (make-string "/~A" ?bucket))
     ;; Set the URL...
     (#_rfc2616:set-url ?http-request ?url)
     (#_rfc2616:set-method ?http-request "GET")
     ;; Dig out the access and secret keys...
     (= ?account (wsmo-role-value ?invocation has-account))
     (#_sign-amazon ?http-request ?canonical-url ?account)))

(def-rule #_lift-for-get-bucket
    ((#_hg:lift get-bucket-web-service ?http-response ?invocation)
     if
     (#_rfc2616:get-content ?http-response ?http-content)
     (set-goal-slot-value ?invocation #_hasContent ?http-content)
     (#_rfc2616:header-value ?http-request "Content-Type" ?content-type)
     (set-goal-slot-value ?invocation #_hasContentType ?content-type)))
;;; }}}

;;; {{{ get-bucket-web-service
(def-class get-bucket-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class get-bucket-mediator (wg-mediator) ?mediator
    ((has-source-component :value get-bucket-goal)
     (has-non-functional-properties
      :value get-bucket-mediator-non-functional-properties)))

(def-class get-bucket-web-service (web-service) ?web-service
    ((has-capability :value get-bucket-web-service-capability)
     (has-interface :value get-bucket-web-service-interface)
     (has-non-functional-properties
      :value get-bucket-web-service-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class get-bucket-web-service-non-functional-properties
    (non-functional-properties)
    nil)

(def-class get-bucket-web-service-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class get-bucket-web-service-capability
           (capability)
           ?capability
           ((used-mediator :value get-bucket-mediator)
            (has-non-functional-properties
             :value
             get-bucket-web-service-capability-non-functional-properties)))

(def-class get-bucket-web-service-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class get-bucket-web-service-interface-choreography
           (choreography)
           ((has-grounding :value ((grounded-to-rest)))
            (has-earthing :value get-bucket-web-service-grounding)))

(def-instance get-bucket-web-service-grounding rest-grounding
  ())

(def-class get-bucket-web-service-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class get-bucket-web-service-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             get-bucket-web-service-interface-orchestration-problem-solving-pattern)))

(def-class get-bucket-web-service-interface (interface) ?interface
    ((has-choreography :value get-bucket-web-service-interface-choreography)
     (has-orchestration :value get-bucket-web-service-interface-orchestration)
     (has-non-functional-properties
      :value
      get-bucket-web-service-interface-non-functional-properties)))

(def-class get-bucket-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value get-bucket-web-service-interface)
     (has-web-service-host :value "s3.amazonaws.com")
     (has-web-service-port :value 80)))

;;; }}}

;;; {{{ General amazon stuff

(def-rule #_sign-amazon
    ((#_sign-amazon ?http-request ?canonical-url ?account) if
     (#_has-amazon-access-key ?account ?access-key)
     (#_has-amazon-secret-key ?account ?secret-key)
     (= ?signature (#_compute-signature ?http-request ?canonical-url ?secret-key))
     (#_has-value ?access-key ?access-key-string)
     (= ?signature-header (make-string "AWS ~A:~A" ?access-key-string ?signature))
     (#_rfc2616:set-header ?http-request "Authorization" ?signature-header)))

;;; XXX This may be a total hack.  Content-Type should probably always
;;; be defined.
(def-rule #_get-content-type-or-empty
    ((#_get-content-type-or-empty ?http-request ?content-type) if
     (or (and (#_rfc2616:get-header ?http-request "Content-Type" ?header)
              (#_rfc2616:field-value ?header ?content-type))
         (= ?content-type ""))))

;;; XXX I think the canonical URL can be computed from the request,
;;; but I can't be bothered right now.
(def-function #_compute-signature (?http-request ?canonical-url ?secret-key)
  -> ?signature
  "Return the cryptographic signature for ?HTTP-REQUEST for Amazon's
S3 services."
  :constraint (and (#_http-request ?http-request)
                   (#_amazon-secret-key ?secret-key))
  :body (the ?signature
          (and
           (#_has-value ?secret-key ?secret-string)
           (#_rfc2616:has-method ?http-request ?method)
           (#_rfc2616:get-header ?http-request "Date" ?date-header)
           (#_rfc2616:field-value ?date-header ?date)
           (#_get-content-type-or-empty ?http-request ?content-type)
           (= ?on-the-line
              (make-string "~A~%~%~A~%~A~%~A" ?method ?content-type ?date
                           ?canonical-url))
           (= ?prebase64 (#_enc:hmac-sha1 ?on-the-line ?secret-string))
           (= ?signature (#_mime:encode-base64 ?prebase64)))))
;;; }}}
