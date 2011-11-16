;;; Community services at CINECA.

;;; {{{ download-file-goal
(def-class download-file-goal (community-goal) ?goal
    ((has-input-role :value has-filename
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-filename "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-download-result)
     (has-output-soap-binding :value (has-download-result "xml"))
     (has-filename :type lhdl-filename)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-download-result :type download-response)
     (has-non-functional-properties :value download-file-goal-non-functional-properties)))

(def-class download-file-goal-non-functional-properties (non-functional-properties)
    ((has-description "Download a file from BiomedTown storage.")))
;;; }}}
;;; {{{ upload-file-goal
(def-class upload-file-goal (community-goal) ?goal
    ((has-input-role :value has-filename
                     :value has-id
                     :value has-comment
                     :value has-content
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-filename "string")
                             :value (has-id "string")
                             :value (has-comment "string")
                             :value (has-content "string")
                             :value (#_hasAccount "string"))
     (has-filename :type lhdl-filename)
     (has-id :type string)
     (has-comment :type string)
     (has-content :type string)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-non-functional-properties :value upload-file-goal-non-functional-properties)))

(def-class upload-file-goal-non-functional-properties (non-functional-properties)
    ((has-description "Upload a file to BiomedTown storage.")))
;;; }}}
;;; {{{ delete-file-goal
(def-class delete-file-goal (community-goal) ?goal
    ((has-input-role :value has-filename
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-filename "string")
                             :value (#_hasAccount "string"))
     (has-filename :type lhdl-filename)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-non-functional-properties :value delete-file-goal-non-functional-properties)))

(def-class delete-file-goal-non-functional-properties (non-functional-properties)
    ((has-description "Delete a file from BiomedTown storage.")))
;;; }}}
;;; {{{ simpleSearchGoal
(def-class #_simpleSearchGoal (community-goal) ?goal
    ((has-input-role :value #_hasQuery
                     :value #_hasAccount)
     (has-input-soap-binding :value (#_hasQuery "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value #_hasResult)
     (has-output-soap-binding :value (#_hasResult "xml"))
     (#_hasQuery :type string)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (#_hasResult :type string)
     (has-non-functional-properties
      :value #_simpleSearchGoal-non-functional-properties)))

(def-class #_simpleSearchGoal-non-functional-properties (non-functional-properties)
    ((has-description "Simple meta-data search of Biomed Town.")))
;;; }}}
;;; {{{ listSandboxGoal
(def-class #_listSandboxGoal (community-goal) ?goal
    ((has-input-role :value #_hasAccount)
     (has-input-soap-binding :value (#_hasAccount "string"))
     (has-output-role :value #_hasResult)
     (has-output-soap-binding :value (#_hasResult "xml"))
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (#_hasResult :type string)
     (has-non-functional-properties
      :value #_listSandboxGoal-non-functional-properties)))

(def-class #_listSandboxGoal-non-functional-properties (non-functional-properties)
    ((has-description "List contents of the user's sandbox.")))
;;; }}}

;;; {{{ download-file-web-service
(DEF-CLASS download-file-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS download-file-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE download-file-GOAL)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE download-file-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS DOWNLOAD-FILE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE DOWNLOAD-FILE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE DOWNLOAD-FILE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE DOWNLOAD-FILE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS DOWNLOAD-FILE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS DOWNLOAD-FILE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS DOWNLOAD-FILE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE DOWNLOAD-FILE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             DOWNLOAD-FILE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS DOWNLOAD-FILE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS DOWNLOAD-FILE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-rest)))
            (has-earthing :value download-file-web-service-grounding)))

(def-instance download-file-web-service-grounding rest-grounding
  ())

(DEF-CLASS DOWNLOAD-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS DOWNLOAD-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             DOWNLOAD-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS DOWNLOAD-FILE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE DOWNLOAD-FILE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE DOWNLOAD-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      DOWNLOAD-FILE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-rule #_lower-for-download-file
    ((#_hg:lower download-file-web-service ?invocation ?http-request) if
     (#_sharedBit ?invocation ?http-request)
     (has-value (wsmo-role-value ?invocation 'has-filename) ?filename)
     (= ?xmlrpc-model
        (#_xmlrpc:MethodCall "XMLDownload"
                            (#_xmlrpc:Param (#_xmlrpc:String ?filename))))
     (#_xmlrpc:mapToXml ?xmlrpc-model ?xml-model)
     (#_xml:serialiseXml ?xml-model ?xml-string)
     (#_rfc2616:set-content ?http-request ?xml-string)))

(def-rule #_lift-for-download-file
    ((#_hg:lift download-file-web-service ?http-response ?invocation)
     if
     (#_rfc2616:get-content ?http-response ?xml)
     (= ?download-result (lift-up-download-response ?xml))
     (set-goal-slot-value ?invocation has-download-result ?download-result)))

;;; XXX This is an ugly hack.  Replace with a general XML
;;; deserialisation mechanism.
(def-function lift-up-download-response (xml)
  :lisp-fun (lambda (xml)
              (xpm::with-ocml-ctx (:class 'download-response)
                (let* ((i (xpm::o-instance))
                       (x (xpm::document-parser xml))
                       (base  (xpm::children (first (xpm::children (first (xpm::children (second (xpm::children (second (xpm::children (second (xpm::children (first (xpm::children x))))))))))))))
                       (filename (dom:node-value (first (xpm::children (first (xpm::children (second base)))))))
                       (content (dom:node-value (first (xpm::children (first (xpm::children (fourth base))))))))
                  (xpm::o-set-slot 'has-filename filename i)
                  (xpm::o-set-slot 'has-content content i)
                  i))))

;;; }}}
;;; {{{ upload-file-web-service
(DEF-CLASS upload-file-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS upload-file-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE upload-file-GOAL)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE upload-file-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS UPLOAD-FILE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE UPLOAD-FILE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE UPLOAD-FILE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE UPLOAD-FILE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS UPLOAD-FILE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS UPLOAD-FILE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS UPLOAD-FILE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE UPLOAD-FILE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             UPLOAD-FILE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS UPLOAD-FILE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS UPLOAD-FILE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-rest)))
            (has-earthing :value upload-file-web-service-grounding)))

(def-instance upload-file-web-service-grounding rest-grounding
  ())

(DEF-CLASS UPLOAD-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS UPLOAD-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             UPLOAD-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS UPLOAD-FILE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE UPLOAD-FILE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE UPLOAD-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      UPLOAD-FILE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-rule #_lower-for-upload-file
    ((#_hg:lower upload-file-web-service ?invocation ?http-request) if
     (#_sharedBit ?invocation ?http-request)
     (has-value (wsmo-role-value ?invocation 'has-filename) ?filename)
     (= ?id (wsmo-role-value ?invocation 'has-id))
     (= ?comment (wsmo-role-value ?invocation 'has-comment))
     (= ?content (wsmo-role-value ?invocation 'has-content))
     (= ?xmlrpc-model
        (#_xmlrpc:MethodCall "XMLUpload"
			     (#_xmlrpc:Param (#_xmlrpc:String ?filename))
			     (#_xmlrpc:Param (#_xmlrpc:String ?id))
			     (#_xmlrpc:Param (#_xmlrpc:String ?comment))
			     (#_xmlrpc:Param (#_xmlrpc:Base64 ?content))))
     (#_xmlrpc:mapToXml ?xmlrpc-model ?xml-model)
     (#_xml:serialiseXml ?xml-model ?xml-string)
     (#_rfc2616:set-content ?http-request ?xml-string)))

(def-rule #_lift-for-get-object
    ((#_hg:lift upload-file-web-service ?http-response ?invocation)
     if
     (#_rfc2616:get-content ?http-response ?http-content)
     (set-goal-slot-value ?invocation #_hasContent ?http-content)
     (#_rfc2616:header-value ?http-request "Content-Type" ?content-type)
     (set-goal-slot-value ?invocation #_hasContentType ?content-type)))

;;; }}}
;;; {{{ delete-file-web-service
(DEF-CLASS delete-file-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS delete-file-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE delete-file-GOAL)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE delete-file-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS DELETE-FILE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE DELETE-FILE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE DELETE-FILE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE DELETE-FILE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS DELETE-FILE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS DELETE-FILE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS DELETE-FILE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE DELETE-FILE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             DELETE-FILE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS DELETE-FILE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS DELETE-FILE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-rest)))
            (has-earthing :value delete-file-web-service-grounding)))

(def-instance delete-file-web-service-grounding rest-grounding
  ())

(DEF-CLASS DELETE-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS DELETE-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             DELETE-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS DELETE-FILE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE DELETE-FILE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE DELETE-FILE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      DELETE-FILE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-rule #_lower-for-delete-file
    ((#_hg:lower delete-file-web-service ?invocation ?http-request) if
     (#_sharedBit ?invocation ?http-request)
     (has-value (wsmo-role-value ?invocation 'has-filename) ?filename)
     (= ?xmlrpc-model
        (#_xmlrpc:MethodCall "XMLDelete"
			     (#_xmlrpc:Param (#_xmlrpc:String ?filename))))
     (#_xmlrpc:mapToXml ?xmlrpc-model ?xml-model)
     (#_xml:serialiseXml ?xml-model ?xml-string)
     (#_rfc2616:set-content ?http-request ?xml-string)))

(def-rule #_lift-for-delete-file
    ((#_hg:lift delete-file-web-service ?http-response ?invocation)))
;;; }}}

;;; {{{ simpleSearchService
(def-class #_simpleSearchMediator-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_simpleSearchMediator (wg-mediator) ?mediator
    ((has-source-component :value #_simpleSearchGoal)
     (has-non-functional-properties
      :VALUE #_simpleSearchMediator-non-functional-properties)))

(def-class #_simpleSearchService (web-service) ?web-service
    ((has-capability :value #_simpleSearchService-capability)
     (has-interface :value #_simpleSearchService-interface)
     (has-non-functional-properties
      :value #_simpleSearchService-non-functional-properties)))

(def-class #_simpleSearchService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_simpleSearchService-capability-non-functional-properties
           (NON-functional-properties)
           nil)

(def-class #_simpleSearchService-capability (capability) ?capability
    ((used-mediator :value #_simpleSearchMediator)
     (has-non-functional-properties
      :value
      #_simpleSearchService-capability-non-functional-properties)))

(def-class #_simpleSearchService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_simpleSearchService-interface-choreography
           (choreography)
           ((has-grounding :value ((grounded-to-rest)))
            (has-earthing :value #_simpleSearchService-grounding)))

(def-instance #_simpleSearchService-grounding rest-grounding
  ())

(def-class #_simpleSearchService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_simpleSearchService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_simpleSearchService-interface-orchestration-problem-solving-pattern)))

(def-class #_simpleSearchService-interface (interface) ?interface
    ((has-choreography :value #_simpleSearchService-interface-choreography)
     (has-orchestration :value #_simpleSearchService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_simpleSearchService-interface-non-functional-properties)))

(def-rule #_lower-for-simpleSearch
    ((#_hg:lower #_simpleSearchService ?invocation ?http-request) if
     (#_sharedBit ?invocation ?http-request)
     (= ?query (wsmo-role-value ?invocation #_hasQuery))
     (= ?xmlrpc-model
        (#_xmlrpc:MethodCall "XMLSimpleSearchMetadata"
                            (#_xmlrpc:Param (#_xmlrpc:String ?query))))
     (#_xmlrpc:mapToXml ?xmlrpc-model ?xml-model)
     (#_xml:serialiseXml ?xml-model ?xml-string)
     (#_rfc2616:set-content ?http-request ?xml-string)))

(def-rule #_lift-for-simpleSearch
    ((#_hg:lift #_simpleSearchService ?http-response ?invocation)
     if
     (#_rfc2616:get-content ?http-response ?http-content)
     (set-goal-slot-value ?invocation #_hasResult ?http-content)))
;;; }}}
;;; {{{ listSandboxService
(def-class #_listSandboxMediator-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_listSandboxMediator (wg-mediator) ?mediator
    ((has-source-component :value #_listSandboxGoal)
     (has-non-functional-properties
      :VALUE #_listSandboxMediator-non-functional-properties)))

(def-class #_listSandboxService (web-service) ?web-service
    ((has-capability :value #_listSandboxService-capability)
     (has-interface :value #_listSandboxService-interface)
     (has-non-functional-properties
      :value #_listSandboxService-non-functional-properties)))

(def-class #_listSandboxService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_listSandboxService-capability-non-functional-properties
           (NON-functional-properties)
           nil)

(def-class #_listSandboxService-capability (capability) ?capability
    ((used-mediator :value #_listSandboxMediator)
     (has-non-functional-properties
      :value
      #_listSandboxService-capability-non-functional-properties)))

(def-class #_listSandboxService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_listSandboxService-interface-choreography
           (choreography)
           ((has-grounding :value ((grounded-to-rest)))
            (has-earthing :value #_listSandboxService-grounding)))

(def-instance #_listSandboxService-grounding rest-grounding
  ())

(def-class #_listSandboxService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_listSandboxService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_listSandboxService-interface-orchestration-problem-solving-pattern)))

(def-class #_listSandboxService-interface (interface) ?interface
    ((has-choreography :value #_listSandboxService-interface-choreography)
     (has-orchestration :value #_listSandboxService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_listSandboxService-interface-non-functional-properties)))

(def-rule #_lower-for-listSandbox
    ((#_hg:lower #_listSandboxService ?invocation ?http-request) if
     (#_sharedBit ?invocation ?http-request)
     (= ?xmlrpc-model
        (#_xmlrpc:MethodCall "listSandbox"))
     (#_xmlrpc:mapToXml ?xmlrpc-model ?xml-model)
     (#_xml:serialiseXml ?xml-model ?xml-string)
     (#_rfc2616:set-content ?http-request ?xml-string)))

(def-rule #_lift-for-listSandbox
    ((#_hg:lift #_listSandboxService ?http-response ?invocation)
     if
     (#_rfc2616:get-content ?http-response ?http-content)
     (set-goal-slot-value ?invocation #_hasResult ?http-content)))
;;; }}}

;;; {{{ HTTP stuff
(def-rule #_authorizeBasic
    ((#_authorizeBasic ?request ?username ?password) if
     (= ?x (make-string "~A:~A" ?username ?password))
     (= ?base64 (#_base64 ?x))
     (= ?value (make-string "Basic ~A" ?base64))
     (#_rfc2616:set-header ?request "Authorization" ?value)))

(def-function #_base64 (string)
  :lisp-fun #'base64:string-to-base64-string)

(def-rule #_sharedBit
    ((#_sharedBit ?invocation ?request) if
     (= ?url "https://www.biomedtown.org/biomed_town/LHDL/users/repository/lhprepository2")
     (#_rfc2616:set-url ?request ?url)
     (#_rfc2616:set-method ?request "POST")
     (#_rfc2616:set-header ?request "Content-Type" "text/xml")
     (#_rfc2616:set-header ?request "Date" (#_rfc2616:format-http-time
                                           (#_hg:current-time)))
     (= ?account (wsmo-role-value ?invocation #_hasAccount))
     (#_domain:hasPassword ?account ?password*)
     (#_domain:hasUsername ?account ?username*)
     (has-value ?username* ?username)
     (has-value ?password* ?password)
     (#_authorizeBasic ?request ?username ?password)))
;;; }}}

