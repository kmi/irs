;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology wsmo)

(def-class invokable-entity ()
  "Captures the input and output roles used in the UPML framework. The OCML basic library contains a relations has-input-role and has-output-role which work on classes. "
  ((has-input-role :type role)
   (has-output-role :type role)))

(def-class non-functional-properties ()
 ((has-accuracy :type string)
   (has-contributor :type string)
   (has-coverage :type string)
   (has-creator :type string)
   (has-date :type string)
   (has-description :type string)
   (has-financial :type string)
   (has-format :type string)
   (has-identifier :type string)
   (has-language :type string)
   (has-net-Related-QoS :type string)
   (has-owner :type string)
   (has-performance :type string)
   (has-publisher :type string)
   ;;relation is an ocml primitive
   (has-relation :type string)
   (has-reliability :type string)
   (has-rights :type string)
   (has-robustness :type string)
   (has-scalability :type string)
   (has-security :type string)
   (has-source :type string)
   (has-subject :type string)
   (has-title :type string)
   (has-transactional :type string)
   (has-trust :type string)
   (has-type :type string)
   (has-type-Of-Match :type string)
   (has-duration :type string)
   (is-automatic :type boolean)
   (has-version :type string)))

(def-class soap-type ())

(def-class symbol ())

(def-class effect ())

(def-class pre-condition (unary-kappa-expression))

(def-class post-condition ())

(def-class assumption (unary-kappa-expression))

(def-class wsmo-entity ()
  ((has-non-functional-properties :type non-functional-properties)))

(def-class wsmo-web-service-entity (wsmo-entity)
  ())

;;no longer used - maybe put back later
(def-class soap-binding ()
  ((has-role-name :type symbol)
   (has-soap-type :type soap-type)))

(def-class goal-type () ?x
  :iff-def (or (= ?x goal)
               (subclass-of ?x goal)))

(def-class goal (wsmo-entity invokable-entity)
  ((used-mediator :type mediator)
   (has-post-condition :type post-condition)
   (has-effect :type effect)
   (has-input-soap-binding :type list)
   (has-output-soap-binding :type list)))

(def-class mediation-service ())

(def-class mediator-type () ?x
  :iff-def (or (= ?x mediator)
               (subclass-of ?x mediator)))

(def-class mediator (invokable-entity wsmo-web-service-entity)
  ((has-source-component :type wsmo-entity)
   (has-target-component :type wsmo-entity)
   (has-mapping-rules :type mapping-rules)
   (has-mediation-service :type mediation-service)))

(def-class web-service-type () ?x
  :iff-def (or (= ?x web-service)
               (subclass-of ?x web-service)))

(def-class web-service (invokable-entity  wsmo-web-service-entity)
  ((has-capability :type capability)
   (has-interface :type interface)
   (used-mediator :type oo-mediator)
   ;;extra to wsmo
   (has-internal-method :type symbol)))

(def-class capability (wsmo-web-service-entity)
  ((used-mediator :type wg-oo-mediator)
   (has-pre-condition :type pre-condition)
   (has-post-condition :type post-condition)
   (has-assumption :type assumption)
   (has-effect :type effect)))

(def-class interface (wsmo-web-service-entity) 
  ((has-choreography :type choreography)
   (has-orchestration :type orchestration)
   (used-mediator :type oo-mediator)))

(def-class lisp-function (atom))

(def-class java-class (atom))

(def-class java-method (atom))

(def-class web-service-provider-type ())

(def-instance apache_soap web-service-provider-type)

(def-instance axis web-service-provider-type)

(def-class message-exchange-pattern ()
  "A choreography is a set of rule packets."
  ((has-rule-packet :type rule-packet)))

(def-class publisher-information ()
  ((has-associated-web-service-interface :type interface)
   (has-web-service-host :type string)
   (has-web-service-port :type integer)
   (has-web-service-location :type string)
   (has-wsdl-file-name :type string)
   (has-wsdl-operation-name :type string)
   (has-wsdl-port-type :type string)
   (has-wsdl-port-type-name-space :type string)
   (has-wsdl-web-service-provider-type 
    :type web-service-provider-type)))

(def-class choreography ()
  ((has-grounding :type list)
   ;; has-earthing is a transitionary slot.  The intention is to
   ;; replace the opaque has-grounding symbolic expression with an
   ;; equivalent specified as ontology objects.
   (has-earthing :type grounding)
   (has-message-exchange-pattern :type message-exchange-pattern)
   (has-guarded-transitions :type list)))

(def-class grounding ())

(def-class http-grounding-mixin ()
   ;; Seconds to wait before declaring a failure to read from the
   ;; socket.
  ((has-connection-read-timeout :type integer)))

;;; This one's all about unfettered control of the HTTP machinary.
;;; More complex but more flexible, this should enable pretty much any
;;; HTTP based service to be invoked from the IRS.
;;;
;;; It should be accessed through a collection of relations that I'll
;;; be working out, mostly over in the ‘HTTP’ ontology.
(def-class rest-grounding (http-grounding-mixin)
    ((lower-rule :type rule)
     (lift-rule :type rule)))

(def-class soap-grounding (grounding http-grounding-mixin)
  (;; The URL for POSTing the SOAP request to.
   (has-url :type string)
   ;; The SOAPAction header value
   (has-soap-action :type string)
   ;; The SOAP message element name
   (has-soap-method :type string)
   ;; The XML namespace of the method
   (has-target-namespace :type string)))

(def-class refiner (mediator))

(def-class bridge (mediator))

(def-class wg-mediator (bridge)
  ((has-source-component :type (or web-service wg-mediator))
   (has-target-component :type (or goal wg-mediator))
   (used-mediator :type oo-mediator)
   (has-reduction :type axiom-definition)))

(def-class gw-mediator (bridge)
  ((has-source-component :type (or goal gw-mediator))
   (has-target-component :type (or web-service gw-mediator))
   (used-mediator :type oo-mediator)
   (has-reduction :type axiom-definition)))

(def-class ww-mediator (bridge)
  ((has-source-component :type (or web-service ww-mediator))
   (has-target-component :type (or web-service ww-mediator))
   (used-mediator :type oo-mediator)))

(def-class gg-mediator (refiner)
  ((used-mediator :type oo-mediator)
   (has-source-component :type (or goal gg-mediator))
   (has-target-component :type (or goal gg-mediator))
   (has-reduction :type axiom-definition)))

(def-class oo-mediator (refiner)
  ((has-source-component :type oo-mediator)))

(def-class wg-oo-mediator () ?x
  :iff-def (or (oo-mediator ?x)
               (wg-mediator ?x)))

(def-relation has-wsmo-input-role (?thing ?role)
  "This definition generalises the notion of 
   'having an input role' to classes as well 
    as goal instances.  If ?class is a capability, then
    it also 'inherits the input roles from the goal type
    to which it is applicable. If ?class is a web-service it gets the
    input roles from the capability and associated goal."
  :sufficient 
  (or (and (instance ?thing)
           (HAS-wsmo-INPUT-ROLE (the-parent ?thing) ?role))
      (and (class ?thing)
           (or 
            (and 
             (subclass-of ?thing invokable-entity)
             (member ?role (all-class-slot-values 
                            ?thing has-input-role)))
            (and (subclass-of ?thing web-service)
                 (associated-goal ?thing ?goal-type)
                 (HAS-wsmo-INPUT-ROLE ?goal-type ?role))))))

(def-relation all-wsmo-input-roles (?thing ?roles)
  "This definition generalises the notion of 
   'having an input role' to classes as well 
    as goal instances.  If ?class is a capability, then
    it also 'inherits the input roles from the goal type
    to which it is applicable. If ?class is a web-service it gets the
    input roles from the capability and associated goal."
  :sufficient 
  (or (and (instance ?thing)
           (all-wsmo-INPUT-ROLEs (the-parent ?thing) ?roles))
      (and (class ?thing)
           (or 
            (and (subclass-of ?thing web-service)
                 (associated-goal ?thing ?goal-type)
                 (all-wsmo-INPUT-ROLEs ?goal-type ?roles))
            (and 
             (subclass-of ?thing invokable-entity)
             (= ?roles (all-class-slot-values 
                        ?thing has-input-role)))))))

(def-relation all-wsmo-input-roles-with-types 
  (?thing ?roles-with-types)
  "This definition generalises the notion of 
   'having an input roles with type values' to classes as well 
    as goal instances.  If ?class is a capability, then
    it also 'inherits the input roles from the goal type
    to which it is applicable. If ?class is a web-service it gets the
    input roles from the capability and associated goal."
  :sufficient 
  (or (and (instance ?thing)
           (all-wsmo-input-roles-with-types
            (the-parent ?thing) ?roles-with-types))
      (and (class ?thing)
           (or 
            (and (subclass-of ?thing web-service)
                 (associated-goal ?thing ?goal-type)
                 (all-wsmo-input-roles-with-types ?goal-type 
                                                  ?roles-with-types))
            (and 
             (subclass-of ?thing invokable-entity)
             (= ?roles (all-class-slot-values 
                        ?thing has-input-role))
             (= ?roles-with-types
                (input-roles-with-types ?thing ?roles)))))))

(def-function input-roles-with-types (?thing ?roles)
  :lisp-fun
  #'(lambda (class roles)
      (mapcar #'(lambda (role) 
                  (list role 
                        (ocml-eval-gen 
                         `(all-class-slot-types ,class ,role))))
              roles)))

(def-function output-role-with-type (?thing ?role)
  :lisp-fun
  #'(lambda (class role)
      (list role 
            (ocml-eval-gen 
             `(all-class-slot-types ,class ,role)))))


(def-relation has-wsmo-input-soap-binding (?thing ?role-soap-binding)
  "This definition generalises the notion of 
   'having an input role soap binding' to classes as well 
    as goal instances.  If ?class is a capability, then
    it also 'inherits the input roles from the goal type
    to which it is applicable. If ?class is a web-service it gets the
    input roles from the capability and associated goal."
  :sufficient 
  (or (and (instance ?thing)
           (HAS-wsmo-INPUT-soap-binding (the-parent ?thing) 
                                        ?role-soap-binding))
      (and (class ?thing)
           (or 
            (and 
             (subclass-of ?thing invokable-entity)
             (member ?role-soap-binding (all-class-slot-values 
                                         ?thing has-input-soap-binding)))
            (and (subclass-of ?thing web-service)
                 (associated-goal ?thing ?goal-type)
                 (has-wsmo-input-soap-binding ?goal-type 
                                              ?role-soap-binding))))))

(def-relation all-wsmo-input-soap-bindings 
  (?thing ?roles-with-soap-binding)
  "This definition generalises the notion of 
   'having an input role soap binding' to classes as well 
    as goal instances.  If ?class is a capability, then
    it also 'inherits the input roles from the goal type
    to which it is applicable. If ?class is a web-service it gets the
    input roles from the capability and associated goal."
  :sufficient 
  (or (and (instance ?thing)
           (all-wsmo-input-soap-bindings (the-parent ?thing) 
                                         ?roles-with-soap-binding))
      (and (class ?thing)
           (or 
            (and (subclass-of ?thing web-service)
                 (associated-goal ?thing ?goal-type)
                 (all-wsmo-input-soap-bindings ?goal-type 
                                               ?roles-with-soap-binding))
            (and 
             (subclass-of ?thing invokable-entity)
             (= ?roles-with-soap-binding 
                (all-class-slot-values 
                 ?thing has-input-soap-binding)))))))

(def-relation has-wsmo-output-role (?thing ?role)
  "This definition generalises the notion of 
   'having an output role' to classes as well 
    as tasks instances.  If ?thing is a method, then

    it also 'inherits the output role from the task type
    to which it is applicable"
  :sufficient  
  (or (and (instance ?thing)
           (has-wsmo-output-role (the-parent ?thing) ?role))
      (and (class ?thing)
           (or 
            (and 
             (subclass-of ?thing invokable-entity)
             (member ?role (all-class-slot-values 
                            ?thing has-output-role)))
            (and (subclass-of ?thing web-service)
                 (associated-goal ?thing ?goal-type)
                 (has-wsmo-output-role ?goal-type ?role))))))

(def-relation wsmo-output-role-with-types (?thing ?role-with-types)
  "This definition generalises the notion of 
   'having an output role with types' to classes as well 
    as tasks instances.  If ?thing is a method, then

    it also 'inherits the output role from the task type
    to which it is applicable"
  :sufficient  
  (or (and (instance ?thing)
           (wsmo-output-role-with-types (the-parent ?thing)
                                        ?role-with-types))
      (and (class ?thing)
           (or 
            (and 
             (subclass-of ?thing invokable-entity)
             (member ?role (all-class-slot-values 
                            ?thing has-output-role))
             (= ?role-with-types
                (output-role-with-type ?thing ?role)))
            (and (subclass-of ?thing web-service)
                 (associated-goal ?thing ?goal-type)
                 (wsmo-output-role-with-types ?goal-type ?role-with-types))))))

(def-relation wsmo-local-output-role-with-types (?thing ?role-with-types)
  "This definition generalises the notion of 
   'having an output role with types' to classes as well 
    as tasks instances.  If ?thing is a method, then

    it also 'inherits the output role from the task type
    to which it is applicable"
  :sufficient  
  (or (and (instance ?thing)
           (wsmo-local-output-role-with-types 
            (the-parent ?thing) ?role-with-types))
      (and (class ?thing)
           (or 
            (and 
             (subclass-of ?thing invokable-entity)
             (member ?role (all-class-slot-local-values 
                            ?thing has-output-role))
             (= ?role-with-types
                (output-role-with-type ?thing ?role)))
            (and (subclass-of ?thing web-service)
                 (associated-goal ?thing ?goal-type)
                 (wsmo-local-output-role-with-types ?goal-type 
                                                    ?role-with-types))))))


(def-relation HAS-wsmo-OUTPUT-soap-binding (?thing ?role-soap-binding)
  "This definition generalises the notion of 
   'having an output role' to classes as well 
    as tasks instances.  If ?thing is a method, then

    it also 'inherits the output role from the task type
    to which it is applicable"
  :sufficient  
  (or (and (instance ?thing)
           (has-wsmo-output-soap-binding (the-parent ?thing) 
                                         ?role-soap-binding))
      (and (class ?thing)
           (or 
            (and 
             (subclass-of ?thing invokable-entity)
             (member ?role-soap-binding (all-class-slot-values 
                                         ?thing has-output-soap-binding)))
            (and (subclass-of ?thing web-service)
                 (associated-goal ?thing ?goal-type)
                 (has-wsmo-output-soap-binding ?goal-type ?role-soap-binding))))))

(def-relation all-local-wsmo-input-roles (?invokable-entity ?roles)
  "A fast relation to get local input roles"
  :sufficient 
  (= ?roles (all-class-slot-local-values ?invokable-entity has-input-role)))

(def-relation all-local-wsmo-output-roles (?invokable-entity ?roles)
  "A fast relation to get the local output roles"
  :sufficient 
  (= ?roles (all-class-slot-local-values ?invokable-entity has-output-role)))

(def-relation all-local-wsmo-input-roles-with-soap-bindings (?goal ?roles)
  "A fast relation to get local input roles and soap bindings"
  :sufficient 
  (= ?roles (all-class-slot-local-values ?goal has-input-soap-binding)))

(def-relation all-local-wsmo-output-roles-with-soap-bindings (?goal ?roles)
  "A fast relation to get the local output roles and soap bindings"
  :sufficient 
  (= ?roles (all-class-slot-local-values ?goal has-output-soap-binding)))

(def-relation associated-goal (?web-service ?goal)
  "Gets the goal associated with a web service"
  :sufficient  
  (or (and (instance ?web-service)
           (associated-goal (the-parent ?web-service) ?goal))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (= ?capability (the-class-slot-value ?web-service has-capability))
           (class ?capability)                        
           (= ?mediator (the-class-slot-value ?capability used-mediator))
           (class ?mediator)
           (= ?goal (the-class-slot-value ?mediator
                                          has-source-component))
           (class ?goal))
      (and (class ?mediator)
           (subclass-of ?mediator mediator)
           (= ?web-service (the-class-slot-value ?mediator
                                                 has-target-component))
           (= ?goal (the-class-slot-value ?mediator
                                          has-source-component))
           (class ?goal))))

(def-relation associated-capability (?web-service ?capability)
  "Gets the capability associated with a web service"
  :sufficient  
  (or (and (instance ?web-service)
           (associated-capability (the-parent ?web-service) ?capability))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (= ?capability (the-class-slot-value ?web-service has-capability)))))

(def-relation associated-interface (?web-service ?interface)
  "Gets the interface associated with a web service"
  :sufficient 
  (or (and (instance ?web-service)
           (associated-interface (the-parent ?web-service) ?interface))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (= ?interface (the-class-slot-value ?web-service has-interface)))))

(def-relation associated-choreography (?web-service ?choreography)
  "Gets the choreography associated with a web service"
  :sufficient  
  (or (and (instance ?web-service)
           (associated-choreography (the-parent ?web-service) ?choreography))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (= ?interface (the-class-slot-value ?web-service has-interface))
           (class ?interface)
           (= ?choreography (the-class-slot-value ?interface has-choreography))
           (class ?choreography))))

(def-relation associated-publisher-information (?web-service ?publisher-information)
  "Gets the publisher information associated with a web service"
  :sufficient  
  (and (associated-interface ?web-service ?interface)
       (class ?interface)
       (subclass-of ?publisher-information
                    publisher-information)
       (= ?interface
          (the-class-slot-value ?publisher-information
                                has-associated-web-service-interface))))

(def-relation associated-message-exchange-pattern 
  (?web-service ?message-exchange-pattern)
  "Gets the message exchange pattern associated with a web service. This is used in the choreography."
  :sufficient  
  (or (and (instance ?web-service)
           (associated-message-exchange-pattern (the-parent ?web-service) 
                                                ?message-exchange-pattern))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (= ?interface (the-class-slot-value ?web-service has-interface))
           (class ?interface)
           (= ?choreography (the-class-slot-value ?interface has-choreography))
           (class ?choreography)
           (= ?message-exchange-pattern 
              (the-class-slot-value ?choreography has-message-exchange-pattern)))))

(def-relation associated-grounding (?web-service ?grounding)
  "Gets the operation input and output mappings and associated function with a web service. This is used in invocation."
  :sufficient  
  (and (associated-choreography ?web-service ?choreography)
       (class ?choreography)
       (= ?grounding (the-class-slot-value ?choreography has-grounding))))

(def-relation associated-earthing (?web-service ?earthing)
  "Get the grounding-specific parameters associated with a web
service. This is used in invocation."
  :sufficient
  (and (associated-choreography ?web-service ?choreography)
       (class ?choreography)
       (= ?earthing (the-class-slot-value ?choreography has-earthing))))

(def-relation associated-guarded-transitions (?web-service ?guarded-transitions)
  "Gets the operation input mapping associated with a web service. This is used in the choreography."
  :sufficient  
  (or (and (instance ?web-service)
           (associated-guarded-transitions (the-parent ?web-service) 
                                           ?guarded-transitions))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (= ?interface (the-class-slot-value ?web-service has-interface))
           (class ?interface)
           (= ?choreography (the-class-slot-value ?interface has-choreography))
           (class ?choreography)
           (= ?guarded-transitions
              (the-class-slot-value ?choreography has-guarded-transitions)))))
                        

(def-relation web-service-message-exchange-pattern (?web-service ?message-exchange-pattern)
  "Gets the exchange pattern associated with a web service"
  :sufficient 
  (or (and (instance ?web-service)
           (web-service-message-exchange-pattern (the-parent ?web-service)
                                                 ?message-exchange-pattern))
      (and (associated-choreography ?web-service ?choreography)
           (= ?message-exchange-pattern 
              (the-class-slot-value ?choreography 
                                    has-message-exchange-pattern)))))

(def-relation web-service-lisp-function (?web-service ?lisp-function)
  "Gets the lisp function associated with a web service"
  :sufficient 
  (or (and (instance ?web-service)
           (web-service-lisp-function (the-parent ?web-service)
                                      ?lisp-function))
      (and (associated-choreography ?web-service ?choreography)
           (= ?lisp-function
              (the-class-slot-value ?choreography 
                                    has-lisp-function)))))

(def-relation has-generic-internal-method (?web-service ?internal-method)
  "Gets the internal method associated with a web service"
  :sufficient 
  (or (and (instance ?web-service)
           (has-generic-internal-method (the-parent ?web-service) 
                                        ?internal-method))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (= ?internal-method 
              (the-class-slot-value ?web-service has-internal-method)))))

(def-relation APPLICABLE-TO-goal (?web-service-class ?goal-inst)
  :iff-def (or (not (and (= ?capability 
                            (the-class-slot-value ?web-service-class has-capability))
                         (class ?capability)
                         (= ?exp (the-class-slot-value ?capability has-assumption))
                         (not (= ?exp :nothing))))
               (and (= ?capability 
                       (the-class-slot-value ?web-service-class has-capability))
                    (class ?capability)
                    (= ?exp (the-class-slot-value ?capability has-assumption))
                    (not (= ?exp :nothing))
                    (holds ?exp  ?goal-inst))))

(def-procedure INSTANTIATE-web-service
  (?goal-inst ?web-service-type)
  :body (in-environment 
         ((?name . (new-symbol ?web-service-type)))
         (tell (append (list-of ?web-service-type ?name) nil))
         (tell (suitable-web-service ?goal-inst ?name))
         ?name))

;;;now use only the used-mediator slot of a capability of a web service
;;;liliana pointed out that this is the only place in wsmo where a wg-mediator is allowed

(def-relation can-solve-goal (?goal ?thing)
  "Returns the web services which solve a goal. Uses the mediator to find the link"
  :sufficient  
  (or (and (instance ?goal)
           (can-solve-goal (the-parent ?goal) ?thing))
      (and ;;(class ?thing) adding this clause makes can solve goal run 10 times slower!
           (subclass-of ?thing web-service)
           ;;need this class here to prevent an error in the delivery version
           ;;of the irs server
           (class ?thing) 
           (= ?capability (the-class-slot-value ?thing has-capability))
           (class ?capability)
           (= ?mediator (the-class-slot-value ?capability used-mediator))
           (class ?mediator)
           (= ?goal (the-class-slot-value ?mediator
                                          has-source-component)))))


(def-relation check-web-service-can-solve-goal (?goal ?web-service ?mediator)
  "Returns the web services which solve a goal. Uses the mediator to find the link"
  :sufficient  
  (or (and (instance ?goal)
           (check-web-service-can-solve-goal 
            (the-parent ?goal) ?web-service ?mediator))
      (and (= ?capability (the-class-slot-value ?web-service has-capability))
           (class ?capability)
           (= ?mediator (the-class-slot-value ?capability used-mediator))
           (class ?mediator)
           (= ?goal (the-class-slot-value ?mediator
                                          has-source-component)))))

(def-relation get-web-service-for-goal-from-mediator (?goal ?mediator ?web-service)
  "Returns the web services which solve a goal. Uses the mediator to find the link"
  :sufficient  
  (or (and (instance ?goal)
           (get-web-service-for-goal-from-mediator 
            (the-parent ?goal) ?mediator ?web-service))
      (and (= ?goal (the-class-slot-value ?mediator
                                          has-source-component))
           (= ?web-service (the-class-slot-value ?mediator
                                                 has-target-component))
           (class ?web-service)
           (subclass-of ?web-service web-service))))

(def-function LOCAL-wsmo-ROLE-VALUE (?goal ?role)
  :body (the ?v (holds ?role ?goal ?v)))

(def-function wsmo-ROLE-VALUE (?invokable-entity ?role)
  :body 
  (in-environment ((?value . (LOCAL-wsmo-ROLE-VALUE 
                              ?invokable-entity ?role)))
                  (if (and (= ?value :nothing)
                           (suitable-web-service ?goal-inst ?invokable-entity))
                      (wsmo-role-value ?goal-inst ?role)
                    ?value)))

;;; XXX This is a hack!  See the TODO file for the fix.
(def-rule set-goal-slot-value
    ((set-goal-slot-value ?invocation ?slot ?value) if
     (suitable-web-service ?goal ?invocation)
     (exec (set-slot-value ?goal ?slot ?value))))

(def-relation wsmo-web-service-host (?web-service ?host)
  :sufficient 
  (or (and (instance ?web-service)
           (wsmo-web-service-host (the-parent ?web-service) ?host))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (or (and (associated-publisher-information ?web-service
                                                      ?publisher-information)
                    (class ?publisher-information)
                    (= ?host (the-class-slot-value 
                              ?publisher-information has-web-service-host)))
               (and (direct-subclass-of ?web-service ?web-service-super) 
                    (wsmo-web-service-host ?web-service-super ?host))))))

(def-relation wsmo-web-service-port (?web-service ?port)
  :sufficient 
  (or (and (instance ?web-service)
           (wsmo-web-service-port (the-parent ?web-service) ?port))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (or (and (associated-publisher-information ?web-service 
                                                      ?publisher-information)
                    (class ?publisher-information)
                    (= ?port 
                       (the-class-slot-value ?publisher-information
                                             has-web-service-port)))
               (and (direct-subclass-of ?web-service ?web-service-super) 
                    (wsmo-web-service-port ?web-service-super ?port))))))

(def-relation wsmo-web-service-location (?web-service ?location)
  :sufficient 
  (or (and (instance ?web-service)
           (wsmo-web-service-location (the-parent ?web-service) 
                                      ?location))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (or (and (associated-publisher-information 
                     ?web-service
                     ?publisher-information)
                    (class ?publisher-information)
                    (= ?location (the-class-slot-value 
                                  ?publisher-information 
                                  has-web-service-location)))
               (and (direct-subclass-of ?web-service 
                                        ?web-service-super) 
                    (wsmo-web-service-location ?web-service-super ?location))))))

(def-relation HAS-soap-binding (?thing ?soap-binding)
  "This definition generalises the notion of 
   soap binding to classes as well 
    as tasks instances. "
  :sufficient  
  (or (and (instance ?thing)
           (has-soap-binding (the-parent ?thing) ?role))
      (and (class ?thing)
           (subclass-of ?thing goal)

           (or 
            (member ?soap-binding (all-class-slot-values 
                                   ?thing HAS-soap-binding))))))

(def-relation wsmo-mediator-non-functional-properties 
  (?mediator ?non-functional-properties)
  :sufficient 
  (or (and (instance ?mediator)
           (wsmo-mediator-non-functional-properties
            (the-parent ?mediator) ?mediation-service))
      (and (class ?mediator)
           (subclass-of ?mediator mediator)
           (= ?non-functional-properties
              (the-class-slot-value ?mediator 
                                    has-non-functional-properties)))))

(def-relation wsmo-mediator-mediation-service
  (?mediator ?mediation-service)
  :sufficient 
  (or (and (instance ?mediator)
           (wsmo-mediator-mediation-service
            (the-parent ?mediator) ?mediation-service))
      (and (class ?mediator)
           (subclass-of ?mediator mediator)
           (= ?mediation-service
              (the-class-slot-value ?mediator 
                                    has-mediation-service)))))

(def-relation wsmo-mediator-mapping-rules
  (?mediator ?mapping-rules)
  :sufficient 
  (or (and (instance ?mediator)
           (wsmo-mediator-mapping-rules
            (the-parent ?mediator) ?mapping-rules))
      (and (class ?mediator)
           (subclass-of ?mediator mediator)
           (= ?mapping-rules
              (the-class-slot-value ?mediator 
                                    has-mapping-rules)))))

(def-relation wsmo-mediator-source-component (?mediator ?source-component)
  :sufficient 
  (or (and (instance ?mediator)
           (wsmo-mediator-source-component
            (the-parent ?mediator) ?mediation-service))
      (and (class ?mediator)
           (subclass-of ?mediator mediator)
           (= ?source-component
              (the-class-slot-value ?mediator 
                                    has-source-component)))))

(def-relation wsmo-mediator-target-component (?mediator ?target-component)
  :sufficient 
  (or (and (instance ?mediator)
           (wsmo-mediator-target-component
            (the-parent ?mediator) ?mediation-service))
      (and (class ?mediator)
           (subclass-of ?mediator mediator)
           (= ?target-component
              (the-class-slot-value ?mediator 
                                    has-target-component)))))

(def-relation wsmo-web-service-used-mediator (?web-service ?mediator)
  :sufficient 
  (or (and (instance ?web-service)
           (wsmo-web-service-used-mediator
            (the-parent ?web-service) ?mediator))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (= ?capability (the-class-slot-value ?web-service has-capability))
           (class ?capability)
           (= ?mediator (the-class-slot-value ?capability used-mediator)))))

(def-relation wsmo-web-service-used-mediator-for-goal (?web-service ?goal ?mediator)
  :sufficient 
  (or (and (instance ?web-service)
           (wsmo-web-service-used-mediator-for-goal
            (the-parent ?web-service) ?goal ?mediator))
      (and (instance ?goal)
           (wsmo-web-service-used-mediator-for-goal
            ?web-service (the-parent ?goal) ?mediator))
      (and (class ?web-service)                       
           (subclass-of ?web-service web-service)
           (= ?capability (the-class-slot-value ?web-service has-capability))
           (class ?capability)
           (= ?mediator (the-class-slot-value ?capability used-mediator))
           (class ?mediator)
           (subclass-of ?mediator wg-mediator)
           (= ?goal (the-class-slot-value ?mediator
                                          has-source-component)))))

(def-function mediators-with-web-service-and-goal (?web-service ?goal)
  :lisp-fun 
  #'(lambda (web-service goal) 
      (internal-mediators-with-web-service-and-goal web-service goal)))

(def-relation wsmo-web-service-all-used-mediators-for-goal 
  (?web-service ?goal ?mediators)
  :sufficient 
  (or (and (instance ?web-service)
           (wsmo-web-service-used-mediator-for-goal
            (the-parent ?web-service) ?goal ?mediator))
      (and (instance ?goal)
           (wsmo-web-service-used-mediator-for-goal
            ?web-service (the-parent ?goal) ?mediator))
      (and (class ?web-service)                       
           (subclass-of ?web-service web-service)
           (= ?capability (the-class-slot-value ?web-service has-capability))
           (class ?capability)
           (= ?capability-mediators 
              (all-class-slot-values ?capability used-mediator))
           (= ?web-service-mediators (all-class-slot-values ?web-service used-mediator))
           (= ?mediators-with-web-service-and-goal 
              (mediators-with-web-service-and-goal ?web-service ?goal))
           (= ?mediators (remove-duplicates
                          (append ?capability-mediators ?web-service-mediators 
                                  ?mediators-with-web-service-and-goal))))))


(def-relation wsmo-web-service-assumption (?web-service ?assumption)
  :sufficient 
  (or (and (instance ?web-service)
           (wsmo-web-service-assumption
            (the-parent ?web-service) ?assumption))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (= ?capability (the-class-slot-value ?web-service has-capability))
           (class ?capability)
           (= ?assumption (the-class-slot-value ?capability has-assumption)))))

(def-class choreography-state ()
  ((has-remote-name :type string)
   (has-web-service-class)
   (has-web-service-instance :type web-service)
   (has-input-role-value-pairs :type list)
   (has-grounding :type list)
   (has-web-service-host :type host) 
   (has-web-service-port :type port)
   (has-web-service-location :type location)
   (has-output-type :type output-type)
   (has-input-roles-and-soap-bindings 
    :type input-roles-and-soap-bindings)
   (has-values :type list)
   (has-choreography-result :cardinality 1)))


(def-function invoke-web-service-from-rule-and-assert
  (?web-service
   ?input-role-value-pairs
   ?operation-io-mappings
   ?operation 
   ?host ?port ?location ?run-number)
  "Invoke a web service from a rule"
  :lisp-fun
  #'(lambda (web-service input-role-value-pairs 
                         operation-io-mappings operation 
                       host port location run-number)
      (let ((choreography-state 
             (findany '?x
                      `(has-run-number ?x ,run-number))))
        (internal-invoke-web-service-from-rule-and-assert
         choreography-state operation input-role-value-pairs
         web-service operation-io-mappings run-number
         host port location))))

;;received-error-message

(def-relation send-message (?operation) 
  :prove-by (exec (send-message-fun ?operation))) 

(def-function send-message-fun
  (?operation) 
  "The simplest way to invoke a web service from a rule"
  :lisp-fun
  #'(lambda (operation)
        (internal-invoke-web-service-from-rule-and-assert
         *choreography-state-name* operation)))

(def-relation has-result (?result) 
  :prove-by 
  (and (exec (unassert 
              (has-choreography-result choreography-state ?x)))
       (exec 
        (tell 
         (has-choreography-result choreography-state ?result)))))


(def-relation send-message-with-new-input-role-pairs
   (?operation ?new-input-role-value-pairs)
   :prove-by 
   (exec (send-message-with-new-input-role-pairs-fun
          ?operation ?new-input-role-value-pairs)))

(def-function send-message-with-new-input-role-pairs-fun
  (?operation ?new-input-role-value-pairs) 
  "The simplest way to invoke a web service from a rule"
  :lisp-fun
  #'(lambda (operation new-input-role-value-pairs)
        (internal-invoke-web-service-from-rule-and-assert
         'choreography-state operation 
         new-input-role-value-pairs)))

(def-relation lift-defined (?class ?web-service ?lift-function))

(def-relation lift-defined-all (?class ?lift-function))

(def-relation lower-defined (?class ?web-service ?lower-function))

(def-relation lower-defined-all (?class ?lower-function))

(def-relation skyhook-defined (?class ?skyhook-function))

(def-relation lift (?xml-string ?ontological-representation)
  :prove-by 
  (= ?ontological-representation (lift-fun ?xml-string)))

(def-function lift-fun (?xml-string)
  :lisp-fun
  #'(lambda (xml-string)
      (handler-case 
          (let ((*package* (find-package "OCML")))
            (if (stringp xml-string)
                (read-from-string xml-string)
              xml-string))
        (error
         (c)
         nil)
        (serious-condition 
         (c) nil))))

(def-relation received-message (?operation-name ?message))

(def-relation received-error (?operation-name ?error-message ?error-type))

(def-relation sent-message-input (?operation-name ?input-role-value-pairs))

(def-relation operation-input (?operation-name ?input-role-name ?input-value))

(def-function get-operation-input-value-fun (?operation-name ?input-role-name))

(def-relation init-choreography ())

(def-relation end-choreography () 
  :prove-by (exec (end-choreography-fun)))

(def-function end-choreography-fun ()
  :lisp-fun
  #'(lambda () 
      (internal-end-choreography)))


(def-function list1 (?l1 &rest ?ls)
  :constraint (and (list ?l1)(every  ?ls list))
   :lisp-fun #'list)

;;; The following classes are defined simply to stop OCML warning
;;; about them being used slot types while undefined.
(def-class host ()
    ())

(def-class port ()
    ())

(def-class location ()
    ())

(def-class axiom-definition ()
    ())

(def-class ontology ()
    ())

(def-class goal-meta-class ()
    ())

(def-class rule-packet ()
    ())

(def-class output-type ()
    ())

(def-class input-roles-and-soap-bindings ()
    ())

(def-class expression ()
    ())

(def-class mapping-rules ()
    ())
