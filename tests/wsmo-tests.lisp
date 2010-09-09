(in-package wsmo-protocol)

(defvar *wsmo-test-forms2*
  '((christoph frankfurt berlin (4 6 18 8 2004))
    (christoph frankfurt berlin)
    (alistair frankfurt berlin)
    (sinuhe frankfurt berlin)
    (liliana paris lyon)
    (john london milton-keynes)
    (sinuhe london milton-keynes)
    (sinuhe paris lyon)
    (sinuhe vienna innsbruck)
    ))

(defun test-vta ()
  (mapc #'(lambda (input) (apply 'test-vta-achieve-goal input)) *wsmo-test-forms2*))

(defun test-vta-achieve-goal (person departure destination &optional 
                                     (date-and-time '(3 4 6 18 8 2004)))
  (format t "~%~%~a going from ~a to ~a at ~a~%" person departure destination date-and-time)
  (ip::handle-achieve-goal 
   *standard-output* 
   `(WSMO-USE-CASE BUY-TRAIN-TICKET-GOAL 4 OCML::HAS-PERSON ,person 
                   OCML::HAS-DEPARTURE-STATION ,departure
                   OCML::HAS-DESTINATION-STATION ,destination
                   OCML::HAS-DATE-AND-TIME ,date-and-time) nil))

(defvar *wsmo-test-forms*
  '((ip::handle-post-soap-service 'user::goal-info *standard-output* "" '(wsmo-test exchange-rate-web-service))
    (ip::handle-post-soap-service 'user::goal-info *standard-output* "" '(wsmo-test european-exchange-rate-web-service))

    ;;(ip::handle-post-soap-service 'user::publish-wsmo-web-service *standard-output* "" '(wsmo-test european-exchange-rate-web-service ((has_source_currency "sexpr") (has_target_currency "sexpr")) sexpr exchange-rate-fun "localhost" 3001 "/soap"))
    

    (ip::handle-post-soap-service 
     'user::save-web-service-message-exchange-pattern *standard-output* "" 
     '("john" test-web-service-lisp wsmo-test message-exchange-pattern9999))


    

    (ip::handle-post-soap-service 
     'user::publish-wsmo-web-service-all-values *standard-output* "" 
     '("john" wsmo-test test-web-service-all 
              ;;((has_source_currency "sexpr") (has_target_currency "sexpr")) sexpr 
              "localhost" 3001 "/soap" message-exchange-pattern1 exchange-rate-fun java-class1 java-method1 
                            http-get-request-url1 
                            http-get-request-variables1 wsdl-file-name1 
                            wsdl-operation-name1 wsdl-port-type1 
                            wsdl-port-type-name-space1 
                            wsdl-web-service-provider-type1))

    (ip::handle-post-soap-service 
     'user::publish-wsmo-web-service *standard-output* "" 
     '("john" wsmo-test test-web-service-web-services 
              ;;((has_source_currency "sexpr") (has_target_currency "sexpr")) sexpr
              "localhost" 3001 "/soap" message-exchange-pattern1  wsdl-file-name1 
                            wsdl-operation-name1 wsdl-port-type1 
                            wsdl-port-type-name-space1 
                            wsdl-web-service-provider-type1))

    (ip::handle-post-soap-service 
     'user::publish-wsmo-lisp-function *standard-output* "" 
     '("john" wsmo-test test-web-service-lisp 
              ;;((has_source_currency "sexpr") (has_target_currency "sexpr")) sexpr
              "localhost" 3001 "/soap" message-exchange-pattern1 exchange-rate-fun))

    (ip::handle-post-soap-service 
     'user::get-web-service-lisp-function *standard-output* "" 
     '(test-web-service-lisp wsmo-test))

    (ip::handle-post-soap-service 
     'user::publish-wsmo-java-method *standard-output* "" 
     '("john" wsmo-test test-web-service-java-method 
              ;;((has_source_currency "sexpr") (has_target_currency "sexpr")) sexpr 
              "www.JavaServer.com" 3001 "/soap" message-exchange-pattern1 java-class1 java-method1))

    (ip::handle-post-soap-service 
     'user::publish-wsmo-http-get-request *standard-output* "" 
     '("john" wsmo-test test-web-service-http 
              ;;((has_source_currency "sexpr") (has_target_currency "sexpr")) sexpr 
              "localhost" 3001 "/soap" message-exchange-pattern1 
                            http-get-request-url1 
                            http-get-request-variables1))

    (ip::handle-post-soap-service 'user::get-web-service-message-exchange-pattern
                                  *standard-output* "" 
                                  '(test-web-service-delete wsmo-test))

    (ip::handle-post-soap-service 'user::save-goal-description *standard-output* "" '("john" exchange-rate-goal2 wsmo-test goal 2 has_source_currency currency sexpr has_target_currency currency sexpr exchange-rate integer float mediator99 (the-exchange-rate has_source_currency has_target_currency) (invoice-sent has_source_currency has_target_currency) core-non-functional-properties-description1))
    (ip::handle-post-soap-service 'user::get-goal-description *standard-output* "" '(exchange-rate-goal wsmo-test))
    (ip::handle-post-soap-service 'user::get-goal-description *standard-output* "" '(exchange-rate-goal2 wsmo-test))
    (ip::handle-post-soap-service 'user::get-goal-description *standard-output* "" '(exchange-rate-goal-child wsmo-test))
    (ip::handle-post-soap-service 'user::get-non-local-goal-description *standard-output* "" '(exchange-rate-goal-child wsmo-test))
    (ip::handle-post-soap-service 'user::delete-goal-description *standard-output* "" '("john" exchange-rate-goal2 wsmo-test))
    ;;;after deleting exchange-rate-goal2 we add it again to put the file back to its 
    ;;original state
    (ip::handle-post-soap-service 'user::save-goal-description *standard-output* "" '("john" exchange-rate-goal2 wsmo-test goal 2 has_source_currency currency sexpr has_target_currency currency sexpr exchange-rate integer float mediator99 (the-exchange-rate has_source_currency has_target_currency) (invoice-sent has_source_currency has_target_currency) core-non-functional-properties-description1))

    (ip::handle-post-soap-service 'user::save-web-service-description *standard-output* "" '("john" exchange-rate-web-service2 wsmo-test web-service 2 has_source_currency currency  has_target_currency currency exchange-rate integer exchange-rate-capability2  exchange-rate-interface2 exchange-rate-oo-mediator2 web-service-functional-properties-description1))
    (ip::handle-post-soap-service 'user::get-web-service-description *standard-output* "" '(exchange-rate-web-service2 wsmo-test))
    (ip::handle-post-soap-service 'user::delete-web-service-description *standard-output* "" '("john" exchange-rate-web-service2 wsmo-test))
    (ip::handle-post-soap-service 'user::save-web-service-description *standard-output* "" '("john" exchange-rate-web-service2 wsmo-test web-service 2 has_source_currency currency  has_target_currency currency exchange-rate integer exchange-rate-capability2  exchange-rate-interface2 exchange-rate-oo-mediator2 web-service-functional-properties-description1))

    (ip::handle-post-soap-service 
     'user::save-capability-description *standard-output* "" 
     '("john" 
       exchange-rate-capability2 wsmo-test capability mediator20 
       (kappa (x) (and (currency-ok source_currency)(currency-ok target_currency)))
       (kappa (?goal) (= (role-value ?goal 'has_source_currency) pound))
       (kappa (x) (currency_available target_currency)) (kappa (x) (invoice_sent x)) 
       ;;exchange-rate-goal2
       ;;(kappa (?goal) (= (role-value ?goal 'has_source_currency) pound))
       web-service-functional-properties-description1))
    (ip::handle-post-soap-service 'user::get-capability-description *standard-output* "" '(exchange-rate-capability2 wsmo-test))
    (ip::handle-post-soap-service 'user::delete-capability-description *standard-output* "" '("john" exchange-rate-capability2 wsmo-test))
    (ip::handle-post-soap-service 
     'user::save-capability-description *standard-output* "" 
     '("john" exchange-rate-capability2 wsmo-test capability mediator20 
              (kappa (x) (and (currency-ok source_currency)(currency-ok target_currency))) 
              (kappa (x) (exchange_rate_valid exchange_rate)) 
              (kappa (?goal) (= (role-value ?goal 'has_source_currency) pound))
              (kappa (x) (invoice_sent x)) 
              ;;exchange-rate-goal2
              ;;(kappa (?goal) (= (role-value ?goal 'has_source_currency) pound))
              web-service-functional-properties-description1))

    (ip::handle-post-soap-service 'user::save-interface-description *standard-output* "" '("john" exchange-rate-interface2 wsmo-test interface exchange-rate-mediator2 exchange-rate-choreography2 exchange-rate-orchestration2 web-service-functional-properties-description1))
    (ip::handle-post-soap-service 'user::get-interface-description *standard-output* "" '(exchange-rate-interface2 wsmo-test ))
    (ip::handle-post-soap-service 'user::delete-interface-description *standard-output* "" '("john" exchange-rate-interface2 wsmo-test ))
    (ip::handle-post-soap-service 'user::save-interface-description *standard-output* "" '("john" exchange-rate-interface2 wsmo-test interface exchange-rate-mediator2 exchange-rate-choreography2 exchange-rate-orchestration2 web-service-functional-properties-description1))

    (ip::handle-post-soap-service 'user::save-mediator-description *standard-output* "" '("john" exchange-rate-mediator2 wsmo-test wg-mediator mediator44 exchange-rate-goal2 exchange-rate-web-service2 mediation-service-fun (kappa (x) (european_exchange-rate x)) web-service-functional-properties-description1))
    (ip::handle-post-soap-service 'user::get-mediator-description *standard-output* "" '(exchange-rate-mediator2 wsmo-test))
    (ip::handle-post-soap-service 'user::delete-mediator-description *standard-output* "" '("john" exchange-rate-mediator2 wsmo-test))
    (ip::handle-post-soap-service 'user::save-mediator-description *standard-output* "" '("john" exchange-rate-mediator2 wsmo-test wg-mediator mediator44 exchange-rate-goal2 exchange-rate-web-service2 mediation-service-fun (kappa (x) (european_exchange-rate x)) web-service-functional-properties-description1))

    (ip::handle-post-soap-service 
     'user::save-core-non-functional-properties-description
     *standard-output* ""
     '("john" core-non-functional-properties-description1 wsmo-test
              core-non-functional-properties
              "this is the title" 
              "John Domingue" subject122 description1 publisher1 contributor1 date1 type1 format1 
        identifier1 source1 language1 has-relation1 coverage1 rights1 version1))
    (ip::handle-post-soap-service 
     'user::get-core-non-functional-properties-description
     *standard-output* ""
     '(core-non-functional-properties-description1 wsmo-test))
    (ip::handle-post-soap-service 
     'user::delete-core-non-functional-properties-description
     *standard-output* ""
     '("john" core-non-functional-properties-description1 wsmo-test))
    (ip::handle-post-soap-service 
     'user::save-core-non-functional-properties-description
     *standard-output* ""
     '("john" core-non-functional-properties-description1 wsmo-test
              core-non-functional-properties
              "this is the title" 
              "John Domingue" subject122 description1 publisher1 contributor1 date1 type1 format1 
        identifier1 source1 language1 has-relation1 coverage1 rights1 version1))

    (ip::handle-post-soap-service 
     'user::save-web-service-non-functional-properties-description
     *standard-output* ""
     '("john" web-service-functional-properties-description1 wsmo-test
              web-service-non-functional-properties
              "this is the title" 
              "John Domingue" subject133 description1 publisher1 contributor1 date1 type1 format1 
        identifier1 source1 language1 has-relation1 coverage1 rights1 version1
        performance1 reliability1 
                                             security1 scalability1
                                             robustness1 accuracy1 
                                             transactional1 trust1 
                                             financial1 
                                             network-related-quality-of-service1))
    (ip::handle-post-soap-service 
     'user::get-web-service-non-functional-properties-description
     *standard-output* ""
     '(web-service-functional-properties-description1 wsmo-test))
    (ip::handle-post-soap-service 
     'user::delete-web-service-non-functional-properties-description
     *standard-output* ""
     '("john" web-service-functional-properties-description1 wsmo-test))
    (ip::handle-post-soap-service 
     'user::save-web-service-non-functional-properties-description
     *standard-output* ""
     '("john" web-service-functional-properties-description1 wsmo-test
              web-service-non-functional-properties
              "this is the title" 
              "John Domingue" subject133 description1 publisher1 contributor1 date1 type1 format1 
        identifier1 source1 language1 has-relation1 coverage1 rights1 version1
        performance1 reliability1 
                                             security1 scalability1
                                             robustness1 accuracy1 
                                             transactional1 trust1 
                                             financial1 
                                             network-related-quality-of-service1))



    (ip::handle-post-soap-service 
     'user::save-choreography-description
     *standard-output* ""
     '("john" choreography99 wsmo-test
              choreography
              "host.open.ac.uk" 3001 "/soap"
                         has-message-exchange-pattern1 has-lisp-function1 
                         has-java-class1 has-java-method1 
                         has-http-get-request-url1 
                         has-http-get-request-variables1 has-wsdl-file-name1 
                         has-wsdl-operation-name1 has-wsdl-port-type1 
                         has-wsdl-port-type-name-space1 
                         has-wsdl-web-service-provider-type1))

    (ip::handle-post-soap-service 
     'user::get-choreography-description
     *standard-output* ""
     '(choreography99 wsmo-test))
    (ip::handle-post-soap-service 
     'user::delete-choreography-description
     *standard-output* ""
     '("john" choreography99 wsmo-test))
    (ip::handle-post-soap-service 
     'user::save-choreography-description
     *standard-output* ""
     '("john" choreography99 wsmo-test
              choreography
              "host.open.ac.uk" 3001 "/soap"
                         has-message-exchange-pattern1 has-lisp-function1 
                         has-java-class1 has-java-method1 
                         has-http-get-request-url1 
                         has-http-get-request-variables1 has-wsdl-file-name1 
                         has-wsdl-operation-name1 has-wsdl-port-type1 
                         has-wsdl-port-type-name-space1 
                         has-wsdl-web-service-provider-type1))


    
    (ip::handle-post-soap-service 
     'user::save-orchestration-description
     *standard-output* ""
     '("john" orchestration99 wsmo-test
              orchestration
              problem-solving-pattern1))

    (ip::handle-post-soap-service 
     'user::get-orchestration-description
     *standard-output* ""
     '(orchestration99 wsmo-test))
    (ip::handle-post-soap-service 
     'user::delete-orchestration-description
     *standard-output* ""
     '("john" orchestration99 wsmo-test))
    (ip::handle-post-soap-service 
     'user::save-orchestration-description
     *standard-output* ""
     '("john" orchestration99 wsmo-test
              orchestration
              problem-solving-pattern1))
    
#| can only run the achieve goals below if services have been published using 
(register-wsmo-services) in the dip-services image
    (ip::handle-post-soap-service 'user::achieve-goal *standard-output* "" '(wsmo-test exchange-rate-goal 2 ocml::has_source_currency ocml::euro ocml::has_target_currency ocml::us-dollar)))

(ip::handle-post-soap-service 'user::achieve-goal *standard-output* "" '(wsmo-test exchange-rate-goal 2 ocml::has_source_currency ocml::yen ocml::has_target_currency ocml::us-dollar)))
(ip::handle-post-soap-service 'user::achieve-goal *standard-output* "" '(wsmo-test exchange-rate-goal 2 ocml::has_source_currency ocml::rupee ocml::has_target_currency ocml::us-dollar)))

|#

    ))


;;;a function to test the irs wsmo protocol
(defun test-wsmo-protocol (&optional (test-forms *wsmo-test-forms*))
  (let ((i 0))
    (dolist (form test-forms)
      (incf i)
      (run-wsmo-test-form form i))))

(defun run-wsmo-test-form (form test-number)
  (format t "Test ~d: ~(~s~)~%" test-number form)
  (eval form)
  (format t "~%~%"))
  

(defun ocml::exchange-rate-fun (ontology web-service)
  ;;(setf xxo ontology xxw web-service)
  (declare (ignore ontology  web-service))
  )

(in-package #:ip)

;;; Should get the file from the server, upload it, create the
;;; ontology and then fail to load the resulting ontology (because
;;; it's a news file, not ocml).

(upload 'test-ip nil :domain 'ocml "http://localhost:8080/irs/news"
        'new "dave" "dave" "enrico mauro" *standard-output*)
