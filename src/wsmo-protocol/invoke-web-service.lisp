;;; Copyright © 2010 The Open University

(in-package wsmo-protocol)

(defun generate-service-soap-name (ontology web-service)
  (format nil "~(~a_~a~)" ontology 
          (ocml::name (ocml::parent-class (ocml::find-current-instance web-service)))))


;;add reverse to all of the input roles to get the roles in the order defined

(defun output-role-with-soap-binding (x)
  (web-onto::findany '?x 
                 `(ocml::has-wsmo-output-soap-binding ,x ?x)))

(defun output-roles-with-soap-binding (x)
  (ocml::setofall '?x 
                 `(ocml::has-wsmo-output-soap-binding ,x ?x)))

(defun local-output-roles-with-soap-bindings (x)
  (web-onto::findany '?x 
                 `(ocml::all-local-wsmo-output-roles-with-soap-bindings ,x ?x)))

(defun output-role-soap-type (x output-role non-local-p)
  (let ((output-roles-with-soap-bindings
         (if non-local-p
             (output-roles-with-soap-binding x)
           (local-output-roles-with-soap-bindings x))))
    (second (assoc output-role output-roles-with-soap-bindings))))

(defun input-roles (x)
  ;;check (reverse (ocml::setofall '?x `(ocml::HAS-wsmo-INPUT-ROLE ,x ?x)))
  (ocml::findany '?x `(ocml::all-wsmo-input-roles ,x ?x)))

(defun output-role (x)
  (web-onto::findany '?x `(ocml::HAS-wsmo-output-ROLE ,x ?x)))

(defun local-input-roles (x)
  (reverse (web-onto::findany '?x `(ocml::all-local-wsmo-input-roles ,x ?x))))

(defun local-output-role (x)
  (car (web-onto::findany '?x `(ocml::all-local-wsmo-output-roles ,x ?x))))

(defun generic-get-internal-method (x)
  (web-onto::findany '?x `(ocml::has-generic-internal-method ,x ?x)))

(defun invoke-service (ontology web-service) ;;goal-type 
  ;;(push web-service www)
  (ocml:with-ontology (ontology)
    (let* ((host (web-onto::findany '?x `(ocml::wsmo-web-service-host ,web-service ?x)))
           (port (web-onto::findany '?x `(ocml::wsmo-web-service-port ,web-service ?x)))
           (location (web-onto::findany '?x `(ocml::wsmo-web-service-location ,web-service ?x)))
           (earthing (web-onto::findany '?x `(ocml::associated-earthing ,web-service ?x)))
           (input-roles (input-roles web-service))
           (output-role-with-soap-binding
            (output-role-with-soap-binding web-service))
           (output-type (intern (string-upcase (second output-role-with-soap-binding))
                                (find-package "API-SOAP-OLD")))
           (orchestration-execution-pattern (get-orchestration-execution-pattern web-service)))
      (cond ((and orchestration-execution-pattern (not (ocml:nothing? orchestration-execution-pattern)))
             (run-orchestration orchestration-execution-pattern web-service))
            ((or host earthing)
	     (internal-invoke-service
		     web-service
		     (generate-service-soap-name ontology web-service)
		     input-roles
		     (let ((input-role-values
			    (mapcar #'(lambda (input-role)
					(ocml::findany
					 '?x
					 `(= ?x (ocml::wsmo-role-value ,web-service ,input-role))))
				    input-roles)))
		       input-role-values)
		     host port location output-type))
            ;;;if no published web service try calling an internal function
            ((generic-get-internal-method web-service)
             (let ((internal-method (generic-get-internal-method web-service)))
               (when internal-method
                 (cond ((ocml-function-p internal-method)
                        (ocml::ocml-eval-gen `(,internal-method ,ontology ,web-service)))
                       ((fboundp internal-method)
                        (funcall internal-method ontology web-service))))))))))

(defun run-orchestration (orchestration-execution-pattern web-service)
  (let* ((*current-orchestration-web-service* web-service)
         (*achieve-goal-results* nil)
         (*internal-goal-instances* nil))
    ;;(ocml::ocml-apply orchestration-execution-pattern (list ontology web-service))))))))))
    (catch 'ocml::ocml-control
      (let ((internal-result nil))
        (dolist (form orchestration-execution-pattern)
          (setf internal-result (ocml::ocml-eval-gen form)))
        internal-result))))


(defun get-orchestration-execution-pattern (x)
  (ocml::findany '?x `(ocml::wsmo-web-service-orchestration-body2 ,x ?x)))

(defun ocml-function-p (x)
  (ocml::get-function x))

(defgeneric invoke-web-service-operation (grounding-type
                                          grounding operation name output-type
                                          input-roles-and-soap-bindings
                                          values host port location service))

(defmethod invoke-web-service-operation (grounding-type 
                                         grounding operation
                                         name output-type input-roles-and-soap-bindings
                                         values host port location service)
  (error "Unknown grounding type: ~a" grounding-type))

(defmethod invoke-web-service-operation ((grounding-type (eql 'ocml::grounded-to-lisp))
                                         grounding operation
                                         name output-type input-roles-and-soap-bindings
                                         values host port location service)
  (iu::soap-http-client
   name (cons (list 'output-type "string") input-roles-and-soap-bindings)
   (cons output-type values)
   :host host :port port 
   :publisher-location location 
   :output-type output-type))

(defmethod invoke-web-service-operation ((grounding-type (eql 'ocml::grounded-to-http))
                                         grounding operation
                                         name output-type input-roles-and-soap-bindings
                                         values host port location service-instance)
  (let* ((implementation-info
          (get-implementation-info-for-web-service-operation grounding operation))
         (session-variables (car implementation-info))
         (http-variables (second implementation-info))
         (session-variables-and-values
          (mapcar #'(lambda (variable value)
                      (list (if (symbolp variable)
                                (string-downcase (symbol-name variable))
                                variable)
                            value))
                  session-variables values))
         (http-variables-and-values
          (mapcar #'(lambda (variable value)
                      (list (if (symbolp variable)
                                (string-downcase (symbol-name variable))
                                variable)
                            value))
                  http-variables (subseq values (length session-variables))))
         (rest (if session-variables
                   (format nil "~a;~{~{~a=~a~}~}?~{~{~a=~a&~}~}~{~{~a=~a~}~}" location
                           session-variables-and-values (butlast http-variables-and-values)
                           (last http-variables-and-values))
                   (format nil "~a?~{~{~a=~a&~}~}~{~{~a=~a~}~}" location
                           (butlast http-variables-and-values) (last http-variables-and-values))))
         (service (ocml::name (ocml::parent-class service-instance)))
         (uri (compose-uri host port rest)))
    (api-js:event :rpc-call service uri)
    (let ((content (utilities:http-request uri)))
      content)))

(defun compose-uri (host port path)
  (format nil "http://~A:~A~A" host port path))

(defmethod invoke-web-service-operation ((grounding-type (eql 'ocml::grounded-to-java))
                                         grounding operation
                                         name output-type input-roles-and-soap-bindings
                                         values host port location service)
  (let ((implementation-info 
         (get-implementation-info-for-web-service-operation grounding operation)))
    (destructuring-bind (java-class java-method) implementation-info
      (iu::soap-http-client
       name (append (list (list 'output-type "string") 
                          (list 'java-class "string") (list 'java-method "string"))
                    input-roles-and-soap-bindings)
       (append (list output-type java-class java-method) values)
       :host host :port port
       :publisher-location location 
       :output-type output-type))))

(defmethod invoke-web-service-operation ((grounding-type (eql 'ocml::grounded-to-wsdl))
                                         grounding operation
                                         name output-type input-roles-and-soap-bindings
                                         values host port location service)
  (let ((implementation-info 
         (get-implementation-info-for-web-service-operation grounding operation)))
    (destructuring-bind (wsdl-url operation-name port-type port-type-name-space 
                                  web-service-provider-type) implementation-info
      (iu::soap-http-client
       name (append (list (list 'output-type "string") 
                          (list 'wsdl-url "string") 
                          (list 'operation-name "string") 
                          (list 'port-type "string") 
                          (list 'port-type-name-space "string") 
                          (list 'web-service-provider-type "string")) 
                    input-roles-and-soap-bindings)
       (append (list output-type wsdl-url operation-name port-type 
                     port-type-name-space web-service-provider-type) 
               values)
       :host host :port port
       :publisher-location location 
       :output-type output-type))))

(defun get-output-role-type (web-service)
  (let ((local-output-role-type 
         (car (second (web-onto::findany '?x `(ocml::wsmo-local-output-role-with-types ,web-service ?x))))))
    (if local-output-role-type
        local-output-role-type
      (car (second (car (last
                         (web-onto::findany '?x `(ocml::wsmo-output-role-with-types ,web-service ?x)))))))))

(defun invoke-soap-client-with-visualization
       (web-service input-role-value-pairs input-roles-and-soap-bindings
                    values host port location output-type
                    grounding operation)
  (handler-case
      (let* ((original-ontology ocml::*current-ontology*) ;; XXX Why?
             (output-role-type (get-output-role-type web-service))
             (lift-function (grounding:get-lift-function output-role-type web-service))
             (result (internal-invoke-soap-client-with-visualization
                      web-service input-role-value-pairs input-roles-and-soap-bindings
                      values host port location output-type
                      grounding operation)))
        (if lift-function
            (ocml:with-ontology ((ocml::name original-ontology))
              (funcall lift-function result))
            result))))

(defun get-input-role-type (web-service input-role)
  (caadr (assoc input-role
         (web-onto::findany 
          '?x `(ocml::all-wsmo-input-roles-with-types ,web-service ?x)))))

(defun internal-invoke-soap-client-with-visualization
       (web-service input-role-value-pairs input-roles
                    values host port location output-type
                    grounding operation)
  (let* ((original-ontology ocml::*current-ontology*)
         (service (ocml::name (ocml::parent-class (ocml::find-current-instance web-service))))
         (name (make-invocation-name-for-web-service-operation
                grounding operation))
         (values-for-invocation
          (mapcar #'(lambda (input-role value)
                      (let ((lower-function (grounding:get-lower-function (get-input-role-type web-service input-role)
                                                                          web-service)))
                        (if lower-function
                            ;; XXX How on Earth do original-ontology and
                            ;; *current-ontology* end up different?  Can
                            ;; we safely use with-ontology here?
                            (with-ontology ((ocml::name original-ontology))
                              (funcall lower-function value))
                          value)))
                  input-roles values)))
    #+:irs-lispworks
    (visualiser:sending-remote-procedure-call-message
     web-service input-role-value-pairs name)
    (handler-case
        (let ((result
               (invoke-web-service-operation
                (get-operation-grounding-type grounding operation)
                grounding operation
                name output-type input-roles values-for-invocation host port location
                (ocml::find-current-instance web-service))))
	  #+:irs-lispworks
          (visualiser:result-of-remote-procedure-call web-service result)
          (irs.api.javascript:event :rpc-return web-service result)
          result)
      (irs.grounding::<grounding-fault> (c)
        (irs.api.javascript:event :rpc-fault
                                  (irs.grounding::service-of c)
                                  (irs.grounding::service-response-of c))
        (error c))
      (t (c)
        (irs.api.javascript:event :rpc-fault service (format nil "~A" c))
        (error (make-condition
                'irs.grounding::<grounding-fault> :other-cause c
                :service service))))))

(defun internal-invoke-service (web-service name input-roles
                                            values host port location 
                                            output-type)
  (let ((*package* (find-package "OCML"))
        (input-role-value-pairs
         (mapcar #'(lambda (input-role value)
                     (list input-role value))
                 input-roles values))
        (choreography
         (web-onto::findany
          '?x
          `(ocml::associated-choreography ,web-service ?x)))
        (guarded-transitions
         (web-onto::findany
          '?x
          `(ocml::associated-guarded-transitions ,web-service ?x)))
        (associated-grounding
         (web-onto::findany 
          '?m  
          `(ocml::associated-grounding ,web-service  ?m))))
    (cond ((and choreography guarded-transitions)
           (run-choreography choreography name web-service input-role-value-pairs
                             host port location output-type 
                             values input-roles
                             associated-grounding))
          (t 
           (let ((result 
                  (invoke-soap-client-with-visualization
                   web-service input-role-value-pairs input-roles
                   values host port location output-type
                   associated-grounding 
                   ;;no longer hardwire this but take the first operation name 
                   ;;'ocml::normal
                   (operation-mappings-name 
                    (wsmo-grounding-operation-mappings (car associated-grounding))))))
             ;;note that lift always does a read from string
             ;;taking this out may cause problems
             ;;(ocml::findany '?x `(ocml::lift ,result ?x))
             result)))))
