(in-package #:wsmo-protocol)

(defun generate-slot-value (x)
  (if (stringp x) 
      x
    (if (listp x)
        (ip::make-ocml-list x)
      (let ((*package* (find-package "OCML")))
        (read-from-string 
         (format nil "~a" x))))))

(defun get-class-parent (class-name)
  ;;(web-onto::findany '?x `(ocml::superclass-of ?x ,task-name)))
  (let ((class (ocml::get-domain-class class-name)))
    (when class
      (mapcar #'ocml::name (ocml::direct-domain-superclasses class)))))


(defun get-input-roles-with-soap-types (goal-name non-local-p)
  (let ((input-role-names 
         (if non-local-p
             (input-roles goal-name)
           (local-input-roles goal-name))))
    (mapcan #'(lambda (input-role-name)
                (list input-role-name 
                      (web-onto::findany 
                       '?x 
                       `(ocml::class-slot-type ,goal-name
                                               ,input-role-name ?x))
                      (ip::input-role-soap-type goal-name input-role-name non-local-p)))
            input-role-names)))

(defun get-input-roles-with-types (item-name non-local-p)
  (let ((input-role-names 
         (if non-local-p
             (input-roles item-name)
           (local-input-roles item-name))))
    (mapcan #'(lambda (input-role-name)
                (list input-role-name 
                      (web-onto::findany 
                       '?x 
                       `(ocml::class-slot-type ,item-name
                                               ,input-role-name ?x))))
            input-role-names)))

(defun get-output (goal-name non-local-p)
  (let ((output-role 
         (if non-local-p
             (output-role goal-name)
           (local-output-role goal-name))))
    (list output-role
          (web-onto::findany '?x 
                             `(ocml::class-slot-type ,goal-name
                                                     ,output-role ?x))  
          (output-role-soap-type goal-name output-role non-local-p))))

(defun ip::internal-get-web-service-message-exchange-pattern (ontology web-service)
  ;;(setf oo ontology ww web-service)
  (ocml::select-ontology ontology)
  (web-onto::findany '?x `(ocml::associated-message-exchange-pattern ,web-service ?x)))

(defun ip::internal-get-web-service-operation-io-mappings (ontology web-service)
  ;;(setf oo ontology ww web-service)
  (ocml::select-ontology ontology)
  (web-onto::findany '?x `(ocml::associated-operation-io-mapping ,web-service ?x)))

(defun ip::internal-get-web-service-lisp-function (ontology web-service)
  (ocml::select-ontology ontology)
  (web-onto::findany '?x `(ocml::web-service-lisp-function ,web-service ?x)))

(defun get-post-condition (goal-name)
  (ip::get-class-slot-value goal-name 'ocml::has-post-condition))

(defun get-mediator (goal-name)
  (ip::get-class-slot-value goal-name 'ocml::used-mediator))

(defun get-mediators (goal-name)
  (ip::get-class-slot-values goal-name 'ocml::used-mediator))

(defun get-capability (web-service-name)
  (ip::get-class-slot-value web-service-name 'ocml::has-capability))

(defun get-interface (web-service-name)
  (ip::get-class-slot-value web-service-name 'ocml::has-interface))

(defun get-internal-method (web-service-name)
  (ip::get-class-slot-value web-service-name 'ocml::has-internal-method))

(defun get-effect (goal-name)
  (ip::get-class-slot-value goal-name 'ocml::has-effect))

(defun get-non-functional-properties (x)
  (ip::get-class-slot-value x 'ocml::has-non-functional-properties))

(defun get-all-non-functional-property-values (x)
  (let* ((non-functional-property-class-name 
          (ip::get-class-slot-value x 'ocml::has-non-functional-properties))
         (found-one-value-p nil) result)
    (cond ((and non-functional-property-class-name
                (ocml::get-domain-class non-functional-property-class-name))
           (setf result 
                 (mapcar #'(lambda (property)
                       (let ((value (ip::get-class-slot-value non-functional-property-class-name property)))
                         (if value
                             (progn (setf found-one-value-p t)
                               value)
                           " ")))
                   *non-functional-property-names*))
           (if found-one-value-p
               result
             nil))
          (t nil)))) ;; (make-list (length *non-functional-property-names*) :initial-element " ")))))

(defun get-all-non-functional-property-values-as-string (x)
  (let ((result (get-all-non-functional-property-values x)))
    (if result
        (format nil "(~{~s~})" result)
      "")))
           

(defun get-pre-condition (capability-name)
  (ip::get-class-slot-value capability-name 'ocml::has-pre-condition))

(defun get-assumption (capability-name)
  (ip::get-class-slot-value capability-name 'ocml::has-assumption))

;;(defun get-goal-solved (capability-name)
;;  (ip::get-class-slot-value capability-name 'ocml::solves-goal))

(defun get-applicability-condition (capability-name)
  (ip::get-class-slot-value capability-name 'ocml::has-applicability-condition))

(defun get-choreography (interface-name)
  (ip::get-class-slot-value interface-name 'ocml::has-choreography))

(defun get-choreography-grounding (choreography)
  (ip::get-class-slot-value choreography 'ocml::has-grounding))

(defun get-choreography-guarded-transitions (choreography)
  (ip::get-class-slot-value choreography 'ocml::has-guarded-transitions))

(defun get-orchestration (interface-name)
  (ip::get-class-slot-value interface-name 'ocml::has-orchestration))

(defun get-orchestration-problem-solving-pattern (orchestration)
  (ip::get-class-slot-value orchestration 'ocml::has-problem-solving-pattern))

(defun get-problem-solving-pattern-body (problem-solving-pattern)
  (ip::get-class-slot-value problem-solving-pattern 'ocml::has-body))

(defun get-orchestration-body (orchestration)
  (let ((problem-solving-pattern (get-orchestration-problem-solving-pattern orchestration)))
    (get-problem-solving-pattern-body problem-solving-pattern)))

(defun get-source (mediator-name)
  (ip::get-class-slot-value mediator-name 'ocml::has-source-component))

(defun get-target (mediator-name)
  (ip::get-class-slot-value mediator-name 'ocml::has-target-component))

(defun get-mediation-service (mediator-name)
  (ip::get-class-slot-value mediator-name 'ocml::has-mediation-service))

(defun get-reduction (mediator-name)
  (ip::get-class-slot-value mediator-name 'ocml::has-reduction))

(defun get-mapping-rules (mediator-name)
  (ip::get-class-slot-value mediator-name 'ocml::has-mapping-rules))

(defun handle-delete-wsmo-description-and-components
       (stream soap-values item-type &rest slot-components)
  (let (user-name 
        ontology item-name)
    (setf user-name (ip::make-ocml-symbol (pop soap-values))
          item-name (ip::make-ocml-symbol (pop soap-values))
          ontology (ip::make-ocml-symbol (pop soap-values)))
    (ocml::select-ontology ontology)
    (cond ((ocml::get-domain-class item-name)
           (internal-handle-delete-wsmo-description-and-components 
            user-name item-name item-type ontology slot-components stream)
           (internal-handle-delete-wsmo-description user-name item-name item-type ontology stream))
          (t (format nil "~a is not a class" item-name)))))

(defun internal-handle-delete-wsmo-description-and-components 
       (user-name item-name item-type ontology slot-components stream)
  (dolist (slot-component slot-components)
    (handle-delete-wsmo-description-component user-name ontology item-name item-type 
                                              slot-component stream))
    ;;
    )

(defun handle-delete-wsmo-description-component
       (user-name ontology item-name item-type slot-component stream)
  (let* ((slot-name (if (listp slot-component)
                        (car slot-component)
                      slot-component))
         (component-item-type (when (listp slot-component)
                                (second slot-component)))
         (slot-sub-components (when (listp slot-component) (cddr slot-component)))
         (all-values (ocml::findany '?x 
                                    `(= ?x (ocml::all-class-slot-values ,item-name ,slot-name)))))
    (dolist (value all-values)
      (when (delete-component-p item-name item-type value slot-name)
        (internal-handle-delete-wsmo-description-and-components
         user-name value component-item-type ontology slot-sub-components stream)
        (internal-handle-delete-wsmo-description user-name value 
                                                 component-item-type ontology stream)))))

(defun delete-component-p (item-name item-type value slot-name)
  (let ((all-related-classes 
         (remove item-name
                 (ocml::setofall '?x 
                                 `(ocml::subclass-of ?x 
                                                     ,(ip::make-ocml-symbol item-type)))))
        (other-places-used nil)
        (subclasses (ocml::setofall '?x `(ocml::subclass-of ?x ,value))))
    (dolist (class all-related-classes)
      (when (find value (ocml::findany 
                         '?x 
                         `(= ?x (ocml::all-class-slot-values ,class ,slot-name))))
        (push class other-places-used)))
    (not (or other-places-used subclasses))))

(defun handle-delete-wsmo-description (stream soap-values item-type)
  (let (user-name 
        ontology item-name)
    (setf user-name (ip::make-ocml-symbol (pop soap-values))
          item-name (ip::make-ocml-symbol (pop soap-values))
          ontology (ip::make-ocml-symbol (pop soap-values)))
    (internal-handle-delete-wsmo-description user-name item-name item-type ontology stream)))

(defun internal-handle-delete-wsmo-description (user-name item-name item-type ontology stream)
  (let ((response-string-header 
         (format nil "DELETE-~a-DESCRIPTION-RESPONSE" item-type)))
    (cond ((ok-to-edit-create-item-in-ontology 
            item-name ontology)
           (multiple-value-bind (result ok-p)
               (delete-wsmo-description
                user-name ontology item-name)
             (iu::send-soap-response2 
              response-string-header
              (list result)
              (if ok-p 
                  `((result "string"))
                `((warning "string")))
              :stream stream)))
          (t (ip::send-not-ok-to-edit item-name item-type ontology 'deleted
                                      response-string-header
                                      stream)))))

(defun handle-get-wsmo-description (stream soap-values item-type &optional property-names)
  (let (ontology item-name)
    (setf item-name (ip::make-ocml-symbol (pop soap-values))
          ontology (ip::make-ocml-symbol (pop soap-values)))
    (cond ((ocml::get-ontology ontology)
           ;;ontology may be a sub-ontology of the interface - e.g. a psm ontology
           (ocml::with-ocml-thread-safety ()
             (ocml::select-ontology ontology)
             (let ((structure (ocml::get-domain-class item-name)))
               (cond (structure
                      (setf ontology (ocml::name
                                      (iu::home-ontology structure)))
                      (get-interface-description item-type item-name ontology stream
                                                 property-names))
                     (t (iu::send-soap-warning-message  
                         (intern 
                          (format nil "ERROR-NO-~A"
                                  item-type))
                         (intern 
                          (format nil "GET-~A-DESCRIPTION-RESPONSE"
                                  item-type))
                         stream))))))
          (t (iu::send-soap-warning-message 'error-no-ontology
                                            (intern 
                                             (format nil "GET-~A-DESCRIPTION-RESPONSE"
                                                     item-type))
                                            stream)))))

(defmethod get-interface-description ((item-type (eql 'goal)) goal-name ontology stream
                                      &optional property-names)
  (declare (ignore property-names))
  (internal-get-goal-description goal-name ontology stream))

(defmethod get-interface-description ((item-type (eql 'non-local-goal)) goal-name ontology stream
                                      &optional property-names)
  (declare (ignore property-names))
  (internal-get-goal-description goal-name ontology stream t))

(defmethod get-interface-description ((item-type (eql 'web-service)) web-service-name
                                      ontology stream
                                      &optional property-names)
  (declare (ignore property-names))
  (internal-get-web-service-description web-service-name ontology stream))

(defmethod get-interface-description ((item-type (eql 'web-service-message-exchange-pattern))
                                      web-service-name
                                      ontology stream
                                      &optional property-names)
  (declare (ignore property-names))
  (internal-get-web-service-message-exchange-pattern
   web-service-name ontology stream))

(defmethod get-interface-description ((item-type (eql 'web-service-lisp-function))
                                      web-service-name
                                      ontology stream
                                      &optional property-names)
  (declare (ignore property-names))
  (internal-get-web-service-lisp-function
   web-service-name ontology stream))

(defmethod get-interface-description ((item-type (eql 'capability)) capability-name
                                      ontology stream &optional property-names)
  (declare (ignore property-names))
  (internal-get-capability-description capability-name ontology stream))

(defmethod get-interface-description ((item-type (eql 'interface)) interface-name
                                      ontology stream &optional property-names)
  (declare (ignore property-names))
  (internal-get-interface-description interface-name ontology stream))

(defmethod get-interface-description ((item-type (eql 'mediator)) mediator-name
                                      ontology stream &optional property-names)
  (declare (ignore property-names))
  (internal-get-mediator-description mediator-name ontology stream))

(defmethod get-interface-description ((item-type 
                                       (eql 'core-non-functional-properties)) 
                                      core-non-functional-properties-name
                                      ontology stream &optional property-names)
  (internal-get-wsmo-class-properties-description
   'core-non-functional-properties
   core-non-functional-properties-name ontology stream property-names))

(defmethod get-interface-description ((item-type 
                                       (eql 'web-service-non-functional-properties)) 
                                      web-service-non-functional-properties-name
                                      ontology stream &optional property-names)
  (internal-get-wsmo-class-properties-description
   'web-service-non-functional-properties
   web-service-non-functional-properties-name ontology stream property-names))

(defmethod get-interface-description ((item-type 
                                       (eql 'choreography)) 
                                      choreography-name
                                      ontology stream &optional property-names)
  ;;(setf cc choreography-name oo ontology)
  (internal-get-wsmo-class-properties-description
   'choreography
   choreography-name ontology stream property-names))

(defmethod get-interface-description ((item-type 
                                       (eql 'orchestration)) 
                                      orchestration-name
                                      ontology stream &optional property-names)
  (internal-get-wsmo-class-properties-description
   'orchestration
   orchestration-name ontology stream property-names))



(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::goal-info))
                                     stream namespace soap-values)
  (let ((ontology (ip::make-ocml-symbol (car soap-values)))
        (web-service (ip::make-ocml-symbol (second soap-values))))
    (cond ((ocml::get-ontology ontology)
           (ocml::with-ocml-thread-safety
             (ocml::select-ontology ontology)
             (multiple-value-bind (input-roles output)
                 (internal-get-goal-info ontology web-service)
               (if (eq input-roles 'no-task)
                   (iu::send-soap-warning-message  
                    'error-no-task 'GOAL-INFO-RESPONSE
                    stream)
                 (iu::send-soap-response2 
                  "GOAL-INFO-RESPONSE"
                  (list input-roles output)
                  `((args "sexpr") (result "string"))
                  :stream stream)))))
          (t 
           (iu::send-soap-warning-message 'error-no-ontology
                                          'GOAL-INFO-RESPONSE
                                          stream)))))

#|
(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::publish-wsmo-web-service))
                                     stream namespace soap-values)
  (destructuring-bind (ontology web-service-name input-roles output 
                                lisp-function publisher-ip-address publisher-port
                                publisher-location)
      soap-values
    ;;(setf ssbb soap-values)
    (setf ontology (ip::make-ocml-symbol ontology)
          web-service-name (ip::make-ocml-symbol web-service-name)
          input-roles 
          (mapcar #'(lambda (input-role) 
                      (list (ip::make-ocml-symbol (car input-role))
                            (second input-role))) input-roles)
          output (ip::make-ocml-symbol output)
          lisp-function (ip::make-ocml-symbol lisp-function)
          publisher-location (ip::make-ocml-symbol publisher-location))
    (ip::internal-publish-wsmo-web-service ontology web-service-name
                                  input-roles output lisp-function
                                  publisher-ip-address
                                  publisher-port publisher-location)
    (let ((*package* (find-package "WSMO-PROTOCOL")))
      (iu::send-soap-response2 
       "PUBLISH-WSMO-WEB-SERVICE-RESPONSE"
       (list 'ok)
       `((result "sexpr"))
       :stream stream))))
|#


(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::publish-wsmo-web-service-all-values))
                                         stream namespace soap-values)
  (destructuring-bind (user ontology web-service-name 
                            web-service-host web-service-port 
                            web-service-location)
      soap-values
    ;;(setf ssbb soap-values)
    (setf ontology (ip::make-ocml-symbol ontology)
          web-service-name (ip::make-ocml-symbol web-service-name)
          web-service-host (ip::make-ocml-symbol web-service-host)
          web-service-port (ip::make-ocml-symbol web-service-port)
          web-service-location (ip::make-ocml-symbol web-service-location))
    (multiple-value-bind (result warning-string)
        (ip::internal-publish-wsmo-web-service user ontology web-service-name
                                               web-service-host web-service-port 
                                               web-service-location)
      (declare (ignore result))
      (if warning-string
          (iu::send-soap-warning-message warning-string 
                                         'publish-wsmo-web-service-all-values-response stream)
        (let ((*package* (find-package "WSMO-PROTOCOL")))
          (iu::send-soap-response2 
           "PUBLISH-WSMO-WEB-SERVICE-RESPONSE"
           (list "OK")
           `((result "string"))
           :stream stream))))))


(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::publish-wsmo-web-service))
                                         stream namespace soap-values)
  (destructuring-bind (user ontology web-service-name 
                            web-service-host web-service-port 
                            web-service-location)
      soap-values
    ;;;(setf ssbb soap-values)
    (setf ontology (ip::make-ocml-symbol ontology)
          web-service-name (ip::make-ocml-symbol web-service-name)
          web-service-host (ip::make-ocml-symbol web-service-host)
          web-service-port (ip::make-ocml-symbol web-service-port)
          web-service-location (ip::make-ocml-symbol web-service-location))
    (multiple-value-bind (result warning-string)
        (ip::internal-publish-wsmo-web-service user ontology web-service-name
                                               web-service-host web-service-port 
                                               web-service-location)
      (declare (ignore result))
      (if warning-string
          (iu::send-soap-warning-message warning-string 
                                         'publish-wsmo-web-service-response stream)
        (let ((*package* (find-package "WSMO-PROTOCOL")))
          (iu::send-soap-response2 
           "PUBLISH-WSMO-WEB-SERVICE-RESPONSE"
           (list "OK")
           `((result "string"))
           :stream stream))))))


(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::publish-wsmo-lisp-function))
                                         stream namespace soap-values)
  (destructuring-bind (user ontology web-service-name 
                            web-service-host web-service-port 
                            web-service-location)
      soap-values
    ;;(setf ssbb soap-values)
    (setf ontology (ip::make-ocml-symbol ontology)
          web-service-name (ip::make-ocml-symbol web-service-name)
          web-service-host (ip::make-ocml-symbol web-service-host)
          web-service-port (ip::make-ocml-symbol web-service-port)
          web-service-location (ip::make-ocml-symbol web-service-location))
    (multiple-value-bind (result warning-string)
        (ip::internal-publish-wsmo-web-service user ontology web-service-name
                                               web-service-host web-service-port 
                                               web-service-location)
      (declare (ignore result))
      (if warning-string
          (iu::send-soap-warning-message warning-string 
                                         'publish-wsmo-lisp-function-response stream)
        (let ((*package* (find-package "WSMO-PROTOCOL")))
          (iu::send-soap-response2 
           "PUBLISH-WSMO-WEB-SERVICE-RESPONSE"
           (list "OK")
           `((result "string"))
           :stream stream))))))


(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::publish-wsmo-java-method))
                                         stream namespace soap-values)
  (destructuring-bind (user ontology web-service-name
                            web-service-host web-service-port 
                            web-service-location )
      soap-values
    ;;(setf ssbb soap-values)
    (setf ontology (ip::make-ocml-symbol ontology)
          web-service-name (ip::make-ocml-symbol web-service-name)
          web-service-host (ip::make-ocml-symbol web-service-host)
          web-service-port (ip::make-ocml-symbol web-service-port)
          web-service-location (ip::make-ocml-symbol web-service-location))
    (multiple-value-bind (result warning-string)
        (ip::internal-publish-wsmo-web-service user ontology web-service-name
                                               web-service-host web-service-port 
                                               web-service-location)
      (declare (ignore result))
      (if warning-string
          (iu::send-soap-warning-message warning-string 
                                         'publish-wsmo-java-method-response stream)
        (let ((*package* (find-package "WSMO-PROTOCOL")))
          (iu::send-soap-response2 
           "PUBLISH-WSMO-WEB-SERVICE-RESPONSE"
           (list "OK")
           `((result "string"))
           :stream stream))))))


(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::publish-wsmo-http-get-request))
                                         stream namespace soap-values)
  (destructuring-bind (user ontology web-service-name
                            web-service-host web-service-port 
                            web-service-location 
                            http-get-request-url 
                            http-get-request-variables)
      soap-values
    ;;;(setf ssbb soap-values)
    (declare (ignore http-get-request-url http-get-request-variables))
    (setf ontology (ip::make-ocml-symbol ontology)
          web-service-name (ip::make-ocml-symbol web-service-name)
          web-service-host (ip::make-ocml-symbol web-service-host)
          web-service-port (ip::make-ocml-symbol web-service-port)
          web-service-location (ip::make-ocml-symbol web-service-location))
    (multiple-value-bind (result warning-string)
        (ip::internal-publish-wsmo-web-service user ontology web-service-name
                                               web-service-host web-service-port 
                                               web-service-location)
      (declare (ignore result))
      (if warning-string
          (iu::send-soap-warning-message warning-string 
                                         'PUBLISH-WSMO-WEB-SERVICE-RESPONSE stream)
        (let ((*package* (find-package "WSMO-PROTOCOL")))
          (iu::send-soap-response2 
           "PUBLISH-WSMO-WEB-SERVICE-RESPONSE"
           (list "OK")
           `((result "string"))
           :stream stream))))))

;;re order the input roles according to the order in the goal definition
(defun re-order-input-role-value-pairs-from-goal (goal-type input-role-value-pairs)
  (let ((input-roles (input-roles goal-type))
        (result nil))
    (when (ocml:nothing? input-roles)
      (error "No input roles defined for goal ~A." goal-type))
    (dolist (input-role input-roles)
      (let* ((input-role-value-pair (assoc input-role input-role-value-pairs))
             (value (second input-role-value-pair)))
        (when input-role-value-pair
          (push (list input-role value) result))))
    (reverse result)))

(defun re-run-achieve-goal (&optional (input-values *achieve-goal-soap-values*))
  (ip::handle-post-soap-service 'cl-user::achieve-goal 
                                *standard-output*
                                "" input-values))

(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::achieve-goal)) 
                                         stream namespace soap-values) 
  (setf *achieve-goal-soap-values* soap-values) 
  (let* ((ontology (ip::make-ocml-symbol (car soap-values))) 
         (goal-type (ip::make-ocml-symbol (second soap-values))) 
         (number-of-input-roles (or (third soap-values) 0))
         (input-values (cdddr soap-values)) 
         (input-role-value-pairs nil)
         result) 
    (cond ((ocml::get-ontology ontology) 
           (ocml::with-ocml-thread-safety 
             (ocml::select-ontology ontology) 
             (dotimes (i number-of-input-roles)
               (let* ((role-name (ip::make-ocml-symbol (elt input-values (* i 2))))
                      (input-role-value (ip::generate-input-role-value goal-type role-name
                                                                       (elt input-values (1+ (* i 2))))))
                 (push (list role-name input-role-value)
                       input-role-value-pairs)))
             ;;(setf input-role-value-pairs
             ;;      (re-order-input-role-value-pairs-from-goal goal-type input-role-value-pairs))
             ;;(setf xxx input-role-value-pairs) 
             ;;(setf result 
             ;;   (irs-solve-goal ontology 
             ;;       goal-type input-role-value-pairs 
             ;;    stream t))
             (setf result
                   (ip::irs-achieve-goal ontology goal-type input-role-value-pairs stream t))
             (if (and (null result) (null (find-all-web-services-which-solve-goal goal-type)))
                 (format stream "Error: No web service contenders found for goal ~a" goal-type)
               result)))
          (t (iu::send-soap-warning-message 'error-no-ontology 
                                            'ACHIEVE-GOAL-RESPONSE stream))))) 



;;check that it's ok to edit the goal
(defun ok-to-edit-create-item-in-ontology (x ontology)
  (ip::task-psm-already-defined-in-ancestor-ontology-p x ontology))

(defun generate-non-functional-properties-class-name (wsmo-entity-name)
  (intern (concatenate 'string
                        (symbol-name wsmo-entity-name)
                        *non-functional-property-class-name-extension*)
          (find-package "OCML")))

(defun handle-non-functional-properties (user-name ontology wsmo-entity-name non-functional-properties)
  (let ((non-functional-properties-class-name 
         (generate-non-functional-properties-class-name wsmo-entity-name)))
    (create-and-save-non-functional-properties-class
     user-name ontology non-functional-properties-class-name
     *default-non-functional-properties-parent-name* non-functional-properties)
    non-functional-properties-class-name))


(defun create-and-save-non-functional-properties-class
       (user-name ontology class-name parent-name
                  non-functional-properties)
  (let (definition-result
        source-code)
    (maybe-create-web-service-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf source-code
                     (generate-non-functional-properties-source-code
                      class-name parent-name *non-functional-property-names*
                      non-functional-properties)
                     definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-non-functional-properties-parent
                       parent-name)
                      (ip::add-irs-knowledge-level-definition
                       ontology class-name
                       source-code))))
             (values 
              (ip::create-irs-browser-response-string definition-result)
              t))
            (t (ip::warn-insufficient-priviliges user-name ontology))))))

(defun generate-non-functional-properties-source-code (class-name parent-name non-functional-property-names
                                                                  non-functional-property-values)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,class-name (,parent-name)
             ,(mapcan
               #'(lambda (non-functional-property-name
                          non-functional-property-value)
                   (when (and non-functional-property-value
                              (not (and (stringp non-functional-property-value)
                                        (or (zerop (length non-functional-property-value))
                                            (and (= (length non-functional-property-value) 1)
                                                 (char= #\space (elt non-functional-property-value 0)))))))
                     `((,non-functional-property-name
                        :value ,non-functional-property-value))))
               non-functional-property-names
               non-functional-property-values))))
    (format nil "~:w" code)))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::save-goal-description))
                                         stream namespace soap-values)
  (let (user-name ontology goal-name goal-parent number-of-input-roles 
                  input-roles
                  output used-mediator
                  post-condition effect non-functional-properties)
   ;;(setf ss soap-values)
    (setf user-name (ip::make-ocml-symbol (pop soap-values))
          goal-name (ip::make-ocml-symbol (pop soap-values))
          ontology (ip::make-ocml-symbol (pop soap-values))
          goal-parent (ip::make-ocml-symbol (pop soap-values))
          number-of-input-roles (pop soap-values))
    (cond ((ok-to-edit-create-item-in-ontology
            goal-name ontology)
           (dotimes (i number-of-input-roles)
             (setf input-roles
                   (append input-roles 
                           ;;input-role-name input-role-type input-role-soap-type
                           (list (list (ip::make-ocml-symbol
                                        (pop soap-values))
                                       (ip::make-ocml-symbol 
                                        (pop soap-values))
                                       (ip::make-soap-type 
                                        namespace
                                        (pop soap-values)))))))
           ;;output-role-name input-role-type input-role-soap-type
           (setf output (list (ip::make-ocml-symbol (pop soap-values))
                              (ip::make-ocml-symbol (pop soap-values))
                              (ip::make-soap-type namespace
                                                  (pop soap-values)))
                 used-mediator (ip::make-ocml-symbol (pop soap-values))
                 post-condition (ip::make-ocml-symbol (pop soap-values))
                 effect (ip::make-ocml-symbol (pop soap-values))
                 non-functional-properties ;;(ip::make-ocml-symbol (pop soap-values))
                 ;;liliana new version
                 (handle-non-functional-properties user-name ontology goal-name (pop soap-values))
                 )
           ;;(setf ppp (list ontology goal-name number-of-input-roles input-roles
           ;;              output precondition goal-expression))
           (multiple-value-bind (result ok-p)
               (internal-save-goal-description
                user-name ontology goal-name goal-parent input-roles
                output used-mediator post-condition effect non-functional-properties)
             (iu::send-soap-response2 
              "SAVE-GOAL-DESCRIPTION-RESPONSE"
              (list result)
              (if ok-p
                  `((result "string"))
                `((warning "string")))
              :stream stream)))
          (t (ip::send-not-ok-to-edit goal-name 'goal ontology 'saved
                                      "SAVE-GOAL-DESCRIPTION-RESPONSE"
                                      stream)))))

(defun maybe-create-goal-ontology (user-name ontology
                                             &optional 
                                             ontology-uses
                                             (allowed-editors ""))
  (ip::maybe-create-ontology 'ocml::goal user-name ontology 
                             ontology-uses allowed-editors))

(defun maybe-create-web-service-ontology (user-name ontology
                                             &optional 
                                             ontology-uses
                                             (allowed-editors ""))
  (ip::maybe-create-ontology 'ocml::web-service user-name ontology 
                             ontology-uses allowed-editors))

(defun maybe-create-mediator-ontology (user-name ontology
                                             &optional 
                                             ontology-uses
                                             (allowed-editors ""))
  (ip::maybe-create-ontology 'ocml::mediator user-name ontology 
                             ontology-uses allowed-editors))

(defun internal-save-goal-description
       (user-name ontology goal-name goal-parent input-roles
                  output used-mediator post-condition effect non-functional-properties)
  (let (goal-definition-result
        goal-source-code)
    (maybe-create-goal-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety () ;;take out thread safety when saving because then new code is 
             ;;;not available?
               (ocml::select-ontology ontology)
               (setf goal-source-code
                     (generate-goal-source-code goal-name goal-parent
                                                input-roles 
                                                output used-mediator 
                                                post-condition effect non-functional-properties)
                     goal-definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-goal-parent
                       goal-parent)
                      (ip::add-irs-knowledge-level-definition ontology goal-name
                                           goal-source-code))))
             (values 
              (ip::create-irs-browser-response-string goal-definition-result)
              t))
            (t (ip::warn-insufficient-priviliges user-name ontology))))))

(defun maybe-create-warning-about-non-functional-properties-parent (non-functional-properties-parent)
  (ip::maybe-create-warning-about-task-psm-parent 
   'non-functional-properties non-functional-properties-parent
   *default-non-functional-properties-parent*))

(defun maybe-create-warning-about-goal-parent (goal-parent)
  (ip::maybe-create-warning-about-task-psm-parent 
   'goal goal-parent
   *default-goal-parent*))

(defun maybe-create-warning-about-choreography-parent (choreography-parent)
  (ip::maybe-create-warning-about-task-psm-parent 
   'choreography choreography-parent
   *default-choreography-parent*))

(defun maybe-create-warning-about-orchestration-parent (orchestration-parent)
  (ip::maybe-create-warning-about-task-psm-parent 
   'orchestration orchestration-parent
   *default-orchestration-parent*))

(defun maybe-create-warning-about-problem-solving-pattern-parent (problem-solving-pattern-parent)
  (ip::maybe-create-warning-about-task-psm-parent 
   'problem-solving-pattern problem-solving-pattern-parent
   *default-problem-solving-pattern-parent*))

(defun maybe-create-warning-about-web-service-parent (web-service-parent)
  (ip::maybe-create-warning-about-task-psm-parent 
   'web-service web-service-parent
   *default-web-service-parent*))

(defun maybe-create-warning-about-mediator-parent (mediator-parent)
  (ip::maybe-create-warning-about-task-psm-parent 
   'mediator mediator-parent
   *default-mediator-parent*))

(defun maybe-create-warning-about-capability-parent (capability-parent)
  (ip::maybe-create-warning-about-task-psm-parent 
   'capability capability-parent
   *default-capability-parent*))

(defun maybe-create-warning-about-interface-parent (interface-parent)
  (ip::maybe-create-warning-about-task-psm-parent 
   'interface interface-parent
   *default-interface-parent*))

(defun maybe-create-warning-about-wsmo-class-properties-parent 
       (core-non-functional-properties-parent default-parent properties-type)
  (ip::maybe-create-warning-about-task-psm-parent 
   properties-type core-non-functional-properties-parent
   default-parent))

(defun generate-goal-source-code (goal-name goal-parent input-roles 
                                            output used-mediator post-condition effect 
                                            non-functional-properties)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,goal-name (,goal-parent) ocml::?goal
             (
              ,@(when input-roles
                  `((ocml::has-input-role 
                     ,@(mapcan #'(lambda (x) 
                                   (list :value (car x)))
                               input-roles))
                    (ocml::has-input-soap-binding 
                     ,@(mapcan #'(lambda (x) 
                                   (list :value (list (car x) (third x))))
                               input-roles))))
              ,@(when (car output)
                  `((ocml::has-output-role :value ,(car output))
                    (ocml::has-output-soap-binding :value (,(car output) ,(third output)))))
              ,@(mapcar #'(lambda (input-role)
                            (if (second input-role)
                                (list (car input-role)
                                      :type (second input-role))
                              (list (car input-role))))
                        input-roles)
              ,@(when (car output)
                  `((,(car output) ,@(when (second output)
                                      `(:type ,(second output))))))
              ,@(when effect
                  `((ocml::has-effect
                     :value ,(read-from-string 
                              (format nil "~a"
                                      effect)))))
              ,@(when used-mediator 
                  `((ocml::used-mediator
                     :value ,(generate-slot-value used-mediator))))
              ,@(when post-condition
                  `((ocml::has-post-condition 
                     :value 
                     ,(generate-slot-value post-condition))))
              ,@(when non-functional-properties
                  `((ocml::has-non-functional-properties 
                     :value 
                     ,(generate-slot-value non-functional-properties))))))))
    (format nil "~:w" code)))


(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::get-goal-description))
                                         stream namespace soap-values)

  (handle-get-wsmo-description stream soap-values 'goal))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::get-non-local-goal-description))
                                         stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'non-local-goal))

;; Inserts the ontology name in the list of input-roles for Goals
;; Added by Carlos 12-9-06
;; We should instead update the method get-goal-description-values
;; Added in here to avoid unpredicted side effects
(defun insert-ontology-name (ontology input-roles)
  (ocml::select-ontology ontology)
  (when input-roles 
    (let ((type (second input-roles)))
      (append (list 
               (first input-roles) 
               type
               (ocml::name (iu::home-ontology (ocml::get-ocml-class type)))
               (third input-roles)) 
              (insert-ontology-name ontology (cdddr input-roles))))))

;;(insert-ontology-name '(a b c d e f g h i))
;;(insert-ontology-name 'ocml::european-travel-service-descriptions '(ORIGIN CITY "sexpr" DESTINATION CITY "sexpr" ))

(defun internal-get-goal-description (goal-name ontology stream &optional non-local-p)
  ;;(setf ty1 goal-name o1 ontology)
  (multiple-value-bind (goal-parent number-of-input-roles input-roles
                                    output used-mediator post-condition effect non-functional-properties)
      (get-goal-description-values goal-name ontology non-local-p)
    (setf input-roles (insert-ontology-name ontology input-roles))
    ;;(setf toto input-roles)
    (let ((*package* (find-package "OCML"))
          (results (append (list goal-parent number-of-input-roles)
                           input-roles
                           (let ((type (second output)))
                           (list (car output) 
                                 type
                                 (ocml::name (iu::home-ontology (ocml::get-ocml-class type)))
                                 (third output) ;;just in case output is nil
                                 used-mediator post-condition effect non-functional-properties)))))
      (iu::send-soap-response2 
       "GET-GOAL-DESCRIPTION-RESPONSE"
       (ip::replace-nils-with-empty-string results)
       (mapcar #'(lambda (result result-tag)
                   (declare (ignore result))
                   ;;now always use string
                    (list result-tag "string")
                   #|(if result
                       (list result-tag "sexpr") 
                     ;;"sexpr" uses the format directive ~s
                     ;;and we want to use ~a
                     (list result-tag "string"))
|#
                   )
              
               results
               (append '(goal-parent number-of-input-role-value-pairs) 
                       (apply #'append 
                              (make-list number-of-input-roles
                                         :initial-element
                                         '(input-name input-type ontology-name input-soap-type)))
                       '(output-name output-type ontology-name output-soap-type 
                                     used-mediator post-condition effect non-functional-properties)))
       :stream stream
       :downcase-p t
       :result-tags-to-leave-case '(non-functional-properties)))))

(defun get-goal-description-values (goal-name ontology non-local-p)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let* ((goal-parent (get-class-parent goal-name))
           (input-roles (get-input-roles-with-soap-types goal-name non-local-p))
           (number-of-input-roles (/ (length input-roles) 3))
           (output (get-output goal-name non-local-p))
           (post-condition (get-post-condition goal-name))
           (used-mediator (get-mediator goal-name))
           (effect (get-effect goal-name))
           (non-functional-properties ;;(get-non-functional-properties goal-name)
                                      (get-all-non-functional-property-values-as-string goal-name)))
      ;;;get rid of the brackets around the goal parents list
      (values (format nil "~{~a ~}" goal-parent) number-of-input-roles input-roles
              output used-mediator post-condition effect non-functional-properties))))

(defun delete-wsmo-description (user-name ontology wsmo-definition)
  (let ((ontology-structure (ocml::get-ontology ontology)))
    (cond (ontology-structure
           (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
                  (values 
                   (ocml::with-ocml-thread-safety ()
                     (with-output-to-string (stream)
                       (let ((web-onto::*ocml-line-separator*
                              #\space))
                         (declare (special 
                                   web-onto::*ocml-line-separator*))
                         (web-onto::internal-delete-items 
                          ontology 
                          `((ocml::class ,wsmo-definition))
                          stream))))
                   t))
                 (t (ip::warn-insufficient-priviliges user-name ontology))))
          (t (ip::warn-no-ontology ontology)))))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 
                                           'cl-user::delete-goal-description))
                                         stream namespace soap-values)
  (handle-delete-wsmo-description stream soap-values 'goal))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::save-web-service-message-exchange-pattern))
                                         stream namespace soap-values)
  (let (user-name ontology web-service-name message-exchange-pattern associated-choreography)
    (setf user-name (ip::make-ocml-symbol (pop soap-values))
          web-service-name (ip::make-ocml-symbol (pop soap-values))
          ontology (ip::make-ocml-symbol (pop soap-values))
          message-exchange-pattern (ip::make-ocml-symbol (pop soap-values)))
    (ocml::select-ontology ontology)
    (setf associated-choreography 
          (web-onto::findany '?x 
                             `(ocml::associated-choreography ,web-service-name ?x)))
    (if associated-choreography
        (multiple-value-bind (parent web-service-host web-service-port web-service-location 
                                               old-message-exchange-pattern lisp-function 
                                               java-class java-method 
                                               http-get-request-url 
                                               http-get-request-variables wsdl-file-name 
                                               wsdl-operation-name wsdl-port-type 
                                               wsdl-port-type-name-space 
                                               wsdl-web-service-provider-type)
            (get-wsmo-class-properties-description-values 
             associated-choreography ontology *choreography-properties*)
          (declare (ignore parent old-message-exchange-pattern))
          (ip::internal-publish-wsmo-web-service
           user-name ontology web-service-name web-service-host web-service-port
           web-service-location))
	(ip::internal-publish-wsmo-web-service
	 user-name ontology web-service-name nil nil nil))))


(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::save-web-service-description))
                                         stream namespace soap-values)
  ;;(setf ssss soap-values)
  (let (user-name ontology web-service-name web-service-parent number-of-input-roles 
                  input-roles
                  output capability interface oo-mediator non-functional-properties)
    (setf user-name (ip::make-ocml-symbol (pop soap-values))
          web-service-name (ip::make-ocml-symbol (pop soap-values))
          ontology (ip::make-ocml-symbol (pop soap-values))
          web-service-parent (ip::make-ocml-symbol (pop soap-values))
          number-of-input-roles (pop soap-values))
    (cond ((ok-to-edit-create-item-in-ontology
            web-service-name ontology)
           (dotimes (i number-of-input-roles)
             (setf input-roles
                   (append input-roles 
                           ;;input-role-name input-role-type input-role-soap-type
                           (list (list (ip::make-ocml-symbol
                                        (pop soap-values))
                                       (ip::make-ocml-symbol 
                                        (pop soap-values)))))))
           ;;output-role-name input-role-type input-role-soap-type
           (setf output (list (ip::make-ocml-symbol (pop soap-values))
                              (ip::make-ocml-symbol (pop soap-values)))
                 capability (ip::make-ocml-symbol (pop soap-values))
                 interface (ip::make-ocml-symbol (pop soap-values))
                 oo-mediator (ip::make-ocml-symbol (pop soap-values))
                 non-functional-properties ;;(ip::make-ocml-symbol (pop soap-values)))
                 (handle-non-functional-properties user-name ontology web-service-name (pop soap-values)))
           ;;(setf ppp (list ontology web-service-name number-of-input-roles input-roles
           ;;              output precondition web-service-expression))
           (multiple-value-bind (result ok-p)
                 (internal-save-web-service-description
                  user-name ontology web-service-name web-service-parent input-roles
                  output capability interface oo-mediator non-functional-properties)
           (iu::send-soap-response2 
            "SAVE-WEB-SERVICE-DESCRIPTION-RESPONSE"
            (list result)
            (if ok-p
            `((result "string"))
            `((warning "string")))
            :stream stream)))
          (t (ip::send-not-ok-to-edit web-service-name 'web-service ontology 'saved
                                  "SAVE-WEB-SERVICE-DESCRIPTION-RESPONSE"
                                  stream)))))

(defun internal-save-web-service-description
       (user-name ontology web-service-name web-service-parent input-roles
                  output capability interface used-mediator non-functional-properties)
  (let (web-service-definition-result
        web-service-source-code)
    (maybe-create-web-service-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf web-service-source-code
                     (generate-web-service-source-code web-service-name web-service-parent
                                                       input-roles 
                                                       output capability interface used-mediator 
                                                       non-functional-properties)
                     web-service-definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-web-service-parent
                       web-service-parent)
                      (ip::add-irs-knowledge-level-definition
                       ontology web-service-name
                       web-service-source-code))))
             (values 
              (ip::create-irs-browser-response-string web-service-definition-result)
              t))
            (t (ip::warn-insufficient-priviliges user-name ontology))))))

(defun generate-web-service-source-code (web-service-name web-service-parent input-roles 
                                                          output capability interface 
                                                          used-mediator 
                                                          non-functional-properties)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,web-service-name (,web-service-parent) ocml::?web-service
             (
              ,@(when input-roles
                  `((ocml::has-input-role 
                     ,@(mapcan #'(lambda (x) 
                                   (list :value (car x)))
                               input-roles))))
              ,@(when (car output)
                  `((ocml::has-output-role :value ,(car output))))
              ,@(mapcar #'(lambda (input-role)
                            (if (second input-role)
                                (list (car input-role)
                                      :type (second input-role))
                              (list (car input-role))))
                        input-roles)
              ,@(when (car output)
                  `((,(car output) ,@(when (second output)
                                       `(:type ,(second output))))))
              ,@(when capability
                  `((ocml::has-capability
                     :value ,(generate-slot-value capability))))
              ,@(when interface
                  `((ocml::has-interface
                     :value ,(generate-slot-value interface))))
              ,@(when used-mediator
                  `((ocml::used-mediator
                     :value 
                     ,(generate-slot-value used-mediator))))
#|
              ,@(when internal-method
                  `((ocml::has-internal-method
                     :value 
                     ,(generate-slot-value internal-method))))
|#
              ,@(when non-functional-properties
                  `((ocml::has-non-functional-properties 
                     :value 
                     ,(generate-slot-value non-functional-properties))))))))
    (format nil "~:w" code)))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 
                                           'cl-user::delete-web-service-description))
                                         stream namespace soap-values)
  (handle-delete-wsmo-description stream soap-values 'web-service))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 
                                           'cl-user::delete-web-service-description-and-components))
                                         stream namespace soap-values)
  (handle-delete-wsmo-description-and-components
   stream soap-values 'web-service 
   '(ocml::has-interface ocml::interface 
                         (ocml::has-choreography ocml::choreography)
                         (ocml::has-orchestration ocml::orchestration)
                         (ocml::used-mediator ocml::mediator))
   '(ocml::has-capability 'ocml::capability
                          (ocml::used-mediator ocml::mediator))))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::get-web-service-description))
                                         stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'web-service))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::get-web-service-message-exchange-pattern))
                                         stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'web-service-message-exchange-pattern))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::get-web-service-lisp-function))
                                         stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'web-service-lisp-function))


;; Inserts the ontology name in the list of input-roles for WebServices
;; Added by Carlos 16-10-06
;; We should instead update the method get-web-service-description-values.
;; Added in here to avoid unpredicted side effects
(defun insert-ontology-name-ws (ontology input-roles)
  (ocml::select-ontology ontology)
  (when input-roles 
    (let ((type (second input-roles)))
      (append (list 
               (first input-roles) 
               type
               (ocml::name (iu::home-ontology (ocml::get-ocml-class type)))) 
              (insert-ontology-name-ws ontology (cddr input-roles))))))

(defun internal-get-web-service-description (web-service-name ontology stream)
  ;;(setf ty1 web-service-name o1 ontology)
  (multiple-value-bind (web-service-parent number-of-input-roles input-roles
                                           output capability interface 
                                           used-mediator 
                                           non-functional-properties)
      (get-web-service-description-values web-service-name ontology)

    (setf input-roles (insert-ontology-name-ws ontology input-roles))

    (let ((*package* (find-package "OCML"))
          (results (append (list web-service-parent number-of-input-roles)
                           input-roles
                           (list (car output) (second output) ;;just in case output is nil
                                 (if (second output)
                                     (ocml::name (iu::home-ontology (ocml::get-ocml-class (second output))))) ;;Include the ontology
                                 capability interface used-mediator  
                                 non-functional-properties))))
      (iu::send-soap-response2 
       "GET-WEB-SERVICE-DESCRIPTION-RESPONSE"
       (ip::replace-nils-with-empty-string results)
       (mapcar #'(lambda (result result-tag)
                   (declare (ignore result))
                   ;;now always use string
                   (list result-tag "string")
                   #|(if result
                       (list result-tag "sexpr") 
                     ;;"sexpr" uses the format directive ~s
                     ;;and we want to use ~a
                     (list result-tag "string"))
|#
                   )
               results
               (append '(web-service-parent number-of-input-role-value-pairs) 
                       (apply #'append 
                              (make-list number-of-input-roles
                                         :initial-element
                                         '(input-name input-type input-type-ontology)))
                       '(output-name output-type output-type-ontology
                                     capability interface used-mediator  
                                     non-functional-properties)))
       :stream stream
       :downcase-p t
       :result-tags-to-leave-case '(non-functional-properties)))))

(defun internal-get-web-service-message-exchange-pattern (web-service-name ontology stream)
  (let* ((message-exchange-pattern
          (ip::internal-get-web-service-message-exchange-pattern 
           ontology web-service-name))
         (*package* (find-package "OCML"))
         (results (list message-exchange-pattern)))
    (iu::send-soap-response2 
     "GET-WEB-SERVICE-MESSAGE-EXCHANGE-PATTERN-RESPONSE"
     (ip::replace-nils-with-empty-string results)
     (mapcar #'(lambda (result result-tag)
                 (declare (ignore result))
                 ;;now always use string
                 (list result-tag "string")
                 #|(if result
                       (list result-tag "sexpr") 
                     ;;"sexpr" uses the format directive ~s
                     ;;and we want to use ~a
                     (list result-tag "string"))
|#
                 )
             results
             '(message-exchange-pattern))
     :stream stream
     :downcase-p t)))

(defun internal-get-web-service-lisp-function (web-service-name ontology stream)
  (let* ((lisp-function
          (ip::internal-get-web-service-lisp-function
           ontology web-service-name))
         (*package* (find-package "OCML"))
         (results (list lisp-function)))
    (iu::send-soap-response2 
     "GET-WEB-SERVICE-LISP-FUNCTION-RESPONSE"
     (ip::replace-nils-with-empty-string results)
     (mapcar #'(lambda (result result-tag)
                 (declare (ignore result))
                 ;;now always use string
                 (list result-tag "string")
                 #|(if result
                       (list result-tag "sexpr") 
                     ;;"sexpr" uses the format directive ~s
                     ;;and we want to use ~a
                     (list result-tag "string"))
|#
                 )
             results
             '(lisp-function))
     :stream stream
     :downcase-p t)))

(defun get-web-service-description-values (web-service-name ontology)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let* ((web-service-parent (get-class-parent web-service-name))
           (input-roles (get-input-roles-with-types web-service-name nil))
           (number-of-input-roles (/ (length input-roles) 2))
           (output (get-output web-service-name nil))
           (capability (get-capability web-service-name))
           (interface (get-interface web-service-name))
           (used-mediator (get-mediator web-service-name))
           ;;(internal-method (get-internal-method web-service-name))
           (non-functional-properties ;;(get-non-functional-properties web-service-name)
                                      (get-all-non-functional-property-values-as-string web-service-name)))
      ;;;get rid of the brackets around the web-service parents list
      (values (format nil "~{~a ~}" web-service-parent) number-of-input-roles input-roles
              output capability interface used-mediator non-functional-properties))))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::save-capability-description))
                                         stream namespace soap-values)
  (let (user-name ontology capability-name capability-parent 
                  used-mediator pre-condition post-condition assumption effect 
                  ;;goal-solved
                  ;;applicability-condition
                  non-functional-properties)
    (setf user-name (ip::make-ocml-symbol (pop soap-values))
          capability-name (ip::make-ocml-symbol (pop soap-values))
          ontology (ip::make-ocml-symbol (pop soap-values))
          capability-parent (ip::make-ocml-symbol (pop soap-values))
          used-mediator (ip::make-ocml-symbol (pop soap-values))
          pre-condition (make-ocml-object (pop soap-values))
          post-condition (make-ocml-object (pop soap-values))
          assumption (make-ocml-object (pop soap-values))
          effect (make-ocml-object (pop soap-values))
          ;;goal-solved (make-ocml-object (pop soap-values))
          ;;applicability-condition (make-ocml-object (pop soap-values))
          non-functional-properties ;;(make-ocml-object (pop soap-values)))          
          (handle-non-functional-properties user-name ontology capability-name (pop soap-values)))
    (cond ((ok-to-edit-create-item-in-ontology
            capability-name ontology)
           (multiple-value-bind (result ok-p)
                 (internal-save-capability-description
                  user-name ontology capability-name capability-parent 
                  used-mediator pre-condition post-condition assumption effect 
                  ;;goal-solved
                  ;;applicability-condition
                  non-functional-properties)
           (iu::send-soap-response2 
            "SAVE-CAPABILITY-DESCRIPTION-RESPONSE"
            (list result)
            (if ok-p
            `((result "string"))
            `((warning "string")))
            :stream stream)))
          (t (ip::send-not-ok-to-edit capability-name 'capability ontology 'saved
                                  "SAVE-CAPABILITY-DESCRIPTION-RESPONSE"
                                  stream)))))

(defun internal-save-capability-description
       (user-name ontology capability-name capability-parent 
                  used-mediator pre-condition post-condition assumption effect 
                  ;;goal-solved
                  ;;applicability-condition
                  non-functional-properties)
  (let (capability-definition-result
        capability-source-code)
    (maybe-create-web-service-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf capability-source-code
                     (generate-capability-source-code capability-name capability-parent
                                                      used-mediator 
                                                      pre-condition post-condition assumption
                                                      effect 
                                                      ;;goal-solved
                                                      ;;applicability-condition
                                                      non-functional-properties)
                     capability-definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-capability-parent
                       capability-parent)
                      (ip::add-irs-knowledge-level-definition
                       ontology capability-name
                       capability-source-code))))
             (values 
              (ip::create-irs-browser-response-string capability-definition-result)
              t))
            (t (ip::warn-insufficient-priviliges user-name ontology))))))

(defun generate-capability-source-code (capability-name capability-parent 
                                                        used-mediator 
                                                        pre-condition post-condition assumption
                                                        effect 
                                                        ;;goal-solved
                                                        ;;applicability-condition
                                                        non-functional-properties)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,capability-name (,capability-parent) ocml::?capability
             (,@(when used-mediator
                  `((ocml::used-mediator
                     :value 
                     ,(generate-slot-value used-mediator))))
              ,@(when pre-condition
                  `((ocml::has-pre-condition
                     :value ,(generate-slot-value pre-condition))))
              ,@(when post-condition
                  `((ocml::has-post-condition
                     :value ,(generate-slot-value post-condition))))
              ,@(when assumption
                  `((ocml::has-assumption
                     :value 
                     ,(generate-slot-value assumption))))
              ,@(when effect
                  `((ocml::has-effect
                     :value 
                     ,(generate-slot-value effect))))
#|
              ,@(when goal-solved
                  `((ocml::solves-goal
                     :value 
                     ,(read-from-string 
                       (format nil "~a"
                               goal-solved)))))
              ,@(when applicability-condition
                  `((ocml::has-applicability-condition
                     :value 
                     ,(generate-slot-value applicability-condition))))
|#
              ,@(when non-functional-properties
                  `((ocml::has-non-functional-properties 
                     :value 
                     ,(generate-slot-value non-functional-properties))))))))
    (format nil "~:w" code)))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 
                                           'cl-user::delete-capability-description))
                                         stream namespace soap-values)
  (handle-delete-wsmo-description stream soap-values 'capability))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::get-capability-description))
                                         stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'capability))

(defun internal-get-capability-description (capability-name ontology stream)
  ;;(setf ty1 capability-name o1 ontology)
  (multiple-value-bind (capability-parent 
                        used-mediator 
                        pre-condition post-condition assumption
                        effect 
                        ;;goal-solved
                        ;;applicability-condition
                        non-functional-properties)
      (get-capability-description-values capability-name ontology)
    (let ((*package* (find-package "OCML"))
          (results (list capability-parent 
                        used-mediator 
                        pre-condition post-condition assumption
                        effect 
                        ;;goal-solved
                        ;;applicability-condition
                        non-functional-properties)))
      (iu::send-soap-response2 
       "GET-CAPABILITY-DESCRIPTION-RESPONSE"
       (ip::replace-nils-with-empty-string results)
       (mapcar #'(lambda (result result-tag)
                   (declare (ignore result))
                   ;;now always use string
                   (list result-tag "string")
                   #|(if result
                       (list result-tag "sexpr") 
                     ;;"sexpr" uses the format directive ~s
                     ;;and we want to use ~a
                     (list result-tag "string"))
|#
                   )
               results
               '(capability-parent 
                        used-mediator 
                        pre-condition post-condition assumption
                        effect 
                        ;;goal-solved
                        ;;applicability-condition
                        non-functional-properties))
       :stream stream
       :downcase-p t
       :result-tags-to-leave-case '(non-functional-properties)))))


(defun get-capability-description-values (capability-name ontology)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let* ((capability-parent (get-class-parent capability-name))
           (used-mediator (get-mediator capability-name))
           (pre-condition (get-pre-condition capability-name))
           (post-condition (get-post-condition capability-name))
           (assumption (get-assumption capability-name))
           (effect (get-effect capability-name))
           ;;(goal-solved (get-goal-solved capability-name))
           ;;(applicability-condition (get-applicability-condition capability-name))
           (non-functional-properties ;;(get-non-functional-properties capability-name)
                                      (get-all-non-functional-property-values-as-string capability-name)))
      ;;;get rid of the brackets around the capability parents list
      (values (format nil "~{~a ~}" capability-parent) 
                        used-mediator 
                        pre-condition post-condition assumption
                        effect 
                        ;;goal-solved
                        ;;applicability-condition
                        non-functional-properties))))



(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::save-interface-description))
                                         stream namespace soap-values)
  (let (user-name ontology interface-name interface-parent 
                  used-mediator choreography-grounding choreography-guarded-transitions
                  orchestration-body non-functional-properties choreography-name orchestration-name)
    (setf user-name (ip::make-ocml-symbol (pop soap-values))
          interface-name (ip::make-ocml-symbol (pop soap-values))
          ontology (ip::make-ocml-symbol (pop soap-values))
          interface-parent (ip::make-ocml-symbol (pop soap-values))
          used-mediator (ip::make-ocml-symbol (pop soap-values))
          choreography-grounding (ip::make-ocml-list (pop soap-values))
          choreography-guarded-transitions (ip::make-ocml-list (pop soap-values))
          orchestration-body (ip::make-ocml-list (pop soap-values))
          non-functional-properties ;;(make-ocml-object (pop soap-values)))          
          (handle-non-functional-properties user-name ontology interface-name (pop soap-values)))
    (setf choreography-name
          (generate-choreography-name-for-web-service interface-name))
    (internal-save-choreography-description
     user-name ontology choreography-name *default-choreography-parent*
     choreography-grounding choreography-guarded-transitions)
    (setf orchestration-name
          (generate-orchestration-name-for-web-service interface-name))
    (internal-save-orchestration-description
     user-name ontology orchestration-name *default-orchestration-parent* orchestration-body)
    (cond ((ok-to-edit-create-item-in-ontology
            interface-name ontology)
           (multiple-value-bind (result ok-p)
                 (internal-save-interface-description
                  user-name ontology interface-name interface-parent 
                  used-mediator choreography-name orchestration-name non-functional-properties)
           (iu::send-soap-response2 
            "SAVE-INTERFACE-DESCRIPTION-RESPONSE"
            (list result)
            (if ok-p
            `((result "string"))
            `((warning "string")))
            :stream stream)))
          (t (ip::send-not-ok-to-edit interface-name 'interface ontology 'saved
                                  "SAVE-INTERFACE-DESCRIPTION-RESPONSE"
                                  stream)))))

(defun internal-save-interface-description
       (user-name ontology interface-name interface-parent 
                  used-mediator choreography orchestration non-functional-properties)
  (let (interface-definition-result
        interface-source-code)
    (maybe-create-web-service-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf interface-source-code
                     (generate-interface-source-code interface-name interface-parent
                                                      used-mediator 
                                                      choreography orchestration non-functional-properties)
                     interface-definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-interface-parent
                       interface-parent)
                      (ip::add-irs-knowledge-level-definition
                       ontology interface-name
                       interface-source-code))))
             (values 
              (ip::create-irs-browser-response-string interface-definition-result)
              t))
            (t (ip::warn-insufficient-priviliges user-name ontology))))))

(defun generate-interface-source-code (interface-name interface-parent 
                                                        used-mediator 
                                                        choreography orchestration non-functional-properties)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,interface-name (,interface-parent) ocml::?interface
             (,@(when used-mediator
                  `((ocml::used-mediator
                     :value 
                     ,(generate-slot-value used-mediator))))
              ,@(when choreography
                  `((ocml::has-choreography
                     :value ,(generate-slot-value choreography))))
              ,@(when orchestration
                  `((ocml::has-orchestration
                     :value ,(generate-slot-value orchestration))))
               ,@(when non-functional-properties
                  `((ocml::has-non-functional-properties 
                     :value 
                     ,(generate-slot-value non-functional-properties))))))))
    (format nil "~:w" code)))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 
                                           'cl-user::delete-interface-description))
                                         stream namespace soap-values)
  (handle-delete-wsmo-description stream soap-values 'interface))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::get-interface-description))
                                         stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'interface))



(defun internal-get-interface-description (interface-name ontology stream)
  ;;(setf ty1 interface-name o1 ontology)
  (multiple-value-bind (interface-parent 
                        used-mediator 
                        choreography choreography-grounding choreography-guarded-transitions
                        orchestration orchestration-body non-functional-properties)
      (get-interface-description-values interface-name ontology)
    (let ((*package* (find-package "OCML"))
          (results (list interface-parent 
                         used-mediator choreography choreography-grounding choreography-guarded-transitions
                         orchestration orchestration-body non-functional-properties)))
      (iu::send-soap-response2 
       "GET-INTERFACE-DESCRIPTION-RESPONSE"
       (ip::replace-nils-with-empty-string results)
       (mapcar #'(lambda (result result-tag-info)
                   (let ((result-tag (if (listp result-tag-info) (car result-tag-info) result-tag-info))
                         ;;use string if the result was nil
                         (soap-type (if (and (listp result-tag-info) result)
                                        (second result-tag-info) "string")))
                   (list result-tag soap-type)))
               results
               '(interface-parent
                 used-mediator
                 choreography
                 (choreography-grounding "sexpr")
                 (choreography-guarded-transitions "sexpr")
                 orchestration orchestration-body non-functional-properties))
       :stream stream
       :downcase-p t
       :result-tags-to-leave-case '(non-functional-properties choreography-grounding)))))

(defun get-interface-description-values (interface-name ontology)
  ;;(setf ii interface-name o ontology)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let* ((interface-parent (get-class-parent interface-name))
           (used-mediator (get-mediator interface-name))
           (choreography (get-choreography interface-name))
           (choreography-grounding 
            (if (and choreography (ocml::get-domain-class choreography))
                (get-choreography-grounding choreography)
              ""))
           (choreography-guarded-transitions 
            (if (and choreography (ocml::get-domain-class choreography))
                (get-choreography-guarded-transitions choreography)
              ""))
           (orchestration (get-orchestration interface-name))
           (orchestration-body (when orchestration
                                 (generate-readable-orchestration-body 
                                  (get-orchestration-body orchestration))))
           (non-functional-properties ;;(get-non-functional-properties interface-name)
                                      (get-all-non-functional-property-values-as-string interface-name)))
      ;;;get rid of the brackets around the interface parents list
      (values (format nil "~{~a ~}" interface-parent) 
              used-mediator choreography choreography-grounding choreography-guarded-transitions
              orchestration orchestration-body non-functional-properties))))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::save-mediator-description))
                                         stream namespace soap-values)
  ;;(setf ooo soap-values)
  (let (user-name ontology mediator-name mediator-parent 
                  used-mediator source target mediation-service mapping-rules non-functional-properties)
    (setf user-name (ip::make-ocml-symbol (pop soap-values))
          mediator-name (ip::make-ocml-symbol (pop soap-values))
          ontology (ip::make-ocml-symbol (pop soap-values))
          mediator-parent (ip::make-ocml-symbol (pop soap-values))
          used-mediator (ip::make-ocml-symbol (pop soap-values))
          source (ip::make-ocml-symbol (pop soap-values))
          target (ip::make-ocml-symbol (pop soap-values))
          mediation-service (ip::make-ocml-symbol (pop soap-values))
          mapping-rules (ip::make-ocml-list (pop soap-values)) 
          non-functional-properties ;;(make-ocml-object (pop soap-values)))         
          (handle-non-functional-properties user-name ontology mediator-name (pop soap-values)))
    (cond ((ok-to-edit-create-item-in-ontology
            mediator-name ontology)
           (multiple-value-bind (result ok-p)
                 (internal-save-mediator-description
                  user-name ontology mediator-name mediator-parent 
                  used-mediator source target mediation-service mapping-rules non-functional-properties)
           (iu::send-soap-response2 
            "SAVE-MEDIATOR-DESCRIPTION-RESPONSE"
            (list result)
            (if ok-p
            `((result "string"))
            `((warning "string")))
            :stream stream)))
          (t (ip::send-not-ok-to-edit mediator-name 'mediator ontology 'saved
                                  "SAVE-MEDIATOR-DESCRIPTION-RESPONSE"
                                  stream)))))

(defun internal-save-mediator-description
       (user-name ontology mediator-name mediator-parent 
                  used-mediator source target mediation-service mapping-rules non-functional-properties)
  (let (mediator-definition-result
        mediator-source-code)
    (maybe-create-mediator-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf mediator-source-code
                     (generate-mediator-source-code mediator-name mediator-parent
                                                      used-mediator 
                                                      source target mediation-service mapping-rules 
                                                      non-functional-properties)
                     mediator-definition-result
                     (concatenate 
                      'string
                      (ip::add-irs-knowledge-level-definition
                       ontology mediator-name
                       mediator-source-code))))
             (values 
              (ip::create-irs-browser-response-string mediator-definition-result)
              t))
            (t (ip::warn-insufficient-priviliges user-name ontology))))))

(defun generate-mediator-source-code (mediator-name mediator-parent 
                                                        used-mediator 
                                                        source target mediation-service mapping-rules
                                                        non-functional-properties)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,mediator-name (,mediator-parent) ocml::?mediator
             (,@(when used-mediator
                  `((ocml::used-mediator
                     :value 
                     ,(generate-slot-value used-mediator))))
              ,@(when source
                  `((ocml::has-source-component
                     :value ,(generate-slot-value source))))
              ,@(when target
                  `((ocml::has-target-component
                     :value ,(generate-slot-value target))))
              ,@(when mediation-service
                  `((ocml::has-mediation-service
                     :value ,(generate-slot-value mediation-service))))
              ,@(when mapping-rules
                  `((ocml::has-mapping-rules
                     :value ,(generate-slot-value mapping-rules))))
              ,@(when non-functional-properties
                  `((ocml::has-non-functional-properties 
                     :value 
                     ,(generate-slot-value non-functional-properties))))))))
    (format nil "~:w" code)))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 
                                           'cl-user::delete-mediator-description))
                                         stream namespace soap-values)
  (handle-delete-wsmo-description stream soap-values 'mediator))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::get-mediator-description))
                                         stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'mediator))

(defun internal-get-mediator-description (mediator-name ontology stream)
  ;;(setf ty1 mediator-name o1 ontology)
  (multiple-value-bind (mediator-parent 
                        used-mediator 
                        source target mediation-service mapping-rules non-functional-properties)
      (get-mediator-description-values mediator-name ontology)
    (let ((*package* (find-package "OCML"))
          (results (list mediator-parent 
                         used-mediator source target mediation-service mapping-rules non-functional-properties)))
      (iu::send-soap-response2 
       "GET-MEDIATOR-DESCRIPTION-RESPONSE"
       (ip::replace-nils-with-empty-string results)
       (mapcar #'(lambda (result result-tag)
                   (declare (ignore result))
                   ;;now always use string
                   (list result-tag "string")
                   #|(if result
                       (list result-tag "sexpr") 
                     ;;"sexpr" uses the format directive ~s
                     ;;and we want to use ~a
                     (list result-tag "string"))
|#
                   )
               results
               '(mediator-parent 
                 used-mediator source target mediation-service reduction non-functional-properties))
       :stream stream
       :downcase-p t
       :result-tags-to-leave-case '(non-functional-properties)))))

(defun get-mediator-description-values (mediator-name ontology)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let* ((mediator-parent (get-class-parent mediator-name))
           (used-mediator (get-mediator mediator-name))
           (source (get-source mediator-name))
           (target (get-target mediator-name))
           (mediation-service (get-mediation-service mediator-name))
           ;;(reduction (get-reduction mediator-name))
           (mapping-rules (get-mapping-rules mediator-name))
           (non-functional-properties ;;(get-non-functional-properties mediator-name)
                                      (get-all-non-functional-property-values-as-string mediator-name)))
      ;;;get rid of the brackets around the mediator parents list
      (values (format nil "~{~a ~}" mediator-parent) 
              used-mediator source target mediation-service mapping-rules non-functional-properties))))

;;choreography



(defmethod ip::handle-post-soap-service ((action 
                                          (eql 
                                           'cl-user::save-choreography-description))
                                         stream namespace soap-values)
  (save-standard-wsmo-description 
   stream soap-values 
   *choreography-properties*
   'choreography
   *default-choreography-parent*))


(defmethod ip::handle-post-soap-service 
           ((action 
             (eql 
              'cl-user::delete-choreography-description))
            stream namespace soap-values)
  (handle-delete-wsmo-description stream soap-values 'choreography))

(defmethod ip::handle-post-soap-service 
           ((action 
             (eql 'cl-user::get-choreography-description))
            stream namespace soap-values)
  ;;;(setf sss soap-values)
  (handle-get-wsmo-description stream soap-values 'choreography
                               *choreography-properties*))

;;;orchestration

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 
                                           'cl-user::save-orchestration-description))
                                         stream namespace soap-values)
  (save-standard-wsmo-description 
   stream soap-values 
   *orchestration-properties*
   'orchestration
   *default-orchestration-parent*))


(defmethod ip::handle-post-soap-service 
           ((action 
             (eql 
              'cl-user::delete-orchestration-description))
            stream namespace soap-values)
  (handle-delete-wsmo-description stream soap-values 'orchestration))

(defmethod ip::handle-post-soap-service 
           ((action 
             (eql 'cl-user::get-orchestration-description))
            stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'orchestration
                               *orchestration-properties*))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 
                                           'cl-user::save-core-non-functional-properties-description))
                                         stream namespace soap-values)
  (save-standard-wsmo-description 
   stream soap-values 
   *core-non-functional-properties*
   'core-non-functional-properties
   *default-core-non-functional-properties-parent*))


(defun save-standard-wsmo-description (stream soap-values property-names
                                              properties-type default-parent)
  (let (user-name ontology wsmo-class-properties-name
                  wsmo-class-properties-parent
                  property-values
                  (response-string (format nil "SAVE-~a-DESCRIPTION-RESPONSE" properties-type)))
    (setf user-name (ip::make-ocml-symbol (pop soap-values))
          wsmo-class-properties-name (ip::make-ocml-symbol (pop soap-values))
          ontology (ip::make-ocml-symbol (pop soap-values))
          wsmo-class-properties-parent (ip::make-ocml-symbol (pop soap-values)))
    (dolist (x property-names)
      (setf property-values 
            (append property-values (list (ip::make-ocml-symbol (pop soap-values))))))
    (cond ((ok-to-edit-create-item-in-ontology
            wsmo-class-properties-name ontology)
           (multiple-value-bind (result ok-p)
               (internal-save-wsmo-class-properties-description
                user-name ontology wsmo-class-properties-name
                wsmo-class-properties-parent
                property-names property-values default-parent properties-type)
             (let ((*package* (find-package "WSMO-PROTOCOL")))
               (iu::send-soap-response2 
                response-string
                (list result)
                (if ok-p
                    `((result "string"))
                  `((warning "string")))
                :stream stream))))
          (t (ip::send-not-ok-to-edit wsmo-class-properties-name 
                                      properties-type
                                      ontology 'saved
                                      response-string
                                      stream)))))

(defun internal-save-wsmo-class-properties-description
       (user-name ontology wsmo-class-properties-name
                wsmo-class-properties-parent
                property-names property-values default-parent properties-type)
  (let (wsmo-class-properties-description-definition-result
        wsmo-class-properties-description-source-code)
    (maybe-create-goal-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf wsmo-class-properties-description-source-code
                     (generate-wsmo-class-properties-description-source-code
                      wsmo-class-properties-name
                      wsmo-class-properties-parent
                      property-names property-values)
                     wsmo-class-properties-description-definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-wsmo-class-properties-parent
                       wsmo-class-properties-parent
                       default-parent properties-type)
                      (ip::add-irs-knowledge-level-definition
                       ontology wsmo-class-properties-name
                       wsmo-class-properties-description-source-code))))
             (values 
              (ip::create-irs-browser-response-string 
               wsmo-class-properties-description-definition-result)
              t))
            (t (ip::warn-insufficient-priviliges user-name ontology))))))


(defun generate-wsmo-class-properties-description-source-code
       (wsmo-class-properties-name 
        wsmo-class-properties-parent
        property-names property-values)                                       
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,wsmo-class-properties-name (,wsmo-class-properties-parent) 
             ,(mapcan #'(lambda (property-name property-value)
                          (when property-value
                            `((,property-name :value ,(generate-slot-value property-value)))))
                      property-names property-values))))
    (format nil "~:w" code)))

(defmethod ip::handle-post-soap-service 
           ((action 
             (eql 
              'cl-user::delete-core-non-functional-properties-description))
            stream namespace soap-values)
  (handle-delete-wsmo-description stream soap-values 'core-non-functional-properties))

(defmethod ip::handle-post-soap-service 
           ((action 
             (eql 'cl-user::get-core-non-functional-properties-description))
            stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'core-non-functional-properties
                               *core-non-functional-properties*))

(defun internal-get-wsmo-class-properties-description (properties-type
                                                       wsmo-class-properties-name
                                                       ontology stream property-names)
  ;;(setf ty1 capability-name o1 ontology)
  (let ((properties-description
         (multiple-value-list
          (get-wsmo-class-properties-description-values 
           wsmo-class-properties-name ontology
           property-names)))
        (*package* (find-package "OCML")))
    (iu::send-soap-response2 
     (format nil "GET-~a-DESCRIPTION-RESPONSE" properties-type)
     (ip::replace-nils-with-empty-string properties-description)
     (mapcar #'(lambda (result result-tag)
                 (declare (ignore result))
                 ;;now always use string
                 (list result-tag "string")
                 #|(if result
                       (list result-tag "sexpr") 
                     ;;"sexpr" uses the format directive ~s
                     ;;and we want to use ~a
                     (list result-tag "string"))
|#
                 )
             properties-description
             (cons (intern (format nil "~a-PARENT" properties-type)
                           (find-package "WSMO-PROTOCOL"))
                   property-names))
     :stream stream
     ;;don't downcase now. Some values are strings and retaining case is important
     :downcase-p nil)))

(defun get-wsmo-class-properties-description-values (wsmo-class-properties-name
                                                     ontology property-names)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let* ((parent (get-class-parent wsmo-class-properties-name))
           (wsmo-class-properties-description-values
            (mapcar #'(lambda (property-name)
                        (ip::get-class-slot-value wsmo-class-properties-name
                                              property-name))
                    property-names)))
      ;;;get rid of the brackets around the parents list
      ;;(setf ocml::toto wsmo-class-properties-description-values)
      ;;(print "sldfkjslkdjflskdfj")
      (values-list
       (cons (format nil "~{~a ~}" parent) 
             wsmo-class-properties-description-values)))))

(defmethod ip::handle-post-soap-service ((action 
                                          (eql 
                                           'cl-user::save-web-service-non-functional-properties-description))
                                         stream namespace soap-values)
  (save-standard-wsmo-description 
   stream soap-values 
   *web-service-non-functional-properties*
   'core-non-functional-properties
   *default-web-service-non-functional-properties-parent*))

(defmethod ip::handle-post-soap-service 
           ((action 
             (eql 
              'cl-user::delete-web-service-non-functional-properties-description))
            stream namespace soap-values)
  (handle-delete-wsmo-description stream soap-values 'web-service-non-functional-properties))

(defmethod ip::handle-post-soap-service 
           ((action 
             (eql 'cl-user::get-web-service-non-functional-properties-description))
            stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'web-service-non-functional-properties
                               *web-service-non-functional-properties*))


(defmethod ip::handle-post-soap-service 
           ((action 
             (eql 'cl-user::get-ontology-goals-matching-input-type-description))
            stream namespace soap-values)
  (let (ontology type-description (*package* (find-package "OCML")))
    (setf ontology  (ip::make-ocml-symbol (pop soap-values))
          type-description (read-from-string (format nil "~a" (pop soap-values))))
    (internal-get-ontology-goals-matching-input-type-description 
     ontology type-description stream)))

(defun internal-get-ontology-goals-matching-input-type-description (ontology input-type-description
                                                                             stream)
  (ocml::select-ontology ontology)
  ;;(setf aa input-type-description)
  (let* ((*package* (find-package "OCML"))
        (type-description 
         (read-from-string (format nil "~a" input-type-description)))
        (goals
         (mapcan #'(lambda (goal)
                     (when (goal-matches-type-description goal type-description t)
                       (list goal)))
                 (ocml::setofall '?x '(ocml::subclass-of ?x 'ocml::goal)))))
    (iu::send-soap-response2 
     "GET-ONTOLOGY-GOALS-MATCHING-INPUT-TYPE-DESCRIPTION-RESPONSE"
     (mapcar #'(lambda (x) (string-downcase (symbol-name x)))
             goals)
     (mapcar #'(lambda (goal)
                 (declare (ignore goal))
                 (list 'goals "string"))
             goals)
     :stream stream
     :downcase-p t)))

(defun goal-matches-type-description (goal type-description &optional include-inherited-roles-p)
  (let ((input-role-names 
         (if include-inherited-roles-p
             (input-roles goal)
           (local-input-roles goal)))
        (result nil))
    (mapc #'(lambda (input-role-name)
              (let ((input-role-type 
                     (web-onto::findany
                      '?x 
                      `(ocml::class-slot-type ,goal
                                              ,input-role-name ?x))))
                (when (goal-type-matches-type-description input-role-type type-description)
                  (setf result (list input-role-name input-role-type)))))
          input-role-names)
    result))

(defun goal-type-matches-type-description (goal-type type-description)
  (let ((types (mapcar #'car (cdr type-description))))
    (case (car type-description)
      ((or)
       (dolist (type types)
         (when (or (eq goal-type type)
                   (ocml::holds? 'ocml::subclass-of goal-type type))
           (return t))))
      ((and)
       (let ((return-value t))
         (mapc #'(lambda (type)
                   (setf return-value
                         (and return-value
                              (or (eq goal-type type)
                                  (ocml::holds? 'ocml::subclass-of goal-type type)))))
               types)
         return-value)))))
;;  (web-onto::findany 'ocml::?x `(and (,goal-type ocml::?x) ,type-description)))

(defun internal-save-choreography-description
       (user-name ontology choreography-name choreography-parent 
                  choreography-grounding choreography-guarded-transitions)
  (let (choreography-definition-result
        choreography-source-code)
    (maybe-create-web-service-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf choreography-source-code
                     (generate-choreography-source-code choreography-name choreography-parent
                                                        choreography-grounding choreography-guarded-transitions)
                     choreography-definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-choreography-parent
                       choreography-parent)
                      (ip::add-irs-knowledge-level-definition
                       ontology choreography-name
                       choreography-source-code))))
             (values 
              (ip::create-irs-browser-response-string choreography-definition-result)
              t))
            (t (ip::warn-insufficient-priviliges user-name ontology))))))

(defun generate-choreography-source-code (choreography-name choreography-parent 
                                                            choreography-grounding choreography-guarded-transitions)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,choreography-name (,choreography-parent) 
             (,@(when choreography-grounding
                  `((ocml::has-grounding
                     :value 
                     ,(generate-slot-value choreography-grounding))))
              ,@(when choreography-guarded-transitions
                  `((ocml::has-guarded-transitions
                     :value ,(generate-slot-value choreography-guarded-transitions))))))))
    (format nil "~:w" code)))

(defun internal-save-orchestration-description
       (user-name ontology orchestration-name orchestration-parent orchestration-body)
  (let (orchestration-definition-result
        orchestration-source-code
        (problem-solving-pattern-name
         (generate-problem-solving-pattern-name-for-orchestration orchestration-name)))
    (internal-save-problem-solving-pattern-description
     user-name ontology problem-solving-pattern-name *default-problem-solving-pattern-parent*
     orchestration-body)
    (maybe-create-web-service-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf orchestration-source-code
                     (generate-orchestration-source-code orchestration-name orchestration-parent
                                                         problem-solving-pattern-name)
                     orchestration-definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-orchestration-parent
                       orchestration-parent)
                      (ip::add-irs-knowledge-level-definition
                       ontology orchestration-name
                       orchestration-source-code))))
             (values 
              (ip::create-irs-browser-response-string orchestration-definition-result)
              t))
            (t (ip::warn-insufficient-priviliges user-name ontology))))))

(defun generate-orchestration-source-code (orchestration-name orchestration-parent 
                                                        problem-solving-pattern-name)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,orchestration-name (,orchestration-parent) 
             (,@(when problem-solving-pattern-name
                  `((ocml::has-problem-solving-pattern
                     :value 
                     ,(generate-slot-value problem-solving-pattern-name))))))))
    (format nil "~:w" code)))


(defun internal-save-problem-solving-pattern-description
       (user-name ontology problem-solving-pattern-name problem-solving-pattern-parent
                  problem-solving-pattern-body)
  (let (problem-solving-pattern-definition-result
        problem-solving-pattern-source-code)
    (maybe-create-web-service-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf problem-solving-pattern-source-code
                     (generate-problem-solving-pattern-source-code 
                      problem-solving-pattern-name problem-solving-pattern-parent
                      problem-solving-pattern-body)
                     problem-solving-pattern-definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-problem-solving-pattern-parent
                       problem-solving-pattern-parent)
                      (ip::add-irs-knowledge-level-definition
                       ontology problem-solving-pattern-name
                       problem-solving-pattern-source-code))))
             (values 
              (ip::create-irs-browser-response-string problem-solving-pattern-definition-result)
              t))
            (t (ip::warn-insufficient-priviliges user-name ontology))))))

(defun get-orchestration-primitive-ocml-construct (orchestration-primitive)
  (cdr (assoc orchestration-primitive *orchestration-primitive-ocml-mappings*)))

(defun internal-substitute-ocml-primitives (atom)
  (or (get-ocml-construct-orchestration-primitive atom)
      atom))

(defun get-ocml-construct-orchestration-primitive (ocml-primitive)
  (car (rassoc ocml-primitive *orchestration-primitive-ocml-mappings*)))

(defun internal-substitute-orchestration-primitives (atom)
  (or (get-orchestration-primitive-ocml-construct atom)
      atom))

(defun substitute-primitives (code substition-function)
  (cond ((null code) nil)
        ((atom code)
         (funcall substition-function code))
        (t (cons (substitute-primitives (car code) substition-function)
                 (substitute-primitives (cdr code) substition-function)))))

(defun substitute-ocml-primitives (code)
  (substitute-primitives code 'internal-substitute-ocml-primitives))

(defun substitute-orchestration-primitives (code)
  (substitute-primitives code 'internal-substitute-orchestration-primitives))


(defun generate-readable-orchestration-body (code)
  (when code
    (if (eq (car code) 'cashew-workflow)
        (progn
          (setq cashew:*ocml-definitions* nil)
          (cashew:parse-cashew (second code))
          (cashew:ocml-define))
        `(ocml::def-orchestration
             ,@(substitute-ocml-primitives code)))))

(defun create-problem-solving-pattern-body-lambda (code)
  (when code
    (substitute-orchestration-primitives (cdr code))))

(defun generate-problem-solving-pattern-source-code (problem-solving-pattern-name problem-solving-pattern-parent 
                                                                                  problem-solving-pattern-body)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,problem-solving-pattern-name (,problem-solving-pattern-parent) 
             (,@(when problem-solving-pattern-body
                  `((ocml::has-body
                     :value 
                     ,(create-problem-solving-pattern-body-lambda
                       (generate-slot-value problem-solving-pattern-body)))))))))
    (format nil "~:w" code)))

(defmethod ip::handle-post-soap-service 
           ((action 
             (eql 
              'cl-user::get-info-mediator-source))
            stream namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (ip::make-ocml-symbol (pop soap-values)))
    (ip::with-ontology-check (ontology stream)
      (ocml::select-ontology ontology)
      (iu::send-soap-response2 
       "GET-INFO-MEDIATOR-SOURCE-RESPONSE"
       (ip::get-mediator-source-with-ontology (ip::make-ocml-symbol (pop soap-values)))
       '((source "sexpr") (ontology "sexpr"))
       :stream stream))))

(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::get-info-mediator-target))
                                         stream  namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (ip::make-ocml-symbol (pop soap-values)))
    (ip::with-ontology-check (ontology stream)
      (ocml::select-ontology ontology)
      (iu::send-soap-response2
       "GET-INFO-MEDIATOR-TARGET-RESPONSE"
       (ip::get-mediator-target-with-ontology (ip::make-ocml-symbol (pop soap-values)))
       '((target "sexpr") (ontology "sexpr"))
       :stream  stream))))

(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::get-info-mediator-targets))
                                         stream  namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (ip::make-ocml-symbol (pop soap-values)))
    (ip::with-ontology-check (ontology stream)
      (ocml::select-ontology ontology)
      (let ((targets 
             (ip::get-mediator-targets-with-ontology (ip::make-ocml-symbol (pop soap-values)))))
        (iu::send-soap-response2
         "GET-INFO-MEDIATOR-TARGET-RESPONSE"
         targets
         (mapcar #'(lambda (x)
                     (declare (ignore x))
                     (list 'usedmediator "sexpr"))
                 targets)
         :stream  stream)))))

(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::get-info-mediation-service))
                                         stream namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (ip::make-ocml-symbol (pop soap-values)))
    (ip::with-ontology-check (ontology stream)
      (ocml::select-ontology ontology)
      (iu::send-soap-response2
       "GET-INFO-MEDIATION-SERVICE-RESPONSE"
       (ip::get-mediator-service-with-ontology (ip::make-ocml-symbol (pop soap-values)))
       '((result "sexpr") (ontology "sexpr"))
       :stream stream))))

(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::get-info-used-Mediators))
                                         stream namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (ip::make-ocml-symbol (pop soap-values)))
    (ip::with-ontology-check (ontology stream)
      (ocml::select-ontology ontology)
      (let ((used-mediators 
             (ip::get-item-used-mediators-with-ontology (ip::make-ocml-symbol (pop soap-values)))))
      (iu::send-soap-response2
       "GET-INFO-USED-MEDIATORS-RESPONSE"
       used-mediators
       (mapcar #'(lambda (x)
                   (declare (ignore x))
                   (list 'usedmediator "sexpr"))
               used-mediators)
       :stream stream)))))

(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::get-web-service-capability))
                                         stream namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (ip::make-ocml-symbol (pop soap-values)))
    (ip::with-ontology-check (ontology stream)
      (ocml::select-ontology ontology)
      (iu::send-soap-response2
       "GET-WEB-SERVICE-CAPABILITY-RESPONSE"
       (ip::get-web-service-capability-with-ontology (ip::make-ocml-symbol (pop soap-values)))
       '((capability "sexpr") (ontology "sexpr"))
       :stream stream))))

(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::get-web-service-interface))
                                         stream namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (ip::make-ocml-symbol (pop soap-values)))
    (ip::with-ontology-check (ontology stream)
      (ocml::select-ontology ontology)
      (iu::send-soap-response2
       "GET-WEB-SERVICE-INTERFACE-RESPONSE"
       (ip::get-web-service-interface-with-ontology (ip::make-ocml-symbol (pop soap-values)))
       '((interface "sexpr") (ontology "sexpr"))
       :stream stream))))

(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::upload-code))
                                         stream namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         (knowledge-model (ip::make-ocml-symbol (pop soap-values)))
         (knowledge-model-uses (ip::make-ocml-list (pop soap-values)))
         (knowledge-model-type (intern (symbol-name (pop soap-values))
                                       (find-package "KEYWORD")))
         (knowledge-model-format (ip::make-ocml-symbol (pop soap-values)))
         (code (pop soap-values))
         (upload-mode (ip::make-ocml-symbol (pop soap-values)))
         (author (pop soap-values))
         (password (pop soap-values))
         (allowed-editors (pop soap-values))) 
    (iu::send-soap-response2
     "UPLOAD-CODE-RESPONSE"
     (list (with-output-to-string (ostream)
             (ip::internal-upload knowledge-model
                                  knowledge-model-uses
                                  knowledge-model-type 
                                  knowledge-model-format
                                  code upload-mode
                                  author
                                  password allowed-editors 
                                  ostream)))
     '((result "string"))
     :stream  stream)))

(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::get-ontology-classes-and-instances))
                                         stream namespace soap-values)

  (let* ((*package* (find-package "OCML"))
         (ontology (ip::make-ocml-symbol (pop soap-values)))
         (view-only-items-in-current-ontology (pop soap-values)))
    
    (if (ocml::get-ontology ontology) 
        (multiple-value-bind (class-descriptions ok-p)
            (iu::generate-class-descriptions ontology view-only-items-in-current-ontology t)
          (if ok-p         
              (iu::send-soap-response2
               "GET-ONTOLOGY-CLASSES-AND-INSTANCES-RESPONSE"
               (list (format nil "<ontology><ontology_name>~a</ontology_name>~{~a~}</ontology>"
                             ontology class-descriptions))
               '((result "string"))
               :stream  stream)
            (iu::send-soap-warning-message 'error-getting-classes-and-instances 
                                           'GET-ONTOLOGY-CLASSES-AND-INSTANCES-RESPONSE stream)))
      (iu::send-soap-warning-message 'error-no-ontology 
                                     'GET-ONTOLOGY-CLASSES-AND-INSTANCES-RESPONSE stream))))

;; Added Carlos 25-10-2006
;; Just retrieve the concepts
(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::get-ontology-classes))
                                         stream namespace soap-values)

  (let* ((*package* (find-package "OCML"))
         (ontology (ip::make-ocml-symbol (pop soap-values)))
         (view-only-items-in-current-ontology (pop soap-values)))
    
    (if (ocml::get-ontology ontology) 
        (multiple-value-bind (class-descriptions ok-p)
            (iu::generate-class-descriptions ontology view-only-items-in-current-ontology t nil) ;; Don't include instances here

          (if ok-p         
              (iu::send-soap-response2
               "GET-ONTOLOGY-CLASSES"
               (list (format nil "<ontology><ontology_name>~a</ontology_name>~{~a~}</ontology>"
                             ontology class-descriptions))
               '((result "string"))
               :stream  stream)
            (iu::send-soap-warning-message 'error-getting-classes-and-instances 
                                           'GET-ONTOLOGY-CLASSES stream)))
      (iu::send-soap-warning-message 'error-no-ontology 
                                     'GET-ONTOLOGY-CLASSES stream))))


(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::find-mediator))
                                         stream namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         (ontology (ip::make-ocml-symbol (pop soap-values)))
         (mediator-type (ip::make-ocml-symbol (pop soap-values)))
         (mediator-filters (ip::make-ocml-list (pop soap-values))))
    (if (ocml::get-ontology ontology)
        (iu::send-soap-response2
         "FIND-MEDIATOR-RESPONSE"
         (list (format nil "~{~a ~}"
                       (find-mediators ontology mediator-type mediator-filters)))
         '((result "string"))
         :stream   stream)
      (iu::send-soap-warning-message 'error-no-ontology 
                                     'GET-ONTOLOGY-CLASSES-AND-INSTANCES-RESPONSE stream))))

;; Added by Carlos 21-9-06
(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::get-class-definition))
                                         stream namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         (ontology-name (ip::make-ocml-symbol (pop soap-values)))
         (class-name (ip::make-ocml-symbol (pop soap-values)))
         (include-instances (pop soap-values)))
    
    (ocml::select-ontology ontology-name)
    (if (ocml::get-ontology ontology-name)
      (let ((ocml-class 
             (ocml::get-ocml-class class-name)))
        (if ocml-class
            (let ((class-definition 
                   (iu::generate-class-description ontology-name class-name ocml-class include-instances t)))
              (iu::send-soap-response2
               "GET-CLASS-DEFINITION-RESPONSE"
               (list class-definition)
               '((result "string"))
               :stream  stream)
              (iu::send-soap-warning-message 'error-getting-class 
                                             'GET-CLASS-DEFINITION-RESPONSE stream))))
        (iu::send-soap-warning-message 'error-no-ontology 
                                       'GET-CLASS-DEFINITION-RESPONSE stream))))

;; Added by Carlos 08-10-06
;; Register an observer for monitoring IRS
(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::add-observer))
                                         stream namespace soap-values)
  (let* ((*package* (find-package "OCML"))
         (observer-host (pop soap-values))
         (observer-port (pop soap-values))
         ;; probably we will need also some sort of session id
         )
    ;; Create the socket, bind to it and if correct notify the success
    (when (null observer-port)
      (setf observer-port 9090))
    (let ((monitor-stream (comm::open-tcp-stream observer-host observer-port)))
      (if monitor-stream
          (let ((monitoring-observer
                 (im::create-monitoring-observer
                  observer-host (irs.api.soap-old::new-monitoring-callback monitor-stream))))
            (iu::send-soap-response2
             "ADD-OBSERVER-RESPONSE"
             (list (format nil "<monitoring_observer>~a</monitoring_observer>"
                           monitoring-observer))
             '((result "string"))
             :stream  stream)))
      (iu::send-soap-response2
       "ADD-OBSERVER-RESPONSE"
       (list "Error adding observer")
       '((result "string"))
       :stream  stream))))

;; Added John 01-11-2006
(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::get-lifting-and-lowering))
                                         stream namespace soap-values)
 
  (let* ((*package* (find-package "OCML"))
         (ontology (ip::make-ocml-symbol (pop soap-values))))   
    (if (ocml::get-ontology ontology)
        (multiple-value-bind (lifting-and-lowering-code ok-p)
            (get-lifting-and-lowering-code ontology)
          (if ok-p        
              (iu::send-soap-response2
               "GET-LIFTING-AND-LOWERING"
               (list (format nil "<lifting_and_lowering>~a</lifting_and_lowering>"
                             lifting-and-lowering-code))
               '((result "string"))
               :stream  stream)
            (iu::send-soap-warning-message 'error-getting-lifting-and-lowering
                                           'GET-LIFTING-AND-LOWERING stream)))
      (iu::send-soap-warning-message 'error-no-ontology
                                     'GET-LIFTING-AND-LOWERING stream))))
 
;; Added John 01-11-2006
(defmethod ip::handle-post-soap-service ((action (eql 'cl-user::save-lifting-and-lowering))
                                         stream namespace soap-values)
 
  (let* ((*package* (find-package "OCML"))
         (ontology (ip::make-ocml-symbol (pop soap-values)))
         (new-code (pop soap-values)))
    (if (ocml::get-ontology ontology)
        (multiple-value-bind (load-message ok-p)
            (save-lifting-and-lowering-code ontology new-code)
          (if ok-p        
              (iu::send-soap-response2
               "SAVE-LIFTING-AND-LOWERING"
               (list "<lifting_and_lowering>OK</lifting_and_lowering>")
               '((result "string"))
               :stream  stream)
            (iu::send-soap-warning-message 'error-saving-lifting-and-lowering
                                           'SAVE-LIFTING-AND-LOWERING stream)))
      (iu::send-soap-warning-message 'error-no-ontology
                                     'SAVE-LIFTING-AND-LOWERING stream))))

;;Added 12/Nov/2008 by Neil B.
;;{{{
(defmethod ip::handle-post-soap-service ((action 
                                          (eql 'cl-user::get-publisher-information))
                                         stream namespace soap-values)
  (handle-get-wsmo-description stream soap-values 'publisher-information))
  
(defmethod get-interface-description ((item-type (eql 'publisher-information))
                                      web-service-name
                                      ontology stream
                                      &optional property-names)
  (declare (ignore property-names))
  (internal-get-publisher-information
   web-service-name ontology stream))  
   
(defun internal-get-publisher-information (web-service-name ontology stream)
  (multiple-value-bind (web-service-host 
                        web-service-port 
                        web-service-location)
      (get-publisher-information-values web-service-name ontology)
    (let ((*package* (find-package "OCML"))
          (results (list web-service-host 
                        web-service-port 
                        web-service-location)))
      (iu::send-soap-response2 
       "GET-PUBLISHER-INFORMATION-RESPONSE"
       (ip::replace-nils-with-empty-string results)
       (mapcar #'(lambda (result result-tag-info)
                   (let ((result-tag (if (listp result-tag-info) (car result-tag-info) result-tag-info))
                         ;;use string if the result was nil
                         (soap-type (if (and (listp result-tag-info) result)
                                        (second result-tag-info) "string")))
                   (list result-tag soap-type)))
               results
               '(web-service-host 
                        web-service-port 
                        web-service-location))
       :stream stream
       :downcase-p t))))

(defun get-publisher-information-values (web-service-name ontology)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let* ((associated-publisher (get-associated-publisher-information web-service-name))
           (web-service-host (get-web-service-host associated-publisher))
           (web-service-port (get-web-service-port associated-publisher))           
           (web-service-location (get-web-service-location associated-publisher)))      
      (values web-service-host web-service-port web-service-location))))

(defun get-associated-publisher-information (web-service-name)
  (web-onto::findany '?x `(ocml::associated-publisher-information ,web-service-name ?x)))
			  
(defun get-web-service-host (publisher-name)
  (ip::get-class-slot-value publisher-name 'ocml::has-web-service-host))
  
(defun get-web-service-port (publisher-name)
  (ip::get-class-slot-value publisher-name 'ocml::has-web-service-port))
  
(defun get-web-service-location (publisher-name)
  (ip::get-class-slot-value publisher-name 'ocml::has-web-service-location))
;;}}}

