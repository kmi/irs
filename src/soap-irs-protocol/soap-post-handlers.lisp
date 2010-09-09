(in-package irs-protocol)


(defun create-irs-browser-response-string (&rest strings)
  (let ((result-string (apply #'concatenate 'string strings)))
    (substitute #\space web-onto::*ocml-line-separator*
                result-string)))


(defmethod handle-post-soap-service (action stream namespace soap-values)
  ;;(setf bb (list action soap-values))
  (cl-user::handle-soap-service
   action stream soap-values))

(defun cl-user::order-input-roles-from-method
       (ontology method-name input-roles)
  ;;(setf oo ontology mm method-name ii input-roles)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((task (get-psm-tackles-task-type method-name)))
      (if task
          (cl-user::order-input-roles-from-task ontology task 
                                                input-roles)
        input-roles))))

(defun cl-user::order-input-roles-from-task 
       (ontology task input-roles)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((task-input-roles (get-all-task-input-roles task)))
      (mapcan
       #'(lambda (task-input-role)
           (let ((given-input-role 
                  (assoc task-input-role input-roles)))
             (when given-input-role
               (list given-input-role))))
       task-input-roles))))

(defmethod handle-post-soap-service ((action (eql 'cl-user::psm-soap-definition))
                                     stream namespace soap-values)
  (destructuring-bind (ontology method-name input-roles output 
                                lisp-function publisher-ip-address publisher-port
                                publisher-location)
      soap-values
    ;;(setf ssbb soap-values)
    (setf ontology (make-ocml-symbol ontology)
          method-name (make-ocml-symbol method-name)
          input-roles 
          (mapcar #'(lambda (input-role) 
                      (list (make-ocml-symbol (car input-role))
                            (second input-role))) input-roles)
          output (make-ocml-symbol output)
          lisp-function (make-ocml-symbol lisp-function)
          publisher-location (make-ocml-symbol publisher-location))
    (setf input-roles
          (cl-user::order-input-roles-from-method 
           ontology method-name input-roles))
    (internal-psm-soap-definition ontology method-name
                                  input-roles output lisp-function
                                  publisher-ip-address
                                  publisher-port publisher-location)
    (let ((*package* (find-package "IRS-PROTOCOL")))
      (iu::send-soap-response2 
       "PSM-SOAP-DEFINITION-RESPONSE"
       (list 'ok)
       `((result "sexpr"))
       :stream stream))))

(defmethod handle-post-soap-service ((action (eql 'cl-user::achieve-task))
                                     stream namespace soap-values)
  ;;;(setf ss soap-values)
  (let* ((ontology (make-ocml-symbol (car soap-values)))
        (task-type (make-ocml-symbol (second soap-values)))
        (input-role-names-and-soap-types 
         (get-task-input-roles-and-soap-types ontology task-type))
        (input-role-value-pairs 
         (make-input-role-value-pairs2 input-role-names-and-soap-types
                                      (cdddr soap-values))))
  (cond ((ocml::get-ontology ontology)
           (ocml::with-ocml-thread-safety
             (ocml::select-ontology ontology)
             (ip::irs-achieve-task ontology 
                                   task-type input-role-value-pairs 
                                   stream t)))
          (t ;;(send-soap-error "ACHIEVE-TASK-RESPONSE" 
             ;;                 'error-no-ontology stream)

             (iu::send-soap-warning-message 'error-no-ontology
                                      'ACHIEVE-TASK-RESPONSE stream)
             ))))

;;this one will match the existing protocol
(defmethod handle-post-soap-service ((action (eql 'cl-user::task-info))
                                     stream namespace soap-values)
  ;;(setf ss soap-values)
  (let ((ontology (make-ocml-symbol (car soap-values)))
        (psm (make-ocml-symbol (second soap-values))))
    (cond ((ocml::get-ontology ontology)
           (ocml::with-ocml-thread-safety
             (ocml::select-ontology ontology)
             (multiple-value-bind (input-roles output)
                 (internal-get-task-info ontology psm)
               (if (eq input-roles 'no-task)
                   ;;send back an error message
                   ;;need to work out what this should be
                   ;;(send-soap-error "TASK-INFO-RESPONSE" 'error-no-task stream)
                   (iu::send-soap-warning-message  
                    'error-no-task 'TASK-INFO-RESPONSE
                    stream)
                 ;;(progn (setf aa input-roles bb output)
                 (iu::send-soap-response2 
                  "TASK-INFO-RESPONSE"
                  (list input-roles output)
                  `((args "sexpr") (result "string"))
                  :stream stream)))))
          (t 
           ;;(send-soap-error "TASK-INFO-RESPONSE" 'error-no-ontology stream)
             (iu::send-soap-warning-message 'error-no-ontology
                                      'TASK-INFO-RESPONSE
                                       stream)))))

(defun task-psm-already-defined-in-ancestor-ontology-p (item-name 
                                                        ontology)
  (or (null (ocml::get-ontology ontology))
      (ocml::with-ocml-thread-safety ()
        (ocml::select-ontology ontology)
        (let ((class (ocml::get-ocml-class item-name)))
          ;;no existing definition
          (or (null class)
              (eq ontology (ocml::name (ocml::home-ontology class))))))))

(defun send-not-ok-to-edit (item-name item-type 
                                      ontology edit-operation-type
                                      response-string stream)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let* ((class (ocml::get-ocml-class item-name))
           (error-message
            (format nil 
                    "The ~(~a~) ~(~a~) was not ~(~a~) because it is defined in the ontology ~(~a~) and not in the ontology ~(~a~)"
                    item-type item-name edit-operation-type 
                    (ocml::name (ocml::home-ontology class))
                    ontology)))
      (iu::send-soap-response2 
       response-string 
       (list error-message)
       `((warning "string"))
       :stream stream))))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::save-task-description))
                                     stream namespace soap-values)
  (let (user-name ontology task-name task-parent number-of-input-roles 
                  input-roles
                  output precondition goal-expression)

    (setf user-name (make-ocml-symbol (pop soap-values))
          task-name (make-ocml-symbol (pop soap-values))
          ontology (make-ocml-symbol (pop soap-values))
          task-parent (make-ocml-symbol (pop soap-values))
          number-of-input-roles (pop soap-values))
    (cond ((task-psm-already-defined-in-ancestor-ontology-p 
            task-name ontology)
           (dotimes (i number-of-input-roles)
             (setf input-roles
                   (append input-roles 
                           ;;input-role-name input-role-type input-role-soap-type
                           (list (list (make-ocml-symbol
                                        (pop soap-values))
                                       (make-ocml-symbol 
                                        (pop soap-values))
                                       (make-soap-type 
                                        namespace
                                        (pop soap-values)))))))
           ;;output-role-name input-role-type input-role-soap-type
           (setf output (list (make-ocml-symbol (pop soap-values))
                              (make-ocml-symbol (pop soap-values))
                              (make-soap-type namespace
                                              (pop soap-values)))
                 precondition (make-ocml-symbol (pop soap-values))
                 goal-expression (make-ocml-symbol (pop soap-values)))
           ;;(setf ppp (list ontology task-name number-of-input-roles input-roles
           ;;              output precondition goal-expression))
           (multiple-value-bind (result ok-p)
                 (internal-save-task-description
                  user-name ontology task-name task-parent input-roles
                  output precondition goal-expression)
           (iu::send-soap-response2 
            "SAVE-TASK-DESCRIPTION-RESPONSE"
            (list result)
            (if ok-p
            `((result "string"))
            `((warning "string")))
            :stream stream)))
          (t (send-not-ok-to-edit task-name 'task ontology 'saved
                                  "SAVE-TASK-DESCRIPTION-RESPONSE"
                                  stream)))))

(defun generate-task-source-code (task-name task-parent input-roles 
                                            output precondition 
                                            goal-expression)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,task-name (,task-parent) ocml::?task
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
              ,@(when goal-expression
                  `((ocml::has-goal-expression 
                     :value ,(read-from-string 
                              (format nil "~a"
                                      goal-expression)))))
              ,@(when precondition
                  `((ocml::has-precondition 
                     :value 
                     ,(read-from-string 
                       (format nil "~a"
                               precondition)))))))))
    (format nil "~(~:w~)" code)))



#|
(iu::def-irs-soap-bindings arthritis-therapy-task ;;this is the ontology name
  arthritis-therapy  ;;this is the task name
  ((condition "sexpr")
   )
  "sexpr"
  )
|#

(defun generate-soap-bindings-source-code (ontology task-name
                                                    input-roles 
                                                    output)
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-irs-soap-bindings ,task-name ,ontology 
             ,(mapcar #'(lambda (x) (list (car x) (third x)))
                      input-roles)
             ,(third output))))
    (format nil "~(~:w~)" code)))


(defun maybe-create-ontology (type user-name ontology
                                             &optional 
                                             ontology-uses
                                             (allowed-editors ""))
  (unless (ocml::get-ontology ontology)
    (web-onto::define-new-ontology 
     *standard-output*
     ontology
     type
     ontology-uses user-name allowed-editors)))

(defun maybe-create-task-ontology (user-name ontology
                                             &optional 
                                             ontology-uses
                                             (allowed-editors ""))
  (maybe-create-ontology 'ocml::task user-name ontology 
                         ontology-uses allowed-editors))

(defun maybe-create-psm-ontology (user-name ontology
                                            &optional
                                            ontology-uses
                                            (allowed-editors ""))
  (maybe-create-ontology 'ocml::method user-name ontology 
                         ontology-uses allowed-editors))

(defun add-psm-definition (ontology psm-name psm-source-code)
  (add-irs-knowledge-level-definition ontology psm-name psm-source-code))

(defun add-task-definition (ontology task-name task-source-code)
  (add-irs-knowledge-level-definition ontology task-name task-source-code))


(defun test-new-source (ontology item-name source-code item-exists-p
                        &optional instance-class-name)
  (ocml::select-ontology ontology)
  (handler-case 
      (let ((*package* (find-package "OCML")))
        (eval (read-from-string source-code))
        (unless item-exists-p
#|
          (web-onto::internal-delete-items 
           ontology 
           (if instance-class-name
               `((ocml::instance ,item-name ,instance-class-name))
             `((ocml::class ,item-name)))
           *standard-output*)
|#
          (if instance-class-name
              (ocml::delete-ocml-object 'ocml::instance item-name instance-class-name)
            (ocml::delete-ocml-object 'ocml::class item-name)))
        (values t nil))
    (serious-condition 
     (c)
     (values nil (format nil "Error: ~a when evaluating ~a. New source not saved." c source-code)))
    (error 
     (c)
     (values nil (format nil "Error: ~a when evaluating ~a. New source not saved." c source-code)))))

(defun add-irs-knowledge-level-definition (ontology item-name item-source-code)
  (let ((item-exists-p (ocml::get-domain-class item-name)))
    (multiple-value-bind (test-ok-p warning-message)
        (test-new-source ontology item-name item-source-code item-exists-p)
      (if test-ok-p
          (with-output-to-string (stream)
            (if item-exists-p
                ;;new source for existing item
                (web-onto::internal-process-new-source 
                 'ocml::def-class item-name item-source-code stream
                 ontology)
              ;;;new definition
              (web-onto::process-new-definition 
               (string ontology) item-source-code stream)))
        warning-message))))


(defun add-irs-instance-definition (ontology item-name item-class-name item-source-code)
  ;;(format t  "~%1 ontology ~a" (ocml::name ocml::*current-ontology*))
  (let ((item-exists-p (ocml::find-current-instance item-name item-class-name)))
    (multiple-value-bind (test-ok-p warning-message)
        (test-new-source ontology item-name item-source-code item-exists-p item-class-name)
      ;;(format t "~%2 ontology ~a" (ocml::name ocml::*current-ontology*))
      (if test-ok-p
          (with-output-to-string (stream)
            (if item-exists-p
                ;;new source for existing item
                (web-onto::internal-process-new-source 
                 'ocml::def-instance item-name item-source-code stream
                 ontology item-class-name)
              ;;;new definition
              (web-onto::process-new-definition 
               (string ontology) item-source-code stream)))
        warning-message))))

(defun cl-user::add-irs-instance-definition (ontology item-name item-class-name item-source-code)
  ;;(format t  "~%3 ontology ~a" (ocml::name ocml::*current-ontology*))
  (add-irs-instance-definition ontology item-name item-class-name item-source-code))

(defun add-soap-binding-source-code (ontology task-name 
                                              soap-bindings-source-code)
  ;;webonto adds a newline to the end of the returned message
  (remove 
   #\return
   (remove
    #\linefeed
    (with-output-to-string (stream)
      (if (iu::get-methods-definition ontology task-name)
          ;;new source for existing item
          (web-onto::internal-process-new-source 
           'ocml::def-irs-soap-bindings
           task-name soap-bindings-source-code stream
           ontology)
        ;;;new definition
        (web-onto::process-new-definition 
         (string ontology) soap-bindings-source-code
         stream))))))

(defvar *default-task-parent*
  'ocml::goal-specification-task)

(defvar *default-psm-parent*
  'ocml::primitive-method)

(defun maybe-create-warning-about-task-parent (task-parent)
  (maybe-create-warning-about-task-psm-parent 
   'task task-parent
   *default-task-parent*))

(defun maybe-create-warning-about-psm-parent (psm-parent)
  (maybe-create-warning-about-task-psm-parent 
   'problem-solving-method psm-parent *default-psm-parent*))

(defun maybe-create-warning-about-task-psm-parent (type parent 
                                                   default-parent)
  (if (eq parent default-parent)
      ""
    (format nil "Warning: the ~(~a~)'s parent is ~(~a~) rather than the standard ~(~a~). " type parent default-parent)))
  

(defun internal-save-task-description
       (user-name ontology task-name task-parent input-roles
                  output precondition goal-expression)
  (let (task-definition-result
        soap-binding-definition-result
        task-source-code soap-bindings-source-code)
    (maybe-create-task-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf task-source-code
                     (generate-task-source-code task-name task-parent
                                                input-roles 
                                                output precondition 
                                                goal-expression)
                     soap-bindings-source-code
                     (generate-soap-bindings-source-code ontology
                                                         task-name input-roles 
                                                         output)
                     task-definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-task-parent
                       task-parent)
                      (add-task-definition ontology task-name
                                           task-source-code))
                     soap-binding-definition-result
                     (add-soap-binding-source-code ontology task-name 
                                                   soap-bindings-source-code)))
             (values 
              (create-irs-browser-response-string task-definition-result 
                          " [for soap bindings " 
                          soap-binding-definition-result "]")
              t))
            (t (warn-insufficient-priviliges user-name ontology))))))


(defmethod handle-post-soap-service ((action 
                                      (eql 
                                       'cl-user::delete-task-description))
                                     stream namespace soap-values)
  ;;(setf ss soap-values)
  (let (user-name ontology task-name)
    (setf user-name (make-ocml-symbol (pop soap-values))
          task-name (make-ocml-symbol (pop soap-values))
          ontology (make-ocml-symbol (pop soap-values)))
    (cond ((task-psm-already-defined-in-ancestor-ontology-p 
            task-name ontology)
           (multiple-value-bind (result ok-p)
               (internal-delete-task-description
                user-name ontology task-name)
             (iu::send-soap-response2 
              "DELETE-TASK-DESCRIPTION-RESPONSE"
              (list result)
              (if ok-p 
                  `((result "string"))
                `((warning "string")))
              :stream stream)))
          (t (send-not-ok-to-edit task-name 'task ontology 'deleted
                                  "DELETE-TASK-DESCRIPTION-RESPONSE"
                                  stream)))))

(defun warn-no-ontology (ontology)
  (format nil "The ontology ~A does not exist." ontology))

(defun warn-insufficient-priviliges (user-name ontology)
  (format nil "The user ~A does not have sufficient priviliges for ontology ~(~a~)." 
          user-name ontology))

(defun internal-delete-task-description (user-name ontology task-name)
  (let ((ontology-structure (ocml::get-ontology ontology)))
    (cond (ontology-structure
           (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
                  (values 
                   (ocml::with-ocml-thread-safety ()
                     (let ((web-onto::*ocml-line-separator*
                            #\space))
                       (declare (special 
                                 web-onto::*ocml-line-separator*))
                       (with-output-to-string (stream)
                         (iu::remove-method-definition ontology task-name)
                         (web-onto::internal-delete-items 
                          ontology 
                          `((ocml::class ,task-name)
                            (ocml::soap-binding
                             ,task-name))
                          stream))))
                   t))
                 (t (warn-insufficient-priviliges user-name ontology))))
          (t (warn-no-ontology ontology)))))

(defmethod handle-post-soap-service ((action 
                                      (eql 
                                       'cl-user::delete-psm-description))
                                     stream namespace soap-values)
  ;;(setf ss soap-values)
  (let (user-name ontology psm-name)
    (setf user-name (make-ocml-symbol (pop soap-values))
          psm-name (make-ocml-symbol (pop soap-values))
          ontology (make-ocml-symbol (pop soap-values)))
    (cond ((task-psm-already-defined-in-ancestor-ontology-p
            psm-name ontology)
           (multiple-value-bind (result ok-p)
               (internal-delete-psm-description
                user-name ontology psm-name)
             (iu::send-soap-response2 
              "DELETE-PSM-DESCRIPTION-RESPONSE"
              (list result)
              (if ok-p
                  `((result "string"))
                `((warning "string")))
              :stream stream)))
          (t (send-not-ok-to-edit psm-name 'problem-solving-method 
                                  ontology 'deleted
                                  "DELETE-PSM-DESCRIPTION-RESPONSE"
                                  stream)))))

(defun internal-delete-psm-description (user-name ontology psm-name)
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
                          `((ocml::class ,psm-name))
                          stream))))
                   t))
                 (t (warn-insufficient-priviliges user-name ontology))))
          (t (warn-no-ontology ontology)))))
                 

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::save-psm-description))
                                     stream namespace soap-values)
  (let (user-name ontology psm-name psm-parent number-of-input-roles
                  input-roles
                  output precondition applicability-condition 
                  tackles-task-type)

    (setf user-name (make-ocml-symbol (pop soap-values))
          psm-name (make-ocml-symbol (pop soap-values))
          ontology (make-ocml-symbol (pop soap-values))
          psm-parent (make-ocml-symbol (pop soap-values))
          number-of-input-roles (pop soap-values))
    (cond ((task-psm-already-defined-in-ancestor-ontology-p psm-name 
                                                            ontology)
           (dotimes (i number-of-input-roles)
             (setf input-roles
                   (append input-roles 
                           ;;input-role-name input-role-type
                           (list (list (make-ocml-symbol (pop soap-values))
                                       (make-ocml-symbol (pop soap-values)))))))
           ;;output-role-name output-role-type output-role-soap-type
           (setf output (list (make-ocml-symbol (pop soap-values))
                              (make-ocml-symbol (pop soap-values)))
                 precondition (make-ocml-symbol (pop soap-values))
                 applicability-condition 
                 (make-ocml-symbol (pop soap-values))
                 tackles-task-type (make-ocml-symbol (pop soap-values)))
           ;;(setf ppp (list ontology task-name number-of-input-roles input-roles
           ;;              output precondition goal-expression))
           (multiple-value-bind (result ok-p)
               (internal-save-psm-description
                user-name ontology psm-name psm-parent input-roles
                output applicability-condition tackles-task-type
                precondition)
             (iu::send-soap-response2 
              "SAVE-PSM-DESCRIPTION-RESPONSE"
              (list result)
              (if ok-p
                  `((result "string"))
                `((warning "string")))
              :stream stream)))
          (t (send-not-ok-to-edit psm-name 'problem-solving-method
                                  ontology 'saved
                                  "SAVE-PSM-DESCRIPTION-RESPONSE"
                                  stream)))))

(defun generate-psm-source-code (psm-name psm-parent
                                          input-roles 
                                          output precondition 
                                          applicability-condition 
                                          tackles-task-type )
  (let* ((*package* (find-package "OCML"))
         (code
          `(ocml::def-class ,psm-name (,psm-parent) ocml::?psm
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
              ,@(when precondition
                  `((ocml::has-precondition 
                     :value 
                     ,(read-from-string 
                       (format nil "~a"
                               precondition))))))
             ,@(when (or applicability-condition tackles-task-type)
                 `(:own-slots             
                   (,@(when applicability-condition
                        `((ocml::applicability-condition 
                           ,(read-from-string 
                             (format nil "~a"
                                     applicability-condition)))))
                    ,@(when tackles-task-type
                        `((ocml::tackles-task-type 
                           ,(read-from-string 
                             (format nil "~a"
                                     tackles-task-type)))))))))))
    (format nil "~(~:w~)" code)))

(defun internal-save-psm-description
       (user-name ontology psm-name psm-parent input-roles
                  output applicability-condition tackles-task-type
                  precondition)
  (let (psm-definition-result
        psm-source-code)
    (maybe-create-psm-ontology user-name ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (cond ((web-onto::ok-to-edit-p ontology-structure user-name)
             (ocml::with-ocml-thread-safety ()
               (ocml::select-ontology ontology)
               (setf psm-source-code
                     (generate-psm-source-code psm-name psm-parent
                                               input-roles 
                                               output precondition
                                               applicability-condition 
                                               tackles-task-type )
                     psm-definition-result
                     (concatenate 
                      'string
                      (maybe-create-warning-about-psm-parent
                       psm-parent)
                      (add-psm-definition ontology psm-name
                                          psm-source-code))))
             (values 
              (create-irs-browser-response-string psm-definition-result)
              t))
            (t (warn-insufficient-priviliges user-name ontology))))))

(defun user-name-and-password-ok-p (name password)
  (let ((stored-name (assoc name web-onto::*users* :test #'string=)))
    (and stored-name (string= (second stored-name) password))))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::login))
                                     stream namespace soap-values)
  ;;(setf s29 soap-values)
  (let (user-name user-password result result-tag)
    (setf user-name (pop soap-values)
          user-password (pop soap-values))
    (if (user-name-and-password-ok-p user-name user-password)
        (setf result "USER_LOGIN_OK" result-tag 'result)
      (setf result "USER_LOGIN_NOT_OK" result-tag 'warning))
    (iu::send-soap-response2 
     "LOGIN-RESPONSE"
     (list result)
     `((,result-tag "string"))
     :stream stream)))

#|
<TASK-NAME xsi:type="sexpr">test-service-task-description</TASK-NAME>
            <ONTOLOGY xsi:type="sexpr">test-service-task-ontology</ONTOLOGY>
            <TASK-PARENT xsi:type="sexpr">goal-specification-task</TASK-PARENT>
            <NUMBER-OF-INPUT-ROLE-VALUE-PAIRS xsi:type="int">3</NUMBER-OF-INPUT-ROLE-VALUE-PAIRS>
            <HAS-SOURCE-CURRENCY xsi:type="sexpr">HAS-SOURCE-CURRENCY</HAS-SOURCE-CURRENCY>
            <INPUT-TYPE xsi:type="sexpr">CURRENCY</INPUT-TYPE>
            <INPUT-SOAP-TYPE xsi:type="sexpr">sexpr</INPUT-SOAP-TYPE>
            <HAS-TARGET-CURRENCY xsi:type="sexpr">HAS-TARGET-CURRENCY</HAS-TARGET-CURRENCY>
            <INPUT-TYPE xsi:type="sexpr">CURRENCY</INPUT-TYPE>
            <INPUT-SOAP-TYPE xsi:type="sexpr">sexpr</INPUT-SOAP-TYPE>
            <HAS-AMOUNT xsi:type="sexpr">HAS-AMOUNT</HAS-AMOUNT>
            <INPUT-TYPE xsi:type="sexpr">POSITIVE-NUMBER</INPUT-TYPE>
            <INPUT-SOAP-TYPE xsi:type="sexpr">float</INPUT-SOAP-TYPE>
            <OUTPUT-NAME xsi:type="sexpr">HAS_CURRENCY_CONVERSION</OUTPUT-NAME>
            <OUTPUT-TYPE xsi:type="sexpr">POSITIVE-NUMBER</OUTPUT-TYPE>
            <OUTPUT-SOAP-TYPE xsi:type="sexpr">float</OUTPUT-SOAP-TYPE>
            <PRE-CONDITION xsi:type="sexpr">nil</PRE-CONDITION>
            <GOAL xsi:type="sexpr">(this is not a goal expression in ocml)</GOAL>
|#


(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::get-task-description))
                                     stream namespace soap-values)
  ;;(setf ss soap-values)
  (let (ontology task-name)
    (setf task-name (make-ocml-symbol (pop soap-values))
          ontology (make-ocml-symbol (pop soap-values)))
    ;;ontology may be a sub-ontology of the task - e.g. a psm ontology
    (ocml::with-ocml-thread-safety ()
      (ocml::select-ontology ontology)
      (setf ontology (ocml::name
                      (ocml::home-ontology (ocml::get-domain-class task-name))))
      (internal-get-task-description task-name ontology stream))))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::get-non-local-task-description))
                                     stream namespace soap-values)
  ;;(setf ss soap-values)
  (let (ontology task-name)
    (setf task-name (make-ocml-symbol (pop soap-values))
          ontology (make-ocml-symbol (pop soap-values)))
    ;;ontology may be a sub-ontology of the task - e.g. a psm ontology
    (ocml::with-ocml-thread-safety ()
      (ocml::select-ontology ontology)
      (setf ontology (ocml::name
                      (ocml::home-ontology (ocml::get-domain-class task-name))))
      (internal-get-task-description task-name ontology stream t))))

(defun replace-nils-with-empty-string (list)
  (substitute "" nil list))

(defun internal-get-task-description (task-name ontology stream &optional non-local-p)
  ;;(setf ty1 task-name o1 ontology)
  (multiple-value-bind (task-parent number-of-input-roles input-roles
                                    output precondition goal-expression)
      (get-task-description-values task-name ontology non-local-p)
    (let ((*package* (find-package "OCML"))
          (results (append (list task-parent number-of-input-roles)
                           input-roles
                           output
                           (list precondition goal-expression))))
      (iu::send-soap-response2 
       "GET-TASK-DESCRIPTION-RESPONSE"
       (replace-nils-with-empty-string results)
       (mapcar #'(lambda (result result-tag)
                   (if result
                       (list result-tag "sexpr")
                     ;;"sexpr" uses the format directive ~s
                     ;;and we want to use ~a
                     (list result-tag "string")))
               results
               (append '(task-parent number-of-input-role-value-pairs) 
                       (apply #'append 
                              (make-list number-of-input-roles
                                         :initial-element
                                         '(input-name input-type input-soap-type)))
                       '(output-name output-type output-soap-type 
                                     precondition goal-expression)))
       :stream stream
       :downcase-p t))))

(defun get-task-description-values (task-name ontology non-local-p)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let* ((task-parent (get-task-parent task-name))
           (input-roles (get-task-input-roles-with-soap-types ontology task-name non-local-p))
           (number-of-input-roles (/ (length input-roles) 3))
           (output (get-task-output ontology task-name non-local-p))
           (precondition (get-task-precondition task-name))
           (goal-expression  (get-task-goal-expression task-name)))
      (values task-parent number-of-input-roles input-roles
              output precondition goal-expression))))

;;;assumme only one superclass
(defun get-task-parent (task-name)
  ;;(web-onto::findany '?x `(ocml::superclass-of ?x ,task-name)))
  (let ((class (ocml::get-domain-class task-name)))
    (when class
      (mapcar #'ocml::name (ocml::direct-domain-superclasses class)))))

#|
(defvar *soap-type-string-start* "")

(defun transform-role-soap-type (x)
  (let* ((*package* (find-package "OCML")))
    (when (and x (stringp x)
               (> (length x) (length *soap-type-string-start*)))
      (read-from-string 
       (string-upcase (subseq x (length *soap-type-string-start*)))))))
|#

(defun transform-role-soap-type (x)
  (let* ((*package* (find-package "OCML")))
    (when (and x (stringp x))
      (read-from-string 
       (string-upcase x)))))

(defun get-input-role-soap-type (ontology task-name input-role-name non-local-p)
  (let ((input-roles 
         (if non-local-p
             (get-task-non-local-input-roles ontology task-name)
           (iu::get-task-input-roles ontology task-name))))
    (transform-role-soap-type 
     (second (assoc input-role-name input-roles)))))

(defun get-output-role-soap-type (ontology task-name non-local-p)
  (transform-role-soap-type
   (if non-local-p 
       (get-task-non-local-output-role ontology task-name)
     (iu::get-task-output-role ontology task-name))))

;;put reverse here so that input role are returned in teh same 
;;order as get-task-local-input-roles
;;now use the fast version
(defun get-all-task-input-roles (task-name)
  ;;(reverse (ocml::setofall '?x 
   ;;                        `(ocml::has-input-role ,task-name ?x))))
   (ocml::findany '?x `(ocml::all-task-input-roles ,task-name ?x)))

(defun get-task-local-input-roles (task-name)
  ;;(ocml::setofall '?x 
  ;;                `(ocml::has-local-input-role ,task-name ?x)))
  ;;now use the fast version
  (ocml::findany'?x `(ocml::all-local-input-roles ,task-name ?x)))

(defun get-task-input-roles-with-soap-types (ontology task-name non-local-p)
  (let ((input-role-names 
         (if non-local-p
             (get-all-task-input-roles task-name)
           (get-task-local-input-roles task-name))))
    (mapcan #'(lambda (input-role-name)
                (list input-role-name 
                      (web-onto::findany 
                       '?x 
                       `(ocml::class-slot-type ,task-name
                                               ,input-role-name ?x))
                      (get-input-role-soap-type ontology task-name input-role-name non-local-p)))
            input-role-names)))

(defun get-task-input-roles-with-types (task-name &optional include-inherited-roles-p)
  (let ((input-role-names 
         (if include-inherited-roles-p
             (get-all-task-input-roles task-name)
           (get-task-local-input-roles task-name))))
    (mapcar #'(lambda (input-role-name)
                (list input-role-name 
                      (web-onto::findany 
                       '?x 
                       `(ocml::class-slot-type ,task-name
                                               ,input-role-name ?x))))
            input-role-names)))

(defun task-has-matching-input-role-p (task-name type &optional include-inherited-roles-p)
  (let ((input-role-names 
         (if include-inherited-roles-p
             (get-all-task-input-roles task-name)
           (get-task-local-input-roles task-name)))
        (result nil))
    (mapc #'(lambda (input-role-name)
              (let ((input-role-type 
                     (web-onto::findany 
                      '?x 
                      `(ocml::class-slot-type ,task-name
                                              ,input-role-name ?x))))
                (when (and input-role-type
                           (or (eq input-role-type type)
                               (ocml::holds? 'ocml::subclass-of input-role-type type)))
                  (setf result (list input-role-name input-role-type)))))
          input-role-names)
    result))

(defun get-task-input-soap-types (ontology task-name)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (setf ontology (ocml::name (ocml::home-ontology (ocml::get-domain-class task-name))))
    (let ((input-role-names 
           ;;(get-task-local-input-roles task-name)
           ;;now get inherited input roles for task assume this is for achieve-task
           (get-all-task-input-roles task-name)))
      (mapcar #'(lambda (input-role-name)
                  (get-input-role-soap-type ontology task-name input-role-name t))
              input-role-names))))

(defun get-task-input-roles-and-soap-types (ontology task-name)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (setf ontology (ocml::name (ocml::home-ontology (ocml::get-domain-class task-name))))
    (let ((input-role-names 
           ;;(get-task-local-input-roles task-name)
           ;;now get inherited input roles for task assume this is for achieve-task
           (get-all-task-input-roles task-name)))
      (mapcar #'(lambda (input-role-name)
                  (list input-role-name (get-input-role-soap-type ontology task-name input-role-name t)))
              input-role-names))))

(defun get-task-output (ontology task-name non-local-p)
  (let ((output-role 
         (if non-local-p
             (web-onto::findany '?x `(ocml::has-output-role ,task-name ?x))
           (web-onto::findany '?x `(ocml::has-local-output-role ,task-name ?x)))))
    (list output-role
          (web-onto::findany '?x 
                             `(ocml::class-slot-type ,task-name
                                                     ,output-role ?x))  
          (get-output-role-soap-type ontology task-name non-local-p))))

(defun task-has-matching-output-role-p (task-name type)
  (let* ((output-role 
          (web-onto::findany '?x `(ocml::has-local-output-role ,task-name ?x)))
         (output-role-type (web-onto::findany '?x 
                                              `(ocml::class-slot-type ,task-name
                                                                      ,output-role ?x))))
    (when (and output-role-type
               (or (eq output-role-type type)
                   (ocml::holds? 'ocml::subclass-of output-role-type type)))
      (list output-role output-role-type))))

(defun get-task-precondition (task-name)
  (web-onto::findany '?x 
                     `(= ?x (ocml::the-class-slot-value 
                             ,task-name ocml::has-precondition))))

(defun get-task-goal-expression (task-name)
  (web-onto::findany '?x 
                     `(= ?x (ocml::the-class-slot-value 
                             ,task-name ocml::has-goal-expression))))



(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::get-psm-description))
                                     stream namespace soap-values)
  (let (ontology psm-name)
    (setf psm-name (make-ocml-symbol (pop soap-values))
          ontology (make-ocml-symbol (pop soap-values)))
    (internal-get-psm-description psm-name ontology stream)))

(defun internal-get-psm-description (psm-name ontology stream)
  (multiple-value-bind (psm-parent number-of-input-roles input-roles
                                   output precondition applicability-condition 
                                   tackles-task-type)
      (get-psm-description-values psm-name ontology)
    (let ((*package* (find-package "OCML"))
          (results (append (list psm-parent number-of-input-roles)
                           input-roles
                           output
                           (list precondition applicability-condition 
                                 tackles-task-type))))
      (iu::send-soap-response2 
       "GET-PSM-DESCRIPTION-RESPONSE"
       (replace-nils-with-empty-string results)
       (mapcar #'(lambda (result result-tag)
                   (if result
                       (list result-tag "sexpr")
                     ;;"sexpr" uses the format directive ~s
                     ;;and we want to use ~a
                     (list result-tag "string")))
               results
               (append '(psm-parent number-of-input-role-value-pairs) 
                       (apply #'append 
                              (make-list number-of-input-roles
                                         :initial-element 
                                         '(input-name input-type)))
                       '(output-name output-type 
                                     precondition applicability-condition 
                                     tackles-task-type)))
       :stream stream
       :downcase-p t))))

(defun get-psm-description-values (psm-name ontology)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let* ((psm-parent (get-psm-parent psm-name))
           (input-roles (get-psm-input-roles psm-name))
           (number-of-input-roles (/ (length input-roles) 2))
           (output (get-psm-output psm-name))
           (precondition (get-psm-precondition psm-name))
           (applicability-condition  
            (get-psm-applicability-condition psm-name))
           (tackles-task-type 
            (get-psm-tackles-task-type psm-name)))
      (values psm-parent number-of-input-roles input-roles output precondition 
              applicability-condition tackles-task-type))))


;;;assumme only one superclass
(defun get-psm-parent (psm-name)
  (get-task-parent psm-name))

(defun get-psm-input-roles (psm-name)
  (let ((input-role-names 
         (ocml::setofall '?x 
                         `(ocml::has-local-input-role ,psm-name ?x))))
    (mapcan #'(lambda (input-role-name)
                (list input-role-name 
                      (web-onto::findany '?x 
                                         `(ocml::class-slot-type ,psm-name
                                                                 ,input-role-name ?x))))
            input-role-names)))

(defun get-psm-output (psm-name)
  (let ((output-role 
         (web-onto::findany '?x 
                            `(ocml::has-local-output-role ,psm-name ?x))))
    (list output-role
          (web-onto::findany '?x 
                             `(ocml::class-slot-type ,psm-name
                                                     ,output-role ?x)))))

(defun get-psm-precondition (psm-name)
  (get-task-precondition psm-name))

(defun get-psm-applicability-condition (psm-name)
  (web-onto::findany '?x `(ocml::applicability-condition ,psm-name ?x)))  

(defun get-psm-tackles-task-type (psm-name)
  (web-onto::findany '?x `(ocml::tackles-task-type ,psm-name ?x)))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::create-ontology))
                                     stream namespace soap-values)
  (let (user-name user-password ontology-name
                  type ontology-uses allowed-editors)
    (setf user-name (pop soap-values)
          user-password (pop soap-values)
          ontology-name (make-ocml-symbol (pop soap-values))
          type (make-ocml-symbol (pop soap-values))
          ontology-uses (mapcar #'make-ocml-symbol (pop soap-values))
          allowed-editors (pop soap-values))
    (irs-create-ontology stream
                         user-name user-password ontology-name
                         type ontology-uses allowed-editors)))

#|
(defun irs-create-ontology (stream
                            user-name user-password ontology-name
                            type ontology-uses allowed-editors)
  ;;(setf ll (list user-name user-password ontology-name
  ;;                        type ontology-uses allowed-editors))
  (if (user-name-and-password-ok-p user-name user-password)
      (if (ocml::get-ontology ontology-name)
          (iu::send-soap-response2 
           "CREATE-ONTOLOGY-RESPONSE"
           (list 'ontology-exists)
           `((result "string"))
           :stream stream)
        (let ((result 
               (maybe-create-ontology 
                type user-name ontology-name
                ontology-uses allowed-editors)))
          (if result
              (iu::send-soap-response2 
               "CREATE-ONTOLOGY-RESPONSE"
               (list 'ok)
               `((result "string"))
               :stream stream)
            
            (iu::send-soap-response2 
             "CREATE-ONTOLOGY-RESPONSE"
             (list 'not_ok)
             `((result "string"))
             :stream stream))))
    (iu::send-soap-response2 
     "CREATE-ONTOLOGY-RESPONSE"
     (list 'invalide-user-name-or-password)
     `((result "string"))
     :stream stream)))
|#


(defun send-irs-action-response (tag response stream)
  (iu::send-soap-response2 
   tag
   (list response)
   `((result "string"))
   :stream stream))

(defun send-irs-warning-response (tag response stream)
  (iu::send-soap-response2 
   tag
   (list response)
   `((warning "string"))
   :stream stream))

(defun irs-create-ontology (stream
                            user-name user-password ontology-name
                            type ontology-uses allowed-editors)
  (if (user-name-and-password-ok-p user-name user-password)
      (if (ocml::get-ontology ontology-name)
          (send-irs-warning-response "CREATE-ONTOLOGY-RESPONSE"
                                    'ontology-exists stream)
        (let ((result 
               (maybe-create-ontology 
                type user-name ontology-name
                ontology-uses allowed-editors)))
          (if result
              (send-irs-action-response "CREATE-ONTOLOGY-RESPONSE"
                                        'ok stream)
            
            (send-irs-warning-response "CREATE-ONTOLOGY-RESPONSE"
                                      'not_ok stream))))
    (send-irs-warning-response "CREATE-ONTOLOGY-RESPONSE" 
                              'invalid-user-name-or-password
                              stream)))


(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::delete-ontology))
                                     stream namespace soap-values)
  (let (user-name user-password ontology-name)
    (setf user-name (pop soap-values)
          user-password (pop soap-values)
          ontology-name (make-ocml-symbol (pop soap-values)))
    (irs-delete-ontology stream
                         user-name user-password ontology-name)))

(defun irs-delete-ontology (stream
                            user-name user-password ontology-name)
  (if (user-name-and-password-ok-p user-name user-password)
      (let ((ontology (ocml::get-ontology ontology-name)))
        (if ontology
            (cond ((or (web-onto::root-user-p user-name)
                       (web-onto::ontology-owner-p ontology user-name))
                   (with-output-to-string (ostream)
                     (web-onto::internal-delete-ontology
                      ostream ontology-name ontology user-name))
                   (send-irs-action-response "DELETE-ONTOLOGY-RESPONSE" 
                                             'ok stream))
                  (t             
                   (send-irs-warning-response "DELETE-ONTOLOGY-RESPONSE"
                                             'not_ontology_owner
                                             stream)))
          (send-irs-warning-response "DELETE-ONTOLOGY-RESPONSE"
                                    'ontology-does-not-exist
                                    stream)))
    (send-irs-warning-response "DELETE-ONTOLOGY-RESPONSE"
                              'invalid-user-name-or-password
                              stream)))


(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::get-ontology-properties))
                                     stream namespace soap-values)
  (let (user-name user-password ontology-name)
    (setf user-name (pop soap-values)
          user-password (pop soap-values)
          ontology-name (make-ocml-symbol (pop soap-values)))
    (irs-get-ontology-properties
     stream
     user-name user-password ontology-name)))

(defun irs-get-ontology-properties (stream
                                    user-name user-password ontology-name)
  (if (user-name-and-password-ok-p user-name user-password)
      (let ((ontology (ocml::get-ontology ontology-name)))
        (if ontology
            (send-ontology-properties ontology stream)
            (send-irs-warning-response
                "GET-ONTOLOGY-PROPERTIES-RESPONSE"
                'ontology-does-not-exist stream)))
    (send-irs-warning-response
     "GET-ONTOLOGY-PROPERTIES-RESPONSE"
     'invalid-user-name-or-password stream)))

(defun send-ontology-properties (ontology stream)
  (let ((results (get-ontology-properties ontology)))
    (iu::send-soap-response2 
     "GET-ONTOLOGY-PROPERTIES-RESPONSE"
     (replace-nils-with-empty-string results)
     (mapcar #'(lambda (result-tag)
                 ;;"sexpr" uses the format directive ~s
                 ;;and we want to use ~a
                 (list result-tag "string"))
             '(ontology-includes ontology-allowed-editors 
                                 ontology-author 
                                 ontology-type))
     :stream stream
     :downcase-p t)))

(defun get-ontology-properties (ontology)
  (let ((ontology-includes
         (ocml::ontology-includes ontology))
        (ontology-allowed-editors 
         (ocml::ontology-allowed-editors ontology)))
    (list
     (when ontology-includes
       (format nil "~a~{ ~a~}" (ocml::name (car ontology-includes))
               (mapcar #'ocml::name (cdr ontology-includes))))
     (when ontology-allowed-editors
       (format nil "~a~{ ~a~}" (car ontology-allowed-editors)
               (cdr ontology-allowed-editors)))
     (ocml::ontology-author ontology)
     (ocml::ontology-type ontology))))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::update-ontology-properties))
                                     stream namespace soap-values)
  (let (old-ontology-name new-ontology-name ontology-type ontology-uses 
                          ontology-new-author ontology-editors user-name 
                          user-password)
    (setf user-name (pop soap-values)
          user-password (pop soap-values)
          old-ontology-name (make-ocml-symbol (pop soap-values))
          new-ontology-name (make-ocml-symbol (pop soap-values))
          ontology-type (make-ocml-symbol (pop soap-values))
          ontology-uses (mapcar #'make-ocml-symbol (pop soap-values))
          ontology-new-author (pop soap-values)
          ontology-editors (pop soap-values))
    (irs-update-ontology-properties
     stream
     user-name user-password old-ontology-name new-ontology-name 
     ontology-type ontology-uses ontology-new-author ontology-editors)))

(defun update-ontology-properties (ontology new-ontology-name 
                                            ontology-new-author 
                                            ontology-type ontology-uses 
                                            ontology-editors)
  (with-output-to-string (ostream)
    (web-onto::internal-update-ontology-properties 
     ostream
     ontology new-ontology-name ontology-new-author
     (intern (symbol-name ontology-type)
             (find-package "KEYWORD"))
     ontology-uses ontology-editors)))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::get-psm-web-service))
                                     stream namespace soap-values)
  (let (ontology psm-name)
    (setf psm-name (make-ocml-symbol (pop soap-values))
          ontology (make-ocml-symbol (pop soap-values)))
    (internal-get-psm-web-service psm-name ontology stream)))

(defun internal-get-psm-web-service (psm-name ontology stream)
  (let ((web-service-pathname
         (cl-user::get-psm-publisher-location 
          psm-name ontology)))
    (multiple-value-bind (web-service-host web-service-port) 
        (get-publisher-host-and-port 
         (intern (concatenate 'string (symbol-name ontology) "_"
                              (symbol-name psm-name))
                 (find-package "OCML")))
      ;;(setf P psm-name o ontology c web-service-pathname w web-service-host)
      (let* ((*package* (find-package "OCML"))
             (psm-post-url-pathname
              (get-psm-post-url-pathname web-service-pathname))
             (result (if (and web-service-host
                              ;;exclude auto generated urls
                              (not psm-post-url-pathname))
                         (format nil
                                 "http://~a~:[~*~;:~d~]~:[~*~;~a~]"
                                 web-service-host
                                 web-service-port web-service-port
                                 web-service-pathname web-service-pathname)
                       "")))
        (iu::send-soap-response2 
         "GET-PSM-WEB-SERVICE-RESPONSE"
         (list result)
         '((result "string"))
         :stream stream
         :downcase-p t)))))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::publish-web-application))
                                     stream namespace soap-values)
  ;(setf n namespace s soap-values)
  (let (host port path ontology psm-name 
             variables)
    (setf host (pop soap-values)
          port (pop soap-values)
          path (pop soap-values)
          ontology (make-ocml-symbol (pop soap-values))
          psm-name (make-ocml-symbol (pop soap-values))
          variables (pop soap-values))
    (http::internal-irs-create-publish-url-code host port path ontology 
                                                psm-name
                                                variables)
    (send-irs-action-response 
     "PUBLISH-WEB-APPLICATION-RESPONSE"
     'ok stream)))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::get-lisp-function-name))
                                     stream namespace soap-values)
  (let (ontology psm-name request-name)
    (setf ontology  (make-ocml-symbol (pop soap-values))
          psm-name (make-ocml-symbol (pop soap-values)))
    (setf request-name
          (get-request-name (iu::make-irs-method-id 
                             psm-name ontology
                             "OCML")))
    (send-irs-action-response 
     "GET-LISP-FUNCTION-NAME-RESPONSE"
     request-name stream)))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::get-all-tasks))
                                     stream namespace soap-values)
  (let ((ontologies-and-tasks (ocml::all-tasks))
        (*package* (find-package "OCML")))
    (iu::send-soap-response2 
     "GET-ALL-TASKS-RESPONSE"
     (mapcan #'(lambda (ontology-and-task)
                 (copy-list ontology-and-task))
             ontologies-and-tasks)
     (mapcan #'(lambda (ontology-and-task)
                 (declare (ignore ontology-and-task))
                 (list (list 'ontology "sexpr") (list 'task "sexpr")))
             ontologies-and-tasks)
     :stream stream
     :downcase-p t)))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::get-ontology-tasks))
                                     stream namespace soap-values)
  (let (ontology (*package* (find-package "OCML"))
                 tasks)
    (setf ontology  (make-ocml-symbol (pop soap-values)))
    (ocml::select-ontology ontology)
    (setf tasks (ocml::setofall '?x '(ocml::subclass-of ?x 'ocml::task)))
    (iu::send-soap-response2 
     "GET-ONTOLOGY-TASKS-RESPONSE"
     tasks 
     (mapcar #'(lambda (task)
                 (declare (ignore task))
                 (list 'task "sexpr"))
             tasks)
     :stream stream
     :downcase-p t)))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::get-ontology-tasks-matching-input-type))
                                     stream namespace soap-values)
  (let (ontology type (*package* (find-package "OCML")))
    (setf ontology  (make-ocml-symbol (pop soap-values))
          type (make-ocml-symbol (pop soap-values)))
    (internal-get-ontology-tasks-matching-input-type ontology type stream)))

(defun internal-get-ontology-tasks-matching-input-type (ontology type stream)
  (ocml::select-ontology ontology)
  (let ((*package* (find-package "OCML"))
        (tasks 
         (mapcan #'(lambda (task)
                     (when (task-has-matching-input-role-p task type t)
                       (list task)))
                 (ocml::setofall '?x '(ocml::subclass-of ?x 'ocml::task)))))
    (iu::send-soap-response2 
     "GET-ONTOLOGY-TASKS-MATCHING-INPUT-TYPE-RESPONSE"
     tasks 
     (mapcar #'(lambda (task)
                 (declare (ignore task))
                 (list 'task "sexpr"))
             tasks)
     :stream stream
     :downcase-p t)))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::get-ontology-tasks-matching-output-type))
                                     stream namespace soap-values)
  (let (ontology type (*package* (find-package "OCML")))
    (setf ontology (make-ocml-symbol (pop soap-values))
          type (make-ocml-symbol (pop soap-values)))
    (internal-get-ontology-tasks-matching-output-type ontology type stream)))

(defun internal-get-ontology-tasks-matching-output-type (ontology type stream)
  (ocml::select-ontology ontology)
  (let ((*package* (find-package "OCML"))
        (tasks 
         (mapcan #'(lambda (task)
                     (when (task-has-matching-output-role-p task type)
                       (list task)))
                 (ocml::setofall '?x '(ocml::subclass-of ?x 'ocml::task)))))
    (iu::send-soap-response2 
     "GET-ONTOLOGY-TASKS-MATCHING-OUTPUT-TYPE-RESPONSE"
     tasks 
     (mapcar #'(lambda (task)
                 (declare (ignore task))
                 (list 'task "sexpr"))
             tasks)
     :stream stream
     :downcase-p t)))

(defun irs-update-ontology-properties (stream
                                       user-name user-password 
                                       old-ontology-name new-ontology-name 
                                       ontology-type ontology-uses
                                       ontology-new-author ontology-editors)
  (if (user-name-and-password-ok-p user-name user-password)
      (let ((ontology (ocml::get-ontology old-ontology-name)))
        (if ontology
            (cond ((or (web-onto::root-user-p user-name)
                       (web-onto::ontology-owner-p ontology user-name)
                       (web-onto::allowed-editor-p ontology user-name))
                   (update-ontology-properties ontology 
                                               new-ontology-name
                                               ontology-new-author
                                               ontology-type 
                                               ontology-uses 
                                               ontology-editors)
                   (send-irs-action-response 
                    "UPDATE-ONTOLOGY-PROPERTIES-RESPONSE"
                    'ok stream))
                  (t             
                   (send-irs-warning-response 
                    "UPDATE-ONTOLOGY-PROPERTIES-RESPONSE"
                    'not_ontology_owner stream)))
          (send-irs-warning-response
           "UPDATE-ONTOLOGY-PROPERTIES-RESPONSE"
           'ontology-does-not-exist stream)))
    (send-irs-warning-response
     "UPDATE-ONTOLOGY-PROPERTIES-RESPONSE"
     'invalid-user-name-or-password stream)))

(defmethod handle-post-soap-service ((action 
                                      (eql 'cl-user::upload-ontology))
                                     stream namespace soap-values)
;;;  (setf ss3 soap-values)
  (let (user-name user-password ontology-name code)
    (setf user-name (pop soap-values)
          user-password (pop soap-values)
          code (pop soap-values)
          ontology-name (intern (string-upcase (pop soap-values))
                                (find-package "OCML")))
    (cond ((user-name-and-password-ok-p user-name user-password)
           (let ((ontology (ocml::get-ontology ontology-name)))
             (if ontology
                 (cond ((or (web-onto::root-user-p user-name)
                            (web-onto::ontology-owner-p ontology user-name)
                            (web-onto::allowed-editor-p ontology user-name))
                        (add-code-to-ontology ontology-name code)
                        (iu::send-soap-response2 
                         "UPLOAD-ONTOLOGY-RESPONSE"
                         (list 'ok)
                         `((ok "string"))
                         :stream stream))
                       (t (send-irs-warning-response 
                           "UPLOAD-ONTOLOGY-RESPONSE"
                           'not_ontology_owner stream)))
               (send-irs-warning-response
                "UPLOAD-ONTOLOGY-RESPONSE"
                'ontology-does-not-exist stream))))
          (t (send-irs-warning-response
              "UPLOAD-ONTOLOGY-RESPONSE"
              'invalid-user-name-or-password stream)))))

(defun add-code-to-ontology (ontology code &optional overwrite-p)
  (let ((new-source-file 
         (ocml::ontology-new-source-location (ocml::get-ontology ontology))))
    (if overwrite-p 
        (internal-overwrite-code-in-ontology ontology code new-source-file)
      (internal-append-code-to-ontology code new-source-file))
    (load new-source-file)))


(defun internal-append-code-to-ontology (code new-source-file)
  (with-open-file (ostream new-source-file :direction :output :if-exists :append)
    (format ostream "~%~%~a" code)))

(defun internal-overwrite-code-in-ontology (ontology code new-source-file)
  (when (probe-file new-source-file)
    (delete-file new-source-file))
  (with-open-file (ostream new-source-file :direction :output :if-does-not-exist :create)
    (format ostream web-onto::*edits-file-header*)
    (format ostream "~%~%(in-ontology ~(~a~))~%~%" ontology)
    (format ostream "~%~%~a" code)))


