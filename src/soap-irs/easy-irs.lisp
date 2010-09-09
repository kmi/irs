(in-package ocml)

;; soap support for the irs server
;; Authors Mauro Gaspari November 2002

(defun create-irs-server-defys-file (method-name 
                                     &optional 
                                     (directory *irs-server-generated-code-directory*))
     ;;(format http::*http-stream* "FFFF~s" method-name)
  (let ((defsys-file-name (merge-pathnames
                           (merge-pathnames 
                            ;;use unique names to avoid multiple registrations
                            ;;clashing "defsys"
                            (format nil "~(~a~)-defsys" method-name)
                            directory)
                           ".lisp"))
        (system-name (intern (concatenate 'string 
                                          (symbol-name method-name) 
                                          "-SERVER")
                             (find-package "CL-USER"))))
    ;;(format http::*http-stream* "FFFFGGGG~s" system-name)
     (when (probe-file defsys-file-name)
      (delete-file defsys-file-name))
    (with-open-file (ostream defsys-file-name :direction :output :if-does-not-exist :create)
      (format ostream "(in-package cl-user)~%

(defsystem ~(~a~) ()
  :members (
            \"run-~a\"
            )
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))"
              system-name (string-downcase (symbol-name method-name))
              (string-downcase (symbol-name method-name))))
    (values defsys-file-name system-name)))

;            (\"~a_idl\" :type :idl-file) remove idl from system

(defun make-irs-server-soap-query-file-name (x &optional 
                                               (directory *irs-server-generated-code-directory*))
  (merge-pathnames
   (merge-pathnames (concatenate 'string "run-"
                                 (string-downcase (symbol-name x))) directory)
   ".lisp"))

(defun create-irs-server-soap-expressions (publisher-ip-address
				     publisher-port
                                     ontology 
                                     method-name
				     input-roles
                                     output-type
				     request-name
                                     &optional 
                                     (directory 
                                     *irs-server-generated-code-directory*)
                                     (publisher-location "/soap"))
  ;;(format http::*http-stream* "HERE9:~%")
  ;;(format http::*http-stream* "HERE8:~a~%" input-roles)
  (declare (ignore ontology))
  (let ((irs-server-soap-query-file-name
         (make-irs-server-soap-query-file-name method-name directory))
        (arguments (mapcar #'car input-roles)))
    ;;(ip::careful-add-file irs-server-soap-query-file-name)
    ;;(format http::*http-stream* "HERE9:~a~%" irs-server-soap-query-file-name)
  (when (probe-file irs-server-soap-query-file-name)
      (delete-file irs-server-soap-query-file-name))
    (with-open-file (ostream irs-server-soap-query-file-name
                             :direction :output :if-does-not-exist :create)
      ;;(format http::*http-stream* "HERE11:~%")
      ;;(ip::careful-add-file method-name)
      (format ostream 
              "(in-package cl-user)~%~%(defun run-~(~a ~a~)
                  (iu::soap-http-client  ~(~s~) (quote ~(~S~)) (list~{ ~(~a~)~}) :host ~s :port ~d :publisher-location ~s :output-type 'api-soap-old::~a))"                
	      method-name arguments 
	      request-name input-roles arguments
	       publisher-ip-address publisher-port publisher-location output-type))))

(defun irs-achieve-task (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream) ;;the achieve-task bit
    (with-ocml-thread-safety
      (let* ((*package* (find-package "OCML"))
             (ontology (read istream))
             (task-type (read istream))
             (input-role-value-pairs (read istream))
             (*in-irs* t))
        (cond ((get-ontology ontology)
               (http::princ-to-binary-stream
                (irs-achieve-task-internal ontology task-type input-role-value-pairs)
                http::*http-stream*))
              (t (http::princ-to-binary-stream
                  (format nil "~a ~a ~a~%" *condition-flag* 'ontology-not-found ontology)
                  http::*http-stream*)))))))


#|

changed since enrico fixed achieve-task to work with the tasks ontology
(defun irs-achieve-task-internal (ontology task-type input-role-value-pairs)
  ;;(setf o ontology y task-type i input-role-value-pairs)
  (select-ontology ontology)
  (let ((task-instance (define-domain-instance 
                        (gentemp (symbol-name task-type))
                        task-type
                        ""
                        input-role-value-pairs)))
    (visualiser:processing ontology task-type input-role-value-pairs)
    (ocml-eval-gen `(solve-task ,(name task-instance)))))
|#

(defun irs-achieve-task-internal (ontology task-type input-role-value-pairs)
  (select-ontology ontology)
  (visualiser:processing ontology task-type input-role-value-pairs)
  (ocml-eval-gen `(achieve-generic-task ',task-type ',input-role-value-pairs)))

#|
;;new use which *only works from the task ontology* but will find items in the 
;;correct psms
(defun irs-achieve-task-internal (ontology task-type input-role-value-pairs)
  (setf o ontology y task-type i input-role-value-pairs)
  (select-ontology ontology)
  (visualiser:processing ontology task-type input-role-value-pairs)
  (ocml-eval-gen `(achieve-generic-task ',task-type ',input-role-value-pairs)))

|#

(define-condition no-role-value-error
    (condition)
  ((role-name  :initarg :role-name
               :reader no-role-value-error-role-name)
   (method-name :initarg :method-name
                :reader no-role-value-error-method-name))
  (:report (lambda (condition stream)
             (format stream "No role value for ~a when calling ~a"
                     (no-role-value-error-role-name  condition)
                     (no-role-value-error-method-name condition)))))

(defun irs-EXECUTE-method (method-name)
  (remote-irs-procedure-call 
   method-name 
   (mapcar #'(lambda (slot) 
               (list slot (role-value method-name slot)))
           (the-slot-value method-name 'has-input-role))))

(defun psm-soap-method-name (x)
  (ocml-eval-gen `(the-parent ,x)))

(defun psm-soap-function-name (x)
  (intern (concatenate 'string *psm-soap-function-name-start*
                       (symbol-name 
                        (name (home-ontology (find-current-instance x))))
                       "_"
                       (symbol-name (ocml-eval-gen `(the-parent ,x))))
          (find-package *psm-soap-function-name-package*)))

(defun psm-soap-module-name (x)
  (name (home-ontology (find-current-instance x))))

(defun psm-soap-interface-name (x)
  (intern (concatenate 'string 
                       (symbol-name (ocml-eval-gen `(the-parent ,x)))
                       "_OBJECT")
          (find-package "OCML")))

(defun get-soap-method-parameters (input-role-value-pairs)
  ;;(mapcar #'(lambda (x) (symbol-name (second x))) input-role-value-pairs))
  (mapcar #'second input-role-value-pairs))

;;
        ;;(format nil "~S~%" (apply psm-soap-function-name psm-obj
          ;;                        (get-soap-method-parameters input-role-value-pairs)))))))

(defun check-input-role-value-pairs (method-instance 
                                     input-role-value-pairs)
  (mapc #'(lambda (pair)
            (when (ocml:nothing? (second pair))
              (error 'no-role-value-error
                     :role-name (car pair)
                     :method-name method-instance)))
        input-role-value-pairs))

(defun remote-irs-procedure-call (method-instance input-role-value-pairs)
  (setf input-role-value-pairs
        (cl-user::order-input-roles-from-method 
         (name *current-ontology*)
         (ocml-eval-gen `(the-parent ,method-instance))
         input-role-value-pairs))
  ;;there will be occasions when a task can have 
  ;;no value but for the summer school i think the students will
  ;;find it easier if i check it
  (check-input-role-value-pairs method-instance
                                input-role-value-pairs)
  (visualiser:sending-remote-procedure-call-message
   method-instance input-role-value-pairs)
  (let* ((psm-soap-function-name (psm-soap-function-name method-instance))
        (result (apply psm-soap-function-name 
                       (get-soap-method-parameters input-role-value-pairs))))
    (visualiser:result-of-remote-procedure-call
     method-instance result)
    ;;    (format nil "~a~%" result)
    result))
