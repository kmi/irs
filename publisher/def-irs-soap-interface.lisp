(in-package cl-user)

(define-condition no-irs-host-error
    (condition)
  ())

(define-condition no-task-info-error
    (condition)
  ((ontology  :initarg :ontology
               :reader no-task-info-error-ontology)
   (task-name :initarg :task-name
                :reader no-task-info-error-task-name))
  (:report (lambda (condition stream)
             (format stream 
                     "Couldn't get task info for ~a in ontology ~a"
                     (no-task-info-error-task-name  condition)
                     (no-task-info-error-ontology condition)))))

(define-condition no-method-definition-error
    (condition)
  ((ontology  :initarg :ontology
               :reader no-method-definition-error-ontology)
   (method-name :initarg :method-name
                :reader no-method-definition-error-method-name))
  (:report (lambda (condition stream)
             (format stream 
                     "Couldn't get method info for ~a in ontology ~a"
                     (no-task-info-error-method-name  condition)
                     (no-task-info-error-ontology condition)))))



(defun get-task-info-from-irs-server (ontology method-name)
  (if *irs-host*
      (let ((result (irssoap-http-client 'task-info
                                         '((ontology "sexpr") 
                                           (method-name "sexpr")
                                           (host "string")
			                   (port "int"))
			                 (list ontology method-name 
				               (tcp::full-hostname) http::*http-port*)
                                         :host *irs-host*
                                         :port *irs-port*)))
        (when (or (error-line-p result)
                  (null result)
                  (not (listp result)))
          (when (error-line-p result)
              (format t "~a~%" result))
          (error 'no-task-info-error
                 :ontology ontology
                 :task-name method-name))          
        (apply #'values result))
    (error 'no-irs-host-error)))

(defvar *publishing-delay* 1)


(defun clear-all-services ()
  (setf iu::*methods-hash-table* (make-hash-table :test #'equal)))

(defun publish-all-services ()
  (maphash #'(lambda (key value)
               (declare (ignore value))
               (apply #'publish-service key)
               (sleep *publishing-delay*))
           iu::*methods-hash-table*))

(defun publish-service (ontology method-name)
  (let ((result 
         (iu::get-methods-definition ontology method-name)))
    (unless (and (listp result)
                 (= (length result) 4))
      (error 'no-method-definition-error
             :ontology ontology
             :method-name method-name))
    (destructuring-bind (input-roles 
                         output lisp-function publisher-location)
        result
      (when lisp-function
        (send-soap-interface-to-irs-server 
         ontology method-name input-roles 
         output lisp-function publisher-location)))))


(defun send-soap-interface-to-irs-server (ontology method-name input-roles 
                                                    output lisp-function 
                                                    publisher-location)
  (when *irs-host*
    ;;(setf xx (list ontology method-name input-roles output lisp-function publisher-location))
    (irssoap-http-message 'PSM-SOAP-DEFINITION
                          '((ontology "sexpr")(metid "sexpr")
			    (args "sexpr")(result "string")
			    (method "string")(host "string")
			    (port "int") (publisher-location "string"))
			  (list ontology method-name 
                                input-roles output lisp-function
				(tcp::full-hostname) http::*http-port* publisher-location)
                     :host *irs-host*
                     :port *irs-port*)))

(defmacro irs-method-registration (ontology method-name lisp-function 
                                            &optional 
                                            (publisher-location "/soap")
                                            other_roles)
  `(internal-irs-method-registration ',ontology ',method-name ',lisp-function
                                     ,publisher-location ,other_roles))

(defun internal-irs-method-registration (ontology method-name lisp-function 
                                                  &optional 
                                                  (publisher-location "/soap")
                                                  other_roles)
  (let ((method-id nil))
    (multiple-value-bind (input-roles output)
        (get-task-info-from-irs-server ontology method-name)
      (when other_roles
        (setf input-roles (append input-roles other_roles)))
      (setf method-id (iu::make-irs-method-id 
                       method-name ontology
                       "CL-USER"))
      (setf (gethash (List ontology method-id)
                     iu::*methods-hash-table*)
            (list input-roles output lisp-function 
                  publisher-location))
      (create-soap-expressions ontology method-id 
                               input-roles output lisp-function)
      (multiple-value-bind (defsys-file-name system-name)
          (create-defys-file method-id)
        (load defsys-file-name)
        (system::compile-system system-name :load t))
      )))

(defun create-defys-file (method-name &optional (directory *soap-code-directory*))
  (let ((defsys-file-name (merge-pathnames
                           (merge-pathnames "defsys" directory)
                           ".lisp"))
        (system-name (intern (concatenate 'string 
                                          (symbol-name method-name) 
                                          "-SERVER")
                             (find-package "CL-USER"))))
    (when (probe-file defsys-file-name)
      (delete-file defsys-file-name))
    (with-open-file (ostream defsys-file-name :direction :output :if-does-not-exist :create)
      (format ostream "(in-package cl-user)

(defsystem ~(~a~) ()
  :members (
            ~s
            )
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))"
              system-name (string-downcase (symbol-name method-name))
              (string-downcase (symbol-name method-name))))
    (values defsys-file-name system-name)))

(defun make-soap-expressions-file-name (x &optional (directory *soap-code-directory*))
  (merge-pathnames
   (merge-pathnames (string-downcase (symbol-name x)) directory)
   ".lisp"))


(defun create-soap-expressions (ontology method-name input-roles 
					 output lisp-function 
					 &optional (directory *soap-code-directory*))
  (let ((soap-expressions-file-name 
         (make-soap-expressions-file-name method-name directory)))
    (when (probe-file soap-expressions-file-name)
      (delete-file soap-expressions-file-name))
    (with-open-file (ostream soap-expressions-file-name :direction :output :if-does-not-exist :create)
      (let ((arguments (mapcar #'car input-roles)))
	(format ostream
                ;;changing ~s to ~a in response
                ;;having the package name generates 
                ;;a soap parse error
              "(defmethod handle-soap-service ((action (eql '~(~a~)))
                                                html-stream args)
                  (let* ((*package* (find-package \"OCML\"))
                         ~{ ~(~a~)~} result)
                     (setf ~{ ~(~a~) (pop args)~%~})
                     (setf result (~a ~{ ~(~a~)~}))
                     (iu::send-soap-response \"~(~a~)\" (list result) '((result ~s)) :stream html-stream)))"
              ;;(princ (~a ~{ ~(~a~)~}) html-stream)))"
		  method-name
		  arguments
		  arguments
		  lisp-function
		  arguments
		  method-name
		  output)))))
