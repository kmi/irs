(in-package irs-protocol)

(defun cl-user::irs-achieve-task (ontology task-type input-role-value-pairs 
                                  html-stream
                                  &optional soap-p)
  (irs-achieve-task ontology task-type input-role-value-pairs 
                                  html-stream
                                  soap-p))

(defun irs-achieve-task (ontology task-type input-role-value-pairs 
                                  html-stream
                                  &optional soap-p)
  (handler-case 
      (progn
	#+:irs-lispworks
        (visualiser:received-achieve-task-message
         ontology task-type input-role-value-pairs)
        (let ((ocml::*in-irs* t))
          (declare (special ocml::*in-irs*))
          (let ((result (ocml::irs-achieve-task-internal ontology task-type
                                                         input-role-value-pairs)))
	    #+:irs-lispworks
            (visualiser:sending-achieve-task-message
             ontology task-type input-role-value-pairs result)
            (if soap-p
                (destructuring-bind (input-roles output function) 
                    (get-task-task-info task-type)
                  (declare (ignore input-roles function))
                  (if (soap-attachment-type-p output)
                      (send-soap-attachment 
                       (symbol-name task-type)
                       result `((result ,output)) html-stream)
                  (iu::send-soap-response2 (symbol-name task-type)
                                           (list result) 
                                           `((result ,output)) 
                                           :stream html-stream)))
              (format html-stream "~s" result)))
          (when (eq task-type 'ocml::shipping-patient-task)
            (sleep 1)
            (go-to-idle))))
    (ocml::no-role-value-error
     (c)
     (send-achieve-task-error-message c task-type html-stream))
    (serious-condition 
     (c)
     (send-achieve-task-error-message c task-type html-stream))
    (error 
     (c)
     (send-achieve-task-error-message c task-type html-stream))))

(defun send-achieve-task-error-message (condition task-type stream)
  (iu::send-soap-error-message condition task-type stream))

(defun task-methods (ontology task html-stream)
  (ocml::select-ontology (ocml::get-ontology ontology))
  (let ((task-psms (ocml::task-children task)))
    (format html-stream "~{~{~(~a~) ~{~(~a~) ~a~}%~}~}~%" 
            (find-associated-corba-methods ontology
                                           (mapcar #'ocml::name task-psms)))))

(defun find-associated-corba-methods (ontology psm-names)
  (let ((corba-methods (ocml::corba-methods ontology)))
    (mapcan #'(lambda (psm-name)
                (let ((associated-corba-methods (assoc psm-name corba-methods)))
                  (when associated-corba-methods
                    (list associated-corba-methods))))
            psm-names)))

(defvar *psm-post-urls* (make-hash-table :test #'equal))

;;method-name already includes the ontology name
(defun make-psm-post-url-pathname (method-name)
  (concatenate 'string "/" (string-downcase (symbol-name method-name))))

(defun make-psm-function-name (method-name)
  (intern (concatenate 'string 
                       ocml::*psm-soap-function-name-start*
                       (symbol-name method-name))
          (find-package ocml::*psm-soap-function-name-package*)))

(defun create-post-url-for-psm-soap-definition (method-name output)
  (let ((url-pathname (make-psm-post-url-pathname method-name))
        (psm-function (make-psm-function-name method-name)))
    (add-publisher-location method-name url-pathname)
    (add-url-pathname url-pathname psm-function method-name output)))

(defun add-url-pathname (url-pathname function-name method-name output)
  (setf (gethash url-pathname *psm-post-urls*)
        (list function-name method-name output)))

(defvar *publisher-locations* (make-hash-table))

(defvar *default-publisher-location* "/soap")


(defvar *publisher-hosts-and-ports* (make-hash-table))

(defun add-publisher-location (method-name publisher-location
                                           &optional publisher-host
                                           publish-port)
  (unless (string= publisher-location *default-publisher-location*)
    (setf (gethash method-name *publisher-locations*)
          publisher-location))
  (when publisher-host ;;port may be nil(and publisher-host publish-port)
    (setf (gethash method-name *publisher-hosts-and-ports*)
          (list publisher-host publish-port))))

(defun get-publisher-host-and-port (method-name)
  (apply #'values 
         (gethash method-name *publisher-hosts-and-ports*)))

(defun get-publisher-location (method-name)
  (gethash method-name *publisher-locations*))

(defun cl-user::get-psm-publisher-location (method-name ontology)
  (get-publisher-location 
   (iu::make-irs-method-id method-name ontology)))

(defun cl-user::get-psm-full-publisher-location (method-name ontology)
  (let ((publiser-pathname 
         (get-publisher-location 
          (iu::make-irs-method-id method-name ontology))))
    (multiple-value-bind (publisher-host publisher-port) 
        (get-publisher-host-and-port 
         (intern (concatenate 'string (symbol-name ontology) "_"
                              (symbol-name method-name))
                 (find-package "OCML")))
      (when publisher-host
        (format nil
                "http://~a~:[~*~;:~d~]~:[~*~;~a~]"
                publisher-host publisher-host publisher-port publisher-port 
                publiser-pathname publiser-pathname)))))

(defun get-psm-post-url-pathname (url-pathname)
  ;;(setf abc url-pathname)
  (gethash url-pathname *psm-post-urls*))

(defvar *psm-soap-definition-values* nil)

(defvar *psm-soap-definition-value-store-file-name*
  "psm-soap-definition-values.lisp")

(defun psm-soap-definition-value-store-file ()
  (merge-pathnames *psm-soap-definition-value-store-file-name*
                   ocml::*irs-server-generated-code-directory*))

(defun store-psm-soap-definition (ontology method-name
                                           input-roles output 
                                           request-name publisher-ip-address
                                           publisher-port
                                           publisher-location)
  (let ((values (list ontology method-name
                            input-roles output 
                            request-name publisher-ip-address
                            publisher-port
                            publisher-location)))
    (unless (find values
                  *psm-soap-definition-values* :test #'equal)
      (setf *psm-soap-definition-values*
            (append *psm-soap-definition-values*
                    (list values)))))
  (write-psm-soap-definitions))

(defun write-psm-soap-definitions ()
  (let ((file (psm-soap-definition-value-store-file)))
    (when (probe-file file)
      (delete-file file))
    (with-open-file (ostream file :direction :output
                             :if-does-not-exist :create)
      (format ostream "~s" *psm-soap-definition-values*))))

(defun read-psm-soap-definitions ()
  (let ((file (psm-soap-definition-value-store-file)))
    (when (probe-file file)
      (let ((*package* (find-package "OCML")))
        (with-open-file (istream file :direction :input)
          (setf *psm-soap-definition-values* (read istream nil nil)))))))

(defun republish-defined-services ()
  (read-psm-soap-definitions)
  (mapc #'(lambda (psm-soap-definition)
            (apply #'internal-psm-soap-definition psm-soap-definition))
        *psm-soap-definition-values*))

(defvar *request-store*
  (make-hash-table))

(defun store-request-name (method-name request-name)
  (setf (gethash method-name *request-store*) request-name))

(defun get-request-name (method-name)
  (gethash method-name *request-store*))

(defun internal-psm-soap-definition (ontology method-name
                                              input-roles output 
                                              request-name
                                              publisher-ip-address
                                              publisher-port
                                              publisher-location)
  (store-psm-soap-definition ontology method-name
                                              input-roles output 
                                              nil publisher-ip-address
                                              publisher-port
                                              publisher-location)
  (multiple-value-bind (defsys-filename system-name)
      (ocml::create-irs-server-defys-file method-name)
;;    (format http::*http-stream* "HERE2:~a~%"  ocml::*irs-server-generated-code-directory*)
    (add-method-to-ontology ontology method-name input-roles)
    (store-request-name method-name request-name)
    ;;(careful-add-file defsys-filename)
    ;;(careful-add-file system-name)
    (create-post-url-for-psm-soap-definition method-name output)
    ;;make sure we add publisher-location after 
    ;;create-post-url-for-psm-soap-definition because this will add
    ;;the automatically generated one.
    (when publisher-location
      (add-publisher-location method-name publisher-location
                              publisher-ip-address publisher-port))
    (ocml::create-irs-server-soap-expressions
     publisher-ip-address
     publisher-port
     ontology method-name input-roles
     output
     (string-downcase (symbol-name method-name))
     ocml::*irs-server-generated-code-directory* publisher-location)
    (load defsys-filename)
    #+:irs-lispworks
    (system::compile-system system-name :load t)
    #+:irs-lispworks
    (visualiser:refresh)))

(defun get-task-info (ontology method-name html-stream)
  (multiple-value-bind (input-roles output)
      (internal-get-task-info ontology method-name)
      (if (eq input-roles 'no-task)
          (format html-stream "error: no-task for ~a~%" method-name)
        (format html-stream "(~s ~s)~%" input-roles output))))

(defvar *ocml-toplevel-task-name* 'ocml::goal-specification-task)

(defun get-task-task-info (task-name)
  (let ((all-input-roles nil)
        (final-output-role nil)
        (final-lisp-function nil)
        (parents 
         ;;;note that it is important that ocml::all-superclasses returns
         ;;;items according the class precedence list
         (ocml::setofall '?x 
                         `(and (ocml::member ?x (ocml::all-superclasses ,task-name)) 
                               (ocml::subclass-of ?x ,*ocml-toplevel-task-name*)))))
    (mapc 
     #'(lambda (parent-task-name)
         (destructuring-bind (input-roles 
                              output lisp-function)
             (iu::get-task-definition 
              ;;task may be in an ontology used by the method's ontology
              (ocml::name (ocml::home-ontology (ocml::get-domain-class parent-task-name)))
              parent-task-name)
           (setf all-input-roles (append input-roles all-input-roles))
           (unless final-output-role
             (setf final-output-role output))
           (unless final-lisp-function
             (setf final-lisp-function lisp-function))))
     parents)
    (destructuring-bind (input-roles 
                         output lisp-function)
        (iu::get-task-definition 
         ;;task may be in an ontology used by the method's ontology
         (ocml::name (ocml::home-ontology (ocml::get-domain-class task-name)))
         task-name)
      (list (remove-duplicates (append all-input-roles input-roles)
                               :key #'car)
            (or output final-output-role)
            (or lisp-function final-lisp-function)))))

(defun get-task-non-local-input-roles (ontology task-name)
  (ocml::select-ontology ontology)
  (car (get-task-task-info task-name)))

(defun get-task-non-local-output-role (ontology task-name)
  (ocml::select-ontology ontology)
  (second (get-task-task-info task-name)))

(defun get-task-for-method (ontology method-name)
  (ocml::select-ontology ontology)
  (ocml::findany '?x 
                 `(ocml::tackles-task-type ,method-name ?x)))


(defun internal-get-task-info (ontology method-name)
  ;;;(setf o ontology m method-name)
  (ocml::select-ontology ontology)
  (let ((task-name 
         (ocml::findany '?x 
                        `(ocml::tackles-task-type ,method-name ?x))))
    (if (ocml:nothing? task-name)
        'no-task
      (destructuring-bind (input-roles 
                           output lisp-function)
          (get-task-task-info task-name)
        (declare (ignore lisp-function))
        (values input-roles output)))))

(defun add-method-to-ontology (ontology method-name input-roles)
  (let ((ontology-structure (ocml::get-ontology ontology)))
    (if ontology-structure
        (setf (ocml::corba-methods ontology-structure)
              (cons (cons method-name input-roles)
                    (ocml::corba-methods ontology-structure)))
      (warn "Warning ontology ~a not found." ontology))))

;;;now defunct no longer use corba
#|
(defun ontology-methods (ontology)
  (ocml::corba-methods (ocml::get-ontology ontology)))
|#

(defun ontology-items
       (ontology 
        view-only-items-in-current-ontology
        item-types)
  (ocml::select-ontology ontology)
  (let ((classes (when (find 'ocml::classes item-types)
                   (list-ontology-classes ontology 
                                          view-only-items-in-current-ontology)))
        (tasks (when (find 'ocml::tasks item-types)
                 (list-ontology-tasks ontology
                                      view-only-items-in-current-ontology)))
        (methods (when (find 'ocml::methods item-types)
                   (list-ontology-methods ontology
                                          view-only-items-in-current-ontology))))
    (list classes tasks methods)))

(defun list-ontology-classes (ontology view-only-items-in-current-ontology)
  (sort (remove 'ocml::goal-specification-task
                (ocml::list-classes view-only-items-in-current-ontology
                            nil 0 (ocml::get-ontology ontology) 
                            nil t nil 
                            nil 'ocml::task
                            nil))
        #'string<))

(defun list-ontology-methods (ontology view-only-items-in-current-ontology)
  (sort ;;(remove 'ocml::primitive-method
        (remove-top-level-tasks-and-psms
                (ocml::list-classes view-only-items-in-current-ontology
                                    nil 0 (ocml::get-ontology ontology) 
                                    nil t nil ;;'ocml::problem-solving-method
                                    'ocml::primitive-method))
        #'string<))

(defun ontology-methods (ontology 
                         &optional
                         (view-only-items-in-current-ontology t))
  (ocml::select-ontology ontology)
  (list-ontology-methods ontology view-only-items-in-current-ontology))

(defvar *toplevel-task-psm-ontologies*
  '(ocml::base-ontology ocml::akt-task-and-psm-ontology))

(defun toplevel-task-psm-ontology-p (ontology-name)
  (find ontology-name *toplevel-task-psm-ontologies*))

(defun remove-top-level-tasks-and-psms (list)
  (mapcan #'(lambda (class-name)
              (let ((class (ocml::get-domain-class class-name)))
                (when class
                  (unless (toplevel-task-psm-ontology-p
                           (ocml::name (ocml::home-ontology class)))
                    (list class-name)))))
          list))
    
(defun list-ontology-tasks (ontology view-only-items-in-current-ontology)
  (sort ;;(remove 'ocml::goal-specification-task
        (remove-top-level-tasks-and-psms
         (ocml::list-classes view-only-items-in-current-ontology
                             nil 0 (ocml::get-ontology ontology) 
                             nil t nil ;;;'ocml::task 
                             'ocml::goal-specification-task
                             ;;'ocml::problem-solving-method
                             nil))
        #'string<))

(defun ontology-tasks (ontology 
                       &optional 
                       (view-only-items-in-current-ontology t))
  (ocml::select-ontology ontology)
  (list-ontology-tasks ontology view-only-items-in-current-ontology))


(defun list-ontology-goals (ontology view-only-items-in-current-ontology)
  (sort 
   (ocml::list-classes view-only-items-in-current-ontology
                       nil 0 (ocml::get-ontology ontology) 
                       nil t nil 
                       'ocml::goal)
   #'string<))

(defun ontology-goals (ontology 
                       &optional 
                       (view-only-items-in-current-ontology t))
  (ocml::select-ontology ontology)
  (list-ontology-goals ontology view-only-items-in-current-ontology))

(defun list-ontology-web-services (ontology view-only-items-in-current-ontology)
  (sort 
   (ocml::list-classes view-only-items-in-current-ontology
                       nil 0 (ocml::get-ontology ontology) 
                       nil t nil 
                       'ocml::web-service)
   #'string<))

(defun ontology-web-services (ontology 
                       &optional 
                       (view-only-items-in-current-ontology t))
  (ocml::select-ontology ontology)
  (list-ontology-web-services ontology view-only-items-in-current-ontology))

(defun list-ontology-mediators (ontology view-only-items-in-current-ontology)
  (sort 
   (ocml::list-classes view-only-items-in-current-ontology
                       nil 0 (ocml::get-ontology ontology) 
                       nil t nil 
                       'ocml::mediator)
   #'string<))

(defun ontology-mediators (ontology 
                           &optional 
                       (view-only-items-in-current-ontology t))
  (ocml::select-ontology ontology)
  (list-ontology-mediators ontology view-only-items-in-current-ontology))
    
(defun invoke-remote-irs-method (ontology method-name arguments)
  (declare (ignore ontology))
  (apply (intern (concatenate 'string "RUN-"
                              (symbol-name method-name))
                 (find-package "CL-USER"))
           arguments))

(defvar *html-file-header* "<html><body>")

(defvar *html-file-footer* "</body></html>")

(defun method-details (ontology method html-stream &optional  (lookup-function
                                                               #'ocml::irs-lookup-current-word))
  (task-details ontology method html-stream lookup-function))

(defun task-details (ontology task html-stream &optional (lookup-function
                                                          #'ocml::irs-lookup-current-word))
  (ocml::select-ontology ontology)
  (let ((string (with-output-to-string (html-stream)
                  (format html-stream *html-file-header*)
                  (ocml::irs-html-describe-class html-stream task nil lookup-function)
                  (format html-stream *html-file-footer*))))
    (format html-stream "~a~%"
            (remove #\linefeed (remove #\return string)))))

(defun irssoap-http-message (proc types args &key host (port 80) (stream t))
  "Send a direct irssoap message to the specified HTTP server. 
   It does not wait for an answer Keywords:
host - the HTTP server
port - the HTTP port (default is 80)
request - the soap request string"
  (declare (ignore stream))
  (let ((stm (iu::open-irs-connection host port))
	;;(headlen (length *soap-header*))
	;;(endlen (length *soap-end*))
	;;(actionlen (length proc))
	(argsstring (iu::gen-soap-args types args))
        ;;(value nil)
        )
    (format stm "IRSSOAP ")
    (format stm "~a" *soap-header*)
    (format stm "<~a>" proc)
    (format stm "~a" argsstring)
    (format stm "</~a>" proc)
    (format stm "~a" *soap-end*)
    (format stm " HTTP/1.0~%~%")
    (force-output stm)))

;;; XXX need to do this properly
(defun get-namespace (soaprequest)
  (declare (ignore soaprequest))
  ;;"xsd"
  "")

(defmethod http::http-reply ((method (eql :post)) request)
  (handler-case 
      (progn
        (when (string= request "/")
          (setq request "/top"))
        (cond ((string= request "/soap")
               (let* ((soaprequest
                       (http::http-info-fields http::*http-info*))
                      (*package* (find-package "USER"))
	              soapmsg action soap-values)
	         (setf soapmsg 
                       (api-soap-old:get-msg
                        (api-soap-old:soap-xml-parse soaprequest)))
	         (setf action 
                 (lisp-read-string 
                  (api-soap-old:xml-name soapmsg))
                 soap-values
                 (api-soap-old:get-soapvalues soapmsg))
                 (http::http-log-message 
                 "~%post-irs-command: ~a ~{~a ~}" 
                 action soap-values)
                 (handle-post-soap-service 
                 action http::*http-stream* 
                 (get-namespace soaprequest)
                 soap-values)))
              ((get-psm-post-url-pathname request)
               (let* ((soaprequest 
                       (http::http-info-fields http::*http-info*))
                      (*package* (find-package "USER"))
	              soapmsg action soap-values)
                 (setf soapmsg (api-soap-old:get-msg 
                                (api-soap-old:soap-xml-parse soaprequest))
                       action
                       (lisp-read-string (api-soap-old:xml-name soapmsg))
                       soap-values
                       (api-soap-old:get-soapvalues 
                        (api-soap-old:get-msg (api-soap-old:soap-xml-parse soaprequest))))
                 (http::http-log-message
                  "~%post-url-pathname: ~a ~{~a ~}"
                  action soap-values)
                 (handle-psm-post-url 
                  request 
                  action soap-values
                  http::*http-stream*)))
              (t (http::call-handler 
                  http::*lispweb-type-table* request))))
    (ocml::no-role-value-error
       (c)
       (iu::send-soap-error-message c 'no-role-value
                                http::*http-stream*))
    (error
     (c)
     (iu::send-soap-error-message c 'error
                                http::*http-stream*))
    (serious-condition 
     (c)
     (iu::send-soap-error-message c 'serious-condition
                                http::*http-stream*))))

(defun make-input-role-value-pairs-from-types-and-values (values types)
  (if (atom values)
      (list (list values (car types)))
    (mapcar #'(lambda (value type) (list value type))
            values types)))

(defmethod handle-psm-post-url (request action soap-values
                                        stream)
  ;;(setf ss soap-values)
  (let (result)
    (destructuring-bind (function-name method-name result-type)
        (get-psm-post-url-pathname request)
      (declare (ignore method-name))
      (setf result (apply function-name soap-values))
      ;;(push result rr) (push method-name mm)
      ;;(push result-type ty) (push function-name ff)
      (iu::send-soap-response 
       (string-downcase (symbol-name action))
       (list result) `((result ,result-type)) :stream stream))))


(defun send-soap-error (response-name error-type stream)
  (iu::send-soap-response2 
   response-name
   (list error-type)
   `((error "sexpr"))
   :stream stream))

(defun make-input-role-value-pairs (input-soap-types list)
  ;;(setf ii input-soap-types ll list)
  (let ((result nil)
        (*package* (find-package "OCML")))
    (dotimes (i (length list))
      (when (evenp i)
        (push (list (make-ocml-symbol (elt list i)) 
                    (if (find (elt input-soap-types (floor (/ i 2)))
                              '(ocml::sexpr float integer))
                        (make-ocml-symbol (read-from-string (string-upcase (elt list (1+ i)))))
                      (make-ocml-symbol (elt list (1+ i)))))
              result)))
    result))

(defun make-input-role-value-pairs2 (input-role-names-and-soap-types list)
  ;;(setf ii input-soap-types ll list)
  (let ((result nil)
        (*package* (find-package "OCML")))
    (dotimes (i (length list))
      (let ((role-name (make-ocml-symbol (elt list i))))
        (when (evenp i)
          (push (list role-name
                      (if (find (second (assoc role-name input-role-names-and-soap-types))
                                '(ocml::sexpr float integer))
                          (make-ocml-symbol (read-from-string (string-upcase (elt list (1+ i)))))
                        (make-ocml-symbol (elt list (1+ i)))))
                result))))
    result))





(defmethod cl-user::handle-soap-service (action html-stream request)
  (iu::send-soap-error-message
   'error-unknown-action 'soap-service-response html-stream))

; This is an alternative to check a soap msg
; but i am still not able to make it work preoperly Mauro
;(soap-type (http::http-info-headers http::*http-info*))
;(defun soap-type (headers) ;; identifies if it is not soap Mauro
;  (let ((value))
;    (mapc #'(lambda (header)
;              (when (eq (http::header-field-name header) :soapaction)
;                (setf value t)))
;          headers)
;    value))



       
    
