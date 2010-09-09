(in-package cl-user)

;;;;;CLIENT SUPPORT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *irs-host* ;;wenjin's "137.108.25.95")
  "localhost" ;;"127.0.0.1" ;;this is the alice demo machine
  )

(defvar *irs-port* 3000)

(defvar *host-ip-address* "127.0.0.1")

(defvar *soap-code-directory* 
  (translate-logical-pathname "irs:src;irs-publisher-generated-code;"))

(defvar *http-ok* "HTTP/1.0 200 OK~%ContentType: text/xml; charset=\"utf-8\"~%")

;; this substitutes the method in http/.../methods.lisp
(defmethod http::http-reply ((method (eql :post)) request)
  (handler-case
      (progn
        (when (string= request "/")
          (setq request "/top"))
        (cond ((string= request "/soap")
               (let* ((soaprequest 
                       (http::http-info-fields http::*http-info*))
                      (*package* (find-package "USER"))
	              soapmsg action)
                 (setf soapmsg (api-soap-old:get-msg
                                (api-soap-old:soap-xml-parse soaprequest)))
	         (setf action (lisp-read-string 
                               (api-soap-old:xml-name soapmsg)))
                 (handle-post-soap-service 
                  action http::*http-stream* 
                  (api-soap-old:get-soapvalues soapmsg))))
              ((get-function-as-web-service request)
               (let* ((soaprequest 
                       (http::http-info-fields http::*http-info*))
                      (*package* (find-package "USER"))
	              (soapmsg (api-soap-old:get-msg 
                                (api-soap-old:soap-xml-parse soaprequest))))
                 (handle-function-as-web-service 
                  request 
                  (lisp-read-string (api-soap-old:xml-name soapmsg))
                  (api-soap-old:get-soapvalues 
                   (api-soap-old:get-msg (api-soap-old:soap-xml-parse soaprequest)))
                  http::*http-stream*)))
              (t (http::call-handler 
                  http::*lispweb-type-table* request))))
    (serious-condition 
        (c)
      (iu::send-soap-error-message (http::send-error-message nil c)
                                   'serious-condition
                                   http::*http-stream*))
    (error
        (c)
      (iu::send-soap-error-message (http::send-error-message nil c) 
                                   'error http::*http-stream*))))

(defmethod handle-post-soap-service (action stream soap-values)
  ;;(push (list action soap-values) bb)

  ;;;change this to just invoke the lisp function now - no more mapping from webservice name
  ;;(handle-soap-service
  ;;action stream soap-values))
  (let ((output-type (car soap-values)))
    (if (fboundp action)
        (let ((result (apply action (cdr soap-values))))
          (iu::send-soap-response (format nil "~(~a~)" action)
                                  (list result) `((result ,output-type)) :stream stream))
      (iu::send-soap-response (format nil "~(~a~)" action)
                              (list (format nil "~(~a~)_is_not_a_defined_lisp_function" action))
                              `((result ,output-type)) :stream stream))))

(defmethod handle-post-soap-service ((action (eql 'user::publish-lisp-function))
                                     stream soap-values)
  ;;(setf abc soap-values)
  (let ((ontology (pop soap-values))
        (method-name (pop soap-values))
        (lisp-function (pop soap-values)))
    (cond ((fboundp (read-from-string (string-upcase lisp-function)))
           (internal-irs-method-registration ontology method-name lisp-function)
           (publish-service ontology (iu::make-irs-method-id 
                                      method-name ontology
                                      "CL-USER"))
           (iu::send-soap-response "PUBLISH-LISP-FUNCTION-RESPONSE" 
                                   (list 'OK) '((result "sexpr")) :stream stream))
          (t (iu::send-soap-response "PUBLISH-LISP-FUNCTION-RESPONSE" 
                                     (list (format nil "The function ~a is not defined." 
                                                   lisp-function)) 
                                     '((result "string")) :stream stream)))))

(defmethod handle-post-soap-service ((action (eql 'user::publish-wsmo-lisp-function))
                                         stream soap-values)
  (let ((*package* (find-package "CL-USER")))
    (destructuring-bind (user ontology web-service-name 
                              web-service-host web-service-port 
                              web-service-location 
                              message-exchange-pattern lisp-function)
        soap-values
      (declare (ignore web-service-host web-service-port web-service-location
                       message-exchange-pattern ))
      (cond ((fboundp (read-from-string (string-upcase lisp-function)))
             (internal-irs-wsmo-web-service-registration ontology web-service-name lisp-function)
             (publish-service ontology (iu::make-irs-method-id 
                                        web-service-name ontology
                                        "CL-USER") t user)
             (iu::send-soap-response "PUBLISH-WSMO-LISP-FUNCTION-RESPONSE" 
                                     (list "OK") '((result "string")) :stream stream))
            (t (iu::send-soap-response "PUBLISH-WSMO-LISP-FUNCTION-RESPONSE" 
                                       (list (format nil "The function ~a is not defined." 
                                                     lisp-function)) 
                                       '((result "string")) :stream stream))))))

(defmethod handle-soap-service (action html-stream request)
  (format html-stream "Unknown method ~a~%" action))

(defun lisp-read-string (string)
  (with-input-from-string (str string)
			  (read str)))

#|
(defun irssoap-http-client (proc types args &key host (port 80) (stream t))
  "Send a direct irssoap request to the specified HTTP server. 
   It still gets a normal response. I leave it because it is easy to 
   integrate to test the irs-protocol.
   See below the irssoap-http-client-soap-response version. Keywords:
host - the HTTP server
port - the HTTP port (default is 80)
request - the soap request string"

  (let ((stm (comm::open-tcp-stream host port))
	(headlen (length iu:*soap-header*))
	(endlen (length iu:*soap-end*))
	(actionlen (length proc))
	(argsstring (gen-soap-args types args))
        (value nil))
    ;;(setf aa argsstring)
    (format stm "IRSSOAP ")
    (format stm "~a" iu:*soap-header*)
    (format stm "<~a>" proc)
    (format stm "~a" argsstring)
    (format stm "</~a>" proc)
    (format stm "~a" iu:*soap-end*)
    (format stm " HTTP/1.0~%~%")
    (force-output stm)
    (handler-case
		;;(loop
        (let ((line (read-line stm)))
          (if (error-line-p line)
              (setf value line)
            (setf value (read-from-string line)))
          (format stream "~a~%" value)
          )
      (end-of-file (c)
	(declare (ignore c))
	(print "Connection closed.")))
    value))
|#

(defun irssoap-http-client (proc types args &key host (port 80) (stream t))
  "Send a direct irssoap request to the specified HTTP server. 
   It still gets a normal response. I leave it because it is easy to 
   integrate to test the irs-protocol.
   See below the irssoap-http-client-soap-response version. Keywords:
host - the HTTP server
port - the HTTP port (default is 80)
request - the soap request string"

  (let ((stm (comm::open-tcp-stream host port))
	(headlen (length iu:*soap-header*))
	(endlen (length iu:*soap-end*))
	(actionlen (length proc))
	(argsstring (gen-soap-args types args))
        (value nil))
    ;;(setf aa argsstring)
    (format stm "IRSSOAP ")
    (format stm "~a" iu:*soap-header*)
    (format stm "<~a>" proc)
    (format stm "~a" argsstring)
    (format stm "</~a>" proc)
    (format stm "~a" iu:*soap-end*)
    (format stm " HTTP/1.0~%~%")
    (force-output stm)
    (handler-case
        ;;;changed by john d 4/8/03 to cope with multiple line
        ;;;return values
        (do ((line (read-line stm nil nil nil)
                   (read-line stm nil nil nil))
             (lines ""))
            ((or (error-line-p line)
                 (null line))
             (if (error-line-p line)
                 (setf value line)
               (setf value (read-from-string lines)))
             ;;(format stream "~a~%" value)
             )
          (setf lines 
                (concatenate 'string lines (string #\linefeed) line))
          )
      (end-of-file (c)
	           (declare (ignore c))
	           (print "Connection closed.")))
    value))

(defvar *error-line-start*
  "error:")

(defun error-line-p (x)
  (and (stringp x) (>= (length x) (length *error-line-start*))
       (string= (subseq (string-downcase x) 0 (length *error-line-start*))
                *error-line-start*)))

(defun irssoap-http-message (proc types args &key host (port 80) (stream t))
  "Send a direct irssoap message to the specified HTTP server. 
   It does not wait for an answer Keywords:
host - the HTTP server
port - the HTTP port (default is 80)
request - the soap request string"

  (let ((stm (comm::open-tcp-stream host port))
	(headlen (length iu:*soap-header*))
	(endlen (length iu:*soap-end*))
	(argsstring (gen-soap-args types args))
        (value nil))
    (format stm "IRSSOAP ")
    (format stm "~a" iu:*soap-header*)
    (format stm "<~a>" proc)
    (format stm "~a" argsstring)
    (format stm "</~a>" proc)
    (format stm "~a" iu:*soap-end*)
    (format stm " HTTP/1.0~%~%")
    (force-output stm)))

;; Note that the sexprs must maintain the original structure 
;; (they are printed with -s because the lisp read function 
;; must be able to read them
(defun gen-soap-args (types values)
  (let ((res ""))
    (dolist (arg types)
      (let ((name (first arg))
	    (type (second arg)))
	(if (string= type "sexpr")
	    (setq res (concatenate 'string res (format nil "<~a type=\"~a\">~s</~a>" name type (pop values) name)))
	  (setq res (concatenate 'string res (format nil "<~a type=\"~a\">~a</~a>" name type (pop values) name))))))
    res))

(defun irssoap-http-client-soap-response (proc types args &key host (port 80) (stream t))
  "Send a direct irssoap request to the specified HTTP server. Keywords:
host - the HTTP server
port - the HTTP port (default is 80)
request - the soap request string"

  (let ((stm (comm::open-tcp-stream host port))
	(headlen (length iu:*soap-header*))
	(endlen (length iu:*soap-end*))
	(actionlen (length proc))
	(argsstring (gen-soap-args types args))
        (values nil))
    (format stm "IRSSOAP ")
    (format stm "~a" iu:*soap-header*)
    (format stm "<~a>" proc)
    (format stm "~a" argsstring)
    (format stm "</~a>" proc)
    (format stm "~a" iu:*soap-end*)
    (format stm " HTTP/1.0~%~%")
    (force-output stm)
    (handler-case
	(setf values (multiple-value-list (parse-soaphttp-response proc stm)))
      (end-of-file (c)
	(declare (ignore c))
	(print "Connection closed.")))
    (apply #'values values)))

(defun achieve-task-through-irs (ontology task-type &rest input-role-value-pairs)
  (irs-http-client :host *irs-host*
                   :port *irs-port* 
                   :request (format nil "irs achieve-task ~a ~a (~{(~{~a ~s~}) ~})" 
                                    ontology task-type input-role-value-pairs)))


(defun irs-http-client (&key host (port 80) request (stream t))
  "Send a request to the specified HTTP server. Keywords:
host - the HTTP server
port - the HTTP port (default is 80)
request - the request string"

  (let ((stm (iu::open-irs-connection host port)) 
        (value nil))
    (format stm "~a HTTP/1.0~%~%" request)
    (force-output stm)
    (handler-case
	;;(loop
        (progn (setf value (read-from-string (read-line stm)))
	       (format stream "~a~%" value))
      (end-of-file (c)
	(declare (ignore c))
	(print "Connection closed.")))
    value))
