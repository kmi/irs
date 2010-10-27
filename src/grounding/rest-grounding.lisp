;;; Copyright © 2008 The Open University.

(in-package :irs.grounding.rest)

(defmethod wsmo-protocol:invoke-web-service-operation
    ((grounding-type (eql 'ocml::grounded-to-rest)) grounding
     operation proc output-type input-roles-and-soap-bindings values
     host port location-ignored service-instance)
  (declare (ignore location-ignored))
  (let* ((service (ocml::name (ocml::parent-class service-instance)))
         (invocation (ocml::name service-instance))
         (earthing (web-onto::findany
                    'ocml::?m `(ocml::associated-earthing ,service ocml::?m)))
         (connection-read-timeout
          (let ((val (ocml::get-role-value earthing 'ocml::has-connection-read-timeout)))
            (if (ocml:nothing? val)
                irs:*connection-read-timeout*
                val)))
         (ocml-request (lower-invocation-to-http service invocation)))
    (let ((method (method-of ocml-request))
          (url (url-of ocml-request)))
      (irs.api.javascript:event
       :rpc-call service (format nil "~A ~A" method url)))
    (let ((ocml-response (invoke-ocml-request ocml-request
                                              :read-timeout connection-read-timeout)))
      (lift-http-to-invocation service ocml-response invocation)
      (let ((output-role (output-role invocation)))
        (if (or (ocml:nothing? output-role) (null output-role))
            nil
            (ocml::findany '?value
                           `(and  (ocml::suitable-web-service ?goal ,invocation)
                                  (,output-role ?goal ?value))))))))

(defun output-role (invocation)
  ;; In this case, we are interested only in direct has-output-role's
  ;; on the service, not the goal.  Because this lifting and lowering
  ;; is clean and consistent, the lifting MUST be to the /service/'s
  ;; output, not the goal's.  We return only the first, hoping that
  ;; this is most useful.  We really should have goals return multiple
  ;; values.
  (first (ocml::findall '?x
                        `(and (ocml::suitable-web-service ?goal ,invocation)
                              (ocml::has-output-role ?goal ?x)))))

(defmethod wsmo-protocol::make-invocation-name-for-web-service-operation2
    ((type (eql 'ocml::grounded-to-rest)) implementation-info)
  implementation-info)

(defun lower-invocation-to-http (service-type invocation)
  "Create an OCML http-request object for INVOCATION, returning its name."
  (let ((res (web-onto::findany
              '?httpmsg
              `(and
                (= ?httpmsg (#_rfc2616:new-instance #_rfc2616:http-request))
                (#_grnd:lower ,service-type ,invocation ?httpmsg)))))
    (if res
	res
	(error (make-condition 'irs.grounding::<grounding-fault>
			       :other-cause "Lowering rule failed")))))

(defun lift-http-to-invocation (service-type http-response invocation)
  "Read the OCML http-response object, invoking the appropriate ‘lift’
rule."
  (web-onto::findany
   '?http-response `(and (= ?http-response ,http-response)
                         (#_grnd:lift ,service-type ?http-response ,invocation))))

;; I want some abstraction here, because at some point OCML names will
;; cease being symbols and become a class instance or something.
(defun ocml-name? (thing)
  "Check THING is an OCML name."
  (symbolp thing))

(defun headers-of (request)
  "Return a list of header-name/header-value pairs from OCML name REQUEST."
  (let* ((lists (webonto::findall '(?field ?value)
                                 `(and (#_rfc2616:has-header ,request ?header)
                                       (#_rfc2616:field-name ?header ?field)
                                       (#_rfc2616:field-value ?header ?value))))
        (cleaned (remove-if (lambda (header)
                              (string= "Content-Type" (first header)))
                            lists)))
    ;; Drakma expects a list of pairs, not a list of lists.
    (mapcar (lambda (list)
              (cons (first list) (second list)))
            cleaned)))

(defun content-of (request)
  (webonto::findany '?content `(#_rfc2616:has-content ,request ?content)))

(defun content-type-of (request)
  (webonto::findany '?content-type
                    `(and (#_rfc2616:has-header ,request ?header)
                          (#_rfc2616:field-name ?header "Content-Type")
                          (#_rfc2616:field-value ?header ?content-type))))

(defun method-of (request)
  (webonto::findany '?method `(#_rfc2616:has-method ,request ?method)))

(defun url-of (request)
  (webonto::findany '?url `(#_rfc2616:has-url ,request ?url)))

(defun response-code-of (response)
  (webonto::findany '?code `(#_rfc2616:has-response-code ,response ?code)))

(defun invoke-ocml-request (ocml-request &key
                            (read-timeout irs:*connection-read-timeout*))
  "Make HTTP request according to OCML-REQUEST."
  #-:lispworks (declare (ignore read-timeout))
  ;; OCML-REQUEST is the (name of?) #_rfc2616:http-request which we
  ;; need to turn into a call to utilities:http-request.  We get the
  ;; response, and create an #_rfc2616:http-response to return.
  (let* ((headers (headers-of ocml-request))
         (content (content-of ocml-request))
         (method (method-of ocml-request))
         (url (url-of ocml-request))
         (content-type (content-type-of ocml-request)))
    (when irs.grounding:*debug-stream*
      (format irs.grounding:*debug-stream* "Content: ~%~S~%"
              content))
    (multiple-value-bind (response-body response-code response-headers)
        (funcall #'utilities:http-request
		 url :method (intern method :keyword) :content-type content-type
		 :content content :additional-headers headers
		 #+:lispworks :read-timeout #+:lispworks read-timeout)
      (declare (ignore response-headers))
      (when irs.grounding:*debug-stream*
        (format irs.grounding:*debug-stream* "Content: ~%~S~%"
                response-body))
      (if (= 200 response-code)
          (create-http-response response-code response-body)
          (error "Non 200 response from service at URL '~A'." url)))))

(defun create-http-response (response-code response-body)
  (let ((response
         (webonto::findany
          '?response `(and (#_grnd:ocmlify-http-response ,response-code ,response-body
                                                         ?response)))))
    response))
