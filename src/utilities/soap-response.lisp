(in-package irs-utilities)


(defvar *soap-header* "<SOAP:Envelope xmlns:SOAP='http://schemas.xmlsoap.org/soap/envelope/' SOAP:encodingStyle='http://schemas.xmlsoap.org/soap/encoding/' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xsd='http://www.w3.org/1999/XMLSchema'> <SOAP:Body>")

;;<?xml version="1.0" encoding="UTF-8"?>
(defvar *soap-header2* "<soap-env:Envelope xmlns:soap-env=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" soap-env:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">
      <soap-env:Header/> 
      <soap-env:Body>")



(defvar *soap-end* "</SOAP:Body> </SOAP:Envelope>")

(defvar *soap-end2* "</soap-env:Body>
   </soap-env:Envelope>")

(define-condition irs-soap-error (condition)
  ())

(define-condition soap-response-error
    (irs-soap-error)
  ((has-soap-response  :initarg :soap-response 
               :reader has-soap-response)))


(define-condition irs-connection-error 
    (irs-soap-error)
  ((has-host  :initarg :host
               :reader has-host)
   (has-port  :initarg :port
               :reader has-port)))

(defun crlf (stream)
  "Send an HTTP-compliant CR/LF line ending down STREAM."
  (format stream "~C~C" #\Return #\Newline))

(defun send-soap-response (queryid results types &key (stream t)
                                   downcase-p result-tags-to-leave-case )
  (let* ((stm stream)
	(response (gen-soap-response queryid results types
                                     downcase-p result-tags-to-leave-case))
	(reslen (+ (length *soap-header*) 
		   (length response) 
		   (length *soap-end*))))
    (format stm "HTTP/1.0 200 OK")
    (crlf stm)
    (format stm "Content-Type: text/xml; charset=\"utf-8\"")
    (crlf stm)
    (format stm "Content-Length: ~d" reslen)
    (crlf stm)
    (crlf stm)
    (format stm "~a" *soap-header*)
    (format stm "~a" response)
    (format stm "~a~%~%" *soap-end*)
    (force-output stm)
    ))

(defun send-soap-response2 (queryid results types &key (stream t)
                                    downcase-p
                                    result-tags-to-leave-case)
  (let* ((stm stream)
	(response (gen-soap-response queryid results types downcase-p result-tags-to-leave-case))
	(reslen (+ (length *soap-header2*) 
		   (length response) 
		   (length *soap-end2*))))
    (format stm "HTTP/1.0 200 OK")
    (crlf stm)
    (format stm "Content-Type: text/xml; charset=\"utf-8\"")
    (crlf stm)
    (format stm "Content-Length: ~d" reslen)
    (crlf stm)
    (crlf stm)
    (format stm "~a" *soap-header2*)
    (format stm "~a" response)
    (format stm "~a~%~%" *soap-end2*)
    (force-output stm)
    ))

(defun internal-create-base-string (string)
  (remove-if #'(lambda (char) (not (base-char-p char))) string))

;;use this to ensure that the IRS only sends out base char strings to ensure that this will
;;work on machines with unicode fonts
(defun create-base-string (control-string &rest args)
  (internal-create-base-string (apply #'format nil control-string args)))

(defun gen-soap-response (queryid results types
                                  downcase-p &optional result-tags-to-leave-case)
  (let ((*package* (find-package "OCML")) ;;"CL-USER"))
        (res (create-base-string "<~aResponse>" queryid)))
    (dolist (arg types)
      (let ((name (first arg))
	    (type (second arg)))
        #|
  	(if (string= type "xsd:sexpr")
	    ;;changed by johnd i think there's a bug - 
            ;;it should look like belwo except with ~s instead of ~a
            ;;(setq res (concatenate 'string res (format nil "<~a xsi:type=\"~a\">~s</~a></~aResponse>" name type (pop results) name)))
          (setq res (concatenate 'string res (format nil "<~a xsi:type=\"~a\">~s</~a>" name type (pop results) name)))
	  (setq res (concatenate 'string res (format nil "<~a xsi:type=\"~a\">~a</~a>" name type (pop results) name))))
        |#
        ;;now always use ~s
        ;;;OK now back to ~a. Liliana complained that we shouldn't send something like
        ;;;<string>"foo"</string>
        ;;(setq res (concatenate 'string res (format nil "<~a xsi:type=\"~a\">~a</~a>" name type (pop results) name)))

        (if (string= type "sexpr")
	    ;;Now back to treating xsd:sexpr differently. 
            ;;need to cope with something like ((has-amount "xsd:float") (has-source-currency "xsd:string"))
          (setq res 
                (concatenate 
                 'string res 
                 (create-base-string (if (and downcase-p (not (find name result-tags-to-leave-case)))
                                 "<~a type=\"~a\">~(~s~)</~a>"
                               "<~a type=\"~a\">~s</~a>")
                         name type (pop results) name)))
	  (setq res 
                (concatenate 
                 'string res 
                 (create-base-string
                         (if (and downcase-p (not (find name result-tags-to-leave-case)))
                             "<~a type=\"~a\">~(~a~)</~a>"
                           "<~a type=\"~a\">~a</~a>")
                         name type (pop results) name))))
        ))
   (concatenate 'string res (create-base-string "</~aResponse>" queryid))))

(defun create-html-value (value &optional use-full-name)
  (insert-html-characters
   (if use-full-name
       (format nil "~s" value)
     (format nil "~a" value))))

(defun gen-soap-args (types values)
  (let ((res ""))
    (dolist (arg types)
      (let ((name (first arg))
	    (type (second arg))
            (value (pop values)))
	(setq res (concatenate 'string res 
                               (if (string= type "sexpr")
                                   (create-base-string "<~a type='~a'>~s</~a>" name type value name)
                                 (create-base-string "<~a type='~a'>~a</~a>" name type value name))))))
    res))

(defun open-irs-connection (host port)
  (let ((stream (comm::open-tcp-stream host port
                                       :read-timeout *connection-read-timeout* 
                                       :timeout *connection-timeout*)))
    (if stream
        stream
      (signal 'irs-connection-error :host host :port port))))

(defun soap-http-client (proc types args &key host (port 80) (stream t)
                              (publisher-location "/soap") output-type)
  "Send a soap request to the specified HTTP server using the standard
   POST method. Keywords:
host - the HTTP server
port - the HTTP port (default is 80)
request - the soap request string"
  (declare (ignore stream))
  (let ((stm (open-irs-connection host port))
	(headlen (length *soap-header2*))
	(endlen (length *soap-end2*))
	(actionlen (length proc))
	(argsstring (gen-soap-args types args))
        (values nil))
    (format stm "POST ~a HTTP/1.0~%" publisher-location)
    (format stm "Host: ~a:~a~%" host port)
    (format stm "Content-Type: text/xml; charset=utf-8~%")
    (format stm "Content-length: ~a~%" (+ headlen endlen (* 2 actionlen) (length argsstring) 5))
    (format stm "SOAPAction: \"/soap\" ~%~%")
    (format stm "~a" ;;*soap-header*)
            ;;changed to *soap-header2* for liliana
            *soap-header2*)
    (format stm "<~a>" proc)
    (format stm "~a" argsstring)
    (format stm "</~a>" proc)
    (format stm "~a~%~%" ;;*soap-end*)
            ;;changed to *soap-end2* for liliana
            *soap-end2*)
    (force-output stm)
    (handler-case
	(setf values (multiple-value-list (parse-soaphttp-response proc stm output-type)))
      (end-of-file (c)
	(declare (ignore c))
	(print "Connection closed.")))
    (apply #'values values)))

(defun uppercase-string (substring string)
  (let ((position (search substring string)))
    (if position 
        (uppercase-string 
         substring
         (string-upcase string :start position 
                        :end (+ position (length substring))))
      string)))

(defun parse-soaphttp-response (proc stm output-type)
  (let ((response-line (http-read-line stm "NIL"))
	soapresponse)
    (setq response-line (string-trim '(#\Space) response-line))
    (let ((first (position #\Space response-line))
	  (last (position #\Space response-line :from-end t)))
      (cond ((null first)		;no spaces, invalid response
	     (format t "Invalid response: ~a~%" response-line))
	    ((= first last)
	     (format t "Server error: ~a~%" response-line))
	    (t (progn
		 (setf (http-info-headers *http-info*)
		       (process-headers stm))
		 (let* ((length (http::get-content-length http::*http-info*))
			(line  (http::http-read-chars stm length nil))
			(lines nil))
		   (when (and line (not (zerop (length line))))
		     (push line lines))
		   (setf soapresponse 
                         (apply #'concatenate 'string (reverse lines))))))))
    (handler-case 
        (cond ((api-soap-old:xml-soap-type-p output-type)
               (multiple-value-bind (result ok-p error-type)
                   (get-irs-publisher-generated-return-value 
                    soapresponse output-type)
                 (if ok-p
                     result
                     (signal error-type :soap-response result))))
              (t (multiple-value-bind (result ok-p error-type)
                     (get-irs-publisher-generated-return-value 
                      soapresponse output-type)
                   (cond (ok-p
                          (let ((soapmsg (api-soap-old:get-msg
                                          (api-soap-old:soap-xml-parse soapresponse))))
                            (if (string= (api-soap-old:xml-name soapmsg)
                                         (concatenate 'string proc "Response"))
                                (apply #'values (api-soap-old:get-soapvalues soapmsg))
                                (format t "IRS error: wrong response id:~a~%"
                                        (api-soap-old:xml-name soapmsg)))))
                         (t (signal error-type :soap-response result))))))
      (error (c)
	(declare (ignore c))
        (multiple-value-bind (result ok-p error-type)
            (get-irs-publisher-generated-return-value 
             soapresponse output-type)
          (if ok-p
              result
              (signal error-type :soap-response result)))))))



(defvar *irs-generated-return-value-start*
  ;;"<RESULT type=\"sexpr\">"
  "<RESULT type=\"")

(defvar *irs-generated-return-value-end* "</RESULT>")

(defvar *soap-error-response-return-value-start*
  "<ERROR type")

(defvar *soap-error-response-return-value-end*
  "</ERROR>")

(defun soap-error-reponse-p (soap-response)
  (and (> (length soap-response) 
          (length *soap-error-response-return-value-start*))
       (string= (string-downcase
                 (subseq soap-response 0 (length *soap-error-response-return-value-start*)))
                *soap-error-response-return-value-start*)))

(defun get-irs-publisher-generated-return-value (soap-response output-type)
  (let ((start-position (search *irs-generated-return-value-start* 
                                soap-response))
        (error-start-position 
         (search *soap-error-response-return-value-start* 
                 soap-response))
        (end-position nil))
    (cond (start-position
           (setf start-position
                 (1+ (search ">" soap-response :start2 start-position))
                 end-position
                 (search *irs-generated-return-value-end* 
                         soap-response :start2 
                         start-position))
           (when end-position
             (values 
              (let ((return-value (subseq soap-response 
                                          start-position
                                          end-position)))
                (if (api-soap-old:string-soap-type-p output-type)
                    return-value
                    (if (char= (elt return-value 0) #\")
                        (if (> end-position start-position)
                            (read-from-string return-value)
                            nil)
                        return-value)))
              t)))
          (error-start-position
           (setf start-position
                 (1+ (search ">" soap-response :start2 error-start-position))
                 end-position
                 (search *soap-error-response-return-value-end*
                         soap-response :start2 
                         start-position))
           (when end-position
             (values 
              (let ((return-value (subseq soap-response 
                                          start-position
                                          end-position)))
                (if (api-soap-old:string-soap-type-p output-type)
                    return-value
                    (if (char= (elt return-value 0) #\")
                        (if (> end-position start-position)
                            (read-from-string return-value)
                            nil)
                        return-value)))
              nil
              'soap-response-error))))))



(defun send-soap-error-message (condition name stream)
  (send-soap-response2 (symbol-name name)
                       (list 
                        (http::send-error-message nil condition))
                       `((error "string"))
                       :stream stream))

(defun send-soap-error-message2 (message name stream)
  (send-soap-response2 (symbol-name name)
                       (list message)
                       `((error "string"))
                       :stream stream))

(defun send-soap-warning-message (condition name stream)
  (send-soap-response2 (symbol-name name)
                       (list 
                        (http::send-error-message nil condition))
                       `((warning "string"))
                       :stream stream))