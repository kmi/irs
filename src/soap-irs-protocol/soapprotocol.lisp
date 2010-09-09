(in-package irs-protocol)

(defvar mysoapmsg nil)

(defvar mymsg nil)

(defvar myaction nil)

(defmethod http::http-reply ((method (eql :irssoap)) request)
  (ocml::with-ocml-thread-safety ()
    (setf mymsg request)
    (handler-case 
        (let ((*package* (find-package "IRS-PROTOCOL"))
	      (soapmsg (api-soap-old:get-msg (api-soap-old:soap-xml-parse request)))
              action)
          (setf mysoapmsg soapmsg)
          (setf myaction (lisp-read-string (api-soap-old:xml-name soapmsg)))
          (setf action (lisp-read-string (api-soap-old:xml-name soapmsg)))
          (handle-soap-request 
           action http::*http-stream*
	   (api-soap-old:get-soapvalues soapmsg))
          )
      (ocml::no-role-value-error
       (c)
       (http::send-error-message 
        http::*http-stream* 
        c))
      (error
       (c)
       (http::send-error-message http::*http-stream* 
                                 c))
      (serious-condition 
       (c)
       (http::send-error-message http::*http-stream* 
                                 c)))))

(defun lisp-read-string (string)
  (with-input-from-string (str string)
			  (read str)))

(defmethod handle-soap-request (action html-stream request)
  (format html-stream "liliana 2 Unknown action ~a~%" action))

(defmethod handle-soap-request ((action (eql 'show-details-with-type))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology type name)
    ;; XXX should this reference ISTREAM or REQUEST?
    (with-input-from-string (istream upcase-request)
      (setf ontology (pop request)
            type (pop request)
            name (pop request))
      (show-details-with-type html-stream ontology type name))))

(defmethod handle-soap-request ((action (eql 'show-details))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology name)     
    (with-input-from-string (istream upcase-request)
      (setf ontology (pop request)
            name (pop request))
      (show-details html-stream ontology name))))      

(defmethod handle-soap-request ((action (eql 'get-all-domain-ontologies))
                               html-stream request)
  (declare (ignore action request))
  (all-domain-ontologies html-stream))

(defmethod handle-soap-request ((action (eql 'get-all-tasks))
                               html-stream request)
  (declare (ignore action request))
  (all-tasks html-stream))

(defmethod handle-soap-request ((action (eql 'get-applicable-psms))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology task)
    (with-input-from-string (istream upcase-request)
      (setf ontology (pop request)
            task (pop request)))
    (applicable-psms html-stream ontology task)))


(defmethod handle-soap-request ((action (eql 'get-task-subtasks))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
        (*package* (find-package "OCML"))
        (task (read-from-string upcase-request)))
    (task-subtasks html-stream task)))

(defmethod handle-soap-request ((action (eql 'get-psm-subpsms))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
        (*package* (find-package "OCML"))
        (ontology-name (read-from-string upcase-request)))))

(defmethod handle-soap-request ((action (eql 'combine-domain-task-models))
                               html-stream request)
   (let* ((upcase-request (string-upcase request))
          (*package* (find-package "OCML"))
          ontology task)     
    (with-input-from-string (istream upcase-request)
      (setf ontology (pop request)
            task (pop request))
      (combine-domain-task-models html-stream ontology task))))

(defmethod handle-soap-request ((action (eql 'combine-psm-with-domain-task-model))
                               html-stream request)
   (let* ((upcase-request (string-upcase request))
          (*package* (find-package "OCML"))
          domain-task-model psm)     
    (with-input-from-string (istream upcase-request)
      (setf domain-task-model (pop request)
            psm (pop request))
      (combine-psm-with-domain-task-model html-stream domain-task-model psm))))

(defmethod handle-soap-request ((action (eql 'get-task-input-roles))
                               html-stream request)
   (let* ((upcase-request (string-upcase request))
          (*package* (find-package "OCML"))
          (task (read-from-string upcase-request)))
     (get-task-input-roles html-stream task)))

(defmethod handle-soap-request ((action (eql 'get-method-input-roles))
                               html-stream request)
   (let* ((upcase-request (string-upcase request))
          (*package* (find-package "OCML"))
          (method (read-from-string upcase-request)))
     (get-method-input-roles html-stream method)))

(defmethod handle-soap-request ((action (eql 'create-application))
                               html-stream request)
   (let* ((upcase-request (string-upcase request))
          (*package* (find-package "OCML")))
     (with-input-from-string (istream upcase-request)
        (let ((task-ontology (pop request))
              (method-ontology (pop request))
              (domain-ontology (pop request))
              (task-type (pop request))
              (method-type (pop request))
              (key-parameters (pop request))
              (task-input (pop request))
              (method-input (pop request)))
          (create-application html-stream task-ontology method-ontology 
                              domain-ontology task-type method-type key-parameters
                              task-input method-input)))))

(defmethod handle-soap-request ((action (eql 'get-instances))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology class)
    (with-input-from-string (istream upcase-request)
      (setf ontology (pop request)
            class (pop request)))
    (get-instances html-stream ontology class)))

(defmethod handle-soap-request ((action (eql 'get-slots-with-type))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology name)
    (with-input-from-string (istream upcase-request)
      (setf ontology (pop request)
            name (pop request)))
    (get-slots-with-type html-stream ontology name)))


(defmethod handle-soap-request ((action (eql 'create-mapping-To-Class-Hierarchy))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology top-classes name)     
    (with-input-from-string (istream upcase-request)
      (setf ontology (pop request) 
            top-classes (pop request) 
            name (pop request)))
    (create-mapping-to-class-hierarchy html-stream ontology top-classes name)))

(defmethod handle-soap-request ((action (eql 'create-class-instance))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology class slot-value-pairs)     
    (with-input-from-string (istream upcase-request)
      (setf ontology (pop request) 
            class (pop request) 
            slot-value-pairs (pop request)))
    (create-class-instance html-stream ontology class slot-value-pairs)))

(defmethod handle-soap-request ((action (eql 'create-named-class-instance))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology class slot-value-pairs instance-name)     
    (with-input-from-string (istream upcase-request)
      (setf ontology (pop request) 
            class (pop request) 
            instance-name (pop request)
            slot-value-pairs (pop request)))
    (create-class-instance html-stream ontology class slot-value-pairs instance-name)))

(defmethod handle-soap-request ((action (eql 'run-application))
                                html-stream request)
  (let* ((*package* (find-package "OCML"))
         (upcase-request (string-upcase request))
         application-ontology application-function key-parameters)
    (with-input-from-string (istream upcase-request)
      (setf application-ontology (pop request)
            application-function (pop request)
            key-parameters (pop request)))
    (run-application html-stream application-ontology application-function key-parameters)))


;;(http::http-reply :IRSSOAP "<SOAP:Envelope xmlns:SOAP='http://schemas.xmlsoap.org/soap/envelope/' SOAP:encodingStyle='http://schemas.xmlsoap.org/soap/encoding/' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xsd='http://www.w3.org/1999/XMLSchema'> <SOAP:Body><PSM-SOAP-DEFINITION><ONTOLOGY xsi:type=\"xsd:sexpr\">GENHOSPITALBEDSERVICE</ONTOLOGY><METID xsi:type=\"xsd:sexpr\">GENHOSPITALBEDSERVICE_ANOTHER_HIPPY_HOSPITAL_BED_SERVICE</METID><ARGS xsi:type=\"xsd:sexpr\">((HAS_MEDICAL_SERVICE \"xsd:sexpr\") (HAS_HOSPITAL_NAME \"xsd:sexpr\"))</ARGS><RESULT xsi:type=\"xsd:string\">xsd:sexpr</RESULT><METHOD xsi:type=\"xsd:string\">ANOTHER_HIPPY_HOSPITAL_BED_SERVICE</METHOD><HOST xsi:type=\"xsd:string\">137.108.24.227</HOST><PORT xsi:type=\"xsd:int\">3000</PORT></PSM-SOAP-DEFINITION></SOAP:Body> </SOAP:Envelope>")

(defmethod handle-soap-request ((action (eql 'psm-soap-definition))
                                html-stream args)
  (let* ((*package* (find-package "OCML"))
         ontology method-name input-roles output lisp-function
         publisher-ip-address publisher-port publisher-location)
    (setf ontology (make-ocml-symbol (pop args))
          method-name (make-ocml-symbol (pop args))
          input-roles (mapcar #'(lambda (x)
                                  (cons (make-ocml-symbol (car x))
                                        (cdr x)))
                              (pop args))
          output (pop args)
          lisp-function (pop args)
          publisher-ip-address (pop args)
	  publisher-port (pop args)
          publisher-location  (pop args))
    (internal-psm-soap-definition ontology method-name
                                  input-roles output lisp-function
                                  publisher-ip-address
                                  publisher-port
                                  (if (stringp publisher-location)
                                      publisher-location
                                    (format nil "~a" publisher-location)))
    (format html-stream "The method ~:(~a~) in the ontology ~:(~a~) has been published." 
            method-name
            ontology)))

(defmethod handle-soap-request ((action (eql 'get-web-service-message-exchange-pattern))
                                html-stream args)
  (let* ((*package* (find-package "OCML"))
         ontology method-name)
    (setf ontology (make-ocml-symbol (pop args))
          method-name (make-ocml-symbol (pop args)))
    (format html-stream
            "~a~%"
            (internal-get-web-service-message-exchange-pattern ontology method-name))))

(defmethod handle-soap-request ((action (eql 'get-web-service-operation-io-mappings))
                                html-stream args)
  (let* ((*package* (find-package "OCML"))
         ontology method-name)
    (setf ontology (make-ocml-symbol (pop args))
          method-name (make-ocml-symbol (pop args)))
    (format html-stream
            "~s~%"
            (internal-get-web-service-operation-io-mappings ontology method-name))))

(defmethod handle-soap-request ((action (eql 'publish-wsmo-lisp-function))
                                html-stream args)
  (let* ((*package* (find-package "OCML"))
         user ontology method-name 
         publisher-ip-address publisher-port publisher-location)
    (setf user (make-ocml-symbol (pop args))
          ontology (make-ocml-symbol (pop args))
          method-name (make-ocml-symbol (pop args))
          publisher-ip-address (pop args)
	  publisher-port (pop args)
          publisher-location  (pop args))
    (internal-publish-wsmo-web-service user ontology method-name
                                       publisher-ip-address
                                       publisher-port
                                       (if (stringp publisher-location)
                                           publisher-location
                                         (format nil "~a" publisher-location)))
    (format html-stream "The method ~:(~a~) in the ontology ~:(~a~) has been published." 
            method-name
            ontology)))

(defun make-ocml-symbol (symbol)
  (if (symbolp symbol)
      (intern (symbol-name symbol) (find-package "OCML"))
    symbol))

(defun make-ocml-list (x)
  (cond ((atom x) 
         (if (symbolp x)
             (make-ocml-symbol x)
           x))
        (t (cons (make-ocml-list (car x)) (make-ocml-list (cdr x))))))

(defun make-soap-type (namespace x)
  (unless (stringp x)
    (setf x (symbol-name x)))
  (setf x (string-downcase x))
;;Liliana
;;(concatenate 'string namespace ":" x))
   (concatenate 'string namespace  x))

(defmethod handle-soap-request ((action (eql 'task-info))
                               html-stream args)
  (let* ((*package* (find-package "OCML"))
         ontology method-name publisher-ip-address publisher-port)
    (setf ontology (make-ocml-symbol (pop args))
          method-name (make-ocml-symbol (pop args))
          publisher-ip-address (pop args)
	  publisher-port (pop args))
    (get-task-info ontology method-name html-stream)))

(defmethod handle-soap-request ((action (eql 'goal-info))
                               html-stream args)
  (let* ((*package* (find-package "OCML"))
         ontology web-service-name publisher-ip-address publisher-port)
    (setf ontology (make-ocml-symbol (pop args))
          web-service-name (make-ocml-symbol (pop args))
          publisher-ip-address (pop args)
	  publisher-port (pop args))
    (get-goal-info ontology web-service-name html-stream)))

(defmethod handle-soap-request ((action (eql 'web-service-operation-info))
                               html-stream args)
  (let* ((*package* (find-package "OCML"))
         ontology web-service-name operation-name publisher-ip-address publisher-port)
    (setf ontology (make-ocml-symbol (pop args))
          web-service-name (make-ocml-symbol (pop args))
          operation-name (make-ocml-symbol (pop args))
          publisher-ip-address (pop args)
	  publisher-port (pop args))
    (get-web-service-operation-info ontology web-service-name operation-name html-stream)))


;;;soap response method
; John this is the soap response method
; it is not supported because I am not able to get the task result type
; from the ocml structure
;;;johnd i've put in the relevant call
(defmethod handle-soap-request ((action
                                 (eql 'call-remote-irs-method-soap-response))
                                html-stream request)
  (let ((*package* (find-package "OCML"))
        ontology method-name arguments result)
    (setf ontology (pop request) 
          method-name (pop request)
          arguments (pop request))
    (multiple-value-bind (input-roles output) 
        (internal-get-task-info ontology method-name)
      (declare (ignore input-roles))
      (setf result (invoke-remote-irs-method ontology method-name arguments))
      (iu::send-soap-response (symbol-name method-name) (list result) 
			           `(result ,output) :stream html-stream))))

(defmethod handle-soap-request ((action (eql 'call-remote-irs-method))
                               html-stream request)
  (let ((*package* (find-package "OCML"))
	 ontology method-name arguments)
      (setf ontology (pop request) 
            method-name (pop request)
            arguments (pop request))
      (format html-stream "~s~%" 
	      (invoke-remote-irs-method ontology method-name arguments))))

(defmethod handle-soap-request ((action (eql 'ontology-methods))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (pop request))
    (with-ontology-check (ontology html-stream)
          (format html-stream "~{~{~(~a~) ~{~(~a~) ~a~}%~}~}~%" 
                (ontology-methods ontology)))))

(defmethod handle-soap-request ((action (eql 'ontology-tasks))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (pop request))
    (with-ontology-check (ontology html-stream)
			 (format html-stream "~(~{~a ~}~)~%"
				 (ontology-tasks ontology)))))

(defmethod handle-soap-request ((action (eql 'task-methods))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology task)
      (setf ontology (pop request)
            task (pop request))
      (with-ontology-check (ontology html-stream)
        (task-methods ontology task html-stream))))

(defmethod handle-soap-request ((action (eql 'method-details))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology method)
    (setf ontology (pop request)
            method (pop request))
      (with-ontology-check (ontology html-stream)
        (method-details ontology method html-stream))))


(defmethod handle-soap-request ((action (eql 'task-details))
                               html-stream request)
  (let* (ontology task)
    (setf ontology (pop request)
	  task (pop request))
      (with-ontology-check (ontology html-stream)
        (task-details ontology task html-stream))))

;;(http-reply :irssoap "<SOAP:Envelope xmlns:SOAP='http://schemas.xmlsoap.org/soap/envelope/' SOAP:encodingStyle='http://schemas.xmlsoap.org/soap/encoding/' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xsd='http://www.w3.org/1999/XMLSchema'> <SOAP:Body><ACHIEVE-TASK><ONTOLOGY xsi:type=\"xsd:sexpr\">FAST-ARTHRITIS-DIAGNOSIS-PSM</ONTOLOGY><TASK-TYPE xsi:type=\"xsd:sexpr\">ARTHRITIS-DIAGNOSIS</TASK-TYPE><NUMBER-OF-INPUT-ROLE-VALUE-PAIRS xsi:type=\"xsd:int\">4</NUMBER-OF-INPUT-ROLE-VALUE-PAIRS><OBSERVABLE1 xsi:type=\"xsd:sexpr\">OCML::OBSERVABLE1</OBSERVABLE1><SEVERE-JOINT-PAIN xsi:type=\"xsd:sexpr\">OCML::SEVERE-JOINT-PAIN</SEVERE-JOINT-PAIN><OBSERVABLE2 xsi:type=\"xsd:sexpr\">OCML::OBSERVABLE2</OBSERVABLE2><SEVERE-JOINT-STIFFNESS xsi:type=\"xsd:sexpr\">OCML::SEVERE-JOINT-STIFFNESS</SEVERE-JOINT-STIFFNESS><OBSERVABLE3 xsi:type=\"xsd:sexpr\">OCML::OBSERVABLE3</OBSERVABLE3><SEVERE-SWELLING xsi:type=\"xsd:sexpr\">OCML::SEVERE-SWELLING</SEVERE-SWELLING><OBSERVABLE4 xsi:type=\"xsd:sexpr\">OCML::OBSERVABLE4</OBSERVABLE4><JOINT-CREAKING xsi:type=\"xsd:sexpr\">OCML::JOINT-CREAKING</JOINT-CREAKING></ACHIEVE-TASK></SOAP:Body> </SOAP:Envelope>")

(defmethod handle-soap-request ((action (eql 'achieve-task))
                               html-stream request)
  ;;(setf rr request)
  (handle-achieve-task html-stream request nil))

(defmethod handle-soap-request ((action (eql 'achieve-task-soap-response))
                               html-stream request)
  ;;(setf rr request)
  (handle-achieve-task html-stream request t))

(defun handle-achieve-task (html-stream request soap-response-p)
  ;;(setf r1 request s soap-response-p)
  (let* ((*package* (find-package "OCML"))
	 (ontology (make-ocml-symbol (pop request)))
	 (task-type (make-ocml-symbol (pop request)))
         (number-of-input-role-value-pairs (pop request))
	 (input-role-value-pairs nil))
    (dotimes (i number-of-input-role-value-pairs)
      (push (list (make-ocml-symbol (pop request)) 
                  (make-ocml-symbol (pop request)))
            input-role-value-pairs))
    (with-ontology-check (ontology html-stream)
        (irs-achieve-task ontology task-type input-role-value-pairs 
                            html-stream soap-response-p))))

(defmethod handle-soap-request ((action (eql 'achieve-goal))
                               html-stream request)
  ;;(setf rr request)
  (handle-achieve-goal html-stream request nil))

(defmethod handle-soap-request ((action (eql 'achieve-goal-soap-response))
                               html-stream request)
  ;;(setf rr request)
  (handle-achieve-goal html-stream request t))

;;(handle-achieve-goal *standard-output* '(WSMO-USE-CASE BUY-TRAIN-TICKET-GOAL 4 OCML::HAS-PERSON CHRISTOPH OCML::HAS-DEPARTURE-STATION FRANKFURT OCML::HAS-DESTINATION-STATION BERLIN OCML::HAS-DATE-AND-TIME (3 4 6 18 8 2004)) nil)

(defun input-roles-with-soap-bindings (x)
  ;;(reverse (ocml::setofall '?x `(ocml::HAS-wsmo-INPUT-soap-binding ,x ?x)))
  (ocml::findany '?x `(ocml::all-wsmo-input-soap-bindings ,x ?x))

  )

(define-constant +non-string-irs-soap-types+
  '("sexpr" "float" "integer"))

(defun non-string-irs-soap-type (x)
  (and (stringp x) (member x +non-string-irs-soap-types+ :test #'string=)))

(defun sexpr-soap-type-p (x)
  (and (stringp x) (string= x "sexpr")))
 
(defun input-role-soap-type (x input-role non-local-p)
  (let ((input-roles-with-soap-bindings
         (if non-local-p
             (input-roles-with-soap-bindings x)
           (local-input-roles-with-soap-bindings x))))
  (second (assoc input-role input-roles-with-soap-bindings))))

(defun local-input-roles-with-soap-bindings (x)
  (reverse (web-onto::findany '?x `(ocml::all-local-wsmo-input-roles-with-soap-bindings ,x ?x))))

(defun generate-input-role-value (goal-type role-name value)
  (let ((soap-type (input-role-soap-type goal-type role-name t)))
    (if (non-string-irs-soap-type soap-type)
        (if (sexpr-soap-type-p soap-type)
            (iu::read-in-sexpr-input value)
            (make-ocml-symbol (read-from-string (string-upcase value))))
        (if (string= soap-type "string")
            value
            (make-ocml-symbol value)))))

(defun handle-achieve-goal (html-stream request soap-response-p)
  ;;(setf r1 request s soap-response-p)
  (let* ((*package* (find-package "OCML"))
	 (ontology (make-ocml-symbol (pop request)))
	 (goal-type (make-ocml-symbol (pop request)))
         (number-of-input-role-value-pairs (pop request))
	 (input-role-value-pairs nil))
    (with-ontology-check (ontology html-stream)
      (ocml::select-ontology ontology)
      (dotimes (i number-of-input-role-value-pairs)
        (let* ((role-name (make-ocml-symbol (pop request)))
               (raw-role-value (pop request))
               (role-value 
                (generate-input-role-value goal-type role-name 
                                           (if (stringp raw-role-value)
                                               raw-role-value
                                             (format nil "~s" raw-role-value)))))
          (push (list role-name role-value)
                input-role-value-pairs)))
      (irs-achieve-goal ontology goal-type (reverse input-role-value-pairs)
                        html-stream soap-response-p))))
      

;;irs upload ontology ontology-uses ontology-type ontology-format 
;;url new/overwrite/append 
;;author password allowed-editors

;;;test with 
;;(irs-http-client :host "villapark.open.ac.uk" :request "irs upload foo kmi-planet-kb :domain ocml \"http://pckm070.open.ac.uk/upload-test.html\" new \"john\" \"bwraf1\" enrico")

;;(handle-irs-request 'upload *standard-output* "foo (kmi-planet-kb) :domain ocml \"http://akt.open.ac.uk/upload-test.html\" new \"john\" \"bwraf1\" enrico")
;;

(defmethod handle-irs-request ((action (eql 'upload-code))
                               html-stream request)
  ;;(setf bob request)
  (with-input-from-string (istream request)
    (let* ((*package* (find-package "OCML"))
           (knowledge-model (read istream))
           (knowledge-model-uses (read istream))
           (knowledge-model-type (read istream))
           (knowledge-model-format (read istream))
           (code (read istream))
           (upload-mode (read istream))
           (author (read istream))
           (password (read istream))
           (allowed-editors (read istream)))             
      (internal-upload (upcase-symbol knowledge-model "OCML")
                       (mapcar #'(lambda (x)
                                   (upcase-symbol x "OCML"))
                               knowledge-model-uses)
                       (upcase-symbol knowledge-model-type "KEYWORD") 
                       (upcase-symbol knowledge-model-format "OCML")
                       code (upcase-symbol upload-mode "OCML")
                       author
                       password allowed-editors 
                       html-stream))))

(defun get-class-slot-value (class-name slot-name)
  (when class-name
    (web-onto::findany '?x 
                       `(= ?x (ocml::the-class-slot-value 
                               ,class-name ,slot-name)))))

(defun get-class-slot-values (class-name slot-name)
  (when class-name 
    (web-onto::findany '?x 
                       `(= ?x (ocml::all-class-slot-values 
                               ,class-name ,slot-name)))))
(defun class-home-ontology (x)
  (let ((class-structure (ocml::get-domain-class x)))
    (when class-structure
      (ocml::name (ocml::home-ontology class-structure)))))

(defun get-mediator-source-with-ontology (mediator)
  (let ((source (get-class-slot-value mediator 'ocml::has-source-component)))
    (list source (class-home-ontology source))))

(defmethod handle-soap-request ((action (eql 'get-info-mediator-source))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (make-ocml-symbol (pop request)))
    (with-ontology-check (ontology html-stream)
      (ocml::select-ontology ontology)
      (format html-stream "~(~{~a ~a~}~)~%"
              (get-mediator-source-with-ontology (make-ocml-symbol (pop request)))))))

(defun get-mediator-targets-with-ontology (mediator)
  (let ((targets (get-class-slot-values mediator 'ocml::has-target-component)))
    (mapcar #'(lambda (target)
                (list target (class-home-ontology target)))
            targets)))

(defmethod handle-soap-request ((action (eql 'get-info-mediator-targets))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (make-ocml-symbol (pop request)))
    (with-ontology-check (ontology html-stream)
      (ocml::select-ontology ontology)
      (format html-stream "~(~{~{~a ~a~}~}~)~%"
              (get-mediator-targets-with-ontology (make-ocml-symbol (pop request)))))))

(defun get-mediator-target-with-ontology (mediator)
  (let ((target (get-class-slot-value mediator 'ocml::has-target-component)))
    (list target (class-home-ontology target))))

(defmethod handle-soap-request ((action (eql 'get-info-mediator-target))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (make-ocml-symbol (pop request)))
    (with-ontology-check (ontology html-stream)
      (ocml::select-ontology ontology)
      (format html-stream "~(~{~a ~a~}~)~%"
              (get-mediator-target-with-ontology (make-ocml-symbol (pop request)))))))

(defun get-mediator-service-with-ontology (mediator)
  (let ((mediation-service (get-class-slot-value mediator 'ocml::has-mediation-service)))
    (list mediation-service (class-home-ontology mediation-service))))

(defmethod handle-soap-request ((action (eql 'get-info-mediation-service))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (make-ocml-symbol (pop request)))
    (with-ontology-check (ontology html-stream)
      (ocml::select-ontology ontology)
      (format html-stream "~(~{~a ~a~}~)~%"
              (get-mediator-service-with-ontology (make-ocml-symbol (pop request)))))))

(defun get-item-used-mediators-with-ontology (item)
  (let ((used-mediators (get-class-slot-values item 'ocml::used-mediator)))
    (mapcar #'(lambda (used-mediator)
                (list used-mediator (class-home-ontology used-mediator)))
            used-mediators)))

(defmethod handle-soap-request ((action (eql 'get-info-used-Mediators))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (make-ocml-symbol (pop request)))
    (with-ontology-check (ontology html-stream)
      (ocml::select-ontology ontology)
      (format html-stream "~(~{~{~a ~a~}~}~)~%"
              (get-item-used-mediators-with-ontology (make-ocml-symbol (pop request)))))))

(defun get-web-service-capability-with-ontology (web-service)
  (let ((capability (get-class-slot-value web-service 'ocml::has-capability)))
    (list capability (class-home-ontology capability))))

(defmethod handle-soap-request ((action (eql 'get-web-service-capability))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (make-ocml-symbol (pop request)))
    (with-ontology-check (ontology html-stream)
      (ocml::select-ontology ontology)
      (format html-stream "~(~{~a ~a~}~)~%"
              (get-web-service-capability-with-ontology (make-ocml-symbol (pop request)))))))

(defun get-web-service-interface-with-ontology (web-service)
  (let ((interface (get-class-slot-value web-service 'ocml::has-interface)))
    (list interface (class-home-ontology interface))))

(defmethod handle-soap-request ((action (eql 'get-web-service-interface))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology)
    (setf ontology (make-ocml-symbol (pop request)))
    (with-ontology-check (ontology html-stream)
      (ocml::select-ontology ontology)
      (format html-stream "~(~{~a ~a~}~)~%"
              (get-web-service-interface-with-ontology (make-ocml-symbol (pop request)))))))
