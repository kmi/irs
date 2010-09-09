(in-package :visualiser)

(defun add-visualizers (ontology)
  (with-ontology (ontology)
    (let ((ontology-structure (ocml::get-ontology ontology)))
      (mapc #'(lambda (web-service)
                (when (and (ocml::get-domain-class web-service)
                           (eq (ocml::home-ontology (ocml::get-domain-class web-service))
                               ontology-structure)
                           (ocml::holds? 'ocml::subclass-of web-service 'ocml::web-service))
                  (let ((input-roles (ip::input-roles-with-soap-bindings web-service))
                        (output (second (wp::output-role-with-soap-binding web-service)))
                        (host (wp::web-service-host web-service))
                        (port (wp::web-service-port web-service)))
                    (when host
                      (add-process-visualizer ontology web-service
                                              input-roles output
                                              host port)
                      (irs.api.javascript:event :register-service web-service
                                                (format nil "~A:~A" host port))))))
            (ocml::setofall '?x '(ocml::subclass-of ?x ocml::web-service))))))

;;; XXX Why on earth do we extract the roles, hostname and port in the
;;; caller, and then *pass the web service we got it from as the
;;; method*?!
(defun add-process-visualizer (ontology method-name input-roles output
			       publisher-ip-address publisher-port)
  (labels ((clean-up-method-name (method-name ontology)
	     (let ((unwanted-prefix (format nil "~A:" ontology)))
	       (if (string= unwanted-prefix method-name
			    :end1 (length unwanted-prefix)
			    :end2 (length method-name))
		   (string-capitalize (subseq method-name (length unwanted-prefix)))
		   (string-capitalize method-name)))))
    (when *irs-process-visualizer*
      (let* ((service-name (clean-up-method-name (symbol-name method-name)
						 (symbol-name ontology)))
	     (button (find-service *irs-process-visualizer* service-name)))
	(cond (button)
	      (t (add-service *irs-process-visualizer*
			      service-name ontology input-roles output
			      publisher-ip-address
			      publisher-port)))))))

(defun received-achieve-task-message (ontology task-type input-role-value-pairs
                                      &optional (service-name *irs-server-service-name*) (type "Goal"))
  (when *irs-process-visualizer*
    (update-images *irs-process-visualizer* service-name 'receiving)
    (iv-format *irs-process-visualizer* service-name
               "~%~%Received Achieve ~a Message~%Ontology: ~(~a~)~%~a Type: ~(~a~)~{~{~%  ~(~a~) ~s~}~}~%"
               type ontology type task-type input-role-value-pairs))
  (im:send-event im:*session-id-placeholder*
		 (im:create-start-achieve-goal-event task-type input-role-value-pairs)))

(defun irs-protocol::go-to-idle (&optional (service-name *irs-server-service-name*))
  (when *irs-process-visualizer*
    (update-images *irs-process-visualizer* service-name 'idle)))

(defun sending-achieve-task-message (ontology task-type input-role-value-pairs result
                                     &optional (service-name *irs-server-service-name*)
        (type "Goal"))
  (when *irs-process-visualizer*
    (update-images *irs-process-visualizer* service-name 'sending)
    (iv-format *irs-process-visualizer* service-name
               "~%~%Achieve ~a Completed~%Ontology: ~(~a~)~%~a Type: ~(~a~)~{~{~%  ~(~a~) ~s~}~}~%Result:~% ~a~%"
               type ontology type task-type input-role-value-pairs result))
  (im:send-event im:*session-id-placeholder*
		 (im:create-end-achieve-goal-event task-type result)))

(defun processing (ontology task-type input-role-value-pairs
                   &optional (service-name *irs-server-service-name*) (type "Goal"))
  (when *irs-process-visualizer*
    (update-images *irs-process-visualizer* service-name 'processing)
    (iv-format *irs-process-visualizer* service-name
               "~%Processing ~a Type ~a in Ontology ~(~a~)~%With Input Role Value Pairs:~{~{~%  ~(~a~) ~s~}~}~%"
               type task-type ontology input-role-value-pairs)))

(defun sending-remote-procedure-call-message
    (method-instance input-role-value-pairs &optional operation-name)
  (let ((service-name (iu:get-service-name method-instance)))
    (when *irs-process-visualizer*
      (update-images *irs-process-visualizer* *irs-server-service-name* 'sending)
      (iv-format *irs-process-visualizer* *irs-server-service-name*
                 "~%Calling service ~:(~a~)~:[~*~; (~:(~a~))~]~{~{~%  ~(~a~) ~s~}~}"
                 service-name operation-name operation-name input-role-value-pairs)
      (update-images *irs-process-visualizer* service-name 'receiving)
      (iv-format *irs-process-visualizer* service-name
                 "~%Received request~:[~*~; (~:(~a~))~]:~{~{~%  ~(~a~) ~s~}~}"
                 operation-name operation-name input-role-value-pairs)
      (sleep 0.5)
      (update-images *irs-process-visualizer* service-name 'processing))
      (im:send-event im:*session-id-placeholder*
		     (im:create-start-invoke-web-service-event
		      service-name input-role-value-pairs))))

(defun result-of-remote-procedure-call (method-instance result)
  (let ((service-name (iu:get-service-name method-instance)))
    (when *irs-process-visualizer*  
      (update-images *irs-process-visualizer* service-name 'sending)
      (iv-format *irs-process-visualizer* service-name
                 "~%Sending result:~%~a~%" result)
      (update-images *irs-process-visualizer* *irs-server-service-name* 
                     'receiving)
      (iv-format *irs-process-visualizer* *irs-server-service-name*
                 ;;"~%Receiving the result of the service ~:(~a~)~%~a~%"
                 "~%Result:~%~a~%"
                 result)
      (sleep 0.5)
      (update-images *irs-process-visualizer* *irs-server-service-name* 'idle)
      (update-images *irs-process-visualizer* service-name 'idle))
    (im:send-event im:*session-id-placeholder*
		   (im::create-end-invoke-web-service-event service-name result))))
