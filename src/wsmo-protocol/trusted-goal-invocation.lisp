(in-package #:wsmo-protocol)

;;; Define dummy functions here so we don't get warnings at compile
;;; time.  The real definitions are in the
;;; trust-heuristic-classification ontology.
(defun ocml::get-observables-from-participant ()
  (error "Dummy function.  Load trust-heuristic-classification function."))

(defun ocml::get-requirement-from-participant ()
  (error "Dummy function.  Load trust-heuristic-classification function."))

(defun ocml::classify-from-observables ()
  (error "Dummy function.  Load trust-heuristic-classification function."))

(defun get-web-service-classes-list (goal-ws-ontology-list)
  (mapcar #'(lambda (gwo)
	      (ocml::the-parent-class (second gwo)))
	  goal-ws-ontology-list))

(defun classify-list-ws-instance-from-trust-user-instance (trust-user-instance list-of-ws)
  (let ((list-of-good-ws nil)
	(user-profile (ocml::get-requirement-from-participant
		       (ocml::the-parent-class trust-user-instance))))
    (loop for ws in list-of-ws
       do (let* ((observable-list (ocml::get-observables-from-participant
				   (ocml::the-parent-class (second ws))))
		 (profile-of-ws (ocml::classify-from-observables observable-list)))
	    (when (eql (first profile-of-ws) user-profile)
	      (push ws list-of-good-ws))))
    (reverse list-of-good-ws)))

(defun ip::trusted-internal-solve-goal (trust-user-instance ontology goal-type role-value-pairs 
                                                            &optional (call-strategy :first))
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((applicable-web-services nil)
          (trusted-applicable-web-services nil)
          (result nil)
          (output nil))
      (setf applicable-web-services (ws-functional-selection ontology goal-type role-value-pairs))
      (setf trusted-applicable-web-services (classify-list-ws-instance-from-trust-user-instance trust-user-instance applicable-web-services))
      (setf result 
            (when trusted-applicable-web-services
              (case call-strategy
                ((:first) (invoke-service (third (car trusted-applicable-web-services)) ;;ontology ;;goal-type
                                          (second (car trusted-applicable-web-services))))
                ((:all) (mapc #'(lambda (trusted-applicable-web-service)
                                  (invoke-service (third trusted-applicable-web-service) ;;ontology ;;goal-type
                                                  (second trusted-applicable-web-service)))
                              trusted-applicable-web-services)))))
      (push (first (list (format nil "~% Applicable Web Services: ~{~%<br/>~a~} ~%<br/><br/> The WS class that matches with ~a trust requirements is : ~a~%<br/><br/>The result is: ~%<br/> ~a" (get-web-service-classes-list applicable-web-services) trust-user-instance (first (get-web-service-classes-list trusted-applicable-web-services)) result))) output) 
      (when (boundp '*achieve-goal-results*)
        (push (list (list  ontology goal-type) result) *achieve-goal-results*))
      output)))


(defun ip::raw-trusted-irs-achieve-goal (trust-user-instance ontology goal-type input-role-value-pairs 
                                                             html-stream soap-response-p &optional http-request-p)
  (ocml:with-ontology (ontology)
    (setf input-role-value-pairs 
          (re-order-input-role-value-pairs-from-goal goal-type input-role-value-pairs))
    (trusted-irs-solve-goal trust-user-instance ontology goal-type input-role-value-pairs html-stream soap-response-p http-request-p)))

(defun ip::trusted-irs-achieve-goal (trust-user-instance ontology goal-type input-role-value-pairs 
                                                         html-stream soap-response-p &optional http-request-p)
  (handler-case
      (ip::raw-trusted-irs-achieve-goal trust-user-instance ontology goal-type input-role-value-pairs 
                                        html-stream soap-response-p http-request-p)
    (iu::soap-response-error
     (condition)
     (send-achieve-goal-error-message2 (iu::has-soap-response condition)
                                       goal-type html-stream))
    (iu::irs-connection-error
     (condition)
     (send-achieve-goal-error-message2 (format nil "Connection problem with host ~a and port ~a"
                                               (iu::has-host condition)
                                               (iu::has-port condition))
                                       goal-type html-stream))
    ((or ocml::no-role-value-error serious-condition error)
     (condition)
     (send-achieve-goal-error-message condition goal-type html-stream))))


(defun trusted-irs-solve-goal (trust-user-instance ontology goal-type input-role-value-pairs 
                               html-stream &optional soap-p http-request-p)
  (let ((*package* (find-package "OCML")))
    #+:irs-lispworks
    (visualiser:received-achieve-task-message
     ontology goal-type input-role-value-pairs)
    (irs.api.javascript:event :goal-call goal-type input-role-value-pairs)
    (let* (;;(*internal-goal-instances* nil)
           (result (ip::trusted-internal-solve-goal trust-user-instance ontology goal-type
                                                    input-role-value-pairs))
           (output-type (get-output-type result))
           (lower-function (grounding:get-lower-function output-type)))
      (irs.api.javascript:event :goal-return goal-type result)
      (when lower-function 
        (setf result (funcall lower-function result)))
      #+:irs-lispworks
      (visualiser:sending-achieve-task-message
       ontology goal-type input-role-value-pairs result)
      (let ((output-type 
             (second (output-role-with-soap-binding goal-type))))
        (if soap-p
            (if (ip::soap-attachment-type-p output-type)
                (ip::send-soap-attachment 
                 (symbol-name goal-type)
                 result `((result ,output-type)) html-stream)
              (iu::send-soap-response2 (symbol-name goal-type)
                                       (list result) 
                                       `((result ,output-type)) 
                                       :stream html-stream)))           
        ;;(setf go goal-output-type lf lower-function rr result)
        (if http-request-p
            (format html-stream "~a" result)
          (format html-stream "~s" result))))))
