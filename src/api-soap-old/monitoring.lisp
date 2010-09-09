;;; Copyright © 2008 The Open University

(in-package #:irs.api.soap-old)

(defun new-monitoring-callback (monitoring-stream)
  ;; Deleting the observer requires that the deleting function have
  ;; access to the stream, so it can send a disconnect message.  We
  ;; accomplish this by overloading the meaning of EVENT-INSTANCE.  If
  ;; it's NIL, we just return the MONITORING-STREAM.
  (lambda (monitor event-instance)
    (unless event-instance
      (values monitoring-stream))
    (let* ((event-name (ocml::name event-instance))
           (inst-class (ocml::get-ocml-class (ocml::findany '?c `(= ?c (ocml::the-parent ,event-name)))))
           (slots (iu::get-domain-slot-names inst-class)))
      (handler-case
          (iu::send-soap-response2
           "MONITORING-EVENT"
           (list (iu::generate-instance-information 'ocml::see-events event-name slots t))
           '((result "string"))
           :stream monitoring-stream)
        (comm::socket-error (c)
          ;; Something went wrong so lets get rid of the monitoring-observer
          (irs.monitoring:delete-monitoring-observer monitor))))))

;;; Unregister an observer that was monitoring
(defmethod ip::handle-post-soap-service
    ((action (eql 'cl-user::delete-observer)) stream namespace soap-values)
  (ocml:with-ontology ('ocml::monitoring-ontology)
    (let* ((*package* (find-package "OCML"))
           (monitoring-observer (ip::make-ocml-symbol (pop soap-values)))
           (monitoring-callback (im::get-monitoring-observer-callback
                                 monitoring-observer))
           (monitoring-stream (funcall monitoring-callback
                                       monitoring-observer nil)))
      (when (ocml::delete-ocml-object 'ocml::instance monitoring-observer)
        (format monitoring-stream "GOOD BYE~%")
        (force-output monitoring-stream)
        (iu::send-soap-response2
         "DELETE-OBSERVER-RESPONSE" (list "Observer correctly removed")
         '((result "string")) :stream stream))
        (iu::send-soap-response2
         "DELETE-OBSERVER-RESPONSE" (list "Error removing observer")
         '((result "string")) :stream stream))))
