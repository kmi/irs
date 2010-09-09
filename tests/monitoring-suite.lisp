(in-package :irs.tests)

(def-suite monitoring-suite
  :description "Tests for IRS monitoring subsystem.")

(in-suite monitoring-suite)

;;; XXX This is the worst test in the world :-(

(eval-when (:compile-toplevel :load-toplevel)
 (ocml:register-namespace
  "math" "http://www.kmi.open.ac.uk/projects/irs/math-ontology#"))

(test monitoring-test
  (is-true (> (length 
               (with-output-to-string (monitor-stream)
                 (let ((monitor (im:create-monitoring-observer
                                 im::*session-id-placeholder*
                                 (irs.api.soap-old::new-monitoring-callback monitor-stream))))
                   (with-output-to-string (http-stream)
                     (ip::raw-irs-achieve-goal 'ocml::math-ontology
                                               '#_math:add-goal
                                               '((#_math:hasA 3)
                                                 (ocml::has-b 10))
                                               http-stream nil t))
                   (im:delete-monitoring-observer monitor))))
               100))
  (is-true (null (irs.monitoring::get-monitoring-observers 'ignored-value))))

;;; Moved from irs-monitoring/utilities.lisp.  It's less a test of
;;; send-event than a test of recipients.  
(defun test-send-event
(session-id event-instance)
  (ocml:with-ontology ('ocml::see-events)
    (let* ((observer-port 9090)
           (observer-host "137.108.24.146")
           (monitor-stream (comm::open-tcp-stream observer-host observer-port))
           (event-name (ocml::name event-instance))
           (inst-class (ocml::get-ocml-class (ocml::findany '?c `(= ?c (ocml::the-parent ,event-name)))))
           (slots (iu::get-domain-slot-names inst-class)))
      (ocml::set-slot-value event-name 'ocml::has-timestamp
                            (im::universal-time-to-list (get-universal-time)))
      (ocml::set-slot-value event-name 'ocml::has-session-id 
                            session-id)
      (iu::send-soap-response2
       "MONITORING-EVENT"
       (list (iu::generate-instance-information 'ocml::see-events event-name slots t))
       '((result "string"))
       :stream monitor-stream))))
