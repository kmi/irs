(in-package #:irs.monitoring)

;;; XXX This should be replaced by a session ID that is unique for
;;; each achieve-goal request.  This will probably be best done by
;;; using a special variable rather than a parameter to the function,
;;; but we'll leave it as it is until someone actually implements it.
(defvar *session-id-placeholder* "session-id-placeholder"
  "Placeholder for a real session-ID.")

;; TODO Implement this for logging SOAP and XML messages locally
(defun log-locally (what)
  (declare (ignore what))
  'message-pointer)

(defun get-monitoring-observers (session-id)
  ;; Fake for now and just retrieve the only observer we have
  ;;  (web-onto::findany '?x `(and (ocml::monitoring-observer ?x) (ocml::has-session-id ?x ,session-id))))
  (declare (ignore session-id))
  (ocml:with-ontology ('ocml::monitoring-ontology)
    (ocml::setofall '?x `(ocml::monitoring-observer ?x))))


(defun get-monitoring-observer-callback (monitor-instance)
  (when monitor-instance
    (ocml:with-ontology ('ocml::monitoring-ontology)
      (web-onto::findany '?x `(ocml::has-callback ,monitor-instance ?x)))))

(defun create-monitoring-observer (session-id callback)
  "Create a monitoring observer instance given a SESSION-ID and a CALLBACK."
  (ocml:with-ontology ('ocml::monitoring-ontology)
    (let ((instance-name (intern (symbol-name (gensym "MONITOR-TEST-")) (find-package "OCML"))))
      (ocml::name (ocml::define-domain-instance
                      instance-name 'ocml::monitoring-observer ""
                      `((ocml::has-session-id ,session-id)
                        (ocml::has-callback ,callback)))))))

(defun delete-monitoring-observer (monitor)
  "Delete MONITOR."
  (ocml:with-ontology ('ocml::monitoring-ontology)
    (ocml::delete-ocml-object 'ocml::instance monitor)))

(defun universal-time-to-list (x)
  "Date and time conversion."
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time x)
    (list seconds minutes hours date month year)))

(defun send-event (session-id event-instance)
  "Send EVENT-INSTANCE to the appropriate monitors."
  (ocml:with-ontology ('ocml::see-events)
    (let ((event-name (ocml::name event-instance)))
      (ocml::set-slot-value event-name 'ocml::has-session-id session-id))
    (mapcar #'(lambda (monitor)
                (let ((callback (get-monitoring-observer-callback monitor)))
                  (when callback
                    (funcall callback monitor event-instance))))
            (get-monitoring-observers session-id))))

(defun generate-data-value-instance (input-role-value-pair)
  "Take an input and generate a data-value instance."
  (let ((instance-name (intern (symbol-name (gensym "DATA-VALUE-")) (find-package "OCML"))))
    (ocml::define-domain-instance 
     instance-name 'ocml::data-value
     ""
     `((ocml::has-slot-name ,(car input-role-value-pair))
      (ocml::has-value ,(cadr input-role-value-pair))))))

(defun addTimestamp (event-instance)
  "Add the timestamp to the event instance"
  (ocml:with-ontology ('ocml::see-events)
    (ocml::set-slot-value (ocml::name event-instance) 
                          'ocml::has-timestamp 
                          (universal-time-to-list (get-universal-time)))))

;; Add generated-by
(defun addGeneratedBy (event-instance)
  (ocml:with-ontology ('ocml::see-events)
    (ocml::set-slot-value (ocml::name event-instance) 
                          'ocml::generated-by 
                          "IRS")))


;; Generate a start-achieve-goal-event
;; Ignore the session id for now
(defun create-start-achieve-goal-event (goal-name input-role-value-pairs)
  ;;  (setf my-goal goal-name)
  ;;  (setf my-inputs input-role-value-pairs)
  (ocml:with-ontology ('ocml::see-events)
    (let* ((instance-name (intern (symbol-name (gensym "START-ACHIEVE-GOAL-EVENT-")) (find-package "OCML")))
           (event-instance (ocml::define-domain-instance 
                            instance-name 'ocml::start-achieve-goal-event
                            ""
                            `((ocml::has-goal ,goal-name)))))
      (addTimestamp event-instance)
      (addGeneratedBy event-instance)
    
      (dolist (input-role-value-pair input-role-value-pairs) 
        (ocml::add-slot-value 
         (ocml::name event-instance) 
         'ocml::has-input-data 
         (ocml::name (generate-data-value-instance input-role-value-pair))))
      ;; return the instance
      event-instance)))
    

;; Generate an end-achieve-goal-event
;; Ignore the session id for now
(defun create-end-achieve-goal-event (goal-name output-value)
  (ocml:with-ontology ('ocml::see-events)
    (let* ((instance-name (intern (symbol-name (gensym "END-ACHIEVE-GOAL-EVENT-")) (find-package "OCML")))
           (event-instance (ocml::define-domain-instance 
                            instance-name 'ocml::end-achieve-goal-event
                            ""
                            `((ocml::has-goal ,goal-name)))))
      (addTimestamp event-instance)
      (addGeneratedBy event-instance)
    
      ;;(ocml::add-slot-value (ocml::name event-instance) 'ocml::has-output-data output-value)
      ;; TODO Replace 'result with the proper name of the output-role
      (if (and (stringp output-value)
               (search "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" output-value))
          (let ((file-name (log-locally output-value)))
            (ocml::add-slot-value 
             (ocml::name event-instance) 
             'ocml::has-output-data 
             (ocml::name (generate-data-value-instance (list "message-logged-locally" file-name)))))

        ;;Its not XML
        (ocml::add-slot-value 
         (ocml::name event-instance) 
         'ocml::has-output-data 
         (ocml::name (generate-data-value-instance (list 'result output-value)))))
      ;; return the instance
      event-instance)))

;; Generate a start-achieve-goal-event
;; Ignore the session id for now
(defun create-start-invoke-web-service-event (service-name input-role-value-pairs)
  ;;  (setf my-goal goal-name)
  ;;  (setf my-inputs input-role-value-pairs)
  (ocml:with-ontology ('ocml::see-events)
    (let* ((instance-name (intern (symbol-name (gensym "START-INVOKE-WEB-SERVICE-EVENT-")) (find-package "OCML")))
           (event-instance (ocml::define-domain-instance 
                            instance-name 'ocml::start-invoke-web-service-event
                            ""
                            `((ocml::has-web-service ,service-name)))))
    
      (addTimestamp event-instance)
      (addGeneratedBy event-instance)

      (dolist (input-role-value-pair input-role-value-pairs) 
        (ocml::add-slot-value 
         (ocml::name event-instance) 
         'ocml::has-input-data 
         (ocml::name (generate-data-value-instance input-role-value-pair))))
      event-instance)))

;; Generate an end-achieve-goal-event
;; Ignore the session id for now
(defun create-end-invoke-web-service-event (service-name output-value)
  (ocml:with-ontology ('ocml::see-events)
    (let* ((instance-name (intern (symbol-name (gensym "END-INVOKE-WEB-SERVICE-EVENT-")) (find-package "OCML")))
           (event-instance (ocml::define-domain-instance 
                            instance-name 'ocml::end-invoke-web-service-event
                            ""
                            `((ocml::has-web-service ,service-name)))))
    
      (addTimestamp event-instance)
      (addGeneratedBy event-instance)

      ;; TODO Replace 'result with the proper name of the output-role
      (if (and (stringp output-value)
               (search "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" output-value))
          (let ((file-name (log-locally output-value)))
            (ocml::add-slot-value 
             (ocml::name event-instance) 
             'ocml::has-output-data 
             (ocml::name (generate-data-value-instance (list "message-logged-locally" file-name)))))

        ;; Its not XML
        (ocml::add-slot-value 
         (ocml::name event-instance) 
         'ocml::has-output-data 
         (ocml::name (generate-data-value-instance (list 'result output-value)))))
      event-instance)))
