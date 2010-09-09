(in-package irs-protocol)

;; XXX. Hack.  Lived in soap-irs/wenjin-irs.lisp, but that gets loaded
;; after the first reference, which is in this subsystem.
(defvar ocml::*default-irs-type* 'ocml-expression)

(defvar ocml::*condition-flag* "CONDITION")

;;;condition names are: ontology-not-found

(defvar ocml::*psm-soap-function-name-start*
  "RUN-")

(defvar ocml::*psm-soap-function-name-package*
  "CL-USER")

(defvar ocml::*last-irs-message* nil
  "Useful for debugging purposes")

(defvar ocml::*irs-server-generated-code-directory*
  (translate-logical-pathname "irs:src;irs-server-generated-code;"))

(defmethod http::http-reply ((method (eql :irs)) request)
  ;;(setf xrr request)
  (ocml::with-ocml-thread-safety ()
    (let ((upcase-request (string-upcase request))
          (*package* (find-package "IRS-PROTOCOL"))
          action)
      (handler-case
          (with-input-from-string (istream upcase-request)
            (setf action (read istream))
            (handle-irs-request 
             action http::*http-stream* 
             (string-trim '(#\space #\tab)
                          (subseq request 
                                  (length (symbol-name action))))))
        (ocml::no-role-value-error
         (c)
         (http::send-error-message 
          http::*http-stream* 
          c))
        (serious-condition 
         (c)
         (http::send-error-message http::*http-stream* 
                                   c))
        (error 
         (c)
         (http::send-error-message http::*http-stream* 
                                   c))))))

(defmethod handle-irs-request (action html-stream request)
  (format html-stream "Unknown action ~a~%" action))

(defun create-ocml-symbol (string)
  (intern (substitute #\- #\space (string-trim '(#\space #\tab) string))
          (find-package "OCML")))

(defmethod handle-irs-request ((action (eql 'show-details-with-type))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology type name)     
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            type (read istream)
            name (read istream))
      (show-details-with-type html-stream ontology type name))))

(defmethod handle-irs-request ((action (eql 'show-details))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology name)     
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            name (read istream))
      (show-details html-stream ontology name))))      

(defmethod handle-irs-request ((action (eql 'get-all-domain-ontologies))
                               html-stream request)
  (declare (ignore action request))
  (all-domain-ontologies html-stream))

(defmethod handle-irs-request ((action (eql 'get-all-tasks))
                               html-stream request)
  (declare (ignore action request))
  (all-tasks html-stream))

(defmethod handle-irs-request ((action (eql 'get-applicable-psms))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology task)
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            task (read istream)))
    (applicable-psms html-stream ontology task)))


(defmethod handle-irs-request ((action (eql 'get-task-subtasks))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
        (*package* (find-package "OCML"))
        (task (read-from-string upcase-request)))
    (task-subtasks html-stream task)))

(defmethod handle-irs-request ((action (eql 'get-psm-subpsms))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
        (*package* (find-package "OCML"))
        (ontology-name (read-from-string upcase-request)))
    (declare (ignore ontology-name))))

(defmethod handle-irs-request ((action (eql 'combine-domain-task-models))
                               html-stream request)
   (let* ((upcase-request (string-upcase request))
          (*package* (find-package "OCML"))
          ontology task)     
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            task (read istream))
      (combine-domain-task-models html-stream ontology task))))

(defmethod handle-irs-request ((action (eql 'combine-psm-with-domain-task-model))
                               html-stream request)
   (let* ((upcase-request (string-upcase request))
          (*package* (find-package "OCML"))
          domain-task-model psm)     
    (with-input-from-string (istream upcase-request)
      (setf domain-task-model (read istream)
            psm (read istream))
      (combine-psm-with-domain-task-model html-stream domain-task-model psm))))

(defmethod handle-irs-request ((action (eql 'get-task-input-roles))
                               html-stream request)
   (let* ((upcase-request (string-upcase request))
          (*package* (find-package "OCML"))
          (task (read-from-string upcase-request)))
     (get-task-input-roles html-stream task)))

(defmethod handle-irs-request ((action (eql 'get-method-input-roles))
                               html-stream request)
   (let* ((upcase-request (string-upcase request))
          (*package* (find-package "OCML"))
          (method (read-from-string upcase-request)))
     (get-method-input-roles html-stream method)))

(defmethod handle-irs-request ((action (eql 'create-application))
                               html-stream request)
   (let* ((upcase-request (string-upcase request))
          (*package* (find-package "OCML")))
     (with-input-from-string (istream upcase-request)
        (let ((task-ontology (read istream))
              (method-ontology (read istream))
              (domain-ontology (read istream))
              (task-type (read istream))
              (method-type (read istream))
              (key-parameters (read istream))
              (task-input (read istream))
              (method-input (read istream)))
          (create-application html-stream task-ontology method-ontology 
                              domain-ontology task-type method-type key-parameters
                              task-input method-input)))))

(defmethod handle-irs-request ((action (eql 'get-instances))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology class)
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            class (read istream)))
    (get-instances html-stream ontology class)))

(defmethod handle-irs-request ((action (eql 'get-slots-with-type))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology name)
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            name (read istream)))
    (get-slots-with-type html-stream ontology name)))


(defmethod handle-irs-request ((action (eql 'create-mapping-To-Class-Hierarchy))
                               html-stream request)
  ;;(setf rr request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology top-classes name)     
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream) 
            top-classes (read istream) 
            name (read istream)))
    (create-mapping-to-class-hierarchy html-stream ontology top-classes name)))


(defmethod handle-irs-request ((action (eql 'ontology-top-classes))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         (ontology (read-from-string upcase-request)))
    (format html-stream "~(~{~a ~}~)~%" (ocml::local-top-classes ontology))))

(defmethod handle-irs-request ((action (eql 'create-class-instance))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology class slot-value-pairs)  
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream) 
            class (read istream) 
            slot-value-pairs (read istream)))
    (create-class-instance html-stream ontology class slot-value-pairs)))

(defmethod handle-irs-request ((action (eql 'create-named-class-instance))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology class slot-value-pairs instance-name)     
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream) 
            class (read istream) 
            instance-name (read istream)
            slot-value-pairs (read istream)))
    (create-class-instance html-stream ontology class slot-value-pairs instance-name)))

(defmethod handle-irs-request ((action (eql 'run-application))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         application-ontology application-function key-parameters)     
    (with-input-from-string (istream upcase-request)
      (setf application-ontology (read istream) 
            application-function (read istream) 
            key-parameters (read istream)))
    (run-application html-stream application-ontology application-function key-parameters)))

(defmethod handle-irs-request ((action (eql 'call-remote-irs-method))
                               html-stream request)
  (let* ((*package* (find-package "OCML"))
         ontology method-name arguments)
    (with-input-from-string (istream request)
      (setf ontology (read istream) 
            method-name (read istream)
            arguments (read istream))
      (format html-stream "~s~%" 
              (invoke-remote-irs-method ontology method-name arguments)))))

(defmacro with-ontology-check ((ontology stream) &rest body)
  `(cond ((ocml::get-ontology ,ontology)
          ,@body)
         (t (warn-ontology-does-not-exist ,ontology ,stream))))

#+lispworks
(editor::setup-indent 'with-ontology-check 0 2)

(defun warn-ontology-does-not-exist (ontology stream)
  (format stream "~a ~a ~a~%" 
                      ocml::*condition-flag*
                      'ontology-not-found
                      ontology))

#|chnaged from corba version
(defmethod handle-irs-request ((action (eql 'ontology-methods))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology)
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream))
      (with-ontology-check (ontology html-stream)
        (format html-stream "~{~{~(~a~) ~{~(~a~) ~a~}%~}~}~%" 
                (ontology-methods ontology))))))
|#


(defmethod handle-irs-request ((action (eql 'ontology-items))
                               html-stream request)
  ;;(setf rr request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology item-types
         (view-only-items-in-current-ontology t))
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            view-only-items-in-current-ontology
            (web-onto::java-true-p (read istream))
            item-types (read istream)
            )
      (with-ontology-check (ontology html-stream)
        (format html-stream "~(~{~{~a ~} %%% ~{~a ~} %%% ~{~a ~}~}~)~%"
                (ontology-items
                 ontology 
                 view-only-items-in-current-ontology
                 item-types))))))

(defmethod handle-irs-request ((action (eql 'ontology-methods))
                               html-stream request)
  ;;(setf rr request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology 
         (view-only-items-in-current-ontology t))
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            view-only-items-in-current-ontology
            (web-onto::java-true-p (read istream))
            )
      (with-ontology-check (ontology html-stream)
        (format html-stream "~(~{~a ~}~)~%"
                (ontology-methods 
                 ontology 
                 view-only-items-in-current-ontology))))))

(defmethod handle-irs-request ((action (eql 'ontology-tasks))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology 
         (view-only-items-in-current-ontology t))
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            view-only-items-in-current-ontology
            (web-onto::java-true-p (read istream))
            )
      (with-ontology-check (ontology html-stream)
        (format html-stream "~(~{~a ~}~)~%"
                (ontology-tasks 
                 ontology 
                 view-only-items-in-current-ontology))))))


;;;now hack this for the ISWC 2007 tutorial that we *always* return only local
;;;items - john domingue october 2007
(defmethod handle-irs-request ((action (eql 'ontology-goals))
                               html-stream request)
  ;;(setf rr request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology 
         (view-only-items-in-current-ontology t))
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            view-only-items-in-current-ontology
            (web-onto::java-true-p (read istream))
            )
      (with-ontology-check (ontology html-stream)
        (format html-stream "~(~{~a ~}~)~%"
                (ontology-goals 
                 ontology 
                 ;;view-only-items-in-current-ontology
                 t))))))

;;;now hack this for the ISWC 2007 tutorial that we *always* return only local
;;;items - john domingue october 2007
(defmethod handle-irs-request ((action (eql 'ontology-web-services))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology 
         (view-only-items-in-current-ontology t))
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            view-only-items-in-current-ontology
            (web-onto::java-true-p (read istream))
            )
      (with-ontology-check (ontology html-stream)
        (format html-stream "~(~{~a ~}~)~%"
                (ontology-web-services
                 ontology 
                 ;;view-only-items-in-current-ontology
                 t))))))

;;;now hack this for the ISWC 2007 tutorial that we *always* return only local
;;;items - john domingue october 2007
(defmethod handle-irs-request ((action (eql 'ontology-mediators))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology 
         (view-only-items-in-current-ontology t))
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            view-only-items-in-current-ontology
            (web-onto::java-true-p (read istream))
            )
      (with-ontology-check (ontology html-stream)
        (format html-stream "~(~{~a ~}~)~%"
                (ontology-mediators
                 ontology 
                 ;;view-only-items-in-current-ontology
                 t))))))

(defmethod handle-irs-request ((action (eql 'task-methods))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology task)
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            task (read istream))
      (with-ontology-check (ontology html-stream)
        (task-methods ontology task html-stream)))))

(defmethod handle-irs-request ((action (eql 'method-details))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology method)
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            method (read istream))
      (with-ontology-check (ontology html-stream)
        (method-details ontology method html-stream)))))


(defmethod handle-irs-request ((action (eql 'task-details))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology task)
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            task (read istream))
      (with-ontology-check (ontology html-stream)
        (task-details ontology task html-stream)))))

(defmethod handle-irs-request ((action (eql 'goal-details))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology goal)
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            goal (read istream))
      (with-ontology-check (ontology html-stream)
        (goal-details ontology goal html-stream)))))

(defmethod handle-irs-request ((action (eql 'web-service-details))
                               html-stream request)
  ;;(setf rr request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology web-service)
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            web-service (read istream))
      (with-ontology-check (ontology html-stream)
        (web-service-details ontology web-service html-stream)))))

(defmethod handle-irs-request ((action (eql 'mediator-details))
                               html-stream request)
  (let* ((upcase-request (string-upcase request))
         (*package* (find-package "OCML"))
         ontology mediator)
    (with-input-from-string (istream upcase-request)
      (setf ontology (read istream)
            mediator (read istream))
      (with-ontology-check (ontology html-stream)
        (mediator-details ontology mediator html-stream)))))

(defmethod handle-irs-request ((action (eql 'achieve-task))
                               html-stream request)
  (let ((upcase-string (string-upcase request)))
    (with-input-from-string (istream upcase-string)
      (let* ((*package* (find-package "OCML"))
             (ontology (read istream))
             (task-type (read istream))
             (input-role-value-pairs (read istream)))
        (with-ontology-check (ontology html-stream)
          (irs-achieve-task ontology task-type input-role-value-pairs 
                            html-stream))))))
      


;;irs upload ontology ontology-uses ontology-type ontology-format 
;;url new/overwrite/append 
;;author password allowed-editors

;;;test with 
;;(irs-http-client :host "villapark.open.ac.uk" :request "irs upload foo kmi-planet-kb :domain ocml \"http://pckm070.open.ac.uk/upload-test.html\" new \"john\" \"bwraf1\" enrico")

;;(handle-irs-request 'upload *standard-output* "foo (kmi-planet-kb) :domain ocml \"http://akt.open.ac.uk/upload-test.html\" new \"john\" \"bwraf1\" enrico")
;;

(defun upcase-symbol (x package-name)
  (intern (string-upcase (symbol-name x))
          (find-package package-name)))

(defmethod handle-irs-request ((action (eql 'upload))
                               html-stream request)
  (with-input-from-string (istream request)
    (let* ((*package* (find-package "OCML"))
           (knowledge-model (read istream))
           (knowledge-model-uses (read istream))
           (knowledge-model-type (read istream))
           (knowledge-model-format (read istream))
           (url-string (read istream))
           (upload-mode (read istream))
           (author (read istream))
           (password (read istream))
           (allowed-editors (read istream)))             
      (upload (upcase-symbol knowledge-model "OCML")
              (mapcar #'(lambda (x)
                          (upcase-symbol x "OCML"))
                      knowledge-model-uses)
              (upcase-symbol knowledge-model-type "KEYWORD") 
              (upcase-symbol knowledge-model-format "OCML")
              url-string (upcase-symbol upload-mode "OCML")
              author
              password allowed-editors 
              html-stream))))
