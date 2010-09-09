;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defvar *data-segment-terminator* (format nil "$$$~%"))

(defmethod http::http-reply ((method (eql :web-onto)) request)
  (http::handle-web-onto http::*http-stream* request))
		         ;;(after-1st-space-string request)))


(defmethod http::http-reply ((method (eql :alice)) request)
  (http::handle-alice http::*http-stream* request))

(defvar *safe-mode* t)

(defun debug-mode ()
  (http::lw-debug t)
  (setf *safe-mode* nil))

(defun safe-mode ()
  (setf *safe-mode* t))

(defun current-mode-p ()
  *safe-mode*)

(defun http::handle-web-onto (stream request-string)
  (if *safe-mode*
      (handler-case (internal-handle-web-onto stream request-string)
        ;;use serious-condition to catch stack overflows
        (serious-condition (c)
               (handler-case
                   (progn
                     (format t "Error with request ~a of type ~a~%"
                             request-string c)
                     (http::princ-to-binary-stream
                      (format nil "Error with request ~a of type ~a~%"
                              request-string
                              c)
                      stream))
                 ;;use serious-condition to catch stack overflows
                 (serious-condition (c1)
		   (declare (ignore c1))
                        (format t
                                "Error with error handler in handle-web-onto~%")))))
      (internal-handle-web-onto stream request-string)))


(defun http::handle-alice (stream request-string)
    (if *safe-mode*
      (handler-case (internal-handle-alice stream request-string)
        ;;use serious-condition to catch stack overflows
        (serious-condition (c)
               (handler-case
                   (progn
                     (format t "Error with request ~a of type ~a~%"
                             request-string c)
                     (http::princ-to-binary-stream
                      (format nil "Error with request ~a of type ~a~%"
                              request-string
                              c)
                      stream))
                 ;;use serious-condition to catch stack overflows
                 (serious-condition (c1)
		   (declare (ignore c1))
                        (format t
                                "Error with error handler in handle-web-onto~%")))))
      (internal-handle-alice stream request-string)))

(defvar *web-onto-edit-file-size-limit*
  10000000
  "Some webonto files are very large")

(defun internal-handle-web-onto (stream request-string)
  (let* ((*package* (find-package "WEB-ONTO"))
         #+:lispworks
         (*standard-output* mp:*background-trace-output*)
         (info http::*http-info*)
         (upcase-string (string-upcase request-string))
         (action (read-from-string upcase-string))
         (host-name "Unknown")
         (address (http::http-info-addr info))
         (editor::*edit-file-size-limit* *web-onto-edit-file-size-limit*)
         integer-network-address port
         (ocml::*current-ontology* ocml::*current-ontology*)
         (ocml::*current-ontologies* ocml::*current-ontologies*)
         (ocml::*defined-relations* ocml::*defined-relations*)
         (ocml::*axioms* ocml::*axioms*)
         (ocml::*defined-functions* ocml::*defined-functions*)
         (ocml::*bc-rules* ocml::*bc-rules*)
         (ocml::*domain-classes* ocml::*domain-classes*))
    (declare (special editor::*edit-file-size-limit*
                      ocml::*current-ontology* ocml::*current-ontologies*
                      ocml::*defined-relations* ocml::*axioms*
                      ocml::*defined-functions* ocml::*bc-rules*
                      ocml::*domain-classes*))
    (unless (or (eq action 'broadcast) (boundp 'http::*broadcasting-p*))
      #+lispworks
      (setq integer-network-address (comm::socket-stream-peer-address stream))
      #+lispworks
      (setq port (comm::socket-stream-peer-address stream))
      (handler-case
	  (setf host-name
                #+:lispworks
                (comm::get-host-entry integer-network-address :fields '(:name))
                #-lispworks
                "unknown")
        ;;use serious-condition to catch stack overflows
	(serious-condition (c) (format t "Error getting host name for ~a~%" address)))
      ;;(format t "address ~a port ~a, host name ~a~%" address port host-name)
      )
    ;;(format *standard-output* "handle request ~a~%" upcase-string)
    (case action
      ((display) (display-ocml-action stream upcase-string))
      ((dynamic_query) (evaluate-dynamic-query stream upcase-string))
      ((reinitialise) (reinitialise-ocml stream))
      ((direct_instances) (direct-instances stream upcase-string))
      ((all_instances) (all-instances  stream upcase-string))
      ((direct_instances_with_class_names)
       (direct-instances-with-class-names stream upcase-string))
      ((all_instances_with_class_names)
       (all-instances-with-class-names stream upcase-string))
      ((load-ontology) (load-ontology stream upcase-string))
      ((view-ontology) (view-ontology stream upcase-string))
      ((start_broadcasting)
         ;(multiple-value-call 'http::add-broadcaster
;	   (get-receiver request-string))

       (http::add-broadcaster host-name port address))
      ((lock_ontology) (lock-ontology-and-start-editing-session stream request-string))
      ((unlock_ontology) (unlock-ontology stream upcase-string))
      ((locked_ontology_p) (locked-ontology-p stream upcase-string))
      ((stop_broadcasting)
       (setf (http-info-fd http::*http-info*) :stop-broadcasting))
      ((broadcast) (broadcast (broadcast-message request-string))) ;;coord
      ((receive)
         ;(multiple-value-call 'http::add-receiver
;	   (get-receiver request-string))
       (http::add-receiver host-name port address))
      ((stop_receiving)
         ;(multiple-value-call 'http::remove-receiver
;	   (get-stop-receiver request-string))
       (http::remove-receiver host-name (get-stop-receiver-port request-string))
       )
      ((login) (login stream request-string))
      ((change-password) (change-password stream request-string))
      ((new-ontology) (new-ontology stream request-string))
      ((delete-ontology) (delete-ontology stream request-string))
      ((get_ontology_properties) (get-ontology-properties stream request-string))
      ((get_ontology_type) (get-ontology-of-type stream request-string))
      ((update_ontology_properties) (update-ontology-properties stream request-string))
      ((reload_ontology) (reload-ontology stream request-string))
      ((local_slots_p) (local-slots-p stream upcase-string))
      ((class_p) (class-p stream upcase-string))
      ((end_emacs_connection) (end-emacs-connection))
      ((new_class_parent) (new-class-parent stream upcase-string))
      ((new_instance_class) (new-instance-class stream upcase-string))
      ((new_instance_slot_value) (new-instance-slot-value stream upcase-string))
      ((delete_items) (delete-items stream upcase-string))
      ((get_source) (get-source stream upcase-string))
      ((get_instance_source) (get-instance-source stream upcase-string))
      ((get_diagrams) (get-diagrams stream upcase-string))
      ((open_diagram) (open-diagram stream upcase-string))
      ((save_diagram) (save-diagram stream upcase-string))
      ((get_ontology_source_file_names)
       (get-ontology-source-file-names stream upcase-string))
      ((display_source_file) (display-source-file stream upcase-string))
      ((export_ontolingua) (export-ontolingua stream upcase-string))
      ((get_all_ontologies) (get-all-ontologies stream request-string))
      ((get_all_ontologies_of_type) (get-all-ontologies-of-type stream request-string))
      ((get_ontologies_owned_by) (get-ontologies-owned-by stream request-string))
      ((get_editable_ontologies) (get-editable-ontologies stream request-string))
      ((class_parents) (class-parents stream upcase-string))
      ((class_slots) (get-class-slots stream upcase-string))
      ((same_class_instance_examples) (same-class-instance-examples stream upcase-string))
      ((other_class_instance_examples) (other-class-instance-examples stream upcase-string))
      ((class_info_for_new_instance) (class-info-for-new-instance stream upcase-string))
      ((slot_default_values)
       (slot-default-values stream upcase-string))
      ((instance_info_for_edit) (instance-info-for-edit stream upcase-string))
      ((display_connected_instances)
       (display-connected-instances stream upcase-string))
      ((check_instances)
       (check-instances stream upcase-string))
      ((class_children) (class-children stream upcase-string))
      ((class_types) (class-types stream upcase-string))
      ((class_default_values) (class-default-values stream upcase-string))
      ;;class_instances gets the class instances contained within a list of
      ;;nodes. class_instances2 gets the instances of a class
      ((class_instances) (class-instances stream upcase-string))
      ;;i think that class-instances might work
      ((class_instances2) (class-instances stream upcase-string))
      ;;all_class_instances returns a straight list of the class instances
      ;;all_instances returns a structured list
      ((all_class_instances) (all-class-instances stream upcase-string))
      ((generalised_all_class_instances)
       (generalised_all-class-instances stream upcase-string))
      ;;((class_descendents_except_type) (class-descendents-except-type stream upcase-string))
      ((slot_types_except_type) (slot-types-except-type stream upcase-string))
      ((lois_query) (lois-query stream upcase-string))
      ((lois_indexes) (lois-indexes stream upcase-string))
      ((index_aspects) (return-index-aspects stream upcase-string))
      ((instance_default_value_of) (instance-default-value-of stream upcase-string))
      ((class_type_of) (class-type-of stream upcase-string))
      ((instance_class_name) (instance-class-name stream upcase-string))
      ;;use original source NOT upcased version
      ((new_source) (new-source stream request-string))
      ;;use original source NOT upcased version
      ((new_definition) (new-definition stream request-string))
      ((upload) (upload-ontology stream request-string))
      ((undo_edit) (undo-edit stream upcase-string))
      ((class_info) (get-detailed-class-info stream upcase-string))
      ((slot_options) (slot-options stream upcase-string))
      ((axiom_info) (get-detailed-axiom-info stream upcase-string))
      ((relation_schema) (get-relation-schema stream upcase-string))
      ((relation_info) (get-detailed-relation-info stream upcase-string))
      ((function_info) (get-detailed-function-info stream upcase-string))
      ((procedure_info) (get-detailed-function-info stream upcase-string))
      ((rule_info) (get-detailed-rule-info stream upcase-string))
      ((instance_info) (get-detailed-instance-info stream upcase-string))
      ((ontology_info) (get-detailed-ontology-info stream upcase-string))
      ((run) (run stream upcase-string))
      ((get_mail) (get-mail stream))
      ((draw-tree) (draw-tree stream upcase-string))
      ((draw) (draw stream upcase-string))
      ((generate_database_table_file) (generate-database-table-file stream upcase-string))
      ((generate_database_table_on_remote_host)
       (generate-database-table-on-remote-host stream upcase-string))
      ((create_web_table) (http::create-web-table stream upcase-string))
      ((find_object) (find-object stream upcase-string))
      ((personal-planet)
       (create-personal-planet stream request-string))
      ((save-planet-profile)
       (save-planet-profile stream request-string))
      ((get-planet-profile)
       (get-planet-profile stream request-string))
      ((get-planet-password)
       (get-planet-password stream request-string))
      ((save-planet-password)
       (save-planet-password stream request-string))
      ;;((email-parser-statistics-results)
       ;;(email-parser-statistics-results stream upcase-string))
       ((flash) (flash stream upcase-string))
       ((ocml_definition_from_text)
        (ocml::ocml-definition-from-text stream request-string))
       ((defined_template_p)
        (defined-template-p stream upcase-string))
       ((get_patterns)
        (get-patterns stream upcase-string))
      (t (unknown-web-onto-action action stream)))))


(defun internal-handle-alice (stream request-string)
  (let* ((*package* (find-package "WEB-ONTO"))
         (upcase-string (string-upcase request-string))
         (action (read-from-string upcase-string)))
    (case action
      ((matchmaker) (output-rel-item-message stream request-string))
      ((money_saver) (output-package-deal-message stream request-string))
      ((organic) (output-organic-message stream))
      ((luxury) (output-missed-items-message stream))
      ((party) (output-party-message stream))
      ((party2) (output-party-message-two stream))
      (t (unknown-web-onto-action action stream)))))


(defun output-package-deal-message (stream agent)
  (declare (ignore agent))
  (http::princ-to-binary-stream (http::get-package-deal-message) stream))

(defun output-rel-item-message (stream agent)
  (http::princ-to-binary-stream (http::get-related-item-message) stream))

(defun output-organic-message (stream)
  (http::princ-to-binary-stream (http::get-organic-message) stream))

(defun output-missed-items-message (stream)
  (http::princ-to-binary-stream (http::get-missed-items-message) stream))

(defun output-party-message (stream)
  (http::princ-to-binary-stream (http::get-party-message) stream))

(defun output-party-message-two (stream)
  (http::princ-to-binary-stream (http::get-party-message-two) stream))

(defun unknown-web-onto-action (action stream)
  (format t "~%Unknown web-onto action ~a" action)
  (http::princ-to-binary-stream (format nil "Unknown web-onto action ~a~%" action)
                                stream))

(defun display-ocml-action (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let (ontology-name)
      (let ((*package* (find-package "OCML")))
        (setf ontology-name (read istream)))
      (let ((*package* (find-package "WEB-ONTO")))
        (ocml::select-ontology ontology-name)
        (funcall (read istream) stream upcase-string)))))

(defun safe-setofall (exp goal)
  (let ((res nil))
    (handler-case
        (setf res (eval `(ocml::setofall ',exp ',goal)))
      ;;use serious-condition to catch stack overflows
      (serious-condition (condition)
                         (declare (ignore condition))
                         (setf res 'error)))
    res))

(defun safe-findany (exp goal)
  (let ((res nil))
    (handler-case
        (setf res (eval `(ocml::findany ',exp ',goal)))
      ;;use serious-condition to catch stack overflows
      (serious-condition (condition)
                         (declare (ignore condition))
                         (setf res 'error)))
    res))

(defvar *previous-setofall-results* nil
  "Previous calls to setofall - cache the results")

(defun safe-cache-setofall (exp goal)
  (let ((previous-result (get-previous-setofall-result exp goal)))
    (cond (previous-result)
          (t (let ((result (safe-setofall exp goal)))
               (store-setofall-result exp goal result)
               result)))))

(defun store-setofall-result (exp goal result)
  (push (cons (list exp goal) result) *previous-setofall-results*))

(defun get-previous-setofall-result (exp goal)
  (cdr (assoc (list exp goal) *previous-setofall-results* :test #'equal)))

(defun clear-setofall-results ()
  (setf *previous-setofall-results* nil))
  
(defun dynamic-query-result-types (single-result)
  (mapcar #'(lambda (x) (if (numberp x) "int" "string"))
          single-result))


(defun evaluate-dynamic-query (stream upcase-string)
  ;;(setf u upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read istream))
           (expression (read istream))
           (goal (read istream))
           result)
      (ocml::select-ontology ontology-name)
      ;;safer not to cache results as KB or ontology may change
      ;;(setf result (safe-setofall expression goal))
      ;;use cache for demo purposes
      (setf result (safe-cache-setofall expression goal))
      (if (eq result 'error)
          (http::princ-to-binary-stream (format nil "error: ~S~%" result) stream)
        (http::princ-to-binary-stream 
         (format nil ;;"~d[~{~{~(~a~)#~}@~}[~{~a@~}~%"
                 "~{~{~(~a~)#~}@~}~%"
                 ;;(length expression)
                 result 
                 ;;(dynamic-query-result-types (car result))
                 )
         stream)))))

;;;both forms allowed
(defun ocml-eval (stream upcase-string)
  (let* ((args (car (get-ocml-args upcase-string 1))))
    (eval-ocml-expression args
			  `(ocml::ocml-eval-gen ',args nil) stream)))


(defun ocml_eval (stream upcase-string)
  (let* ((args (car (get-ocml-args upcase-string 1))))
    (eval-ocml-expression args
			  `(ocml::ocml-eval-gen ',args nil) stream)))


(defun tell (stream upcase-string)
  (let* ((args (car (get-ocml-args upcase-string 1))))
    (eval-ocml-expression `(tell ,args)
			  `(ocml::tell1 ',args nil) stream)))

(defun setofall (stream upcase-string)
  (let* ((args (get-ocml-args upcase-string 2))
         (arg1 (car args))
         (arg2 (second args)))
    (eval-ocml-expression `(setofall ,arg1 ,arg2)
			  `(ocml::setofall ',arg1 ',arg2) stream)))

(defun get-ocml-args (upcase-string number-of-args &optional (args-to-skip 3))
  (with-input-from-string (istream upcase-string)
    (dotimes (i args-to-skip)
      (read istream))
    (let* ((*package* (find-package "OCML"))
           (args nil))
      (dotimes (i number-of-args)
        (push (read istream nil nil) args))
      (reverse args))))

(defun parse-string-by-spaces (string)
  (parse-string-by string #\space))

(defun parse-string-by (string char)
  (setf string (string-trim (list char) string))
  (do ((start 0 (and start
                     (> (length string) (1+ start))
		     (find char string :start (1+ start))
		     (1+ (position char string :start (1+ start)))))
       (end (and (find char string)
		 (position char string))
            (and end
                 (> (length string) (1+ end))
                 (find char string :start (1+ end))
                 (position char string :start (1+ end))))
       (parts nil (if start
                      (if end
                          (cons (subseq string start end) parts)
                          (cons (subseq string start) parts))
                      parts)))
      ((null start) (apply #'values (reverse parts)))))

(defun eval-ocml-expression (original-expression expression stream)
  (let ((*package* (find-package "OCML"))
        res output)
    (handler-case
        (progn 
	  (setf output
                (string-downcase
                 (substitute *ocml-line-separator*
                             #\return
                             (substitute *ocml-line-separator*
                                         #\linefeed 
                                         (with-output-to-string
                                             (*standard-output*)
                                           (setf res (eval expression)))))))
	  (http::princ-to-binary-stream
	   (string-downcase
            (format nil "Evaluated ~a to give ~a@~a~%"
                    original-expression output res))
	   stream))
      ;;use serious-condition to catch stack overflows
      (serious-condition (condition)
	     (http::princ-to-binary-stream
	      (format nil "Running ~a I got an error of type ~a~%" expression
		      condition)
	      stream)))))

(defun all-expression-instantiations (expression)
  (let ((envs (ocml::ask-top-level expression :all t :query-mode nil)))
    (unless (eq envs :fail)
      (mapcar #'(lambda (env)
                  (ocml::instantiate expression env))
              envs))))

(defun ask (stream upcase-string)
  (let ((args (car (get-ocml-args upcase-string 1))))
    (eval-ocml-expression `(ask ,args)
                          `(ocml::ask-top-level ',args :all t :query-mode t)
                          stream)))

(defun unassert (stream upcase-string)
  (let ((args (car (get-ocml-args upcase-string 1))))
    (eval-ocml-expression `(ocml::unassert ,args)
                          `(ocml::unassert1 ',args) stream)))

(defun direct-instances (stream upcase-string)
  (internal-get-instances stream upcase-string #'ocml::all-current-direct-instances))

(defun all-instances (stream upcase-string)
  (internal-get-instances stream upcase-string #'ocml::all-current-instances))

(defun internal-get-instances (stream upcase-string instance-function)
  (let ((ontology-name (car (get-ocml-args upcase-string 1 1))))
    (if (get-ontology ontology-name)
        (let ((class-name (car (get-ocml-args upcase-string 1 2))))
          (ocml::select-ontology ontology-name)
          (if (ocml::get-ocml-class class-name)
              (http::princ-to-binary-stream
               (string-downcase
                (format nil "~a%~a~%"
                        class-name
                        (ocml::make-ocml-info-string
                         (mapcar #'ocml::name
                                 (remove-duplicates
                                  (funcall instance-function class-name))))))
               stream)
              (http::princ-to-binary-stream
               (format nil "~a is an unknown class" class-name)
               stream)))
        (http::princ-to-binary-stream
         (format nil "Sorry, I can't find the ~a ontology" ontology-name)
         stream))))

(defun direct-instances-with-class-names (stream upcase-string)
  (internal-get-instances-with-class-names
   stream upcase-string #'ocml::all-current-direct-instances))

(defun all-instances-with-class-names (stream upcase-string)
  (internal-get-instances-with-class-names
   stream upcase-string #'ocml::all-current-instances))

(defun internal-get-instances-with-class-names (stream upcase-string instance-function)
  (let ((ontology-name (car (get-ocml-args upcase-string 1 1))))
    (if (get-ontology ontology-name)
        (let ((class-name (car (get-ocml-args upcase-string 1 2))))
          (ocml::select-ontology ontology-name)
          (if (ocml::get-ocml-class class-name)
              (http::princ-to-binary-stream
               (format nil "~a~%"
                       (ocml::make-ocml-info-string
                        (mapcan #'(lambda (x)
                                    (list (ocml::name x)
                                          (ocml::name (class-of x))))
                                (remove-duplicates
                                 (funcall instance-function class-name)))))
               stream)
              (http::princ-to-binary-stream
               (format nil "~a is an unknown class" class-name)
               stream)))
        (http::princ-to-binary-stream
         (format nil "Sorry, I can't find the ~a ontology" ontology-name)
         stream))))

;;;old version before a change was made to solve-application
;(defun run (stream upcase-string)
;  (with-input-from-string (istream upcase-string)
;    (read istream)   ;;the "run bit"
;    (let* ((*package* (find-package "OCML"))
;           (application (read istream))
;           (application-string
;            (make-array '(0) :element-type 'character
;                        :fill-pointer t :adjustable t)))
;      (case application
;        ((ocml::truck-cabin ocml::phys-mech
;		      ocml::cost-ontology ocml::health-rel-payoff
;                      ocml::medical-ontology 
;		      ocml::sis1-as-gen-design ocml::sis1-as-a-star-design
;		      ocml::sis1-as-hc-design ocml::kmi-as-pardes
;                      ocml::kmi-as-p-r-i
;		      ocml::kmi-as-p-and-i
;                      ocml::kmi-as-p-and-r
;		      ocml::vt-as-p-and-r)
;	 (with-output-to-string (*standard-output* application-string)
;           (handler-case
;	       (progn 
;                 (ocml::execute-application application)
;                 (http::princ-to-binary-stream
;	          (format nil "Ran application successfully ~a ~%" application)
;	          stream))
;             ;;use serious-condition to catch stack overflows
;             (serious-condition (condition)
;                    (http::princ-to-binary-stream
;	             (format nil "Running ~a I got an error of type ~a~%" application
;                             condition)
;	             stream)))
;           (with-input-from-string (application-run-stream
;                                    (coerce application-string 'string))
;             (do ((line (read-line application-run-stream nil nil)
;                        (read-line application-run-stream nil nil)))
;                 ((null line))
;               (http::princ-to-binary-stream
;		(string-downcase (format nil "~a~%" line))
;		stream)))))
;        (t (http::princ-to-binary-stream
;            (format nil "Unknown application ~a ~%" application)
;	    stream)))))
;  (http::princ-to-binary-stream *data-segment-terminator* stream))

(defun application-p (instance-name class-name)
  (let ((instance (ocml::find-current-instance instance-name class-name))) 
    (when instance
      (ocml::subclass-of? (ocml::parent-class instance)
                    (ocml::get-domain-class 'ocml::application)))))

(defun run (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)   ;;the "run bit"
    (let* ((*package* (find-package "OCML"))
           (instance-name (read istream))
           (class-name (read istream))
           (ontology (read istream))
           (application-string
            (make-array '(0) :element-type 'character
                        :fill-pointer t :adjustable t)))
      (ocml::select-ontology ontology)
      (cond ((application-p instance-name class-name)
	     (with-output-to-string (*standard-output* application-string)
               (handler-case
	           (progn 
                     (ocml::execute-application instance-name)
                     (http::princ-to-binary-stream
	              (format nil "Ran application successfully ~a ~%" instance-name)
	              stream))
                 ;;use serious-condition to catch stack overflows
                 (serious-condition (condition)
                                    (http::princ-to-binary-stream
	                             (format nil "Running ~a I got an error of type ~a~%"
                                             instance-name
                                             condition)
	                             stream)))
               (with-input-from-string (application-run-stream
                                        (coerce application-string 'string))
                 (do ((line (read-line application-run-stream nil nil)
                            (read-line application-run-stream nil nil)))
                     ((null line))
                   (http::princ-to-binary-stream
		    (string-downcase (format nil "~a~%" line))
		    stream)))))
            ((ocml::find-current-instance instance-name class-name)
             (http::princ-to-binary-stream
              (format nil "The class ~a needs to be a subclass of application ~%"
                      class-name)
	      stream))
            (t (http::princ-to-binary-stream
                (format nil "~a needs to be an instance of type application.~%"
                        instance-name)
	        stream)))))
  (http::princ-to-binary-stream *data-segment-terminator* stream))

;(select-ontology 'vt-domain)
;(def-class jurgen ())

(defvar *mail-string*
    "Holly, jurgen has changed the platform-model which
may change to value of the following parameters in your module:
car-runby, car-buffer-stroke, car-buffer-height, pit-depth,
car-buffer-blocking-height, and
car-footing-channel-height .
You may want to check these parameters.~%")

(defun get-mail (stream)
  (http::princ-to-binary-stream
   (format nil *mail-string*)
   stream)
  (http::princ-to-binary-stream *data-segment-terminator* stream))

(defun lock-ontology-and-start-editing-session (stream request-string)
  (let ((ok-to-edit-p (start-editing-session-from-request-string stream request-string)))
    (when ok-to-edit-p
      (lock-ontology request-string))))

(defun start-editing-session-from-request-string (stream request-string)
  (with-input-from-string (istream request-string)
    (read istream) ;;command
    (let* ((*package* (find-package "OCML"))
	   (author-name  (read istream))
	   (ontology-name (read istream))
	   (ontology (get-ontology ontology-name)))
      (cond (ontology
	     (cond ((ok-to-edit-p ontology author-name)
		    (start-editing-session ontology)
		    (http::princ-to-binary-stream (format nil "OK~%")
						  stream)
		    t)
		   (t (http::princ-to-binary-stream
		       (format nil "Sorry you can't edit the ontology ~a as it is owned by ~a.~%"
			       ontology-name (ocml::ontology-author ontology))
		       stream)
		      nil)))
	    (t (http::princ-to-binary-stream
	        (format nil "Unknown~%" ontology-name)
	        stream)
	       nil)))))

(defvar *root-user-name* "root")

(defun ok-to-edit-p (ontology author-name)
  (or (string= author-name *root-user-name*)
      (string= (ocml::ontology-author ontology) author-name)
      (find "all" (ocml::ontology-allowed-editors ontology) :test #'string=)
      (find author-name (ocml::ontology-allowed-editors ontology) :test #'string=)
      (let ((user-groups (user-groups author-name)))
        (when user-groups
          (intersection (multiple-value-list (parse-string-by-spaces user-groups))
                        (cons (ocml::ontology-author ontology)
                              (ocml::ontology-allowed-editors ontology))
                        :test #'string=)))))

(defun end-editing-session-from-request-string (request-string)
  (with-input-from-string (request-string-stream request-string)
    (read request-string-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read request-string-stream))
           (ontology (get-ontology ontology-name)))
      (if ontology
          (end-editing-session ontology-name)
          (web-onto-warning "~%~a is an unknown ontology" ontology-name)))))

(defun lock-ontology (request-string)
  (with-input-from-string (request-string-stream request-string)
    (read request-string-stream)
    (let* ((*package* (find-package "OCML"))
	    (author-name  (read request-string-stream))
           (ontology-name (read request-string-stream))
           (ontology (get-ontology ontology-name)))
      (cond (ontology
             (setf (ocml::locked-p ontology) t
                   (ocml::current-editor ontology) author-name))
            (t (web-onto-warning "~%~a is an unknown ontology" ontology-name))))))

(defun unlock-ontology (stream request-string)
  (end-editing-session-from-request-string request-string)
  (set-ontology-lock stream request-string nil))

(defun web-onto-warning (&rest args)
  (apply #'format *terminal-io* args))

(defun set-ontology-lock (stream request-string value)
  (with-input-from-string (request-string-stream request-string)
    (read request-string-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read request-string-stream))
           (ontology (get-ontology ontology-name)))
      (cond (ontology
             (setf (ocml::locked-p ontology) value)
             (when (null value)
               (setf (ocml::current-editor ontology) nil))
             (when stream               
               (http::princ-to-binary-stream
                (format nil "~a is now ~:[unlocked~;locked~]~%" ontology-name value)
                stream)))
            (t (when stream               
                 (http::princ-to-binary-stream
                  (format nil "couldn't find ontology ~a~%" ontology-name)
                  stream))
               (web-onto-warning "~%~a is an unknown ontology" ontology-name))))))

(defun locked-ontology-p (stream request-string)
  (with-input-from-string (request-string-stream request-string)
    (read request-string-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read request-string-stream))
           (user-name (string-downcase (read request-string-stream)))
           (ontology (get-ontology ontology-name)))
      (http::princ-to-binary-stream
       (format nil
	       (if ontology
		   (if (and (ocml::locked-p ontology)
                            (ocml::current-editor ontology)
                            (not (string= user-name
                                          (ocml::current-editor ontology))))
		       "yes~%"
		       "no~%")
		   "unknown~%"))
       stream)
      (unless ontology
	(web-onto-warning "~%~a is an unknown ontology" ontology-name)))))

(defun local-slots-p (stream upcase-string)
  (with-input-from-string (local-slots-info-stream upcase-string)
    ;;;the local_slots_p bit
    (read local-slots-info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read local-slots-info-stream))
           (type (read local-slots-info-stream))
           (name (read local-slots-info-stream)))
      (ocml::select-ontology ontology-name)
      (if (ocml::has-local-slots-p name)
           (http::princ-to-binary-stream
            (format nil "has_slots~%")
            stream)
           (http::princ-to-binary-stream
            (format nil "doesnt_have_slots~%")
            stream)))))


(defun class-p (stream upcase-string)
  (with-input-from-string (local-slots-info-stream upcase-string)
    ;;;the local_slots_p bit
    (read local-slots-info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read local-slots-info-stream))
           (name (read local-slots-info-stream)))
      (ocml::select-ontology ontology-name)
      (if (ocml::get-domain-class name)
           (http::princ-to-binary-stream
            (format nil "is_a_class~%")
            stream)
           (http::princ-to-binary-stream
            (format nil "not_a_class~%")
            stream)))))

(defun get-ontologies-owned-by (stream upcase-string)
  (get-accessible-ontologies stream upcase-string))

(defun get-editable-ontologies (stream upcase-string)
  (get-accessible-ontologies stream upcase-string t))

(defun get-all-ontologies (stream upcase-string)
  (get-accessible-ontologies stream upcase-string t t))

(defun get-ontology-type (x)
  (let ((ontology (get-ontology x)))
    (when ontology
      (ocml::ontology-type ontology))))

(defun keyword-symbol (x)
  (intern (symbol-name x) (find-package :keyword)))

(defun get-all-ontologies-of-type (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    ;;;the command name
    (read istream)
    (let* ((*package* (find-package "KEYWORD"))
           ;;;temporary hack to add mediators (read istream)) john 1/7/05
           (type (cons :mediator (read istream))) 
           (result (ocml::sorted-ontologies type)))
      (if result
          (http::princ-to-binary-stream (format nil "~{~(~a~)[~}~%"
                                                result)
                                        stream)
        (http::princ-to-binary-stream (format nil "nil~%")
                                      stream)))))


(defun get-accessible-ontologies (stream upcase-string
                                         &optional 
                                         include-editable-ontologies-p
                                         include-all-ontologies
                                         (type :all))
  (with-input-from-string (istream upcase-string)
    ;;;the command name
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (user-name (read istream))
           (result (cond (include-all-ontologies
                          (ocml::sorted-ontologies type))
                         (include-editable-ontologies-p
                          (ocml::sorted-editable-ontologies user-name
                                                            ))
                         (t (ocml::sorted-ontologies-owned-by user-name)))))
      (if result
          (http::princ-to-binary-stream (format nil "~{~(~a~)[~}~%"
                                                result)
                                        stream)
          (http::princ-to-binary-stream (format nil "nil~%")
                                        stream)))))

(defun class-parents (stream request-string)
  (return-list stream request-string #'ocml::get-ocml-class #'ocml-class-parents #'ocml-name))

(defun get-class-slots (stream request-string)
  (return-list stream request-string #'ocml::get-ocml-class
               ;;#'ocml-class-slots
               #'ocml::ocml-most-specific-class-slots
               #'ocml::pretty-string))

;;old version
;(defun class-children (stream request-string)
;  (return-list stream request-string #'ocml::get-ocml-class #'ocml-class-slots #'ocml::pretty-string))

(defun get-ontology-source-file-names (stream upcase-string)
  (with-input-from-string (info-stream upcase-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (ontology (ocml::get-ontology ontology-name)))
      (cond (ontology
             (ocml::select-ontology ontology-name)
             (http::princ-to-binary-stream
              (format nil "ok[~{~(~a~)[~}~%"
                      (ocml::ontology-files ontology))
              stream))
            (t (http::princ-to-binary-stream
                (format nil "unknown-ontology~%")
                stream))))))

(defun class-children (stream request-string)
  (return-list stream request-string #'ocml::get-ocml-class #'ocml-class-children #'ocml-name))

(defun same-class-instance-examples (stream request-string)
  (internal-class-instance-examples stream request-string t))

(defun other-class-instance-examples (stream request-string)
  (internal-class-instance-examples stream request-string))

(defun instance-of-same-class-p (x y)
  (eq (ocml::instance-of-class x) (ocml::instance-of-class y)))

(defun internal-class-instance-examples (stream request-string &optional same-class-p)
  (with-input-from-string (info-stream request-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (type (read info-stream))
           (name (read info-stream))
           (class-name (read info-stream))
           items)
      (ocml::select-ontology ontology-name)
      (setf items
	    (mapcan #'(lambda (item)
			(cond ((and same-class-p
				    (eq (ocml::instance-of-class (second item)) class-name))
			       (list item))
			      ((and (not same-class-p)
				    (not (eq (ocml::instance-of-class (second item)) class-name)))
			       (list item))))
		    (web-onto::all-expression-instantiations `(,name ?x ?y))))
      (if items
	  (http::princ-to-binary-stream
	   (format nil "~{~{~*~a[~a~}[~}~%" items)
	   stream)
	  (http::princ-to-binary-stream
	   (format nil "nil~%")
	   stream)))))

(defun class-instances (stream request-string)
  (return-list stream request-string #'identity
               #'ocml::careful-direct-instances-from-type
               #'(lambda (x) (ocml::pretty-string (ocml::name x)))))

(defun all-class-instances (stream request-string)
  (return-list stream request-string #'identity
               #'ocml::careful-all-instances-from-type
               #'(lambda (x) (ocml::pretty-string x))))

(defun sorted-careful-generalised-all-instances-from-type (type)
  (sort (ocml::careful-generalised-all-instances-from-type type)
        #'string<))

(defun generalised_all-class-instances (stream request-string)
  (return-list stream request-string #'identity
               #'sorted-careful-generalised-all-instances-from-type
               #'(lambda (x) (ocml::pretty-string x))))

(defun slot-types-or-index-subclasses (index-name slot-name)
  (cond ((eq slot-name *lois-sub-type-name*)
         (remove-duplicates
          (cons index-name
                (sort (ocml::all-current-subclass-names index-name)
                      #'string<))))
        ((eq slot-name *lois-super-type-name*)
         (remove-duplicates
                (cons index-name
                      (sort (ocml::all-superclass-names index-name)
                            #'string<))))
        (t (index-aspect-types index-name slot-name))))

(defun slot-types-except-type (stream request-string)
  (return-list stream request-string #'identity #'slot-types-or-index-subclasses #'ocml::pretty-string t))

(defun instance-default-value-of (stream request-string)
  (return-list stream request-string #'identity #'ocml::default-value-of-class #'ocml::pretty-string t))

(defun class-types (stream request-string)
  (return-list stream request-string #'identity #'ocml::class-types #'ocml::pretty-string t))

(defun class-default-values (stream request-string)
  (return-list stream request-string #'identity #'ocml::class-default-values #'ocml::pretty-string t))

(defun class-type-of (stream request-string)
  (return-list stream request-string #'identity #'ocml::type-of-class #'ocml::pretty-string t))

(defun class-info-for-new-instance (stream request-string)
  (return-list stream request-string #'identity #'ocml::get-class-info-for-new-instance
               #'ocml::pretty-string))

(defun slot-default-values (stream request-string)
  (with-input-from-string (info-stream request-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (class-name (read info-stream))
           (slot-name (read info-stream))
           items)
      (ocml::select-ontology ontology-name)
      (setf items
            (ocml::get-slot-default-values class-name slot-name))
      (if items
          (http::princ-to-binary-stream
           (format nil "~{~a[~}~%" items)
           stream)
          (http::princ-to-binary-stream
           (format nil "nil~%")
           stream)))))

(defun instance-info-for-edit (stream request-string)
  (return-list stream request-string #'identity #'ocml::get-instance-info-for-edit
               #'ocml::pretty-string))

(defun return-list (stream request-string structure-function list-function get-name-function
                           &optional selection-list-p)
  (with-input-from-string (info-stream request-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (type (read info-stream))
           (name (read info-stream))
           (selection-list (when selection-list-p (read info-stream)))
           items)
      (ocml::select-ontology ontology-name)
      (setf items  (if selection-list-p
                       (funcall list-function selection-list(funcall structure-function name))
                       (funcall list-function (funcall structure-function name))))
      (if items
          (http::princ-to-binary-stream
           (format nil "~{~a[~}~%"
                   (mapcar get-name-function items))
           stream)
          (http::princ-to-binary-stream
           (format nil "nil~%")
           stream)))))

(defun instance-class-name (stream request-string)
  (with-input-from-string (info-stream request-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (type (read info-stream))
           (name (read info-stream))
           class-name)
      (ocml::select-ontology ontology-name)
      (setf class-name (ocml::instance-of-class name))
      (http::princ-to-binary-stream
       (format nil "~a~%" (ocml::pretty-string class-name))
       stream))))

(defun ontology-type-p (type)
  (eq type 'ocml::def-ontology))

(defun get-source (stream request-string)
  (with-input-from-string (get-source-info-stream request-string)
    ;;;the get_source bit
    (read get-source-info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read get-source-info-stream))
           (type (read get-source-info-stream))
           (name (read get-source-info-stream))
           source-string)
      (ocml::select-ontology ontology-name)
      ;;;ontologies are always defined with the ontology-name
      ;;;the same as their name
      (when (ontology-type-p type)
        (setf ontology-name name))
      (setf source-string (get-source-string (list type name ontology-name)))
      (send-back-source source-string type name stream))))

(defmethod send-back-source (source-string type name stream)
  (if source-string
      (http::princ-to-binary-stream
       (format nil "~a~%"
               (ocml::convert-multiple-line-string source-string))
       stream)
      (http::princ-to-binary-stream
       (format nil "(~a ~a) has no source~%" type name)
       stream)))

(defun get-instance-source (stream request-string)
  (with-input-from-string (get-source-info-stream request-string)
    ;;;the get_source bit
    (read get-source-info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read get-source-info-stream))
           (name (read get-source-info-stream))
           (class-name (read get-source-info-stream))
           source-string)
      (ocml::select-ontology ontology-name)
      (setf source-string (get-source-string
                           (list 'ocml::def-instance name class-name ontology-name)))
      (send-back-source source-string 'def-instance name stream))))

(defun export-ontolingua (stream upcase-string)
  (with-input-from-string (info-stream upcase-string)
    ;;;the export_ontolingua bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (ontology (get-ontology ontology-name)))
       (cond (ontology
              (ocml::generate-ontolingua ontology-name stream *ocml-line-separator*)
              (http::princ-to-binary-stream
                (format nil "~%")
                stream))
            (t (http::princ-to-binary-stream
                (format nil "ontology_not_found]Sorry, can't find the ontology ~(~a~)~%" ontology-name)
                stream))))))

(defun display-source-file (stream request-string)
  (with-input-from-string (info-stream request-string)
    ;;;the display_source bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (file-name (string-downcase (read info-stream)))
           (ontology (get-ontology ontology-name))
           source)
      (cond (ontology
             (let ((file (merge-pathnames
                          (merge-pathnames (ontology-directory ontology)
                                           file-name)
                          ".lisp")))                      
               (cond ((probe-file file)
                      (setf source
                            (get-file-contents file))
                      (http::princ-to-binary-stream
                       source
                       stream))
                     (t (http::princ-to-binary-stream
                         (format nil
                                 "file_not_found]Sorry, can't find the file associated with the ontology ~(~a~)~%"
                                 ontology-name)
                         stream)))))
            (t (http::princ-to-binary-stream
                (format nil "ontology_not_found]Sorry, can't find the ontology ~(~a~)~%" ontology-name)
                stream))))))

(defvar *maximum-number-of-lines* 2000
  "The maximum number of lines of source code that can be transmitted to the webonto client")

(defun get-file-contents (file &optional (include-line-separator t))
  (with-open-file (istream file)
    (do ((line (read-line istream nil nil) (read-line istream nil nil))
         (number-of-lines 0 (1+ number-of-lines))
         (result "" (concatenate 'string result
                                 (if (and include-line-separator
                                          (not (zerop (length result))))
				     (string *ocml-line-separator*)
				     "")
                                 line)))
        ((or (> number-of-lines *maximum-number-of-lines*)
             (null line))
         (concatenate 'string
                      (if (< number-of-lines *maximum-number-of-lines*)
                          (ok-p-result-string
                           (< number-of-lines *maximum-number-of-lines*))
                          (concatenate 'string
                                       (ok-p-result-string
                                        (< number-of-lines *maximum-number-of-lines*))
                                       "["
                                       (format nil "~d" *maximum-number-of-lines*)))
                      "["
                      (string *ocml-line-separator*)
                      result
                      (if include-line-separator
                          (string *ocml-line-separator*)
                          "")
                      line
                      (format nil "~%"))))))

(defun instance-type-p (type)
  (find type '(ocml::instance ocml::instances)))


(defun class-type-p (type)
  (find type '(ocml::class ocml::classes)))


(defun relation-type-p (type)
  (find type '(ocml::relation ocml::relations)))

(defun delete-all-instance-sources (name defining-type ontology-name)
  (let ((class (ocml::get-ocml-class name)))
    (when class
      (mapc
       #'(lambda (instance)
           (delete-instance-source
            'ocml::def-instance (ocml::name instance) name ontology-name))
       (ocml::get-current-direct-instances class)))))


(defun collect-instance-source-buffers (name ontology-name)
  (let ((class (ocml::get-ocml-class name)))
    (when class
      (mapcar
       #'(lambda (instance)
           (list (get-instance-source-buffer
                  'ocml::def-instance (ocml::name instance) name ontology-name)
                 'ocml::def-instance (ocml::name instance) ontology-name
                 t name))
       (ocml::get-current-direct-instances class)))))

(defun collect-relation-instance-source-buffers (name ontology-name)
  (let ((relation (ocml::get-relation name)))
    (when relation
      (mapcar
       #'(lambda (relation-instance)
           (list
            (get-source-buffer
             'ocml::def-relation-instances
             (ocml::get-relation-instance-expression relation-instance) ontology-name)
            'ocml::def-relation-instances
            (ocml::get-relation-instance-expression relation-instance)
            ontology-name t nil))
       (ocml::get-direct-relation-instances relation)))))

(defun get-instance-source-buffer (defining-type name instance-class-name ontology-name)
  (get-dspec-buffer
   (list defining-type name instance-class-name ontology-name)))

(defun get-source-buffer (defining-type name ontology-name)
  (get-dspec-buffer
   (list defining-type name ontology-name)))

(defun delete-items (stream request-string)
  (with-input-from-string (info-stream request-string)
    ;;;the delete_items bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (types-and-names (read info-stream)))
      (internal-delete-items ontology-name types-and-names stream))))

(defun internal-delete-items (ontology-name types-and-names stream)
  (let* ((*package* (find-package "OCML"))
         (error-strings nil)
         (actions nil)
         source-buffer)
    (ocml::select-ontology ontology-name)
    (mapc #'(lambda (type-and-name)
              (let* ((type (if (eq (car type-and-name) 'ocml::all)
                               (ocml::get-unknown-type (second type-and-name))
                             (car type-and-name)))
                     (name (second type-and-name))
                     (instance-class-name nil)
                     (instances-to-delete-info nil)
                     (defining-type
                      (ocml::convert-type-to-defining-type-name type)))
                (cond (type
                       ;;(delete-source defining-type name )
                       (when (class-type-p type)
                         (setf instances-to-delete-info
                               (collect-instance-source-buffers
                                name
                                ontology-name)))
                       (when (relation-type-p type)
                         (setf instances-to-delete-info
                               (collect-relation-instance-source-buffers
                                name ontology-name)))
                       (when (instance-type-p type)
                         (setf instance-class-name (third type-and-name)))
                       (setf source-buffer
                             (if (instance-type-p type)
                                 (get-instance-source-buffer
                                  defining-type name instance-class-name
                                  ontology-name)
                               (get-source-buffer defining-type name ontology-name)))
                       (multiple-value-bind (ok-p error-string action)
                           (ocml::delete-ocml-object type name instance-class-name)

                         (cond (ok-p
                                (delete-source source-buffer defining-type name
                                               ontology-name t instance-class-name)
                                (mapc
                                 #'(lambda (instance-to-delete-info)
                                     (apply #'delete-source instance-to-delete-info))
                                 instances-to-delete-info)
                                (when action
                                  (push action actions))
                                (push error-string error-strings))
                               (t (push error-string error-strings)))))
                      (t (push (format nil "Sorry couldn't work out the type of ~a ~a" name type)
                               error-strings)))))
          types-and-names)
    (http::princ-to-binary-stream
     (format nil 
             "~{~a~a~}Results of deletion. ~{~{~aFor ~(~a~) ~(~a~) ~:[OK~*~;~a~]. ~}~} ~%"
             (mapcar #'(lambda (action)
                        (list *ocml-line-separator* action))
                     actions)
             (mapcar #'(lambda (x y)
                         (list *ocml-line-separator*
                               (car x) (second x) (and y 
                                                       (> (length y) 0))
                               y))
                     types-and-names (reverse 
                                      error-strings)))
     stream)))

(defun new-class-parent (stream request-string)
  (with-input-from-string (get-source-info-stream request-string)
    ;;;the new_class_parent bit
    (read get-source-info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read get-source-info-stream))
           (name (read get-source-info-stream))
           (new-parent (read get-source-info-stream))
           source-string)
      (ocml::select-ontology ontology-name)
      (setf source-string (get-source-string (list 'ocml::def-class name
                                                   ontology-name)))
      (cond (source-string
             (add-new-parent-to-source ontology-name
                                       name 'ocml::def-class source-string
                                       new-parent stream)
             (http::princ-to-binary-stream
              (format nil "~a~%"
                      (ocml::convert-multiple-line-string source-string))
              stream))
            (t (http::princ-to-binary-stream
                (format nil "(~a ~a) has no source~%" 'def-class name)
                stream))))))

(defun new-instance-class (stream request-string)
  (with-input-from-string (get-source-info-stream request-string)
    ;;;the new_instance_class bit
    (read get-source-info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read get-source-info-stream))
           (name (read get-source-info-stream))
           (new-class (read get-source-info-stream))
           source-string)
      (ocml::select-ontology ontology-name)
      (setf source-string (get-source-string
                           (list 'ocml::def-instance name ontology-name)))
      (cond (source-string
             (add-new-parent-to-source ontology-name
                                       name 'ocml::def-instance source-string new-class stream)
             (http::princ-to-binary-stream
              (format nil "~a~%"
                      (ocml::convert-multiple-line-string source-string))
              stream))
            (t (http::princ-to-binary-stream
                (format nil "(~a ~a) has no source~%" 'def-class name)
                stream))))))

(defun new-instance-slot-value (stream request-string)
  (with-input-from-string (get-source-info-stream request-string)
    ;;;the new_instance_class bit
    (read get-source-info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read get-source-info-stream))
           (name (read get-source-info-stream))
           (slot-name (read get-source-info-stream))
	   (new-value (read get-source-info-stream))
           source-string)
      (ocml::select-ontology ontology-name)
      (setf source-string (get-source-string
                           (list 'ocml::def-instance name ontology-name)))
      (cond (source-string
             (change-instance-slot-value-in-source
              ontology-name
              name source-string slot-name new-value stream)
             (http::princ-to-binary-stream
              (format nil "Successfully changed the value of ~a to be ~a in instance ~a~%"
                      slot-name new-value name)
              stream))
            (t (http::princ-to-binary-stream
                (format nil "(~a ~a) has no source~%" 'def-class name)
                stream))))))

(defun change-instance-slot-value-in-source (ontology
                                             name source-string slot-name
                                             new-value stream)
  (let* ((*package* (find-package "OCML"))
         (source (read-from-string source-string))
         new-source)
    (setf new-source
          (append (subseq source 0 3)
                  (list (reset-instance-slot-value (elt source 3) slot-name new-value))))
    (internal-process-new-source 'ocml::def-instance name
                                 (string-downcase (format nil "~:w" new-source))
                                 stream
                                 ontology)))

(defun reset-instance-slot-value (slots slot-name new-value)
  (when (assoc slot-name slots)
      (setf (second (assoc slot-name slots)) new-value))
  slots)

(defun add-new-parent-to-source (ontology name type source-string new-parent stream)
  
  (let ((source (read-from-string source-string))
        new-source)
    (setf new-source
          (append (list (car source) (second source))
                  (if (eq type 'ocml::def-class)
                      (list (append (third source) (list new-parent)))
                      (list new-parent))
                  (cdddr source)))
    (internal-process-new-source type name
                                 (string-downcase
                                  (format nil "~:w" new-source)) stream
                                 ontology)))

(defun new-source (stream request-string)
  (process-new-piece-of-code stream request-string "new_source" #'process-new-source))

(defun new-definition (stream request-string)
  (process-new-piece-of-code stream request-string "new_definition" #'process-new-definition))

(defun process-new-piece-of-code (stream request-string action-name process-function)
  (let* ((processed-string (substitute #\linefeed *ocml-line-separator*
				       request-string))
         (action-string-length (1+ (length action-name))) ;;add a space
         (source-info
          (with-input-from-string (istream (subseq processed-string action-string-length))
	    (read-line istream))))
    (funcall process-function
             source-info
	     (read-chars-in-form (subseq processed-string
					 ;;one for linefeed at end of 1st line
					 (+ 1 action-string-length
					    (length source-info))))
	     stream)))


(defun read-chars-in-form (string)
  (do* ((i 0 (1+ i))
        (ch (elt string 0) (elt string i))
        ;;;for semicolon commented lines
        (comment-line-p nil)
        (read-non-whitespace-char-p (non-whitespace-char-p ch comment-line-p)
                                    (or read-non-whitespace-char-p
                                        (non-whitespace-char-p ch comment-line-p)))
        (bracket-count (bracket-count ch) (+ bracket-count (bracket-count ch comment-line-p)))
        (return-string (string ch) (concatenate 'string return-string (string ch))))
       ((or (>= (1+ i) (length string))
            (and read-non-whitespace-char-p (zerop bracket-count)))
        return-string)
    (when (eq ch #\;)
      (setf comment-line-p t))
    (when (find ch '(#\return #\linefeed) :test #'char=)
      (setf comment-line-p nil))))

(defun non-whitespace-char-p (ch &optional comment-line-p)
  (and (not comment-line-p) (not (find ch '(#\space #\tab #\return #\linefeed)))))

(defun bracket-count (ch  &optional comment-line-p)
  (if comment-line-p
      0
      (case ch
        ((#\() 1)
        ((#\)) -1)
        (t 0))))

(defun undo-edit (stream request-string)
  (with-input-from-string (request-string-stream request-string)
    ;;;the undo_edit bit
    (read request-string-stream)
    (let* ((how-many (read request-string-stream)))
      (cond ((eq how-many 'all)
             (http::princ-to-binary-stream
              (format nil "~a~%" (undo-source-changes))
	      stream))
            ((numberp how-many)
             (http::princ-to-binary-stream
              (format nil "~a~%" (undo-source-changes how-many))
	      stream))
            (t (http::princ-to-binary-stream
                (format nil "Sorry couldn't undo ~a changes~%" how-many)
                stream))))))
          

(defun send-maybe-broadcast (header string stream)
  (http::princ-to-binary-stream (format nil "~a~%" string) stream)
  (when http::*broadcasting-p*
    (broadcast (concatenate 'string header string))))

(defun slot-options (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    ;;the "get_class" bit
    (read load-info-stream) 
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read load-info-stream))
           (ontology (get-ontology ontology-name))
	   (class-name (read load-info-stream)))
      (cond (ontology
             (ocml::select-ontology ontology-name)
             (http::princ-to-binary-stream
              (if (ocml::get-ocml-class class-name)
	          (format nil "~a~%" (ocml::internal-slot-options (ocml::get-domain-class class-name)))
                (format nil "class_does_not_exist~%"))
              stream))
            (t (send-ontology-does-not-exist stream))))))


(defun get-detailed-class-info (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    ;;the "get_class" bit
    (read load-info-stream) 
    (let* ((*package* (find-package "OCML"))
           (ocml::*list-length-limit* (read load-info-stream))
           (ontology-name (read load-info-stream))
           (ontology (get-ontology ontology-name))
	   (class-name (read load-info-stream)))
      ;;(format t "get-detailed-class-info ~a ~a ~a~%" ontology-name ontology class-name)
      (cond (ontology
             (ocml::select-ontology ontology-name)
             ;;(format t "~%get-detailed-class-info class name ~a" class-name)
             (send-maybe-broadcast
              (string-downcase (format nil "show_class_info ~a " class-name))
              (if (ocml::get-ocml-class class-name)
	          (format nil
                          "~{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~}"
		          (ocml::list-detailed-class-info class-name))
	          (format nil "class_does_not_exist~%"))
              stream))
            (t (send-ontology-does-not-exist stream))))))

(defun get-detailed-axiom-info (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    ;;the "axiom_info" bit
    (read load-info-stream) 
    (let* ((*package* (find-package "OCML"))
           (ocml::*list-length-limit* (read load-info-stream))
           (ontology-name (read load-info-stream))
           (ontology (get-ontology ontology-name))
	   (axiom-name (read load-info-stream)))
      (cond (ontology
             (ocml::select-ontology ontology-name)
             (send-maybe-broadcast
              (string-downcase (format nil "show_axiom_info ~a " axiom-name))
              (if (ocml::get-axiom axiom-name)
	          (format nil "~{~a{~a{~a{~}"
		          (ocml::list-detailed-axiom-info axiom-name))
	          (format nil "axiom_does_not_exist~%"))
              stream))
            (t (send-ontology-does-not-exist stream))))))

(defun string-class-info (ocml-name)
  (let ((class-info (elt (ocml::list-detailed-class-info ocml-name) 6)))
    (substitute #\space #\}
                (substitute #\linefeed #\% class-info))))
    

(defun send-ontology-does-not-exist (stream)
  (http::princ-to-binary-stream
   (format nil "ontology_does_not_exist~%")
   stream))


(defun get-detailed-relation-info (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    ;;the "get_relation" bit
    (read load-info-stream) 
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read load-info-stream))
           (ontology (get-ontology ontology-name))
	   (relation-name (read load-info-stream)))
      (cond (ontology
             (ocml::select-ontology ontology-name)
             ;;(format t "~%get-detailed-relation-info relation name ~a"
	     ;;relation-name)
             (http::princ-to-binary-stream
              (if (ocml::get-ocml-relation relation-name)
	          (format nil "~{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~a{~}~%"
		          (ocml::list-detailed-relation-info relation-name))
	          (format nil "relation_does_not_exist~%"))
              stream))            
            (t (send-ontology-does-not-exist stream))))))

(defun get-relation-schema (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    ;;the "get_relation_schema" bit
    (read load-info-stream) 
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read load-info-stream))
           (ontology (get-ontology ontology-name))
	   (relation-name)
           (relation ))
      (cond (ontology
             (ocml::select-ontology ontology-name)
             ;;(format t "~%get-detailed-relation-info relation name ~a"
	     ;;relation-name)
             (setf relation-name (read load-info-stream)
                   relation (ocml::get-ocml-relation relation-name))
             (http::princ-to-binary-stream
              (if relation
	          (format nil "~{~(~a~)[~}~%"
		          (ocml::schema relation))
	          (format nil "relation_does_not_exist~%"))
              stream))            
            (t (send-ontology-does-not-exist stream))))))


(defun get-detailed-function-info (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    ;;the "get_function" bit
    (read load-info-stream) 
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read load-info-stream))
           (ontology (get-ontology ontology-name))
	   (function-name (read load-info-stream)))
      (cond (ontology
             (ocml::select-ontology ontology-name)
             ;;(format t "~%get-detailed-function-info function name ~a ~a"
		   ;;  function-name ontology-name)
             (http::princ-to-binary-stream
              (if (ocml::get-ocml-function function-name)
	          (format nil "~{~a{~a{~a{~a{~a{~a{~a{~a{~}~%"
			  (ocml::list-detailed-function-info function-name))
	          (format nil "function_does_not_exist~%"))
              stream))            
            (t (send-ontology-does-not-exist stream))))))

(defun get-detailed-rule-info (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    ;;the "get_rule" bit
    (read load-info-stream) 
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read load-info-stream))
           (ontology (get-ontology ontology-name))
	   (rule-name (read load-info-stream)))
      (cond (ontology
             (ocml::select-ontology ontology-name)
             ;;(format t "~%get-detailed-rule-info rule name ~a"
               ;;      rule-name)
             (http::princ-to-binary-stream
              (if (ocml::find-bc-rule rule-name)
	          (format nil "~{~a{~a{~a{~a{~}~%"
		          (ocml::list-detailed-rule-info rule-name))
	          (format nil "rule_does_not_exist~%"))
              stream))            
            (t (send-ontology-does-not-exist stream))))))

(defun get-detailed-instance-info (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    ;;the "get_instance" bit
    (read load-info-stream) 
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read load-info-stream))
           (ontology (get-ontology ontology-name))
	   (instance-name (read load-info-stream))
           ;;must get the class name now because we can have
           ;;multiple instances with the same name in the same
           ;;ontology
           (class-name (read load-info-stream))
           class 
           instance-structure)
      (cond (ontology
             (ocml::select-ontology ontology-name)
             (setf class (ocml::get-domain-class class-name))
             (cond (class
                    ;;(format t "~%get-detailed-instance-info instance name ~a"
                    ;;      instance-name)
                    (setf instance-structure
                          (ocml::find-current-direct-instance class instance-name))
                    (http::princ-to-binary-stream
                     (if instance-structure
	                 (format nil "~{~a{~a{~a{~a{~}~%"
		                 (ocml::list-detailed-instance-info instance-name
                                                                    class-name
                                                                    instance-structure))
	                 (format nil "instance_does_not_exist~%"))
                     stream))
                   (t (http::princ-to-binary-stream
                       (format nil "instance_does_not_exist~%"))
                      stream)))
            (t (send-ontology-does-not-exist stream))))))

;(defun string-instance-info (instance-name)
;  (substitute #\space #\}
;              (substitute #\linefeed #\{
;                          (substitute #\linefeed #\%
;                                      (format nil "~{~a{~a{~}~%"
;					      (ocml::list-detailed-instance-info instance-name))))))
;                 

(defun get-detailed-ontology-info (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    ;;the "get_ontology" bit
    (read load-info-stream) 
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read load-info-stream))
           (ontology (get-ontology ontology-name))
	   (ontology-to-view-name (read load-info-stream)))
      (cond (ontology
             (ocml::select-ontology ontology-name)
             ;;(format t "~%get-detailed-ontology-info ontology name ~a"
               ;;      ontology-to-view-name)
             (http::princ-to-binary-stream
              (if (ocml::get-ontology ontology-to-view-name)
	          (format nil "~{~a{~a{~a{~a{~a{~a{~a{~}~%"
		          (ocml::list-detailed-ontology-info ontology-to-view-name))
	          (format nil "ontology_does_not_exist~%"))
              stream))            
            (t (send-ontology-does-not-exist stream))))))

(defvar *or-filters* nil)

(defvar *and-filters* nil)

(defun load-ontology (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    (read load-info-stream) ;;the load-ontology string
    (let ((*package* (find-package "OCML")))
      (multiple-value-bind (or-filters and-filters)
          (parse-filters (read load-info-stream))
        (let ((*or-filters* or-filters)
              (*and-filters* and-filters)
              (ocml::*list-length-limit* (read load-info-stream))
              (ontology (read load-info-stream))
              (ocml-type (read load-info-stream)))
          (if (find ontology *ontologies*)
              (internal-load-ontology ontology ocml-type stream)
              (if (loaded-ontology-p ontology)
                  ;;not one we can load but loaded already
                  ;;by virtue of being included-by one of the above
                  (internal-load-ontology ontology ocml-type stream)
                  (http::princ-to-binary-stream
                   (format nil "unknown-ontology~a~%"
                           *ocml-line-separator*)
	           stream))))))))

(defvar *ontology-name-to-load-name*
    '((ocml::kmi-as-pardes . ocml::kmi-as-gen-design)
      (ocml::kmi-as-p-and-i . ocml::kmi-as-propose-and-improve)
      (ocml::kmi-as-p-and-r . ocml::kmi-as-propose-and-revise) 
      (ocml::vt-as-p-and-r . ocml::vt-as-propose-and-revise)))

(defun lookup-ontology-load-name (name)
  (or (cdr (assoc name *ontology-name-to-load-name*))
	       name))

(defun internal-load-ontology (ontology ocml-type stream)
  (declare (ignore ocml-type))
  (internal-load-single-ontology ontology)
  (ocml::select-ontology ontology)
  (send-whole-ontology stream))

(defvar *ontology-being-current-loaded* nil)

(defun internal-load-single-ontology (ontology &optional load-anyway-p)
  (unless (and (loaded-ontology-p ontology) (not load-anyway-p))
    (let ((ontology-load-name (lookup-ontology-load-name ontology)))
      (setf *ontology-being-current-loaded* ontology)
      (load-ontology-by-name ontology))))

(defvar *planet-onto-mode* nil)

(defun reload-library ()
  (ocml:initialize-ocml)
  (setup-library))

(defun reinitialise-ocml (stream)
  (handler-case
      (progn
        (ocml:initialize-ocml)
        (if *planet-onto-mode*
            (setup-planet-onto-library)
            (setup-library))
        (http::princ-to-binary-stream
         (format nil "Successfully reinitialised OCML~%")
         stream))
    ;;use serious-condition to catch stack overflows
    (serious-condition (condition)
	   (http::princ-to-binary-stream
	    (format nil "Whilst loading the ontology ~a I got an error of type ~a~%" 
		    (if  ocml::*current-ontology*
                        (ocml::name ocml::*current-ontology*)
                        *ontology-being-current-loaded*)
                    condition)
	    stream))))

(defun load-all-ontologies ()
  (setf *ontologies* (union *required-ontologies* *ontologies*))
  (mapc #'(lambda (ontology)
            ;;now make sure the ontology isn't already loaded
            (unless (get-ontology ontology)
              (internal-load-single-ontology ontology)))
        *ontologies*)
  ;;(compile-all-system-ontologies)
  )

#+:irs-lispworks
(defun compile-all-system-ontologies (&optional (ontologies *ontologies*) force-p)
  (mapcar #'(lambda (x)
              (let ((ontology (lookup-ontology-load-name x)))
                (when (find-system ontology)
                    (system::compile-system ontology :force-p force-p :load t))))
          ontologies))

(defun view-ontology (stream request-string)
  (with-input-from-string (load-info-stream request-string)
    (read load-info-stream) ;;the "view-ontology" string
    (let ((*package* (find-package "OCML")))
      (multiple-value-bind (or-filters and-filters)
          (parse-filters (read load-info-stream))
        (let ((*or-filters* or-filters)
              (*and-filters* and-filters)
              (ocml::*list-length-limit* (read load-info-stream))
              (view-only-items-in-current-ontology
               (java-true-p (read load-info-stream)))
              (include-base-ontology-p (java-true-p (read load-info-stream)))
              (ontology (read load-info-stream))
              (ocml-type (read load-info-stream)))
          (internal-view-ontology ontology
                                  ocml-type view-only-items-in-current-ontology
                                  include-base-ontology-p stream))))))

(defun internal-view-ontology (ontology ocml-type
                                        view-only-items-in-current-ontology
                                        include-base-ontology-p stream)
  (ocml::select-ontology ontology)
  (case ocml-type
    ((ocml::classes) (send-classes view-only-items-in-current-ontology
                                   include-base-ontology-p stream))
    ((ocml::top_classes) (send-classes view-only-items-in-current-ontology
                                   include-base-ontology-p stream t))
    ((ocml::axioms) (send-axioms view-only-items-in-current-ontology
                                   include-base-ontology-p stream))
    ((ocml::tasks) (send-tasks view-only-items-in-current-ontology
                               include-base-ontology-p stream))
    ((ocml::problem_solving_methods) (send-problem-solving-methods
                                      view-only-items-in-current-ontology
                                      include-base-ontology-p stream))
    ((ocml::functions) (send-functions view-only-items-in-current-ontology
                                   include-base-ontology-p stream))
    ((ocml::procedures) (send-procedures view-only-items-in-current-ontology
                                   include-base-ontology-p stream))
    ((ocml::relations) (send-relations view-only-items-in-current-ontology
                                   include-base-ontology-p stream))
    ((ocml::relation_instances)
     (send-relation-instances view-only-items-in-current-ontology
                                   include-base-ontology-p stream))
    ((ocml::rules) (send-rules view-only-items-in-current-ontology
                                   include-base-ontology-p stream))
    ((ocml::instances) (send-instances view-only-items-in-current-ontology
                                   include-base-ontology-p stream))
    ((ocml::all) (send-all-types view-only-items-in-current-ontology
                                   include-base-ontology-p stream))
    (t (http::princ-to-binary-stream
	(format nil "Unknown ocml type ~a ~%" ocml-type)
	stream))))

(defun class-info (class-name)
  (ocml::list-class-info class-name
                         (gethash class-name ocml::*domain-classes*)))

(defun current-ontology-includes (&optional
                                  (current-ontology ocml::*current-ontology*))
  (append (mapcar #'ocml::pretty-structure-name
                (ocml::ontology-ancestors current-ontology))
        (list (ocml::pretty-structure-name current-ontology))))

(defun all-ontologies ()
  (mapcar #'car ocml::*all-ontologies*))

(defvar *ontology-info-character-separator* "$"
  "The character used to separate ontology information from other
                              when an ontology is loaded or viewed")

;;;make sure to send tasks and psms before classes and
;;;rules before relations
(defun send-whole-ontology (stream)
  (let (tasks-string problem-solving-methods-string
                     classes-string axioms-string instances-string rules-string
                     relations-string relation-instances-string
                     functions-string procedures-string
                     temp-ok (ok-p t) (number-of-items 0))
    (multiple-value-setq (relations-string temp-ok number-of-items)
        (relations-string number-of-items t))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (tasks-string temp-ok number-of-items)
        (tasks-string number-of-items t))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (problem-solving-methods-string temp-ok number-of-items)
        (problem-solving-methods-string number-of-items t))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (classes-string temp-ok number-of-items)
        (classes-string number-of-items t))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (axioms-string temp-ok number-of-items)
        (axioms-string number-of-items t))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (rules-string temp-ok number-of-items)
        (rules-string number-of-items t))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (functions-string temp-ok number-of-items)
        (functions-string number-of-items t))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (procedures-string temp-ok number-of-items)
        (procedures-string number-of-items t))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (instances-string temp-ok number-of-items)
        (instances-string number-of-items t))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (relation-instances-string temp-ok number-of-items)
        (relation-instances-string number-of-items t))
    (setf ok-p (and ok-p temp-ok))
    #+:irs-lispworks
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~a~a~a~a~%"
	     ;;(current-ontology-includes)
             ;;sending all ontologies enables us to describe them
             (ok-p-result ok-p)
             *ocml-line-separator*
             (all-ontologies)
             relations-string
             tasks-string problem-solving-methods-string
             classes-string
             axioms-string
             rules-string
             functions-string
             procedures-string
             instances-string
             relation-instances-string)
     stream)))

(defun list-subclasses-of (view-only-items-in-current-ontology
                           include-base-ontology-p
                           class-name not-class-name root-classes-only-p
                           &optional (number-of-items 0))
  (ocml::list-classes view-only-items-in-current-ontology
                      include-base-ontology-p
                      number-of-items
                      ocml::*current-ontology* nil nil nil class-name not-class-name
                      root-classes-only-p))

(defun classes-string (view-only-items-in-current-ontology
                       include-base-ontology-p
                       &optional (number-of-items 0)
                       include-type&separator subclass-of not-subclass-of
                       root-classes-only-p)
  (multiple-value-bind (classes ok-p new-number-of-items)
      (list-subclasses-of view-only-items-in-current-ontology include-base-ontology-p
                          subclass-of not-subclass-of root-classes-only-p
                          number-of-items)
    (values
     (if include-type&separator
         (format nil "~aclasses~a~{~{~a{~a{~a{~a{~}[~}"
                 *ontology-info-character-separator*
                 *ontology-info-character-separator*
                 (sort classes
                       #'(lambda (x y)
			   (string< (car x) (car y)))))
         (format nil "~{~{~a{~a{~a{~a{~}[~}"
                 (sort classes #'(lambda (x y) (string< (car x) (car y))))))
     ok-p
     new-number-of-items)))

(defun axioms-string (view-only-items-in-current-ontology
                      include-base-ontology-p
                      &optional (number-of-items 0)
                      include-type&separator)
  (multiple-value-bind (axioms ok-p new-number-of-items)
      (ocml::list-axioms view-only-items-in-current-ontology include-base-ontology-p
                         number-of-items)
    (values
     (if include-type&separator
         (format nil  "~aaxioms~a~{~{~a{~a{~}[~}"
                 *ontology-info-character-separator*
                 *ontology-info-character-separator*
                 (sort axioms #'(lambda (axiom-item1 axiom-item2)
                                  (string< (car axiom-item1)
                                           (car axiom-item2)))))
         (format nil "~{~{~a{~a{~}[~}"
                 (sort axioms #'(lambda (axiom-item1 axiom-item2)
                                  (string< (car axiom-item1) (car axiom-item2))))))
     ok-p
     new-number-of-items)))

(defun tasks-string (view-only-items-in-current-ontology
                     include-base-ontology-p
                     &optional (number-of-items 0) include-type&separator)
  (classes-string view-only-items-in-current-ontology
                  include-base-ontology-p
                  number-of-items
                  include-type&separator 'ocml::task 'ocml::problem-solving-method))

(defun problem-solving-methods-string (view-only-items-in-current-ontology
                                       include-base-ontology-p
                                       &optional (number-of-items 0)
                                       include-type&separator)
  (classes-string view-only-items-in-current-ontology
                  include-base-ontology-p
                  number-of-items
                  include-type&separator 'ocml::problem-solving-method))

(defun ok-p-result (ok-p)
  (if ok-p
      'ok
      'too_large))

(defun ok-p-result-string (ok-p)
  (format nil "~(~a~)"
          (ok-p-result ok-p)))

(defvar *classes-to-hide* 
  '(ocml::task ocml::goal ocml::web-service ocml::mediator
                                   ocml::capability ocml::interface 
                                   ocml::orchestration ocml::choreography
                                   ocml::core-non-functional-properties 
                                   ocml::non-functional-properties 
                                   ocml::problem-solving-pattern
                                   ocml::publisher-information
                                   ))

(defun send-classes (view-only-items-in-current-ontology
                     include-base-ontology-p stream &optional root-classes-only-p)
  (multiple-value-bind (classes-string ok-p)
      (classes-string view-only-items-in-current-ontology
                      include-base-ontology-p 0 nil nil 
                      *classes-to-hide*
                      root-classes-only-p)
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~a~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
	     (all-ontologies)
             *ontology-info-character-separator*
             classes-string)
     stream)))

(defun send-axioms (view-only-items-in-current-ontology
                    include-base-ontology-p stream)
  (multiple-value-bind (axioms-string ok-p)
      (axioms-string view-only-items-in-current-ontology
                     include-base-ontology-p 0)
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~(~a~)~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
             (all-ontologies)
             *ontology-info-character-separator*
             axioms-string)
     stream)))

(defun send-tasks (view-only-items-in-current-ontology
                   include-base-ontology-p stream)
  (multiple-value-bind (tasks-string ok-p)
      (tasks-string view-only-items-in-current-ontology
                    include-base-ontology-p)
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~a~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
	     (all-ontologies)
             *ontology-info-character-separator*
             tasks-string)
     stream)))

(defun send-problem-solving-methods (view-only-items-in-current-ontology
                                     include-base-ontology-p stream)
  (multiple-value-bind (problem-solving-methods-string ok-p)
      (problem-solving-methods-string view-only-items-in-current-ontology
                                      include-base-ontology-p)
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~a~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
	     (all-ontologies)
             *ontology-info-character-separator*
             problem-solving-methods-string)
     stream)))

(defun functions-string (view-only-items-in-current-ontology
                         include-base-ontology-p &optional (number-of-items 0)
                                   include-type&separator)
  (multiple-value-bind (functions ok-p new-number-of-items)
      (ocml::list-functions view-only-items-in-current-ontology
                            include-base-ontology-p number-of-items)
    (values 
     (if include-type&separator      
         (format nil "~afunctions~a~{~{~a{~a{~}[~}"
                 *ontology-info-character-separator*
                 *ontology-info-character-separator*
                 (sort functions #'(lambda (x y)
                                     (string< (car x) (car y)))))
         (format nil "~{~{~a{~a{~}[~}"
                 (sort functions #'(lambda (x y)
                                     (string< (car x) (car y))))))
     ok-p new-number-of-items)))

(defun send-functions (view-only-items-in-current-ontology
                       include-base-ontology-p stream)
  (multiple-value-bind (functions-string ok-p)
      (functions-string view-only-items-in-current-ontology
                        include-base-ontology-p)
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~a~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
	     (all-ontologies)
             *ontology-info-character-separator*
             functions-string)
     stream)))

(defun procedures-string (view-only-items-in-current-ontology
                          include-base-ontology-p &optional (number-of-items 0)
                                    include-type&separator)
  (multiple-value-bind (procedures ok-p new-number-of-items)
      (ocml::list-procedures view-only-items-in-current-ontology
                             include-base-ontology-p number-of-items)
    (values 
     (if include-type&separator      
         (format nil "~aprocedures~a~{~{~a{~a{~}[~}"
                 *ontology-info-character-separator*
                 *ontology-info-character-separator*
                 (sort procedures #'(lambda (x y)
                                      (string< (car x) (car y)))))
         (format nil "~{~{~a{~a{~}[~}"
                 (sort procedures #'(lambda (x y)
                                      (string< (car x) (car y))))))
     ok-p new-number-of-items)))

(defun send-procedures (view-only-items-in-current-ontology
                        include-base-ontology-p stream)
  (multiple-value-bind (procedures-string ok-p)
      (procedures-string view-only-items-in-current-ontology
                         include-base-ontology-p)
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~a~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
	     (all-ontologies)
             *ontology-info-character-separator*
             procedures-string)
     stream)))

(defun relations-string (view-only-items-in-current-ontology
                         include-base-ontology-p &optional (number-of-items 0)
                         include-type&separator)
  (multiple-value-bind (relations ok-p new-number-of-items)
      (ocml::list-relations view-only-items-in-current-ontology
                            include-base-ontology-p number-of-items)
    (values 
     (if include-type&separator
         (format nil "~arelations~a~{~{~a{~a{~}[~}"
                 *ontology-info-character-separator*
                 *ontology-info-character-separator*
                 (sort relations #'(lambda (x y)
                                     (string< (car x) (car y))))) 
         (format nil "~{~{~a{~a{~}[~}"
                 (sort relations #'(lambda (x y)
                                     (string< (car x) (car y))))))
     ok-p new-number-of-items)))

(defun send-relations (view-only-items-in-current-ontology
                       include-base-ontology-p stream)
  (multiple-value-bind (relations-string ok-p)
      (relations-string view-only-items-in-current-ontology
                        include-base-ontology-p)
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~a~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
	     (all-ontologies)
             *ontology-info-character-separator*
             relations-string)
     stream)))

(defun relation-instances-string (view-only-items-in-current-ontology
                                  include-base-ontology-p &optional (number-of-items 0)
                                  include-type&separator)
  (multiple-value-bind (relation-instance-items ok-p new-number-of-items)
      (ocml::list-relation-instances view-only-items-in-current-ontology
                                     include-base-ontology-p number-of-items)
    (let ((string-relation-instance-items
           (mapcar #'(lambda (relation-instance-item)
                       (list (princ-to-string (car relation-instance-item))
                             (second relation-instance-item)))
                   relation-instance-items)))
      (values
	;; XXX
	#+:irs-lispworks
       (if include-type&separator
           (format nil "~arelation_instances~a~{~{~a{~a{~}[~}"
                   (sort string-relation-instance-items
                         #'string< :key #'car))
           (format nil "~{~{~a{~a{~}[~}"
                   (sort string-relation-instance-items
                         #'string< :key #'car)))
       ok-p new-number-of-items))))

(defun send-relation-instances (view-only-items-in-current-ontology
                                include-base-ontology-p stream)
  (multiple-value-bind (relation-instances-string ok-p)
      (relation-instances-string view-only-items-in-current-ontology
                                 include-base-ontology-p)
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~(~a~)~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
	     (all-ontologies)
             *ontology-info-character-separator*
             relation-instances-string)
     stream)))

(defun rules-string (view-only-items-in-current-ontology
                     include-base-ontology-p
                     &optional (number-of-items 0) include-type&separator)
  (multiple-value-bind (rules ok-p new-number-of-items)
      (ocml::list-rules view-only-items-in-current-ontology
                        include-base-ontology-p number-of-items)
    (values 
     (if include-type&separator
         (format nil "~arules~a~{~{~a{~a{~}[~}"
                 *ontology-info-character-separator*
                 *ontology-info-character-separator*
                 (sort rules #'(lambda (x y)
                                 (string< (car x) (car y)))))
         (format nil "~{~{~a{~a{~}[~}"
                 (sort rules #'(lambda (x y)
                                 (string< (car x) (car y))))))
     ok-p new-number-of-items)))

(defun send-rules (view-only-items-in-current-ontology
                   include-base-ontology-p stream)
  (multiple-value-bind (rules-string ok-p)
      (rules-string view-only-items-in-current-ontology
                    include-base-ontology-p)
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~a~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
	     (all-ontologies)
             *ontology-info-character-separator*
             rules-string)
     stream)))

(defun instances-string (view-only-items-in-current-ontology
                         include-base-ontology-p
                         &optional (number-of-items 0) include-type&separator)
  (multiple-value-bind (instances ok-p new-number-of-items)
      (ocml::list-instances view-only-items-in-current-ontology
                            include-base-ontology-p number-of-items)
    (values 
     (if include-type&separator
         (format nil "~ainstances~a~{~{~a{~a{~a{~}[~}"
                 *ontology-info-character-separator*
                 *ontology-info-character-separator*
                 (sort instances #'(lambda (x y)
                                     (string< (car x) (car y)))))
         (format nil "~{~{~a{~a{~a{~}[~}"
                 (sort instances #'(lambda (x y)
                                     (string< (car x) (car y))))))
     ok-p new-number-of-items)))

(defun add-type-to-type-list (list type)
  (mapcar #'(lambda (x) (cons type x))
          list))

;;;make sure to send tasks and psms before classes and
;;;rules before relations
(defun all-types-string (view-only-items-in-current-ontology
                         include-base-ontology-p)
  (let (tasks problem-solving-methods
              classes axioms instances rules
              relations relation-instances functions procedures
              temp-ok (ok-p t) (number-of-items 0))
    (multiple-value-setq (tasks temp-ok number-of-items)
        (ocml::list-tasks view-only-items-in-current-ontology
                          include-base-ontology-p number-of-items))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (problem-solving-methods temp-ok number-of-items)
        (ocml::list-problem-solving-methods view-only-items-in-current-ontology
                          include-base-ontology-p number-of-items))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (classes temp-ok number-of-items)
        (ocml::list-classes view-only-items-in-current-ontology
                          include-base-ontology-p number-of-items))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (axioms temp-ok number-of-items)
        (ocml::list-axioms view-only-items-in-current-ontology
                          include-base-ontology-p number-of-items))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (relations temp-ok number-of-items)
        (ocml::list-relations view-only-items-in-current-ontology
                          include-base-ontology-p number-of-items))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (rules temp-ok number-of-items)
        (ocml::list-rules view-only-items-in-current-ontology
                          include-base-ontology-p number-of-items))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (functions temp-ok number-of-items)
        (ocml::list-functions view-only-items-in-current-ontology
                          include-base-ontology-p number-of-items))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (procedures temp-ok number-of-items)
        (ocml::list-procedures view-only-items-in-current-ontology
                          include-base-ontology-p number-of-items))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (instances temp-ok number-of-items)
        (ocml::list-instances view-only-items-in-current-ontology
                          include-base-ontology-p number-of-items))
    (setf ok-p (and ok-p temp-ok))
    (multiple-value-setq (relation-instances temp-ok number-of-items)
        (ocml::list-relation-instances view-only-items-in-current-ontology
                          include-base-ontology-p number-of-items))
    (setf ok-p (and ok-p temp-ok))
    (values 
     (format nil "~{~{~a{~}[~} "
             ;;put instances last because they are the least useful
             ;;relations first because they are used in slot names
	     (sort (append 
		           (add-type-to-type-list relations "relations")
                           (add-type-to-type-list tasks "tasks")
                           (add-type-to-type-list problem-solving-methods
                                                  "problem_solving_methods")
		           (add-type-to-type-list classes "classes")
                           (add-type-to-type-list axioms "axioms")
		           (add-type-to-type-list rules "rules")
		           (add-type-to-type-list functions "functions")
		           (add-type-to-type-list procedures "procedures")
                           (add-type-to-type-list instances "instances")
                           (add-type-to-type-list relation-instances "relation_instances")
                           )
		   #'(lambda (x y)
		       (string< (second x) (second y)))))
     ok-p)))

(defun send-instances (view-only-items-in-current-ontology
                          include-base-ontology-p stream)
  (multiple-value-bind (instances-string ok-p)
      (instances-string view-only-items-in-current-ontology
                        include-base-ontology-p )
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~a~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
	     (all-ontologies)
             *ontology-info-character-separator*
             instances-string)
     stream)))

(defun send-all-types (view-only-items-in-current-ontology
                       include-base-ontology-p stream)
  (multiple-value-bind (all-types-string ok-p)
      (all-types-string view-only-items-in-current-ontology
                          include-base-ontology-p)
    (http::princ-to-binary-stream
     (format nil "~(~a~)~a~{~(~a~)[~}~a~a~%"
             (ok-p-result ok-p)
             *ocml-line-separator*
	     (all-ontologies) ;;(current-ontology-includes)
             *ontology-info-character-separator*
             all-types-string)
     stream)))

(defun send-tree (type colour tree stream)
  (http::princ-to-binary-stream
   (format nil
            (format nil
                    "~~(~~a~~)~~a~~{~~{~a{~~a{~~a{~~a{~~a{~~d{~~d{~~d{~~d{~a~~}[~~}~~%"
		    type colour)
	    (ok-p-result (or (ocml::unlimited-p ocml::*list-length-limit*)
                             (< *number-of-tree-items* ocml::*list-length-limit*)))
            *ocml-line-separator*
            tree)
    stream))

(defun send-raw-tree (tree stream)
  (http::princ-to-binary-stream
    (format nil "~(~a~)~a~{~{~a{~a{~a{~a{~a{~d{~d{~d{~d{~a~}[~}~%"
	    (ok-p-result (or (ocml::unlimited-p ocml::*list-length-limit*)
                             (< *number-of-tree-items* ocml::*list-length-limit*)))
            *ocml-line-separator*
            tree)
    stream))

(defun send-ontology-tree (tree stream)
  (send-raw-tree tree stream))

(defun send-task-psm-tree (tree stream)
  (send-raw-tree tree stream))

(defmethod internal-draw-tree ((type (eql 'ocml::classes)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::class  name existing-classes existing-instances
                      orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::top_classes)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::class  name existing-classes existing-instances
                      orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::class)) name existing-classes
                               existing-instances orientation stream)
  (when (ocml::get-domain-class name)
    (send-tree "class" *html-class-colour*
	       (tree-draw-classes 
	        (ocml::get-domain-class name)
                existing-classes existing-instances
                orientation)
	       stream)
    t))

(defmethod internal-draw ((type (eql 'ocml::classes)) name existing-classes
                          existing-instances stream)
  (internal-draw 'ocml::class  name existing-classes
                 existing-instances stream))

(defmethod internal-draw ((type (eql 'ocml::top_classes)) name existing-classes
                          existing-instances stream)
  (internal-draw 'ocml::class  name existing-classes
                 existing-instances stream))

(defmethod internal-draw ((type (eql 'ocml::class)) name existing-classes
                          existing-instances stream)
  (when (ocml::get-domain-class name)
    (send-tree "class" *html-class-colour*
	       (tree-draw-class
		name (ocml::get-domain-class name)
                existing-classes existing-instances)
	       stream)
    t))

(defmethod internal-draw ((type (eql 'ocml::tasks)) name existing-classes
                                              existing-instances stream)
  (internal-draw 'ocml::task  name existing-classes
                 existing-instances stream))

(defmethod internal-draw ((type (eql 'ocml::task)) name existing-classes
                                              existing-instances stream)
  (declare (ignore existing-classes existing-instances))
  (when (task-p name)
    (send-tree "task" *task-colour*
	       (tree-draw-ocml-structure
		name (ocml::get-domain-class name))
	       stream)
    t))

(defmethod internal-draw ((type (eql 'ocml::problem_solving_methods)) name
                          existing-classes
                          existing-instances stream)
  (internal-draw 'ocml::problem-solving-method  name existing-classes
                 existing-instances stream))

(defmethod internal-draw ((type (eql 'ocml::problem-solving-method)) name
                          existing-classes
                          existing-instances stream)
  (declare (ignore existing-classes existing-instances))
  (when (problem-solving-method-p name)
    (send-tree "problem_solving_method" *problem-solving-method-colour*
	       (tree-draw-ocml-structure
		name (ocml::get-domain-class name))
	       stream)
    t))

(defmethod internal-draw ((type (eql 'ocml::ontologies)) name
                          existing-classes
                          existing-instances stream)
  (internal-draw 'ocml::ontology name existing-classes
                 existing-instances stream))

(defmethod internal-draw ((type (eql 'ocml::ontology)) name existing-classes
                          existing-instances stream)
  (declare (ignore existing-classes existing-instances))
  (when (ocml::get-ontology name)
    (send-tree "ontology" *ontology-colour*
	       (tree-draw-ocml-structure
		name (ocml::get-ontology name))
	       stream)
    t))

(defmethod internal-draw ((type (eql 'ocml::all)) name existing-classes
                          existing-instances stream)
  (cond ((task-p name)
         (internal-draw 'ocml::task name existing-classes
                        existing-instances stream))
        ((problem-solving-method-p name)
         (internal-draw 'ocml::problem-solving-method name existing-classes
                        existing-instances stream))
        ((ocml::get-domain-class name)
         (internal-draw 'ocml::class name existing-classes
                        existing-instances stream))
        ((ocml::get-ontology name)
         (internal-draw 'ocml::ontology name existing-classes
                        existing-instances stream))
        (t (internal-draw-tree type name nil existing-classes
                               existing-instances stream))))

(defmethod internal-draw-tree ((type (eql 'ocml::ontologies)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::ontology name existing-classes existing-instances
                      orientation stream))
  
(defmethod internal-draw-tree ((type (eql 'ocml::ontology)) name
                               existing-classes existing-instances orientation stream)
  (declare (ignore existing-classes existing-instances))
  (when (ocml::get-ontology name)
    (send-ontology-tree (tree-draw-ontology
		         (ocml::get-ontology name)
                         orientation)
	                stream)
    t))

;;;no longer used
;(defmethod internal-draw-tree ((type (eql 'ocml::instances)) name
;                               existing-classes existing-instances orientation stream)
;  (internal-draw-tree 'ocml::instance name
;                      existing-classes existing-instances orientation stream))
;
;(defmethod internal-draw-tree ((type (eql 'ocml::instance)) name
;                               existing-classes existing-instances orientation stream)
;  (declare (ignore orientation existing-classes existing-instances))
;  (when (ocml::find-instance name)
;    (send-tree "instance" *instance-colour*
;	       (tree-draw-ocml-structure
;		name (ocml::find-instance name))
;	       stream)
;    t))

(defmethod internal-draw-tree ((type (eql 'ocml::tasks)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::task name existing-classes existing-instances
                      orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::task)) name
                               existing-classes existing-instances orientation stream)
  (declare (ignore orientation existing-classes existing-instances))
  (when (ocml::get-domain-class name)
    (send-task-psm-tree
     (tree-draw-tasks
      (ocml::get-domain-class name) orientation)
     stream)
    t))

(defmethod internal-draw-tree ((type (eql 'ocml::problem_solving_methods)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::problem-solving-method name
                      existing-classes existing-instances orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::problem_solving_method)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::problem-solving-method name
                      existing-classes existing-instances orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::problem-solving-methods)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::problem-solving-method name
                      existing-classes existing-instances orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::problem-solving-method)) name
                               existing-classes existing-instances orientation stream)
  (declare (ignore orientation existing-classes existing-instances))
  (when (ocml::get-domain-class name)
    (send-task-psm-tree
     (tree-draw-problem-solving-methods
      (ocml::get-domain-class name) orientation)
	                stream)
    t))

(defmethod internal-draw-tree ((type (eql 'ocml::relations)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::relation name existing-classes existing-instances
                      orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::relation)) name
                               existing-classes existing-instances orientation stream)
  (declare (ignore orientation existing-classes existing-instances))
  (when (ocml::get-ocml-relation name)
    (send-tree "relation" *relation-colour*
                                  (tree-draw-ocml-structure
                                   name (ocml::get-ocml-relation name))
                              stream)
    t))

(defmethod internal-draw-tree ((type (eql 'ocml::relation_instances)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::relation_instance name existing-classes existing-instances
                      orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::relation_instance)) name
                               existing-classes existing-instances orientation stream)
  (declare (ignore orientation existing-classes existing-instances))
  (let ((relation-instance (ocml::get-relation-instance name)))
    (when relation-instance
      (send-tree "relation_instance" *relation-instance-colour*
                 (tree-draw-ocml-structure
                  (format nil "~(~a~)" name) relation-instance)
                 stream)
      t)))


(defmethod internal-draw-tree ((type (eql 'ocml::axioms)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::axiom name existing-classes existing-instances
                      orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::axiom)) name
                               existing-classes existing-instances orientation stream)
  (declare (ignore orientation existing-classes existing-instances))
  (when (ocml::get-axiom name)
    (send-tree "axiom" *axiom-colour*
                                  (tree-draw-ocml-structure
                                   name (ocml::get-axiom name))
                                  stream)
    t))

(defmethod internal-draw-tree ((type (eql 'ocml::procedures)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::procedure name existing-classes existing-instances
                      orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::procedure)) name
                               existing-classes existing-instances orientation stream)
  (declare (ignore orientation existing-classes existing-instances))
  (when (ocml::get-ocml-function name)
    (send-tree "procedure" *procedure-colour*
	       (tree-draw-ocml-structure
		name (ocml::get-ocml-function name))
	       stream)
    t))

(defmethod internal-draw-tree ((type (eql 'ocml::functions)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::function name
                      existing-classes existing-instances orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::function)) name
                               existing-classes existing-instances orientation stream)
  (declare (ignore orientation existing-classes existing-instances))
  (when (ocml::get-ocml-function name)
    (send-tree "function" *function-colour*
                                  (tree-draw-ocml-structure
                                   name (ocml::get-ocml-function name))
                              stream)
    t))

(defmethod internal-draw-tree ((type (eql 'ocml::rules)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::rule name existing-classes existing-instances
                      orientation stream))

(defmethod internal-draw-tree ((type (eql 'ocml::rule)) name
                               existing-classes existing-instances
                               orientation stream)
  (declare (ignore orientation existing-classes existing-instances))
  (when (ocml::get-rule name)
    (send-tree "rule" *rule-colour*
                                  (tree-draw-ocml-structure
                                   name (ocml::get-rule name))
                              stream)
    t))

(defmethod internal-draw-tree ((type (eql 'ocml::all)) name
                               existing-classes existing-instances orientation stream)
  (internal-draw-tree 'ocml::unknown name existing-classes existing-instances
                      orientation stream))

(defun task-p (name)
  (let ((structure (ocml::get-domain-class name)) superclasses)
    (and structure
         (setf superclasses (mapcar #'ocml::name (ocml::domain-superclasses structure)))
         (find 'ocml::task superclasses)
         (not (find 'ocml::problem-solving-method superclasses)))))

(defun executable-task-p (name)
  (let ((structure (ocml::get-domain-class name)) superclasses)
    (and structure
         (setf superclasses (mapcar #'ocml::name
                                    (ocml::domain-superclasses structure)))
         (find 'ocml::task superclasses)
         (find 'ocml::executable-task superclasses)
         (not (find 'ocml::problem-solving-method superclasses)))))

(defun problem-solving-method-p (name)
  (let ((structure (ocml::get-domain-class name)))
    (and structure
         (find 'ocml::task (mapcar #'ocml::name (ocml::domain-superclasses structure))))))
         

(defmethod internal-draw-tree ((type (eql 'ocml::unknown)) name
                               existing-classes existing-instances orientation stream)
  (cond ((task-p name)
         (send-task-psm-tree (tree-draw-tasks
		              (ocml::get-domain-class name)
                              orientation)
		             stream))
        ((problem-solving-method-p name)
         (send-task-psm-tree 
	  (tree-draw-problem-solving-methods
	   (ocml::get-domain-class name)
	   orientation)
	  stream))
        ((ocml::get-domain-class name)
	 (send-tree "class" "green"
		    (tree-draw-classes 
		     (ocml::get-domain-class name)
                     existing-classes existing-instances
                     orientation)
		    stream))
	((ocml::get-ontology name)
	 (send-ontology-tree (tree-draw-ontology
		              (ocml::get-ontology name)
                              orientation)
		             stream))
        ((ocml::find-current-instance name)
         (send-tree "instance" *instance-colour*
	            (tree-draw-ocml-structure
		     name (ocml::find-current-instance name)
                     existing-classes existing-instances)
	            stream))
        ((ocml::get-ocml-relation name)
         (send-tree "relation" *relation-colour*
		    (tree-draw-ocml-structure
		     name (ocml::get-ocml-relation name))
		    stream))
        ((ocml::get-relation-instance name)
         (send-tree "relation_instance" *relation-instance-colour*
		    (tree-draw-ocml-structure
		     name (ocml::get-relation-instance name))
		    stream))
        ((ocml::get-axiom name)
         (send-tree "axiom" *axiom-colour*
		    (tree-draw-ocml-structure
		     name (ocml::get-axiom name))
		    stream))
        ((ocml::get-ocml-function name)
         (if (ocml::procedure-p (ocml::get-ocml-function name))
             (send-tree "procedure" *procedure-colour*
	                (tree-draw-ocml-structure
		         name (ocml::get-ocml-function name))
	                stream)
             (send-tree "function" *function-colour*
			(tree-draw-ocml-structure
			 name (ocml::get-ocml-function name))
			stream)))
        ((ocml::get-rule name)
         (send-tree "rule" *rule-colour*
		    (tree-draw-ocml-structure
		     name (ocml::get-rule name))
		    stream))
	(t (http::princ-to-binary-stream
	    (format nil "class_or_ontology_does_not_exist~%")
	    stream))))

(defmethod internal-draw (type name existing-classes
                               existing-instances stream)
  (internal-draw-tree type name nil existing-classes
                      existing-instances stream))

(defvar *number-of-tree-items*)

(defun draw-tree (stream upcase-string)
  (let* ((*package* (find-package "OCML")))
    (with-input-from-string (istream upcase-string)
      (read istream)
      (let* ((ocml::*list-length-limit* (read istream))
             (ontology-name (read istream))
             (existing-classes (read istream))
             (existing-instances (read istream))
             (type (read istream))
             (ontology (get-ontology ontology-name))
	     (name (read istream))
             (info (read istream))
             (*number-of-tree-items* 0)
             (orientation))
        (let ((*package* (find-package "KEYWORD")))
          (setf orientation (read istream)))
        (cond (ontology
               (ocml::select-ontology ontology-name)
               (cond ((or (eq type 'ocml::instance)
                          (eq type 'ocml::instances))
                      (internal-draw-instance name info existing-classes
                                              existing-instances stream))
                     (t (or (internal-draw-tree type name
                                                existing-classes existing-instances
                                                orientation stream)
                            (internal-draw-tree 'ocml::unknown name
                                                existing-classes existing-instances
                                                orientation
                                                stream)))))
              (t (send-ontology-does-not-exist stream)))))))

(defmethod internal-draw-instance (name class-name existing-classes
                                        existing-instances stream)
  (let ((class (ocml::get-domain-class class-name))
        instance-structure)
    (when class
      (setf instance-structure
            (ocml::find-current-direct-instance class name))
      (when instance-structure
        (send-tree "instance" *instance-colour*
	           (tree-draw-instance
		    name class-name existing-classes existing-instances)
	           stream)))))

(defun draw (stream upcase-string)
  (let* ((*package* (find-package "OCML")))
    (with-input-from-string (istream upcase-string)
      (read istream)
      (let* ((ocml::*list-length-limit* (read istream))
             (*number-of-tree-items* 0)
             (ontology-name (read istream))
             (existing-classes (read istream))
             (existing-instances (read istream))
             (type (read istream))
             (ontology (get-ontology ontology-name))
	     (name (read istream))
             (info (read istream)))
        (cond (ontology
               (ocml::select-ontology ontology-name)
               (cond ((or (eq type 'ocml::instance)
                          (eq type 'ocml::instances))
                      (internal-draw-instance name info existing-classes
                                              existing-instances stream))
                     (t (or (internal-draw type name existing-classes
                                           existing-instances stream)
                            (internal-draw 'ocml::unknown name existing-classes
                                           existing-instances stream)))))
              (t (send-ontology-does-not-exist stream)))))))

(defun flash (stream string)
  (setf xx stream)
  (setf a string)
  (http::princ-to-binary-stream
   ;;(setf b (format nil  #\null))
   (setf b
         (format nil "<?xml version \"1.0\"?><author>john</author>~a"
                  #\null
                 ))
   stream)
  (force-output stream))
