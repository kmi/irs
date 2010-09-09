(in-package wsmo-protocol)

;;; {{{ OCML tools
(defun make-ocml-object (x)
  (cond ((or (numberp x) (stringp x)) x)
        ((and (atom x) (symbolp x))
         (intern (symbol-name x) (find-package "OCML")))
        ((listp x)
         (cons (make-ocml-object (car x))
               (make-ocml-object (cdr x))))
        (t (let ((*package* (find-package "OCML")))
             (read-from-string (format nil "~a" X))))))

(defun ocml::make-ocml-object (x)
  (make-ocml-object x))
;;; }}}

(defun ip::get-goal-info (ontology web-service-name html-stream)
 ;; (setf o ontology w web-service-name)
  (multiple-value-bind (input-roles output)
      (internal-get-goal-info ontology web-service-name)
      (if (eq input-roles 'no-goal)
          (format html-stream "error: no-goal for ~a~%" web-service-name)
        (format html-stream "(~s ~s)~%" input-roles output))))


(defun internal-get-goal-info (ontology web-service-name)
  ;;;(setf o ontology m web-service-name)
  (ocml::select-ontology ontology)
  (let* ((goal (ocml::findany 
                '?x
                `(ocml::associated-goal ,web-service-name ?x))))
    (if (ocml:nothing? goal)
        'no-goal
      (let* ((input-roles-with-soap-bindings
              (ocml::setofall '?x `(ocml::HAS-wsmo-INPUT-soap-binding ,goal ?x)))
             (output-role-with-soap-binding
              (ocml::findany '?x 
                             `(ocml::has-wsmo-output-soap-binding ,goal ?x)))
             (output-type (second output-role-with-soap-binding)))
        (values input-roles-with-soap-bindings output-type)))))


(defun ip::get-web-service-operation-info (ontology web-service-name operation-name html-stream)
  (ocml::select-ontology ontology)
  (let* ((grounding-operation-mappings
         (web-onto::findany '?x `(ocml::associated-operation-mappings ,web-service-name ?x)))
        (operation-io-mappings
         (assoc operation-name grounding-operation-mappings))
        (input-roles-with-soap-bindings
         (third operation-io-mappings))
        (output-type
         (fourth operation-io-mappings)))
    (if (and input-roles-with-soap-bindings output-type)
        (format html-stream "(~s ~s)~%" input-roles-with-soap-bindings output-type)
      (format html-stream "error: no info for ~a ~a ~a~%" ontology web-service-name operation-name))))
  

(defun ocml::irs-run-ocml-expression (ontology web-service)
  ;;(setf xoo ontology xxw web-service)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((instance (ocml::find-current-instance web-service)))
      (when instance
        (ocml::select-ontology (ocml::name (ocml::home-ontology instance)))
        (let* ((goal-instance
                (web-onto::findany 
                 '?x 
                 `(ocml::suitable-web-service ?x ,web-service)))
               (variables
                (web-onto::findany 
                 '?x 
                 `(ocml::has-variables ,goal-instance ?x)))
               (expression
                (web-onto::findany 
                 '?x 
                 `(ocml::has-expression ,goal-instance ?x))))
          ;;(setf vv variables
            ;;    ee expression)
         (ocml::setofall (make-ocml-object variables) (make-ocml-object expression)))))))