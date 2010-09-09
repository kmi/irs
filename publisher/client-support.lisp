(in-package cl-user)


(defun call-remote-irs-method (ontology method-name &rest arguments)
  (let ((method-id (intern (concatenate 'string
					    (symbol-name ontology) "_" 
					    (symbol-name method-name)))))
    (irssoap-http-client 'call-remote-irs-method
		       '((ontology "sexpr")(method-name "sexpr")(arguments "sexpr"))
		       (list ontology method-id arguments)
                   :host *irs-host*
                   :port *irs-port*)))

(defun achieve-task-through-irs (ontology task-type &rest input-role-value-pairs)
  (internal-achieve-task-through-irs ontology task-type input-role-value-pairs
                                     nil))

(defun achieve-task-through-irs-soap-response (ontology task-type 
                                                        &rest 
                                                        input-role-value-pairs)
  (internal-achieve-task-through-irs ontology task-type input-role-value-pairs
                                     t))

(defun internal-achieve-task-through-irs (ontology task-type 
                                                   input-role-value-pairs 
                                                   soap-response-p)
  (irssoap-http-client (if soap-response-p
                           'achieve-task-soap-response
                         'achieve-task)
                       `((ontology "sexpr")(task-type "sexpr")
                         (number-of-input-role-value-pairs "int")
                         ,@(mapcan 
                            #'(lambda (role-value-pair)
                                (declare (ignore role-value-pair))
                                (list (list 'input-role-name 
                                            "sexpr")
                                      (list 'input-role-value 
                                            "sexpr")))
                            input-role-value-pairs))
                       (append (list ontology task-type 
                                     (length input-role-value-pairs))
                               (apply #'append 
                                      input-role-value-pairs))
                       :host *irs-host*
                       :port *irs-port*))

(defun achieve-goal-through-irs (ontology goal-type &rest input-role-value-pairs)
  (internal-achieve-goal-through-irs ontology goal-type input-role-value-pairs
                                     nil))

(defun achieve-goal-through-irs-soap-response (ontology goal-type 
                                                        &rest 
                                                        input-role-value-pairs)
  (internal-achieve-goal-through-irs ontology goal-type input-role-value-pairs
                                     t))

(defun internal-achieve-goal-through-irs (ontology goal-type 
                                                   input-role-value-pairs 
                                                   soap-response-p)
  (irssoap-http-client (if soap-response-p
                           'achieve-goal-soap-response
                         'achieve-goal)
                       `((ontology "sexpr")(goal-type "sexpr")
                         (number-of-input-role-value-pairs "int")
                         ,@(mapcan 
                            #'(lambda (role-value-pair)
                                (declare (ignore role-value-pair))
                                (list (list 'input-role-name 
                                            "sexpr")
                                      (list 'input-role-value 
                                            "sexpr")))
                            input-role-value-pairs))
                       (append (list ontology goal-type 
                                     (length input-role-value-pairs))
                               (apply #'append 
                                      input-role-value-pairs))
                       :host *irs-host*
                       :port *irs-port*))

;;(achieve-task-through-irs 'ocml::exchange-rate-provider-ontology 'ocml::exchange-rate-provision '(ocml::has-source-currency us-dollar) '(ocml::has-target-currency pound))



