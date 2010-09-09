(in-package wsmo-protocol)

(defmacro ocml::def-guarded-transition (name choreography &optional documentation &rest args)
  `(ocml::def-rule-internal ',name ',documentation (append (list :packet ',choreography) ',args)))

(defun define-guarded-transition (name choreography documentation body)
  (ocml::def-rule-internal name documentation (append (list :packet choreography) body)))

#+:lispworks 
(editor::setup-indent 'ocml::def-guarded-transition 0 2)



(defun compile-choreography-guarded-transitions (choreography)
  (let ((guarded-transitions
         (web-onto::findany 
          '?x 
          `(= ?x (ocml::the-class-slot-value ,choreography 
                                             ocml::has-guarded-transitions))))
        (rule-packet (ocml::get-rule-packet choreography)))
    (when rule-packet
        (ocml::remove-all-fc-rules-in-packet rule-packet))
      (mapc #'(lambda (guarded-transition)
                (let ((name (car guarded-transition)))
                  (define-guarded-transition 
                   name
                   choreography
                   (format nil "~:(~a~) guarded transition for choreography ~:(~a~)"
                           name choreography)
                   (cdr guarded-transition))))
            guarded-transitions)))

(defun create-choreography-ontology (current-ontology web-service)
  (let ((new-ontology-name (intern (format nil "~a-CHOREOGRAPHY" current-ontology)
                                   (find-package "OCML"))))
    (eval `(ocml::def-ontology-internal ',new-ontology-name
                                        ,(format nil  "Ontology to run choreography for ~a" web-service)
                                        '(:includes (,current-ontology)
                                          :type :web-service
                                          :author "john")))
    new-ontology-name))

(defun run-choreography (choreography name web-service input-role-value-pairs
                                      host port location output-type values 
                                      input-roles-and-soap-bindings 
                                      associated-grounding)
  (let* ((*choreography-result* nil)
         (current-ontology (ocml::name ocml::*current-ontology*))
         (choreography-run-ontology 
          (create-choreography-ontology current-ontology web-service))
         result)
    (declare (special *choreography-result*))
    (ocml::select-ontology choreography-run-ontology) 
    (compile-choreography-guarded-transitions choreography)
    (ocml::tell1 `(ocml::init-choreography))
    (assert-choreography-state-instance name web-service input-role-value-pairs
                                        host port location output-type values
                                        input-roles-and-soap-bindings
                                        associated-grounding)
    (ocml::run (list choreography))
    (setf result 
          (ocml::findany '?x 
                         `(ocml::has-choreography-result ,wp::*choreography-state-name* ?x)))
    (ocml::select-ontology current-ontology)
    (ocml::delete-ontology choreography-run-ontology)
    ;;(setf aa choreography-run-ontology)
    result))

(defun assert-choreography-state-instance (name web-service input-role-value-pairs
                                                host port location 
                                                output-type values
                                                input-roles-and-soap-bindings
                                                associated-grounding)
  ;;(setf xo (list web-service host port location output-type values))
  ;;(format t "assert-choreography-state-instance ~a~%" (ocml::name ocml::*current-ontology*))
  (let ((web-service-class-name (ocml::findany '?c `(= ?c (ocml::the-parent ,web-service)))))
    (ocml::define-domain-instance wp::*choreography-state-name*
                                  'ocml::choreography-state
                                  ""
                                  `((ocml::has-remote-name ,name)
                                    (ocml::has-web-service-class ,web-service-class-name)
                                    (ocml::has-web-service-instance ,web-service)
                                    (ocml::has-input-role-value-pairs ,input-role-value-pairs)
                                    (ocml::has-web-service-host ,host)
                                    (ocml::has-web-service-port ,port)
                                    (ocml::has-web-service-location ,location)
                                    (ocml::has-output-type ,output-type)
                                    (ocml::has-input-roles-and-soap-bindings 
                                     ,input-roles-and-soap-bindings)
                                    (ocml::has-values ,values)
                                    (ocml::has-grounding
                                     ,associated-grounding)))))


(defun ocml::add-new-values-to-choreography-state
       (instance-name new-input-roles-and-soap-bindings new-invocation-values)
  (let ((new-input-role-value-pairs
         (mapcar #'(lambda (input-role-and-soap-binding value)
                     (list (car input-role-and-soap-binding) value))
                 new-input-roles-and-soap-bindings new-invocation-values))
        (input-role-value-pairs
         (web-onto::findany '?x
                            `(ocml::has-input-role-value-pairs ,instance-name ?x)))
        (invocation-values
         (web-onto::findany '?x
                            `(ocml::has-values ,instance-name ?x)))
        (input-roles-and-soap-bindings
         (web-onto::findany '?x
                            `(ocml::has-input-roles-and-soap-bindings ,instance-name ?x))))
    (ocml::tell1 `(ocml::has-input-role-value-pairs 
                   ,instance-name
                   ,(append input-role-value-pairs new-input-role-value-pairs)))
    (ocml::tell1 `(ocml::has-values
                   ,instance-name
                   ,(append invocation-values new-invocation-values)))
    (ocml::tell1 `(ocml::has-input-roles-and-soap-bindings
                   ,instance-name
                   ,(append input-roles-and-soap-bindings new-input-roles-and-soap-bindings)))))
