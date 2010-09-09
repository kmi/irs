(in-package wsmo-protocol)

(defun ocml::describe-goal-internals (goal-name class ontology stream
                                                &optional
                                                (lookup-function
                                                 #'ocml::mnm-lookup-current-word))

  (let (
        (input-roles (input-roles goal-name)))
    (format stream "associated-web services")))


#|
(defun ocml::describe-goal-internals (goal-name class ontology stream
                                                &optional
                                                (lookup-function
                                                 #'ocml::mnm-lookup-current-word))

  (let ((associated-web-services 
         (find-all-web-services-which-solve-goal goal-name))
        (input-roles (input-roles goal-name))
        (output-role (output-role goal-name))
        (other-slots (intersection (ocml::domain-slots class)
                                   *standard-other-goal-slots*))
        (non-functional-properties
         (web-onto::findany 
          '?x 
          `(= ?x (ocml::the-class-slot-value ,goal-name 
                                             ocml::has-non-functional-properties)))))
    (when associated-web-services
      (let ((associated-web-services-info 
             (mapcar #'(lambda (x)
                           (list (ocml::name x) (ocml::name (ocml::home-ontology x))))
                     associated-web-services)))
        (format stream "~A" (http::bold "Associated Web Services: "))
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "~(~{~a (~a)~}~{~{; ~a (~a)~}~}~)" 
                         (car associated-web-services-info)
                         (cdr associated-web-services-info))
                 lookup-function ontology))
        (format stream "<br>")))
    (when input-roles
      (format stream "~A" (http::bold "Input Roles:"))
      (loop for slot in input-roles
            do
            (ocml::describe-slot slot class ontology stream
                                 lookup-function))
      (format stream "<br>"))
    (when output-role
      (format stream "~A" (http::bold "Output Role:"))
      (ocml::describe-slot output-role class ontology stream
                           lookup-function)
      (format stream "<br>"))
    (when other-slots
      (format stream "~A" (http::bold "Other Slots:"))
      (loop for slot in other-slots
            do
            (ocml::describe-slot slot class ontology stream
                                 lookup-function))
      (format stream "<br>"))
    (when (html-describe-class-slots-p non-functional-properties)
      (format stream "~A" (http::bold "Non Functional Properties:"))
      (describe-wsmo-class-slots non-functional-properties 
                                 ontology stream lookup-function))))
|#