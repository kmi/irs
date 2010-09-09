(in-package wsmo-protocol)

(defun ip::goal-details (ontology method html-stream &optional (lookup-function
                                                                #'ocml::irs-lookup-current-word))
  (ip::task-details ontology method html-stream lookup-function))

(defun ip::web-service-details (ontology method html-stream &optional (lookup-function
                                                                       #'ocml::irs-lookup-current-word))
  (ip::task-details ontology method html-stream lookup-function))

(defun ip::mediator-details (ontology method html-stream &optional (lookup-function
                                                                    #'ocml::irs-lookup-current-word))
  (ip::task-details ontology method html-stream lookup-function))

(defvar *standard-other-goal-slots*
  '(ocml::used-mediator ocml::has-effect ocml::has-post-condition))

(defun html-describe-class-slots-p (x)
  (and x (ocml::get-domain-class x)
       (ocml::domain-slots (ocml::get-domain-class x))))

;;defined for the delivery version of the IRS server
;;for some reason deleted classes get through
(defun check-class-and-find-all-web-services-which-solve-goal (goal-name)
  (and (ocml::get-domain-class goal-name)
       (find-all-web-services-which-solve-goal goal-name)))

(defun ocml::describe-goal-internals (goal-name class ontology stream
                                                &optional
                                                (lookup-function
                                                 #'ocml::mnm-lookup-current-word))
  (let ((associated-web-services 
         (check-class-and-find-all-web-services-which-solve-goal goal-name))
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
            (describe-wsmo-slot slot class ontology stream
                                 lookup-function))
      (format stream "<br>"))
    (when output-role
      (format stream "~A" (http::bold "Output Role:"))
      (describe-wsmo-slot output-role class ontology stream
                           lookup-function)
      (format stream "<br>"))
    (when other-slots
      (format stream "~A" (http::bold "Other Slots:"))
      (loop for slot in other-slots
            do
            (describe-wsmo-slot slot class ontology stream
                                 lookup-function))
      (format stream "<br>"))
    (when (html-describe-class-slots-p non-functional-properties)
      (format stream "~A" (http::bold "Non Functional Properties:"))
      (describe-wsmo-class-slots non-functional-properties 
                                 ontology stream lookup-function
                                 #'(lambda (class slot) (ip::get-class-slot-value class slot))))))

(defun describe-wsmo-slot (slot class ontology stream 
                           &optional
                           (lookup-function
                            #'mnm-lookup-current-word))
  (format
   stream
   (http::internal-insert-ocml-links
    (format nil "<br>~a<b>~(~a~)</b>" 
            (ocml::insert-spaces 4) (ocml::add-html-relation-colour slot))
    lookup-function ontology))
  (multiple-value-bind (values defaults)
      (ocml::get-slot-values-from-class-structure
       class slot )
    (when values
      (format stream 
              (http::internal-insert-ocml-links
               (format nil "<br>~a<b><i>~:(~a~)</i></b>~{~a~}"
                       (ocml::insert-spaces 5)
                       (ocml::add-html-slot-option-colour :value)
                       (mapcar #'(lambda (x) 
                                   (ocml::format-ocml-value x 1 nil))
                               values))
               lookup-function
               ontology)))
    (when defaults
      (format stream
              (http::internal-insert-ocml-links
               (format nil "<br>~a<b><i>~:(~a~):</i></b>~{~a~}"
                       (ocml::insert-spaces 5)
                       (ocml::add-html-slot-option-colour :default-value)
                       (mapcar #'(lambda (x) 
                                   (ocml::format-ocml-value x 1 nil))
                               defaults))
               lookup-function
               ontology)))
    (let ((type-info (remove-duplicates
                      (ocml::find-option-value class slot :type))))
      (when type-info
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "<br>~a<b><i>~:(~a~):</i></b>~{~a~}"
                         (ocml::insert-spaces 5)
                         (ocml::add-html-slot-option-colour :type)
                         (mapcar #'(lambda (x) 
                                     (ocml::format-ocml-value x 1 nil))
                                 type-info))
                 lookup-function
                 ontology))))
    (loop for option in '(:min-cardinality
                          :max-cardinality
                          :inheritance)
          for value = (ocml::find-option-value class slot option)
          when value
          do
          (format stream
                  (http::internal-insert-ocml-links
                   (format nil "<br>~a<b><i>~:(~a~):</i></b> ~:(~a~)"
                           (ocml::insert-spaces 5)
                           (ocml::add-html-slot-option-colour option)
                           value)
                   lookup-function
                   ontology)))
    (let ((doc (ocml::find-slot-documentation class slot)))
      (when doc
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "<br>~a<b><i>~:(~a~):</i></b> <i>~a</i>"
                         (ocml::insert-spaces 5) 
                         (ocml::add-html-slot-option-colour :documentation) doc)
                 lookup-function
                 ontology))))))

(defun describe-wsmo-class-slots (class-name 
                                  ontology stream lookup-function
                                  &optional slot-test)
  (let ((class (ocml::get-domain-class class-name)))
    (when class
      (dolist (slot (ocml::domain-slots class))
        (when (or (null slot-test)
                  (funcall slot-test class-name slot))
          (describe-wsmo-slot slot class ontology stream
                               lookup-function))))))

(defun ocml::describe-web-service-internals (web-service-name class ontology stream
                                                       &optional
                                                       (lookup-function
                                                        #'ocml::mnm-lookup-current-word))
  (let* ((input-roles (input-roles web-service-name))
        (output-role (output-role web-service-name))
        (host 
         (web-onto::findany
          '?x `(ocml::wsmo-web-service-host ,web-service-name ?x)))
        (port
         (web-onto::findany
          '?x `(ocml::wsmo-web-service-port ,web-service-name ?x)))
        (location 
         (web-onto::findany
          '?x `(ocml::wsmo-web-service-location ,web-service-name ?x)))
        (non-functional-properties
         (web-onto::findany 
          '?x 
          `(= ?x (ocml::the-class-slot-value ,web-service-name 
                                             ocml::has-non-functional-properties))))
        (capability
         (web-onto::findany 
          '?x 
          `(= ?x (ocml::the-class-slot-value ,web-service-name 
                                             ocml::has-capability))))
        (mediators
         (web-onto::findany 
          '?x 
          `(= ?x (ocml::all-class-slot-values ,web-service-name 
                                             ocml::used-mediator))))
        (interface
         (web-onto::findany 
          '?x 
          `(= ?x (ocml::the-class-slot-value ,web-service-name 
                                             ocml::has-interface))))
        (orchestration
         (when (and interface (ocml::get-domain-class interface))
           (web-onto::findany 
            '?x 
            `(= ?x (ocml::the-class-slot-value ,interface 
                                               ocml::has-orchestration)))))
        (choreography
         (when (and interface (ocml::get-domain-class interface))
           (web-onto::findany 
            '?x 
            `(= ?x (ocml::the-class-slot-value ,interface 
                                               ocml::has-choreography)))))
        (interface-mediators
         (when (and interface (ocml::get-domain-class interface))
           (web-onto::findany 
            '?x 
            `(= ?x (ocml::the-class-slot-value ,interface 
                                               ocml::used-mediator))))))
    (when host
      (format stream "~A" (http::bold "Publisher URL:"))
      (cond ((and port location)
             (format stream " http://~a:~a~a" host port location))
            (location
             (format stream " http://~a~a" host location))
            (port
             (format stream " http://~a:~a" host port))
            (t (format stream " http://~a" host)))
      (format stream "<br>"))
    (when input-roles
      (format stream "~A" (http::bold "Input Roles:"))
      (loop for slot in input-roles
            do
            (describe-wsmo-slot slot class ontology stream
                                 lookup-function))
      (format stream "<br>"))
    (when output-role
      (format stream "~A" (http::bold "Output Role:"))
      (describe-wsmo-slot output-role class ontology stream
                           lookup-function)
      (format stream "<br>"))    
    (when mediators
      (format stream "~A" (http::bold "Mediators:"))
      (dolist (mediator mediators)
        (describe-wsmo-slot mediator class ontology stream lookup-function))
      (format stream "<br>"))
    (when (html-describe-class-slots-p capability)
      (format stream "~A" (http::bold "Capability:"))
      (describe-wsmo-class-slots capability
                                          ontology stream lookup-function)
      (format stream "<br>"))
    (when (or (html-describe-class-slots-p orchestration)
              (html-describe-class-slots-p choreography)
              (html-describe-class-slots-p interface-mediators))
      (format stream "~A" (http::bold "Interface Components:<br>"))
      (when (html-describe-class-slots-p orchestration)
        (format stream "~A" (http::bold (http::italic "Orchestration:")))
        (describe-wsmo-class-slots orchestration 
                                            ontology stream lookup-function)
        (format stream "<br>"))
      (when (html-describe-class-slots-p choreography)
        (format stream "~A" (http::bold (http::italic "Choreography:")))
        (describe-wsmo-class-slots choreography
                                            ontology stream lookup-function)
        (format stream "<br>"))
      (when (html-describe-class-slots-p interface-mediators)
        (format stream "~A" (http::bold (http::italic "Interface Mediators:")))
        (describe-wsmo-class-slots interface-mediators
                                            ontology stream lookup-function)
        (format stream "<br>")))
    (when (html-describe-class-slots-p non-functional-properties)
      (format stream "~A" (http::bold (http::italic "Non Functional Properties:")))
      (describe-wsmo-class-slots non-functional-properties 
                                          ontology stream lookup-function
                                          #'(lambda (class slot) (ip::get-class-slot-value class slot))))))

(defun ocml::describe-mediator-internals (mediator-name class ontology stream
                                                        &optional
                                                        (lookup-function
                                                         #'ocml::mnm-lookup-current-word))
  (let* ((non-functional-properties
          (web-onto::findany 
           '?x 
           `(= ?x (ocml::the-class-slot-value ,mediator-name 
                                              ocml::has-non-functional-properties))))
        
         (main-slots (remove 'ocml::has-non-functional-properties
                              (ocml::domain-slots class))))
    (when main-slots
      (format stream "~A" (http::bold "Main Slots:"))
      (loop for slot in main-slots
            do
            (describe-wsmo-slot slot class ontology stream
                                 lookup-function))
      (format stream "<br>"))
    (when (html-describe-class-slots-p non-functional-properties)
      (format stream "~A" (http::bold "Non Functional Properties:"))
      (describe-wsmo-class-slots non-functional-properties 
                                          ontology stream lookup-function
                                          #'(lambda (class slot) (ip::get-class-slot-value class slot))))))


