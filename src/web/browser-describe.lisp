;;; Copyright © 2007, 2008 The Open University

;;; Rationale: a (sigh) new half-replication of the
;;; html-describe-stuff functions, tailored to the irs-browser

;;; Everything (almost) which was a MEANINGFUL link in the java irs
;;; browser, is also a javascript link here

;;; TODO: a high level representation of the 'ocml descriptions', so
;;; that nobody will have to replicate code anymore

;;; Need refinement on some css selectors, and on the descriptions of
;;; tasks, goals etc.. (more examples needed!)

(in-package ocml)

;; two functions to create dynamic links

;;tx to dave's code  --> from irs.web
(defun my-link (symbol ontology)
  (let* ((str (if (stringp symbol)
                  symbol
                  (if (symbolp symbol)
                      (symbol-name symbol)
                    (format nil "~a" symbol))))
         (word-to-lookup (intern (string-upcase str) :ocml)))
    (multiple-value-bind (found-p home-ontology-p colour home-ontology type)
        (ocml::ocml-lookup word-to-lookup ontology)
      (declare (ignore home-ontology-p colour))
      (if found-p
          (format nil "<span class=\"~A\" onclick=\"inspect('~a', '~a', '~a');\" style=\"cursor: pointer;\">~a</span>" type word-to-lookup home-ontology type (string-capitalize word-to-lookup))
          ;; if not found (just output as it is - no css refs)
          (format nil "~a" symbol)))))

(defun irsbrowser-format-ocml-value (x number-of-spaces ontology &optional (newline-p t))
  (if (listp x)
      (format nil (format nil "<code><br>~~~dt~~:w</code>" 
                          number-of-spaces)
              x)  ;;not sure what this does
    (if (stringp x)        
        (format nil "~:[~;<br>~]~a ~a" 
                newline-p (insert-spaces number-of-spaces) (format nil "~a"  (my-link x ontology)))
      (format nil "~:[~;<br>~]~a ~a" 
              newline-p (insert-spaces number-of-spaces) (format nil "~a" (my-link x ontology))))))

;;; {{{ Slots

(defun irsbrowser-describe-slot (slot class ontology stream 
                           &optional
                           (lookup-function
                            #'mnm-lookup-current-word))
  (declare (ignore lookup-function))
  (format stream
          (format nil "<br />~a<b>~a</b>" 
            (insert-spaces 4) (my-link slot ontology)))

  (multiple-value-bind (values defaults)
      (get-slot-values-from-class-structure
       class slot )
    (when values
      (format stream "<br />~a<span class=\"SLOT-OPTION\">~a:</span> ~{~a~}"   ;; this is static also in the java browser
              (insert-spaces 5)
              "Value" 
              (mapcar #'(lambda (x) 
                                  (irsbrowser-format-ocml-value x 1 ontology nil))  ;;i added ontology
                               values)))  ;; does it work?     

    (when defaults
      (format stream  "<br />~a<span class=\"SLOT-OPTION\">~a:</span> ~{~a~}"
              (insert-spaces 5)
              "Default-Value"
              (mapcar #'(lambda (x) 
                                   (irsbrowser-format-ocml-value x 1 ontology nil))
                               defaults)))

    (let ((type-info (remove-duplicates
                      (find-option-value class slot :type))))
      (when type-info
        (format stream  "<br />~a<span class=\"SLOT-OPTION\"><i>~a:</i></span> ~{~a~}"
                (insert-spaces 5)
                "Type"
                (mapcar #'(lambda (x) 
                            (irsbrowser-format-ocml-value x 1 ontology nil))
                        type-info))))

    (loop for option in '(:min-cardinality
                          :max-cardinality
                          :inheritance)
          for value = (find-option-value class slot option)
          when value
          do
          (format stream  "<br />~a<span class=\"SLOT-OPTION\"><i>~:(~a~):</i></span> ~a"  ;;maybe more specific css names
                   (insert-spaces 5)
                   option
                   value))  ;;here no links


    (let ((doc (find-slot-documentation class slot)))
      (when doc
        (format stream "<br />~a<b><i><span class=\"SLOT-OPTION\">~a:</span></i></b> <span class=\"DOCUMENTATION\">~a</span>"
                (insert-spaces 5)
                "Documentation"
                doc)))))

#+:irs-lispworks
(defun irsbrowser-describe-own-slot (slot-name-and-value ontology stream
                                              &optional
                                              (lookup-function
                                               #'mnm-lookup-current-word))
  (declare (ignore lookup-function))
 (let ((slot (car slot-name-and-value))
        (slot-value (second slot-name-and-value)))
    (format stream
            (format nil "<br>~a<b>~:~a</b>" 
                     (insert-spaces 4)   ;; double check
                     (my-link slot ontology)
                     (add-html-relation-colour slot)))
    (when slot-value
      (format stream "<br>~a<b><i>~a:</i></b>~a"
              (insert-spaces 5)
              "Value"
              (irsbrowser-format-ocml-value slot-value 7 ontology)))))
;;; }}}
;;; }}}
;;; {{{ Tasks
;; not much linking here because 1) I need examples 2) not sure whether it would make sense
;; so I almost just replicated the code, so that it'll be handy whenever we'll tweak it more

(defun irsbrowser-describe-task-internals (task-name class ontology stream
                                          &optional
                                          (lookup-function
                                           #'mnm-lookup-current-word))
  ;;;(setf ll (list task-name class ontology stream lookup-function))
  (let ((associated-psms 
         (ocml::find-all-psms-tackling-task-type task-name ontology))
        (input-roles ;;(setofall '?x 
                       ;;        `(has-input-role ,task-name ?x)))
                       (web-onto::findany '?x
                                `(all-task-input-roles ,task-name ?x)))
        (output-role 
         ;;(web-onto::findany '?x 
         ;;                   `(has-output-role ,task-name ?x)))
         (car (web-onto::findany '?x
                                 `(all-task-output-roles ,task-name ?x))))
        (other-slots (intersection (domain-slots class)
                                   *standard-other-task-slots*)))
    (when associated-psms
      (let ((associated-psms-info 
             (mapcar #'(lambda (x)
                         (list (name x) (name (home-ontology x))))
                     associated-psms)))
        (format stream "~A" (http::bold "Associated PSMs: "))
        (format stream "~(~{~a (~a)~}~{~{; ~a (~a)~}~}~)"
                (car associated-psms-info)
                (cdr associated-psms-info))
               

        (format stream "<br>")))
    (when input-roles
      (format stream "~A" (http::bold "Input Roles:"))
      (loop for slot in input-roles
            do
            (irsbrowser-describe-slot slot class ontology stream
                           lookup-function))

      (format stream "<br>"))
    (when output-role
      (format stream "~A" (http::bold "Output Role:"))
      (irsbrowser-describe-slot output-role class ontology stream
                     lookup-function)

      (format stream "<br>"))
    (when other-slots
      (format stream "~A" (http::bold "Other Slots:"))
      (loop for slot in other-slots
            do
            (irsbrowser-describe-slot slot class ontology stream
                           lookup-function)))))
;;; }}}
;;; {{{ Problem solving methods
;; same recommendations as for tasks (changed little, need more info)

(defun irsbrowser-describe-psm-internals (psm-name class ontology stream 
                                        &optional
                                        (lookup-function
                                         #'mnm-lookup-current-word))
  (let* ((described-input-roles nil)
         (associated-tasks
          (setofall '?x `(tackles-task-type ,psm-name ?x)))
         (psm-input-roles 
          (all-class-slot-values 
           psm-name 'has-input-role))
         (associated-tasks-input-roles
          (mapcar
           #'(lambda (task) 
               (cons task 
                     (setofall '?x 
                               `(has-input-role ,task ?x))))
           associated-tasks))


         (output-role
          (car (all-class-slot-values 
                psm-name 'has-output-role)))
         (output-role-class class)
         
         (other-slots 
          (intersection (domain-slots class)
                        *standard-other-psm-slots*))
         (own-slots 
          (intersection (own-slots class) 
                        *standard-psm-own-slots*
                        :test
                        #'(lambda (x y)
                            (eq (car x) y)))))
    (when (null output-role)
      (do* ((tasks associated-tasks (cdr tasks))
            (task-output-role (when tasks
                                (car (all-class-slot-values 
                                    (car tasks) 'has-output-role)))
                              (when tasks
                                (car (all-class-slot-values 
                                    (car tasks) 'has-output-role)))))
           ((or (null tasks) task-output-role)
            (unless (null tasks)
              (setf output-role task-output-role
                    output-role-class (get-domain-class (car tasks)))))))
    (when (or psm-input-roles (and associated-tasks-input-roles 
                                   (second (car associated-tasks-input-roles))))
      (format stream "~A" (http::bold "Input Roles:"))
      (loop for slot in psm-input-roles
            do
            (progn (push slot described-input-roles)
              (irsbrowser-describe-slot slot class ontology stream   ;; changed here
                             lookup-function)))
      (mapc #'(lambda (associated-task-input-roles)
                (let ((task-structure (get-domain-class (car associated-task-input-roles))))
                  (loop for slot in (cdr associated-task-input-roles)
                        do
                        (unless (find slot described-input-roles)
                          (push slot described-input-roles)
                          (irsbrowser-describe-slot slot task-structure ontology stream   ;; changed here
                                         lookup-function)))))
            associated-tasks-input-roles)
      (format stream "<br>"))
    (when output-role
      (format stream "~A" (http::bold "Output Role:"))
      (irsbrowser-describe-slot output-role output-role-class ontology stream  ;; changed here
                     lookup-function)
      (format stream "<br>"))
    (when (or other-slots own-slots)
      (format stream "~A" (http::bold "Other Slots:"))
      (loop for slot in other-slots
            do
            (irsbrowser-describe-slot slot class ontology stream   ;; changed here
                           lookup-function))
      (loop for slot-name-and-value in own-slots
            do
            (irsbrowser-describe-own-slot slot-name-and-value ontology stream   ;; changed here
                               lookup-function)))))
;;; }}}
;;; {{{ Goals
;; original was in wp-html-describe.lisp
;; some work to be done here: there's reference to external describe-function we havent imported yet

(defun irsbrowser-describe-goal-internals (goal-name class ontology stream
                                                &optional
                                                (lookup-function
                                                 #'ocml::mnm-lookup-current-word))
  (let ((associated-web-services 
         (wsmo-protocol::check-class-and-find-all-web-services-which-solve-goal goal-name))
        (input-roles (wsmo-protocol::input-roles goal-name))   ;;external function
        (output-role (wsmo-protocol::output-role goal-name))   ;;external function
        (other-slots (intersection (domain-slots class)
                                   wsmo-protocol::*STANDARD-OTHER-GOAL-SLOTS*))
        (non-functional-properties
         (web-onto::findany 
          '?x 
          `(= ?x (the-class-slot-value ,goal-name 
                                             has-non-functional-properties)))))
    (when associated-web-services
      (let ((associated-web-services-info 
             (mapcar #'(lambda (x)
                           (list (name x) (name (home-ontology x))))
                     associated-web-services)))
        (format stream "~A" (http::bold "Associated Web Services: "))
        (format stream "~(~{~a (~a)~}~{~{; ~a (~a)~}~}~)"
                (car associated-web-services-info)   ;;not changed much for now
                (cdr associated-web-services-info))


        (format stream "<br>")))
    (when input-roles
      (format stream "~A" (http::bold "Input Roles:"))
      (loop for slot in input-roles
            do
            (irsbrowser-describe-wsmo-slot slot class ontology stream    ;; changes
                                 lookup-function))
      (format stream "<br>"))
    (when output-role
      (format stream "~A" (http::bold "Output Role:"))
      (irsbrowser-describe-wsmo-slot output-role class ontology stream      ;; changed
                           lookup-function)
      (format stream "<br>"))
    (when other-slots
      (format stream "~A" (http::bold "Other Slots:"))
      (loop for slot in other-slots
            do
            (irsbrowser-describe-wsmo-slot slot class ontology stream        ;; changed
                                 lookup-function))
      (format stream "<br>"))
    (when (wsmo-protocol::html-describe-class-slots-p non-functional-properties)      ;; function we dont have
      (format stream "~A" (http::bold "Non Functional Properties:"))
      (irsbrowser-describe-wsmo-class-slots non-functional-properties            ;; changed
                                 ontology stream lookup-function
                                 #'(lambda (class slot) (ip::get-class-slot-value class slot))))))


;; imported from wsmo-protocol


(defun irsbrowser-describe-wsmo-slot (slot class ontology stream
                                      &optional (lookup-function #'mnm-lookup-current-word))
  (declare (ignore lookup-function))
  (format stream "<br>~a<b>~a</b>"
          (insert-spaces 4)
          (my-link slot ontology))
  (multiple-value-bind (values defaults)
      (get-slot-values-from-class-structure
       class slot )
    (when values
      (format stream "<br />~a<span class=\"SLOT-OPTION\">~a:</span> ~{~a~}"
              (insert-spaces 5)
              "Value" 
              (mapcar #'(lambda (x) 
                          (irsbrowser-format-ocml-value x 1 ontology nil))
                      values)))
    (when defaults
      (format stream  "<br />~a<span class=\"SLOT-OPTION\">~a:</span> ~{~a~}"
              (insert-spaces 5)
              "Default-Value"
              (mapcar #'(lambda (x) 
                          (irsbrowser-format-ocml-value x 1 ontology nil))
                      defaults)))
    (let ((type-info (remove-duplicates
                      (ocml::find-option-value class slot :type))))
      (when type-info
        (format stream "<br />~a<span class=\"SLOT-OPTION\"><i>~a:</i></span> ~{~a~}"
                (insert-spaces 5)
                "Type"
                (mapcar #'(lambda (x) 
                            (irsbrowser-format-ocml-value x 1 ontology nil))
                        type-info))))
    (loop for option in '(:min-cardinality
                          :max-cardinality
                          :inheritance)
       for value = (ocml::find-option-value class slot option)
       when value
       do
       (format stream  "<br />~a<span class=\"SLOT-OPTION\"><i>~:(~a~):</i></span> ~a" ;;maybe more specific css names
               (insert-spaces 5)
               option
               value))
    (let ((doc (ocml::find-slot-documentation class slot)))
      (when doc
        (format stream  "<br />~a<b><i><span class=\"SLOT-OPTION\">~a:</span></i></b> <span class=\"DOCUMENTATION\">~a</span>"
                (insert-spaces 5)
                "Documentation"
                doc)))))



(defun irsbrowser-describe-wsmo-class-slots (class-name 
                                  ontology stream lookup-function
                                  &optional slot-test)
  (let ((class (get-domain-class class-name)))
    (when class
      (dolist (slot (domain-slots class))
        (when (or (null slot-test)
                  (funcall slot-test class-name slot))
          (irsbrowser-describe-wsmo-slot slot class ontology stream
                               lookup-function))))))

;;; }}}
;;; {{{ Web services
;; original was in wp-html-describe.lisp
;; just changed the function names (irsbrowser-describe...) and the external ones...

(defun irsbrowser-describe-web-service-internals (web-service-name class ontology stream
                                                       &optional
                                                       (lookup-function
                                                        #'mnm-lookup-current-word))
  (let* ((input-roles (wsmo-protocol::input-roles web-service-name))
        (output-role (wsmo-protocol::output-role web-service-name))
        (host 
         (web-onto::findany
          '?x `(wsmo-web-service-host ,web-service-name ?x)))
        (port
         (web-onto::findany
          '?x `(wsmo-web-service-port ,web-service-name ?x)))
        (location 
         (web-onto::findany
          '?x `(wsmo-web-service-location ,web-service-name ?x)))
        (non-functional-properties
         (web-onto::findany 
          '?x 
          `(= ?x (the-class-slot-value ,web-service-name 
                                             has-non-functional-properties))))
        (capability
         (web-onto::findany 
          '?x 
          `(= ?x (the-class-slot-value ,web-service-name 
                                             has-capability))))
        (mediators
         (web-onto::findany 
          '?x 
          `(= ?x (all-class-slot-values ,web-service-name 
                                             used-mediator))))
        (interface
         (web-onto::findany 
          '?x 
          `(= ?x (the-class-slot-value ,web-service-name 
                                             has-interface))))
        (orchestration
         (when (and interface (get-domain-class interface))
           (web-onto::findany 
            '?x 
            `(= ?x (the-class-slot-value ,interface 
                                               has-orchestration)))))
        (choreography
         (when (and interface (get-domain-class interface))
           (web-onto::findany 
            '?x 
            `(= ?x (the-class-slot-value ,interface 
                                               has-choreography)))))
        (interface-mediators
         (when (and interface (get-domain-class interface))
           (web-onto::findany 
            '?x 
            `(= ?x (the-class-slot-value ,interface 
                                               used-mediator))))))
    (when host
      (format stream "~A" (http::bold "Publisher URL:"))    ;; left untouched
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
            (irsbrowser-describe-wsmo-slot slot class ontology stream    ;;changed here
                                 lookup-function))
      (format stream "<br>"))
    (when output-role
      (format stream "~A" (http::bold "Output Role:"))
      (irsbrowser-describe-wsmo-slot output-role class ontology stream   ;;changed here
                           lookup-function)
      (format stream "<br>"))    
    (when mediators
      (format stream "~A" (http::bold "Mediators:"))
      (dolist (mediator mediators)
        (irsbrowser-describe-wsmo-slot mediator class ontology stream lookup-function))    ;;changed here
      (format stream "<br>"))
    (when (wsmo-protocol::html-describe-class-slots-p capability)
      (format stream "~A" (http::bold "Capability:"))
      (irsbrowser-describe-wsmo-class-slots capability
                                          ontology stream lookup-function)
      (format stream "<br>"))
    (when (or (wsmo-protocol::html-describe-class-slots-p orchestration)
              (wsmo-protocol::html-describe-class-slots-p choreography)
              (wsmo-protocol::html-describe-class-slots-p interface-mediators))
      (format stream "~A" (http::bold "Interface Components:<br>"))
      (when (wsmo-protocol::html-describe-class-slots-p orchestration)
        (format stream "~A" (http::bold (http::italic "Orchestration:")))
        (irsbrowser-describe-wsmo-class-slots orchestration 
                                            ontology stream lookup-function)
        (format stream "<br>"))
      (when (wsmo-protocol::html-describe-class-slots-p choreography)
        (format stream "~A" (http::bold (http::italic "Choreography:")))
        (irsbrowser-describe-wsmo-class-slots choreography
                                            ontology stream lookup-function)
        (format stream "<br>"))
      (when (wsmo-protocol::html-describe-class-slots-p interface-mediators)
        (format stream "~A" (http::bold (http::italic "Interface Mediators:")))
        (irsbrowser-describe-wsmo-class-slots interface-mediators
                                            ontology stream lookup-function)
        (format stream "<br>")))
    (when (wsmo-protocol::html-describe-class-slots-p non-functional-properties)
      (format stream "~A" (http::bold (http::italic "Non Functional Properties:")))
      (irsbrowser-describe-wsmo-class-slots non-functional-properties 
                                          ontology stream lookup-function
                                          #'(lambda (class slot) (ip::get-class-slot-value class slot))))))

;;; }}}
;;; {{{ Mediators
;; original was in wp-html-describe.lisp
(defun irsbrowser-describe-mediator-internals (mediator-name class ontology stream
                                                        &optional
                                                        (lookup-function
                                                         #'ocml::mnm-lookup-current-word))
  (let* ((non-functional-properties
          (web-onto::findany 
           '?x 
           `(= ?x (the-class-slot-value ,mediator-name 
                                              has-non-functional-properties))))
        
         (main-slots (remove 'has-non-functional-properties
                              (domain-slots class))))
    (when main-slots
      (format stream "~A" (http::bold "Main Slots:"))
      (loop for slot in main-slots
            do
            (irsbrowser-describe-wsmo-slot slot class ontology stream
                                 lookup-function))
      (format stream "<br>"))
    (when (wsmo-protocol::html-describe-class-slots-p non-functional-properties)
      (format stream "~A" (http::bold "Non Functional Properties:"))
      (irsbrowser-describe-wsmo-class-slots non-functional-properties 
                                          ontology stream lookup-function
                                          #'(lambda (class slot) (ip::get-class-slot-value class slot))))))

;;; }}}
