;;;from harlequin
;;;(REFINEMENT-THROUGH-SUBCLASS-OF-LINKS)
(in-package ocml)

;;(defmethod handle-irs-request ((request-name (eql 'displayfromExampleTree)) 

#|

(defmethod http::http-reply ((method (eql :ocml)) request)
  ;(setf *current-ontologies* (mapcar #'cdr *all-ontologies*))
  (setf r request)
  (let* ((*package* (find-package "OCML"))
         (upcase-string (string-upcase request))
         (action (read-from-string upcase-string)))
    ;;(handle-irs-request action upcase-string
    (case action
      ((displayfromExampleTree) (irs-dictionary-query-from-example-tree upcase-string))
      ((showExample) (irs-show-example upcase-string))
      ((displayfromtree) (irs-dictionary-query-from-tree upcase-string))
      ((displayLocalTaskTopClasses) (irs-local-task-top-classes))
      ((displayLocalMethodTopClasses) (irs-local-method-top-classes upcase-string))
      ((displayLocalDirectTaskSubclasses) (irs-local-direct-task-subclasses upcase-string))
      ((displayLocalDirectMethodSubclasses) (irs-local-direct-method-subclasses upcase-string))
      ((displayfromarea) (irs-dictionary-query-from-area upcase-string))
      ((displayLocalDomainOntologies) (irs-display-domain-ontologies upcase-string))
      ((displayLocalApplicationOntologies) (irs-display-application-ontologies upcase-string))
      ;((unfoldontology) (irs-unfold-task-ontology upcase-string))
      ((getInputRoles) (get-input-roles upcase-string))
      ((getSlotsWithType) (get-slots-with-type upcase-string))
      ;((taskMethodType) (get-direct-subtypes upcase-string))
      ;((configInstances) (irs-get-all-instances upcase-string))
      ((getInstances) (irs-get-all-instances upcase-string))
      ((readInUserDomainKb) (get-application-inputs upcase-string))
      ((createApplication) (create-application upcase-string))
      ((solvingApplicationInstance) (solving-application-instance upcase-string))
      ((createTaskConfigOntology) (create-task-configuration-ontology upcase-string))
      ((createMethodConfigOntology) (create-method-configuration-ontology upcase-string))
      ((createClassInstance) (create-class-instance upcase-string))
      ((createNamedClassInstance) (create-named-class-instance upcase-string))
      ((mappingToClassHierarchy) (mapping-to-class-hierarchy upcase-string))
      ((IrsCorbaTest) (irs-corba-claimer upcase-string))
      ((IrsCorbaDemoTotalCost) (irs-corba-total-cost upcase-string))
      ((currencyConvertor) (irs-corba-currency-convertor upcase-string))
      ((achieve-task) (irs-achieve-task upcase-string))
      (t (unknown-action action)))))
|#

(defun irs-get-all-instances (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let ((*package* (find-package "OCML"))
          (type-in (read istream))
          instances)
      ;;inform the type-in infmation to irsapplet
      (cond ((holds? 'subclass-of type-in 'set)
             (format  stream "~a~%" 'set)
             (setf instances 
                   (remove-duplicates
                    (apply #'append (setofall '?x `(,type-in ?x))))))
            (t (format  stream "~a~%" 'nonset)
               (setf instances
                     (setofall '?x `(,type-in ?x)))))
      (mapcar #'(lambda (x) (format stream "~a~%" x)) instances))))

;;IRS-SHOW-EXAMPLE
(defun irs-show-example (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let ((*package* (find-package "OCML"))
          (type-in (read istream)))
      (if (holds? 'subclass-of type-in 'set)
          (find-examples-in-all-ontologies (car (all-class-slot-values type-in 'element-type)))
        (find-examples-in-all-ontologies type-in)))))





(defun find-examples-in-all-ontologies (unary-relation)
  (setf *ontologies-worth-searching* (remove-if
                         #'(lambda (x) (find  'y *all-ontologies*
                                              :TEST #'(LAMBDA (y o)
                                                        (member (cdr x)
                                                                (ontology-includes
                                                                 (cdr o))))))
                         *all-ontologies*))

  (let ((current-ontology *current-ontology*))
    (unwind-protect
      (let* ((pairs (loop for pair in *ontologies-worth-searching*
                          with result = nil 
                          with values
                          do
                          (select-ontology (car pair))
                          (setf values (setofall '?x (list 'asserted (list unary-relation '?x))))
                          (when values
                            (push (cons values (car pair)) result))
                          finally
                          (return result)))
             (normalised-examples (normalize-examples unary-relation pairs)))
        (print normalised-examples)
        (mapcar #'(lambda (x) (progn 
                                (format http::*http-stream* "~a~%" (first x))
                                (format http::*http-stream* "~a~%" (second x)))) normalised-examples))  
      (switch-to-ontology current-ontology))))

(defun normalize-examples  (unary-rel pairs)
  (loop with class = (get-ocml-class unary-rel)
        with result = nil
        for pair in pairs
        do
        (destructuring-bind (examples . ontology) pair
          (select-ontology ontology)
          (loop for example in examples
                do
                (if class 
                    (pushnew  (list ontology (name (find-current-instance-of-class class example)))
                                    result)
                (pushnew (list ontology (name (find-relation-instance (get-relation unary-rel) (List example))))
                           result))))
        finally (return result)))





(defun old-find-examples-in-all-ontologies (unary-relation)
  (let ((current-ontology *current-ontology*))
    (unwind-protect
     
      (loop for pair in *ontologies-worth-searching*
            with result = nil 
            with values
            do
            (select-ontology (car pair))
            (setf values (setofall '?x (list unary-relation '?x)))
            (when values
              ;(format http::*http-stream* "~a~%" (car pair))
              (mapcar #'(lambda (x) (progn 
                                      (format http::*http-stream* "~a~%" (car pair))
                                      (format http::*http-stream* "~a~%" x))) values)))
              ;(format http::*http-stream* "~a~%" values)
              ;(format http::*http-stream* "~a~%" (cons values (car pair))))
              ;(push (cons values (car pair)) result))
            ;finally
            ;(return result))
      (switch-to-ontology current-ontology))))



;;CREATE-TASK-CONFIGURATION-ONTOLOGY
(defun create-task-configuration-ontology (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((domain-ontology (read istream))
           (task (read istream))
           (task-ontology (find-ontology task (all-tasks-with-top-ontologies)))
           (created-ontology (def-ontology-internal (gentemp "TASK-CONFIG-ONTOLOGY")
                                                          ""
                                                          (list :includes (list domain-ontology task-ontology)
                                                                :type :application))))
      (format http::*http-stream* "~a~%" (name created-ontology))))) 

;;CREATE-METHOD-CONFIGURATION-ONTOLOGY
(defun create-method-configuration-ontology (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((task-config-ontology (read istream))
           ;(method (read istream))
           ;(method-ontology (find-ontology method (all-tasks-with-top-ontologies)))
           (created-ontology (def-ontology-internal (gentemp "METHOD-CONFIG-ONTOLOGY")
                                                          ""
                                                          (list :includes (list task-config-ontology (name *current-ontology*))
                                                                :type :application))))
      (format t  "~a~%" task-config-ontology)
      (format http::*http-stream* "~a~%" (name created-ontology))))) 

  
;;MAPPING-TO-CLASS-HIERARCHY
(defun mapping-to-class-hierarchy (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    ;;need ontology infomation
    (let ((ontology (read istream))
          (top-classes (read istream))
          (name (read istream))
          target-class)
      (select-ontology ontology)
      (if (holds? 'subclass-of name 'set)
          (setf target-class (car (all-class-slot-values name 'element-type)))
        (setf target-class name))
      (dolist (class top-classes)
        (let ((mapping `(def-relation-mapping ,target-class :up
                                           ((,target-class ?x)
                                            if
                                            (or (= ?x ',class)
                                                (subclass-of ?x ',class))))))
        (format http::*http-stream* "~a~%" (eval mapping)))))))

        ;(format http::*http-stream* "~a~%" (name application-ontology)))))

;;FUNCTION GET-SLOTS-WITH-TYPE
(defun get-slots-with-type (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (Let* ((name (read istream))
           )
      (if (holds? 'subclass-of name 'seT)
          (progn
          (format  stream "~a~%" (car (all-class-slot-values name 'element-type)));;pass element-type
          (get-slots-from-set name))
        (progn
           (format  stream "~a~%" name) ;;name is non-set 
           (internal-get-slots-with-type name 'non-set))))))

(defun internal-get-slots-with-type (name set-type)
  (let* ((class (get-ocml-class name))
         (slots (domain-slots class))
         (pairs nil))
    (dolist (slot slots)
      (let ((type-info (remove-duplicates
                        (find-option-value class slot :type)))
            (default-value (find-option-value class slot :default-value)))
        (setf pairs (cons (list slot 
                                (or (car type-info) *default-irs-type*) 
                                (or (car default-value) "case input"))
                          pairs))))
    (mapcar #'(lambda (x) (format  http::*http-stream*

 "~a~%" (first x))
                (format  http::*http-stream* "~a~%" (second x))
                (format  http::*http-stream* "~a~%" (third x)))
                ;;(format  http::*http-stream* "~a~%" set-type))
            (reverse pairs))))

(defun get-slots-from-set (name)
  (internal-get-slots-with-type (car (all-class-slot-values name 'element-type))
                                'set))

(defun local-supers (ontology class)
  (let ((supers (direct-domain-superclasses (get-domain-class class))))
    (mapcan #'(lambda (x)
                (when (eq (name (home-ontology x)) ontology)
                  (list (name x))))
            supers)))

(defun class-subclasses (class)  
  (mapcar #'name (current-direct-subclasses (get-domain-class class))))


;;;FUNCTION IRS-LOCAL-TOP-CLASSES
(defun irs-local-task-top-classes (stream)
  (let* ((top-classes nil)
         (tasks (all-tasks)))
    (dolist (task tasks)
      (select-ontology (car task))
      (unless (local-supers (car task) (second task))
        (push (list (second task) (if (class-subclasses (second task))
                                      'false 'true))
              top-classes)))
    (setf top-classes (sort (remove-duplicates top-classes :test #'equal) #'string<
                            :key #'car))
    ;;(setf xx (format nil "~{~{~:(~a~)~% ~(~a~)~% ~}~}" top-classes))
    ;;(format stream "~{~{~:(~a~)~% ~(~a~)~% ~}~}~%" top-classes)
    (format stream "~{~:(~a~)~%~}~%" (mapcar #'car top-classes))
    ))
    

;;;FUNCTION IRS-LOCAL-DIRECT-TASK-SUBCLASSES
(defun irs-local-direct-task-subclasses (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (name (read istream))
           (tasks (all-tasks))
           (ontology (find-ontology name tasks))
           (task-names (remove nil (mapcar #'(lambda (x)(if (eql (car x) ontology) (second x))) (all-tasks)))))
      (select-ontology ontology) 
      (mapcar #'(lambda (x) (format stream "~:(~a~)~%" x))
              (eval `(ocml-eval (setofall ?x (and (member ?x ',task-names)(direct-subclass-of ?x ',name)))))))))

(defun irs-local-direct-method-subclasses (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (task (read istream))
           (name (read istream))
           (methods (all-methods task))
           (ontology (find-ontology name methods))
           (method-names (remove nil (mapcar #'(lambda (x)(if (eql (car x) ontology) (second x))) methods))))
      (print ontology)
      (select-ontology ontology) 
      (mapcar #'(lambda (x) (format http::*http-stream* "~:(~a~)~%" x))
              (eval `(ocml-eval (setofall ?x (and (member ?x ',method-names)(direct-subclass-of ?x ',name)))))))))
                                        




;;;FIND-ONTOLOGY 
;;;here name is a class has type :task or :method
;;;and list is a list of pairs of (ontology class)
;;;which is produced by function ALL-TASKS or 
;;;function ALL-METHODS                               
(defun find-ontology (name list)
  (if (null list) nil
    (if (eql name (second (car list))) 
        (first (car list))
      (find-ontology name (cdr list)))))

(defun irs-local-method-top-classes (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (task (read istream))
           (methods (all-methods task))
           (top-classes nil))
      (dolist (method methods)
        (select-ontology (car method)) 
        (unless (local-supers (car method) (second method))
          (push (second method) top-classes)))
      (setf top-classes (sort (remove-duplicates top-classes) #'string<))
      (mapcar #'(lambda (x) (format stream "~:(~a~)~%" x)) top-classes))))

;;;FUNCTION CREATE-APPLICATION-INSTANCE
(defun create-application-instance (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (task-type (read istream))
           (method-type (read istream))
           (domain-ontology (read istream))
           (task-configuration-input (read istream))
           (method-configuration-input (read istream))
           (task (define-domain-instance 
                  (gentemp (format nil "~a" task-type))
                 task-type
                ""
                task-configuration-input))
           (method (define-domain-instance 
                   (gentemp "METHOD")
                   method-type
                   ""
                   method-configuration-input))
           (application (define-domain-instance 
                        (gentemp (string-append (format nil "~a" domain-ontology)
                                                "-"
                                                 (format nil "~a" task-type)))
                        'application
                        ""
                        `((tackles-domain ,domain-ontology)
                          (uses-method ,(name method))
                          (tackles-task ,(name task))))))
    (tell1 `(tackles-task ,(name method) ,(name task)))
    ;(format http::*http-stream* "~a~%" application)
    (irs-describe-instance (name application))
    application)))



;;;SOLVING-APPLICATION-INSTANCE
(defun solving-application-instance-old (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (task-type (read istream))
           (method-type (read istream))
           (domain-ontology (read istream))
           (task-configuration-input (read istream))
           (method-configuration-input (read istream))
           (task-ontology (find-ontology task-type (all-tasks-with-top-ontologies)))
           (method-ontology (find-ontology method-type (all-methods task-type)))
           (application-ontology (create-application-ontology domain-ontology 
                                                              task-ontology
                                                              method-ontology)))
      (select-ontology (name application-ontology))
      (let*
          ((task (define-domain-instance 
                        (gentemp (format nil "~a" task-type))
                        task-type
                        ""
                        task-configuration-input))
           (method (define-domain-instance 
                          (gentemp "METHOD")
                          method-type
                          ""
                          method-configuration-input))
           (application (define-domain-instance 
                               (gentemp (string-append (format nil "~a" domain-ontology)
                                                             "-"
                                                             (format nil "~a" task-type)))
                               'application
                               ""
                               `((tackles-domain ,domain-ontology)
                                 (uses-method ,(name method))
                                (tackles-task ,(name task)))))
          ; (method-ontology (find-ontology method-type (all-methods task-type)))
          ; (task-ontology (find-ontology task-type (all-tasks-with-top-ontologies)))
          ; (application-ontology (create-application-ontology domain-ontology 
          ;                                                    task-ontology
          ;                                                    method-ontology)
        result)
      (print application)
      (print task-ontology)
      (print method-ontology)
      (print domain-ontology)
      (print application-ontology)
      ;(select-ontology (name application-ontology))
      ;(tell1 `(tackles-task ,(name method) ,(name task)))
      (setf result (ocml-eval-gen `(solve-application ,(name application))))
      (format http::*http-stream* "~a~%" result)))))
    ;(irs-describe-instance (name application))))))
    ;application)))

;;;CREATE-CLASS-INSTANCE
(defun create-class-instance (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (ontology (read istream))
           (class (read istream))
           (slot-value-pairs (read istream)))
      (select-ontology ontology)
      (let ((instance (define-domain-instance 
                       (gentemp (format nil "~a" class))
                       class
                       ""
                       slot-value-pairs)))
        (format stream "~:(~a~)~%" (name instance))))))

;;;CREATE-NAMED-CLASS-INSTANCE
(defun create-named-class-instance (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (ontology (read istream))
           (class (read istream))
           (instance-name (read istream))
           (slot-value-pairs (read istream)))
      (select-ontology ontology)
      (let ((instance (define-domain-instance 
                       instance-name   ;;;;;(gentemp (format nil "~a" class))
                       class
                       ""
                       slot-value-pairs)))
        (format http::*http-stream* "~a~%" (name instance))))))




;;;CREATE-APPLICATION
(defun create-application-good (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (task-type (read istream))
           (method-type (read istream))
           (domain-ontology (read istream))
           (key-parameters (read istream))
           (task-configuration-input (read istream))
           (method-configuration-input (read istream))
           (task-ontology (find-ontology task-type (all-tasks-with-top-ontologies)))
           (method-ontology (find-ontology method-type (all-methods task-type)))
           (application-ontology (create-application-ontology domain-ontology 
                                                              task-ontology
                                                              method-ontology))
           domain-application)
      (select-ontology (name application-ontology))
      (let ((function 
             `(defun ,(gentemp)  ,key-parameters
                (let*
                    ((domain-ontology ',domain-ontology)
                     ;(task-configuration-input ,task-configuration-input)
                     ;(method-configuration-input ,method-configuration-input)
                    (task (define-domain-instance 
                            ',(gentemp (format nil "~a" task-type))
                            ',task-type
                            ""
                            ,task-configuration-input))
                    (method (define-domain-instance 
                             ',(gentemp "METHOD")
                             ',method-type
                             ""
                            ,method-configuration-input))
                    (application (define-domain-instance 
                                 ',(gentemp (string-append (format nil "~a" domain-ontology)
                                                               "-"
                                                               (format nil "~a" task-type)))
                                 'application
                                 ""
                                 `((tackles-domain ,domain-ontology)
                                   (uses-method ,(name method))
                                   (tackles-task ,(name task))))))
                 (ocml-eval-gen `(solve-application ,(name application)))))))
        (format http::*http-stream* "~a~%" (eval function))
        (format http::*http-stream* "~a~%" (name application-ontology))))))


(defun create-application (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (task-config-ontology (read istream))
           (method-config-ontology (read istream))
           (domain-ontology (read istream))
           (task-type (read istream))
           (method-type (read istream))
           (key-parameters (read istream))
           (task-configuration-input (read istream))
           (method-configuration-input (read istream))
           ;(task-ontology (find-ontology task-type (all-tasks-with-top-ontologies)))
           ;(method-ontology (find-ontology method-type (all-methods task-type)))
           (application-ontology (create-application-ontology domain-ontology 
                                                              task-config-ontology
                                                              method-config-ontology))
           domain-application)
      (select-ontology (name application-ontology))
      (let ((function 
             `(defun ,(gentemp)  ,key-parameters
                (let*
                    ((domain-ontology ',domain-ontology)
                     ;(task-configuration-input ,task-configuration-input)
                     ;(method-configuration-input ,method-configuration-input)
                    (task (define-domain-instance 
                            ',(gentemp (format nil "~a" task-type))
                            ',task-type
                            ""
                            ,task-configuration-input))
                    (method (define-domain-instance 
                             ',(gentemp "METHOD")
                             ',method-type
                             ""
                            ,method-configuration-input))
                    (application (define-domain-instance 
                                 ',(gentemp (string-append (format nil "~a" domain-ontology)
                                                               "-"
                                                               (format nil "~a" task-type)))
                                 'application
                                 ""
                                 `((tackles-domain ,domain-ontology)
                                   (uses-method ,(name method))
                                   (tackles-task ,(name task))))))
                 (ocml-eval-gen `(solve-application ,(name application)))))))
        (format stream "~a~%" (eval function))
        (format stream "~a~%" (name application-ontology))))))


(defun solving-application-instance (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (application-ontology (read istream))
           (application-function (read istream))
           (key-parameters (read istream)))
      (select-ontology application-ontology)
      (format stream "~a~%" (apply application-function key-parameters)))))
                           


(defun create-application-ontology (domain-ontology task-ontology method-ontology)
  (def-ontology-internal (gentemp "APPLICATION-ONTOLOGY")
                               ""
                               (list :includes (list domain-ontology task-ontology method-ontology)
                                     :type :application)))




;;;FUNCTION IRS-DICTIONARY-QUERY-FROM-TREE
(defun irs-dictionary-query-from-tree (stream upcase-string)
  (format stream "<html>~%<body>~%")
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (type (read istream))
           (name (read istream))
           documentation)
      (if (eql type ':TASKS) (select-ontology (find-ontology name (all-tasks))))
      (cond
       ((get-ocml-class name) 
        (setf documentation (ocml-documentation (get-ocml-class name)))
        (when documentation
          (format stream "~a~%" documentation))
        ;;(format stream "~a~%" "end-of-documentation")
        (irs-describe-class stream name))
       ((get-ontology name)
        (if (member (get-ontology name) (sub-ontologies *current-ontology*)) (select-ontology name))
        (setf documentation (ocml-documentation (get-ontology name)))
        (when documentation
          (format stream "~a~%" documentation))
        (describe-ontology name stream))
       (t (format stream "No definition of ~:(A~) is found in current ontology ~:(A~)~%" name 
                  (name *current-ontology*))))))
    (format stream "</body></html>~%"))


(defun irs-dictionary-query-from-example-tree (stream upcase-string)
  (let ((current-ontology *current-ontology*))
    (with-input-from-string (istream upcase-string)
      (read istream)
      (let* ((*package* (find-package "OCML"))
             (ontology (read istream))
             (name (read istream)))
        (select-ontology ontology)
        (irs-describe-instance stream name)))
    (switch-to-ontology current-ontology)))


(defun irs-dictionary-query-from-tree-old (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (type (read istream))
           (name (read istream)))
           ;(classes (all-tasks)))
           ;(ontology (find-ontology name classes)))
      ;(print classes)
      ;(print ontology)
      ;(print type)
      ;(print name)
      ;(print *current-ontology*)
      (if (eql type ':TASK) (select-ontology (find-ontology name (all-tasks))))
      ;;to find the result of a given query we have to find the ontology.  
      ;(select-ontology 'classificationv2)
      ;(select-ontology 'kmi-as-gen-design)
      (cond
       ((get-ocml-class name) (irs-describe-class name))
       ;((find-instance name) (irs-describe-instance name))
       ;((get-relation name) (irs-describe-relation name))
       ;((get-function name) (irs-describe-function name))
       (t (format http::*http-stream* "no definition of ~A is found in current ontology ~A~%" name (name *current-ontology*)))))))




;;;FUNCTION IRS-DICTIONARY-QUERY
(defun irs-dictionary-query-from-area (stream upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let ((*package* (find-package "OCML"))
          (name (read istream)))
      (cond
       ((get-ocml-class name) (irs-describe-class stream name))
       ((find-instance name) (irs-describe-instance stream name))
       ((get-relation name) (irs-describe-relation stream name))
       ((get-function name) (irs-describe-function stream name))
       ;((get-ontology name)(format http::*http-stream* "~a~%" (ocml-documentation (get-ontology name))))
       ((get-ontology name)
        (select-ontology name)
        (describe-ontology name stream))
       (t (format stream "no definition of ~A is found in current ontology ~A~%" 
                  name (name *current-ontology*)))))))

(defun all-domain-ontology-names ()
  (mapcan #'(lambda (x)
              (when (eql (slot-value (cdr x) 'ontology-type) :DOMAIN)
                (list (car x))))
          *all-ontologies*))

;;;FUNCTION IRS-DISPLAY-DOMAIN-ONTOLOGIES
(defun irs-display-domain-ontologies (stream)
  (let* ((*package* (find-package "OCML"))
         (domain-ontologies (sort (all-domain-ontology-names) #'string<)))
    (format stream "~{~:(~a~)~%~}" domain-ontologies)))

(defun irs-display-application-ontologies (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML")))
           ;(type (read istream)))
      (dolist (ont *all-ontologies*)
        (if (eql (slot-value (cdr ont) 'ontology-type) :APPLICATION)
        (format http::*http-stream* "~a~%" (car ont)))))))


(defun all-tasks ()
  (let ((current-ontology (name *current-ontology*))
        (tasks nil)
        (task-ontologies (task-ontologies)))
    (dolist (onto task-ontologies)
      (select-ontology (name onto))
      (let ((onto-task-pairs 
             (mapcar #'(lambda (x)(list (name (home-ontology (get-ocml-class x))) x)) 
                     (eval `(ocml-eval (setofall ?x (problem-type ?x)))))))
       ;(print onto-task-pairs)
        (setf tasks (append onto-task-pairs tasks))))
    (select-ontology current-ontology)
    tasks))


(defun all-tasks-with-top-ontologies ()
  (let ((current-ontology (name *current-ontology*))
        (tasks nil)
        (task-ontologies (task-ontologies)))
    (dolist (onto task-ontologies)
      (select-ontology (name onto))
      (let ((onto-task-pairs 
             (mapcar #'(lambda (x)(list (name onto) x)) (eval `(ocml-eval (setofall ?x (problem-type ?x)))))))
       ;(print onto-task-pairs)
        (setf tasks (append onto-task-pairs tasks))))
     (select-ontology current-ontology)
     tasks))

(defun all-methods (task)
  "display all methods which tackle the current task."
  (let* ((current-ontology (name *current-ontology*))
         (methods nil)
         (ontology (name (home-ontology (get-ocml-class task))))
         (method-ontologies (method-ontologies-include ontology)))
    (dolist (onto method-ontologies)
      (select-ontology (name onto))
      (let ((onto-method-pairs
             (mapcar #'(lambda (x)(list (name (home-ontology (get-ocml-class x))) x))
                     (eval `(ocml-eval (setofall ?x (tackles-task-type ?x ',task)))))))
        (setf methods (append onto-method-pairs methods))))
    (select-ontology current-ontology)
    methods))

;;;FUNCTION IRS-UNFOLD-TASK-ONTOLOGY
;(defun irs-unfold-task-ontology (upcase-string)
;  (with-input-from-string (istream upcase-string)
;    (read istream)
;    (let ((*package* (find-package "OCML")))
;      (select-ontology (read istream))
;      (let
;          ((problem-types (eval `(ocml-eval (setofall ?x (problem-type ?x)))))) 
;        (mapcar #'(lambda (x) (format http::*http-stream* "~a~%" x)) problem-types)))))


;;;FUNCTION IRS-DISPLAY-METHOD-ONTOLOGIES
;(defun irs-display-method-ontologies (upcase-string)
;  (with-input-from-string (istream upcase-string)
;    (read istream)
;    (let* ((*package* (find-package "OCML"))
;           (type (read istream))
;           (current-ontology (read istream)))
;      (dolist (ont *all-ontologies*)
;        (if (and (right-value current-ontology (slot-value (cdr ont) 'includes))
;                 (eql (slot-value (cdr ont) 'ontology-type) ':METHOD))
;        (format http::*http-stream* "~a~%" (car ont)))))))

;;;FUNCTION IRS-UNFOLD-METHOD-ONTOLOGY
;(defun irs-unfold-method-ontology (upcase-string)
;  (with-input-from-string (istream upcase-string)
;    (read istream)
;    (let ((*package* (find-package "OCML")))
;      (select-ontology (read istream))
;      (let
;          ((problem-types (eval `(ocml-eval (setofall ?x (problem-type ?x)))))) 
;        (mapcar #'(lambda (x) (format http::*http-stream* "~a~%" x)) problem-types)))))


(defun get-input-roles (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (name (read istream))
           (class (get-ocml-class name))
           (roles (get-slot-values-from-class-structure class 'has-input-role))
           (pairs nil))
      (dolist (role roles)
        (let ((type-info (remove-duplicates
                          (find-option-value class role :type)))
              (default-value (find-option-value class role :default-value)))
          
          (if (holds? 'subclass-of (car type-info) 'set)
              ;;role has set type
              (setf pairs (cons (list role 
                                      (or (car type-info) *default-irs-type*)
                                      (or (car default-value) "()"))
                                      ;'set)
                                pairs))
            ;;role has non set type
            (setf pairs (cons (list role 
                                      (or (car type-info) *default-irs-type*)
                                      (or (car default-value) "no default value"))
                                      ;'nonset)
                              pairs)))))

      (mapcar #'(lambda (x) (format  http::*http-stream* "~a~%" (first x))
                  (format  http::*http-stream* "~a~%" (second x))
                  (format  http::*http-stream* "~a~%" (third x)))
                  ;(format  http::*http-stream* "~a~%" (fourth x)))
             (reverse pairs)))))


;(defun get-input-roles-old (upcase-string)
;  (with-input-from-string (istream upcase-string)
;    (read istream)
;    (let* ((*package* (find-package "OCML"))
;          (name (read istream))
;          (input-role-list (get-slot-values-from-class-structure (get-ocml-class name) 'has-input-role)))
;      (mapcar #'(lambda (x)(format http::*http-stream* "~a~%" x)) input-role-list))))

(defun unknown-action (action)
 (format http::*http-stream* "sorry! unknown action ~a~%" action))


(defun foo ()
(multiple-value-bind (values defaults)
    (get-slot-values-from-class-structure (get-ocml-class 'OPTIMAL-CLASSIFICATION-TASK) 'has-match-criterion)
(format t "~a~%" values)
(format t "~a~%" defaults)))


;;this version does not work
;(defun get-direct-subtypes (upcase-string)
;  (with-input-from-string (istream upcase-string)
;    (read istream)
;    (let* ((*package* (find-package "OCML"))
;          (name (read istream))
;          (subtype-list (eval `(ocml-eval (setofall ?x (direct-subclass-of ?x name))))))
;     (mapcar #'(lambda (x) (format http::*http-stream* "~a~%" x)) subtype-list))))

;(defun get-direct-subtypes (upcase-string)
;  (with-input-from-string (istream upcase-string)
;    (read istream)
;    (let* ((*package* (find-package "OCML"))
;           (name (read istream))
;           (subtype-list (eval `(ocml-eval (setofall ?x (direct-subclass-of ?x ',name))))))
;      (mapcar #'(lambda (x) (format http::*http-stream* "~a~%" x)) subtype-list))))

(defun get-direct-subtypes (upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (name (read istream))
           (subtype-list (eval `(ocml-eval (setofall ?x (direct-subclass-of ?x ',name))))))
      (mapcar #'(lambda (x) (format http::*http-stream* "~a~%" x)) subtype-list))))

;(defun get-direct-subtypes (upcase-string)
;  (with-input-from-string (istream upcase-string)
;    (read istream)
;    (let* ((*package* (find-package "OCML"))
;           (name (read istream))
;           (class (get-ocml-class name))
;           (ontology (name (home-ontology class)))
;           subtype-list)
;      (select-ontology ontology)
;      (format http::*http-stream* "~a~%" ontology)
;      (setf subtype-list (eval `(ocml-eval (setofall ?x (direct-subclass-of ?x ',name)))))
;      (mapcar #'(lambda (x) (format http::*http-stream* "~a~%" x)) subtype-list))))

;(defun foo ()
;  (dolist ont *all-ontologies*
;    (if (eq (slot-value (cdr x) 'ontology-type) ':task)
;        (format t "~a~%" (name ont)))))




;;find all instance
;(defun irs-get-all-instances(upcase-string)
;  (with-input-from-string (istream upcase-string)
;    (read istream)
;    (let* ((*package* (find-package "OCML"))
;           (class (read istream))
;           (slot (read istream))
;           (type (find-option-value (get-ocml-class class) slot :type))
;           (instances (eval `(ocml-eval (all-instances ',(car type))))))
;      (mapcar #'(lambda (x) (format http::*http-stream* "~a~%" x)) instances))))

;;IRS-GET-ALL-INSTANCES
;(defun irs-get-all-instances(upcase-string)
;  (with-input-from-string (istream upcase-string)
;    (read istream)
;    (let* ((*package* (find-package "OCML"))
;           (type (read istream))
;           (instances (eval `(ocml-eval (all-instances ',type)))))
;      (mapcar #'(lambda (x) (format http::*http-stream* "~a~%" x)) instances))))

(defun old-irs-get-all-instances(upcase-string)
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let ((*package* (find-package "OCML"))
           (type-in (read istream))
           type)
      ;;inform the type-in infmation to irsapplet
      (if (holds? 'subclass-of type-in 'set)
          (progn
            (setf type (car (all-class-slot-values type-in 'element-type)))
            (format  http::*http-stream* "~a~%" 'set))
        (progn
          (format  http::*http-stream* "~a~%" 'nonset)
          (setf type type-in)))
      (let ((instances (eval `(ocml-eval (setofall ?x (,type ?x))))))
        (mapcar #'(lambda (x) (format http::*http-stream* "~a~%" x)) instances)))))



;(find-option-value (get-ocml-class 'SINGLE-SOLUTION-CLASSIFICATION-TASK) 'has-candidate-solutions :type)


;;these functions are used to load user's domain ontology and application definitions
(defun read-in-user-domain-kb (upcase-string)
  (let ((*package* (find-package "OCML"))
        (new-upcase-string (substitute #\Newline #\@ upcase-string)))
    (with-input-from-string (istream new-upcase-string)
      (read istream)
      (do ((forms nil)
           (form (read istream nil nil nil) (read istream nil nil nil)))
          ((null form) (reverse forms))
        (push form forms)))))
       ;; (format http::*http-stream* "~a~%" forms)))))

(defun read-in-user-domain-kb-saved-as-file (upcase-string)
  (let ((*package* (find-package "OCML"))
        (new-upcase-string (substitute #\Newline #\@ upcase-string)))
    (with-open-file (ofile "c:\wenjintest" :direction :output :if-does-not-exist :create)
      (format ofile "~a~%" new-upcase-string)
      (with-input-from-string (istream new-upcase-string)
        (read istream)
        (do ((forms nil)
             (form (read istream nil nil nil) (read istream nil nil nil)))
            ((null form) (reverse forms))
          (push form forms)
          (format http::*http-stream* "~a~%" forms))))))

;;load user domain 
;;(defmethod get-application-inputs ((task-type (eql 'classification-task)) input-strings)
;;  (let* ((classes-string (get-input-type 'candidate-classes input-strings))
         ;;(filter-out-html classes-string))) don't do this because of
         ;;<=
(defun get-application-inputs (input-strings)
  (let* ((forms (read-in-user-domain-kb input-strings)) 
         all-classes 
         application-inputs)
    (multiple-value-bind (domain-ontology defined-ok-p)
        (create-or-select-domain-ontology forms)
      (cond (defined-ok-p
	     (setf all-classes (get-all-classes forms))
	     (select-ontology domain-ontology)
	     (setf application-inputs 
	           (list (list 'candidate-classes all-classes)))
             (load-domain-kb forms)
             (values application-inputs domain-ontology t)
             (format http::*http-stream* "~a~%" domain-ontology))
            ;;domain-ontology contains an error message if defined-ok-p is nil
            (t (format http::*http-stream* "~a~%" "domain-ontology contains an error message"))))))

(defun create-or-select-domain-ontology (forms)
  (let ((ontology-definition (find-ontology-definition forms)))
    (if ontology-definition
        (cond ((eq (car ontology-definition) 'def-ontology)
               (eval ontology-definition) (values (second ontology-definition) t))
              ((eq (car ontology-definition) 'in-ontology)
               (if (get-ontology (second ontology-definition))
                   (values (second ontology-definition) t)
                   (values (format nil "~a is not a defined ontology."
                                   (second ontology-definition)) nil))))
        (values (create-domain-ontology) t))))

(defun find-ontology-definition (forms)
  (do ((current-forms forms (cdr current-forms)))
      ((or (null current-forms) (eq (caar current-forms) 'def-ontology)
           (eq (caar current-forms) 'in-ontology))
       (car current-forms))))

(defun create-domain-ontology ()
  (let ((name (gentemp "DOMAIN-ONTOLOGY")))
    (def-ontology-internal name "This was defined by IRS" nil)
    name))
  
(defun get-all-classes (forms)
  (mapcan #'(lambda (form)
              (when (eq (car form) 'def-class)
                (list (second form))))
          forms))

(defun load-domain-kb (forms)
  (mapc #'eval forms))


;;(ocml-eval (setofall ?x (tackles-task-type ?x 'OPTIMAL-CLASSIFICATION-TASK)))

;;;;;;;;;;;;;;;;;;Staff from experiment.lisp;;;;;;;;;;;;;;;;;;;;;
(defun task-ontologies ()
 (let ((task-ontologies nil))
  (dolist (ont *all-ontologies*)
    ;(print ont)
    ;(print (slot-value (cdr ont) 'ontology-type))
    (if (eql (slot-value (cdr ont) 'ontology-type) ':task)
        (setf task-ontologies (cons (cdr ont) task-ontologies))))
  ;(print task-ontologies)
  ;;(remove-subsumed-ontologies task-ontologies)
  task-ontologies
))

(defun method-ontologies-include (task-ontology)
 (let ((method-ontologies nil))
  (dolist (ont *all-ontologies*)
    ;(print ont)
    ;(print (slot-value (cdr ont) 'ontology-type))
    (if (eql (slot-value (cdr ont) 'ontology-type) ':method)
        (setf method-ontologies (cons (cdr ont) method-ontologies))))
  ;(print task-ontologies)
  (remove-subsumed-ontologies (local-super-ontologies task-ontology method-ontologies))))

(defun local-super-ontologies (ontology-name ontology-list)
  (let ((ontology (get-ontology ontology-name)))
    (remove nil (mapcar #'(lambda (x)(if (member ontology (sub-ontologies x)) x)) ontology-list))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;IRS-Describe-function;;;;;;;;;;;;;;;;;;;;;;;


(defun collect-names-matching-substring-in-all-ontologies (string
                                                           &key
 
(home-ontology-only? t)
(include-classes? t)
 
(include-relations? t)
(include-axioms? t)
 
(include-functions? t)
 
(include-bc-rules? t))
   (Let ((result))
     (dolist (pair *all-ontologies*)
       (let ((result2 (collect-names-matching-substring-in-ontology
              string (cdr pair)
              :home-ontology-only? home-ontology-only?
              :include-classes? include-classes?
              :include-relations? include-relations?
              :include-axioms? include-axioms?
              :include-functions? include-functions?
              :include-bc-rules? include-bc-rules?)))
         (when result2
           (push result2 result))))
     result))


(defun collect-names-matching-substring-in-ontology (string ontology &key
 
(home-ontology-only?)
 
(include-classes? t)
 
(include-relations? t)
(include-axioms? t)
 
(include-functions? t)
 
(include-bc-rules? t))
   (let* ((classes)
          (directory (ontology-directory ontology))
          (result (append
                   (when include-classes?
                     (setf classes
                           (collect-all-keys-containing-substring
                            (ontology-classes directory)
                            ontology
                            string
                            home-ontology-only?))
                     (when classes
                       (list :classes classes)))
                   (when include-relations?
                     (Let ((keys
                            (collect-all-keys-containing-substring
                             (ontology-relations directory) ontology
                             string
                             home-ontology-only? )))
                       (when classes
                         (setf keys (set-difference keys classes)))
                       (when keys
                         (list :relations keys))))
                   (when include-functions?
                     (Let ((keys  (collect-all-keys-containing-substring
                                   (ontology-functions directory)
                                   ontology
                                   string
                                   home-ontology-only?)))
                       (when keys
                         (list :functions keys))))
                   (when include-axioms?
                     (Let ((keys
                            (collect-all-keys-containing-substring
                             (ontology-axioms directory)
                             ontology
                             string
                             home-ontology-only?)))
                       (when keys
                         (list :axioms keys))))
                   (when include-bc-rules?
                     (Let ((keys

                            (collect-all-keys-containing-substring
                             (ontology-bc-rules directory)
                             ontology
                             string
                             home-ontology-only?)))
                       (when keys
                         (list :bc-rules keys)))))))
     (when result
       (cons result
             ontology))))


;(defun collect-all-keys-containing-substring (hash-table
;                                                ontology
;                                                string home-ontology-only?
;                                                &aux result)
;    (maphash #'(lambda (key value)
;                 key
;                 (let ((key-string (format nil "~A" key )))
;                   (when (search string key-string :test #'string-equal)
;                     (unless (and home-ontology-only?
;                                  (not (eq (home-ontology value)ontology)))
;                       (push key result)))))
;             hash-table)
;    result)

(defun collect-all-keys-containing-substring (hash-table
                                                 ontology
                                                 string home-ontology-only?
                                                 &aux result)
     (maphash #'(lambda (key value)
                  key
                  (let ((key-string (if (and (atom key) (symbolp key))
                                        (symbol-name key)
                                        (format nil "~A" key ))))
                    (when (search string key-string :test #'string-equal)
                      (unless (and home-ontology-only?
                                   (not (eq (home-ontology value)ontology)))
                        (push key result)))))
              hash-table)
     result)

(defun irs-describe-instance (stream name &key class-name deduce-all-values?)
   "If deduce-all-values? is T then we use all inference methods
    at our disposal to find slot values"
     (Let ((inst (find-current-instance name class-name))
           (unbound-slots)
            (parent))
       (when inst
         (setf parent (parent-class inst))
         (format stream "~%Instance ~s of class ~S~%"
                 name (name parent))
         (loop for slot in (domain-slots parent)
               for values = (if deduce-all-values?
                              (setofall '?x `(,slot ,name ?x))
                              (get-slot-values inst slot))
               ;;;;;when values
               do
               (if values
                 (format stream "~%~S: ~S~{, ~S~}~%"
                         slot (car values)(cdr values))
                 (push slot unbound-slots))
               finally
               (when unbound-slots
                 (format stream "~%The following slots have no value: ~S~{, ~S~}~%"
                          (car unbound-slots)(cdr unbound-slots))))

         (values))))

(defun irs-describe-class (stream name &optional local-defs-only?)
   (if local-defs-only?
     (irs-describe-class-local-info stream name)
     (irs-describe-class-info stream name)))


;;;IRS-DESCRIBE-CLASS-INFO -  Prints out all the currently applicable
;;;information about a class.  By 'currently applicable' I mean
;;;that superseded slot values are not displayed.
;(defun irs-describe-class-info (name)
;  (Let* ((class (get-ocml-class name))
;         (supers (mapcar #'name (domain-superclasses class))))
;    (when class
;      (format http::*http-stream* "~%Class ~S~%" name)
;      (format http::*http-stream* "~% Superclasses: ~S~{, ~S~}"
;              (car supers)
;              (cdr supers))
;      (loop for slot in (domain-slots class)
;            do
;            (format http::*http-stream* "~2% ~s"slot)
;            (multiple-value-bind (values defaults)
;                                 (get-slot-values-from-class-structure
;                                  class slot )
;              (if values
;                (format http::*http-stream* "~%  ~S: ~S~{, ~S~}"
;                          :value
;                          (car values)
;                          (cdr values))
;                (when defaults
;                  (format http::*http-stream* "~%  ~S: ~S~{, ~S~}"
;                          :default-value
;                          (car defaults)
;                          (cdr defaults)))))
;            (let ((type-info (find-option-value class slot :type)))
;              (when type-info
;                (format http::*http-stream* "~%  ~S: ~S~{, ~S~}"
;                          :type
;                          (car type-info)
;                          (cdr type-info))))
;            (loop for option in '(:min-cardinality
;                                  :max-cardinality
;                                  :inheritance)
;                  for value = (find-option-value class slot option)
;                  when value
;                  do
;                  (format http::*http-stream* "~%  ~S: ~S"
;                          option value))
;            (let ((doc (find-slot-documentation class slot)))
;              (when doc
;                (format http::*http-stream* "~%  ~S: ~S"
;                          :documentation doc)))))))
;
;(defun irs-describe-class-local-info (name)
;
;  (Let* ((class (get-ocml-class name))
;         (supers (mapcar #'name (domain-superclasses class))))
;    (when class
;
;      (format http::*http-stream* "~%Class ~S~%" name)
;      (format http::*http-stream* "~% Superclasses: ~S~{, ~S~}"
;              (car supers)
;              (cdr supers))
;
;      (loop
;            for slot-info in (ocml-options class)
;            do
;            (format http::*http-stream* "~2% ~s"(car slot-info))
;            (loop
;                  for pair in (cdr slot-info)
;                  do
;                  (format http::*http-stream* "~%  ~S: ~S"
;                          (car pair)
;(second pair)))))))


;;;IRS-DESCRIBE-CLASS-INFO -  Prints out all the currently applicable
;;;information about a class.  By 'currently applicable' I mean
;;;that superseded slot values are not displayed.
(defun irs-describe-class-info (stream name)
   (Let* ((class (get-ocml-class name))
          (supers (mapcar #'name (domain-superclasses class)))
          (subs (mapcar #'name (current-subclasses class))))
     (when class
       (format stream "~%Class ~:(~S~)~%" name)
       (format stream "~% Ontology: ~:(~s~)~%" (name (home-ontology class)))

       (format stream "~% Superclasses: ~:(~s~)~{, ~:(~s~)~}"
               (car supers)
               (cdr supers))
       (format stream "~2% Subclasses: ~:(~s~)~{, ~:(~s~)~}~%"
               (car subs)
               (cdr subs))
       (loop for slot in (domain-slots class)
             do
             (format stream "~2% ~:(~s~)"slot)
             (multiple-value-bind (values defaults)
                                  (get-slot-values-from-class-structure
                                   class slot )
               (if values
                 (format stream "~%  ~:(~s~): ~:(~s~)~{, ~:(~s~)~}"
                           :value
                           (car values)
                           (cdr values))
                 (when defaults
                   (format stream "~%  ~:(~s~): ~:(~s~)~{, ~:(~s~)~}"
                           :default-value
                           (car defaults)
                           (cdr defaults)))))
             (let ((type-info (remove-duplicates
                               (find-option-value class slot :type))))
               (when type-info
                 (format stream "~%  ~:(~s~): ~:(~s~)~{, ~:(~s~)~}"
                           :type
                           (car type-info)
                           (cdr type-info))))
             (loop for option in '(:min-cardinality
                                   :max-cardinality
                                   :inheritance)
                   for value = (find-option-value class slot option)
                   when value
                   do
                   (format stream "~%  ~:(~s~): ~:(~s~)"
                           option value))
             (let ((doc (find-slot-documentation class slot)))
               (when doc
                 (format stream "~%  ~:(~s~): ~:(~s~)"
                           :documentation doc)))))))


(defun get-class-heading (name class-name task-name psm-name goal-name web-service-name mediator-name)
  (if (holds? 'subclass-of name 'task)
      (if (holds? 'subclass-of name 'problem-solving-method)
          psm-name
        task-name)
    (cond ((holds? 'subclass-of name 'goal)
           goal-name)
          ((holds? 'subclass-of name 'web-service)
           web-service-name)
          ((holds? 'subclass-of name 'mediator)
           mediator-name)
          (t class-name))))

(defun get-class-title-type (name)
  (get-class-heading name "Class" "Task" "Problem Solving Method" "Goal" "Web Service" "Mediator"))

(defun get-class-superclass-title-type (name)
  (get-class-heading name "Superclass" "Parent Task" 
                     "Parent Problem Solving Method" "Parent Goal" "Parent Web Service" "Parent Mediator"))

(defun get-class-subclass-title-type (name)
  (get-class-heading name "Subclass" "Child Task" 
                     "Child Problem Solving Method" "Child Goal" "Child Web Service" "Child Mediator"))

(defun irs-describe-class-local-info (stream name)

   (Let* ((class (get-ocml-class name))
          (supers (mapcar #'name (domain-superclasses class)))
          (subs (mapcar #'name (current-subclasses class))))
     (when class

       (format stream "~%~a ~:(~:(~s~)~)~%" (get-class-title-type name) name)
       (format stream "~% Ontology: ~:(~:(~s~)~)~%" (name (home-ontology class)))
       (format stream "~% ~a: ~:(~:(~s~)~)~{, ~:(~:(~s~)~)~}"
               (get-class-superclass-title-type name)
               (car supers)
               (cdr supers))
       (format stream "~2% ~a: ~:(~:(~s~)~)~{, ~:(S~)~}"
               (get-class-subclass-title-type name)
               (car subs)
               (cdr subs))

       (loop
             for slot-info in (ocml-options class)
             do
             (format stream "~2% ~:(~:(~s~)~)"(car slot-info))
             (loop
                   for pair in (cdr slot-info)
                   do
                   (format stream "~%  ~:(~:(~s~)~): ~:(~:(~s~)~)"
                           (car pair)
                           (second pair)))))))

(defun irs-describe-relation (stream name)
   (Let* ((rel (get-relation name)))
     (if rel
       (irs-describe-relation-internal stream name rel)
       (ocml-warn "~:(~s~) is not a relation" name))))

(defun irs-describe-relation-internal (stream name rel)
   (let ((flag t))
     (format stream "~%Relation ~:(~s~) ~:(~s~)~%" name (schema rel))
     (format stream "~% Ontology: ~:(~s~)~%" (name (home-ontology rel)))
     (if (ocml-documentation rel)
       (format stream "~% Documentation: ~:(~s~)~%" (ocml-documentation rel))
       (setf flag nil))

     (when (local-slot-of rel)
       (unless flag
         (format stream "~%"))
       (format stream "~% Local slot of: ~:(~s~)~{, ~:(~s~)~}"
               (name (car (local-slot-of rel)))
               (mapcar #'name  (cdr (local-slot-of rel)))))
     (when (slot-of rel)
       (Let ((non-local-slots (set-difference (slot-of 
rel)(local-slot-of rel))))
         (when non-local-slots
           (format stream "~2% Also slot of: ~:(~s~)~{, ~:(~s~)~}"
                   (name (car non-local-slots))
                   (mapcar #'name (cdr non-local-slots))))))
     (unless flag
       (format stream "~%"))
     (when (constraint rel)
       (format stream "~% Constraints: ~:(~s~)~%" (constraint rel)))

     (when (iff-def rel)
       (format stream "~% Iff-def: ~:(~s~)~%"
               (car (bc-clause-antecedents  (iff-def rel)))))

     (when (sufficient rel)
       (format stream "~% Sufficient: ~:(~s~)~%"
               (car (bc-clause-antecedents (sufficient rel)))))
     (when (defined-by-rule rel)
       (format stream "~% Associated rules: ~:(~s~)~{, ~:(~s~)~}~%"
                   (name (car (defined-by-rule rel)))
                   (mapcar #'name
                           (cdr (defined-by-rule rel)))))

     (when (prove-by rel)
       (format stream "~% Prove by: ~:(~s~)~%"  (car (bc-clause-antecedents
                                            (prove-by rel)))))

     (when (lisp-fun rel)
       (format stream "~% Prove by: ~:(~s~)~%" (lisp-fun rel)))

     (when (relation-instances rel)
       (format stream "~% Relation Instances: ~:(~s~)~{, ~:(~s~)~}"
              (args (car (relation-instances rel)))
               (mapcar #'args (cdr (relation-instances rel)))))))

(defun irs-describe-function (stream name)
   (Let* ((f (get-function name)))
     (if f
       (irs-describe-function-internal stream name f)
       (ocml-warn "~:(~s~) is not a function" name))))

(defun irs-describe-function-internal (stream name f)
   (let ((flag t))
     (format stream "~%Function ~:(~s~) ~:(~s~)~%" name (schema f))

     (format stream "~% Ontology: ~:(~s~)~%" (name (home-ontology f)))
     (if (ocml-documentation f)
       (format stream "~% Documentation: ~:(~s~)~%" (ocml-documentation f))
       (setf flag nil))

     (unless flag
       (format stream "~%"))
     (when (constraint f)
       (format stream "~% Constraints: ~:(~s~)~%" (constraint f)))

     (when (definition f)
       (format stream "~% Def: ~:(~s~)~%"
               (definition f)))

     (when (body f )
       (format stream "~% Body: ~:(~s~)~%"
               (body f)))

     (when (lisp-fun f)
       (format stream "~% Lisp attachment: ~:(~s~)~%" (lisp-fun f)))))
