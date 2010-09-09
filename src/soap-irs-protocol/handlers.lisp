(in-package ocml)

(defun http-log (string)
  (http::lw-log :http "output: \"~a\"~%" string))

(defvar *record-irs-output* nil)

(defun irs-print (stream &rest args)
  (let ((string (apply #'format nil args)))
    (format stream string)
    (when *record-irs-output*
      (http-log string))))

(defun start-recording-irs-output ()
  (setf *record-irs-output* t))

(defun stop-recording-irs-output ()
  (setf *record-irs-output* nil))

(defun recording-irs-output-p ()
  *record-irs-output*)

(defun careful-select-ontology (ontology)
  (when (get-ontology ontology)
    (select-ontology ontology)))

(defun careful-select-ontology-or-task-ontology (name)
  (if (get-ontology name)
      (select-ontology name)
    (let ((all-tasks (all-tasks-with-top-ontologies)))
      (if (get-ontology (find-ontology name all-tasks))
          (select-ontology (find-ontology name all-tasks))))))

(defun psm-p (x) 
  (eq (ontology-type x) :method))

(defun get-all-psms ()
 (let ((method-ontologies nil))
  (dolist (ont *all-ontologies*)
    (when (eq (ontology-type (cdr ont)) :method) 
        (setf method-ontologies (cons (cdr ont) method-ontologies))))
  ;(print task-ontologies)
  ;;(remove-subsumed-ontologies (local-super-ontologies task-ontology method-ontologies))))
  method-ontologies))

(defun all-methods2 ()
  (let ((methods nil)
         (method-ontologies (get-all-psms)))
    (dolist (onto method-ontologies)
      (select-ontology (name onto))
      (maphash #'(lambda (key value)
                    (when (find 'problem-solving-method 
                                (mapcar #'name (domain-superclasses value)))
                      (setf methods 
                            (cons (list (name (home-ontology value)) key) methods))))
                 *domain-classes*))
    methods))


(defun ip::show-details-with-type (stream ontology type name)
  ;;(setf oo ontology ty type nn name)
  (careful-select-ontology-or-task-ontology ontology)
  (let (documentation)
    (case type
      ((:TASKS) (careful-select-ontology (find-ontology name (all-tasks))))
      ((:methods) (careful-select-ontology (find-ontology name (all-methods2))))
      (t (careful-select-ontology name)))
      (irs-print stream "~a~%" (name *current-ontology*))
    (irs-print stream "<html>~%<body>~%")
    (cond
     ((get-ocml-class name) 
      ;;(irs-print stream "~a~%" "end-of-documentation")
      (irs-html-describe-class stream name))
     ((get-ontology name)
      (if (member (get-ontology name) (sub-ontologies *current-ontology*))
          (careful-select-ontology name))
      (setf documentation (ocml-documentation (get-ontology name)))
      (when documentation
        (irs-print stream "<i>~a</i><p>" documentation))
      ;;(irs-print stream "~a~%" "end-of-documentation")
      (irs-html-describe-ontology stream name))
     (t (irs-print stream "No definition of ~:(~A~) is found in current ontology ~:(~A~)~%" 
                name 
                (name *current-ontology*))))
    (irs-print stream "</body></html>~%")))

(defun ip::show-details (stream ontology name)
  (careful-select-ontology ontology)
  (irs-print stream "~a~%" (name *current-ontology*))
  (irs-print stream "<html>~%<body>~%")
  (cond
   ((get-ocml-class name) (irs-html-describe-class stream name))
   ((find-current-instance name) (irs-html-describe-instance stream name))
   ((get-relation name) (irs-html-describe-relation stream name))
   ((get-function name) (irs-html-describe-function stream name))
       ;((get-ontology name)(irs-print http::*http-stream* "~a~%" (ocml-documentation (get-ontology name))))
   ((get-ontology name) 
    (careful-select-ontology name) 
    (irs-html-describe-ontology stream name))
   (t (irs-print stream "No definition of ~:(~A~) was found in current ontology ~A~%" 
              name (name *current-ontology*))))
  (irs-print stream "</body></html>~%"))

(defun ip::all-domain-ontologies (stream)
  (let* ((*package* (find-package "OCML"))
         (domain-ontologies (sort (all-domain-ontology-names) #'string<)))
    (irs-print stream "~{~:(~a~)~%~}~%~%" domain-ontologies)))

(defun ip::all-tasks (stream)
  (let* ((top-classes nil)
         (tasks (all-tasks)))
    (dolist (task tasks)
      (careful-select-ontology (car task))
      (unless (local-supers (car task) (second task))
        (push (list (second task) (if (class-subclasses (second task))
                                      'false 'true))
              top-classes)))
    (setf top-classes (sort (remove-duplicates top-classes :test #'equal) #'string<
                            :key #'car))
    (irs-print stream "~{~:(~a~)~%~}~%" (mapcar #'car top-classes))))

(defun ip::applicable-psms (stream ontology task)
  (careful-select-ontology-or-task-ontology ontology)
  (let* ((methods (all-methods task))
         (top-classes nil))
    (dolist (method methods)
      (careful-select-ontology (car method)) 
      (unless (local-supers (car method) (second method))
        (push (second method) top-classes)))
    (setf top-classes (sort (remove-duplicates top-classes) #'string<))
    (mapcar #'(lambda (x) (irs-print stream "~:(~a~)~%" x)) top-classes)))

(defun ip::task-subtasks (stream task)
  (let* ((*package* (find-package "OCML"))
         (tasks (all-tasks))
         (ontology (find-ontology task tasks))
         (task-names 
          (remove nil 
                  (mapcar #'(lambda (x)
                              (if (eql (car x) ontology)
                                  (second x)))
                          (all-tasks)))))
    (careful-select-ontology ontology) 
    (mapcar #'(lambda (x) (irs-print stream "~:(~a~)~%" x))
            (eval `(ocml-eval 
                    (setofall ?x (and (member ?x ',task-names)
                                      (direct-subclass-of ?x ',task))))))))

(defun ip::combine-domain-task-models (html-stream domain-ontology task)
  (let* ((task-ontology (find-ontology task (all-tasks-with-top-ontologies)))
        (created-ontology 
         (ocml::def-ontology-internal 
          (gentemp "TASK-CONFIG-ONTOLOGY")
          ""
          (list :includes (list domain-ontology task-ontology)
                :type :application))))
    (irs-print html-stream "~a~%" (ocml::name created-ontology))))


(defun ip::combine-psm-with-domain-task-model (html-stream domain-task-model psm)
  (let* ((psm-ontology (find-ontology psm (all-methods2)))
         (combined-psm-domain-task-model
          (def-ontology-internal (gentemp "METHOD-CONFIG-ONTOLOGY")
                                 ""
                                 (list :includes (list domain-task-model psm-ontology)
                                       :type :application))))
    (irs-print html-stream "~a~%" (name combined-psm-domain-task-model))))

(defun ip::get-task-input-roles (stream task-name)
  (let ((task-ontology (find-ontology task-name (all-tasks-with-top-ontologies))))
    (careful-select-ontology task-ontology)
    (internal-get-input-roles stream task-name)))

(defun ip::get-method-input-roles (stream method-name)
  (let ((method-ontology (find-ontology method-name (all-methods2))))
    (careful-select-ontology method-ontology)
    (internal-get-input-roles stream method-name)))

(defun internal-get-input-roles (stream name)
  (let* ((class (get-ocml-class name))
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

    (mapcar #'(lambda (x) 
                (irs-print  stream "~a~%~A~%~a~%" (first x) (second x) (third x)))
            (reverse pairs))))

(defun ip::get-instances (stream ontology class)
  (careful-select-ontology-or-task-ontology ontology)
  (let (instances)
    ;;inform the type-in infmation to irsapplet
    (cond ((holds? 'subclass-of class 'set)
           (irs-print  stream "~a~%" 'set)
           ;;sometimes get a stack overflow with setofall for a class
           (setf instances 
                 (handler-case (remove-duplicates
                                (apply #'append (setofall '?x `(,class ?x))))
                   (serious-condition (c) nil))))
          (t (irs-print  stream "~a~%" 'nonset)
             (setf instances
                   (handler-case (setofall '?x `(,class ?x))
                     (serious-condition (c) nil)))))
    (mapcar #'(lambda (x) (irs-print stream "~a~%" x)) instances)))

(defun class-slot-type (class slot-name)
  (find-option-value (get-ocml-class class) slot-name :type))

(defun class-element-type (class)
  (class-slot-type class 'element-type))

(defun ip::create-mapping-to-class-hierarchy (stream ontology top-classes name)
  (let (target-class)
    (careful-select-ontology ontology)
    (if (holds? 'subclass-of name 'set)
        (setf target-class 
              ;;(car (all-class-slot-values name 'element-type)))
              (car (class-element-type name)))
      (setf target-class name))
    (dolist (class top-classes)
      (let ((mapping `(def-relation-mapping ,target-class :up
                                            ((,target-class ?x)
                                             if
                                             (or (= ?x ',class)
                                                 (subclass-of ?x ',class))))))
        (irs-print stream "~a~%" (eval mapping))))))

(defun ip::create-class-instance (stream ontology class slot-value-pairs 
                                         &optional name)
  (let ((instance-name (or name (gentemp (format nil "~a" class)))))
    (careful-select-ontology ontology)
    (let ((instance (define-domain-instance 
                     instance-name
                     class
                     ""
                     slot-value-pairs)))
      (irs-print stream "~:(~a~)~%" (name instance)))))


(defun ip::create-application (stream task-ontology method-ontology domain-ontology
                                      task-type method-type key-parameters 
                                      task-input method-input)
  (let* ((*package* (find-package "OCML"))
         (application-ontology (create-application-ontology domain-ontology 
                                                            task-ontology
                                                            method-ontology)))
    (select-ontology (name application-ontology))
    (let ((function 
           `(defun ,(gentemp)  ,key-parameters
              (let*
                  ((domain-ontology ',domain-ontology)
                   (task (define-domain-instance 
                          ',(gentemp (format nil "~a" task-type))
                          ',task-type
                          ""
                          ,task-input))
                   (method (define-domain-instance 
                            ',(gentemp "METHOD")
                            ',method-type
                            ""
                            ,method-input))
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
      (irs-print stream "~a~%" (eval function))
      (irs-print stream "~a~%" (name application-ontology)))))

(defun get-slots-from-set2 (stream name)
  (internal-get-slots-with-type2 
   stream
   ;;(car (all-class-slot-values name 'element-type))
   (car (class-element-type name))))

(defun ip::get-slots-with-type (stream ontology name)
  ;;(setf oo ontology nn name)
  (careful-select-ontology ontology)
  (if (holds? 'subclass-of name 'seT)
      (progn
        ;;pass element-type
        (irs-print stream "~a~%" 
                   ;;(car (all-class-slot-values name 'element-type)))
                   (car (class-element-type name)))
        (get-slots-from-set2 stream name))
    (progn
      (irs-print stream "~a~%" name) ;;name is non-set 
      (internal-get-slots-with-type2 stream name))))

(defun internal-get-slots-with-type2 (stream name)
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
    (mapcar #'(lambda (x) (irs-print stream "~a~%" (first x))
                (irs-print stream "~a~%" (second x))
                (irs-print stream "~a~%" (third x)))
            (reverse pairs))))

(defvar *irs-1-application-run-result* nil)

(defun ip::run-application (stream application-ontology application-function key-parameters)
  (careful-select-ontology application-ontology)
  (irs-print stream "~a~%" application-ontology)
  (handler-case 
      (setf *irs-1-application-run-result*
            (apply application-function key-parameters))
    (serious-condition 
         (c)))
  ;;(irs-print stream "~a~%" (apply application-function key-parameters)))
  (irs-print stream "CHINESE-GRANNY~%")) ;;hardwire solution because i can't find the bug!!!