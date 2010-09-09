(in-package :bpmo-to-irs)

(defmethod initialise-application ((app (eql :bpmo-to-irs)))
  (webonto:require-ontologies
   '(:bpmo12)))

;;(wp::irs-solve-goal 'process1-goal 'process1-goal '((REQUESTED-CONTENT football) (BY-USER john)) *standard-output*)

(defmethod start-application ((app (eql :bpmo-to-irs)))
  'no-operation)

(defvar *tid-demo-ontology*
  'ocml::tid-bpmo-1-2)

(defvar *iswc2007-wsmo-ontology*
  'ocml::iswc2007-goals)

(defun test-bpmo-orchestration-generator ()
  (ocml::select-ontology *tid-demo-ontology*)
  (let ((processes (ocml::setofall '?x '(ocml::process ?x)))
        (result nil))
    (dolist (process processes)
      (let ((orchestration 
             (ocml::findany 
              '(?x ?i ?o) 
              `(ocml::generate-wsmo-orchestration-elements ,process ?x ?i ?o))))
        (unless (eq orchestration :nothing)
          (push (cons process orchestration)
                result))))
    result))

(defun test-bpmo-to-irs-translator ()
  (generate-irs-code "wsmo" 'ocml::SERVICE-PROVIDER *tid-demo-ontology*))

(defun ip::generate-irs-code-for-all-processes 
       (user-name source-ontology html-stream)
  (generate-irs-code-for-all-processes 
   user-name source-ontology html-stream))

(defun send-error-message (message stream)
  ;;also send the error message to standard-output so it can be seen!
  (format t message)
  (format stream message))

(defun generate-irs-code-for-all-processes (user-name source-ontology html-stream)
  ;;(setf uu user-name yy source-ontology)
  (handler-case 
      (progn 
        (ocml::select-ontology source-ontology)
        (let ((processes (ocml::setofall '?x '(ocml::process ?x))))
          (dolist (process processes)
            (generate-irs-code user-name process source-ontology))))
    (ocml::parse-wsmo-goal-uri-error
            (c)
      (send-error-message 
       (format nil "Error: had a problem parsing the uri for the goal in the goal task ~a~%"
               (ocml::has-goal-task c))
       html-stream))
    (ocml::goal-missing-error
            (c)
      (send-error-message 
       (format nil "Error: A goal needs to be defined for ~a~%"
               (ocml::has-goal-task c))
       html-stream))
    (error (c)
      (send-error-message
       (format nil
               "Error: An error has occurred when trying to translate from BPMO to IRS-III~%")
       html-stream))))

(defun create-new-bpmo-to-irs-name (process type)
  (intern (concatenate 'string 
                       ;;(remove-if #'digit-char-p 
                                  (remove #\_ (symbol-name process))
                                  ;;)
                       "-" (symbol-name type))
          (find-package "OCML")))

(defun create-bpmo-to-irs-ontology-names (process)
  (values (create-new-bpmo-to-irs-name process 'goal)
          (create-new-bpmo-to-irs-name process 'mediator)
          (create-new-bpmo-to-irs-name process 'web-service)))

(defun pathname-equal (x y)
  (and x y 
        (pathnamep x) (pathnamep y)
        (equal (pathname-host x) (pathname-host y))
        (equal (pathname-device x) (pathname-device y))
        (equal (pathname-directory x) (pathname-directory y))
        (equal (pathname-name x) (pathname-name y))
        (or (or (eq (pathname-version x) :unspecific)
                (eq (pathname-version y) :unspecific))
            (equal (pathname-version x) (pathname-version y)))))

(defun make-ontology-file-pathname (ontology-struct ontology-filename)
  (pathname 
   (translate-logical-pathname (format nil "~a~a.lisp"  (ocml::ontology-pathname ontology-struct) 
                                       ontology-filename))))

(defun maybe-delete-ontology-buffers (ontology)
  (let* ((ontology-struct (ocml::get-ontology ontology))
         (ontology-files (cons (pathname-name ocml:*load-filename*)
                               (ocml::ontology-files ontology-struct)))
         (ontology-pathnames 
          (mapcar #'(lambda (file)
                      (make-ontology-file-pathname ontology-struct file))
                  ontology-files)))
    ;;goto the main buffer because we don't want the buffer we delete
    ;;to be the current buffer
    ;;take out for delivery version
    ;;(editor::goto-buffer (editor:buffer-from-name "Main") t)
    (dolist (buffer editor:*buffer-list*)
      (let ((buffer-pathname (editor:buffer-pathname buffer)))
        (when (and buffer-pathname
                   (find buffer-pathname ontology-pathnames
                         :test #'pathname-equal))
          (editor::delete-buffer buffer))))))

(defun maybe-delete-ontology (ontology)
  (when (ocml::get-ontology ontology)
    (ocml::select-ontology 'ocml::wsmo)
    (maybe-delete-ontology-buffers ontology)
    (ocml::delete-ontology ontology)))

(defun create-ontology (user-name ontology type parents)
  (web-onto::define-new-ontology *standard-output* ontology type parents
                                 user-name nil))

(defun create-target-ontologies (process ontologies user-name)
  (multiple-value-bind (goal-ontology mediator-ontology web-service-ontology)
      (create-bpmo-to-irs-ontology-names process)
    (mapc #'maybe-delete-ontology (list goal-ontology mediator-ontology web-service-ontology))
    (create-ontology user-name goal-ontology :goal ontologies)
    (create-ontology user-name web-service-ontology :web-service (list goal-ontology))
    (create-ontology user-name mediator-ontology :mediator (list web-service-ontology))
    (values goal-ontology mediator-ontology web-service-ontology)))

(defun generate-irs-code (user-name process source-ontology)
  (ocml::select-ontology source-ontology)
  (let ((result (ocml::findany 
                 '(?x ?input-roles ?output-roles ?ontologies)
                 `(ocml::generate-wsmo-orchestration-elements 
                   ,process ?x 
                   ?input-roles ?output-roles ?ontologies))))
    (unless (or (null result) (eq result :nothing))
      (destructuring-bind (orchestration-definition input-roles output-roles ontologies)
          result 
        (unless (or (null orchestration-definition) (eq orchestration-definition :nothing))
          (multiple-value-bind (goal-ontology mediator-ontology web-service-ontology)
              (create-target-ontologies process ontologies user-name)
            ;;goal has the same name as the goal ontology to help in transferring 
            ;;over to WSMO Studio (ocml::add-extension process 'ocml::-goal))
            (let ((goal-name goal-ontology) 
                  ;;mediator  has the same name as the mediator ontology to help in transferring 
                  ;;over to WSMO Studio
                  (mediator-name mediator-ontology) 
                  ;;(ocml::add-extension process 'ocml::-mediator))
                  ;;web-service  has the same name as the web-service 
                  ;;ontology to help in transferring 
                  ;;over to WSMO Studio
                  ;;(ocml::add-extension process 'ocml::-web-service))
                  (web-service-name web-service-ontology) 
                  (interface-name (ocml::add-extension process 'ocml::-interface))
                  (orchestration-name (ocml::add-extension process 'ocml::-orchestration)))
              (generate-bpmo-derived-goal user-name goal-name input-roles output-roles goal-ontology)
              (generate-bpmo-derived-mediator user-name mediator-name 
                                              goal-name web-service-name mediator-ontology)
              (generate-bpmo-derived-web-service 
               user-name web-service-name interface-name web-service-ontology)
              (generate-bpmo-derived-interface 
               user-name interface-name orchestration-name web-service-ontology)
              (generate-bpmo-derived-orchestration 
               user-name orchestration-name orchestration-definition web-service-ontology))
            (load-ontologies goal-ontology web-service-ontology mediator-ontology)))))))

(defun load-ontologies (&rest ontologies)
  (mapc #'(lambda (ontology)
            (web-onto::internal-load-single-ontology ontology t))
        ontologies))

(defvar *default-role-type* 'ocml::string) ;;'ocml::symbol)

(defvar *default-role-soap-grounding* "string") ;;"sexpr")

(defun generate-role (role-name)
  (list role-name *default-role-type* *default-role-soap-grounding*)) 

(defun make-non-functional-properties-name (x)
  (ocml::add-extension x 'ocml::-non-functional-properties))

(defun generate-bpmo-derived-goal (user-name goal-name input-roles output-roles target-ontology)
  (let ((non-functional-properties (make-non-functional-properties-name goal-name)))
    (wp::create-and-save-non-functional-properties-class
     user-name target-ontology non-functional-properties 'ocml::non-functional-properties
     nil)
    (wp::internal-save-goal-description
     user-name target-ontology
     goal-name 
     'ocml::goal (mapcar #'generate-role input-roles)
     (generate-role (car output-roles)) nil nil nil non-functional-properties)))

(defun generate-bpmo-derived-mediator (user-name mediator-name goal-name web-service-name 
                                                 target-ontology)
  (let ((non-functional-properties 
         (make-non-functional-properties-name mediator-name)))
    (wp::create-and-save-non-functional-properties-class
     user-name target-ontology non-functional-properties 'ocml::non-functional-properties
     nil)
    (wp::internal-save-mediator-description
     user-name target-ontology mediator-name  
     'ocml::mediator nil goal-name web-service-name nil nil non-functional-properties)))

(defun generate-bpmo-derived-web-service (user-name web-service-name interface-name target-ontology)
  (let ((non-functional-properties 
         (make-non-functional-properties-name web-service-name)))
    (wp::create-and-save-non-functional-properties-class
     user-name target-ontology non-functional-properties 'ocml::non-functional-properties
     nil)
    (wp::internal-save-web-service-description
     user-name target-ontology web-service-name 
     'ocml::web-service nil nil nil interface-name nil non-functional-properties)))

(defun generate-bpmo-derived-interface (user-name interface-name orchestration-name target-ontology)
  (let ((non-functional-properties 
         (make-non-functional-properties-name interface-name)))
    (wp::create-and-save-non-functional-properties-class
     user-name target-ontology non-functional-properties 'ocml::non-functional-properties
     nil)
    (wp::internal-save-interface-description
     user-name target-ontology interface-name 'ocml::interface
     nil nil orchestration-name non-functional-properties)))

(defun generate-bpmo-derived-orchestration (user-name orchestration-name 
                                                      orchestration-definition target-ontology)
  (wp::internal-save-orchestration-description
   user-name target-ontology orchestration-name 'ocml::orchestration
                 (list 'orchestration-definition orchestration-definition)))
