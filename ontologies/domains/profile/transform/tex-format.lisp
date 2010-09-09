(in-package ocml)


(defun html-describe-item (stream name type)
  (let ((output (with-output-to-string (stream)
                  (format stream "<html>~%<body>~%")
                  (case type
                    ((:class) (html-describe-class stream name))
                    ((:instance) (html-describe-instance stream name))
                    ((:relation) (html-describe-relation stream name))
                    ((:function) (html-describe-function stream name))
                    ((:ontology)
                     (select-ontology name) 
                     (html-describe-ontology stream name))
                    ((:unknown) 
                     (html-describe-unknown-item stream name))
                    (t (format stream "Unknown type to describe ~a." 
                               type)))
                  (format stream "</body></html>~%"))))
    (format stream "~a~%"
            (remove #\linefeed (remove #\return output)))))


(defun html-describe-unknown-item (stream name)
  (cond ((get-ocml-class name)
         (html-describe-class stream name))
        ((get-ocml-relation name)
         (html-describe-relation stream name))
        ((get-ocml-function name)
         (html-describe-function stream name))
        ((get-ontology name)
         (select-ontology name)
         (html-describe-ontology stream name))
        ((find-current-instance name)
         (html-describe-instance stream name))
        (t (format stream "Sorry couldn't find ~(~a~)" name))))

(defvar *html-class-colour*
  'GREEN)

(defvar *html-instance-colour*
  "#1063B5")

(defvar *html-relation-colour*
  "#6E8c9c")

(defvar *html-function-colour*
  "#BB0004")

(defvar *html-rule-colour*
  "RED")

;;MediumBrown = new Color((float)0.99, (float)0.95, (float)0.85),
(defvar *html-ontology-colour* 
  (http::color-string :red 0.721567 :green 0.52549 :blue 0.0431372))

;;DarkSlateGray = new Color((float)0.1843133, (float)0.309803, (float)0.309803),
;;DarkSlateGrey = new Color((float)0.1843133, (float)0.309803, (float)0.309803),
;;DimGray = new Color((float)0.411764, (float)0.411764, (float)0.411764),
;;DimGrey = new Color((float)0.411764, (float)0.411764, (float)0.411764),
;;SlateGray = new Color((float)0.4392157, (float)0.501961, (float)0.564705),
;;SlateGrey = new Color((float)0.4392157, (float)0.501961, (float)0.564705),
;;LightSlateGray = new Color((float)0.466666, (float)0.533333, (float)0.599998),
;;LightSlateGrey = new Color((float)0.466666, (float)0.533333, (float)0.599998),

(defvar *html-slot-option-colour* 
  ;;SlateGray (http::color-string :red 0.4392157 :green 0.501961 :blue 0.564705))
   (http::color-string :red 0.1843133 :green 0.309803 :blue 0.309803))

(defun add-html-class-colour (x)
  (http::font x :color *html-class-colour*))

(defun add-html-instance-colour (x)
  (http::font x :color *html-instance-colour*))

(defun add-html-ontology-colour (x)
  (http::font x :color *html-ontology-colour*))

(defun add-html-relation-colour (x)
  (http::font x :color *html-relation-colour*))

(defun add-html-slot-option-colour (x)
  (http::font x :color *html-slot-option-colour*))

(defun insert-spaces (number)
  (let ((string ""))
    (dotimes (i number)
      (setf string (concatenate 'string string "&nbsp;")))
    string))
          
(defvar *number-of-instances-to-describe-limit*
  50)

(defun get-limited-current-instances (class)
  (let ((result (get-current-instances class)))
    (sort (if (> (length result)
                 *number-of-instances-to-describe-limit*)
              (subseq result 0 *number-of-instances-to-describe-limit*)
            result)
          #'(lambda (x y) (string< (name x) (name y))))))
        

(defun html-describe-class (stream name 
                                   &optional
                                   (lookup-function
                                    #'mnm-lookup-current-word))
  (Let* ((class (get-ocml-class name))
         supers subs ontology
         documentation)
    (cond (class    
           (setf supers
                 (remove-duplicates
                  (mapcar #'name (direct-domain-superclasses class)))
                 subs  (remove-duplicates
                        (mapcar #'name (current-subclasses class)))
                 ontology (name *current-ontology*))
           (format stream 
                   (http::internal-insert-ocml-links
                    (format nil
                            "<h2>~a ~:(~a~) (~:(~a~))</h2>" 
                            (get-class-title-type name)
                            name (name (home-ontology class)))
                    lookup-function
                    ontology))
           (setf documentation (ocml-documentation class))
           (when documentation
             (format stream 
                     (http::internal-insert-ocml-links
                      (format nil
                              "<i>~a</i><br>" documentation)
                      lookup-function
                      ontology)))
           (let ((publisher-location
                  (when (holds? 'subclass-of name 'problem-solving-method)
                    (cl-user::get-psm-full-publisher-location 
                     name
                     (name (home-ontology class))))))
             (when publisher-location
               (format stream "<b>Associated with web service:</b> ~a<br>" 
                       publisher-location)))
           (when supers
             (format stream 
                     (http::internal-insert-ocml-links
                      (format nil
                              "<b>~a:</b> ~:(~a~)~{, ~:(~a~)~}<br>"
                              (get-class-superclass-title-type name)
                              (add-html-class-colour (car supers))
                              (mapcar #'add-html-class-colour (cdr supers)))
                      lookup-function
                      ontology)))
           (when subs
             (format stream 
                     (http::internal-insert-ocml-links
                      (format nil
                              "<b>~a:</b> ~:(~a~)~{, ~:(~a~)~}<br>"
                              (get-class-subclass-title-type name)
                              (add-html-class-colour (car subs))
                              (mapcar #'add-html-class-colour (cdr subs)))
                      lookup-function
                      ontology)))
           (cond ((or (eq name 'goal)
                      (holds? 'subclass-of name 'goal))
                  (describe-goal-internals name class ontology stream
                                           lookup-function))
                 ((or (eq name 'web-service)
                      (holds? 'subclass-of name 'web-service))
                  (describe-web-service-internals name class ontology stream
                                                  lookup-function))
                 ((or (eq name 'mediator)
                      (holds? 'subclass-of name 'mediator))
                  (describe-mediator-internals name class ontology stream
                                               lookup-function))
                 ((or (eq name 'task)
                      (holds? 'subclass-of name 'task))
                  (if (or (eq name 'problem-solving-method)
                          (holds? 'subclass-of name 'problem-solving-method))
                      (describe-psm-internals name class ontology stream
                                              lookup-function)
                    (describe-task-internals name class ontology stream
                                             lookup-function)))
                 (t (describe-class-internals class ontology stream lookup-function))))
          (t (format stream "Sorry can't find ~a within the IRS Server" name)))))

(defun describe-slot (slot class ontology stream 
                           &optional
                           (lookup-function
                            #'mnm-lookup-current-word))
  (format stream "<br>~a<b>~:(~a~)</b>" 
          (insert-spaces 4) (add-html-relation-colour slot))
  (multiple-value-bind (values defaults)
      (get-slot-values-from-class-structure
       class slot )
    (when values
      (format stream 
              (http::internal-insert-ocml-links
               (format nil "<br>~a<b><i>~:(~a~):</i></b>~{~a~}"
                       (insert-spaces 5)
                       (add-html-slot-option-colour :value)
                       (mapcar #'(lambda (x) 
                                   (format-ocml-value x 1 nil))
                               values))
               lookup-function
               ontology)))
    (when defaults
      (format stream
              (http::internal-insert-ocml-links
               (format nil "<br>~a<b><i>~:(~a~):</i></b>~{~a~}"
                       (insert-spaces 5)
                       (add-html-slot-option-colour :default-value)
                       (mapcar #'(lambda (x) 
                                   (format-ocml-value x 1 nil))
                               defaults))
               lookup-function
               ontology)))
    (let ((type-info (remove-duplicates
                      (find-option-value class slot :type))))
      (when type-info
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "<br>~a<b><i>~:(~a~):</i></b>~{~a~}"
                         (insert-spaces 5)
                         (add-html-slot-option-colour :type)
                         (mapcar #'(lambda (x) 
                                     (format-ocml-value x 1 nil))
                                 type-info))
                 lookup-function
                 ontology))))
    (loop for option in '(:min-cardinality
                          :max-cardinality
                          :inheritance)
          for value = (find-option-value class slot option)
          when value
          do
          (format stream
                  (http::internal-insert-ocml-links
                   (format nil "<br>~a<b><i>~:(~a~):</i></b> ~:(~a~)"
                           (insert-spaces 5)
                           (add-html-slot-option-colour option)
                           value)
                   lookup-function
                   ontology)))
    (let ((doc (find-slot-documentation class slot)))
      (when doc
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "<br>~a<b><i>~:(~a~):</i></b> <i>~a</i>"
                         (insert-spaces 5) 
                         (add-html-slot-option-colour :documentation) doc)
                 lookup-function
                 ontology))))))

(defun describe-own-slot (slot-name-and-value ontology stream
                                              &optional
                                              (lookup-function
                                               #'mnm-lookup-current-word))
  (let ((slot (car slot-name-and-value))
        (slot-value (second slot-name-and-value)))
    (format stream "<br>~a<b>~:(~a~)</b>" 
            (insert-spaces 4) (add-html-relation-colour slot))
    (when slot-value
      (format stream 
              (http::internal-insert-ocml-links
               (format nil "<br>~a<b><i>~:(~a~):</i></b>~a"
                       (insert-spaces 5)
                       (add-html-slot-option-colour :value)
                       (format-ocml-value slot-value 7))
               lookup-function
               ontology)))))

(defvar *standard-other-task-slots*
  '(has-precondition has-goal-expression))

(defvar *standard-other-psm-slots*
  '(has-precondition))

(defvar *standard-psm-own-slots*
  '(applicability-condition tackles-task-type))

(defun describe-task-internals (task-name class ontology stream
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
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "~(~{~a (~a)~}~{~{; ~a (~a)~}~}~)" 
                         (car associated-psms-info)
                         (cdr associated-psms-info))
                lookup-function ontology))
        (format stream "<br>")))
    (when input-roles
      (format stream "~A" (http::bold "Input Roles:"))
      (loop for slot in input-roles
            do
            (describe-slot slot class ontology stream
                           lookup-function))
      (format stream "<br>"))
    (when output-role
      (format stream "~A" (http::bold "Output Role:"))
      (describe-slot output-role class ontology stream
                     lookup-function)
      (format stream "<br>"))
    (when other-slots
      (format stream "~A" (http::bold "Other Slots:"))
      (loop for slot in other-slots
            do
            (describe-slot slot class ontology stream
                           lookup-function)))))
    

(defun describe-psm-internals (psm-name class ontology stream 
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
         #|
         ;;;this is extremely slow 
         ;;(setofall '?x 
         ;;`(has-input-role ,psm-name ?x)))
         ;;so do it by hand
         (remove-duplicates
          (append (all-class-slot-values 
                   psm-name 'has-input-role)
                  (mapcan 
                   #'(lambda (task) 
                       (copy-list 
                        (setofall '?x 
                                  `(has-input-role ,task ?x))))
                   associated-tasks)))
|#
         ;;now use faster one
         ;;;(web-onto::findany '?x `(all-psm-input-roles ,psm-name ?x)))
         (output-role
          (car (all-class-slot-values 
                psm-name 'has-output-role)))
         (output-role-class class)
          
         ;;make this one faster too
         ;;(web-onto::findany '?x 
         ;;                 `(has-output-role ,psm-name ?x)))
         #|
          (or (car (all-class-slot-values 
                     psm-name 'has-output-role))
               (do* ((tasks associated-tasks (cdr tasks))
                    (output-role (web-onto::findany '?x
                                                    `(has-output-role ,(car tasks) ?x))
                                 (web-onto::findany '?x
                                                    `(has-output-role ,(car tasks) ?x))))
                   ((or (null tasks) output-role)
                    output-role))))
        |#
         ;; (car (web-onto::findany '?x
         ;;                       `(all-psm-output-roles ,psm-name ?x))))
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
              (describe-slot slot class ontology stream
                             lookup-function)))
      (mapc #'(lambda (associated-task-input-roles)
                (let ((task-structure (get-domain-class (car associated-task-input-roles))))
                  (loop for slot in (cdr associated-task-input-roles)
                        do
                        (unless (find slot described-input-roles)
                          (push slot described-input-roles)
                          (describe-slot slot task-structure ontology stream
                                         lookup-function)))))
            associated-tasks-input-roles)
      (format stream "<br>"))
    (when output-role
      (format stream "~A" (http::bold "Output Role:"))
      (describe-slot output-role output-role-class ontology stream
                     lookup-function)
      (format stream "<br>"))
    (when (or other-slots own-slots)
      (format stream "~A" (http::bold "Other Slots:"))
      (loop for slot in other-slots
            do
            (describe-slot slot class ontology stream
                           lookup-function))
      (loop for slot-name-and-value in own-slots
            do
            (describe-own-slot slot-name-and-value ontology stream
                               lookup-function)))))

(defun describe-class-internals (class ontology stream
                                       &optional
                                       (lookup-function
                                        #'mnm-lookup-current-word))
  (let ((domain-slots (domain-slots class)))
    (when domain-slots
      (format stream "~A" (http::bold "Slots:"))
      (loop for slot in domain-slots
            do
            (describe-slot slot class ontology stream)))
    (let ((own-slots (own-slots class)))
      (when own-slots
        (format stream "<br>~A" (http::bold "Own Slots:"))
        (loop for slot-name-and-value in own-slots
              do
              (describe-own-slot slot-name-and-value ontology stream))))
    (let ((instances 
           (mapcar 
            #'(lambda (x)
                (add-html-instance-colour
                 (name x)))
            (get-limited-current-instances class))))
      (when instances
        (format stream "<br>~A" (http::bold "All Instances:"))
        (format stream 
                (http::internal-insert-ocml-links
                 (format nil "~(~{~a ~}~)"
                         instances)
                 lookup-function
                 ontology))))))



(defun mnm-lookup-current-word (current-word current-word-length ontology
                                              bold-home-ontology-p link-url)
  (declare (ignore link-url bold-home-ontology-p))
  (let* ((current-word-string (coerce (subseq current-word 0 current-word-length)
                                      'string))
         (reverse-current-word-string (reverse current-word-string))
         (word-to-lookup (intern (string-upcase current-word-string)
                                 (find-package "OCML"))))
    (multiple-value-bind (found-p home-ontology-p colour)
        (ocml-lookup word-to-lookup ontology)
      (declare (ignore home-ontology-p))
      (if found-p
          (reverse (http::font current-word-string :color colour))
          reverse-current-word-string))))

  

(defun format-ocml-value (x number-of-spaces &optional (newline-p t))
  (if (listp x)
      (format nil (format nil "<code><br>~~~dt~~:(~~:w~~)</code>" 
                          number-of-spaces)
              x) 
    (if (stringp x)        
        (format nil "~:[~;<br>~]~a ~:(~s~)" 
                newline-p (insert-spaces number-of-spaces) x)
      (format nil "~:[~;<br>~]~a ~:(~a~)" 
              newline-p (insert-spaces number-of-spaces) x))))
    
              

(defun html-describe-instance (stream name &key class-name deduce-all-values?)
   "If deduce-all-values? is T then we use all inference methods
    at our disposal to find slot values"
     (Let ((inst (find-current-instance name class-name))
           (unbound-slots)
            (parent))
       (when inst
         (setf parent (parent-class inst))
         (format stream "<h2>Instance ~:(~a~) of class ~:(~a~)</h2>"
                 name (name parent))
         (loop for slot in (domain-slots parent)
               for values = (if deduce-all-values?
                              (setofall '?x `(,slot ,name ?x))
                              (get-slot-values inst slot))
               ;;;;;when values
               do
               (cond (values
                      (format stream "<b>~:(~a~):</b>" 
                              (add-html-relation-colour slot))
                      (format stream
                        (http::internal-insert-ocml-links
                      (format nil ;;"<pre>~% ~:(~:w~)</pre>~{<pre>~% ~:(~:w~)</pre>~}<br>"
                              "~{~a~}<br>" (mapcar #'(lambda (x) (format-ocml-value x 1))
                                               values))
                      #'mnm-lookup-current-word
                      (name *current-ontology*))))
                     (t (push slot unbound-slots)))
               finally
               (when unbound-slots
                 (format stream
                         (http::internal-insert-ocml-links
                          (format 
                           nil 
                           "The following slots have no value: ~:(~a~)~{, ~:(~a~)~}~%"
                           (car unbound-slots)
                           (cdr unbound-slots))
                          #'mnm-lookup-current-word
                          (name *current-ontology*)))))
         (values))))


(defun html-describe-relation (stream name)
  (Let* ((rel (get-relation name)))
    (if rel
        (html-describe-relation-internal stream name rel)
      (format stream "~:(~a~) is not a relation." name))))

(defun html-describe-relation-internal (stream name rel)
  (format stream "<h2>Relation ~:(~a~)</h2>" name)
  (format stream "<b>Schema:</b> ~:(~a~)" (schema rel))
  (format stream "<br><b>Ontology:</b> ~:(~a~)" 
          (add-html-ontology-colour (name (home-ontology rel))))
  (when (ocml-documentation rel)
    (format stream "<br><b>Documentation:</b> <i>~:(~a~)</i>" 
            (http::internal-insert-ocml-links
             (ocml-documentation rel)
             #'mnm-lookup-current-word
             (name *current-ontology*))))

  (when (local-slot-of rel)
    (format stream "<br><b>Local slot of:</b> ")
    (format stream 
            (http::internal-insert-ocml-links
             (format nil 
                     "~:(~a~)~{, ~:(~a~)~}"
                     (name (car (local-slot-of rel)))
                     (mapcar #'name  (cdr (local-slot-of rel))))
             #'mnm-lookup-current-word
             (name *current-ontology*))))
  (when (slot-of rel)
    (Let ((non-local-slots (set-difference (slot-of rel) (local-slot-of rel))))
      (when non-local-slots
        (format stream "<br><b>Also slot of:</b> ")
        (format stream
                (http::internal-insert-ocml-links
                 (format nil
                         "~:(~a~)~{, ~:(~a~)~}"
                         (name (car non-local-slots))
                         (mapcar #'name (cdr non-local-slots)))
                 #'mnm-lookup-current-word
                 (name *current-ontology*))))))
  (when (constraint rel)
    (format stream "<br><b>Constraints:</b> ")
    (format stream 
            (http::internal-insert-ocml-links
             (format nil "~:(~a~)" (constraint rel))
             #'mnm-lookup-current-word
             (name *current-ontology*))))
  (when (iff-def rel)
    (format stream "<br><b>Iff-def:</b> ")
    (http::internal-insert-ocml-links
     (format nil
             "~:(~a~)"
             (car (bc-clause-antecedents (iff-def rel))))
     #'mnm-lookup-current-word
     (name *current-ontology*)))
  (when (sufficient rel)
    (format stream "<br><b>Sufficient:</b> ")
    (format stream
            (http::internal-insert-ocml-links
             (format nil "~(~a~)"
                     (car (bc-clause-antecedents (sufficient rel))))
             #'mnm-lookup-current-word
             (name *current-ontology*))))
  (when (defined-by-rule rel)
    (format stream "<br><b>Associated rules:</b> ")
    (format stream
            (http::internal-insert-ocml-links
             (format nil
                     "~:(~a~)~{, ~:(~a~)~}"
                     (name (car (defined-by-rule rel)))
                     (mapcar #'name
                             (cdr (defined-by-rule rel))))
             #'mnm-lookup-current-word
             (name *current-ontology*))))
  (when (prove-by rel)
    (format stream "<br><b>Prove by:</b> ")
    (format stream 
            (http::internal-insert-ocml-links
             (format nil
                     "~:(~a~)~%"  
                     (car (bc-clause-antecedents
                           (prove-by rel))))
             #'mnm-lookup-current-word
             (name *current-ontology*))))
  (when (lisp-fun rel)
    (format stream "<br><b>Prove by:</b> ")
    (format stream
            (http::internal-insert-ocml-links
             (format nil
                     "~:(~a~)" (lisp-fun rel))
             #'mnm-lookup-current-word
             (name *current-ontology*))))
  (when (relation-instances rel)
    (format stream "<br><b>Relation Instances:</b> ")
    (format stream 
            (http::internal-insert-ocml-links
             (format nil
                     "~:(~a~)~{, ~:(~a~)~}"
                     (args (car (relation-instances rel)))
                     (mapcar #'args (cdr (relation-instances rel))))
             #'mnm-lookup-current-word
             (name *current-ontology*)))))



(defun html-describe-function (stream name)
  (Let* ((f (get-function name)))
    (if f
        (html-describe-function-internal stream name f)
      (format stream "~:(~a~) is not a function." name))))

(defun html-describe-function-internal (stream name f)
     (format stream "<h2>Function ~:(~a~)</h2>"  name (schema f))
     (format stream "<b>Schema</b>: ~:(~a~)" (schema f))
       (format stream "<br><b>Ontology:</b> ~:(~a~)" 
               (add-html-ontology-colour (name (home-ontology f))))
     (when (ocml-documentation f)
       (format stream "<br><b>Documentation:</b> <i>~a</i>" 
            (http::internal-insert-ocml-links
             (ocml-documentation f)
             #'mnm-lookup-current-word
             (name *current-ontology*))))
     (when (constraint f)
       (format stream "<br><b>Constraints:</b> ")
       (format stream
               (http::internal-insert-ocml-links
                (format nil "~:(~a~)~%" (constraint f))
                #'mnm-lookup-current-word
                (name *current-ontology*))))
     (when (definition f)
       (format stream "<br><b>Def:</b> ")
       (format stream
               (http::internal-insert-ocml-links
                (format nil "~:(~a~)" 
                        (definition f))
                #'mnm-lookup-current-word
                (name *current-ontology*))))
     (when (body f)
       (format stream "<br><b>Body:</b> ")
       (format stream 
               (http::internal-insert-ocml-links
                (format nil "~:(~a~)~%"
                        (body f))
                #'mnm-lookup-current-word
                (name *current-ontology*))))
     (when (lisp-fun f)
       (format stream "<br><b>Lisp attachment:</b> ")
       (http::internal-insert-ocml-links 
        (format nil "~:(~a~)~%" (lisp-fun f))
        #'mnm-lookup-current-word
        (name *current-ontology*))))

(defun slot-value-not-zero-length-p (structure slot-name)
  (let ((value (slot-value structure slot-name)))
    (and value (typep value 'sequence)
         (not (zerop (length value))))))

(defun html-describe-ontology (stream name)
  "If deduce-all-values? is T then we use all inference methods
    at our disposal to find slot values"
  (Let ((ontology (get-ontology name)))
    (when ontology 
      (when (slot-value ontology 'NAME) 
        (format stream 
                (http::internal-insert-ocml-links
                 (format nil 
                         "<h2>Ontology ~:(~a~)</h2>" 
                         (slot-value ontology 'NAME))
                 #'mnm-lookup-current-word
                 (name *current-ontology*))))
      (when (slot-value ontology 'DOCUMENTATION)
        (format stream "<b>Documentation:</b> <i>~:(~a~)</i>" 
                (http::internal-insert-ocml-links
                 (ocml-documentation ontology)
                 #'mnm-lookup-current-word
                 (name *current-ontology*))))
      (when (slot-value ontology 'includes) 
        (format stream "<br><b>Includes:</b> ")
        (format stream 
                (http::internal-insert-ocml-links
                 (format nil "~{~:(~a ~)~}~%" 
                         (mapcar #'name (slot-value ontology 'includes)))
                 #'mnm-lookup-current-word
                 (name *current-ontology*))))
      (when (slot-value ontology 'INCLUDED-BY)
        (format stream "<br><b>Included By:</b> ")
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "~{~:(~a ~)~}~%" 
                         (mapcar #'name (slot-value ontology 'INCLUDED-BY)))
                 #'mnm-lookup-current-word
                 (name *current-ontology*))))
      (when (slot-value ontology 'ONTOLOGY-TYPE)
        (format stream "<br><b>Ontology Type:</b> ")
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "~:(~a~)" (slot-value ontology 'ONTOLOGY-TYPE))
                 #'mnm-lookup-current-word
                 (name *current-ontology*))))
      (when (slot-value-not-zero-length-p ontology 'AUTHOR)
        (format stream "<br><b>Author:</b> ")
        (format stream 
                (http::internal-insert-ocml-links
                 (format nil "~:(~a~)"
                         (read-from-string (slot-value ontology 'AUTHOR)))
                 #'mnm-lookup-current-word
                 (name *current-ontology*))))
      (when (slot-value ontology 'ALLOWED-EDITORS)
        (format stream "<br><b>Allowed Editors:</b> ")
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "~{~:(~a ~)~}" 
                         (mapcan #'(lambda (x)
                                     (unless (zerop (length x))
                                       (list (read-from-string x))))
                                 (slot-value ontology 'ALLOWED-EDITORS)))
                 #'mnm-lookup-current-word
                 (name *current-ontology*))))
         ;(format stream "~%PATHNAME: ~:(~a~)~%" (slot-value ontology 'PATHNAME))
      (when (slot-value-not-zero-length-p ontology 'ONTOLOGY-FILES)
        (format stream "<br><b>Ontology Files:</b> ")
        (format stream 
                (http::internal-insert-ocml-links
                 (format nil "~{~:(~a ~)~}" 
                         (mapcan #'(lambda (x)
                                     (unless (zerop (length x))
                                       (list (read-from-string x))))
                                 (slot-value ontology 'ONTOLOGY-FILES)))
                 #'mnm-lookup-current-word
                 (name *current-ontology*)))))))