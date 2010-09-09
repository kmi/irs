(in-package http)

(defun get-achieve-goal-input-variable-value (form variable-end)
  (let ((input-string nil))
    (cond ((find #\space form :start variable-end)
           (setf input-string
                 (subseq form variable-end
                         (position #\space form :start variable-end))))
          ((find-variable-start form variable-end)
           (setf input-string
                 (subseq form variable-end
                         (position-variable-start form variable-end))))
          (t (setf input-string
                   (subseq form variable-end))))
    (let ((*package* (find-package "OCML")))
      (setf input-string
            (handler-case (clean-up-browser-spaces input-string)
              (error (c) input-string)))
      (handler-case (read-from-string input-string)
        (error (c) input-string)))))

(defun get-achieve-goal-value-pairs (form names-to-ignore &optional (start 0))
  (when (find-variable-start form start)
    (let* ((position (1+ (position-variable-start form start)))
           (variable-end (and (find #\= form :start position)
                              (position #\= form :start position)))
           (variable-name (make-ocml-symbol
                           (string-trim '(#\space)
                                        (subseq form 
                                                position variable-end)))))
      (cond ((find variable-name names-to-ignore)
             (get-achieve-goal-value-pairs form names-to-ignore
                              variable-end))
            (t (cons (list variable-name 
                           (get-achieve-goal-input-variable-value form
                                                     (1+ variable-end)))
                     (get-achieve-goal-value-pairs form names-to-ignore
                                      variable-end)))))))


(defmacro with-achieve-goal-page-info ((info &optional ontology goal input
                                             soap-response-p)
                                             &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,goal (get-decoded-form-value ,info :goal))
          (,input (get-achieve-goal-value-pairs  
                       ,info 
                       '(ocml::goal ocml::input 
                                    ocml::ontology 
                                    ocml::soap-response-p)))
          (,ontology (get-decoded-form-value ,info :ontology))
          (,soap-response-p
           (get-decoded-form-value ,info :soap-response-p)))
    ,@body))


#+lispworks
(editor::setup-indent 'with-achieve-goal-page-info 0 2)


(defmacro with-wsmo-goal-page-info ((info &optional ontology item entity object)
                                    &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,item (get-decoded-form-value ,info :item))
          (,entity (get-decoded-form-value ,info :entity))
          (,object (get-decoded-form-value ,info :object))
          (,ontology (get-decoded-form-value ,info :ontology)))
    ,@body))


#+lispworks
(editor::setup-indent 'with-wsmo-goal-page-info 0 2)

(http::define-page ("Achieve Goal Page"
                    :func-name achieve-goal
                    :class :user 
                    )
    (&rest info)
  (http-achieve-goal info html-stream))


(http::define-page2 ("Achieve Goal Page"
                    :func-name achieve-goal2
                    :class :user 
                    )
    (&rest info)
  (http-achieve-goal info html-stream))

(http::define-page3 ("Achieve Goal Page"
                    :func-name achieve-goal3
                    :class :user 
                    )
    (&rest info)
  (http-achieve-goal info html-stream))

(http::define-page ("Achieve Multiple Goal Page"
                    :func-name achieve-multiple-goals
                    :class :user 
                    )
    (&rest info)
  (http-achieve-goal info html-stream))


(http::define-page2 ("Achieve Multiple Goal Page"
                    :func-name achieve-multiple-goals2
                    :class :user 
                    )
    (&rest info)
  (http-achieve-goal info html-stream))

(http::define-page3 ("Achieve Multiple Goal Page"
                    :func-name achieve-multiple-goals3
                    :class :user 
                    )
    (&rest info)
  (http-achieve-goal info html-stream))

(defun process-info-string (info-string)
  (let ((result ""))
    (dolist (x info-string)
      (setf result
            (concatenate 'string result "/" x)))
    result))


(defun http-achieve-goal (info html-stream)
  
  (setf info (process-info-string info)) 
  ;;(setf ii info)
  (with-achieve-goal-page-info (info ontology goal-type 
                                     input-role-value-pairs soap-response-p)
    ;;(setf oo ontology gg goal-type ss soap-response-p  aa input-role-value-pairs)
    (cond ((and (ocml::get-ontology ontology)
                goal-type)
           (ocml::with-ocml-thread-safety
             (ocml::select-ontology ontology)
             (let ((number-of-input-roles (length (wp::input-roles goal-type))))
               ;; (setf l (list ontology task-type 
               ;;                        input-role-value-pairs soap-response-p))
               (dotimes (i (/ (length input-role-value-pairs) number-of-input-roles))
                 (ip::irs-achieve-goal ontology 
                                       goal-type (subseq input-role-value-pairs (* i number-of-input-roles)
                                                         (* (1+ i) number-of-input-roles))
                                       html-stream soap-response-p t)))))
          ((ocml::get-ontology ontology)
           (html-out "You need to specify a goal."))
          (t (html-out "The ontology ~a does not exist." ontology)))))

(http::define-page ("WSMO Goal Info Page"
                    :func-name wsmo-info
                    :class :user 
                    )
    (&rest info)
  (http-wsmo-entity-info info html-stream))

(defvar *irs-lookup-link-url* 
  "/wsmo-info?item=~a&ontology=~a")

(defun irs-lookup-current-word-insert-link-for-browser
       (current-word current-word-length ontology
                     bold-home-ontology-p link-url)
  (declare (ignore link-url bold-home-ontology-p))
  (let* ((current-word-string (coerce (subseq current-word 0 current-word-length)
                                      'string))
         (reverse-current-word-string (reverse current-word-string))
         (word-to-lookup (intern (string-upcase current-word-string)
                                 (find-package "OCML"))))
    (multiple-value-bind (found-p home-ontology-p colour)
        (ocml::ocml-lookup word-to-lookup ontology)
      (declare (ignore home-ontology-p))
      (if found-p
          (reverse 
           (http::anchor 
            (format nil *irs-lookup-link-url* 
                         word-to-lookup ontology)
            (http::font current-word-string 
                        :color colour)))
          reverse-current-word-string))))

(defun http-display-wsmo-entity (ontology wsmo-entity html-stream lookup-function)
  ;;;(setf oo ontology ww wsmo-entity pp lookup-function)
  (ocml::select-ontology ontology)
  (let ((string (with-output-to-string (html-stream)
                  (format html-stream ip::*html-file-header*)
                  (ocml::html-describe-unknown-item html-stream wsmo-entity lookup-function)
                  (format html-stream ip::*html-file-footer*))))
    (format html-stream "~a~%"
            (remove #\linefeed (remove #\return string)))))

(defun http-wsmo-entity-info (info html-stream)
  (setf info (car info)) 
  (with-wsmo-goal-page-info (info ontology item entity object)
    (cond ((ocml::get-ontology ontology)
           (if (or item entity object)
           (ocml::with-ocml-thread-safety
             (ocml::select-ontology ontology)
             (http-display-wsmo-entity ontology (or item entity object) html-stream
                                       #'irs-lookup-current-word-insert-link-for-browser))
             (html-out "You need to specify the thing to be described using one of: item, entity or object.")))
          (t (html-out "The ontology ~a does not exist." ontology)))))



;;------------------ Trusted based invocation: functions added by Stefania ------------




;;added-by stafania 07/09/2006
;;new function to invoke ip::trusted-irs-achieve-goal 



(defmacro with-trusted-achieve-goal-page-info ((info &optional trust-user-instance ontology goal input
                                             soap-response-p)
                                             &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,goal (get-decoded-form-value ,info :goal))
          (,input (get-achieve-goal-value-pairs
                       ,info 
                       '(ocml::trust-user-instance ocml::goal ocml::input 
                                    ocml::ontology 
                                    ocml::soap-response-p)))
          (,ontology (get-decoded-form-value ,info :ontology))
          (,trust-user-instance (get-decoded-form-value ,info :trust-user-instance))
          (,soap-response-p
           (get-decoded-form-value ,info :soap-response-p)))
    ,@body))

#+lispworks
(editor::setup-indent 'with-trusted-achieve-goal-page-info 0 2)


(http::define-page ("Achieve Goal Page using trust-based selection"
                    :func-name trusted-achieve-goal
                    :class :user 
                    )
    (&rest info)
  (http-trusted-achieve-goal info html-stream))



(defun http-trusted-achieve-goal (info html-stream)
  (setf info (process-info-string info)) 
  ;;(setf ii info)
  (with-trusted-achieve-goal-page-info (info trust-user-instance ontology goal-type 
                                     input-role-value-pairs soap-response-p)
    ;;(setf oo ontology gg goal-type ss soap-response-p  aa input-role-value-pairs)
    (cond ((and (ocml::get-ontology ontology)
                goal-type)
           (ocml::with-ocml-thread-safety
             (ocml::select-ontology ontology)
            ;; (setf l (list ontology task-type 
             ;;                        input-role-value-pairs soap-response-p))
            ;; (setf aaa input-role-value-pairs)
             (ip::trusted-irs-achieve-goal trust-user-instance
                                           ontology 
                                           goal-type input-role-value-pairs 
                                           html-stream soap-response-p t)))
          ((ocml::get-ontology ontology)
           (html-out "You need to specify a goal."))
          (t (html-out "The user: ~a  goal: ~a input: ~a ontology ~a html-stream: ~a soap-response: ~a does not exist." trust-user-instance goal-type input-role-value-pairs ontology html-stream soap-response-p)))))

(defun create-symbol (string &optional (package-name "OCML"))
  (intern (string-upcase string) (find-package package-name)))

(http::define-page ("IRS Definitions"
                    :func-name irs4
                    :class :user 
                    )
    (ontology  type item-name)
  (setf ontology (create-symbol ontology)
        type (create-symbol type "KEYWORD")
        item-name (create-symbol item-name))
  (cond ((ocml::get-ontology ontology)
         (let ((source-string 
                (web-onto::get-source-string 
                 (web-onto::dspec-from-name-type-ontology item-name type ontology))))
           (html-out source-string)))
        (t (html-out "The ontology ~a does not exist" ontology))))


(http::define-page ("IRS Definitions"
                    :func-name irs3
                    :class :user 
                    )
    (ontology type item-name)
  (setf ontology (create-symbol ontology)
        type (create-symbol type "KEYWORD")
        item-name (create-symbol item-name))
  (let ((*package* (find-package "OCML")))
    (cond ((ocml::get-ontology ontology)
           (generate-ocml-definition-html item-name ontology))
          (t (html-out "The ontology ~a does not exist" ontology)))))

(http::define-page ("IRS Definitions"
                    :func-name irs
                    :class :user 
                    )
    (ontology)
  (setf ontology (create-symbol ontology))
  (let ((*package* (find-package "OCML")))
    (cond ((ocml::get-ontology ontology)
           (wp::generate-ontology-page ontology))
          (t (html-out "The ontology ~a does not exist" ontology)))))

(http::define-page ("IRS Definitions"
                    :func-name irs2
                    :class :user 
                    )
    (ontology)
  (setf ontology (create-symbol ontology))
  (let ((*package* (find-package "OCML")))
    (cond ((ocml::get-ontology ontology)
           (wp::generate-ontology-page ontology nil))
          (t (html-out "The ontology ~a does not exist" ontology)))))
