(in-package http)

(defun read-string-from-browser (string)
  (unless (zerop (length string))
    (read-from-string (clean-up-browser-spaces (string-upcase string)))))

;;modif by vladt 31/01/07, was failing on x=%220.21%22
(defun clean-up-browser-spaces (string)
  (let ((new-chars nil) (last-char nil) (last-last-char nil))
    (dolist (x (coerce string 'list))
      ;;(print (list x last-char last-last-char))
      (cond ((and (char= x #\0) 
                  last-char last-last-char 
                  (char= last-char #\2)    
                  (char= last-last-char #\%))
             (setf new-chars (cons #\space (cddr new-chars)) 
                   last-last-char last-char 
                   last-char x))
            ((and (char= x #\2) 
                  last-char last-last-char
                  (char= last-char #\2)
                  (char= last-last-char #\%))
             (setf new-chars (cons #\" (cddr new-chars)) 
                   last-last-char last-char
                   last-char x))
            (t (push x new-chars)
               (setf last-last-char last-char
                     last-char x))))
    (coerce (reverse new-chars) 'string)))
                  
(defun make-ocml-symbol (string)
  (intern (string-upcase string) (find-package "OCML")))
    
(defun get-input-variable-value (form variable-end)
  (cond ((find #\space form :start variable-end)
         (make-ocml-symbol
          (subseq form variable-end
                  (position #\space form :start variable-end))))
        ((find-variable-start form variable-end)
         (make-ocml-symbol
          (subseq form variable-end
                  (position-variable-start form variable-end))))
        (t (make-ocml-symbol
            (subseq form variable-end)))))

(defun get-value-pairs (form names-to-ignore &optional (start 0))
  (when (find-variable-start form start)
    (let* ((position (1+ (position-variable-start form start)))
           (variable-end (and (find #\= form :start position)
                              (position #\= form :start position)))
           (variable-name (make-ocml-symbol
                           (string-trim '(#\space)
                                        (subseq form 
                                                position variable-end)))))
      (cond ((find variable-name names-to-ignore)
             (get-value-pairs form names-to-ignore
                              variable-end))
            (t (cons (list variable-name 
                           (get-input-variable-value form
                                                     (1+ variable-end)))
                     (get-value-pairs form names-to-ignore
                                      variable-end)))))))

(defmacro with-achieve-task-page-info ((info &optional ontology task input
                                             soap-response-p)
                                             &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,task (get-decoded-form-value ,info :task))
          (,input (or (read-string-from-browser 
                       (get-decoded-form-string-value ,info :input))
                      (get-value-pairs 
                       ,info 
                       '(ocml::task ocml::input 
                                    ocml::ontology 
                                    ocml::soap-response-p))))
          (,ontology (get-decoded-form-value ,info :ontology))
          (,soap-response-p
           (get-decoded-form-value ,info :soap-response-p)))
    ,@body))


#+lispworks
(editor::setup-indent 'with-achieve-task-page-info 0 2)


(defmacro with-task-info-page-info ((info &optional ontology task psm)
                                    &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,task (get-decoded-form-value ,info :task))
          (,ontology (get-decoded-form-value ,info :ontology))
          (,psm
           (get-decoded-form-value ,info :psm)))
    ,@body))


#+lispworks
(editor::setup-indent 'with-task-info-page-info 0 2)

(http::define-page ("Achieve Task Page"
                    :func-name achieve-task
                    :class :user 
                    )
    (&rest info)
  (http-achieve-task info html-stream))


(http::define-page2 ("Achieve Task Page"
                    :func-name achieve-task2
                    :class :user 
                    )
    (&rest info)
  (http-achieve-task info html-stream))

(defun http-achieve-task (info html-stream)
  (setf info (car info)) 
  (with-achieve-task-page-info (info ontology task-type 
                                     input-role-value-pairs soap-response-p)
    (cond ((ocml::get-ontology ontology)
           (ocml::with-ocml-thread-safety
             (ocml::select-ontology ontology)
             ;;(setf l (list ontology task-type 
              ;;                       input-role-value-pairs soap-response-p))
             (ip::irs-achieve-task ontology 
                                   task-type input-role-value-pairs 
                                   html-stream soap-response-p)))
          (t (html-out "The ontology ~a does not exist." ontology)))))

(http::define-page ("Achieve Task Page"
                    :func-name task-info
                    :class :user 
                    )
    (&rest info)
  (http-task-info info html-stream))


(defun http-task-info (info html-stream)
  (setf info (car info)) 
  (with-task-info-page-info (info ontology task psm)
    (cond ((ocml::get-ontology ontology)
           (ocml::with-ocml-thread-safety
             (ocml::select-ontology ontology)
             (if psm
                 (ip::get-task-info ontology 
                                    psm
                                    html-stream)
               (destructuring-bind (input-roles 
                                    output lisp-function)
                   (ip::get-task-task-info task)
                 (declare (ignore  lisp-function))
               (html-out "input: ~(~{~{~a ~a~}~}~)<br>Output Type: ~a" 
                         input-roles 
                         output)))))
          (t (html-out "The ontology ~a does not exist." ontology)))))