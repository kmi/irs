;;; Mode: Lisp; Package: http

;;; Author: John Domingue

;;; The Open University

(in-package "HTTP")

(defvar *unilever-tables* nil)

(defvar *saved-unilever-tables* nil)

(defvar *published-unilever-tables* nil)

(defvar *tables-file* nil)

(defun read-into-list (string)
  (let ((*package* (find-package "OCML")))
    (read-from-string (concatenate 'string "(" string ")"))))

(defun create-web-table (binary-stream request-string)
  (with-input-from-string (stream request-string)
    ;;;the action bit
    (read stream)
    (let* ((*package* (find-package "OCML"))
           (name (read stream))
           (symbols (read-into-list (read stream)))
           (rows (read-into-list (read stream)))
           (columns (read-into-list (read stream))))
      (internal-create-web-table name symbols rows columns)
      (princ-to-binary-stream
       (format
        nil
        "The table with rows ~(~{~a ~}~) and columns ~(~{~a ~}~) has been created.~%"
        rows columns)
       binary-stream))))

(defun save-tables ()
  (when *tables-file*
    (when (probe-file *tables-file*)
      (delete-file *tables-file*))
    (with-open-file (ostream *tables-file* :direction :output :if-does-not-exist :create)
      (print *unilever-tables* ostream)
      (terpri) (terpri)
      (print *saved-unilever-tables* ostream)
      (terpri) (terpri)
      (print *published-unilever-tables* ostream))))

(defun clear-tables ()
  (setf *unilever-tables* nil
        *saved-unilever-tables* nil
        *published-unilever-tables* nil)
  (when *tables-file*
    (when (probe-file *tables-file*)
      (delete-file *tables-file*))))

(defun read-in-tables ()
  (when *tables-file*
    (when (probe-file *tables-file*)
      (let ((*package* (find-package "OCML")))
        (with-open-file (istream *tables-file* :direction :input)
          (setf *unilever-tables* (read istream)
                *saved-unilever-tables* (read istream)
                *published-unilever-tables* (read istream)))))))



(defun internal-create-web-table (name symbols rows columns)
  (let ((existing-table (assoc name *unilever-tables*)))
    (when existing-table
      (setf *unilever-tables*
            (delete existing-table *unilever-tables* :test #'equal)))
    (push (list name symbols rows columns) *unilever-tables*)
    (save-tables)))

(defstruct (unilever-table (:type list))
  name symbols rows columns)

(defmacro with-unilever-tables-info ((info &optional name) &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,name (get-decoded-form-value ,info :name)))
    ,@body))


#+lispworks
(editor::setup-indent 'with-unilever-tables-info 0 2)

(defun get-table (name)
  (assoc name *unilever-tables*))

(defun get-saved-table (name)
  (assoc name *saved-unilever-tables*))

(define-page ("Unilever Tables Page" :func-name table
				     :class :user
                                     :bgcolor web-onto::*default-page-colour*
				     :base "/tadzebao/webonto.html"
				     :header-p nil
				     ) 
    (&rest info)
  (setf info (car info))
  (with-unilever-tables-info (info name)
    (let* ((table (get-table name))
           (symbols (unilever-table-symbols table))
           (rows (unilever-table-rows table))
           (columns (unilever-table-columns table)))
      (html-out (center (header 1 (format nil "~:(~a~) Table" name))))
      (html-out
       (in-form (:action (url 'save-table2))
         (html-out
          (in-table
              (:border 2)
            (html-out
             (apply #'table-row
                    (table-cell " ")
                    (mapcar #'(lambda (column)
                                (table-cell (bold (format nil "~:(~a~)" column))))
                            columns)))
            (mapc #'(lambda (row)
                      (html-out
                       (apply #'table-row
                              (table-cell (bold (format nil "~:(~a~)" row)))
                              (mapcar
                               #'(lambda (column)
                                   (table-cell
                                    (cond (symbols
                                           (menu-select (format nil "~a-~a"
                                                                row column)
                                                        symbols))
                                          (t
                                           (text-field
                                            (format nil "~a-~a"
                                                    row column)
                                            :size
                                            (length
                                             (format nil "~a" column)))))))
                               columns))))
                  rows)))
         (html-out "<p>~a</p>"
                   (submit-button (format nil "Save Table ~:(~a~)" name)
                                  :name "SAVE-TABLE-NAME")))))))

(defun get-table-name (save-table-name)
  (let ((*package* (find-package "OCML")))
    (with-input-from-string (istream (string-upcase save-table-name))
      (read istream) (read istream)
      (read istream))))

(defmacro save-table-field-values (table-name rows columns)
  `(save-unilever-table
    ,table-name
    (mapcar #'(lambda (row)
                (mapcar #'(lambda (column)
                            (form-value
                             (intern (format nil "~a-~a"
                                             row column)
                                     (find-package "KEYWORD"))))
                        ,columns))
            ,rows)))

#+lispworks
(editor::setup-indent 'save-table-field-values 0 2)

(defun save-unilever-table (table-name table-values)
  (let ((existing-table (assoc table-name *saved-unilever-tables*)))
    (when existing-table
      (setf *saved-unilever-tables*
            (delete existing-table *saved-unilever-tables* :test #'equal)))
    (push (cons table-name table-values)
          *saved-unilever-tables*)
    (save-tables)))

(define-page ("Unilever Save Table Page"
              :func-name save-table2
              :class :user
              :bgcolor web-onto::*default-page-colour*
              :base "/tadzebao/save-table.html"
              :header-p nil
              ) 
    ()
  (with-field-value (save-table-name)
    (let* ((table-name (get-table-name save-table-name))
           (table (get-table table-name))
           (rows (unilever-table-rows table))
           (columns (unilever-table-columns table)))
    (html-out (center (header 1 (format nil "~:(~a~) Table" table-name))))
    (save-table-field-values table-name rows columns)
    (html-out (format nil "Table ~:(~a~) saved. You can see the saved table ~a." 
                      table-name
                      (anchor (format nil "/table-values?name=~a" table-name)
                              "here"))))))
      
    
(define-page ("Unilever Tables Values Page"
              :func-name table-values
              :class :user
              :bgcolor web-onto::*default-page-colour*
              :base "/tadzebao/webonto.html"
              :header-p nil
              ) 
    (&rest info)
  (setf info (car info))
  (with-unilever-tables-info (info name)
    (let* ((saved-table (get-saved-table name))
           (table (get-table name))
           (rows (unilever-table-rows table))
           (columns (unilever-table-columns table))
           (table-values (cdr saved-table)))
      (html-out (center (header 1 (format nil "~:(~a~) Table" name))))
      (html-out
      (in-form (:action (url 'publish-table))
      (html-out
       (in-table
           (:border 2)
         (html-out
          (apply #'table-row
                 (table-cell " ")
                 (mapcar #'(lambda (column)
                             (table-cell (bold (format nil "~:(~a~)" column))))
                         columns)))
         (mapc #'(lambda (row saved-row)
                   (html-out
                    (apply #'table-row
                           (table-cell (bold (format nil "~:(~a~)" row)))
                           (mapcar
                            #'(lambda (saved-value)
                                (table-cell saved-value))
                            saved-row))))
               rows table-values)))
 (html-out "<p>~a</p>"
           (submit-button (format nil "Publish Table ~:(~a~)" name)
                          :name "PUBLISH-TABLE-NAME")))))))


(define-page ("Unilever Publish Tables Page"
              :func-name publish-table
              :class :user
              :bgcolor web-onto::*default-page-colour*
              :base "/tadzebao/webonto.html"
              :header-p nil
              ) 
   ()
  (with-field-value (publish-table-name)
    (let* ((name (get-table-name publish-table-name))
           (saved-table (get-saved-table name))
           (table (get-table name))
           (rows (unilever-table-rows table))
           (columns (unilever-table-columns table))
           (table-values (cdr saved-table))
           published-url)      
      (push name *published-unilever-tables*)
      (save-tables)
      (web-onto::republish-all-tables (reverse *published-unilever-tables*))
      (setf published-url (web-onto::generate-unilever-table-d3e-doc))
      (web-onto::add-ocml-table-instance name rows columns table-values published-url)
      (html-out (center (header 1 (format nil "~:(~a~) Table" name))))
      (html-out (format nil "The table ~(~a~) has been published. You can see the plain table ~a or the associated discussion page ~a " 
                        name
                        (anchor web-onto::*input-unilever-file-url*
                                "here")
                        (anchor published-url
                                "here"))))))
