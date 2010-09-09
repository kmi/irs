;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defvar *connected-instances-page*
  "/connected-instances-page")

(defvar *connected-instances-results* nil)

(defun get-connected-instances-result (name)
  (cdr (assoc name *connected-instances-results*)))

(http::define-page ("Connected Instances Page" :func-name connected-instances-page
                                               :class :user :bgcolor *default-page-colour*
                                               :Header-p nil
                                               )
    (&rest info)
  (when info
    (setf info (car info)))
  (with-result-name (info result-name)
    (http::html-out (http::center (http::header 1 "Connected Instances Page")))
    (http::html-out (get-connected-instances-result result-name))))

(defun display-connected-instances (stream upcase-string)
  (with-input-from-string (info-stream upcase-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (instance-name (read info-stream))
           (depth (read info-stream))
           (inline-p (java-true-p (read info-stream)))
           (ocml::*current-ontology* ocml::*current-ontology*)
           (ocml::*current-ontologies* ocml::*current-ontologies*)
           (ocml::*defined-relations* ocml::*defined-relations*)
           (ocml::*axioms* ocml::*axioms*)
           (ocml::*defined-functions* ocml::*defined-functions*)
           (ocml::*bc-rules* ocml::*bc-rules*)
           (ocml::*domain-classes* ocml::*domain-classes*))
      (declare (special ocml::*current-ontology* ocml::*current-ontologies*
                        ocml::*defined-relations* ocml::*axioms*
                        ocml::*defined-functions* ocml::*bc-rules*
                        ocml::*domain-classes*))
      (internal-display-connected-instances
       stream ontology-name instance-name depth inline-p))))

(defun internal-display-connected-instances (stream ontology-name instance-name depth
                                                    inline-p)
  (when (ocml::get-ontology ontology-name)
    (ocml::select-ontology ontology-name)
    (let ((result-name (new-result-name))
          (connected-instances
           (ocml::get-all-connected-instances instance-name depth 0 inline-p)))
      (http::princ-to-binary-stream
       (format nil "~a~%" result-name)
       stream)
      (push (create-connected-instances-page
             result-name
             (if inline-p
                 connected-instances
                 (cons (car connected-instances)
                       (sort (cdr connected-instances)
                             #'(lambda (x y)
                                 (cond ((and (numberp x) (numberp y))
                                        (< x y))
                                       ((numberp x) t)
                                       ((numberp y) nil)
                                       (t (string< x y))))
                             :key #'car)))
             ontology-name
             inline-p
             depth)
            *connected-instances-results*)
      result-name)))

(defvar *connect-instances-url*
  "/connected-instances?ocml-name=~~a?ocml-ontology=~~a?inline-p=~a?depth=~a")

(defmacro with-connected-instance-info ((info ocml-name ocml-ontology
                                              inline-p depth) &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,ocml-name (cl-user::get-decoded-form-value ,info :ocml-name))
          (,ocml-ontology (cl-user::get-decoded-form-value ,info :ocml-ontology))
          (,inline-p (cl-user::get-decoded-form-value ,info :inline-p))
          (,depth (cl-user::get-decoded-form-value ,info :depth)))
    ,@body))

#+lispworks
(editor::setup-indent 'with-connected-instance-info 0 2)

(http::define-page ("Display Connected Instances Page"
                    :func-name connected-instances
                    :class :user :bgcolor http::*planet-onto-page-colour*
                    )
    (info)
  (with-connected-instance-info (info ocml-name ontology inline-p depth)
    (let ((ocml::*current-ontology* ocml::*current-ontology*)
           (ocml::*current-ontologies* ocml::*current-ontologies*)
           (ocml::*defined-relations* ocml::*defined-relations*)
           (ocml::*axioms* ocml::*axioms*)
           (ocml::*defined-functions* ocml::*defined-functions*)
           (ocml::*bc-rules* ocml::*bc-rules*)
           (ocml::*domain-classes* ocml::*domain-classes*))
      (declare (special ocml::*current-ontology* ocml::*current-ontologies*
                        ocml::*defined-relations* ocml::*axioms*
                        ocml::*defined-functions* ocml::*bc-rules*
                        ocml::*domain-classes*))
      (http::html-out (http::center (http::header 1 "Connected Instances Page")))
      (cond ((ocml::get-ontology ontology)
             (ocml::select-ontology ontology)
             (if (ocml::find-current-instance ocml-name)
    (let ((result-name (internal-display-connected-instances
                        *standard-output* ontology ocml-name depth inline-p)))
      (http::html-out (get-connected-instances-result result-name)))
    (http::generate-ocml-definition-html ocml-name ontology)))
            (t (http::html-out "Sorry the ontology ~a is not known."))))))

(defun create-connected-instances-page (result-name connected-instances ontology
                                                    inline-p depth)
  (cons result-name
        (http::internal-insert-ocml-links
         (with-output-to-string (*standard-output*)
           (if inline-p
               (create-inline-connected-instances-page connected-instances)
               (create-not-inline-connected-instances-page connected-instances)))
         'http::ocml-lookup-current-word ontology t
         (format nil *connect-instances-url*
                 inline-p depth))))

(defun create-inline-connected-instances-page (connected-instances &optional (current-depth 0))
  (format t "<code>")
  (internal-create-inline-connected-instances-page connected-instances current-depth)
  (format t "<\code>"))

(defvar *colour-decrease-multiplier* ;;5 increase for k-cap demo
  15)

(defun connected-instance-background-colour (depth)
  (let ((number (max 0 (- 250 (* *colour-decrease-multiplier* (1- depth))))))
    #+:irs-use-lispweb
    (http::color-string :red number :blue number :green number)))

(defun insert-tab (depth)
  (format t "<td colspan=~d></td>~%" 
          (* 2 depth)))

(defun insert-table-header (&optional (border 0))
  (format t "<table border=~d cellpadding=0 cellspacing=0><tr>~%"
          border))

(defun insert-table-cell (depth)
  (format t "<td BGCOLOR=\"~a\" nowrap=true>~%"
          (connected-instance-background-colour depth)))

(defun insert-table-cell-end ()
  (format t "</td>~%"))

(defun insert-table-row-end ()
  (format t "</tr>~%"))

(defun insert-table-footer ()
  (format t "</table>~%"))

(defun internal-create-inline-connected-instances-page (connected-instances current-depth)
  (let* ((value (car connected-instances))
        (instance-object (ocml::find-current-instance value))
        (slots (cdr connected-instances)))
    (insert-table-header) ;;(if slots 1 0))
    (insert-tab current-depth) 
    (insert-table-cell current-depth)
    (format t "~:(~a~) ~:[~*~; (~:(~a~))~]~%"
                 value instance-object
                 (ocml::name
                  (ocml::parent-class instance-object)))
  (mapc
   #'(lambda (slot-info)
       (let ((slot-name (car slot-info))
             (slot-values (cdr slot-info)))
         (insert-table-header)
         (insert-tab (1+ current-depth))
         (insert-table-cell current-depth)
         (format t " ~:(~a~) " slot-name)
         (mapc #'(lambda (slot-value)
                   (cond ((listp slot-value)
                          (internal-create-inline-connected-instances-page slot-value
                                                                  (+ 2 current-depth)))
                         (t (if (stringp slot-value)
                                (format t "~s " slot-value)
                                (format t "~:(~a~) " slot-value)))))
               slot-values)
         (insert-table-cell-end)
         (insert-table-row-end)
         (insert-table-footer)))
       
   slots)
  (insert-table-cell-end)
  (insert-table-row-end)
  (insert-table-footer)))

(defun create-not-inline-connected-instances-page (connected-instances)
  (mapc
   #'(lambda (instance-description)
       (let ((instance-name (car instance-description))
             (slots (cdr instance-description)))
         (format t
                 "<H2>~:(~a~) of class ~:(~a~)</H2>~%"
                 instance-name (ocml::name
                                (ocml::parent-class
                                 (ocml::find-current-instance instance-name))))
         (format t "<Table>~%")
         (mapc
          #'(lambda (slot-info)
              (format t "<tr><td><b>~(~a~):</b></td> <td>~(~{~a ~}~)</td></tr>~%"
                      (car slot-info) (cdr slot-info)))
          slots)
         (format t "</Table>~%")))
   connected-instances))