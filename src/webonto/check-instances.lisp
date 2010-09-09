;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defun check-instances (stream upcase-string)
  (with-input-from-string (info-stream upcase-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (check-inherited-instances-p (read info-stream))
           (ocml::*current-ontology* ocml::*current-ontology*)
           (ocml::*current-ontologies* ocml::*current-ontologies*)
           (ocml::*defined-relations* ocml::*defined-relations*)
           (ocml::*axioms* ocml::*axioms*)
           (ocml::*defined-functions* ocml::*defined-functions*)
           (ocml::*bc-rules* ocml::*bc-rules*)
           (ocml::*domain-classes* ocml::*domain-classes*)
           (result nil))
      (declare (special ocml::*current-ontology* ocml::*current-ontologies*
                        ocml::*defined-relations* ocml::*axioms*
                        ocml::*defined-functions* ocml::*bc-rules*))
      (when (ocml::get-ontology ontology-name)
        (ocml::select-ontology ontology-name)
        (maphash
         #'(lambda (key value)
             (declare (ignore value))
             (mapc #'(lambda (instance)
                       (when (if check-inherited-instances-p
                                 (find (ocml::home-ontology instance)
                                       ocml::*current-ontologies* :test #'eq)
                                 (eq (ocml::home-ontology instance)
                                     ocml::*current-ontology*))
                         (setf result
                               (append result (check-instance instance)))))
                   (ocml::all-current-direct-instances key)))
         ocml::*domain-classes*)
        (http::princ-to-binary-stream
         (if result
             (format nil
                     (format nil "The result of checking ~~:(~~a~~) was: ~~{~~{~aIn the instance ~~(~~a~~) ~A  the slot ~~(~~a~~) has the unknown value~~p ~~{~~(~~a~~) ~~}~~}~~}~~%" *ocml-line-separator* *ocml-line-separator*)
                     ontology-name result)
             (format nil  "No inconsistencies were found in the ~a~%" ontology-name))
         stream)))))

(defun check-instance (structure)
  (let ((name (ocml::name structure)))
    (mapcan #'(lambda (slot)
                (let* ((values (ocml::get-slot-values structure slot))
                       (unknown-values (collect-unknown-values values)))
                  (when unknown-values
                    (list (list name slot (length unknown-values) unknown-values)))))
            (ocml::ocml-most-specific-class-slots (class-of structure)))))


(defun collect-unknown-values (values)
  (mapcan #'(lambda (value)
              (unless (or (numberp value)
                          (stringp value)
                          (ocml::get-ocml-class value)
                          (ocml::find-current-instance value)
                          (ocml::find-bc-rule value)
                          (ocml::get-ocml-relation value)
                          (ocml::get-ocml-function value)
                          (ocml::get-ontology value))
                (list value)))
          values))


