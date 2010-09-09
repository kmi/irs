(in-package #:ocml)

(eval-when (:compile-toplevel :load-toplevel)
  (export '(read-ocml-from-string)))

(defun read-ocml-from-string (string)
  "Read from string, in the OCML package."
  (let ((*package* (find-package :ocml)))
    (read-from-string string)))

;;;define an instance with no name
(defmacro def-anonymous-instance (parent &optional documentation
                                         slots)
  `(internal-define-anonymous-instance ',parent ',documentation ',slots))

(defun internal-define-anonymous-instance (parent documentation slots)
  (let ((name (intern (symbol-name (gensym "INSTANCE-"))
                       (find-package "OCML"))))
     (define-domain-instance name parent documentation slots)))

#+:lispworks 
(editor::setup-indent 'def-anonymous-instance 0 2)

(defun get-included-ontologies (ontoname)
  "Returns the ocml-internal ontology representation. Quite self-explanatory: I need it for updating the class tree accordingly... now it also works recursively for nested ontologies"
  (let* ((ontology (ocml::get-ontology ontoname))
         (result '()))
    (dolist (incl-onto (ocml::slot-value ontology 'ocml::includes))
      (setf result (append result
                           (append  (list incl-onto)
                                    (get-included-ontologies (ocml::name incl-onto ))))))
    result))

(defun describe-ontology (name &optional (stream *standard-output*))
  (let ((ontology (get-ontology name)))
    (if ontology
        (describe-object ontology stream)
        (error "~A does not name any ontology." name))))

(defmethod describe-object ((ontology ocml-ontology) stream)
  (format stream "Name:~15,0:T~A~%" (slot-value ontology 'NAME))
  (format stream "Namespace URI:~15,0T~A~%" (namespace-uri-of ontology))
  (format stream "Includes:~15,0T~A~%"
          (mapcar #'name (slot-value ontology 'includes)))
  (format stream "Included by:~15,0T~A~%"
          (mapcar #'name (slot-value ontology 'INCLUDED-BY)))
  (format stream "Type:~15,0T~A~%"
          (slot-value ontology 'ONTOLOGY-TYPE))
  (format stream "Author:~15,0T~A~%"
          (slot-value ontology 'AUTHOR))
  (format stream "Editors:~15,0T~A~%"
          (slot-value ontology 'ALLOWED-EDITORS))
  (format stream "Pathname:~15,0T~A~%"
          (slot-value ontology 'PATHNAME))
  (format stream "Files:~15,0T~A~%"
          (slot-value ontology 'ONTOLOGY-FILES))
  (format stream "Documentation:~15,0T~A~%"
          (ocml-documentation ontology)))
