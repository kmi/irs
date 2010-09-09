;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defun get-patterns (stream request-string)
  (with-input-from-string (istream request-string)
    ;;;the get-pattern bit
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read istream))
           (class-name (read istream))
           (pattern-type (read istream)))
      (declare (ignore pattern-type))
      (cond ((ocml::get-ontology ontology-name)
             (ocml::select-ontology ontology-name)
             (let ((class (ocml::get-domain-class class-name)))
               (cond (class
                      (let* ((patterns (ocml::class-patterns class))
                             (matching-pattern (car patterns))
                             (values-pattern (second patterns)))
                        (http::princ-to-binary-stream
                         (format nil "OK~a~s~a~s~%" *ocml-line-separator*
                                 matching-pattern
                                 *ocml-line-separator* values-pattern)
                        stream)))
                     (t (http::princ-to-binary-stream
                         (format nil "Sorry, couldn't find the class ~a~%" class-name)
                         stream)))))
            (t (http::princ-to-binary-stream
                (format nil "Sorry, couldn't find the ontology ~a~%" ontology-name)
                stream))))))



