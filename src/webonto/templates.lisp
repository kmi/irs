;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defmethod send-back-source (source-string (type (eql 'ocml::def-new-instance-template))
                                           name stream)
  (if source-string
      (http::princ-to-binary-stream
       (format nil "~a~%"
               (ocml::convert-multiple-line-string source-string))
       stream)
      (http::princ-to-binary-stream
       (format nil "(def-new-instance-template ~(~a~) (<vars>)~a  (def-instance $<var> $class-name))~%"
               name *ocml-line-separator*)
       stream)))

(defun defined-template-p (stream request-string)
  (with-input-from-string (istream request-string)
    ;;;the defined-template-p bit
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read istream))
           (defining-term (read istream))
           (defining-name (read istream))) 
      (cond ((ocml::get-ontology ontology-name)
             (ocml::select-ontology ontology-name)
             (let ((source (get-source-string
                            (list defining-term defining-name ontology-name))))
               (if source
                   (http::princ-to-binary-stream
                    (format nil "true~%")
                    stream)
                   (http::princ-to-binary-stream
                    (format nil "false~%")
                    stream))))                   
            (t (http::princ-to-binary-stream
                (format nil "Sorry, couldn't find the ontology ~a~%" ontology-name)))))))





