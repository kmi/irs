;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defun reload-ontology  (stream request-string)
  (with-input-from-string (request-string-stream request-string)
    (read request-string-stream)
    (let* ((*package* (find-package "OCML"))
           (user-name (read request-string-stream))
           (ontology-name (read request-string-stream))
           (ontology  (get-ontology ontology-name)))           
      (cond (ontology
             (cond ((or (root-user-p user-name)
                        (ontology-owner-p ontology user-name)
                        (allowed-editor-p ontology user-name))
                    (internal-load-single-ontology ontology-name t)
                    (when stream               
                        (http::princ-to-binary-stream
                         (format nil "Successfully reloaded the ontology ~a~%"
                                 ontology-name)
                         stream)))
                   (t (http::princ-to-binary-stream
                       (format nil "Sorry, the ontology ~a is owned by ~a. ~:[~*~;The allowed editors are ~{~a ~}~]~%"
                               ontology-name
                               (ocml::ontology-author ontology)
                               (ocml::ontology-allowed-editors ontology)
                               (ocml::ontology-allowed-editors ontology))
                       stream))))
            (t (http::princ-to-binary-stream
                (format nil "Sorry, the ontology ~a does not exists~%" ontology-name)
                stream))))))
