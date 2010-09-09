(in-package ocml)

(defvar *aktive-portal-kb* 'ocml::akt-planet-stories-kb)

(defmethod http::http-reply ((method (eql :mnm)) request)
  (with-ocml-thread-safety ()
    (handler-case
        (let ((upcase-request (string-upcase request))
              (*package* (find-package "OCML"))
              action)
          (with-input-from-string (istream upcase-request)
            (setf action (read istream))
            (handle-mnm-request 
             action http::*http-stream* 
             (string-trim '(#\space #\tab)
                          (subseq request 
                                  (length (symbol-name action)))))))
      (no-role-value-error
       (c)
       (http::send-error-message 
        http::*http-stream* 
        c))
      (condition 
       (c)
       (http::send-error-message http::*http-stream* 
                                 c))
      (error 
       (c)
       (http::send-error-message http::*http-stream* 
                                 c)))))


(defmethod handle-mnm-request (action html-stream request)
  (format html-stream "Unknown action ~a~%" action))

(defun create-ocml-symbol (string)
  (intern (substitute #\- #\space (string-trim '(#\space #\tab) string))
          (find-package "OCML")))

(defmethod handle-mnm-request ((action (eql 'html_info))
                                         html-stream request)
  (let ((upcase-request (string-upcase request))
        item-name item-type ontology-name)
    (with-input-from-string (istream upcase-request)
      (setf ontology-name (read istream)
            item-name (read istream)
            item-type (read istream))
      (cond ((get-ontology ontology-name)
             (select-ontology ontology-name)
             (html-describe-item html-stream item-name item-type))
            (t (format html-stream "unknown ontology ~a~%"
                       ontology-name))))))





