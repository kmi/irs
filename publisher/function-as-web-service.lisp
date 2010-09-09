(in-package cl-user)

(defvar *url-function-mappings* (make-hash-table :test #'equal))

(defun clear-all-web-service-functions ()
  (clrhash *url-function-mappings*))

(defun get-function-as-web-service (url)
  (gethash url *url-function-mappings*))

(defun function-as-web-service (function-name 
                                        url 
                                        &optional 
                                        (result-soap-type "sexpr"))
  (setf (gethash url *url-function-mappings*)
        (list function-name result-soap-type)))

(defun handle-function-as-web-service (request action soap-values
                                        stream)
  (let (result)
    (destructuring-bind (function-name result-type)
        (get-function-as-web-service request)
      (setf result (apply function-name soap-values))
      (iu::send-soap-response 
       (string-downcase (symbol-name action))
       (list result) `((result ,result-type)) :stream stream))))


