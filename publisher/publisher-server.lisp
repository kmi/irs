(in-package cl-user)

(defun start-publisher-server ()
  (setf *irs-host*  "localhost")
  (http::start-tcp-services '(http:*http-port* 3001)))
