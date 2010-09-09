(in-package cl-user)

(defmacro irs-wsmo-web-service-registration (ontology web-service-name
                                            &optional 
                                            (publisher-location "/soap"))
  `(internal-irs-wsmo-web-service-registration ',ontology ',web-service-name 
                                               ,publisher-location))


(defun internal-irs-wsmo-web-service-registration (ontology web-service-name 
                                                            &optional 
                                                            (publisher-location "/soap"))
  (setf (gethash (List ontology web-service-name)
                 iu::*methods-hash-table*) publisher-location))

(defun send-wsmo-soap-interface-to-irs-server (user ontology web-service-name
                                                    publisher-location)
  (when *irs-host*
    (irssoap-http-client 'publish-wsmo-lisp-function
                         '((user "string")
                           (ontology "sexpr")(metid "sexpr")
                           (host "string")
                           (port "int") (publisher-location "string"))
                         (list user ontology web-service-name
                               (tcp::full-hostname)
                               http::*http-port* publisher-location)
                         :host *irs-host*
                         :port *irs-port*)))


(defun publish-all-wsmo-services (user)
  (maphash #'(lambda (key value)
               (declare (ignore value))
               (apply #'publish-wsmo-service (append key (list user))))
           iu::*methods-hash-table*))

(defun publish-wsmo-service (ontology method-name user)
  (let ((publisher-location 
         (iu::get-methods-definition ontology method-name)))
    (send-wsmo-soap-interface-to-irs-server 
     user ontology method-name publisher-location)))