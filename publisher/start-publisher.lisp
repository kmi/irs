(in-package cl-user)

(defun load-ocml-library ()
  (ocml:initialize-ocml)
  (webonto:initialise))

(defvar *proxy-host* nil)

(defvar *proxy-port* nil)

(defun start-publisher (&key host
			(irs-host-ip-address "localhost")
			(proxy-host irs:*proxy-host*)
			(proxy-port irs:*proxy-port*))
  (setf irs:*host* host
        http::*host* host)
  (setf http::*LispWeb-conf-dir* (translate-logical-pathname "irs:config;")
        web-onto::*users-file*
        (merge-pathnames (translate-logical-pathname "irs:config;")
                         "registered-users")
        web-onto::*ontology-names-file*
        (merge-pathnames (translate-logical-pathname "irs:config;")
                         "ontology-names")
        web-onto::*old-source-directory*
        (translate-logical-pathname "irs:old-ocml-source;"))
  (http::start-tcp-services '(http::*http-port* 3001))
  (setf web-onto::*default-port* 3001
        *irs-host* irs-host-ip-address
        *proxy-host* proxy-host *proxy-port* proxy-port)
  (load-ocml-library))
