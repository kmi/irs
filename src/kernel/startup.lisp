(in-package #:irs)

(defvar *initialised* nil)

;;; INITIALISE should hold across image-saving.  START needs to be run
;;; after every startup.

(defun initialise ()
  (unless *initialised*
    (ocml:initialize-ocml)
    ;; The Prolog format is much more readable than the OCML default.
    (setf ocml:*binding-print-format* :prolog)
    (webonto:require-ontologies '(:monitoring-ontology
				  :see-events
				  :top-level-ontology))
    (setf *initialised* t)))

(defun start (&rest variables &key (visualiser-columns nil)
              &allow-other-keys)
  "Start IRS server.

If VISUALISER-COLUMNS is an integer, start visualiser with that many
columns.  Other keywords are passed to SETUP-VARIABLES."
  (initialise)
  (apply #'setup-variables variables)
  (webonto:initialise)
  ;; Set this now, because until the OCML/WebOnto ontology machinary
  ;; is initialised, they just don't work safely.
  (set-dispatch-macro-character #\# #\l #'load-sys2)
  (set-dispatch-macro-character #\# #\i #'in-package-reader)
  (set-dispatch-macro-character #\# #\o #'select-ocml-ontology-reader)
  ;;
  (irs.api.javascript:initialise)
  (irs.api.rest:initialise)
  (irs.api.soap-old:initialise)
  (web:start-web-interface)
  (when visualiser-columns
    (visualiser:start visualiser-columns)))

(defun setup-variables (&key (host *host*) (port *port*) proxy-host proxy-port
			base-href
			(connection-timeout *connection-timeout*)
			(connection-read-timeout *connection-read-timeout*)
                        (yui :auto)
                        &allow-other-keys)
  "Set up global variables for the server.

HOST is the hostname with which to open the network sockets, thus
allowing multiple IRS servers on one machine.  If HOST is NIL, listen
on all interfaces.

BASE-HREF, if non-null, is used to set the base URI for web
interactions that go through Hunchentoot.  This is especially useful
for running the IRS behind Apache.  For example, if Apache is set to
proxy ‘http://foo/irsone/’ to ‘http://localhost:8181/’, BASE-HREF
should be set to ‘http//foo/irsone/’."
  (setf *host* host
        *port* port
        *proxy-host* proxy-host
        *proxy-port* proxy-port
	*connection-timeout* connection-timeout
	*connection-read-timeout* connection-read-timeout

	#+:irs-use-lispweb http::*LispWeb-conf-dir*
	#+:irs-use-lispweb (translate-logical-pathname "irs:config;")

	web-onto::*users-file*
	(merge-pathnames (translate-logical-pathname "irs:config;")
			 "registered-users")
	web-onto::*ontology-names-file*
	(merge-pathnames (translate-logical-pathname "irs:config;")
			 "ontology-names")
        web-onto::*old-source-directory*
        (translate-logical-pathname "irs:old-ocml-source;"))
  (irs.web:set-yui-location yui)
  (cascade-variables))

(defun cascade-variables ()
  "Secondary values that can be set from the primaries."
  (setf http::*host* irs:*host*))

(defun load-sys2 (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((target (read stream t nil t))
         (ontology? (ocml:find-ontology-directory target))
         (asdf? (asdf:find-system target nil)))
    (cond ((and ontology? asdf?)
            (error "Ambiguous load target `~A' identifies an ASDF system and an ontology." target))
           ((not (or asdf? ontology?))
            (error "Cannot find an ASDF system or OCML ontology named `~A'."
                   target))
           (ontology?
            (ocml:load-ontology-by-name target))
           (asdf?
            (irs:use-application (intern (symbol-name target) :keyword)))))
  '(values))

(defun in-package-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(in-package ,(read stream t nil t)))

(defun select-ocml-ontology-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(progn
     (in-package "OCML")
     (ocml::select-ontology (intern (symbol-name ',(read stream t nil t))
                                    (find-package "OCML")))))
