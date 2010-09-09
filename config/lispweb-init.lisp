;;; Sample initialization file for the LispWeb server. 

;;; When starting up, the server first loads the file whose name is
;;; the value of *LispWeb-init-file* in the directory indicated in 
;;; *LispWeb-conf-dir*; subsequently, it looks for *LispWeb-init-file* 
;;; (with a prepended ".") in the user's personal directory. Specific 
;;; options can also be passed to START-TCP-SERVICES, e.g.
;;; (START-TCP-SERVICES '(*http-port* 8080 *eval-port* 8081))

(in-package :http)

;;; General setup - Ports, root directory, init file.

;;; This sets the TCP port used by the HTTP server. The standard value is
;;; 80, but you must be root to use it, and it might not be a wise
;;; thing to do for security reasons. 

(setq *http-port* 3000)

;;; The TCP port used by the Telnet listener. Set to NIL to disable it.

(setq *eval-port* 3001)

;;; This is the path to the root of the server file system. 

;;; (setq *lispweb-default-dir* "/users/ipvaim/staff/Alberto/lisp/http/stuff/")

;;; The name of the initialization file to load at startup.
;;; (It makes very little sense to change it from here :))

;;; (setq *lispweb-init-file* "lispweb-init.lisp")

;;; The email address of the server operator. NOTE: the hostname is
;;; required.

;;; (setq *operator-address* "alb@ipvaim")

;;; The password to use the setup page and the Network Listener.

(setq *operator-password* "On The Run")

;;; Set to T if you want Lisp source for all pages to be visible by default.

;;; (setq *LispWeb-visible-pages* nil)

;;; Timeout for Keep-alive connections in seconds (not working yet)

;;; (setq *http-keepalive-timeout* 10)

;;; Allow serving regular HTML files? (Might be dangerous...)

;;; (setq *LispWeb-allow-html-files* nil)

;;; Allow the server to enter the debugger in case of errors?

;;; (setq *LispWeb-debug* nil)

;;; Logging - Log streams can be changed at run-time with the
;;; LW-SET-LOG function, e.g. (lw-set-log :debug nil).

;;; The name of the log file for server requests. Set to NIL to
;;; disable logging, or to T to send messages to standard output.

(setq *lispweb-log-file* (translate-logical-pathname "irs:log;lispweb-log-file.txt"))

;;; The name of the log file for debugging info. Set to NIL to disable
;;; logging, or to T to send messages to standard output.

(setq *lispweb-debug-file* (translate-logical-pathname "irs:log;lispweb-debug-file.txt"))

;;; The name of the log file for timing info. Set to NIL to disable
;;; logging, or to T to send messages to standard output.

;;; (setq *lispweb-timing-file* nil)

;;; Robot access - contents of the /robots.txt file, used to prevent 
;;; robots from accessing the server.

;;; (setq *LispWeb-robots-txt* "")

;;; Server extensions - 

;;; The HTTP-REPLY generic function can be specialized in order to 
;;; define new methods:

;;; (defmethod http-reply ((method :stat) request)
;;;   "Handle requests of the form: STAT request HTTP/1.0"
;;;   (handle-stat-request request))

;;; The HTTP-DELIVER generic function can be specialized to handle 
;;; different file types in GET requests. The file type must be 
;;; added to the list of known file types, identified by a name
;;; and a keyword. Example:

;;; (setq *lispweb-type-table*
;;;    (add-obj *lispweb-type-table* "/xdb" :xdb))

;;; (defmethod http-deliver ((type (eql :xdb)) request)
;;;   "Handle requests of the form GET /xdb/request HTTP/1.0"
;;;   (serve-xdb-file request))

;;; The HTTP-LOG generic function can be specialized to modify
;;; the way logging takes place. The two defined methods are:

;;; defmethod http-log ((info http-info))
;;; defmethod http-log ((message string))

;;; The first one is the top-level logging function, it is called 
;;; after the request has been processed with the *http-info* 
;;; structure as argument. Redefine it to change *what* will be logged.

;;; The second one deals with the actual output of the log message, 
;;; putting it in the appropriate format. Redefine this to change
;;; *how* requests are logged.