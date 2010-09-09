;;; Copyright © The Open University 2007-2010

(in-package #:irs.web)

(defvar *web-interface-server* nil)

(defvar *irs-plugins* '())

(defvar *yui-location*)

(defvar *yui-location-mode*)

(eval-when (:compile-toplevel :load-toplevel)
  (define-constant +plugin-types+ '(:api :application :core)))

(deftype plugin-type ()
  "IRS web plugin types."
  `(member ,@+plugin-types+))

(defstruct (plugin (:type list))
  name type description dispatch-table)

(defun register-plugin (name type description dispatch-table)
  (assert (typep type 'plugin-type))
  (let ((el (assoc name *irs-plugins*)))
    (if el
	(setf (cdr el) (list type description dispatch-table))
	(push (list name type description dispatch-table) *irs-plugins*))
    (when *web-interface-server*
      (restart-web-server))
    el))

;;; {{{ Yahoo YUI URL handling
(defun set-yui-location (mode)
  "Configure how to serve Yahoo's YUI Javascript.

With mode :AUTO checks for the files locally, and if found, serves
them.  :LOCAL forces local serving; :REMOTE forces use of Yahoo's
server provision.  :PROXY will cause the uses a local link which the
IRS HTTP server will redirect to the Yahoo servers."
  (setf *yui-location-mode* mode)
  (case mode
    ((:auto)
     (if (webonto::file-exists? "irs:external;yui;")
         (set-yui-location :local)
         (set-yui-location :remote)))
    ((:local)
     (setf *yui-location* "irs/yui/"))
    ((:proxy)
     (setf *yui-location* "irs/yui/"))
    ((:remote)
     (setf *yui-location* "http://yui.yahooapis.com/2.7.0/"))))

(defun handle-yui-url ()
  (ecase *yui-location-mode*
    ((:local)
     (send-url/file "/irs/yui" "irs:external;yui"))
    ((:proxy)
     (redirect-url "/irs/yui" "http://yui.yahooapis.com/2.7.0"))
    ((:remote)
     (setf (hunchentoot:return-code*)
	   hunchentoot:+http-not-found+))))

;;; }}}

(defun construct-dispatch-table ()
  "Create the dispatch table based on current plugins."
  (append (apply #'append (mapcar #'plugin-dispatch-table *irs-plugins*))
	  (list #'hunchentoot:default-dispatcher)))

(defun plugins-of-type (type)
  (assert (typep type 'plugin-type))
  (remove-if-not (lambda (x) (eq x :application)) *irs-plugins*
                 :key #'plugin-type))

(defun start-web-interface ()
  (setf hunchentoot:*default-handler* #'draw-default-page)
  ;; Otherwise Hunchentoot will write it's own message to the client,
  ;; and that mucks up our corporate image.
  (setf hunchentoot:*handle-http-errors-p* nil)
  (register-plugin
   :irs :core
   "The Internet Reasoning Service"
   (nconc (mapcar (lambda (args)
                    (apply #'hunchentoot:create-regex-dispatcher args))
                  `(("irs/events$" draw-events-page)
                    ("irs/graph$" draw-graph-page)
                    ("irs/goals/" draw-goals-page)
                    ("irs/javascript/" ,(lambda () (send-url/file "/irs/javascript" "irs:javascript")))
                    ("irs/news$" draw-news-page)
                    ("irs/ontologies$" draw-ontologies-page)
                    ("irs/ontology/ocml/" draw-ontology-ocml-page)
                    ("irs/ontology/rdf/" draw-ontology-rdf-page)
                    ("irs/plugins$" draw-plugins-page)
                    ("irs/services/" draw-services-page)
                    ("irs/applications" draw-applications-page)
                    ("irs/assets/" ,(lambda () (send-url/file "/irs/assets" "irs:assets")))
                    ("irs/yui/" handle-yui-url)
                    ;Added by Neil B. to allow Pierre G. to serve ontology files from /ontologies/public/
                    ("ontologies/public/" ,(lambda () 
                                              (setf (logical-pathname-translations "ontologies") 
                                                    '(("**;*.*.*" "c:\\ontologies/**/*"))) 
                                              (send-url/file "/ontologies/public" "ontologies:public")))  
                    ("irs$" draw-top-page)
                    ("robots.txt$" draw-robots-page)
                    ("/$" redirect-to-top-page)))))
  (restart-web-server)
  (irs.browser::start-browser-interface))

(defun restart-web-server ()
  (setf ;; XXX hunchentoot:*show-lisp-backtraces-p* t
        hunchentoot:*show-lisp-errors-p* t)
  (when *web-interface-server*
    (hunchentoot:stop *web-interface-server*)
    (setf *web-interface-server* nil))
  (setf hunchentoot:*dispatch-table* (construct-dispatch-table)
	*web-interface-server* (make-instance 'hunchentoot:acceptor
					      :port irs:*port* :address irs:*host*))
  (setf hunchentoot:*message-log-pathname*
	(logical-pathname "irs:log;hunchentoot-message.log")
	hunchentoot:*access-log-pathname*
	(logical-pathname "irs:log;hunchentoot-access.log"))
  (hunchentoot:start *web-interface-server*))

(defmacro with-html (&body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue nil :indent nil)
     (htm ,@body)))

(defmacro with-html-top (&body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent nil)
     ,@body))

(defmacro with-maybe-error-page (&body body)
  `(handler-case
       (progn ,@body)
     (error (c) (draw-error-page c))))

(defun yui-file-url (uri-part)
  (format nil "~A~A" *yui-location* uri-part))

(defun send-yui-links ()
  (with-html
   (htm
    ((:link :rel "stylesheet" :type "text/css"
	    :href (yui-file-url "build/container/assets/container.css")))
    ((:link :rel "stylesheet" :type "text/css"
	    :href (yui-file-url "build/logger/assets/skins/sam/logger.css")))
    ((:link :rel "stylesheet" :type "text/css"
	    :href (yui-file-url "build/assets/skins/sam/container.css")))
    ((:link :rel "stylesheet" :type "text/css"
	    :href (yui-file-url "build/container/assets/skins/sam/container.css")))
    ((:link :rel "stylesheet" :type "text/css"
	    :href (yui-file-url "build/assets/skins/sam/tabview.css")))
    ((:link :rel "stylesheet" :type "text/css"
	    :href (yui-file-url "build/treeview/assets/skins/sam/treeview.css")))
    ((:link :rel "stylesheet" :type "text/css"
	    :href (yui-file-url "build/menu/assets/skins/sam/menu.css")))
    ((:link :rel "stylesheet" :type "text/css"
	    :href (yui-file-url "build/assets/skins/sam/button.css")))
    ((:link :rel "stylesheet" :type "text/css"
	    :href (yui-file-url "build/assets/skins/sam/resize.css")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/yahoo-dom-event/yahoo-dom-event.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/dragdrop/dragdrop-min.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/container/container-min.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/event/event-min.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/json/json-min.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/element/element-min.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/resize/resize-min.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/menu/menu.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/tabview/tabview-min.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/slider/slider-min.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/treeview/treeview-debug.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/button/button.js")))
    ((:script :type "text/javascript"
	      :src (yui-file-url "build/logger/logger-min.js"))))))

(defun base-href ()
  (if irs:*base-href*
      irs:*base-href*
      (format nil "http://~A:~A/" (or irs:*host* "localhost") irs:*port*)))

(defmacro standard-page (title &body body)
  `(with-maybe-error-page
     (setf (hunchentoot:content-type*) "text/html;charset=UTF-8")
     (with-html-top
       (:html
	(:head (:title ,title)
	       ((:base :id "baseid" :href (base-href)))
	       ((:link :rel "stylesheet" :type "text/css"
                       :href "irs/assets/css/stylesheet.css"))
               ((:link :rel "stylesheet" :type "text/css"
                       :href "irs/assets/css/browser.css"))
	       ((:link :rel "icon" :type "image/png"
		       :href "irs/assets/images/iris.png"))
               (str (send-yui-links)))
	(:body
         (str (draw-menu ,title))
         ((:div :id "content") ,@body))))))

(defun draw-error-page (condition)
  (declare (ignore condition))
  (setf (hunchentoot:return-code*)
	hunchentoot:+http-internal-server-error+)
  (with-html-top
    (:html
     (:head (:title "Internal error")
	    ((:link :href "irs/assets/css/stylesheet.css"
		    :rel "stylesheet" :type "text/css"))
	    ((:link :rel "icon" :type "image/png"
		    :href "irs/assets/images/iris.png")))
     (:body
      (str (draw-menu "Internal error"))
      ((:div :id "content")
       (:p "The IRS has experienced an internal error.  If you wish to
      report this bug, please send an email
      to " ((:a :href "mailto:j.b.domingue@open.ac.uk") "John
      Domingue") "."))))))

(defmacro with-parameters ((kind vars) &body body)
  ""
  (let* ((getter (ecase kind
		   (:post 'hunchentoot:post-parameter)
		   (:get 'hunchentoot:get-parameter)))
	 (bindings
	  (mapcar (lambda (sym)
		    `(,sym (,getter ,(string-downcase (symbol-name sym)))))
		  vars)))
    `(let ,bindings
       ,@body)))

(defun draw-menu (&optional (title ""))
  (with-html
    ((:div :id "banner")
     (:h1 (str title)))
    ((:div :id "menu")
     (:table
      (:tr (:td ((:a :href "http://www.kmi.open.ac.uk/projects/irs/")
                 (:img :src "irs/assets/images/irs-logo.png"))))
      (:tr (:td (:br)))
      (:tr (:td ((:a :href "irs/applications") "applications")))
      (:tr (:td ((:a :href "irs/browser") "browser")))
      (:tr (:td ((:a :href "irs/editor") "editor")))
      (:tr (:td ((:a :href "irs/events") "events")))
      (:tr (:td ((:a :href "irs/graph") "graph")))
      (:tr (:td ((:a :href "irs") "home")))
      (:tr (:td ((:a :href "irs/assets/html/monitoring-tool.html")
                 "monitoring")))
      (:tr (:td ((:a :href "irs/ontologies") "ontologies")))
      (:tr (:td ((:a :href "irs/plugins") "plugins")))
      (:tr (:td (:br)))
      (:tr (:td ((:a :href "http://www.kmi.open.ac.uk/")
                 (:img :src "irs/assets/images/kmi-logo.png"))))
      (:tr (:td (:br)))
      (:tr (:td ((:a :href "http://www.open.ac.uk/")
                 (:img :src "irs/assets/images/ou-shield.jpg"))))))))

;;;; Pages

(defun draw-top-page ()
  (standard-page "The Internet Reasoning Service"
    ((:div :class "reading-width")
     (:p "The Internet Reasoning Service - IRS - is KMi's Semantic Web
    Services platform.  It allows applications to semantically
    describe and execute Web services. The IRS supports the provision
    of semantic reasoning services within the context of the Semantic
    Web.")

     (:p "The IRS is currently being applied within the "
         (:a :href "http://projects.kmi.open.ac.uk/soa4all" "SOA4All") " and "
         (:a :href "http://projects.kmi.open.ac.uk/notube/" "NoTube")
         " projects.  IRS technologies and experiences feed into the "
         ((:a :href "http://cms-wg.sti2.org/") "Conceptual Models of Services")
         " working group, successor to
        the " ((:a :href "http://www.wsmo.org/") "WSMO working
        group") ".")

     (:ul (:li ((:a :href "http://www.kmi.open.ac.uk/projects/irs/")
                "IRS homepage"))
          (:li ((:a :href "irs/news" ) "News")))
     (:center
      ((:table :class "logo-table")
       (:tr (:td :align "center"
                 ((:a :href "http://cms-wg.sti2.org/")
                  (:img :src "irs/assets/images/cms-logo.png")))
            (:td)
            (:td :align "center"
                 ((:a :href "http://www.w3.org/2001/sw/")
                  (:img :src "irs/assets/images/sw-horz.png")))
            (:td))
       (:tr (:td) (:td :align "center"
                       ((:a :href "http://en.wikipedia.org/wiki/Common_Lisp")
                        (:img :src "irs/assets/images/logo120x80.png")))
            (:td)
            (:td :align "center"
                 ((:a :href "http://www.wsmo.org/")
                  (:img :src "irs/assets/images/wsmo-logo.png")))))))))

(defun redirect-to-top-page ()
  (hunchentoot:redirect (format nil "~A~A" (base-href) "irs")))

(defun draw-default-page ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (standard-page "You seem to be lost"
    (:p "There is no known page here.  Click on one of the links above.")))

;;; {{{ Graphing
(defun draw-graph-page ()
  (standard-page "IRS graph view prototype"
    ((:form :method :get :action "response" :name "graphform")
     ((:table)
      (:tr (:td "Ontology")
	   (:td ((:input :type "text" :name "ontology" :size 40))))
      (:tr (:td "Concept")
	   (:td ((:input :type "text" :name "concept" :size 40))))
      (:tr (:td ((:a :href "javascript:drawGraph()") "Draw"))))
     ((:div :id "canvas"))
     ((:div :id "status"))
     ((:script :type "text/javascript" :src "irs/javascript/ocml-api.js"))
     ((:script :type "text/javascript" :src "irs/javascript/irs-api.js"))
     ((:script :src "irs/javascript/graph.js" :type "text/javascript")))))

;;; }}}

(defun draw-robots-page ()
  (write-http-stream
   "text/plain"
   "# This is an Internet Reasoning Service.
# There is nothing of interest to robots here :-)
User-agent: *
Disallow: /
"))

(defun draw-news-page ()
  (standard-page "IRS News"
    (with-open-file (in (cl-user::from-irs-home "NEWS"))
      (htm (:pre
	(loop for line = (read-line in nil :eof)
	   until (or (eq line :eof) (string= line ""))
	   do
             (progn
               (str (escape-string-minimal line))
               (fmt "~%"))))))))

(defun draw-applications-page ()
  (standard-page "Applications"
    (:p "These are the currently loaded IRS applications.  For
    other projects using the IRS, check the "
	((:a :href "http://www.kmi.open.ac.uk/projects/irs/") "IRS homepage")
	".")
    (:table
     (:tr (:th "name") (:th "description"))
     (dolist (app (sort (plugins-of-type :application)
			#'string< :key #'(lambda (x) (symbol-name (plugin-name x)))))
       (htm (:tr (:td ((:a :href (format nil "~(~a~)" (first app)))
		       (str (string-downcase (symbol-name (plugin-name app))))))
		 (:td (str (plugin-description app)))))))))

(defun draw-plugins-page ()
  (standard-page "Plugins"
    (:p "The currently loaded IRS plugins are:")
    (:table
     (:tr (:th "name") (:th "type") (:th "description"))
     (dolist (app (sort *irs-plugins*
			#'string< :key #'(lambda (x) (symbol-name (plugin-name x)))))
       (htm (:tr (:td (str (string-downcase (symbol-name (plugin-name app)))))
		 (:td (str (string-downcase (symbol-name (plugin-type app)))))
                 (:td (str (plugin-description app)))))))))

(defmacro capturing-http-stream (&body body)
  `(with-output-to-string (str)
     (let ((html:html-stream str))
       (declare (special html:html-stream))
       ,@body)))

(defun draw-ontology-ocml-page ()
  (let ((ontology (last-bit-of-url)))
    (setf (hunchentoot:content-type*) "text/html;charset=UTF-8")
    (capturing-http-stream
         (wp::generate-ontology-page (intern (string-upcase ontology) :ocml)))))

;;; {{{ Static files, MIME types
(defun write-http-stream (mime-type payload)
  (setf (hunchentoot:content-type*) mime-type)
  (let ((stream (hunchentoot:send-headers))
	(octets (if (stringp payload)
		    (flexi-streams:string-to-octets payload)
		    payload)))
    (write-sequence octets stream)))

(defun http-write-file (filename mime-type)
  "Send contents of FILENAME to the HTTP stream, along with its MIME-TYPE."
      (setf (hunchentoot:content-type*) mime-type)
  (let* ((stream (hunchentoot:send-headers))
	 (buffer (make-array 1024 :element-type '(unsigned-byte 8))))
    (with-open-file (in filename :element-type '(unsigned-byte 8))
      (loop for pos = (read-sequence buffer in)
	 until (zerop pos)
	 do (write-sequence buffer stream :end pos)))))

;;; XXX This should be "application/rdf+xml", but several browsers do
;;; not understand it and either print unreadable streams of the
;;; contents (Firefox) or refuse to deal with it at all (Konqueror).
(define-constant +rdf-mime-type+ "application/xml")

(define-constant +mime-file-extensions+
  `((".css" "text/css")
    (".gif" "image/gif")
    (".html" "text/html")
    (".jpg" "image/jpg")
    (".js" "text/javascript")
    (".png" "image/png")
    (".svg" "image/svg+xml")
    (".lisp" "text/plain")
    (".rdf" ,+rdf-mime-type+)
    (".zip" "application/zip")))

(defun mime-type (filename)
  (let* ((extension (subseq filename (position #\. filename :from-end t)))
	 (type (second (assoc extension +mime-file-extensions+ :test 'string=))))
    (if type
	type
	(error "Unknown MIME type for file ~S." filename))))

(defun send-url/file (url-prefix fs-base)
  "Strips URL-PREFIX from requested URL, prepends FS-BASE, and sends
the file with MIME type from filename extension."
  (let ((path (send-url/file* (hunchentoot:request-uri*) url-prefix fs-base)))
    (http-write-file path (mime-type path))))

(defun send-url/file* (url url-prefix fs-base)
  "Strips URL-PREFIX from requested URL, prepends FS-BASE, and sends
the file with MIME type from filename extension."
  (let ((relpath (substitute #\; #\/ (subseq url (length url-prefix)))))
    (format nil "~A~A" fs-base relpath)))

(defun redirect-url (url-prefix new-base)
  "Strips URL-PREFIX from requested URL, prepends FS-BASE, and sends
the file with MIME type from filename extension."
  (let* ((url (hunchentoot:request-uri*))
	 (keep (subseq url (length url-prefix)))
	 (new (format nil "~A~A" new-base keep)))
    (hunchentoot:redirect new)))

;;; }}}

(defun draw-ontology-rdf-page ()
  (with-maybe-error-page
    (let* ((ontology (last-bit-of-url))
	  (rdf (with-output-to-string (rdf-stream)
		 (ocml:translate :ocml :rdfs (intern (string-upcase ontology) :ocml)
				 :where rdf-stream))))
     (with-parameters (:get (format))
       (if (string= format "raw")
           (progn
             (setf (hunchentoot:content-type*) +rdf-mime-type+)
             rdf)
	   (standard-page "RDFS"
	     (:p ((:a :href (format nil "~A~A" (hunchentoot:request-uri) "?format=raw" ))
		  "Raw RDFS"))
	     (:pre (str (escape-string-minimal rdf)))))))))

(defun draw-ontologies-page ()
  (standard-page "Ontologies"
    (:table
     (dolist (ontology (sort (mapcar #'cdr ocml::*all-ontologies*)
			     #'string-lessp :key #'ocml::name))
       (ocml:with-ontology (ontology)
	 (let ((ont (ocml::name ontology)))
	  (htm
	   (:tr  (:td (:b (fmt "~(~A~)" (ocml::name ontology))))
		 (:td (fmt "~(~A~)" (ocml::ontology-type ontology) ))
		 (:td ((:a :href (format nil "irs/ontology/ocml/~(~A~)" ont))
		       "OCML (WebOnto)"))
		 (:td ((:a :href (format nil "irs/ontology/rdf/~(~A~)" ont))
		       "RDFS"))
		 (:td  (fmt "~A<small>[~A]</small>"
			    (link-to ont :services)
			    (length (services-interesting-to-users))
			    ))
		 (:td (fmt "~A<small>[~A]</small>" (link-to ont :goals)
			   (length (goals-interesting-to-users))))))))))))

(defun link-to (ocml-symbol &optional (link-type :irs) (fragment nil))
  "Create HTML link to the relevant page about OCML-SYMBOL.  
If FRAGMENT is true, encode a URL with fragment."
  (ecase link-type
    (:irs
     (let* ((str (symbol-name ocml-symbol)))
       (reverse (wsmo-protocol::irs-uri-lookup-current-word
                 str (length str) (ocml::name ocml::*current-ontology*) nil
                 "irs/ontology/ocml/~(~a#~a~)"))))
    ((:goals :services)
     (http::anchor (if fragment
                       (format nil "irs/~(~a/~a#~a~)" link-type (ocml::name ocml::*current-ontology*) ocml-symbol)
                       (format nil "irs/~(~a/~a~)" link-type  ocml-symbol))
                   (if fragment 
                       (string-downcase (symbol-name ocml-symbol))
                       (string-downcase (symbol-name link-type)))))))

;;;; Pages to give folks something that looks like a UDDI registry.

(defun services-interesting-to-users ()
  "OCML names of web services that are of interest to non-IRS developers."
  (delete-if (lambda (service)
               (not (webonto::findany
		     '?x `(ocml::wsmo-web-service-host ,service ?x))))
             (mapcar #'ocml::name (kinds-of 'ocml::web-service))))

(defun goals-interesting-to-users ()
  "OCML names of goals, most of which are of interest to non-IRS developers."
  (delete-if (lambda (goal)
               (member goal '(ocml::run-ocml-expression-goal
                              ocml::suitable-web-service-goal
                              ocml::find-web-services-for-goal
                              ocml::mediate-input-role-values-goal)))
             (mapcar #'ocml::name (kinds-of 'ocml::goal))))

(defun kinds-of (thing &optional (ontology (ocml::name ocml::*current-ontology*)))
  (ocml:with-ontology (ontology)
    (let ((class (ocml::get-domain-class thing)))
      (if class
          (sort (ocml::current-subclasses class)
                #'string-lessp :key #'ocml::name)
        nil))))

(defun last-bit-of-url ()
  (let* ((uri (hunchentoot:request-uri))
	 (bit (subseq uri (+ 1 (position #\/ uri :from-end t))))
	 (without-params (subseq bit 0 (position #\? bit))))
    without-params))

;;; Actual page definitions.

(defun draw-services-page ()
  (let ((onto (intern (string-upcase (last-bit-of-url)) :ocml)))
    (ocml:with-ontology (onto)
      (standard-page "Services"
	(:p "Web Services in the " (:b (str (link-to onto)))  " ontology.")
	(:br)
	(:hr)
	(dolist (service (services-interesting-to-users))
	  (str (draw-service service)))))))

(defun draw-goals-page ()
  (let ((onto (intern (string-upcase (last-bit-of-url)) :ocml)))
    (ocml:with-ontology (onto)
      (standard-page "Goals"
	(:p "Goals available in the " (:b (str (link-to onto))) " ontology.")
	(:br)
	(:hr)
	(dolist (goal (goals-interesting-to-users))
	    (str (draw-goal goal)))))))

(defun draw-service (service)
  "Write HTML describing SERVICE."
  (let ((nfp (web-onto::findany
	      '?x `(= ?x (ocml::the-class-slot-value
	       	   ,service ocml::has-non-functional-properties)))))
    (with-html
     (:h3 ((:a :name (string-downcase service)) (fmt "~(~A~)" service)))
     (:table
      (str (draw-io-types service))
      (str (draw-associated-goals service))
      (str (draw-grounding service))
      (str (draw-non-functional-properties nfp))))))

(defun draw-grounding (service)
  (let ((host (web-onto::findany '?x `(ocml::wsmo-web-service-host ,service ?x)))
	(port (web-onto::findany '?x `(ocml::wsmo-web-service-port ,service ?x)))
	(location (web-onto::findany '?x `(ocml::wsmo-web-service-location ,service ?x))))
    (with-html
      (:tr (:td (:b "grounding")) (:td (fmt "~A" (service-grounding-pretty-name service))))
      (:tr (:td (:b "host")) (:td (fmt "~A" host)))
      (:tr (:td (:b "port")) (:td (fmt "~A" port)))
      (:tr (:td (:b "location")) (:td (fmt "~A" location))))))


(defun service-grounding (service)
  (handler-case       
      (let* ((associated-grounding
              (web-onto::findany '?m `(ocml::associated-grounding ,service  ?m)))
             (operation (wsmo-protocol::operation-mappings-name (wsmo-protocol::wsmo-grounding-operation-mappings
                                                                 (car associated-grounding)))))
        (wsmo-protocol::get-operation-grounding-type associated-grounding operation))
    ;; The dummy function `grounding' messes up the above expression, so we just
    ;; catch the condition and return a useful symbol.
    (t (condition) 'grounded-to-dummy-function)))

(defun service-grounding-pretty-name (service)
  "Short name, as string, of the grounding for SERVICE."
  (let ((sym (service-grounding service)))
    (subseq (symbol-name sym) (length "grounded-to-"))))

(defun draw-io-types (source)
  (with-html
    (:tr (:td (:b "input types"))
	 (:td (dolist (type (ip::input-roles-with-soap-bindings source))
		(fmt "~(~A~) " (link-to (wsmo-protocol::get-input-role-type source (first type)))))))
    (:tr (:td (:b "output type"))
	 (:td     (fmt "~(~A~)" (link-to (wsmo-protocol::get-output-role-type source)))))))

(defun draw-goal (goal)
  "Write HTML about GOAL."
  (let* ((nfp (web-onto::findany
	       '?x `(= ?x (ocml::the-class-slot-value
			   ,goal ocml::has-non-functional-properties)))))
    (with-html
      (:h3 (str (link-to goal)))
      (:table
       (str (draw-io-types goal))
       (str (draw-associated-services goal))
       (str (draw-non-functional-properties nfp))))))

(defun draw-non-functional-properties (nfp)
  "Output HTML about the OCML non-functional-property denoted by NFP."
  (let ((class (ocml::get-domain-class nfp)))
    (if class
	(let ((vals (loop for slot in (ocml::domain-slots class)
		       collect (multiple-value-bind (values default)
				   (ocml::get-slot-values-from-class-structure class slot)
				 (when (or values default)
				   (list slot (first (or values default))))))))
	  (if (some #'identity vals)
	      (with-html
		(:tr (:td (:b "non-functional properties"))
		     (:td (:table
			   (dolist (slot vals)
			     (when slot
			       (htm (:tr
				     (:td (str (string-downcase
						(subseq (symbol-name (first slot))
							(length "has-")))))
				     (:td(str (second slot)))))))))))
	      (with-html
		(:tr (:td (:b "non-functional properties")) (:td "none")))))
	(with-html
		(:tr (:td (:b "non-functional properties")) (:td "none"))
	  (:br)))))

(defun draw-associated-services (goal)
  (with-html 
    (:tr (:td (:b "uses services"))
	 (:td (dolist (service (delete-if #'ocml:nothing?
				       (web-onto::findall
					'?service `(ocml::associated-goal  ?service ,goal))))
		(str (link-to service :services t))
		(str " "))))))

(defun draw-associated-goals (service)
  (with-html 
    (:tr (:td (:b "used by goals"))
	 (:td (dolist (service (delete-if #'ocml:nothing?
				       (web-onto::findall '?goal `(ocml::associated-goal ,service ?goal))))
		(str (link-to service :goals t))
		(str " "))))))

;;; {{{ Events
(defun draw-events-page ()
  (standard-page "IRS Events Visualiser"
    (htm
     ((:div :class "yui-skin-sam")
      ((:div :id "eventspage" :class "yui-navset")
       ((:ul :class "yui-nav")
        (:li ((:a :href "#tab1") (:em "Events")))
        (:li ((:a :href "#tab2") (:em "Events by Service")))
        (:li ((:a :href "#tab3") (:em "Services"))))
       ((:div :class "yui-content")
        ((:div :id "tab1")
         ((:table :width "100%")
          (:thead
           (:tr (:th "Time") (:th "Type") (:th "Service") (:th "Comment")))
          ((:tbody :id "events"))))
        ((:div :id "tab2")
         ((:table :width "100%")
          ((:tbody :id "events-by-service"))))
        ((:div :id "tab3")
         ((:table :width "100%")
          (:thead (:tr (:th "Name") (:th "Location")))
          ((:tbody :id "services")))))))
     ((:script :src "irs/javascript/irs-api.js" :type "text/javascript"))
     ((:script :src "irs/javascript/events.js" :type "text/javascript"))
     ((:script :type "text/javascript") "uponLoad();"))))
;;; }}}

;;; {{{ MIME Multipart support
(define-constant +mime-multipart-boundary+ "xxx")

(defun setup-multipart ()
  "Send HTTP headers indicating this is for  , and stop Hunchentoot closing stream.  Returns
the HTTP stream for writing."
  (setf (hunchentoot:content-type*)
        (format nil "multipart/x-mixed-replace; boundary=~S" +mime-multipart-boundary+))
  (let ((stream (hunchentoot:send-headers)))
    ;; Stop Hunchentoot closing this socket.
    (setf hunchentoot::*hunchentoot-stream* nil)
    stream))

(defun xformat (stream control &rest rest)
  (let ((string (apply #'format nil control rest)))
    (write-sequence (flexi-streams:string-to-octets string) stream)))

(defun send-multipart (stream content-type content)
  (xformat stream "--~A~%" +mime-multipart-boundary+)
  (xformat stream "Content-type: ~A~%~%" content-type)
  (xformat stream content)
  (xformat stream "--~A~%" +mime-multipart-boundary+)
  (force-output stream))

;;; }}}
