;;; Copyright © 2007-2009 The Open University

#-:irs-use-lispweb
(defpackage #:http
    (:use #:common-lisp)
  (:shadow #:log))

#-:irs-use-lispweb
(defpackage #:html
    (:use #:common-lisp)
  (:export #:html-out
	   #:inline-image
	   #:html-stream))

#-:irs-lispworks
(defpackage #:editor
    (:use #:common-lisp))

#-:irs-lispworks
(defpackage #:parsergen
    (:use #:common-lisp))

#-:irs-lispworks
(defpackage #:comm
    (:use #:common-lisp))

(defpackage #:irs
  (:use #:common-lisp)
  (:export #:*host*
           #:*port*
           #:*proxy-host*
           #:*proxy-port*
	   #:*base-href*
           #:*connection-timeout*
           #:*connection-read-timeout*
           #:find-ontology
           #:tobool
	   #:start
	   #:start-ocml
	   #:start-server
	   #:start-visualizer
	   #:initialise-application
	   #:register-source-namespace
	   #:start-application
	   #:use-application))

(defpackage #:irs.utilities
  (:nicknames #:iu #:irs-utilities #:utilities)
  (:use #:common-lisp #+:irs-lispworks #:fli #:http #:irs #:ocml)
  (:export #:*soap-end*
           #:*soap-end2*
           #:*soap-header*
           #:*soap-header2*
	   #:base-char-p
	   #:compose
	   #:define-constant
           #:get-accurate-time
	   #:http-request
           #:lisp-to-unix-epoch
           #:unix-to-lisp-epoch
           #:get-service-name
           #:make-xsd-date
           #:make-xsd-time
           #:make-xsd-datetime
	   #:replace-all
           #:xsd-date
           #:xsd-datetime
           #:xsd-time
           #:xsd-date-p
           #:xsd-datetime-p
           #:xsd-time-p))

(defpackage #:irs.api.javascript
  (:use #:common-lisp #:ocml #:irs #:irs.utilities)
  (:nicknames #:api-js)
  (:export #:event
           #:initialise))

(defpackage #:irs.api.rest
  (:use #:common-lisp #:ocml #:irs #:irs.utilities)
  (:nicknames #:api-rest)
  (:export #:initialise))

(defpackage #:irs.api.soap-old
  (:nicknames #:api-soap-old)
  (:use #:common-lisp)
  (:export #:initialise
           #:soap-xml-parse
           #:get-msg
           #:get-soapvalues
           #:string-soap-type-p
           #:xml-name
           #:xml-soap-type-p))

(defpackage #:irs.web
  (:use #:cl-who #:common-lisp #:utilities)
  (:nicknames #:web)
  (:export #:http-write-file
	   #:base-href
           #:send-url/file
	   #:register-plugin
           #:send-multipart
           #:setup-multipart
           #:set-yui-location
	   #:standard-page
	   #:start-web-interface
	   #:with-html
	   #:with-html-top
	   #:with-parameters
           #:write-http-stream
           #:yui-file-url))

(defpackage #:irs.browser
  (:use #:cl-who #:common-lisp #:irs)
  (:export)
  (:import-from #:irs.web #:with-html #:with-html-top #:yui-file-url))

(defpackage #:wsmo-protocol
  (:nicknames #:wp #:wsmo)
  (:use #:common-lisp #:http #:ocml #:irs.utilities)
  (:export #:invoke-web-service-operation
	   #:achieve-goal-slotted))

(defpackage #:cashew
  (:use #:common-lisp #:http #:ocml #:parsergen)
  (:export #:*ocml-definitions*
           #:ocml-define
           #:parse-cashew))

(defpackage #:irs.monitoring
  (:use #:common-lisp)
  (:nicknames #:im)
  (:export #:send-event
	   #:*session-id-placeholder*
	   #:create-start-invoke-web-service-event
	   #:create-end-invoke-web-service-event
	   #:create-start-achieve-goal-event
	   #:create-end-achieve-goal-event
	   #:create-monitoring-observer
	   #:delete-monitoring-observer))

;;; Elevation is my new name for lifting and lowering, because we need
;;; to do the skyhook process too.
(defpackage #:irs.grounding
  (:use :common-lisp)
  (:export #:apply-skyhooks
           #:get-lift-function
           #:get-lower-function
           #:set-debug-stream
           #:*debug-stream*)
  (:nicknames :grounding))

(defpackage #:irs.grounding.rest
  (:use :common-lisp :irs :ocml))

(defpackage #:irs.grounding.soap
  (:use :common-lisp :irs :ocml))

(defpackage #:visualiser
  (:nicknames #:ipv)
  (:use #:ocml #+:irs-lispworks #:capi #+:irs-lispworks #:gp #:common-lisp)
  (:export #:processing
           #:received-achieve-task-message
           #:refresh
           #:result-of-remote-procedure-call
           #:sending-achieve-task-message
           #:sending-remote-procedure-call-message
           #:start
           ))

(defpackage #:irs-protocol
  (:use #:common-lisp #:irs-utilities #:ocml)
  (:nicknames #:ip))

(defpackage #:kmi.vt.xpath-mini
  (:nicknames :xpm :xpath-mini)
  (:use #:common-lisp #:ocml #:parsergen))

(defpackage #:irs.webonto
  (:nicknames #:webonto #:web-onto)
  (:use #:common-lisp #:ocml #:irs.utilities)
  (:export #:require-ontologies
	   #:setup-library
	   #:initialise))
