;;; Copyright © 2007,2008,2009 The Open University.

;;; XXX The dependencies listed here are enough for the IRS to build
;;; from checkout without warnings, but are surely incomplete.
;;; Dependencies between sub-systems are simply a subset of the linear
;;; dependencies implied by the code/systems/irs.lisp load sequence.

;; Required for the xpath-mini module.
#+:irs-lispworks (require "parsergen")

(defsystem :irs
  :description "IRS"
  :version "3.5.1"
  :depends-on (:cl-who :cxml :drakma :hunchentoot
               :ironclad :cl-json #+:irs-use-lispweb lispweb :ocml)
  :components
  ((:module :api-javascript
	    ;; API for Javascript clients: RESTian, with JSON output.
	    :depends-on (:kernel :web)
	    :components
	    ((:file "api")
             (:file "events")
             (:file "javascript")))
   (:module :api-rest
	    ;; API for RESTian clients.
	    :depends-on (:api-soap-old :kernel :utilities :web)
	    :components
	    ((:file "api")
             (:file "support")))
   (:module :api-soap-old
	    ;; API for the legacy SOAP API on port 3000.
            :depends-on (:kernel :web)
            :components
            ((:file "monitoring")
             (:file "soap-xml")))
   #+:irs-lispworks
   (:module :cashew
	    ;; Parser for the Cashew process language.
	    :depends-on (:kernel)
	    :components
	    ((:file "instances")
	     (:file "interpret")
             (:file "parser")))
   (:module :grounding
	    ;; Native IRS groundings, and elevation.
	    :depends-on (:kernel #+:irs-lispworks :xpath-mini)
	    :components
	    ((:file "elevation")
             (:file "grounding")
             (:file "rest-grounding" :depends-on ("elevation"))
             (:file "soap-grounding" :depends-on ("elevation"))))
   #+:irs-lispworks
   (:module :visualiser
	    ;; Lispworks CAPI-based GUI events visualiser.
	    :depends-on (:kernel :monitoring :soap-irs)
	    :components
	    ((:file "client" :depends-on ("image"))
	     (:file "image")
	     (:file "irs-interface" :depends-on ("client"))))
   (:module :kernel
	    ;; Basic bootstrapping for the IRS.
	    :components
            ((:file "api-support" :depends-on ("defpackage"))
             (:file "defpackage")
             (:file "globals" :depends-on ("defpackage"))
             (:file "application" :depends-on ("defpackage"))
             (:file "namespaces" :depends-on ("defpackage"))
             (:file "startup" :depends-on ("defpackage" "globals"))))
   (:module :monitoring
	    ;; Event monitoring.
	    :depends-on (:kernel)
	    :components
	    ((:file "monitoring")))
   (:module :soap-irs
	    :depends-on (:kernel :soap-irs-protocol)
	    :components
	    ((:file "easy-irs")
             (:file "wenjin-irs")))
   (:module :soap-irs-protocol
	    :depends-on (:kernel :utilities :webonto)
	    :components
            ((:file "protocol")
             (:file "soapprotocol" :depends-on ("protocol"))
             (:file "handlers")
             (:file "soap")
             (:file "soap-post-handlers" :depends-on ("protocol"))
             (:file "upload")
             (:file "soap-attachments")
             (:file "init")))
   (:module :utilities
	    :depends-on (:kernel)
	    :components
            ((:file "def-irs-soap-bindings")
             (:file "generate-ocml-source")
             (:file "ocml-extension")
             (:file "soap-response" :depends-on ("utilities"))
             (:file "time" :depends-on ("utilities"))
             (:file "utilities")
             (:file "view-ontology")
             (:file "xsd-types")))
   (:module :web
	    ;; Web browser interface.

	    ;; XXX We depend on these only because we scooped the code
	    ;; out of those packages and haven't disentangled it yet.
	    :depends-on (:soap-irs-protocol :soap-irs :utilities :wsmo-protocol)
	    ;; Because many things are multiply defined, until we fix
	    ;; that, we have to load in the right order.  This order
	    ;; is the same as the old package/file order.
	    :serial t
	    ;; Filenames are prefixed with the abbreviated subsystem
	    ;; they came from.
            :components
            ((:file "web")
             (:file "browser")
             (:file "browser-describe")
             (:file "mnm-html-describe")
             (:file "mnm-server")
             (:file "sip-pages")
             (:file "si-html-describe")
             (:file "si-service-pages")
             (:file "si-publish-url-pages")
             (:file "si-knowledge-level-description-pages")
             (:file "wp-generate-ontology-pages")
             (:file "wp-html-describe")
             (:file "wp-pages")))
   ;; XXX The dependencies listed here are enough for WebOnto to build
   ;; from checkout without warnings, but are surely incomplete.
   ;; Dependencies between sub-systems are simply a subset of the
   ;; linear dependencies implied by the old defsys.lisp.
   (:module :webonto
	    :depends-on (:kernel :utilities)
	    :components
	    (#-:irs-use-lispweb (:file "compat")
	     (:file "vars" :depends-on (#-:irs-use-lispweb "compat"))
	     (:file "files" :depends-on ("vars"))
	     (:file "utilities" :depends-on ("vars"))
	     (:file "ontology-init" :depends-on ("vars"))
	     (:file "ocml-utilities" :depends-on ("vars"))
	     (:file "input" :depends-on ("vars"))
	     (:file "output" :depends-on ("diagrams" "edit-ontology-properties"
						     "reload-ontology" "users" "vars"
						     "lois"))
	     (:file "lois" :depends-on ("vars"))
	     (:file "refine-query" :depends-on ("vars"))
	     (:file "parse-requests" :depends-on ("vars"))
	     (:file "drawing" :depends-on ("vars"))
	     (:file "tree-drawing" :depends-on ("vars" "drawing"))
	     (:file "ocml-interface" :depends-on ("vars" "drawing"))
	     (:file "editing" :depends-on ("vars"))
	     (:file "new-definition" :depends-on ("vars"))
	     ;;(:file  "generate-planet-forms" :depends-on ("vars"))
	     (:file "planet-pages" :depends-on ("vars"))
	     (:file "html-describe-ocml-objects" :depends-on ("vars"))
	     (:file "generate-database-table" :depends-on ("vars"))
	     (:file "diagrams" :depends-on ("vars"))
	     (:file "new-ontology" :depends-on ("vars" "ocml-interface"))
	     (:file "delete-ontology" :depends-on ("vars"))
	     (:file "users" :depends-on ("vars"))
	     (:file "edit-ontology-properties" :depends-on ("vars" "new-ontology"))
	     (:file "reload-ontology" :depends-on ("vars"))
	     (:file "find-ocml-object" :depends-on ("vars"))
	     (:file "filters" :depends-on ("vars"))
	     (:file "web-tables" :depends-on ("vars"))
	     (:file "display-connected-instances" :depends-on ("vars"))
	     (:file "check-instances" :depends-on ("vars"))
	     (:file "show-class" :depends-on ("vars"))
	     (:file "usage-data" :depends-on ("vars"))
	     ;;(:file  "email-parser-statistics" :depends-on ("vars"))
	     (:file "ocml-from-text" :depends-on ("vars"))
	     (:file "patterns" :depends-on ("vars"))
	     (:file "templates" :depends-on ("vars"))
	     (:file "publish-ontology" :depends-on ("vars"))
	     (:file "publish-ontolingua-ontology" :depends-on ("vars"))
	     (:file "upload" :depends-on ("vars"))
	     (:file "init" :depends-on ("vars"))))
   (:module :wsmo-protocol
	    :depends-on (:kernel :soap-irs-protocol)
	    :components
	    ((:file "choreography" :depends-on ("globals"))
 	     (:file "cs-goal-invocation" :depends-on ("globals"))
	     (:file "globals")
	     (:file "goal-invocation" :depends-on ("globals"))
	     (:file "invoke-web-service" :depends-on ("globals"))
	     (:file "lifting-and-lowering")
	     (:file "mediate")
	     (:file "mediation-library")
	     (:file "ocml-choreography-support")
	     (:file "orchestration" :depends-on ("globals"))
	     (:file "publish")
	     (:file "soap-post-handlers")
	     (:file "trusted-goal-invocation" :depends-on ("globals"))
	     (:file "utilities")))
   #+:irs-lispworks
   (:module :xpath-mini
	    ;; Partial implementation of XPath.
            :depends-on (:kernel)
            :components
            ((:file "globals")
             (:file "ocml-accessors" :depends-on ("globals"))
             (:file "ocml-generators" :depends-on ("globals"))
             (:file "ocml-lift" :depends-on
                    ("globals" "ocml-generators" "xml-accessors"))
             (:file "ocml-lower" :depends-on
                    ("globals" "ocml-generators" "xml-accessors"))
             (:file "utilities")
             (:file "xml-accessors" :depends-on ("globals"))
             (:file "xpath-parser" :depends-on ("globals"))))))
