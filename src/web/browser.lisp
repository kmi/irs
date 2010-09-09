;;; Copyright © 2007,2008 The Open University

(in-package #:irs.browser)

(defun start-browser-interface ()
  (irs.web:register-plugin
   :browser :core
   "A Web2.0 glance into the turbulence of the IRS world."
   (nconc (mapcar (lambda (args)
                    (apply #'hunchentoot:create-regex-dispatcher args))
                  '(("irs/browser$" gen-mainpage)
                    ("irs/editor" gen-irseditor)
                    ("soap" redirect-to-port-3000) ;;Added by Neil B. so that http://<hostname>:8080/soap can redirect to http://<hostname>:3000/soap
                    ("irsb-update-info" gen-info) ;;info panel
                    ("irsb-update-server" gen-server-visualizer))))))

;;; {{{ Include stylesheets and Javascript libraries.
(defun generate-stylesheet-links (args)
  "Generate HTML to include the CSS files ARGS."
  (apply #'concatenate 'string
         (mapcar (lambda (arg)
                   (with-html (:link :rel "stylesheet" :type "text/css"
                                     :href (format nil "irs/assets/css/~a" arg))))
                 args)))

(defun generate-middle-javascript (args)
  "Generate HTML to include the Javascript files ARGS."
  (apply #'concatenate 'string
         (mapcar (lambda (arg)
                   (with-html (:script :type "text/javascript"
                                       :src (format nil "irs/assets/js/~a"  arg))))
                 args)))
;;; }}}

(defun filter-for-ontology (items ontology)
  (mapcan #'(lambda (item)
              (when (eq (ocml::Home-ontology item) ontology)
                (list item)))
          items))

(defun safe-subclass (ontologies supername)
"Takes a list of ontologies and a class- returns all the subclasses of the class within those ontologies only"
(let ((out nil))
  (dolist (ontology ontologies)
    (ocml:with-ontology (ontology)
      (let ((super (ocml::get-ocml-class supername)))
        (if super
            (setf out (append (filter-for-ontology (ocml::current-subclasses super) ontology) out))
          nil))))
  out))

(defun find-right-home-onto (classname bunch-of-ontos)
  "From a classname and a list of ontologies returns the name of the home ontology"
  (let ((ocml-class (ocml::get-ocml-class classname))
        (right-onto nil))
    (dolist (onto bunch-of-ontos)
      (if (filter-for-ontology (list ocml-class) onto)
          (return (setf right-onto (ocml::name onto)))))
    right-onto))

(defun what-wsmo-type? (ocml-class ontoname)
  (let ((ocml-onto (ocml::get-ontology ontoname)))
    (cond ((member ocml-class (mapcar #'ocml::name (safe-subclass (list ocml-onto) 'ocml::goal)))
           'goal)
          ((member ocml-class (mapcar #'ocml::name (safe-subclass (list ocml-onto) 'ocml::web-service)))
           'web-service)
          ((member ocml-class (mapcar #'ocml::name (safe-subclass (list ocml-onto) 'ocml::mediator)))
           'mediator)
          (t nil))))

(defun gen-mainpage ()
  (web:standard-page "IRS Browser"
    (htm
     ((:div :class "yui-skin-sam" :id "browser-table")
      ((:table :width "100%")
       (:tr ((:td :id  "menubar-hole"))
            ((:td :id "ontology-control-hole")))
       (:tr
        ((:td :width "50%")
         ((:div :id "leftab")
          ((:div :class "yui-navset" :id "leftTab")
           ((:ul :class "yui-nav")
            (:li ((:a :href "#classtree") (:em "Classes")))
	    (:li ((:a :href "#functionlist") (:em "Functions")))
            (:li ((:a :href "#instancelist") (:em "Instances")))
	    (:li ((:a :href "#relationlist") (:em "Relations")))
	    (:li ((:a :href "#rulelist") (:em "Rules")))
            (:li ((:a :href "#classlist") (:em "WSMO Entities"))))
           ((:div :class "yui-content")
            ((:div :id "classtree" :class "tabbycat")
             (:p "Select an ontology first."))
            ((:div :id "functionlist" :class "tabbycat")
             (:p "Select an ontology first."))
            ((:div :id "instancelist" :class "tabbycat")
             (:p "Select an ontology first."))
            ((:div :id "relationlist" :class "tabbycat")
             (:p "Select an ontology first."))
             ((:div :id "rulelist" :class "tabbycat")
             (:p "Select an ontology first."))
            ((:div :id "classlist" :class "tabbycat")
             (:p "Select an ontology first."))))))
        ((:td)
         ((:div :class "yui-navset" :id "rightTab")
          ((:ul :class "yui-nav")
           (:li ((:a :href "#info") (:em "Inspector")))
           (:li ((:a :href "#servervis") (:em "WSMO View")))
           (:li ((:a :href "#repl") (:em "Listener"))))
          ((:div :class "yui-content")
           ((:div :id "info" :class "tabbycat")
            (:p "Nothing has been clicked yet"))
           ((:div :id "servervis" :class "tabbycat")
            (:p "Double click on an entity to activate this view"))
           ((:div :id "repl-tab" :class "tabbycat")
            ((:div :id "repl-input-hole")
             ((:form :id "repl-form")
              "OCML> " ((:input :id "repl-input-area" :name "repl-input"
                                :size 70 :class "codebox"))
              (:br)
              ((:textarea :id "repl-output-area" :class "codebox"
                          :rows 30 :cols 80 :wrap "off" :readonly t))))))))))
       ((:script :type "text/javascript" :src "irs/javascript/ocml-api.js"))
       ((:script :type "text/javascript" :src "irs/javascript/irs-api.js"))
       ((:script :type "text/javascript" :src "irs/javascript/browser.js"))
       ((:script :type "text/javascript")
        "browser = new Browser(); YAHOO.util.Event.onContentReady(\"browser-table\", browser.init);")))))

(defun figure-out-what-to-include (ontoname filters)
  "Outputs a list of ocml internal ontology symbols corresponding to
what can and must be included."
  (let ((imported (not (first filters)))
        (base (not (second filters)))
        (wsmo (not (third filters)))
        (result '()))
    (if (ocml::get-included-ontologies ontoname)
        (cond ((and imported (not base) (not wsmo))  ;; T F F
               (setf result (remove-if #'(lambda (x) (or (equal x (ocml::get-ontology 'ocml::base-ontology)) 
                                                         (equal x (ocml::get-ontology 'ocml::wsmo))))
                                       (ocml::get-included-ontologies ontoname))))
              ((and imported base (not wsmo))  ;; T T F
               (setf result (remove-if #'(lambda (x) (equal x (ocml::get-ontology 'ocml::wsmo)))
                                       (ocml::get-included-ontologies ontoname))))
              ((and imported (not base) wsmo)  ;; T F T
               (setf result (remove-if #'(lambda (x) (equal x (ocml::get-ontology 'ocml::base-ontology)))
                                       (ocml::get-included-ontologies ontoname))))
              ((and (not imported) base wsmo)  ;; F T T
               (setf result (intersection (ocml::get-included-ontologies ontoname)
                                          (list (ocml::get-ontology 'ocml::wsmo)
                                                (ocml::get-ontology 'ocml::base-ontology)))))
              ((and (not imported) base (not wsmo))  ;; F T F
               (setf result (intersection (ocml::get-included-ontologies ontoname)
                                          (list  (ocml::get-ontology 'ocml::base-ontology)))))
              ((and (not imported) (not base) wsmo)  ;; F F T
               (setf result (intersection (ocml::get-included-ontologies ontoname)
                                          (list (ocml::get-ontology 'ocml::wsmo)))))
              ((and imported base wsmo)  ;; T T T
               (setf result (ocml::get-included-ontologies ontoname)))
              (t (setf result nil))))
    result))

;;; {{{ right box

(defun gen-server-visualizer ()
  (web:with-parameters (:get (class onto typeclass))
    (let ((class (intern class :ocml))
          (onto (intern onto :ocml)))
      (gen-server-visualizer-inner onto class typeclass))))

(defun gen-server-visualizer-inner (ontoname classname typeclass)
"Puts the contents of the server visualization into the appropriate div-structure"
  (multiple-value-bind (ontos goals  mediators services publishers groundings class-value)
      (get-visualizer-values-for 
       (find-right-home-onto classname 
                             (append (list (ocml::get-ontology ontoname))
                                     (ocml::get-included-ontologies ontoname)))
       classname typeclass)

    (with-html 
      (:h2 (str (format nil "Focus--> ~a" (api-js::extern-ocml-symbol classname))))
      (:div :id "visbox"
       (:div :class "one"
        (str (format nil "<h3 class='~a'>~a</h3>"
                             (if (equal class-value 1) "active"  "inactive")
                             "Ontologies"))
        (cl-who::str (gen-list ontos ontoname)))
       (:div :class "two"
        (str (format nil "<h3 class='~a'>~a</h3>"
                             (if (equal class-value 2) "active"  "inactive")
                             "Goals"))
        (cl-who::str (gen-list goals ontoname)))
       (:div :class "three"
        (str (format nil "<h3 class='~a'>~a</h3>"
                             (if (equal class-value 3) "active"  "inactive")
                             "Mediators"))
        (cl-who::str (gen-list mediators ontoname)))
       (:div :class "four"
        (str (format nil "<h3 class='~a'>~a</h3>"
                             (if (equal class-value 4) "active"  "inactive")
                             "Web-Services"))
        (cl-who::str (gen-list services ontoname)))
       (:div :class "five"
        (str (format nil "<h3 class='~a'>~a</h3>"
                             (if (equal class-value 5) "active"  "inactive")
                             "WS-Publisher"))
        (cl-who::str (gen-list publishers ontoname)))
       (:div :class "six"
        (str (format nil "<h3 class='~a'>~a</h3>"
                             (if (equal class-value 6) "active"  "inactive")
                             "WS-grounding"))
        (cl-who::str (gen-list groundings ontoname)))))))




;; this applies also to ontologies and ws-publishers/groundings... even if for now double clicking on them doesnt do anything!!
(defun gen-list (items ontoname  &optional (type "unknown"))
  "Just generates a list - the class-type is left unspecified for the moment"
  (let ((result ""))
    (if items
        (dolist (i items)
          (setf result (format nil "~a<li class=\"~a\" ondblclick=\"updateServerVis('~a', '~a', 'CLASS');\">~a</li>" 
                               result type 
                               i ontoname (api-js::extern-ocml-symbol i)))))
    result))

(defun get-visualizer-values-for (ontology classname typeclass)
  "From a wsmo-related class, gets all the related classes so to
visualize them in the browser-table (+ a class-value variable to keep
track of the wsmo-type in focus)"
  ;; XXX the typeclass is not used much for now!
  (declare (ignore typeclass))
  (let ((wsmotype (what-wsmo-type? classname ontology))
        (class-value 0)
        (ontos (list ontology))
        (goals '())
        (mediators '())
        (services '())
        (publishers '())
        (groundings '()))
    (case wsmotype
      (goal ;; +++++++++goals
       (let ((associated-web-services 
              (wsmo-protocol::check-class-and-find-all-web-services-which-solve-goal classname))
             (associated-mediators
              (mapcar #'cdr (wsmo-protocol::find-all-web-services-with-mediators-which-solve-goal classname ontology))))
         (setf goals (append goals (list classname)))
         (setf class-value 2)
         (dolist (l associated-mediators)
           (setf mediators (append mediators (mapcar #'ocml::name l))))
         (setf services (append services (mapcar #'ocml::name associated-web-services)))))
      (mediator ;;+++++++++mediators
       (let ((goal-sources (ocml::find-option-value (ocml::get-ocml-class classname)  'ocml::has-source-component :value))
             (wservice-targets (ocml::find-option-value (ocml::get-ocml-class classname)  'ocml::has-target-component :value)))
         (if goal-sources
             (dolist (g goal-sources)
               (if (equal (what-wsmo-type? g ontology) 'goal)
                   (setf goals (append goals (list g))))))
         (setf mediators (append mediators (list classname)))
         (setf class-value 3)
         (if wservice-targets
             (dolist (w wservice-targets)
               (if (equal (what-wsmo-type? w ontology) 'web-service)
                   (setf services (append services (list w))))))))
      (web-service ;;+++++++++wservices   --> not sure I'm using the right ocml-relations... not enough data to test it!!!
       (let ((possible-goals 
              (ocml::setofall '?x `(ocml::can-solve-goal ?x ,classname))))
         (if possible-goals
             (dolist (g possible-goals)
               (setf goals (append goals (list g))))))
       (setf class-value 4)
       (setf services (append services (list classname)))
       (let ((possible-mediators
              (ocml::setofall '?x `(ocml::wsmo-web-service-used-mediator ,classname ?x))))
         (if possible-mediators
             (dolist (me possible-mediators)
               (setf mediators (append mediators (list me))))))
       (let ((possible-publisher
              (ocml::setofall '?x `(ocml::wsmo-web-service-host ,classname ?x))))
         (if possible-publisher
             (dolist (pu possible-publisher)
               (setf publishers (append publishers (list pu))))))
       (let ((possible-groundings
              (ocml::setofall '?x `(ocml::associated-grounding ,classname ?x))))
         (if possible-groundings
             (dolist (gr possible-groundings)
               (setf groundings (append groundings (list gr))))))))
    (values ontos goals  mediators services publishers groundings class-value )))

;;; }}}

;;;{{{ Added by Neil B. 14/Oct/2008 - New IRS Ontology Editor
(defun gen-irseditor ()
  "The start screen for the IRS Editor"
  ;; So that /irs/editor?ontology=ONT_NAME launches the browser with
  ;; ONT_NAME open in a window.
  (setf ontoname (hunchentoot:get-parameter "ontology"))
  (if (eq nil ontoname)
      (setf ontoname ""))
	(setf (hunchentoot:content-type*) "text/html;charset=UTF-8")
	(with-html-top
		(:html
			(:head (:title "IRS Ontology Editor")
			       ((:base :id "baseid" :href (irs.web:base-href)))
				((:link :rel "stylesheet" :type "text/css"
					:href (str (yui-file-url "build/assets/skins/sam/skin.css"))))
				((:link :rel "stylesheet" :type "text/css"
					:href (str (yui-file-url "build/fonts/fonts-min.css"))))
				((:link :rel "stylesheet" :type "text/css"
					:href (str (yui-file-url "build/editor/assets/skins/sam/simpleeditor.css"))))
				((:link :rel "stylesheet" :type "text/css"
					:href "irs/assets/css/stylesheet.css"))
				((:link :rel "stylesheet" :type "text/css"
					:href "irs/assets/css/browser.css"))
				((:link :rel "stylesheet" :type "text/css"
					:href "irs/assets/css/editor-styles.css"))
				((:link :rel "icon" :type "image/png"
					:href "irs/assets/images/irs3-favicon.ico"))
				((:script :type "text/javascript" :src (str (yui-file-url "build/utilities/utilities.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/container/container-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/menu/menu-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/button/button-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/cookie/cookie-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/datasource/datasource-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/datatable/datatable-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/editor/editor-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/history/history-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/imageloader/imageloader-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/json/json-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/resize/resize-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/selector/selector-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/layout/layout-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/tabview/tabview-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/treeview/treeview-min.js"))))
				((:script :type "text/javascript" :src (str (yui-file-url "build/uploader/uploader-experimental.js")))))
                                ((:script :type "text/javascript" :src "irs/javascript/ocml-api.js"))
                                ((:script :type "text/javascript" :src "irs/javascript/irs-api.js"))
                                ((:script :type "text/javascript" :src "irs/javascript/browser.js"))
                                ((:script :type "text/javascript" :src "irs/javascript/editor-dialogs.js"))
                                ((:script :type "text/javascript" :src "irs/javascript/editor-ontologywindow.js"))
                                ((:script :type "text/javascript" :src "irs/javascript/editor-renderer.js"))
                                ((:script :type "text/javascript" :src "irs/javascript/editor-menubar.js"))
                                ((:script :type "text/javascript" :src "irs/javascript/editor.js"))
			)
		
			(:body :class "yui-skin-sam"
  
				((:div :id "menubarhole"))
				((:div :style "display:table; width: 100%")
					((:div :style "display:table-row; height: 50px"))
					((:div :style "display:table-row;")
						((:div :style "display:table-cell;width:33%"))
						((:div :style "display:table-cell;width:33%;text-align: center")
							"Welcome to the IRS Ontology Editor")
						((:div :style "display:table-cell;width:33%"))
					)
					((:div :style "display:table-row;")
						((:div :style "display:table-cell;width:33%"))
						((:div :style "display:table-cell;width:33%;text-align: center") 
							((:img :src "irs/assets/images/irs3-logo.png")))
						((:div :style "display:table-cell;width:33%"))
					)
				)
                                ((:script :type "text/javascript") (str (format nil "FULL_EDITOR_MODE = true;  run(\"~a\");" ontoname)))
                         )))
      



;;;}}}
;;;{{{Added by Neil B.
(defun redirect-to-port-3000 ()
  "This function is a workaround for the problem that XMLHTTPRequests don't allow cross-domain (or even same-domain but cross-port) requests"

  ;Get the raw post data from the soap request
  (setf post-data (hunchentoot::raw-post-data))  
  
  ;Use the DRAKMA package to forward the SOAP request to the soap handler listing on port 3000 and write the result back to the client
  (web:write-http-stream 
   "text/xml"
   (drakma:http-request "http://127.0.0.1:3000/soap"
                          :method :post
                          :content-type "text/xml"
                          :external-format-out :utf-8
                          :content post-data))
  
)
;;;}}}

