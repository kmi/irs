(in-package :sepc-to-bpmo)

(defmethod initialise-application ((app (eql :sepc-to-bpmo)))
  (webonto:require-ontologies '(:sepc2bpmo :sEPC-e1 :sEPC-e2))
  (start-web-interface))

(defmethod start-application ((app (eql :sepc-to-bpmo)))
  'no-operation)

;;;;;;; WEB


(defun start-web-interface ()
  (web:register-plugin
   :sepc-to-bpmo :application
   "sEPC to BPMO Translator"
   (nconc
    (mapcar (lambda (args)
	      (apply #'hunchentoot:create-regex-dispatcher args))
	    `(("/sepc-to-bpmo$" draw-top-page)
              ("/sepc-to-bpmo/translation-sepc$" draw-translation-sepc-page)
              ("/sepc-to-bpmo/translation-bpmo$" draw-translation-bpmo-page)
              ("/sepc-to-bpmo/translation-response$" draw-translation-response-page)
              ("/sepc-to-bpmo/web"
               ,(lambda () (web:send-url/file "/sepc-to-bpmo/web"
                                         "irs:apps;sepc-to-bpmo;web"))))))))

(defun draw-top-page ()
  (web:standard-page "sEPC to BPMO Translator"
    (:p "The sEPC to BPMO Translator")
    (:ul (:li ((:a :href "/sepc-to-bpmo/translation-sepc")
	       "Start")))))

(defun get-sepc-ontologies (ontology-list)
  (let ((r nil))
    (loop for i in ontology-list do
          (ocml:with-ontology ((ocml::get-ontology (intern (string-upcase (first i)) :ocml)))
            (let ()
              (setf p (ocml::findany '?x `(OCML::|http://www.kmi.open.ac.uk/sEPC#BusinessProcess| ?x)))
              (if (not (equalp p :nothing))
                  (setf r (append r (list (string-upcase (first i))))))
              )))
    r))

(defun draw-translation-sepc-page ()
  (web:with-html-top
    (str (let ()            
           (web:with-html
             (:html
              (:head (:title "Select sEPC Ontology"))
              ((:link :href "stylesheet.css" :rel "stylesheet" :type "text/css")))
             (:body 
              (:center (:h2 "sEPC to BPMO Translator"))

              ((:form :method "GET" :action "translation-bpmo")
               ((:table :width 480 :border 1 :align "center" :cellpadding 0
                 :cellspacing 0 :bordercolor "7ca5ce" :bgcolor "e5edf5" )
                (:tr ((:td :align "center")
                      ((:table :width 366 :border 0 :align "center"
                        :cellpadding 0 :cellspacing 0)

                       (:tr (:td (:br))
                        (:td "&nbsp;"))


                       (:tr ((:td :valign "top" :class "vtaoptions")
                             ((:div :align "right") "sEPC Ontology:"))
                        ((:td :valign "bottom")
                         "&nbsp;&nbsp;"
                         ((:select :name "sepc")                            
                            (dolist (ontologies (get-sepc-ontologies ocml::*all-ontologies*))
                              (fmt "<option>~A</option>" ontologies)
                              
                              )                                
                            ) 
                         ))

                       (:tr ((:td :valign "bottom" :class "vtaoptions") "&nbsp;")
                        ((:td :valign "bottom") (:br) (:br)
                         ((:input :type "image" :src "web/submit.png"
                           :alt "Select Ontology" :name "submit" :border 0))))
                       )))))
              
              )
             )
           ))))


(defun draw-translation-bpmo-page ()
  (web:with-html-top
    (str (web:with-parameters (:get (sepc))
           (let ()
             (ocml:with-ontology ((ocml::get-ontology (intern (string-upcase sepc) :ocml)))
               (web:with-html
                 (:html
                  (:head (:title "Select sEPC Process and Target Ontology"))
                  ((:link :href "stylesheet.css" :rel "stylesheet" :type "text/css")))
                 (:body 
                  (:center (:h2 "sEPC to BPMO Translator"))
                                
                ((:form :method "GET" :action "translation-response")
                 ((:table :width 480 :border 1 :align "center" :cellpadding 0
                   :cellspacing 0 :bordercolor "7ca5ce" :bgcolor "e5edf5" )
                  (:tr ((:td :align "center")
                        ((:table :width 366 :border 0 :align "center"
                          :cellpadding 0 :cellspacing 0)
                         
                         (:tr (:td (:br))
                          (:td "&nbsp;"))

                         (:tr ((:td :valign "top" :class "vtaoptions")
                               ((:div :align "right") "Selected sEPC Ontology:"))
                          ((:td :valign "bottom")
                           "&nbsp;&nbsp;"
                           ((:input :type "text" :name "sepc" :size 15
                             :value sepc))
                           ))
                         
                         (:tr ((:td :valign "bottom" :class "vtaoptions")
                               ((:div :align "right") "Available sEPC Processes:"))
                          ((:td :valign "bottom")
                           (:br) "&nbsp;&nbsp;"
                           ((:select :name "sepcprocess")                            
                            (dolist (process (ocml::setofall '?x `(and (OCML::|http://www.kmi.open.ac.uk/sEPC#BusinessProcess| ?x)  
                                                                       (not (OCML::|http://www.kmi.open.ac.uk/sEPC#representsProcess| ?y ?x)))) ;;; remove subprocesses from the list   
                                             )
                              (fmt "<option>~A</option>" process)
                              ;(fmt "<option>~A</option>" "all") ;;yet to implement in the translator
                              )                                
                            )))
                         
                         (:tr ((:td :valign "bottom" :class "vtaoptions")
                               ((:div :align "right") "Target Ontology Name:"))
                          ((:td :valign "bottom")
                           (:br) "&nbsp;&nbsp;"
                           ((:input :type "text" :name "ontologyname" :size 15
                             :value "output-bpmo"))
                           ))
                         
                         (:tr ((:td :valign "bottom" :class "vtaoptions") "&nbsp;")
                          ((:td :valign "bottom") (:br) (:br)
                           ((:input :type "image" :src "web/submit.png"
                             :alt "Translate" :name "submit" :border 0))))
                         )))))
                          
                  
                )))))
         )))
            
(defun draw-translation-response-page ()
  (web:with-parameters (:get (sepc sepcprocess ontologyname))
    (let ()             
      (web-onto::define-new-ontology *standard-output* ontologyname :domain (list 'ocml::sEPC2BPMO) "alessio" "john")                 
      (ocml:with-ontology ((ocml::get-ontology (intern (string-upcase sepc) :ocml)))
        (setf f (ocml::set-output-file (iu:replace-all "irs:ontologies;domains;XXX;XXX.lisp" "XXX" ontologyname)))
        (setf p (ocml::mapping-businessProcess (ocml::make-ocml-object (intern (string-upcase sepcprocess) :ocml))))
        (setf ps (intern (string-upcase p) :ocml))
        (web-onto::load-new-ontology ontologyname)
        (web:with-html-top
          (str    
           (web:with-html
             (:html
              (:head
               (:title "Translation response")
               ((:link :href "stylesheet.css" :rel "stylesheet" :type "text/css")))
              (:body
               (:center (:h2 "Translation response"))
               (:center (fmt "The BPMO ontology <code>\"~a\"</code> has been created at: <code>~a</code>" ontologyname f))
               (:br)
               (:center (fmt "The resulting BPMO process instance is: <code>~a</code>" p))
               (:br)
               
               (:ul (:li ((:a :href (concatenate 'string (concatenate 'string "http://localhost:3000/irs/" ontologyname) "#"))
                          "Go to the Ontology"))
                (:li ((:a :href "/sepc-to-bpmo")
                      "Home"))
                )
               
               )))
           ))))))


