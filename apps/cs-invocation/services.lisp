(in-package :cs-invocation)

(defmethod initialise-application ((app (eql :cs-invocation)))
  (webonto:require-ontologies '(:wsmo :conceptual-spaces :cs-goals :booking-request)) ;; add here all ontologies storing goal descriptions to be achieved
  (start-web-interface))

(defmethod start-application ((app (eql :cs-invocation)))
  'no-operation)

;;;;;;; WEB


(defun start-web-interface ()
  (web:register-plugin
   :cs-invocation :application
   "cs-invocation"
   (nconc
    (mapcar (lambda (args)
	      (apply #'hunchentoot:create-regex-dispatcher args))
	    `(("/cs-invocation$" draw-top-page)
              ("/cs-invocation/achieve-cs-goal$" achieve-cs-goal) 
           ;;   ("/cs-invocation/achieve-cs-goal3$" achieve-cs-goal3)
              ("/cs-invocation/achieve-cs-goal-web$" achieve-cs-goal-web)
           ;;   ("/cs-invocation/achieve-cs-goal-irs$" achieve-cs-goal-irs)
           ;;   ("/cs-invocation/achieve-cs-goal-irs-web$" achieve-cs-goal-irs-web)
              ("/cs-invocation/achieve-cs-goal2$" achieve-cs-goal2)
              ("/cs-invocation/cs-invoke-ws$" cs-invoke-ws)
              ("/cs-invocation/cs-invoke-ws-irs$" cs-invoke-ws-irs)
              ("/cs-invocation/web"
               ,(lambda () (web:send-url/file "/cs-invocation/web"
                                         "irs:apps;cs-invocation;web"))))))))

(defun draw-top-page ()
  (web:standard-page "CS Goal Invocation"
    (:p "CS Goal Invocation")))



;;;;;; functions

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

;;


(defun achieve-cs-goal-old () 
;;; the one which directly invokes the selected WS
;;; takes inputs from assumption, i.e. these would need to be read the traditional way
;;; does handle the assumption as assumption and sets of inputs.
  (web:with-html
    (str
     (web:with-parameters (:get (ontology goal assumption))
       (ocml::with-ontology ((ocml::get-ontology ontology))
         (let ((inputs (read-from-string assumption)))
           (read-from-string
            (with-output-to-string (ostream)
              (ip::cs-irs-achieve-goal (http::make-ocml-symbol ontology)
                                        (http::make-ocml-symbol goal)
                                        inputs
                                        ostream nil))))))     
     )))

;;

(defun achieve-cs-goal2 () 
  (web:with-html
    (str     
     (web:with-parameters (:get (ontology goal assumption))
       (let* ((o (http::make-ocml-symbol ontology))
              (g (http::make-ocml-symbol goal)))
         (ocml::with-ontology ((ocml::get-ontology o))
           (let* ((a (instanciate o (read-from-string assumption)))
                  (roles (wp::input-roles g)))
             (let ((roles+values (mapcar #'(lambda (slot)
                                             (list slot
                                                   (get-value-from-assumption o a g slot)))
                                         roles)))
               (with-output-to-string (ostream)
                 (setf r (ip::cs-irs-select-ws (http::make-ocml-symbol ontology)
                                               (http::make-ocml-symbol goal)
                                               a
                                               roles+values
                                               ostream nil)))  
               r))))))))

;;

(defun achieve-cs-goal ()
  (let ((res nil)
        (rol nil)
        (wslist nil)
        (wselement nil)
        (selectedws nil)
        (simvalue nil)
        (roles nil)
        (roles+values nil)
        (highest-value 0) 
        (pos 0))
  (web:with-parameters (:get (ontology goal assumption))
    (let* ((o (http::make-ocml-symbol ontology))
           (g (http::make-ocml-symbol goal)))
               
      (ocml:with-ontology ((ocml::get-ontology o))
        (let* ((a (instanciate o (read-from-string assumption)))
                    (roles (wp::input-roles g)))
          (let ((roles+values (mapcar #'(lambda (slot)
                                          (list slot
                                                (let ((*package* (find-package :ocml)))
                                                  (hunchentoot:get-parameter (string-downcase (symbol-name slot))))))
                                      roles)))
              ;; list of input roles and associated inputs (no input = :nothing)
              (setf rol roles+values)
              ;; get all possible WSs and their similarities (first element of the result) 
              ;; the second element of the result is the list of all instances of WSs associated with the goal (this info is needed for the ws invocation)   
              (with-output-to-string (ostream)
                (setf res (ip::cs-irs-select-ws (http::make-ocml-symbol ontology)
                                                (http::make-ocml-symbol goal)
                                                a
                                                roles+values
                                                ostream nil)))      
             (setf wsinstances (cdr res))
                (setf wslist (car res))
            ;; wslist jetzt: "((GET3-UK-BOOKING-REQUEST-WS 1000000.0) (GET3-FRANCE-BOOKING-REQUEST-WS 0.09298831))"
            ;; (car res) sollte die liste aller ws-instances sein
            ;; die muesste durchgegangen werden, jeweils muesste der value genommen werden und falls der vorgaenger groesser ist, wird selectedws auf 
            ;; als identifikator fuer den value koennte man von jedem element ab dem ersten blank bis zum ende alles extrahieren.     
      (setf highest-value 0.0)
      (setf selectedws " ")  
         (loop for j in wslist do   
            (setf wselement j)
            ;;(setf pos (search " " wselement))    
            ;;(setf simvalue (subseq wselement pos))
            (setf simvalue (second wselement))              
            (if (equalp simvalue "Infinity")
            (setf simvalue 999999.99))
            (if (> simvalue highest-value)
            (let () 
              (setf selectedws wselement)
              (setf highest-value simvalue)))
            ;; aktuellen gesetzt, e.g. selectedws="(GET3-FRANCE-BOOKING-REQUEST-WS 0.09298831)"
            ) ;;; end loop
            ) ;; end second let

          ;;; funktioniert, aber cs-invoke-ws macht noch probleme!
          ;;; a new cs-invoke-ws is needed which gets the variables not from the web-form but either as parameters or as global variables!
          (cs-invoke-ws-irs ontology goal rol wsinstances selectedws)
          ))))))        

;;; following cs-invoke-ws meant to be used when ws is being directly invoked, without intermediate web-forms, as through achieve-cs-goal-irs

(defun cs-invoke-ws-irs (ontology goal rol wsinstances selectedws)
  (web:with-html
    (str
      ;;; function invokes "selectedws" after being automatically selected by "cs-achieve-goal-irs" 
      (let ((o (http::make-ocml-symbol ontology))
             (g (http::make-ocml-symbol goal))
             (inputs nil)
             (instances nil)
             (ws nil)
             (result nil))
         (ocml::with-ontology ((ocml::get-ontology o))
        
           ;; ws inputs
           (loop for i in rol do
                 (setf inputs (append inputs (list  (second i)))))  ;; get-instance-from-string ????
           ;;; inputs are not set properly! (i.e. are empty!) because there are no parameters set in the webform!
           ;;; either take the roles&values here or simply create a list with the input roles!


           ;; ws instances
         ;;  (setf instances (first (read-from-string (replace-all wsinstances "+" " "))))
          (setf instances (first wsinstances))
  
           ;; ws to invoke
           (setf ws (http::make-ocml-symbol (first selectedws)))

           ;; invoke service
           (loop for i in instances do
                 (if (equalp (ocml::the-parent-class (http::make-ocml-symbol (second i))) ws)
                             (setf ws (list (http::make-ocml-symbol (first i))  
                                            (http::make-ocml-symbol (second i))
                                            (http::make-ocml-symbol (third i))))))
           (setf result (ip::cs-irs-invoke-ws o g ws inputs))
           
           )))))

(defun get-instance-from-string (ontology string)
  (ocml::with-ontology ((ocml::get-ontology (http::make-ocml-symbol ontology)))
    (let ((r nil))
      (setf s (replace-all string "\"" ""))
      (setf r (ocml::findany '?x `(ocml::has-instance-name ?x ,s)))
      (if (equalp r :nothing)
          (setf r string))
      r)))

(defun instanciate (ont list)
  (ocml::with-ontology ((ocml::get-ontology ont))
    (let ((l nil))
      (loop for i in list do
            (setf l (append l (list (get-instance-from-string ont i)))))
      l)))

(defun get-value-from-assumption (o a g s)
  (ocml::with-ontology ((ocml::get-ontology o))
    (let ((v nil)
          (c (first (ocml::get-slot-type (ocml::get-domain-class g) (http::make-ocml-symbol s)))))
      (ocml::findany '?x `(and (class ,c) 
                               (,c ?x)
                               (ocml::member ?x ,a)))
      
      )))

(defun achieve-cs-goal-web ()
  (let ((res nil)
        (rol nil))
  (web:with-parameters (:get (ontology goal assumption))
    (let* ((o (http::make-ocml-symbol ontology))
           (g (http::make-ocml-symbol goal)))
               
      (ocml:with-ontology ((ocml::get-ontology o))
        (let* ((a (instanciate o (read-from-string assumption)))
                    (roles (wp::input-roles g)))
          (let ((roles+values (mapcar #'(lambda (slot)
                                          (list slot
                                                (let ((*package* (find-package :ocml)))
                                                  (hunchentoot:get-parameter (string-downcase (symbol-name slot))))))
                                      roles)))

              ;; list of input roles and associated inputs (no input = :nothing)
              (setf rol roles+values)

              ;; get all possible WSs and their similarities (first element of the result) 
              ;; the second element of the result is the list of all instances of WSs associated with the goal (this info is needed for the ws invocation)   
              (with-output-to-string (ostream)
                (setf res (ip::cs-irs-select-ws (http::make-ocml-symbol ontology)
                                                (http::make-ocml-symbol goal)
                                                a
                                                roles+values
                                                ostream nil))))
            ;; show the results in a web page            
            (web:with-html
              (:html
               (:head (:title "Select WS"))
               ((:link :href "stylesheet.css" :rel "stylesheet" :type "text/css")))
              (:body 
               (:center (:h2 "WS Invocation"))           
               ((:form :method "GET" :action "cs-invoke-ws")
                ((:table :width 600 :border 1 :align "center" :cellpadding 0
                  :cellspacing 0 :bordercolor "7ca5ce" :bgcolor "e5edf5" )
                 (fmt "<input type=\"hidden\" name=\"ontology\" value=\"~A\">" o) ;; ontology is needed in the next stage (ws invocation)
                 (fmt "<input type=\"hidden\" name=\"goal\" value=\"~A\">" g) ;; goal-type is needed in the next stage
                 (fmt "<input type=\"hidden\" name=\"roles\" value=\"~A\">" roles) ;; input roles are needed in the next stage
                 (fmt "<input type=\"hidden\" name=\"wsinstances\" value=\"~A\">" (cdr res)) ;;list of WSs instances 
                 (:tr 
                  ((:td :align "center")
                   ((:table :width 600 :border 0 :align "center" :cellpadding 0 :cellspacing 0)
                    (:tr 
                     (:td (:br))
                     (:td "&nbsp;"))                  
                    (:tr 
                     ((:td :valign "top" :class "ws-options")
                      ((:div :align "right") "Available Web Services:"))
                     ((:td :valign "bottom")
                      "&nbsp;&nbsp;"
                      ((:select :name "selectedws")                            
                       (dolist (wss (car res)) (fmt "<option>~A</option>" wss))) ;; get the WS to invoke
                      ))                    
                    ;; get inputs for all goal input roles
                    (dolist (r rol)
                      ;;; probably the following has to be changed in order to ha
                      ;;; (setf j (http::make-ocml-symbol (second r)))
                      (fmt "<tr><td><br/></td><td>&nbsp;</td></tr>
                              <tr>
                                 <td valign=\"top\" class=\"vtaoptions\">
                                    <div align=\"right\">~A</div> 
                                 </td>
                                 <td valign=\"bottom\">
                                    &nbsp;&nbsp;
                                    <input name=\"~A\" type=\"text\" value=\"~A\" /> 
                                 </td>
                              <tr/>" 
                           (first r) ;;; assigning first value for ~A
                           (first r) ;;; assigning second value for ~A
                           (second r) ;;; part needs to be tested
                           ))          
                    (:tr 
                     ((:td :valign "bottom" :class "vtaoptions") "&nbsp;")
                     ((:td :valign "bottom") 
                      (:br) 
                      (:br)
                      ((:input :type "image" :src "web/submit.png" :alt "Invoke WS" :name "submit" :border 0))))
                    ))))))
     )
  ))))))        

(defun achieve-cs-goal3 () 
  (let ((res nil)
        (rol nil))
    (web:with-parameters (:get (ontology goal assumption))
      (let* ((o (http::make-ocml-symbol ontology))
             (g (http::make-ocml-symbol goal)))
        (ocml::with-ontology ((ocml::get-ontology o))
          (let* ((a (instanciate o (read-from-string assumption)))
                    (roles (wp::input-roles g)))
            (let ((roles+values (mapcar #'(lambda (slot)
                                            (list slot
                                                  ;; get a possible input from the current assumptions for each input role
                                                  (get-value-from-assumption o a g slot)))
                                        roles)))

              ;; list of input roles and associated inputs (no input = :nothing)
              (setf rol roles+values)

              ;; get all possible WSs and their similarities (first element of the result) 
              ;; the second element of the result is the list of all instances of WSs associated with the goal (this info is needed for the ws invocation)   
              (with-output-to-string (ostream)
                (setf res (ip::cs-irs-select-ws (http::make-ocml-symbol ontology)
                                                (http::make-ocml-symbol goal)
                                                a
                                                roles+values
                                                ostream nil))))
            ;; show the results in a web page
            (web:with-html
              (:html
               (:head (:title "Select WS"))
               ((:link :href "stylesheet.css" :rel "stylesheet" :type "text/css")))
              (:body 
               (:center (:h2 "WS Invocation"))           
               ((:form :method "GET" :action "cs-invoke-ws")
                ((:table :width 600 :border 1 :align "center" :cellpadding 0
                  :cellspacing 0 :bordercolor "7ca5ce" :bgcolor "e5edf5" )
                 (fmt "<input type=\"hidden\" name=\"ontology\" value=\"~A\">" o) ;; ontology is needed in the next stage (ws invocation)
                 (fmt "<input type=\"hidden\" name=\"goal\" value=\"~A\">" g) ;; goal-type is needed in the next stage
                 (fmt "<input type=\"hidden\" name=\"roles\" value=\"~A\">" roles) ;; input roles are needed in the next stage
                 (fmt "<input type=\"hidden\" name=\"wsinstances\" value=\"~A\">" (cdr res)) ;;list of WSs instances 
                 (:tr 
                  ((:td :align "center")
                   ((:table :width 600 :border 0 :align "center" :cellpadding 0 :cellspacing 0)
                    (:tr 
                     (:td (:br))
                     (:td "&nbsp;"))                  
                    (:tr 
                     ((:td :valign "top" :class "vtaoptions")
                      ((:div :align "right") "Available Web Services:"))
                     ((:td :valign "bottom")
                      "&nbsp;&nbsp;"
                      ((:select :name "selectedws")                            
                       (dolist (wss (car res)) (fmt "<option>~A</option>" wss))) ;; get the WS to invoke
                      ))                    
                    ;; get inputs for all goal input roles
                    (dolist (r rol)
                      (setf j (http::make-ocml-symbol (second r)))
                      (fmt "<tr><td><br/></td><td>&nbsp;</td></tr>
                              <tr>
                                 <td valign=\"top\" class=\"vtaoptions\">
                                    <div align=\"right\">~A</div> 
                                 </td>
                                 <td valign=\"bottom\">
                                    &nbsp;&nbsp;
                                    <input name=\"~A\" type=\"text\" value=\"~A\" /> 
                                 </td>
                              <tr/>" 
                           (first r) 
                           (first r) 
                           (ocml::findany '?x `(and (ocml::refined-by ,j ?y) 
                                                    (ocml::has-title ?y ?x)))))          
                    (:tr 
                     ((:td :valign "bottom" :class "vtaoptions") "&nbsp;")
                     ((:td :valign "bottom") 
                      (:br) 
                      (:br)
                      ((:input :type "image" :src "web/submit.png" :alt "Invoke WS" :name "submit" :border 0))))
                    ))))))
    
              )))))))

(defun cs-invoke-ws ()
  (web:with-html
    (str
     (web:with-parameters (:get (ontology goal roles wsinstances selectedws))
       (let ((o (http::make-ocml-symbol ontology))
             (g (http::make-ocml-symbol goal))
             (inputs nil)
             (instaces nil)
             (ws nil)
             (result nil))
         (ocml::with-ontology ((ocml::get-ontology o))
        
           ;; ws inputs
           (loop for i in (read-from-string roles) do
                 (setf inputs (append inputs (list (hunchentoot:get-parameter (symbol-name i))))))  ;; get-instance-from-string ????

           ;; ws instances
           (setf instances (first (read-from-string (replace-all wsinstances "+" " "))))
           
           ;; ws to invoke
           (setf ws (http::make-ocml-symbol (first (read-from-string selectedws))))

           ;; invoke service
           (loop for i in instances do
                 (if (equalp (ocml::the-parent-class (http::make-ocml-symbol (second i))) ws)
                             (setf ws (list (http::make-ocml-symbol (first i))  
                                            (http::make-ocml-symbol (second i))
                                            (http::make-ocml-symbol (third i))))))
           (setf result (ip::cs-irs-invoke-ws o g ws inputs))
           
           ))))))


;http://localhost:8080/cs-invocation/cs-invoke-ws?ontology=BOOKING-REQUEST&inputs=((HAS-METHOD+NOTHING)+(HAS-DEPARTURE-CITY+NOTHING)+(HAS-DEPARTURE-COUNTRY+DEPARTING-UK)+(HAS-ARRIVAL-CITY+NOTHING)+(HAS-ARRIVAL-COUNTRY+ARRIVING-GERMANY)+(HAS-OUTBOUND-DATE+NOTHING)+(HAS-RETURN-DATE+NOTHING))&invocable=(((INSTANCE654+GET3-UK-BOOKING-REQUEST-WS655+BOOKING-REQUEST)+(INSTANCE660+GET3-FRANCE-BOOKING-REQUEST-WS661+BOOKING-REQUEST)))&WS=(GET3-UK-BOOKING-REQUEST-WS+1.0)&HAS-METHOD=NOTHING&HAS-DEPARTURE-CITY=NOTHING&HAS-DEPARTURE-COUNTRY=Location-UK&HAS-ARRIVAL-CITY=NOTHING&HAS-ARRIVAL-COUNTRY=Location-Germany&HAS-OUTBOUND-DATE=NOTHING&HAS-RETURN-DATE=NOTHING&submit.x=57&submit.y=15

#|

          (let* ((roles (wp::input-roles g)))
            (let ((roles+values (mapcar #'(lambda (slot)
                                             (list slot
                                                   (let ((*package* (find-package :ocml))
                                                         (s nil))
                                                     (http::make-ocml-symbol (get-instance-from-string o (hunchentoot:get-parameter (string-downcase (symbol-name slot)))))
                                                     )))
                                        
                                        roles)))
              (print roles+values)))))))))
  






    ;; to get all parameters from the http request: problems with strings and ocml instances
    (web:with-parameters (:get (ontology goal))
      (let* ((o (http::make-ocml-symbol ontology))
             (g (http::make-ocml-symbol goal)))
        (ocml::with-ontology ((ocml::get-ontology o))
          (let* ((roles (wp::input-roles g)))
            (let ((roles+values (mapcar #'(lambda (slot)
                                             (list slot
                                                   (let ((*package* (find-package :ocml))
                                                         (s nil))
                                                     (http::make-ocml-symbol (get-instance-from-string o (hunchentoot:get-parameter (string-downcase (symbol-name slot)))))
                                                     )))
                                        
                                        roles)))
              (print roles+values)))))))))


|#






