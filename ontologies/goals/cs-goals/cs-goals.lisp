;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology cs-goals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWS descriptions for similarity calculation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *similarity-value*)

(defvar *similarity-values*)

(defun get-similarity-value (item)
  (setf *similarity-value* (append *similarity-value* (list (dom:node-value (dom:first-child item))))))
  
(defun get-similarities-values (item)
  (let ((r nil)
        (*similarity-value* nil))    
    (dom:map-node-list 'get-similarity-value (dom:child-nodes item))
    (setf *similarity-values* (append *similarity-values* (list *similarity-value*))) 
    )) 

(defun get-best-similarity (string)
  (let ((*similarity-values* nil)
        (best-value 0)
        (best nil))
    (dom:map-node-list 'get-similarities-values (dom:get-elements-by-tag-name 
                                                 (dom:document-element 
                                                  (cxml:parse-rod string (cxml-dom:make-dom-builder))) 
                                                 "similarity"))
    (loop for i in *similarity-values* do
          (if (or (equalp (second i) "Infinity")
                  (> (read-from-string (second i)) best-value))
              (setf best (first i))))
    best
    ))



(defun insert (target list)
  (if (null list)
    (cons target nil)
    (if (>= (second target) (second (first list)))
      (cons target list)
      (cons (first list) (insert target (rest list))))))
 
(defun insertSort (myList)
  (if (null myList)
    nil
    (insert (first myList) (insertSort (rest myList)))))

(defun get-all-similarities (string)
  (let ((*similarity-values* nil)
        (r nil))
    (dom:map-node-list 'get-similarities-values (dom:get-elements-by-tag-name 
                                                 (dom:document-element 
                                                  (cxml:parse-rod string (cxml-dom:make-dom-builder))) 
                                                 "similarity"))
    (loop for i in *similarity-values* do
          (setf v (second i))
          (if (equalp v "Infinity")
              (setf v "999999.99"))
    ;;;      (if (search "E" v)
    ;;;          (setf v "0.0"))
          (setf r (append  r (list (list (read-from-string (first i)) (read-from-string v))))))
    (insertSort r)
    ))
  
;; returns the best simialirity only

;(lift-similarity-result "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soapenv:Body><getSimilaritiesResponse soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><getSimilaritiesReturn xsi:type=\"xsd:string\"> <similarities><similarity><swsid>france-booking</swsid><simvalue>0.20719687173525925</simvalue></similarity><similarity><swsid>uk-booking</swsid><simvalue>0.34245903572457626</simvalue></similarity></similarities></getSimilaritiesReturn></getSimilaritiesResponse></soapenv:Body></soapenv:Envelope>")

(def-class similarity-result ())

(deflift lift-similarity-result similarity-result ()
  (lambda (string)
    (let ()
      (setf string (iu:replace-all (iu:replace-all string "&gt;" ">") "&lt;" "<"))
      (read-from-string (get-best-similarity string)))))
      ;(get-all-similarities string))))
    

(DEF-CLASS similarity-calculation-non-functional-properties (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS similarity-calculation-goal (GOAL) ?GOAL
  ((HAS-INPUT-ROLE :VALUE has-method
                   :VALUE has-cs
                   :VALUE has-base-member
                   :VALUE has-target-member)
   (HAS-INPUT-SOAP-BINDING :VALUE (has-method "string")
                           :VALUE (has-cs "string")
                           :VALUE (has-base-member "string")
                           :VALUE (has-target-member "string"))
   (HAS-OUTPUT-ROLE :VALUE has-similarity-result)
   (HAS-OUTPUT-SOAP-BINDING :VALUE (has-similarity-result "xml"))
   (has-method :TYPE string)
   (has-cs :TYPE string)
   (has-base-member :TYPE string)
   (has-target-member :TYPE string)
   (has-similarity-result :TYPE similarity-result)            
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE similarity-calculation-non-functional-properties)))

(DEF-CLASS similarity-calculation-mediator (WG-MEDIATOR) ?MEDIATOR
  ((HAS-SOURCE-COMPONENT :VALUE similarity-calculation-goal)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE similarity-calculation-non-functional-properties)))
             
(DEF-CLASS similarity-calculation-ws (WEB-SERVICE) ?WEB-SERVICE
  ((HAS-CAPABILITY :VALUE similarity-calculation-capability)
   (HAS-INTERFACE :VALUE similarity-calculation-interface)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE similarity-calculation-non-functional-properties)))
 
(DEF-CLASS similarity-calculation-capability (CAPABILITY) ?CAPABILITY
  ((USED-MEDIATOR :VALUE similarity-calculation-mediator)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE similarity-calculation-non-functional-properties)))

(DEF-CLASS similarity-calculation-interface (INTERFACE) ?INTERFACE
  ((HAS-CHOREOGRAPHY :VALUE similarity-calculation-choreography)
   (HAS-ORCHESTRATION :VALUE similarity-calculation-orchestration)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE similarity-calculation-non-functional-properties)))

(DEF-CLASS similarity-calculation-choreography (CHOREOGRAPHY)
  ((HAS-GROUNDING :VALUE ((GROUNDED-TO-HTTP (NORMAL (NIL ("method" "conceptualspaces" "basemembers" "targetmembers"))))))))
  
(DEF-CLASS similarity-calculation-orchestration-problem-solving-pattern (PROBLEM-SOLVING-PATTERN)
  NIL)

(DEF-CLASS similarity-calculation-orchestration (ORCHESTRATION) 
  ((HAS-PROBLEM-SOLVING-PATTERN :VALUE similarity-calculation-orchestration-problem-solving-pattern)))

(DEF-CLASS similarity-calculation-ws-publisher-information (PUBLISHER-INFORMATION)
  ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE :VALUE similarity-calculation-interface)
   (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
   (HAS-WEB-SERVICE-PORT :VALUE 8080) 
   (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/csDistances.jws")))

;; returns all the similarities

;(lift-all-similarity-results "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soapenv:Body><getSimilaritiesResponse soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><getSimilaritiesReturn xsi:type=\"xsd:string\"> <similarities><similarity><swsid>france-booking</swsid><simvalue>0.20719687173525925</simvalue></similarity><similarity><swsid>uk-booking</swsid><simvalue>0.34245903572457626</simvalue></similarity></similarities></getSimilaritiesReturn></getSimilaritiesResponse></soapenv:Body></soapenv:Envelope>")

(def-class all-similarity-results ())

(deflift lift-all-similarity-results all-similarity-results ()
  (lambda (string)
    (let ()
      (setf string (iu:replace-all (iu:replace-all string "&gt;" ">") "&lt;" "<"))
      (get-all-similarities string))))
    

(DEF-CLASS all-similarities-calculation-non-functional-properties (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS all-similarities-calculation-goal (GOAL) ?GOAL
  ((HAS-INPUT-ROLE :VALUE has-method
                   :VALUE has-cs
                   :VALUE has-base-member
                   :VALUE has-target-member)
   (HAS-INPUT-SOAP-BINDING :VALUE (has-method "string")
                           :VALUE (has-cs "string")
                           :VALUE (has-base-member "string")
                           :VALUE (has-target-member "string"))
   (HAS-OUTPUT-ROLE :VALUE has-similarity-results)
   (HAS-OUTPUT-SOAP-BINDING :VALUE (has-similarity-results "xml"))
   (has-method :TYPE string)
   (has-cs :TYPE string)
   (has-base-member :TYPE string)
   (has-target-member :TYPE string)
   (has-similarity-results :TYPE all-similarity-results)            
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE all-similarities-calculation-non-functional-properties)))

(DEF-CLASS all-similarities-calculation-mediator (WG-MEDIATOR) ?MEDIATOR
  ((HAS-SOURCE-COMPONENT :VALUE all-similarities-calculation-goal)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE all-similarities-calculation-non-functional-properties)))
             
(DEF-CLASS all-similarities-calculation-ws (WEB-SERVICE) ?WEB-SERVICE
  ((HAS-CAPABILITY :VALUE all-similarities-calculation-capability)
   (HAS-INTERFACE :VALUE all-similarities-calculation-interface)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE all-similarities-calculation-non-functional-properties)))
 
(DEF-CLASS all-similarities-calculation-capability (CAPABILITY) ?CAPABILITY
  ((USED-MEDIATOR :VALUE all-similarities-calculation-mediator)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE all-similarities-calculation-non-functional-properties)))

(DEF-CLASS all-similarities-calculation-interface (INTERFACE) ?INTERFACE
  ((HAS-CHOREOGRAPHY :VALUE all-similarities-calculation-choreography)
   (HAS-ORCHESTRATION :VALUE all-similarities-calculation-orchestration)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE all-similarities-calculation-non-functional-properties)))

(DEF-CLASS all-similarities-calculation-choreography (CHOREOGRAPHY)
  ((HAS-GROUNDING :VALUE ((GROUNDED-TO-HTTP (NORMAL (NIL ("method" "conceptualspaces" "basemembers" "targetmembers"))))))))
  
(DEF-CLASS all-similarities-calculation-orchestration-problem-solving-pattern (PROBLEM-SOLVING-PATTERN)
  NIL)

(DEF-CLASS all-similarities-calculation-orchestration (ORCHESTRATION) 
  ((HAS-PROBLEM-SOLVING-PATTERN :VALUE all-similarities-calculation-orchestration-problem-solving-pattern)))

(DEF-CLASS all-similarities-calculation-ws-publisher-information (PUBLISHER-INFORMATION)
  ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE :VALUE all-similarities-calculation-interface)
   (HAS-WEB-SERVICE-HOST :VALUE "luisa.open.ac.uk")
   (HAS-WEB-SERVICE-PORT :VALUE 8080) 
   (HAS-WEB-SERVICE-LOCATION :VALUE "/axis/csDistances.jws")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Simple Distance Calculation Service ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; stefan: I suggest to integrate the old distance calculation goal here.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; THIS PART HAS BEEN MOVED WITHIN WSMO-PROTOCOL (cs-goal-invocation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

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

(defun get-distance-inputs-internal (ontology goal)
  (ocml:with-ontology ((ocml::get-ontology ontology))
    (let ((g nil)
          (assum nil)
          (roles nil)
          (spaces nil)
          (webservices nil)
          (conceptualspaces nil)
          (basemembers nil)
          (targetmembers nil)
          (result nil)
          (string ""))
      (setf g (ocml::findany '?x `(and (,goal ?x)
                                       (not (ocml::has-effect ?x ?y))))) 
      (if (not (equalp g :nothing))
          (let ()
            ;;dealing with the goal side
            (setf assum (ocml::setofall '?x `(and (ocml::refined-by ?x ?m)   ;; change the way to get the assumptions from the goal
                                                  (OCML::|http://www.kmi.open.ac.uk/css#location-member| ?m)
                                                  (ocml::has-input-role ,g ?r)
                                                  (= (ocml::the-slot-value ,g ?r)
                                                     ?x)
                                                  )))
            (loop for i in assum do
                  (let ((space nil)
                        (member nil)
                        (vals nil))
                    (setf member (ocml::the-slot-value i 'ocml::refined-by))
                    (setf space (ocml::the-slot-value member 'ocml::member-in))
                    
                    ;involved roles
                    (setf roles (append roles (ocml::setofall '?r `(and (ocml::has-input-role ,g ?r)
                                                                        (= (ocml::the-slot-value ,g ?r)
                                                                           ,i)))))
                    ;involved cs
                    (if (not (member space spaces))
                        (let ((prominences nil))
                          (setf spaces (append spaces (list space)))
                          (loop for j in (ocml::the-slot-value member 'ocml::has-valued-dimension) do
                                (setf prominences (append prominences (ocml::setofall '?x `(and (ocml::values ,j ?d)  
                                                                                                (ocml::has-prominence ?d ?x))))))
                          (setf conceptualspaces (append conceptualspaces (list (list space prominences))))
                          ))
                  
                    ;base members
                    (loop for j in (ocml::the-slot-value member 'ocml::has-valued-dimension) do
                          (setf vals (append vals (list (ocml::the-slot-value j 'ocml::has-value)))))
                    (setf basemembers (append basemembers (list (list member space vals)))) 
                    ))

            ;;dealing with the ws side
            (setf webservices (ocml::setofall '?w `(and (ocml::class ?m)
                                                        (= (ocml::the-class-slot-value ?m 'ocml::has-source-component)
                                                           ,goal) 
                                                        (ocml::class ?c)
                                                        (= (ocml::the-class-slot-value ?c 'ocml::used-mediator)
                                                           ?m)
                                                        (ocml::class ?w)
                                                        (= (ocml::the-class-slot-value ?w 'ocml::has-capability)
                                                           ?c))))
            (loop for i in webservices do
                  (let ((targetmember nil))
                    (setf assum (first (ocml::all-class-slot-local-values (first (ocml::all-class-slot-local-values i 'ocml::has-capability)) 'ocml::has-assumption)))
                    
                    ;; improve here the interpretation of the assumption; at the moment it's in the form (OR (A B C)) and I deal with the list of assumptions
                    (loop for j in (second assum) do
                          (let ((space nil)
                                (member nil)
                                (vals nil))
                            (setf member (ocml::the-slot-value j 'ocml::refined-by))
                            (setf space (ocml::the-slot-value member 'ocml::member-in))
                            (loop for z in (ocml::the-slot-value member 'ocml::has-valued-dimension) do
                                  (setf vals (append vals (list (ocml::the-slot-value z 'ocml::has-value)))))
                            (setf targetmember (append targetmember (list (list space vals))))
                            ))
                    (setf targetmembers (append targetmembers (list (list i targetmember))))
                    ))
            ))           

    ;(print conceptualspaces)
      (setf string "(spaces)")
      (loop for i in conceptualspaces do
            (setf string (concatenate 'string string "(cs)"))
            (setf string (concatenate 'string string "(csid)"))
            (setf string (concatenate 'string string (format nil "~s" (first i))))
            (setf string (concatenate 'string string "(/csid)"))
            (setf string (concatenate 'string string "(prominences)"))
            (loop for j in (second i) do
                  (setf string (concatenate 'string string (format nil "~s;" j))))
            (setf string (string-right-trim ";" string))
            (setf string (concatenate 'string string "(/prominences)"))          
            (setf string (concatenate 'string string "(/cs)")))
      (setf string (concatenate 'string string "(/spaces)"))
      (setf result (append result (list (replace-all string "#_css:" ""))))

    ;(print basemembers)
      (setf string "(members)")
      (loop for i in basemembers do
            (setf string (concatenate 'string string "(member)"))
            (setf string (concatenate 'string string "(csid)"))
            (setf string (concatenate 'string string (format nil "~s" (second i))))
            (setf string (concatenate 'string string "(/csid)"))
            (setf string (concatenate 'string string "(values)"))
            (loop for j in (third i) do
                  (setf string (concatenate 'string string (format nil "~s;" j))))
            (setf string (string-right-trim ";" string))
            (setf string (concatenate 'string string "(/values)"))          
            (setf string (concatenate 'string string "(/member)")))
      (setf string (concatenate 'string string "(/members)"))
      (setf result (append result (list (replace-all string "#_css:" ""))))
      
    ;(print targetmembers)    
      (setf string "(members)")
      (loop for i in targetmembers do
            (setf string (concatenate 'string string "(sws)"))
            (setf string (concatenate 'string string "(swsid)"))
            (setf string (concatenate 'string string (format nil "~s" (first i))))
            (setf string (concatenate 'string string "(/swsid)"))
            (loop for j in (second i) do
                  (setf string (concatenate 'string string "(member)"))
                  (setf string (concatenate 'string string "(csid)"))
                  (setf string (concatenate 'string string (format nil "~s" (first j))))
                  (setf string (concatenate 'string string "(/csid)"))
                  (setf string (concatenate 'string string "(values)"))
                  (loop for z in (second j) do
                        (setf string (concatenate 'string string (format nil "~s;" z))))
                  (setf string (string-right-trim ";" string))
                  (setf string (concatenate 'string string "(/values)"))          
                  (setf string (concatenate 'string string "(/member)")))                    
            (setf string (concatenate 'string string "(/sws)")))
      (setf string (concatenate 'string string "(/members)"))
      (setf result (append result (list (replace-all string "#_css:" ""))))
      
      (setf *distance-inputs-results* result)  
      )))


(def-function get-distance-inputs (?ontology ?goal)
  :lisp-fun #'(lambda (ontology goal)
                (get-distance-inputs-internal (intern (string-upcase ontology) :ocml) (intern (string-upcase goal) :ocml))))



(defun get-current-goal-input-role-internal (goal role)
  (let ((g nil)
        (o nil))
    (setf o (first (all-class-slot-values goal 'ocml::has-output-role)))
    (setf g (ocml::findany '?x `(and (,goal ?x)
                                     (not (,o ?x ?y))))) 
    (the-slot-value g role)))


(def-function get-current-goal-input-role (?goal ?role)
  :lisp-fun #'(lambda (goal role)
                (get-current-goal-input-role-internal (intern (string-upcase goal) :ocml) (intern (string-upcase role) :ocml))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWS descriptions for service mediation

(DEF-CLASS css-mediation-non-functional-properties (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS css-mediation-goal (GOAL) ?GOAL
  ((HAS-INPUT-ROLE :VALUE has-source-goal
                   :VALUE has-source-goal-ontology)
   (HAS-INPUT-SOAP-BINDING :VALUE (has-source-goal "sexpr")
                           :VALUE (has-source-goal-ontology "sexepr"))
   (HAS-OUTPUT-ROLE :VALUE has-closest-ws)
   (HAS-OUTPUT-SOAP-BINDING :VALUE (has-closest-ws "sexpr"))
   (has-source-goal :TYPE ocml-thing)
   (has-source-goal-ontology :TYPE ocml-thing)
   (has-closest-ws :TYPE ocml-thing)            
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE css-mediation-non-functional-properties)))

(DEF-CLASS css-mediation-mediator (WG-MEDIATOR) ?MEDIATOR
  ((HAS-SOURCE-COMPONENT :VALUE css-mediation-goal)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE css-mediation-non-functional-properties)))
             
(DEF-CLASS css-mediation-ws (WEB-SERVICE) ?WEB-SERVICE
  ((HAS-CAPABILITY :VALUE css-mediation-capability)
   (HAS-INTERFACE :VALUE css-mediation-interface)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE css-mediation-non-functional-properties)))
 
(DEF-CLASS css-mediation-capability (CAPABILITY) ?CAPABILITY
  ((USED-MEDIATOR :VALUE css-mediation-mediator)
   ;(HAS-ASSUMPTION :VALUE (OR (br-france br-germany)))
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE css-mediation-non-functional-properties)))

(DEF-CLASS css-mediation-interface (INTERFACE) ?INTERFACE
  ((HAS-CHOREOGRAPHY :VALUE css-mediation-choreography)
   (HAS-ORCHESTRATION :VALUE css-mediation-orchestration)
   (HAS-NON-FUNCTIONAL-PROPERTIES :VALUE css-mediation-non-functional-properties)))

(DEF-CLASS css-mediation-choreography (CHOREOGRAPHY)
  ((HAS-GROUNDING :VALUE ((NORMAL DUMMY-FUNCTION)))))
 
(DEF-CLASS css-mediation-orchestration-problem-solving-pattern (PROBLEM-SOLVING-PATTERN)
  ((HAS-BODY :VALUE ((orch-seq (achieve-goal similarity-calculation-goal
                                             "getSimilarities"
                                             (first (get-distance-inputs (get-current-goal-input-role 'css-mediation-goal 'has-source-goal-ontology)
                                                                         (get-current-goal-input-role 'css-mediation-goal 'has-source-goal)))
                                             (second (get-distance-inputs (get-current-goal-input-role 'css-mediation-goal 'has-source-goal-ontology)
                                                                          (get-current-goal-input-role 'css-mediation-goal 'has-source-goal)))
                                             (third (get-distance-inputs (get-current-goal-input-role 'css-mediation-goal 'has-source-goal-ontology)
                                                                         (get-current-goal-input-role 'css-mediation-goal 'has-source-goal))))
                               )
                     (orch-get-goal-value similarity-calculation-goal)
                     ))))
 
(DEF-CLASS css-mediation-orchestration (ORCHESTRATION) 
  ((HAS-PROBLEM-SOLVING-PATTERN :VALUE css-mediation-orchestration-problem-solving-pattern)))

|#






