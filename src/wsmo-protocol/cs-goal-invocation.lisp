(in-package #:wsmo-protocol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; core function: gets the inputs for the similarity calculation given a goal invocation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun get-inputs-for-distance-calculation (ontology goal inputs)
  ;(ocml:with-ontology ((ocml::get-ontology ontology))
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((assum nil)
          (spaces nil)
          (webservices nil)
          (conceptualspaces nil)
          (basemembers nil)
          (targetmembers nil)
          (result nil)
          (string ""))
      
      ;;dealing with the goal side
      (loop for i in inputs do ;;in case, change here the way to get the assumptions from the goal  
            (let ((x (ocml::make-ocml-object (second i)))
                  (element nil))
              (setf element (ocml::findany '?m `(and (ocml::refined-by ,x ?m)
                                                     (ocml::|http://www.kmi.open.ac.uk/cs#member| ?m))))
              
              (if (and (not (not element))
                       (not (ocml:nothing? element)))
                  (setf assum (append assum (list x))))))

      (loop for i in assum do
            (let ((space nil)
                  (member nil)
                  (vals nil))
              (setf member (ocml::the-slot-value i 'ocml::refined-by))
              (setf space (ocml::the-slot-value member 'ocml::member-in))
                
                ;involved roles
                #|
                (setf roles (append roles (ocml::setofall '?r `(and (ocml::has-input-role ,g ?r)
                                                                    (= (ocml::the-slot-value ,g ?r)
                                                                       ,i)))))
                |#

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
              (setf assum (third (third assum)))         ;;assumption example: (KAPPA (?WEB-SERVICE) (OR (= 1 1) (OR (br-france br-germany)))
              
              ;;in case, improve here the interpretation of the assumption;
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
      (setf result (append result (list (replace-all string "#_cs:" ""))))

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
      (setf result (append result (list (replace-all string "#_cs:" ""))))
      
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
      (setf result (append result (list (replace-all string "#_cs:" ""))))
       
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modified functions to achieve the goal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ip::cs-internal-solve-goal (ontology goal-type role-value-pairs 
                                             &optional (call-strategy :first)) ;;;;;;;;;;;;;;;;;;;
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((applicable-web-services nil)
          (cs-applicable-web-services nil)
          (cs-mediation-goal-inputs nil)
          (cs-list nil)
          (base-member-list nil)
          (target-member-list nil)
          (result nil)
          (output nil))

      ;; possible ws
      (setf applicable-web-services (ws-functional-selection ontology goal-type role-value-pairs))

      ;; the most similar ws
      (setf cs-mediation-goal-inputs (get-inputs-for-distance-calculation ontology goal-type role-value-pairs))
      (setf cs-list (first cs-mediation-goal-inputs))
      (setf base-member-list (second cs-mediation-goal-inputs))
      (setf target-member-list (third cs-mediation-goal-inputs))
      (setf cs-applicable-web-services 
            (ocml::make-ocml-object
             (read-from-string 
              (with-output-to-string (oostream)
                (ip::irs-achieve-goal 'ocml::cs-goals   
                                      'ocml::similarity-calculation-goal 
                                      `((ocml::has-method "getSimilarities")
                                        (ocml::has-cs ,cs-list)
                                        (ocml::has-base-member ,base-member-list)
                                        (ocml::has-target-member ,target-member-list))
                                      oostream nil)))))

      (loop for i in applicable-web-services do 
            (if (equalp (ocml::the-parent-class (second i)) cs-applicable-web-services)
                (setf result i)))
      (setf result 
            (when cs-applicable-web-services
              (invoke-service (third result) ;;ontology ;;goal-type
                              (second result))))
      (when (boundp '*achieve-goal-results*)
          (push (list (list ontology goal-type) result) *achieve-goal-results*))
      result)))


(defun ip::raw-cs-irs-achieve-goal (ontology goal-type input-role-value-pairs 
                                              html-stream soap-response-p &optional http-request-p)
  (ocml:with-ontology (ontology)
    (setf input-role-value-pairs 
          (re-order-input-role-value-pairs-from-goal goal-type input-role-value-pairs))
    (cs-irs-solve-goal ontology goal-type input-role-value-pairs html-stream soap-response-p http-request-p)))

(defun ip::cs-irs-achieve-goal (ontology goal-type input-role-value-pairs 
                                          html-stream soap-response-p &optional http-request-p)
  (handler-case
      (ip::raw-cs-irs-achieve-goal ontology goal-type input-role-value-pairs 
                                    html-stream soap-response-p http-request-p)
    (iu::soap-response-error
     (condition)
     (send-achieve-goal-error-message2 (iu::has-soap-response condition)
                                       goal-type html-stream))
    (iu::irs-connection-error
     (condition)
     (send-achieve-goal-error-message2 (format nil "Connection problem with host ~a and port ~a"
                                               (iu::has-host condition)
                                               (iu::has-port condition))
                                       goal-type html-stream))
    ((or ocml::no-role-value-error serious-condition error)
     (condition)
     (send-achieve-goal-error-message condition goal-type html-stream))))


(defun cs-irs-solve-goal (ontology goal-type input-role-value-pairs 
                                    html-stream &optional soap-p http-request-p)
  (let ((*package* (find-package "OCML")))
    (irs.api.javascript:event :goal-call goal-type input-role-value-pairs)
    (let* (;;(*internal-goal-instances* nil)
           (result (ip::cs-internal-solve-goal ontology goal-type input-role-value-pairs))
           (output-type (get-output-type result))
           (lower-function (grounding:get-lower-function output-type))
           (soap-output-type (second (output-role-with-soap-binding goal-type))))

      (irs.api.javascript:event :goal-return goal-type result)

      ;; Alessio 16-09-2008 - Set the output of the goal invocation
      (setf output-role (first (output-role-with-soap-binding goal-type)))
      (setf goal-instance (ocml::findany '?x `(and (,goal-type ?x)
                                                   (not (,output-role ?x ?y)))))
      (ocml::set-slot-value goal-instance output-role result)

      (when lower-function 
        (setf result (funcall lower-function result)))
      
      (let ((output-type 
             (second (output-role-with-soap-binding goal-type))))
        (if soap-p
            (if (ip::soap-attachment-type-p output-type)
                (ip::send-soap-attachment 
                 (symbol-name goal-type)
                 result `((result ,output-type)) html-stream)
              (iu::send-soap-response2 (symbol-name goal-type)
                                       (list result) 
                                       `((result ,output-type)) 
                                       :stream html-stream)))           
        ;;(setf go goal-output-type lf lower-function rr result)
        (if http-request-p
            (format html-stream "~a" result)
          (format html-stream "~s" result))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; SECOND VERSION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-inputs-for-distance-calculation2 (ontology goal assumption)

  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((assum nil)
          (spaces nil)
          (webservices nil)
          (conceptualspaces nil)
          (basemembers nil)
          (targetmembers nil)
          (result nil)
          (string ""))
      
      ;; check if the elements of the assumption are grounded to a CS member (if not, they're not taken into consideration)
      (loop for i in assumption do   
            (let ((element nil))
              (setf element (ocml::findany '?m `(and (ocml::refined-by ,i ?m)
                                                     (ocml::|http://www.kmi.open.ac.uk/cs#member| ?m))))
              
              (if (and (not (not element))
                       (not (ocml:nothing? element)))
                  (setf assum (append assum (list i))))))

      (loop for i in assum do
            (let ((space nil)
                  (member nil)
                  (vals nil))
              (setf member (ocml::the-slot-value i 'ocml::refined-by))
              (setf space (ocml::the-slot-value member 'ocml::member-in))
                
              ;involved roles (may be necessary in future)
               #|
                (setf roles (append roles (ocml::setofall '?r `(and (ocml::has-input-role ,g ?r)
                                                                    (= (ocml::the-slot-value ,g ?r)
                                                                       ,i)))))
               |#

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
              (setf assum (third (third assum)))         ;;assumption example: (KAPPA (?WEB-SERVICE) (OR (= 1 1) (OR (arriving-france arriving-germany)))
              
              ;;in case, improve here the interpretation of the assumption;
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
      (setf result (append result (list (replace-all string "#_cs:" ""))))

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
      (setf result (append result (list (replace-all string "#_cs:" ""))))
      
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
      (setf result (append result (list (replace-all string "#_cs:" ""))))
       
      )))


(defun ip::cs-irs-internal-select-ws (ontology goal-type assumption role-value-pairs &optional (call-strategy :first)) ;;;;;;;;;;;;;;;;;;;
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((applicable-web-services nil)
          (cs-applicable-web-services nil)
          (cs-mediation-goal-inputs nil)
          (cs-list nil)
          (base-member-list nil)
          (target-member-list nil)
          (result nil)
          (output nil))

      ;; the most similar ws
      (setf cs-mediation-goal-inputs (get-inputs-for-distance-calculation2 ontology goal-type assumption))
      (setf cs-list (first cs-mediation-goal-inputs))
      (setf base-member-list (second cs-mediation-goal-inputs))
      (setf target-member-list (third cs-mediation-goal-inputs))
      (setf cs-applicable-web-services 
            (ocml::make-ocml-object
             (read-from-string 
              (with-output-to-string (oostream)
                (ip::irs-achieve-goal 'ocml::cs-goals   
                                      'ocml::all-similarities-calculation-goal 
                                      `((ocml::has-method "getSimilarities")
                                        (ocml::has-cs ,cs-list)
                                        (ocml::has-base-member ,base-member-list)
                                        (ocml::has-target-member ,target-member-list))
                                      oostream nil)))))

      ;; possible ws for invocation
      (setf applicable-web-services (ws-functional-selection ontology goal-type role-value-pairs))
     
      (list cs-applicable-web-services applicable-web-services))))

(defun ip::cs-irs-select-ws (ontology goal-type assumption input-role-value-pairs html-stream soap-response-p &optional http-request-p)
  (ocml:with-ontology (ontology)
    (setf input-role-value-pairs 
          (re-order-input-role-value-pairs-from-goal goal-type input-role-value-pairs))

    (let ((*package* (find-package "OCML")))
      (irs.api.javascript:event :goal-call goal-type assumption)
      (let* ((result (ip::cs-irs-internal-select-ws ontology goal-type assumption input-role-value-pairs)))
        (irs.api.javascript:event :goal-return goal-type result)
        result
        ))))

;;;;;;;;;;;; phase two: ws invocation


;; redefined because now the inputs cannot be taken from the goal
(defun cs-invoke-service (ontology web-service inputs)
  ;;(push web-service www)
  (ocml:with-ontology (ontology)
    (let* ((host (web-onto::findany '?x `(ocml::wsmo-web-service-host ,web-service ?x)))
           (port (web-onto::findany '?x `(ocml::wsmo-web-service-port ,web-service ?x)))
           (location (web-onto::findany '?x `(ocml::wsmo-web-service-location ,web-service ?x)))
           (input-roles-with-soap-bindings
            (ip::input-roles-with-soap-bindings web-service))
           (output-role-with-soap-binding
            (output-role-with-soap-binding web-service))
           (output-type (intern (string-upcase (second output-role-with-soap-binding))
                                (find-package "API-SOAP-OLD")))
           (orchestration-execution-pattern (get-orchestration-execution-pattern web-service)))
      ;;(format t "ws info ~a~%" (list web-service host port location))
      (cond ((and orchestration-execution-pattern
                  (not (ocml:nothing? orchestration-execution-pattern)))
             (run-orchestration orchestration-execution-pattern web-service))
            (host ;;;(and host port location)
                  (internal-invoke-service 
                   web-service
                   (generate-service-soap-name ontology web-service)
                   input-roles-with-soap-bindings
                   inputs                   
                   host 
                   port 
                   location 
                   output-type))
            ;;;if no published web service try calling an internal function
            ((generic-get-internal-method web-service)
             (let ((internal-method (generic-get-internal-method web-service)))
               (when internal-method
                 (cond ((ocml-function-p internal-method)
                        (ocml::ocml-eval-gen `(,internal-method ,ontology ,web-service)))
                       ((fboundp internal-method)
                        (funcall internal-method ontology web-service))))))))))


(defun ip::internal-cs-irs-invoke-ws (ontology ws inputs)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (setf result 
          (cs-invoke-service (third ws)
                             (second ws)
                             inputs))
    (when (boundp '*achieve-goal-results*)
          (push (list (list ontology goal-type) result) *achieve-goal-results*))
    result))


(defun ip::cs-irs-invoke-ws (ontology goal-type ws inputs)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (irs.api.javascript:event "WS-CALL" (second ws) inputs)

    (let* ((result (ip::internal-cs-irs-invoke-ws ontology ws inputs))
           (output-type (get-output-type result))
           (lower-function (grounding:get-lower-function output-type))  ;;;; may be here we need to redefine the function to get the output type
           (soap-output-type (second (output-role-with-soap-binding goal-type))))

      (irs.api.javascript:event "WS-RETURN" (second ws) result)

      ;; Alessio 16-09-2008 - Set the output of the goal invocation
      (setf output-role (first (output-role-with-soap-binding goal-type)))
      (setf goal-instance (ocml::findany '?x `(and (,goal-type ?x)
                                                   (not (,output-role ?x ?y)))))
      (ocml::set-slot-value goal-instance output-role result)

      (when lower-function 
        (setf result (funcall lower-function result)))

#|
      
      (let ((output-type 
             (second (output-role-with-soap-binding goal-type))))
        (if soap-p
            (if (ip::soap-attachment-type-p output-type)
                (ip::send-soap-attachment 
                 (symbol-name goal-type)
                 result `((result ,output-type)) html-stream)
              (iu::send-soap-response2 (symbol-name goal-type)
                                       (list result) 
                                       `((result ,output-type)) 
                                       :stream html-stream)))           
        ;;(setf go goal-output-type lf lower-function rr result)
        (if http-request-p
            (format html-stream "~a" result)
          (format html-stream "~s" result)))
|#


result

    )))






    



   



