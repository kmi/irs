;;; Copyright © 2010 The Open University

(in-package wsmo-protocol)

(defun send-achieve-goal-error-message (condition goal-type stream)
  (iu::send-soap-error-message condition goal-type stream))

(defun send-achieve-goal-error-message2 (message goal-type stream)
  (iu::send-soap-error-message2 message goal-type stream))

;;; This function constitutes the non-semantic route to invoking
;;; goals.  By that, I mean that messy implementation issues, like
;;; HTTP or SOAP invocation, count here.  More importantly, the input
;;; values are non-semantic.  A string may name an OCML instance, for
;;; example, but isn't one.  We have to convert such things, through
;;; skyhooks, so that the irs-solve-goal call is dealing with pure,
;;; clean OCML instances and primitive values.  So let's get going.
;;; There's no other choice.  God willing, we will prevail in peace
;;; and freedom from fear and in true health through the purity and
;;; essence of our natural fluids.  God bless you all.
(defun ip::raw-irs-achieve-goal (ontology goal-type input-role-value-pairs
                                 html-stream soap-response-p
                                 &optional http-request-p)
  (ocml:with-ontology (ontology)
    (unless (ocml::findall '?x `(ocml::subclass-of ,goal-type ocml::goal))
      (error "No goal class matching name ~S." goal-type))
    ;; XXX We shouldn't be calling get-domain-class here.  This should
    ;; only be done if the lift function wants an ocml name from the
    ;; invoker, and even then the lift should do the call.  We can't
    ;; remove it at the moment because most services implicitly assume
    ;; it's here..
    (let* ((role/value-pairs
            (re-order-input-role-value-pairs-from-goal
             goal-type input-role-value-pairs))
           (slots (mapcar #'first role/value-pairs))
           (values (mapcar #'second role/value-pairs))
           (classes (mapcar
                     #'(lambda (pair)
                         (let* ((type-name (wsmo-protocol::get-input-role-type
                                            goal-type (first pair)))
                                (class (ocml::get-domain-class type-name)))
                           (if class
                               (ocml::name class)
                               (error "No domain class found for type of slot ~A." (first pair)))))
                     role/value-pairs))
           (skyhooked (grounding:apply-skyhooks slots classes values))
           (final-pairs (mapcar #'list slots skyhooked)))
      (irs-solve-goal ontology goal-type final-pairs html-stream soap-response-p http-request-p))))

(defun achieve-goal-slotted (ontology goal slot-value-pairs)
  "A simpler entry point to the invocation machinary."
  ;; Hides the nonsense about SOAP/HTTP, output to string etc.
  (with-output-to-string (str)
    (ip::raw-irs-achieve-goal ontology goal slot-value-pairs str nil t)))

;;; 2007/04/05 Dave: Factor out raw-irs-achieve-goal, so we can get
;;; any signalled conditions if we want.
(defun ip::irs-achieve-goal (ontology goal-type input-role-value-pairs 
                                      html-stream soap-response-p &optional http-request-p)
  (handler-case
      (ip::raw-irs-achieve-goal ontology goal-type input-role-value-pairs 
                                html-stream soap-response-p http-request-p)
    (iu::soap-response-error
     (c)
     (send-achieve-goal-error-message2 (iu::has-soap-response c)
                                       goal-type html-stream))
    (iu::irs-connection-error
     (c)
     (send-achieve-goal-error-message2 (format nil "Connection problem with host ~a and port ~a"
                                               (iu::has-host c) (iu::has-port c))
                                       goal-type html-stream))
    ((or ocml::no-role-value-error serious-condition error)
     (c)
     (send-achieve-goal-error-message c goal-type html-stream))))

(defun get-output-type (x)
  (when (and (symbolp x)
             (ocml::find-current-instance x))
    (web-onto::findany '?x `(ocml::instance-of ,x ?x))))

;; Dave 2007-03-29: Clean up.
;; XXX Are soap-p/http-request-p mutually exclusive?  If so, shouldn't
;; we have an invocation-path type, with values :soap/:http?
(defun irs-solve-goal (ontology goal-type input-role-value-pairs 
                       html-stream &optional soap-p http-request-p)
  (ocml:with-ontology (nil)
    #+:irs-lispworks
    (visualiser:received-achieve-task-message
     ontology goal-type input-role-value-pairs))
  (irs.api.javascript:event :goal-call goal-type input-role-value-pairs)
  (let* ((*package* (find-package "OCML"))
         (result (ip::internal-solve-goal ontology goal-type
                                          input-role-value-pairs))
         (output-type (get-output-type result))
         (lower-function (grounding:get-lower-function output-type))
         (soap-output-type (second (output-role-with-soap-binding goal-type))))
    ;; Carlos 22-10-2006 - Put visualization before the lowering
    (ocml:with-ontology (nil)
      #+:irs-lispworks
      (visualiser:sending-achieve-task-message
       ontology goal-type input-role-value-pairs result))
    (irs.api.javascript:event :goal-return goal-type result)
    (when lower-function 
      (setf result (funcall lower-function result)))
    (if soap-p
        (if (ip::soap-attachment-type-p soap-output-type)
            (ip::send-soap-attachment 
             (symbol-name goal-type)
             result `((result ,soap-output-type)) html-stream)
          (iu::send-soap-response2 (symbol-name goal-type)
                                   (list result) 
                                   `((result ,soap-output-type)) 
                                   :stream html-stream))
      ;;(setf go goal-output-type lf lower-function rr result)
      ;; Minor fix to avoid duplication of the result in SOAP messages (Carlos 23-1-07)
      (if http-request-p
          (format html-stream "~a" result)
        (format html-stream "~s" result)))))

;;(ip::internal-solve-goal 'ocml::wsmo-test 'ocml::exchange-rate-goal '((ocml::has_source_currency ocml::euro) (ocml::has_target_currency ocml::us-dollar)))

;;(ip::internal-solve-goal 'ocml::wsmo-use-case 'ocml::buy-train-ticket-goal '((ocml::has-person ocml::christoph) (ocml::has-departure-station ocml::frankfurt) (ocml::has-destination-station ocml::berlin) (ocml::has-date-and-time (3 4 6 18 8 2004))))

;;(ip::internal-solve-goal 'ocml::currency-converter-orchestration 'ocml::exchange-rate-goal '((ocml::has_source_currency pound) (ocml::has_target_currency euro)))

;;(ip::internal-solve-goal 'ocml::currency-converter-orchestration 'ocml::exchange-rate-goal '((ocml::has_source_currency pound) (ocml::has_target_currency euro)))

;;(ip::internal-solve-goal 'ocml::currency-converter-orchestration 'ocml::currency-converter-goal '((ocml::has_source_currency pound) (ocml::has_target_currency euro) (ocml::has_amount 10)))

;;;ontology
;;now solve the goal in the home ontology of the link 
;;mediator we now assume that the goal and web service 
;;ontologies can be separate but that the 
;;home ontology of the mediator inherits from both
;;maybe in the future we will change this
;;john 24/6/05

;;now get the 'deepest ontology' by removing subsumed ontologies john 23/9/05
(defun ontology-for-invocation (ontology web-service mediator)
  (let ((mediator-ontology (ocml::home-ontology mediator))
        (web-service-ontology (ocml::home-ontology web-service)))
    (ocml::name (car (ocml::remove-subsumed-ontologies 
                      (list mediator-ontology (ocml::get-ontology ontology) web-service-ontology))))))

;;; XXX Surely ONTOLOGY-FOR-INVOCATION is mishandled?  There're
;;; multiple mediators, each with their own ontology.  But we end up
;;; using the last value of ONTOLOGY-FOR-INVOCATION, regardless of
;;; which mediator we use...
(defun ip::internal-solve-goal (ontology goal-type role-value-pairs
                                &optional (call-strategy :first))
  (ocml:with-ontology (ontology)
    (let* ((goal (ocml::name (ocml::new-instance goal-type role-value-pairs)))
           ontology-for-invocation
           (web-service-contenders-with-mediators
            (find-all-web-services-with-mediators-which-solve-goal goal-type ontology))
           (applicable-web-services
            (mapcan #'(lambda (web-service-and-mediator)
                        (destructuring-bind (web-service mediator)
                            web-service-and-mediator
                          (setf ontology-for-invocation
                                (ontology-for-invocation ontology web-service mediator))
                          (let ((result 
                                 (suitable-web-service? ontology-for-invocation goal-type
                                                        goal role-value-pairs web-service)))
                            (when result 
                              (list result)))))
                    web-service-contenders-with-mediators)))
      (unless applicable-web-services
        (error "No applicable web services."))
      (let ((result (case call-strategy
                      ((:first) (invoke-service (third (car applicable-web-services))
                                                (second (car applicable-web-services))))
                      ((:all) (mapc #'(lambda (applicable-web-service)
                                        (invoke-service (third applicable-web-service)
                                                        (second applicable-web-service)))
                                    applicable-web-services)))))
        (when (boundp '*achieve-goal-results*)
          (push (list (list ontology goal-type) result) *achieve-goal-results*))
        (values result ontology-for-invocation)))))

(defvar *internal-irs-broker-goals*
  '(ocml::suitable-web-service-goal ocml::find-web-services-for-goal ocml::mediate-input-role-values-goal))

(defun internal-irs-broker-goal-p (x)
  (find x *internal-irs-broker-goals*))

(defun mediate-role-pairs (ontology goal web-service actual-role-pairs)
  (let* ((role-names (mapcar #'car actual-role-pairs))
         (role-values (mapcar #'second actual-role-pairs))
         (result (ip::internal-solve-goal
                  ontology 'ocml::mediate-input-role-values-goal
                  (list (list 'ocml::has-target-goal goal)
                        (list 'ocml::has-mediation-ontology ontology)
                        (list 'ocml::has-web-service web-service)
                        (list 'ocml::has-input-role-values role-values)
                        (list 'ocml::has-input-roles role-names))))
         (role-values (car result))
         (combined-oo-mediators-ontology (second result)))
    (values
     (mapcar #'(lambda (role-name role-value)
                 (list role-name role-value))
             role-names role-values)
     combined-oo-mediators-ontology)))

(defun suitable-web-service? (ontology goal-type goal actual-role-pairs web-service)
  ;;(setf ty2 task-type a2 actual-role-pairs p psm)
  ;;(format t "R: ~a~%" actual-role-pairs)
  (if (internal-irs-broker-goal-p goal-type)
      (internal-suitable-web-service? goal-type actual-role-pairs web-service ontology)
    (multiple-value-bind (role-pairs combined-oo-mediator-ontology)
        (mediate-role-pairs ontology goal
                            (ocml::name web-service) actual-role-pairs)
      ;;(format t "ontology ~a; goal: ~a, web-service: ~a combined ontology ~a~%"
        ;;      ontology  goal-type web-service combined-oo-mediator-ontology)
      (ip::internal-solve-goal 
       combined-oo-mediator-ontology
       'ocml::suitable-web-service-goal
       `((ocml::has-goal ,goal-type)
         (ocml::has-actual-role-pairs ,role-pairs)
         (ocml::has-web-service ,web-service)
         (ocml::has-combined-oo-mediator-ontology ,combined-oo-mediator-ontology))))))


(defun ocml::suitable-web-service-internal-method (ontology web-service)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((instance (ocml::find-current-instance web-service)))
      (when instance
        ;;not needed anymore with the oo-mediator combined ontology
        (ocml::select-ontology (ocml::name (ocml::home-ontology instance)))
        (let* ((goal-inst 
                (web-onto::findany 
                 '?goal-inst
                 `(ocml::suitable-web-service ?goal-inst ,web-service)))
               (goal
                (web-onto::findany 
                 '?goal
                 `(ocml::has-goal ,goal-inst ?goal)))
               (actual-role-pairs
                (web-onto::findany 
                 '?actual-role-pairs
                 `(ocml::has-actual-role-pairs ,goal-inst ?actual-role-pairs)))
               (combined-oo-mediator-ontology
                (web-onto::findany 
                 '?combined-oo-mediator-ontology
                 `(ocml::has-combined-oo-mediator-ontology ,goal-inst ?combined-oo-mediator-ontology)))
               (candidate-web-service
                (web-onto::findany 
                 '?candidate-web-service
                 `(ocml::has-web-service ,goal-inst ?candidate-web-service))))
          ;;(setf gg goal-inst arp actual-role-pairs)
          (internal-suitable-web-service? goal actual-role-pairs candidate-web-service combined-oo-mediator-ontology))))))

;;;borrowed and adapted from control4
(defun internal-suitable-web-service? (goal-type actual-role-pairs web-service combined-oo-mediator-ontology)
  ;;(setf ty2 task-type a2 actual-role-pairs p psm)
  (let ((original-ontology ocml::*current-ontology*))
    (unwind-protect
        (progn 
          (let ((*package* (find-package "OCML")))
            ;;(format t "~%original ontology ~a ws ~a comb ~a~%" (ocml::name original-ontology)
            ;;        (ocml::name (ocml::home-ontology web-service))
            ;;       combined-oo-mediator-ontology)
                
            ;;not needed anymore with the oo-mediator combined ontology
            ;;(ocml::switch-to-ontology (ocml::home-ontology web-service)))
            
            (ocml::select-ontology combined-oo-mediator-ontology)
            (let ((inst (ocml::new-instance goal-type actual-role-pairs))
                  result)
              (setf result (ocml::holds? 'ocml::APPLICABLE-TO-goal (ocml::name web-service)
                                         (ocml::name inst)))              
              (when result
                (when (not (internal-irs-broker-goal-p goal-type))
                  ;(format t "going to push instance ~a~%" inst)
                  (when (boundp '*internal-goal-instances*)
                    (setf *internal-goal-instances* 
                          (acons goal-type (ocml::name inst) *internal-goal-instances*)))
                  ;(format t "goal instances in memory ~a~%" *internal-goal-instances*)
                  )
                (list (ocml::name inst)
                      (ocml::ocml-eval-gen `(ocml::instantiate-web-service
                                             ,(ocml::name inst)
                                             ,(ocml::name web-service)
                                             ))
                      combined-oo-mediator-ontology)))))
          (ocml::switch-to-ontology original-ontology))))


(defun find-all-web-services-with-mediators-which-solve-goal (goal-type ontology)
  (if (internal-irs-broker-goal-p goal-type)
      (internal-find-all-web-services-which-solve-goal goal-type ontology)
    (ip::internal-solve-goal 
     ontology
     'ocml::find-web-services-for-goal
     `((ocml::has-goal ,goal-type)
       (ocml::has-ontology ,ontology)))))

(defun find-all-web-services-which-solve-goal (goal-type
                                               &optional 
                                               (ontology
                                                (ocml::name ocml::*current-ontology*)))
  (mapcar #'car (find-all-web-services-with-mediators-which-solve-goal goal-type ontology)))

(defun ocml::web-services-which-solve-goal-internal-method (ontology web-service)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((instance (ocml::find-current-instance web-service)))
      (when instance
        (ocml::select-ontology (ocml::name (ocml::home-ontology instance)))
        (let* ((goal-inst 
                (web-onto::findany 
                 '?goal-inst
                 `(ocml::suitable-web-service ?goal-inst ,web-service)))
               (goal
                (web-onto::findany 
                 '?goal
                 `(ocml::has-goal ,goal-inst ?goal)))
               (goal-ontology
                (web-onto::findany 
                 '?ontology
                 `(ocml::has-ontology ,goal-inst ?ontology))))
          (internal-find-all-web-services-which-solve-goal goal goal-ontology))))))

;;;borrowed and adapted from control4
(defun internal-find-all-web-services-which-solve-goal (goal-type
                                               &optional (ontology
                                                          (ocml::name ocml::*current-ontology*)))
  (if (eq ontology (ocml::name ocml::*current-ontology*))
    (internal-find-all-web-services-which-solve-goal2 goal-type )
    (let ((current-ontology ocml::*current-ontology*))
    (unwind-protect 
      (progn
        (ocml::select-ontology ontology)
        (internal-find-all-web-services-which-solve-goal2 goal-type))
      (ocml::switch-to-ontology current-ontology)))))

;;;borrowed and adapted from control4
#|
(defun internal-find-all-web-services-which-solve-goal2 (goal-type)
  ;;(setf gg goal-type)
  (let* ((home-ontology (ocml::home-ontology (ocml::get-ocml-class goal-type)))
         (relevant-ontologies (cons home-ontology
                                    (ocml::dependent-ontologies 
                                     home-ontology)))
         
         (result))
   (loop for ontology in (ocml::remove-subsumed-ontologies relevant-ontologies)
          do
          (ocml::switch-to-ontology ontology)
          (setf result (append result 
                               (mapcar #'ocml::get-ocml-class
                                       (ocml::setofall '?x 
                                                 `(ocml::can-solve-goal
                                                   ,goal-type ?x))))))
      
    (remove-duplicates result)))
|#

;;;re-write to be faster
;;;(ocml::holds? 'ocml::can-solve-goal2 goal-type web-service)
(defun internal-find-all-web-services-which-solve-goal2 (goal-type)
  (let* ((home-ontology (ocml::home-ontology (ocml::get-ocml-class goal-type)))
         (relevant-ontologies (cons home-ontology
                                    (ocml::dependent-ontologies 
                                     home-ontology)))
         
         (result nil))
    (loop for ontology in (ocml::remove-subsumed-ontologies relevant-ontologies)
          do
          (ocml::switch-to-ontology ontology)
          (setf result
                (append result
                        (find-all-web-services-with-mediators-which-solve-goal-in-current-ontology
                         goal-type))))
    (remove-duplicates result :test #'equal)))


(defun get-all-web-service-associated-with-goal-in-current-ontology (goal-type)
  (remove-duplicates
   (mapcar #'car 
          (find-all-web-services-with-mediators-which-solve-goal-in-current-ontology 
           goal-type))))

(defun find-all-web-services-with-mediators-which-solve-goal-in-current-ontology (goal-type)
  (let ((web-services 
         ;;make sure the class exists
         (mapcan
          #'(lambda (x) (when (ocml::get-domain-class x) (list x)))
          (web-onto::findany 
           '?x '(= ?x (ocml::all-subclasses ocml::web-service)))))
        (mediators
         ;;make sure the class exists
         (mapcan
          #'(lambda (x) (when (ocml::get-domain-class x) (list x)))
          (web-onto::findany 
           '?x '(= ?x (ocml::all-subclasses ocml::mediator))))))
    (remove-duplicates
     (append (mapcan 
              #'(lambda (web-service)
                  (let ((mediator 
                         (ocml::findany 
                          '?mediator
                          `(ocml::check-web-service-can-solve-goal
                            ,goal-type ,web-service ?mediator))))
                    (unless (ocml:nothing? mediator)
                      (list (list (ocml::get-ocml-class web-service)
                                  (ocml::get-ocml-class mediator))))))
              web-services)
             (mapcan 
              #'(lambda (mediator)
                  (let ((web-service
                         (ocml::findany 
                          '?web-service
                          `(ocml::get-web-service-for-goal-from-mediator
                            ,goal-type ,mediator ?web-service))))
                    (unless (ocml:nothing? web-service)
                      (list (list (ocml::get-ocml-class web-service)
                                  (ocml::get-ocml-class mediator))))))
              mediators))
     :test #'equal)))

(defun ocml::internal-mediators-with-web-service-and-goal (web-service goal)
  (remove-duplicates
   (mapcan
    #'(lambda (x) 
        (when (and (ocml::get-domain-class x)
                   (eq (ip::get-class-slot-value x 'ocml::has-source-component) goal)
                   (eq (ip::get-class-slot-value x 'ocml::has-target-component) web-service))
          (list x)))
    (web-onto::findany 
     '?x '(= ?x (ocml::all-subclasses ocml::mediator))))))

;;; Shared code used by the context sensitive and trusted goal
;;; invocation strategies.

(defun ocml::the-parent-class (i)
  "Returns name of class of input instance I."
  (ocml::name (ocml::parent-class (ocml::find-current-instance i))))

(defun ws-functional-selection (ontology goal-type role-value-pairs)
  (ocml:with-ontology (ontology)
    (let* ((goal (ocml::name (ocml::new-instance goal-type role-value-pairs)))
           (applicable-web-services
            (mapcan #'(lambda (web-service-and-mediator)
                        (destructuring-bind (web-service mediator)
                            web-service-and-mediator
                          (let ((result
                                 (suitable-web-service?
                                  (ontology-for-invocation ontology web-service mediator)
                                  goal-type goal role-value-pairs web-service)))
                            (when result
                              (list result)))))
                    (find-all-web-services-with-mediators-which-solve-goal goal-type ontology))))
      (first (list applicable-web-services)))))
