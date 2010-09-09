;;; Mode: Lisp; Package: cl-user

(in-package cl-user)

(defvar *currency-rates* (make-hash-table :test #'equal))


(eval-when (eval load)
  (setf (gethash '(us-dollar euro) *currency-rates*) 0.808444
        (gethash '(us-dollar pound) *currency-rates*) 0.546209
        (gethash '(us-dollar lira) *currency-rates*) 1565.51
        (gethash '(us-dollar yen) *currency-rates*) 109.171
        (gethash '(us-dollar rupee) *currency-rates*) 46.3050
        (gethash '(us-dollar franc) *currency-rates*) 5.30304
        (gethash '(us-dollar mark) *currency-rates*) 1.58136))

(defun string-to-cl-user-symbol (x)
  (intern (string-upcase x)
          (find-package "CL-USER")))

(defun exchange-rate-provider-fun3 (source-currency target-currency)
  (/ (exchange-rate-provider-fun2 source-currency target-currency) 2))

(defun exchange-rate-provider-fun4 (source-currency target-currency)
  (* (exchange-rate-provider-fun2 source-currency target-currency) 10))

(defun exchange-rate-provider-fun2 (source-currency target-currency)
  ;;(sleep 60) ;;give me time to setup a screen snapshot
  ;;(shipping-patient-delay)
  ;;;(setf s source-currency ta target-currency)
  (when (stringp source-currency)
    (setf source-currency (string-to-cl-user-symbol source-currency)))
  (when (stringp target-currency)
    (setf target-currency (string-to-cl-user-symbol target-currency)))
  (when (symbolp source-currency)
    (setf source-currency (intern (symbol-name source-currency) (find-package "CL-USER"))))
  (when (symbolp target-currency)
    (setf target-currency (intern (symbol-name target-currency) (find-package "CL-USER"))))
  (cond ((eq source-currency 'us-dollar)
         (or (gethash (list source-currency target-currency)
                              *currency-rates*)
               0))
        ((eq target-currency 'us-dollar)
         (let ((inverse-rate (gethash (list target-currency source-currency)
                              *currency-rates*)))
           (if inverse-rate
               (/ 1 inverse-rate)
             0)))
        (t
         (let ((rate1 (gethash (list 'us-dollar source-currency)
                              *currency-rates*))
               (rate2 (gethash (list 'us-dollar target-currency)
                               *currency-rates*)))
           (if (and rate1 rate2)
               (/ rate2 rate1)
             0)))))

;;the exchange rate mediator
(defvar *currency-symbol-mappings*
  '(($ ocml::us-dollar) (£ ocml::pound) (¥ ocml::yen) 
    (ocml::$ ocml::us-dollar) (ocml::£ ocml::pound) (ocml::¥ ocml::yen)))
  
(defun mediate-exchange-rate-value (x)
  (let ((mapping (second (assoc x *currency-symbol-mappings*))))
    (or mapping x)))

(defun exchange-rate-mediator-fun (x)
 (mediate-exchange-rate-value x))

(defun multiply (x y)
  (* x y))

(defun orchestration-mediation (x)
  x)

(eval-when (eval load)
  (irs-wsmo-web-service-registration currency-converter-orchestration ;;this is the ontology name
                           exchange-rate-mediation-service  ;;this is the web service name
                           ;;exchange-rate-mediator-fun  ;;this is the function name
                           )
  (irs-wsmo-web-service-registration currency-converter-orchestration ;;this is the ontology name
                           european-exchange-rate-web-service  ;;this is the web service name
                          ;; exchange-rate-provider-fun2  ;;this is the function name
                           )
  (irs-wsmo-web-service-registration currency-converter-orchestration ;;this is the ontology name
                           non-european-exchange-rate-web-service  ;;this is the method name
                           ;;exchange-rate-provider-fun3  ;;this is the function name
                           )
  (irs-wsmo-web-service-registration currency-converter-orchestration ;;this is the ontology name
                           european-bank-exchange-rate-web-service  ;;this is the method name
                           ;;exchange-rate-provider-fun4  ;;this is the function name
                           )
  (irs-wsmo-web-service-registration currency-converter-orchestration ;;this is the ontology name
                                     multiply-web-service  ;;this is the method name
                                     ;;multiply  ;;this is the function name
                                     )
  (irs-wsmo-web-service-registration currency-converter-orchestration ;;this is the ontology name
                                     exchange-rate-multiply-mediator-web-service   ;;this is the method name
                                     ;;orchestration-mediation  ;;this is the function name
                                     )
                                     
  )


;;(achieve-goal-through-irs 'wsmo-exchange-rate 'exchange-rate-goal '(ocml::has_source_currency ocml::euro) '(ocml::has_target_currency ocml::$))

;;(achieve-goal-through-irs 'wsmo-exchange-rate 'exchange-rate-goal '(ocml::has_source_currency ocml::euro) '(ocml::has_target_currency ocml::us-dollar))

;;(achieve-goal-through-irs 'wsmo-exchange-rate 'exchange-rate-goal '(ocml::has_source_currency ocml::yen) '(ocml::has_target_currency ocml::us-dollar))

;;(achieve-goal-through-irs 'wsmo-exchange-rate 'exchange-rate-goal '(ocml::has_source_currency ocml::rupee) '(ocml::has_target_currency ocml::us-dollar))

(defvar *service-test-forms*
  '((achieve-goal-through-irs 'wsmo-exchange-rate 'exchange-rate-goal '(ocml::has_source_currency ocml::euro) '(ocml::has_target_currency ocml::$))
    (achieve-goal-through-irs 'wsmo-exchange-rate 'exchange-rate-goal 
                              '(ocml::has_source_currency ocml::euro) 
                              '(ocml::has_target_currency ocml::us-dollar))
    (achieve-goal-through-irs 'wsmo-exchange-rate 'exchange-rate-goal 
                              '(ocml::has_source_currency ocml::yen) 
                              '(ocml::has_target_currency ocml::us-dollar))
    (achieve-goal-through-irs 'wsmo-exchange-rate 'exchange-rate-goal 
                              '(ocml::has_source_currency ocml::rupee)
                              '(ocml::has_target_currency ocml::us-dollar))))

(defun test-exchange-services (&optional (service-test-forms *service-test-forms*))
  (let ((i 0))
    (dolist (form service-test-forms)
      (incf i)
      (run-wsmo-exchange-test-form form i))))

(defun run-wsmo-exchange-test-form (form test-number)
  (format t "Test ~d: ~(~s~)~%" test-number form)
  (format t "~a~%~%" (eval form)))

;;;(publish-all-services)
;;;(start-tcp-services)
