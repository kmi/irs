;;; Mode: Lisp; Package: cl-user

(in-package cl-user)

(defvar *currency-rates* (make-hash-table :test #'equal))


(eval-when (eval load)
  (setf (gethash '(us-dollar euro) *currency-rates*) 1.09706 
        (gethash '(us-dollar pound) *currency-rates*) 0.689562
        (gethash '(us-dollar lira) *currency-rates*) 2124.03
        (gethash '(us-dollar franc) *currency-rates*)7.19565
        (gethash '(us-dollar mark) *currency-rates*) 2.14549))


(defun exchange-rate-provider-fun (source-currency target-currency)
  (setf s source-currency ta target-currency)
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

#|
(def-irs-soap-interface exchange-rate-provider  ;;this is the ontology name
  exchange-rate-provider  ;;this is the method name
  ((has-source-currency "xsd:string") ;; read-from-string) Mauro
                                      ;; At the moment is not supported
   (has-target-currency "xsd:string")
   )
  "xsd:float"
   exchange-rate-provider   ;;this is the lisp function
  )
|#


(irs-method-registration exchange-rate-provider-ontology ;;this is the ontology name
  exchange-rate-provider  ;;this is the method name
  exchange-rate-provider-fun  ;;this is the function name
)






