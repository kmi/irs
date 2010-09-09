;;; Mode: Lisp; Package: cl-user

(in-package cl-user)

(defvar *currency-rates* (make-hash-table :test #'equal))


(eval-when (eval load)
  (setf (gethash '(us-dollar euro) *currency-rates*) 1.09706 
        (gethash '(us-dollar pound) *currency-rates*) 0.689562
        (gethash '(us-dollar lira) *currency-rates*) 2124.03
        (gethash '(us-dollar franc) *currency-rates*)7.19565
        (gethash '(us-dollar mark) *currency-rates*) 2.14549))


(defun generic_currency_converter_fun (source-currency target-currency amount)
  (shipping-patient-delay)
  ;;(setf s source-currency ta target-currency a amount)
  (let ((exchange-rate (achieve-task-through-irs
                        'ocml::exchange-rate-provider-ontology
                        'ocml::exchange_rate_provision
                        (List 'ocml::has_source_currency source-currency)
                        (list 'ocml::Has_target_currency target-currency))))
    ;;(setf aa exchange-rate)
    (* exchange-rate amount)))


(eval-when (eval load)
  (irs-method-registration  generic-currency-converter ;;this is the ontology name
                            generic_currency_converter  ;;this is the method name
                            generic_currency_converter_fun  ;;this is the function name
                            ))

#|
(achieve-task-through-irs 'generic-currency-converter 'ocml::generic_currency_conversion
                          (List 'ocml::has_source_currency 'pound)
                          (list 'ocml::Has_target_currency 'us-dollar)
                          (list 'ocml::has_amount 200))
|#
