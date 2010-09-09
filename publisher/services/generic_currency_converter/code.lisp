;;; Mode: Lisp; Package: cl-user

(in-package cl-user)

(defvar *currency-rates* (make-hash-table :test #'equal))


(eval-when (eval load)
  (setf (gethash '(us-dollar euro) *currency-rates*) 1.09706 
        (gethash '(us-dollar pound) *currency-rates*) 0.689562
        (gethash '(us-dollar lira) *currency-rates*) 2124.03
        (gethash '(us-dollar franc) *currency-rates*)7.19565
        (gethash '(us-dollar mark) *currency-rates*) 2.14549))


(defun generic_currency_converter (source-currency target-currency amount)
  (let ((exchange-rate (achieve-task-through-irs
                        'exchange_rate_provider
                        'exchange_rate_provision
                        'has_source_currency source-currency
                        'Has_target_currency target_currency)))
    (* exchange-rate amount)))


(def-irs-corba-interface generic_currency_converter  ;;this is the ontology name
  generic_currency_converter  ;;this is the method name
  ((has_source_currency "string" read-from-string)
   (has_target_currency "string")
   (has_amount "long"))
  "long"
   generic_currency_converter   ;;this is the lisp function
  )






