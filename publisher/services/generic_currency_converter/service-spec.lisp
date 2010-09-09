;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")


(in-ontology generic_currency_converter)

(def-class generic_currency_conversion  (goal-specification-task)?task
  ((has-input-role :value has_source_currency 
                   :value has_target_currency
                   :value has_amount)
   (has-output-role :value has_exchange_rate)
   (has_source_currency :type currency :cardinality 1)
   (has_target_currency :type currency :cardinality 1)
   (has_amount :type positive-number)
   (has_exchange-rate :type positive-number)
   (has-precondition 
    :value (kappa (?psm)
                  (and (positive-number (role-value ?psm has_amount))
                       (currency (role-value ?psm has_source_currency))
                       (currency (role-value ?psm has_target_currency)))))
   (has-goal-expression 
    :value (kappa (?psm ?sol)
                  (positive-number ?sol)))))


(def-class generic_currency_converter  (primitive-method) ?psm
  ((has-input-role :value has_source_currency 
                   :value has_target_currency
                  :value has_amount )
   (has-output-role :value has_exchange_rate)
   (has_source_currency :type currency :cardinality 1)
   (has_target_currency :type currency :cardinality 1)
   (has_amount :type positive-number)
   (has_exchange-rate :type positive-number)
   (has-postcondition 
    :value (kappa (?psm ?sol)
                  (positive-number ?sol))))
  
  :own-slots ((tackles-task-type   generic_currency_conversion)))
