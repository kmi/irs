;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")


(in-ontology exchange-rate-provider-ontology)

(def-class exchange-rate-provision  (goal-specification-task)?task
  ((has-input-role :value has-source-currency 
                   :value has-target-currency
                   )
   (has-output-role :value has-exchange-rate)
   (has-source-currency :type currency :cardinality 1)
   (has-target-currency :type currency :cardinality 1)
   (has-exchange-rate :type positive-number)
   (has-precondition 
    :value (kappa (?psm)
                  (and (currency (role-value ?psm has-source-currency))
                       (currency (role-value ?psm has-target-currency)))))
   (has-goal-expression 
    :value (kappa (?psm ?sol)
                  (positive-number ?sol)))))


(cl-user::def-irs-soap-bindings exchange-rate-provider-ontology ;;this is the ontology name
  exchange-rate-provision  ;;this is the task name
  ((has-source-currency "xsd:sexpr") 
   (has-target-currency "xsd:sexpr")
   )
  "xsd:float"
  )


(def-class exchange-rate-provider (primitive-method) ?psm
  ((has-input-role :value has-source-currency 
                   :value has-target-currency
                   )
   (has-output-role :value has-exchange-rate)
   (has-source-currency :type currency :cardinality 1)
   (has-target-currency :type currency :cardinality 1)
   (has-exchange-rate :type positive-number)
   (has-precondition 
    :value (kappa (?psm)
                  (and (currency (role-value ?psm has-source-currency))
                       (currency (role-value ?psm has-target-currency)))))
   (has-postcondition 
    :value (kappa (?psm ?sol)
                  (positive-number ?sol))))
  
  :own-slots ((tackles-task-type  exchange-rate-provision)))
