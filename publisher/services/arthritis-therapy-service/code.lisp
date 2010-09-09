(in-package cl-user)




(defun arthritis-therapy (condition)
  (shipping-patient-delay)
  (let ((ocml::*current-ontology* ocml::*current-ontology*)
        (ocml::*current-ontologies* ocml::*current-ontologies*)
        (ocml::*defined-relations* ocml::*defined-relations*)
        (ocml::*axioms* ocml::*axioms*)
        (ocml::*defined-functions* ocml::*defined-functions*)
        (ocml::*bc-rules* ocml::*bc-rules*)
        (ocml::*domain-classes* ocml::*domain-classes*))
    (declare (special ocml::*current-ontology* ocml::*current-ontologies*
                      ocml::*defined-relations* ocml::*axioms*
                      ocml::*defined-functions* ocml::*bc-rules*
                      ocml::*domain-classes*))
    (ocml::select-ontology 'ocml::arthritis-diagnostic-fast)
    (first 
     (ocml::ocml-eval-gen `(ocml::the-class-slot-value 
                            ',(make-ocml-symbol condition)
                            'ocml::can-be-treated-by)))))
                                            



(eval-when (eval load)
  (irs-method-registration arthritis-therapy-service-ontology ;;this is the ontology name
                            arthritis-therapy-service  ;;this is the method name
                            arthritis-therapy  ;;this is the function name
                            ))

#|
(achieve-task-through-irs 'fast-arthritis-diagnosis-psm 'arthritis-diagnosis
                            '(ocml::observable1 ocml::severe-joint-pain)
                            '(ocml::observable2 ocml::severe-joint-stiffness)
                            '(ocml::observable3 ocml::severe-swelling)
                            '(ocml::observable4 ocml::joint-creaking))
|#