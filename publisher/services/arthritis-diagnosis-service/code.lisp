(in-package cl-user)


(defun diagnose (&optional (obs1 'ocml::severe-joint-pain)
                  (obs2 'ocml::severe-joint-stiffness)
                  (obs3 'ocml::severe-swelling)
                  (obs4 'ocml::joint-creaking))
  ;;(setf aaa (list obs1 obs2 obs3 obs4))
  (let ((ocml::*current-ontology* ocml::*current-ontology*)
        (ocml::*current-ontologies* ocml::*current-ontologies*)
        (ocml::*defined-relations* ocml::*defined-relations*)
        (ocml::*axioms* ocml::*axioms*)
        (ocml::*defined-functions* ocml::*defined-functions*)
        (ocml::*bc-rules* ocml::*bc-rules*)
        (ocml::*domain-classes* ocml::*domain-classes*) instances)
    (declare (special ocml::*current-ontology* ocml::*current-ontologies*
                      ocml::*defined-relations* ocml::*axioms*
                      ocml::*defined-functions* ocml::*bc-rules*
                      ocml::*domain-classes*))
  (ocml::select-ontology 'ocml::arthritis-diagnosis-service-ontology)
  (setf instances 
        (make-observable-instances obs1 obs2 obs3 obs4))
  (internal-diagnose instances)))

(defun internal-diagnose (&optional (obs '(obs1 obs2 obs3 obs4))
                                    (match-criterion 'ocml::default-match-criterion)
                                    (solution-admissibility-criterion 
                                     'ocml::DEFAULT-SOLUTION-ADMISSIBILITY-criterion))
  (ocml::flat-optimal-classify 
   :observables obs
   :domain-name 'ocml::arthritis-domain
   :task-type 'ocml::optimal-classification-task
   :method-type 'ocml::non-hierarchical-optimal-classifier
   :match-criterion match-criterion
   :solution-admissibility-criterion solution-admissibility-criterion))

;;(def-instance obs1 observable
;;  ((has-observable-feature severe-joint-pain)
;;   (has-observable-value yes)))

(defun make-ocml-symbol (x)
  (intern (symbol-name x) (find-package "OCML")))

(defun make-observable-instances (&rest instance-values)
  (mapcar #'(lambda (instance-value)
              (let ((name (intern (symbol-name (gensym "OBS-")) (find-package "OCML"))))
                (ocml::define-domain-instance name
                                              'ocml::observable
                                              ""
                                              `((ocml::has-observable-feature 
                                                 ,(make-ocml-symbol instance-value))
                                                (ocml::has-observable-value ocml::yes)))
                name))
          instance-values))
                                            



(eval-when (eval load)
  (irs-method-registration arthritis-diagnosis-service-ontology ;;this is the ontology name
                            arthritis-diagnosis-service  ;;this is the method name
                            diagnose  ;;this is the function name
                            ))

#|
(achieve-task-through-irs 'fast-arthritis-diagnosis-psm 'arthritis-diagnosis
                            '(ocml::observable1 ocml::severe-joint-pain)
                            '(ocml::observable2 ocml::severe-joint-stiffness)
                            '(ocml::observable3 ocml::severe-swelling)
                            '(ocml::observable4 ocml::joint-creaking))
|#