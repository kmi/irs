;;; Copyright © 2008 The Open University

(in-package "OCML")

(in-ontology wsmo)

(def-class problem-solving-pattern ()
  ((has-body :type unary-procedure)))

(def-class orchestration ()
  ((has-problem-solving-pattern :type problem-solving-pattern)))

;;;relation for orchestration
(def-relation wsmo-web-service-orchestration-body2 (?web-service ?body)
  :sufficient 
  (or (and (instance ?web-service)
           (wsmo-web-service-orchestration-body2 
            (the-parent ?web-service) ?body))
      (and (class ?web-service)
           (subclass-of ?web-service web-service)
           (or (and (= ?interface 
                       (the-class-slot-value ?web-service has-interface))
                    (class ?interface)
                    (= ?orchestration
                       (the-class-slot-value ?interface has-orchestration))
                    (class ?orchestration)
                    (= ?problem-solving-pattern 
                       (the-class-slot-value 
                        ?orchestration has-problem-solving-pattern))
                    (class ?problem-solving-pattern)
                    (= ?body (the-class-slot-value 
                              ?problem-solving-pattern has-body)))))
      (and (direct-subclass-of ?web-service ?web-service-super) 
           (wsmo-web-service-orchestration-body2 ?web-service-super ?body))))

(def-relation goal-associated-mediator (?goal ?mediators)
  "Gets the mediators used by a goal"
  :sufficient  
  (or (and (instance ?goal)
           (goal-associated-mediator (the-parent ?goal)
                                     ?mediators))
      (and (class ?goal)
           (subclass-of ?goal goal)
           (= ?mediators 
              (append
               (all-class-slot-values ?goal used-mediator)
               (setofall 
                ?m
                (and (subclass-of ?m mediator)
                     (class ?m)
                     (= ?goal
                        (the-class-slot-value 
                         ?m has-target-component)))))))))


(def-relation wsmo-mediator-source (?mediator ?source)
  "Gets the source associated with a mediator"
  :sufficient  
  (or (and (instance ?mediator)
           (wsmo-mediator-source (the-parent ?mediator) ?source))
      (and (class ?mediator)
           (subclass-of ?mediator mediator)
           (= ?source (the-class-slot-value 
                       ?mediator has-source-component)))))

(def-relation wsmo-mediator-target (?mediator ?target)
  "Gets the target associated with a mediator"
  :sufficient  
  (or (and (instance ?mediator)
           (wsmo-mediator-target (the-parent ?mediator) ?source))
      (and (class ?mediator)
           (subclass-of ?mediator mediator)
           (= ?target (the-class-slot-value ?mediator 
                                            has-target-component)))))

;;;procedure for orchestration

(def-procedure set-goal-value (?instance ?slot ?value)
  :body (do
            (unassert (list-of ?slot ?instance '?any))
            (tell (list-of ?slot ?instance ?value))))

(def-function orch-get-goal-value (?goal-name)
  :lisp-fun
  #'(lambda (goal-name) 
      (get-last-achieve-goal-value goal-name)))

(def-function orch-get-all-goal-values (?goal-name)
  :lisp-fun
  #'(lambda (goal-name) 
      (get-all-achieve-goal-values goal-name)))


(defun generate-source (inst)
  (let ((objects (ocml::slot-values inst 'ocml::has-objects)))
    (ocml::generate-ocml-instances-source (cons inst objects))))


;;;added by alessio to manage instance exchange between server and publisher
(def-function orch-get-instance-name (?inst ?ontology)
  :lisp-fun
  #'(lambda (inst ontology)
      (ocml::select-ontology ontology)
      (ocml::generate-ocml-instances-source 
       (cons (first inst) (ocml::slot-values (first inst) 'ocml::has-objects)))))

(defun eval-ocml-instance (x)
  (let ((*package* (find-package "OCML")))
    (eval (make-ocml-object x))))

(def-function orch-get-instance-value (?inst-list ?ontology)
 :lisp-fun
 #'(lambda (inst-list ontology)
     (ocml::select-ontology ontology)
     (eval-ocml-instance (first (cdr inst-list)))
     (car inst-list)))

(def-function progn (&rest forms)
  :lisp-fun 
  #'(lambda (&rest forms)
      (car (last forms))))

(def-function orch-seq (&rest ?goals) -> ?sequence-result
  :lisp-fun 
  #'(lambda (&rest goals) 
      (internal-sequence-execution goals)))

(defvar *sequence-results* nil)

(def-function orch-parallel (&rest ?goals) -> ?sequence-result
  :lisp-fun 
  #'(lambda (&rest goals) 
      (let ((*sequence-results* nil))
        (internal-sequence-execution goals)
        (reverse *sequence-results*))))

(def-function achieve-goal (?goal-type &rest ?goal-values) 
  :lisp-fun 
  #'(lambda (goal-type &rest goal-values)
      (internal-achieve-goal goal-type goal-values)))








