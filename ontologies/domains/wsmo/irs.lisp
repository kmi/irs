;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology wsmo)

;;;goals and web services which implement IRS III

;;
(def-class mediate-input-role-values-goal (goal)
  "This goal transforms the inputs of a goal to the inputs of a web service."
  ((has-input-soap-binding 
    :value (has-target-goal "sexpr")
    :value (has-web-service "sexpr")
    :value (has-mediation-ontology "sexpr")
    :value (has-input-role-values "sexpr")
    :value (has-input-roles "sexpr"))
   (has-output-soap-binding 
    :value (has-mediated-input-role-values "sexpr"))
   (has-input-role :value has-target-goal
                   :value has-web-service
                   :value has-input-role-values
                   :value has-input-roles
                   :value has-mediation-ontology)
   (has-output-role 
    :value has-mediated-input-role-values)
   (has-web-service :type web-service-type)
   (has-input-role-values :type list)
   (has-input-roles :type list)
   (has-target-goal :type goal)
   (has-mediation-ontology :type ontology)
   (has-mediated-input-role-values :type list)
   (has-post-condition 
    :value 
    (kappa (goal) 
           (are-mediationally-equivalent
            (has-role-value goal has-input-role-values)
            (has-role-value goal has-mediated-input-role-values))))))

(def-class mediate-input-role-values-mediator (wg-mediator)
  ((has-source-component :value mediate-input-role-values-goal)))

(def-class mediate-input-role-values-web-service (web-service)
  ((has-internal-method :value irs-input-role-values-mediator)
   (has-capability :value mediate-input-role-values-capability)))

(def-class mediate-input-role-values-capability (capability)
  ((used-mediator :value mediate-input-role-values-mediator)))

(def-class find-web-services-for-goal (goal)
  "This goal finds the web services which can solve a goal"
  ((has-input-role :value has-goal
                   :value has-ontology)
   (has-input-soap-binding 
    :value (has-goal "sexpr")
    :value (has-ontology "sexpr"))
   (has-output-role :value associated-web-services)
   (associated-web-services :type list)
   (has-output-soap-binding 
    :value (associated-web-services "sexpr"))
   (has-goal :type goal-type)
   (has-ontology :type ontology)
   (has-post-condition 
    :value 
    (kappa (goal) 
           (and (member web-service 
                        (has-role-value goal associated-web-services))
                (can-solve-goal 
                 (has-role-value goal has-goal) web-service))))))

(def-class find-web-services-for-goal-mediator (wg-mediator)
  ((has-source-component :value find-web-services-for-goal)))

(def-class find-web-services-for-goal-web-service (web-service)
  ((has-internal-method :value web-services-which-solve-goal-internal-method)
   (has-capability :value find-web-services-for-goal-web-service-capability)))

(def-class find-web-services-for-goal-web-service-capability (capability)
  ((used-mediator :value find-web-services-for-goal-mediator)))

(def-class suitable-web-service-goal (goal)
  "This goal checks if a web service is suitable for a goal by checking its 
applicability condition"
  ((has-input-role 
    :value has-goal
    :value has-actual-role-pairs
    :value has-web-service
    :value has-combined-oo-mediator-ontology)
   (has-input-soap-binding 
    :value (has-goal "sexpr")
    :value (has-actual-role-pairs "sexpr")
    :value (has-web-service "sexpr")
    :value (has-combined-oo-mediator-ontology "sexpr"))
   (has-output-role 
    :value has-goal-and-web-service-instances)
   (has-goal-and-web-service-instances :type list)
   (has-output-soap-binding 
    :value (has-goal-and-web-service-instances "sexpr"))
   (has-goal :type goal-type)
   (has-actual-role-pairs :type list)
   (has-web-service :type web-service-type)
   (has-combined-oo-mediator-ontology :type ontology)
   (has-post-condition 
    :value 
    (kappa (goal) 
           (is-suitable-for-goal
            (instantiate (has-role-value goal has-goal) 
                         (has-role-value goal has-actual-role-pairs))
            web-service)))))

(def-class suitable-web-service-mediator (wg-mediator)
  ((has-source-component :value suitable-web-service-goal)))

(def-class suitable-web-service-web-service (web-service)
  ((has-internal-method :value suitable-web-service-internal-method)
   (has-capability :value suitable-web-service-capability)))

(def-class suitable-web-service-capability (capability)
  ((used-mediator :value suitable-web-service-mediator)))