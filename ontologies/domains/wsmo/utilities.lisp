;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology wsmo)


(def-class run-ocml-expression-goal-non-functional-properties
  (non-functional-properties)
  nil)


(def-class run-ocml-expression-goal
  (goal)
  ?goal
  ((has-input-role :value has-variables :value has-expression)
   (has-input-soap-binding
    :value
    (has-variables "sexpr")
    :value
    (has-expression "sexpr"))
   (has-output-role :value has-evaluation-result)
   (has-output-soap-binding
    :value
    (has-evaluation-result "sexpr"))
   (has-variables :type list)
   (has-expression :type list)
   (has-evaluation-result :type expression)
   (has-non-functional-properties
    :value
    run-ocml-expression-goal-non-functional-properties)))

(def-class run-ocml-expression-web-service-non-functional-properties
  (non-functional-properties)
  nil)

(def-class run-ocml-expression-web-service
  (web-service)
  ?web-service
  ((has-capability
    :value
    run-ocml-expression-web-service-capability)
   (has-internal-method :value irs-run-ocml-expression)
   (has-interface
    :value
    run-ocml-expression-web-service-interface)
   (has-non-functional-properties
    :value
    run-ocml-expression-web-service-non-functional-properties)))

(def-class run-ocml-expression-web-service-capability-non-functional-properties
  (non-functional-properties)
  nil)

(def-class run-ocml-expression-web-service-capability
  (capability)
  ?capability
  ((used-mediator :value un-ocml-expression-mediator)
   (has-non-functional-properties
    :value
    run-ocml-expression-web-service-capability-non-functional-properties)))

(def-class run-ocml-expression-web-service-interface-non-functional-properties
  (non-functional-properties)
  nil)

(def-class run-ocml-expression-web-service-interface-choreography
  (choreography)
  ((has-grounding :value ((normal dummy-function)))))

(def-class run-ocml-expression-web-service-interface-orchestration-problem-solving-pattern
  (problem-solving-pattern)
  nil)

(def-class run-ocml-expression-web-service-interface-orchestration
  (orchestration)
  ((has-problem-solving-pattern
    :value
    run-ocml-expression-web-service-interface-orchestration-problem-solving-pattern)))

(def-class run-ocml-expression-web-service-interface
  (interface)
  ?interface
  ((has-choreography
    :value
    run-ocml-expression-web-service-interface-choreography)
   (has-orchestration
    :value
    run-ocml-expression-web-service-interface-orchestration)
   (has-non-functional-properties
    :value
    run-ocml-expression-web-service-interface-non-functional-properties)))

(def-class run-ocml-expression-mediator-non-functional-properties
  (non-functional-properties)
  nil)

(def-class run-ocml-expression-mediator
  (wg-mediator)
  ?mediator
  ((has-source-component :value run-ocml-expression-goal)
   (has-target-component
    :value
    run-ocml-expression-web-service)
   (has-non-functional-properties
    :value
    run-ocml-expression-mediator-non-functional-properties)))