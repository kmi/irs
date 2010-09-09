;;; Mode: Lisp; Package: ocml

(in-package "OCML")

(in-ontology cashew-ontology)

;;; Should be constrained to be an abstract superclass
(def-class workflow (problem-solving-pattern invokable-entity)
  "The superclass to all Cashew workflow types. cf. 'Process' in the
original Cashew-S paper."
  ((has-dataflow-mediator :type dataflow-mediator)))


(def-class dataflow-mediator (mediator)
  "The superclass to all dataflow connection types in Cashew
workflows")

(def-class pp-mediator (dataflow-mediator)
  "A dataflow connection between two peer performances (i.e. composed
in same workflow). cf. 'Connect' in original Cashew-S paper."
  ((has-source-component :type perform :cardinality 1)
   (has-target-component :type perform :cardinality 1)))

(def-class pf-mediator (dataflow-mediator)
  "A dataflow connection from a performance to its parent workflow.
cf. 'Producer' in original Cashew-S paper."
  ((has-source-component :type (or perform workflow) :cardinality 1)
   (has-target-component :type (or perform workflow) :cardinality 1)))


(def-class perform ()
  "The superclass to all types of performance in Cashew, being the
unit of composition in a workflow")

(def-class receive (perform)
  "Accepts a message"
  ((has-output-role :type role))) ;; :cardinality 1)))

(def-class send (perform)
  "Sends a message"
  ((has-input-role :type role))) ;; :cardinality 1)))

(def-class perform-workflow (perform)
  "A performance of a (sub-)workflow"
  ((has-workflow :type workflow :cardinality 1)))

(def-class perform-goal (perform)
  "A performance of a goal"
  ((has-goal :type goal :cardinality 1)))


(def-class sequential (workflow)
  "A workflow performing its (list of) components in a sequence"
  ((has-perform-list :type perform-list :cardinality 1)))

(def-class perform-list (list)
  ((has-element-type :value perform)))

(def-class concurrent (workflow)
  "A workflow performing its components concurrently. cf. SplitJoin in original Cashew-S paper."
  ((has-perform-list :type perform-list :cardinality 1)))

(def-class interleaved (workflow)
  "A workflow performing its components interleavedly. cf. AnyOrder in original Cashew-S paper."
  ((has-perform-list :type perform-list :cardinality 1)))

(def-class xor-choice (workflow)
  "A workflow that resolves a choice between alternatives, using an
explicit condition, and performs exactly one"
  ((decision :type unary-kappa-expression :cardinality 1)
   (if-true :type perform :cardinality 1)
   (if-false :type perform :cardinality 1)))

(def-class deferred-choice (workflow)
  "A workflow that resolves a choice between alternatives, using their
readiness, and performs exactly one"
  ((has-perform-list :type perform-list :cardinality 1)))

(def-class xor-while (workflow)
  "A workflow that performs some number of iterations of the body
workflow, according to the value of an explicit logical decision"
  ((decision :type unary-kappa-expression :cardinality 1)
   (body :type perform :cardinality 1)))

(def-class deferred-while (workflow)
  "A workflow that performs some number of iterations of its body,
according to the readiness of an alternative"
  ((break :type perform :cardinality 1)
   (body :type perform :cardinality 1)))

(def-class xor-until (workflow) 
  "A workflow that performs some positive number of iterations of its
body, subsequent iterations according to the value of an explicit
logical decision"
  ((body :type perform :cardinality 1)
   (decision :type unary-kappa-expression :cardinality 1)))

(def-class deferred-until (workflow)
  "A workflow that performs some positive number of iterations of its
body, subsequent iterations according to the readiness of some
alternative"
  ((body :type perform :cardinality 1)
   (break :type perform :cardinality 1)))


(def-relation has-perform-list (?thing ?perform-list)
  "This definition generalises the notion of 
   'having a perform list' to classes as well 
    as workflow instances."
  :sufficient (or (and (instance ?thing)
                       (has-perform-list (the-parent ?thing) ?perform-list))
                  (and (class ?thing)
                       (member ?perform-list (all-class-slot-values
                                              ?thing has-perform-list)))))

(def-relation has-perform (?thing ?perform)
  "This definition generalises the notion of 
   'having a component perform' to all workflow 
   classes, not just those with this as a direct attribute, 
   as well as to their instances"
  :sufficient (or (and (instance-of ?thing 'workflow)
                       (has-perform (the-parent ?thing) ?perform))
                  (or (and (subclass-of ?thing 'sequential)
                           (member ?perform (the-class-slot-value
                                             ?thing has-perform-list)))
                      (or (and (subclass-of ?thing 'int-choice)
                               (or (= ?perform (the-class-slot-value
                                                ?thing if-true))
                                   (= ?perform (the-class-slot-value
                                                ?thing if-false))))
                          (or (and (or (subclass-of ?thing 'int-while)
                                       (subclass-of ?thing 'int-until))
                                   (= ?perform (the-class-slot-value
                                                ?thing body)))
                              (or (and (or (subclass-of ?thing 'ext-while)
                                           (subclass-of ?thing 'ext-until))
                                       (or (= ?perform (the-class-slot-value
                                                        ?thing break))
                                           (= ?perform (the-class-slot-value
                                                        ?thing body))))
                                  (or (and (subclass-of ?thing 'workflow) ;;otherwise concurrent, interleaved, ext-choice
                                           (member ?perform (all-class-slot-values
                                                             ?thing has-perform))))))))))

(def-function run-cashew-orchestration (?wf)
  :lisp-fun 
  #'(lambda (wf)
      (let ((wfi (cl-user::instantiate-workflow wf)))
        (cl-user::execute-workflow-instance wfi))))

(def-function define-cashew-orchestration (?def)
  :lisp-fun
  #'(lambda (def)
      (cl-user::parse-cashew def)))
