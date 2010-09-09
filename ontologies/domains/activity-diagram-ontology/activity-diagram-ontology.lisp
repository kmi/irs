;;; Mode: Lisp; Package: ocml

(in-package "OCML")

(in-ontology activity-diagram-ontology)

(def-class ad-choreography ()
  ((has-header :type string)
   (has-activity-group :type ad-activity-group)
   (has-start-node     :type ad-node)))

(def-class ad-orchestration ()
  ((has-header :type string)
   (has-activity-group :type ad-activity-group)
   (has-start-node     :type ad-node)))

(def-class ad-activity-group ()
  ((has-node :type ad-node :min-cardinality 1)
   (has-edge :type ad-edge)
   (has-activity-group :type ad-activity-group)))
 
(def-class ad-interruptible-activity-region (ad-activity-group))

(def-class ad-node ())

(def-class ad-action-node (ad-node)
  ((has-pin         :type pin)))

; Abstract Class
(def-class ad-abstract-split (ad-node))

(def-class ad-decision (ad-abstract-split))

(def-class ad-fork (ad-abstract-split))

(def-class ad-join (ad-node))

(def-class ad-decision (ad-node))

(def-class ad-abstract-join (ad-node))

(def-class ad-merge (ad-abstract-join))

(def-class ad-join  (ad-abstract-join))

(def-class ad-send-event-action (ad-action-node)
  ((has-web-service :type (or web-service goal) :cardinality 1)
   (has-node :type ad-node)))

(def-class ad-accept-event-action (ad-action-node)
  ((has-web-service :type (or web-service goal) :cardinality 1)
   (has-node :type ad-node)))

(def-class ad-object-node (ad-node))

(def-class ad-pin ()
  "A pin is a connection for dataflow")

(def-class ad-input-pin (ad-pin))
(def-class ad-output-pin (ad-pin))

(def-class ad-general-action (ad-action-node)
  "A general action node")

(def-class ad-oo-mediator (ad-action-node)
  "A node representing an OO Mediator"
  ((has-oo-mediator :type oo-mediator :cardinality 1)))

(def-class ad-flow-start (ad-node))
(def-class ad-flow-final (ad-node))

(def-class ad-activity-final (ad-node))

(def-class ad-aggregation (ad-action-node)
  ((has-mediation-service :type (or web-service goal) :cardinality 1)))

(def-class ad-extraction (ad-action-node)
  ((has-mediation-service :type (or web-service goal) :cardinality 1)))

(def-class ad-operation (ad-action-node)
  ((has-goal :type goal)))
   

(def-class ad-edge ()
  ((has-node-source :type ad-node :cardinality 1)
   (has-node-target :type ad-node :cardinality 1)))

(def-class ad-dataflow-edge (ad-edge))

(def-class ad-control-flow-edge (ad-edge))

(def-class ad-decision-edge (ad-control-flow-edge)
  ((has-guard       :type string :max-cardinality 1)))





