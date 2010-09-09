;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology upo-1-4-20080109)

(def-class thing ( )
((has-variant-name :type string)
(has-pretty-name :type string))
)

(def-class business-process-mediator ( thing ))

(def-class gender ( thing ))

(def-class process-orchestration-formalism ( thing ))

(def-class process-orchestration-specification ( thing )
((has-process-orchestration-element :type process-orchestration-element)
(is-expressed-in :type process-orchestration-formalism)
(has-specification-completeness :type specification-completeness :max-cardinality 1)
(has-process-orchestration-specification-type :type process-orchestration-specification-type))
)

(def-class resource ( thing ))

(def-class temporal-thing ( thing )
((has-time-interval :type time-interval))
)

(def-class logical-expression ( thing ))

(def-class organisation-size ( thing ))

(def-class process-orchestration-element ( thing ))

(def-class specification-completeness ( thing ))

(def-class intangible-thing ( thing ))

(def-class process-orchestration-specification-type ( thing ))

(def-class event ( temporal-thing )
((has-location :type location)
(has-sub-event :type event)
(has-other-agents-involved :type agent)
(has-main-agent :type agent))
)

(def-class explicit-process-orchestration-specification ( process-orchestration-specification-type ))

(def-class business-domain ( intangible-thing ))

(def-class effect ( logical-expression ))

(def-class business-process-metric ( intangible-thing ))

(def-class role ( intangible-thing ))

(def-class business-policy ( intangible-thing ))

(def-class business-activity ( intangible-thing )
((has-involved-role :type role)
(has-ongoing-business-effect :type effect)
(has-pre-condition :type pre-condition)
(has-involved-object :type thing)
(has-involved-agent :type agent)
(has-final-business-effect :type effect)
(has-model-purpose :type business-activity-model-purpose))
)

(def-class implicit-process-orchestration-specification ( process-orchestration-specification-type ))

(def-class pre-condition ( logical-expression ))

(def-class business-function ( intangible-thing ))

(def-class time-position ( intangible-thing )
((in-timezone :type string))
)

(def-class business-activity-model-purpose ( intangible-thing ))

(def-class tangible-thing ( temporal-thing ))

(def-class business-process-goal ( intangible-thing ))

(def-class business-strategy ( intangible-thing ))

(def-class agent ( tangible-thing )
((has-email-address :type string)
(has-description :type string)
(has-web-address :type string))
)

(def-class time-interval ( time-position )
((ends-at-time-instant :type time-instant :max-cardinality 1)
(begins-at-time-instant :type time-instant :max-cardinality 1))
)

(def-class business-event ( event )
((generates-resource :type resource)
(has-involved-business-activity :type business-activity)
(has-involved-resource :type resource)
(has-owner :type agent)
(consumes-resource :type resource))
)

(def-class time-instant ( time-position )
((hour-of :type integer)
(second-of :type integer)
(micro-second-of :type integer)
(month-of :type integer)
(year-of :type integer)
(day-of :type integer)
(minute-of :type integer)
(milli-second-of :type integer))
)

(def-class location ( tangible-thing ))

(def-class atomic-business-activity ( business-activity ))

(def-class composite-business-activity ( business-activity )
((has-process-orchestration-specification :type process-orchestration-specification))
)

(def-class business-process-model ( composite-business-activity ))

(def-class legal-agent ( agent )
((has-fax-number :type string)
(has-telephone-number :type string)
(has-postal-address :type string))
)

(def-class organisation-unit ( agent )
((has-sub-unit :type organisation-unit)
(has-postal-address :type string)
(has-affiliated-person :type person)
(has-fax-number :type string)
(has-size :type organisation-size)
(is-unit-of-organisation :type organisation)
(is-sub-unit-of-organisation-unit :type organisation-unit)
(is-headed-by :type person)
(has-telephone-number :type string))
)

(def-class person ( legal-agent )
((has-family-name :type string)
(has-given-name :type string)
(has-full-name :type string)
(has-gender :type gender))
)

(def-class organisation ( legal-agent )
((has-size :type organisation-size)
(has-sub-unit :type organisation-unit)
(is-headed-by :type person)
(has-affiliated-person :type person)
(organisation-is-part-of :type organisation))
)

(def-instance constraint-based-process-orchestration-specification implicit-process-orchestration-specification
)

(def-instance control-flow-based-process-orchestration-specification explicit-process-orchestration-specification
)

(def-instance document-current-state business-activity-model-purpose
)

(def-instance rule-based-process-orchestration-specification implicit-process-orchestration-specification
)

(def-instance data-flow-based-process-orchestration-specification explicit-process-orchestration-specification
)

(def-instance document-intended-state business-activity-model-purpose
)

(def-instance incomplete-specification specification-completeness
)

(def-instance complete-specification specification-completeness
)


