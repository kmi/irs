;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology see-events)


(def-class see-event (event)
((has-session-id :type string))
)

(def-class see-process-event (see-event)

)

;; make this strings for now
(def-class data-value ()
((has-slot-name :type string)
 (has-value :type string))
)

(def-class achieve-goal-event (see-process-event)
((has-goal :type goal-meta-class))
)


(def-class start-achieve-goal-event (achieve-goal-event)
((has-input-data :type data-value))
)

#|
(def-instance event-test start-achieve-goal
  ((ocml::has-goal ocml::get-gis-data-goal)
   (ocml::has-input-data "some input"))
)
|#

(def-class end-achieve-goal-event (achieve-goal-event)
((has-output-data :type data-value))
 ;;(has-output-data :type string))
)

(def-class invoke-web-service-event (see-process-event)
((has-web-service :type web-service-type))
)

(def-class start-invoke-web-service-event (invoke-web-service-event)
((has-input-data :type data-value))
)

(def-class end-invoke-web-service-event (invoke-web-service-event)
((has-output-data :type data-value))
)

(def-class see-data-event (see-event)

)

(def-class achieve-goal-request (see-data-event)
((has-goal :type goal-meta-class)
 (has-input-data :type data-value))
)

(def-class achieve-goal-result (see-data-event)
((has-goal :type goal-meta-class)
 (has-output-data :type data-value))
)

(def-class invoke-web-service-request (see-data-event)
((has-web-service :type web-service-type)
 (has-input-data :type data-value))
)

(def-class invoke-web-service-result (see-data-event)
((has-web-service :type web-service-type)
 (has-output-data :type data-value))
)




