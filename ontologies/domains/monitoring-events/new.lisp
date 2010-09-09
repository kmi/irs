;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology monitoring-events)

(def-class event ( )
((has-process-instance-id :type string)
(has-timestamp :type list-date-and-time)
(generated-by :type string))
)


