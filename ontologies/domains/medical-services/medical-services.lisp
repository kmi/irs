;;; Mode: Lisp; Package: ocml

(in-package "OCML")


(in-ontology medical-services)


;;;HOSPITAL - redefined from medical-ontoloy
;;;
(def-class hospital (health-care-organization)
   ((provides-medical-service :type medical-service)))

(def-class therapy (clinical-procedure))

(def-class medical-service (therapy)
   ())

(def-class in-patient-medical-service (medical-service)
   ((requires-hospitalization? :value no)))

(def-class out-patient-medical-service (medical-service)
   ((requires-hospitalization? :value yes)
    (average-length-of-stay-in-hospital :type duration)))


(def-instance hip-replacement out-patient-medical-service
   ((average-length-of-stay-in-hospital '(7 day))))

(def-instance hip-hip-hospital hospital
   ((provides-medical-service hip-replacement)))

(def-instance the-hippy-hospital hospital
   ((provides-medical-service hip-replacement)))

(def-instance another-hippy-hospital hospital
   ((provides-medical-service hip-replacement)))

(def-instance not-the-hippy-hospital hospital
   ((provides-medical-service physio-therapy)))