;;; Mode: Lisp; Package: cl-user

(in-package cl-user)


(def-ocml-corba-mapping hospital_bed_service  HospitalBedService
 ((has_medical_service symbol)
  (has_hospital_name symbol)
  )
 has_first_available_date structure
 :if-fails "FAILURE")
