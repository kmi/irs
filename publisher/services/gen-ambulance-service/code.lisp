;;; Mode: Lisp; Package: cl-user

(in-package cl-user)



(defun  air_ambulance_service (start destination currency)
  (shipping-patient-delay)
  3000)

(defun  rail_ambulance_service (start destination currency)
  (shipping-patient-delay)
  1500)



(eval-when (eval load)
  (irs-method-registration  GenAmbulanceService ;;this is the ontology name
                            air_ambulance_service  ;;this is the method name
                            air_ambulance_service  ;;this is the function name
                            )
  (irs-method-registration  GenAmbulanceService ;;this is the ontology name
                            rail_ambulance_service  ;;this is the method name
                            rail_ambulance_service  ;;this is the function name
                            ))

#|
(achieve-task-through-irs 'GenAmbulanceService 'ambulance_service
                          (List 'ocml::has_start_location 'milton-keynes)
                          (list 'ocml::has_destination_location 'paris)
                          (list 'ocml::has_currency 'pound))

;;;below doesn't work - need to be in the method ontology
(achieve-task-through-irs 'AmbulanceService 'ambulance_service
                          (List 'ocml::has_start_location 'milton-keynes)
                          (list 'ocml::has_destination_location 'paris)
                          (list 'ocml::has_currency 'pound))
|#
#|
(def-irs-corba-interface GenAmbulanceService ;;this is the ontology name
  air_ambulance_service  ;;this is the method name
  ((has_start_location "string" read-from-string)
   (has_destination_location "string" read-from-string)
   (has_currency "string" read-from-string))
  ;;"long"
  "long"
    air_ambulance_service ;;this is the lisp function
  )


(def-irs-corba-interface GenAmbulanceService ;;this is the ontology name
  rail_ambulance_service  ;;this is the method name
  ((has_start_location "string" read-from-string)
   (has_destination_location "string" read-from-string)
   (has_currency "string" read-from-string))
  ;;"long"
  "long"
   rail_ambulance_service ;;this is the lisp function
  )
|#








