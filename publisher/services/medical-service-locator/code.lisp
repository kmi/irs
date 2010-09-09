;;; Mode: Lisp; Package: cl-user

(in-package cl-user)

(defun make-ocml-symbol (x)
  (intern (symbol-name x) (find-package "OCML")))


(defun medical_service_locator (service)
  (shipping-patient-delay)
;;  (setf s service)
  (let ((temp ocml::*current-ontology*))
    (unwind-protect 
      (progn 
        (ocml::select-ontology 'ocml::medical-services)  
        ;;(format nil "~A"
        (ocml::setofall 'ocml::?x 
                             `(ocml::provides-medical-service 
                               ocml::?x 
                               ,(make-ocml-symbol service))))
      (ocml::switch-to-ontology temp))))

(eval-when (eval load)
  (irs-method-registration medicalservicelocator ;;this is the ontology name (must be for the method)!!
                            medical_service_locator  ;;this is the method name
                            medical_service_locator ;;this is the function name
                            ))

#|

;;;note ontology must be the one for the method!!!
(achieve-task-through-irs 'medicalservicelocator 'medical_service_location
                          (List 'ocml::has_medical_service 'hip-replacement))
      

(def-irs-corba-interface MedicalServiceLocator ;;this is the ontology name
  medical_service_locator  ;;this is the method name
  ((has_medical_service "string" read-from-string)
   
   )
  ;;"long"
  "string"
   medical_service_locator  ;;this is the lisp function
  )
|#




