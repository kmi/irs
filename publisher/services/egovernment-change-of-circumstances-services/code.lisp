;;; Mode: Lisp; Package: cl-user

(in-package cl-user)

(defun select-egovernment-change-of-circumstances-goals  ()
  (ocml::select-ontology 'ocml::egovernment-change-of-circumstances-goals))


(defun Create-Citizen-Record-To-Create-New-Assessment-For-Client-Mediation-Service-fun 
       (input)
  input)

(defun create-new-assessment-for-client-to-order-service-for-assessment-mediation-service-fun
       (input)
  input)

(defun Order-Service-For-Assessment-To-Get-Equipment-Assessment-Client-Impairment-Mediation-Service-fun (assessment)
  ;;(setf i11 input)
  (select-swift-goals-ontology) ;;;not sure why i have to choose swift goals(select-egovernment-change-of-circumstances-goals)
  (setf assessment (make-ocml-symbol assessment))
  (web-onto::findany '?x `(and (ocml::assessed-to ,assessment ?client)
                               (ocml::has-impairment ?client ?x))))

(defun Order-Service-For-Assessment-To-Get-Equipment-Assessment-Client-Weight-Mediation-Service-fun (assessment)
  ;;(setf i12  input)
  (select-swift-goals-ontology) ;;(select-egovernment-change-of-circumstances-goals) 
  (setf assessment (make-ocml-symbol assessment))
  (web-onto::findany '?x `(and (ocml::assessed-to ,assessment ?client)
                               (ocml::weight ?client ?x))))

(defun get-equipment-assessment-to-order-equipment-mediation-service-fun (input)
  input)

(eval-when (eval load)
  (irs-wsmo-web-service-registration 
   egovernment-change-of-circumstances-goals 
   Create-Citizen-Record-To-Create-New-Assessment-For-Client-Mediation-Service-Web-Service)
  (irs-wsmo-web-service-registration
   egovernment-change-of-circumstances-goals
   create-new-assessment-for-client-to-order-service-for-assessment-mediation-service-web-service)
  (irs-wsmo-web-service-registration
   egovernment-change-of-circumstances-goals
   Order-Service-For-Assessment-To-Get-Equipment-Assessment-Client-Impairment-Mediation-Service-web-service)
  (irs-wsmo-web-service-registration
   egovernment-change-of-circumstances-goals
   Order-Service-For-Assessment-To-Get-Equipment-Assessment-Client-Weight-Mediation-Service-web-service)
  (irs-wsmo-web-service-registration
   egovernment-change-of-circumstances-goals
   get-equipment-assessment-to-order-equipment-mediation-service-web-service)
  )

