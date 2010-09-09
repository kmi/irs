(in-package cl-user)


(defun select-change-of-circumstances-equipment-ontology ()
  (ocml::select-ontology 'ocml::change-of-circumstances-equipment-ontology))


(defun catalogue-entry-by-weight-interface-out (user-weight)
  ;;(setf ii input)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-equipment-ontology)
    (ocml::setofall '?item
                    `(and (ocml::max-user-weight ?item ?weight)
                          (ocml::> ?weight ,user-weight)))))

(defun redirect-equipment-to-new-address-interface-out (client contact-details)
  ;;(setf aa client bb contact-details)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-equipment-ontology)
    (setf client (make-ocml-item client)
          contact-details (make-ocml-item contact-details))
    (let ((name (ocml::findany '?x `(ocml::has-name ,client ?x)))
          (surname (ocml::findany '?x `(ocml::surname ,client ?x)))
          (post-code (ocml::findany '?x `(ocml::post-code ,contact-details ?x)))
          (street-number (ocml::findany '?x `(ocml::street-number ,contact-details ?x)))
          (phone-number (ocml::findany '?x `(ocml::phone-number ,contact-details ?x)))
          (street (ocml::findany '?x `(ocml::street ,contact-details ?x))))
      (format nil "\"Redirecting for ~a ~a to ~a ~a, post code: ~a\, phone number: ~a\""
              name surname street-number street post-code phone-number))))

(defun return-equipment-interface-out (client adaptation-item date)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-equipment-ontology)
    (setf client (make-ocml-item client) 
          adaptation-item (make-ocml-item adaptation-item)
          date (make-ocml-item date))
    (let ((name (ocml::findany '?x `(ocml::has-name ,client ?x)))
          (surname (ocml::findany '?x `(ocml::surname ,client ?x)))
          (day (ocml::findany '?x `(ocml::day-of ,date ?x)))
          (month (ocml::findany '?x `(ocml::month-of ,date ?x)))
          (year (ocml::findany '?x `(ocml::year-of ,date ?x))))
      (format nil "\"~a ~a's equipment ~:(~a~) returned on ~a ~a ~a\""
              name surname adaptation-item day month year))))


(defun find-items-matching-order-fun (order)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-equipment-ontology)
    (setf order (make-ocml-item order))
    (ocml::setofall '?x `(ocml::ordered-item ,order ?x))))


(defun get-client-weight-fun (client)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-equipment-ontology)
    (setf client (make-ocml-item client))
    (ocml::findany '?x `(ocml::weight ,client ?x))))

(defun get-client-impairment-fun (client)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-equipment-ontology)
    (setf client (make-ocml-item client))
    (ocml::findany '?x `(ocml::has-impairment ,client ?x))))

(defun find-items-matching-impairment-fun (impairment-descriptor)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-equipment-ontology)
    (setf impairment-descriptor (make-ocml-item impairment-descriptor))
    (ocml::setofall '?item
                    `(and (ocml::related-impairment ?care-type ,impairment-descriptor)
                          (ocml::used-for ?item ?care-type)
                          (ocml::care-item ?item)))))

(defun order-equipment-fun (adaptation-item)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-equipment-ontology)
    (setf adaptation-item (make-ocml-item adaptation-item))
    (let ((supplier-name
                (web-onto::findany
                 '?name
                 `(and (ocml::main-supplier ,adaptation-item ?supplier)
                       (ocml::has-name ?supplier ?name)))))
    (if supplier-name 
          (format nil "\"~:(~a~) has been orderd from ~:(~a~)\""
                  adaptation-item supplier-name)
      (format nil "\"Sorry, couldn't find a supplier for ~:(~a~).\"" adaptation-item)))))

(eval-when (eval load)
  (irs-wsmo-web-service-registration change-of-circumstances-equipment-web-services
                                     get-client-weight-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-equipment-web-services
                                     get-client-impairment-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-equipment-web-services
                                     find-items-matching-impairment-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-equipment-web-services
                                     find-items-matching-order-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-equipment-web-services
                                     find-items-matching-weight-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-equipment-web-services
                                     list-intersection-web-service)
  (irs-wsmo-web-service-registration 
   change-of-circumstances-equipment-web-services
   impairment-to-list-intersection-mediation-service-web-service)
  (irs-wsmo-web-service-registration
   change-of-circumstances-equipment-web-services
   weight-to-list-intersection-mediation-service-web-service)
  (irs-wsmo-web-service-registration
   change-of-circumstances-equipment-web-services
   order-equipment-web-service)

  (irs-wsmo-web-service-registration
   change-of-circumstances-equipment-web-services
   redirect-equipment-to-new-address-web-service)
  (irs-wsmo-web-service-registration
   change-of-circumstances-equipment-web-services
   return-equipment-web-service))


