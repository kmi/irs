;;; Mode: Lisp; Package: cl-user

(in-package cl-user)

(defun select-change-of-circumstances-ontology ()
  (ocml::select-ontology 'ocml::change-of-circumstances-via-ontology-goals))

(defun find-items-matching-order-fun (order)
  ;;select-change-of-circumstances-ontology
)


(defun get-client-weight-fun (client)
  (ocml::with-ocml-thread-safety ()
    (setf client (make-ocml-symbol client))
    (select-change-of-circumstances-ontology)
    (ocml::findany '?x `(ocml::weight ,client ?x))))

(defun get-client-impairment-fun (client)
  (ocml::with-ocml-thread-safety ()
    (setf client (make-ocml-symbol client))
    (select-change-of-circumstances-ontology)
    (ocml::findany '?x `(ocml::has-impairment ,client ?x))))

(defun find-items-matching-impairment-fun (impairment-descriptor)
  (ocml::with-ocml-thread-safety ()
    (setf impairment-descriptor (make-ocml-symbol impairment-descriptor))
    (select-change-of-circumstances-ontology)
    (ocml::setofall '?item
                    `(and (ocml::related-impairment ?care-type ,impairment-descriptor)
                          (ocml::used-for ?item ?care-type)
                          (ocml::care-item ?item)))))

(defun find-items-matching-weight-fun (user-weight)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-ontology)
    (ocml::setofall '?item
                    `(and (ocml::max-user-weight ?item ?weight)
                          (ocml::> ?weight ,user-weight)))))

(defun order-equipment-fun (adaptation-items)
  (ocml::with-ocml-thread-safety ()
    (setf adaptation-items (make-ocml-item adaptation-items))
    (select-change-of-circumstances-ontology)
    (let ((items-and-suppliers nil)
          (items-with-no-supplier nil))
    (mapc
     #'(lambda (adaptation-item)
         (let ((supplier-name
                (web-onto::findany
                 '?name
                 `(and (ocml::main-supplier ,adaptation-item ?supplier)
                       (ocml::has-name ?supplier ?name)))))
      (if supplier-name
          (push (list adaptation-item supplier-name) items-and-suppliers)
        (push adaptation-item items-with-no-supplier))))
     adaptation-items)
    (if adaptation-items
          (format nil "\"~:[~*~;The following items have been orderd:~{~{ ~:(~a~) from ~a~}~}.~] ~:[~*~;For the following items we could not find a supplier ~(:~{~a ~}~)~]\""
                  items-and-suppliers items-and-suppliers 
                  items-with-no-supplier  items-with-no-supplier)
      (format nil "\"No items to order.\"")))))


(eval-when (eval load)
  (irs-wsmo-web-service-registration change-of-circumstances-via-ontology-goals
                                     get-client-weight-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-via-ontology-goals
                                     get-client-impairment-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-via-ontology-goals
                                     find-items-matching-impairment-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-via-ontology-goals
                                     find-items-matching-order-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-via-ontology-goals
                                     find-items-matching-weight-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-via-ontology-goals
                                     list-intersection-web-service)
  (irs-wsmo-web-service-registration 
   change-of-circumstances-via-ontology-goals
   impairment-to-list-intersection-mediation-service-web-service)
  (irs-wsmo-web-service-registration
   change-of-circumstances-via-ontology-goals
   weight-to-list-intersection-mediation-service-web-service)
  (irs-wsmo-web-service-registration
   change-of-circumstances-via-ontology-goals
   order-equipment-web-service))