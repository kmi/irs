;;; Mode: Lisp; Package: cl-user

(in-package cl-user)

(defun select-change-of-circumstances-ontology ()
  (ocml::select-ontology 'ocml::change-of-circumstances-ontology))

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
                          (ocml::item ?item)))))

(defun find-items-matching-weight-fun (user-weight)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-ontology)
    (ocml::setofall '?item
                    `(and (ocml::max-user-weight ?item ?weight)
                          (ocml::> ?weight ,user-weight)))))



(eval-when (eval load)
  (irs-wsmo-web-service-registration change-of-circumstances-goals
                                     get-client-weight-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-goals
                                     get-client-impairment-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-goals
                                     find-items-matching-impairment-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-goals
                                     find-items-matching-order-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-goals
                                     find-items-matching-weight-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-goals
                                     list-intersection-web-service)
  (irs-wsmo-web-service-registration 
   change-of-circumstances-goals
   impairment-to-list-intersection-mediation-service-web-service)
  (irs-wsmo-web-service-registration
   change-of-circumstances-goals
   weight-to-list-intersection-mediation-service-web-service)

  )

