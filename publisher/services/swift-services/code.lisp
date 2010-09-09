

(defun select-swift-goals-ontology ()
  (ocml::select-ontology 'ocml::swift-goals))

(defun find-items-matching-order-fun (order)
  ;;select-change-of-circumstances-ontology
)

(defun catalogue-entry-by-weight-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-elms-goals-ontology)
    'catalogue-entry-by-weight-interface-out))

(defun change-details-of-citizen-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    'change-details-of-citizen-interface-out))

(defun citizen-address-by-code-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    'citizen-address-by-code-interface-out))

(defun citizen-address-by-name-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    'citizen-address-by-name-interface-out))

(defun citizen-data-by-citizen-code-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    'citizen-data-by-citizen-code-interface-out))

(defun citizen-data-by-name-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    'citizen-data-by-name-interface-out))

;;;a triple name impairment weight
(defun create-citizen-record-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-item input))
    (select-swift-goals-ontology)
    (ocml::define-domain-instance 
     (car input)
     'ocml::client
     ""
     `((ocml::has-impairment ,(second input))
       (ocml::weight ,(third input))))
    (car input)))

(defun create-new-assessment-for-client-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    (let ((name 
           (intern (symbol-name (gensym "ASSESSMENT-OF-CARE-SERVICE-"))
                   (find-package "OCML"))))
      (ocml::define-domain-instance 
       name 'ocml::ASSESSMENT-OF-CARE-SERVICE
       `((ocml::assessed-to ,input)))
      name)))

(defun ethnicity-by-code-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    'ethnicity-by-code-interface-out))

(defun finalize-service-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    'finalize-service-interface-out))

(defun gender-by-code-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    'gender-by-code-interface-out))

(defun notify-citizen-deceased-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    'notify-citizen-deceased-interface-out))

(defun order-service-for-assessment-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    input))

(defun title-by-code-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-swift-goals-ontology)
    'title-by-code-interface-out))

(eval-when (eval load)
  (irs-wsmo-web-service-registration 
   swift-goals change-details-of-citizen-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals citizen-address-by-code-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals citizen-address-by-name-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals citizen-data-by-citizen-code-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals citizen-data-by-name-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals create-citizen-record-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals create-new-assessment-for-client-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals ethnicity-by-code-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals finalize-service-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals gender-by-code-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals notify-citizen-deceased-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals order-service-for-assessment-web-service)
  (irs-wsmo-web-service-registration 
   swift-goals title-by-code-web-service)
  (irs-wsmo-web-service-registration
   swift-goals redirect-equipment-to-new-address-web-service)
  (irs-wsmo-web-service-registration
   swift-goals catalogue-entry-by-weight-web-service))

