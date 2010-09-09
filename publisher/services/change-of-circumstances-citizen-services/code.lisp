(in-package cl-user)



(defun select-change-of-circumstances-citizen-ontology ()
  (ocml::select-ontology 'ocml::change-of-circumstances-citizen-ontology))



(defun change-details-of-citizen-interface-out (client new-address) ;;string
  ;;(setf cc client aaa new-address)
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-citizen-ontology)
    (setf client (make-ocml-item client)
          new-address (make-ocml-item new-address))
    (let ((family-name (ocml::findany '?x `(ocml::family-name ,client ?x)))
          (given-name (ocml::findany '?x `(ocml::given-name ,client ?x)))
          (house-number (ocml::findany '?x `(ocml::has-number-house ,new-address ?x)))
          (street (ocml::findany '?x `(ocml::has-street ,new-address ?x)))
          (city (ocml::findany '?x `(ocml::has-municipal-unit ,new-address ?x)))
          (county (ocml::findany '?x `(ocml::has-county ,new-address ?x)))
          (country (ocml::findany '?x `(ocml::has-country ,new-address ?x))))
      (ocml::tell1 `(ocml::has-address ,client ,new-address))
      (if (eq county :nothing)
          (format nil "\"Address for ~a ~a changed to ~d, ~a, ~a, ~a\""
                  given-name family-name house-number street city country)
        (format nil "\"Address for ~a ~a changed to ~d, ~a, ~a, ~a, ~a\""
                given-name family-name house-number street city county country)))))

(defun create-citizen-record-interface-out (client) ;string
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-citizen-ontology)
    (setf client (make-ocml-item client))
    (let ((family-name (ocml::findany '?x `(ocml::family-name ,client ?x)))
          (given-name (ocml::findany '?x `(ocml::given-name ,client ?x))))
    (format nil "\"Record created for ~a ~a\""
            given-name family-name))))

(defun gensymbol (string &optional (package-name "OCML"))
  (intern (symbol-name (gensym string))
          (find-package package-name)))

(defun create-new-assessment-for-client-interface-out (client) ;;assessment
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-citizen-ontology)
    (setf client (make-ocml-item client))
    (let ((new-assessment-name 
           (gensymbol "ASSESSMENT-"))
          (new-assessment-care-of-service-name 
           (gensymbol "ASSESSMENT-CARE-OF-SERVICE")))
      (ocml::define-domain-instance new-assessment-name 'ocml::assessment)
      (ocml::define-domain-instance new-assessment-care-of-service-name 
                                    'ocml::assessment-of-care-service)
      (ocml::tell1 
       `(ocml::Has-Related-Assessment-Of-Care-Service 
         ,new-assessment-name ,new-assessment-care-of-service-name))
      (ocml::tell1
       `(ocml::Assessed-To ,new-assessment-care-of-service-name ,client))
      new-assessment-name)))

(defun finalize-service-interface-out (client care-item) ; string
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-citizen-ontology)
    (setf client (make-ocml-item client)
          care-item (make-ocml-item care-item))
    (let ((family-name (ocml::findany '?x `(ocml::family-name ,client ?x)))
          (given-name (ocml::findany '?x `(ocml::given-name ,client ?x))))
      (format nil "\"Service finalized for ~a ~a and item ~:(~a~)\""
              given-name family-name care-item))))

(defun citizen-address-by-code-interface-out (client) ;;address
  ;;(setf cc client)
 (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-citizen-ontology)
    (setf client (make-ocml-item client))
    (let* ((family-name (ocml::findany '?x `(ocml::family-name ,client ?x)))
           (given-name (ocml::findany '?x `(ocml::given-name ,client ?x)))
           (address (ocml::findany '?x `(ocml::has-address ,client ?x)))
           (house-number (ocml::findany '?x `(ocml::has-number-house ,address ?x)))
           (street (ocml::findany '?x `(ocml::has-street ,address ?x)))
           (city (ocml::findany '?x `(ocml::has-municipal-unit ,address ?x)))
           (county (ocml::findany '?x `(ocml::has-county ,address ?x)))
           (country (ocml::findany '?x `(ocml::has-country ,address ?x))))
      (if (eq county :nothing)
          (format nil "\"Address for ~a ~a is ~d, ~a, ~a, ~a\""
                  given-name family-name house-number street city country)
        (format nil "\"Address for ~a ~a is ~d, ~a, ~a, ~a, ~a\""
                given-name family-name house-number street city county country)))))

(defun citizen-data-by-code-interface-out (client) ;;client
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-citizen-ontology)
    (setf client (make-ocml-item client))))

(defun ethnicity-by-code-interface-out (client) ;;ethnicity
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-citizen-ontology)
    (setf client (make-ocml-item client))
    (ocml::findany '?x `(ocml::has-ethnicity ,client ?x))))

(defun gender-by-code-interface-out (client) ;;gender
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-citizen-ontology)
    (setf client (make-ocml-item client))
    (ocml::findany '?x `(ocml::has-gender ,client ?x))))

(defun notify-citizen-deceased-interface-out (client date-of-death) ;;string
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-citizen-ontology)
    (setf client (make-ocml-item client)
          date-of-death (make-ocml-item date-of-death))
    (let ((family-name (ocml::findany '?x `(ocml::family-name ,client ?x)))
          (given-name (ocml::findany '?x `(ocml::given-name ,client ?x)))
          (day-of (ocml::findany '?x `(ocml::day-of ,date-of-death ?x)))
          (month-of (ocml::findany '?x `(ocml::month-of ,date-of-death ?x)))
          (year-of (ocml::findany '?x `(ocml::year-of ,date-of-death ?x))))
      (format nil "\"Noted that ~a ~a died ~a, ~a, ~a\""
              given-name family-name day-of month-of year-of))))

(defun order-service-for-assessment-interface-out (referral care-item) ;;string
  (ocml::with-ocml-thread-safety ()
    (select-change-of-circumstances-citizen-ontology)
    (setf referral  (make-ocml-item referral)
          care-item (make-ocml-item care-item))
    (let ((family-name (ocml::findany '?x `(ocml::family-name ,referral ?x)))
          (given-name (ocml::findany '?x `(ocml::given-name ,referral ?x))))
      (format nil "\" ~:(~a~) ordered for ~a ~a\""
              care-item given-name family-name))))

(eval-when (eval load)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     get-client-weight-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     change-details-of-client-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     create-client-record-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     create-new-assessment-for-client-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     finalize-service-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     get-client-address-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     get-client-data-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     get-client-ethnicity-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     get-client-gender-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     notify-client-deceased-web-service)
  (irs-wsmo-web-service-registration change-of-circumstances-citizen-web-services
                                     order-service-for-assessment-web-service))


                                     