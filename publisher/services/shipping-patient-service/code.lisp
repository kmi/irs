(in-package cl-user)


(defvar *medical-service-for-therapy-mappings*
  '((ocml::arthroplasty . hip-replacement)
    (arthroplasty . hip-replacement)))

(defun medical-service (therapy)
  (cdr (assoc therapy *medical-service-for-therapy-mappings*)))

(defun cleanup-date (date)
  (format nil "~a of ~a, ~d"
          (get-day date)
          (convert-month (get-month date))
          (get-year date)))


;;;all services except shipping patient and diagnose now have an inbuilt
;;;delay specified here
(defvar *shipping-patient-delay* 0)

(defun shipping-patient-delay ()
  (sleep *shipping-patient-delay*))


(defun shipping-patient-service 
       (&optional (obs1 'ocml::severe-joint-pain)
                  (obs2 'ocml::severe-joint-stiffness)
                  (obs3 'ocml::severe-swelling)
                  (obs4 'ocml::joint-creaking)
                  (patient-location 'milton-keynes))
  (let (result)
    (setf result 
          (apply #'format nil "The patient has ~:(~a~) for which the recommended therapy is ~:(~a~) which requires ~:(~a~). The ~:(~a~) hospital in ~:(~a~) can treat the patient on the ~a. It will cost ~d ~:(~a~) to ship the patient."
                 (internal-shipping-patient-service obs1 obs2 obs3 obs4 patient-location)))
    result))


(defun internal-shipping-patient-service 
       (&optional (obs1 'ocml::severe-joint-pain)
                  (obs2 'ocml::severe-joint-stiffness)
                  (obs3 'ocml::severe-swelling)
                  (obs4 'ocml::joint-creaking)
                  (patient-location 'milton-keynes))
  (let ((diagnosis (car (diagnose-patient obs1 obs2 obs3 obs4)))
        therapy hospital-and-date hospital-date hospital medical-service
        hospital-location hospital-currency patient-currency
        cost-in-patient-currency)
    (format t "The diagnosis for the patient is ~:(~a~)~%" diagnosis)
    (setf therapy (recommend-therapy diagnosis)
          medical-service (medical-service therapy))
    (format t "The recommended therapy for ~:(~a~) is ~:(~a~) which requires ~:(~a~)~%" 
            diagnosis therapy medical-service)
    (setf hospital-and-date  
          (find-soonest-medical-service medical-service)
          hospital (car hospital-and-date)
          hospital-date (cleanup-date (second hospital-and-date))
          hospital-location (get-hospital-location hospital)
          hospital-currency (get-location-currency hospital-location)
          patient-currency (get-location-currency patient-location))
    (format t "The ~(~a~) is located in ~:(~a~) which uses the ~:(~a~).~%" hospital
            hospital-location hospital-currency)
    (format t "Going to ship the patient from ~:(~a~) which uses the ~:(~a~).~%" 
            patient-location patient-currency)
    (setf cost-in-patient-currency
          (ambulance-service-cost patient-location 
                                  hospital-location patient-currency 
                                  hospital-currency))
    (format t "It will cost ~d ~:(~a~) to ship the patient.~%" 
            cost-in-patient-currency patient-currency)
    ;;result is just a string describing what happens to the patient
    ;;
    (list diagnosis therapy medical-service hospital
          hospital-location
          hospital-date cost-in-patient-currency 
          patient-currency)))


(defun diagnose-patient (&optional (obs1 'ocml::severe-joint-pain) 
                                   (obs2 'ocml::severe-joint-stiffness)
                                   (obs3 'ocml::severe-swelling)
                                   (obs4 'ocml::joint-creaking))
  (achieve-task-through-irs ;;'arthritis-diagnosis-service-ontology
                            ;;no longer need the ontology of the psm, 
                            ;;can use the ontology of the task
                            'fast-arthritis-diagnosis-task
                            'arthritis-diagnosis
                            `(ocml::observable1 ,obs1)
                            `(ocml::observable2 ,obs2)
                            `(ocml::observable3 ,obs3)
                            `(ocml::observable4 ,obs4)))

(defun recommend-therapy (condition)
  (achieve-task-through-irs ;;'arthritis-therapy-service-ontology
                            ;;no longer need the ontology of the psm, 
                            ;;can use the ontology of the task
                            'arthritis-therapy-task
                            'arthritis-therapy
                            `(ocml::condition ,condition)))


;;(shipping-patient 'hip-replacement 'milton-keynes)
(defun shipping-patient (medical-service patient-location)
  (let ((hospital (find-soonest-medical-service medical-service))
        hospital-location hospital-currency patient-currency cost-in-patient-currency)
    (setf hospital-location (get-hospital-location hospital)
          hospital-currency (get-location-currency hospital-location)
          patient-currency (get-location-currency patient-location))
    (format t "The ~(~a~) is located in ~:(~a~) which uses the ~:(~a~).~%" hospital
            hospital-location hospital-currency)
    (format t "Going to ship the patient from ~:(~a~) which uses the ~:(~a~).~%" 
            patient-location patient-currency)
    (setf cost-in-patient-currency
          (ambulance-service-cost patient-location hospital-location patient-currency 
                                  hospital-currency))
    (format t "It will cost ~d ~:(~a~) to ship the patient.~%" 
            cost-in-patient-currency patient-currency)))
        

;;'hip-replacement
(defun find-medical-service (service)
  (achieve-task-through-irs ;;psm ontology 'medicalservicelocator 
                            ;;task ontology
                            'medical-service-location
                            'medical_service_location
                            (List 'has_medical_service service)))

(defun make-lowercase-string (x)
  (cond ((symbolp x)
         (string-downcase (symbol-name x)))
        ((stringp x)
         (string-downcase x))
        (t (format nil "~(~a~)" x))))

(defun ambulance-service-cost (start destination start-currency destination-currency)
  ;;(setf s start d destination c1 start-currency c2 destination-currency)
  (let* ((amount-in-destination-currency
          (achieve-task-through-irs ;;;psm ontology 'GenAmbulanceService 
                                    ;;task ontology
                                    'AmbulanceService
                                    'ambulance_service
                                    (List 'ocml::has_start_location start)
                                    (list 'ocml::has_destination_location destination)
                                    (list 'ocml::has_currency destination-currency)))
        (amount-in-start-currency
         (achieve-task-through-irs 
                                   'generic-currency-conversion 
                                   'ocml::generic_currency_conversion
                                   (List 'ocml::has_source_currency 
                                         (make-lowercase-string destination-currency))
                                   (list 'ocml::Has_target_currency 
                                         (make-lowercase-string start-currency))
                                   (list 'ocml::has_amount amount-in-destination-currency))))
    (format t "Have now found an ambulance for ~d ~:(~a~)s or ~d ~:(~a~)s~%" 
            amount-in-destination-currency destination-currency 
            (round amount-in-start-currency) start-currency)
    (round amount-in-start-currency)))



;;'hip-hip-hospital 'hip-replacement
(defun get-hospital-bed-service (hospital-name medical-service)
  (achieve-task-through-irs 
   ;;psm ontology 
   ;;'genHospitalBedService
   ;;task ontology
   'hospitalbedservice
   'hospital_bed_service
   (List 'ocml::has_medical_service medical-service)
   (List 'ocml::has_hospital_name hospital-name)))

(defvar *months*
  '("January" "February" "March" "April" "May" "June" "July" "August" "September"
              "October" "November" "December"))

(defun convert-month (num)
  (elt *months* (1- num)))

;;;'hip-replacement
(defun find-soonest-medical-service (service)
  (let* ((hospitals (find-medical-service service))
         result soonest-hospital
         hospitals-and-dates)
    (format t "Found the following hospitals for ~(~a~): ~(~{~a ~}~)~%" service hospitals)
    (setf hospitals-and-dates
          (mapcar #'(lambda (hospital) 
                      (list hospital (get-hospital-bed-service hospital service)))
                  hospitals))
    (setf result
          (sort hospitals-and-dates #'(lambda (x y) (earlier-date-p (second x) (second y))))
          soonest-hospital (car result))
    (format t "~{~{~(~a~) can schedule a patient for the ~d of ~a ~d~%~}~}~%"
            (mapcar #'(lambda (hospital-and-date)
                        (list (car hospital-and-date) (get-day (second hospital-and-date))
                              (convert-month (get-month (second hospital-and-date)))
                              (get-year (second hospital-and-date)))) 
                    result))
    (format t "~(~a~) hospital recommended~%" (car soonest-hospital))
    soonest-hospital))

(eval-when (eval load)
  ;;here to be used by other kmi folks
  (irs-method-registration shipping-patient-service-ontology ;;this is the ontology name
                            shipping-patient-service  ;;this is the method name
                            shipping-patient-service  ;;this is the function name
                            ))

#|
;;;now publish shipping patient code as a web application
(eval-when (eval load)
  (irs-method-registration shipping-patient-service-ontology ;;this is the ontology name
                            shipping-patient-service  ;;this is the method name
                            shipping-patient-service  ;;this is the function name
                            ))

(achieve-task-through-irs 'shipping-patient-service-ontology 
                          'shipping-patient-task
                          (list 'ocml::observable1 
                                'ocml::severe-joint-pain)
                          (list 'ocml::observable2 
                                'ocml::severe-joint-stiffness)
                          (list 'ocml::observable3 
                                'ocml::severe-swelling)
                          (list 'ocml::observable4 
                                'ocml::joint-creaking)
                          (list 'ocml::patient-location 
                                'ocml::milton-keynes))
|#



(defmacro with-shipping-patient-page-info ((info &optional 
                                                 obs1 
                                                 obs2
                                                 obs3
                                                 obs4
                                                 patient-location)
                                             &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,obs1 (http::get-decoded-form-value ,info :observable1))  
          (,obs2 (http::get-decoded-form-value ,info :observable2)) 
          (,obs3 (http::get-decoded-form-value ,info :observable3)) 
          (,obs4 (http::get-decoded-form-value ,info :observable4)) 
          (,patient-location (http::get-decoded-form-value ,info :patient-location)))
    ,@body))


#+lispworks
(editor::setup-indent 'with-shipping-patient-page-info 0 2)

(defvar *xml-footer*
  (format nil "</xml>~%~%"))


(defvar *content-type-header* "Content-type: text/xml")

(defvar *http-header* "HTTP/1.0 200 OK")

(defvar *xml-header*
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")

(http::define-page2 ("Shipping Patient Page"
                     :func-name shipping-patient-page
                     :class :user 
                     )
    (&rest info)
  (progn 
    (setf info (car info))  
    (with-shipping-patient-page-info (info observable1 
                                           observable2
                                           observable3
                                           observable4
                                           patient-location)
      (unless observable1
        (setf observable1 'ocml::severe-joint-pain))
      (unless observable2
        (setf observable2 'ocml::severe-joint-stiffness))
      (unless observable3 
        (setf observable3 'ocml::severe-swelling))
      (unless observable4
        (setf observable4 'ocml::joint-creaking))
      (if patient-location
          (setf patient-location (intern (symbol-name patient-location) (find-package "CL-USER")))
        (setf patient-location 'milton-keynes))
     ;;(setf ll (list observable1 observable2 observable3 
       ;;      observable4 patient-location))
      (let ((result 
             (internal-shipping-patient-service 
              observable1 observable2 observable3 
              observable4 patient-location)))
        (http::html-out (format nil "~a~%~a~%~%~a~%"
                                *http-header* *content-type-header* *xml-header*))
        (http::html-out
         (generate-shipping-patient-result-output result))))))

(defun generate-shipping-patient-result-output (result)
  ;;(setf rr result)
  (destructuring-bind (diagnosis therapy therapy-common-name 
                                 selected-hospital location date cost currency) result
    (declare (ignore therapy))
    (format nil "<shipping-patient-result>~%  <comment>~a</comment>~%  <diagnosis>~a</diagnosis>~%  <therapy>~a</therapy>~%  <hospital>~a</hospital>~%  <location>~a</location>~%  <date>~a</date~%  ><cost>~a</cost>~% <currency>~a</currency></shipping-patient-result>~%~%" 
            (apply #'format nil "The patient has ~:(~a~) for which the recommended therapy is ~:(~a~) which requires ~:(~a~). The ~:(~a~) hospital in ~:(~a~) can treat the patient on the ~a. It will cost ~d ~:(~a~) to ship the patient." result)
                   
            diagnosis therapy-common-name 
            selected-hospital location date cost currency)))
                 

