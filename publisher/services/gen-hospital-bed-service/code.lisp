;;; Mode: Lisp; Package: cl-user

(in-package cl-user)

(defun make-date (day month &optional (year 2003))
  (list (gentemp "DATE")
        'ocml::calendar-date
        `(cl-user::day-of ,day)
        `(cl-user::month-of ,month)
        `(cl-user::year-of ,year)))

(defun get-year (date)
  (or (get-date-part 'year-of date)
      ;;why do i need this for some reason when called through the irs
      ;;all of the items appear in the ocml page
      (get-date-part 'ocml::year-of date)))

(defun get-month (date)
  (or (get-date-part 'month-of date)
      (get-date-part 'ocml::month-of date)))

(defun get-day (date)
  (or (get-date-part 'day-of date)
      (get-date-part 'ocml::day-of date)))

(defun get-date-part (part-name date)
  (second (assoc part-name (cddr date))))

(defun earlier-date-p (date1 date2)
  (or (< (get-year date1) (get-year date2))
      (and (= (get-year date1) (get-year date2))
           (or (< (get-month date1) (get-month date2))
               (and (= (get-month date1) (get-month date2))
                    (< (get-day date1) (get-day date2)))))))
      

(defun dummy-date ()
  (make-date 1 10))

(defvar *hospital-locations*
  '((hip-hip-hospital paris)
    (the-hippy-hospital milan)
    (not-the-hippy-hospital amsterdam)
    (another-hippy-hospital barcelona)
    (ocml::hip-hip-hospital paris)
    (ocml::the-hippy-hospital milan)
    (ocml::not-the-hippy-hospital amsterdam)
    (ocml::another-hippy-hospital barcelona)))


(defvar *location-currency*
  '((paris euro)
    (amsterdam euro)
    (barcelona euro)
    (milan euro)
    (milton-keynes pound)))

(defun get-hospital-location (hospital-name)
  (second (assoc hospital-name *hospital-locations*)))


(defun get-location-currency (location)
  (second (assoc location *location-currency*)))

(defun hospital_bed_service_for_hip_replacement (condition)
  (if (eq condition 'hip-replacement) ;;ignore
    (make-date 20 6)
    "FAILURE"))


(defun hip_hip_hospital_bed_service (service hospital)
  hospital ;;ignore
  (shipping-patient-delay)
  (hospital_bed_service_for_hip_replacement service))


(defun  not_the_hippy_hospital_bed_service (service hospital)
  hospital ;;ignore
  (shipping-patient-delay)
  (if (eq service 'hip-replacement) ;;ignore
      "FAILURE"
    (make-date 15 6)))

(defun the_hippy_hospital_bed_service  (service hospital)
  hospital ;;ignore
  (shipping-patient-delay)
  ;;(hospital_bed_service_for_hip_replacement service))
  (if (eq service 'hip-replacement) ;;ignore
    (make-date 7 7)
    "FAILURE"))


(defun another_hippy_hospital_bed_service (service hospital)
    hospital ;;ignore
    (shipping-patient-delay)
    ;;(hospital_bed_service_for_hip_replacement service)
  (if (eq service 'hip-replacement) ;;ignore
    (make-date 13 8)
    "FAILURE"))

(eval-when (eval load)
  (irs-method-registration GenHospitalBedService ;;this is the ontology name
                           another_hippy_hospital_bed_service  ;;this is the method name
                           another_hippy_hospital_bed_service ;;this is the lisp function
                           )

  (irs-method-registration GenHospitalBedService ;;this is the ontology name
                           hip_hip_hospital_bed_service  ;;this is the method name
                           hip_hip_hospital_bed_service ;;this is the lisp function
                           )

  (irs-method-registration GenHospitalBedService ;;this is the ontology name
                           the_hippy_hospital_bed_service   ;;this is the method name
                           the_hippy_hospital_bed_service  ;;this is the lisp function
                           )

  (irs-method-registration GenHospitalBedService ;;this is the ontology name
                           not_the_hippy_hospital_bed_service   ;;this is the method name
                           not_the_hippy_hospital_bed_service ;;this is the lisp function
                           )
  )

#|


;;;note ontology must be the one for the method!!!
(achieve-task-through-irs 'genHospitalBedService 'hospital_bed_service
                          (List 'ocml::has_medical_service 'hip-replacement)
                          (List 'ocml::has_hospital_name 'hip-hip-hospital))

|#




