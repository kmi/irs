(in-package cl-user)

;; snow-level-average
#|
(defun get-average-snow-level (inst)
;;(setf dd dom cc c)
(let ((obj (ocml::setofall '?x `(ocml::has-slot-value ,inst 'ocml::has-objects ?x)))
      (sum 0)
      (count 0))
     (loop for o in obj
           do (setf snow-level (ocml::THE-SLOT-VALUE o 'ocml::has-snow-level))
              (setf sum (+ sum snow-level))
              (setf count (+ count 1)))
     (/ sum count)))
     
;;(get-average-snow-level 'instance2815) 
|#

;;worflow

(defun classify-current-situation (dom c)
  ;;(setf dd dom cc c)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology (ocml::make-ocml-symbol dom))
    (let ((obs nil)
          (sol nil)
          (abstrcs nil))
      (setf obs (ocml::get-observables-from-class (ocml::make-ocml-symbol c)))
      (setf sol (ocml::get-solution-space-from-class (ocml::make-ocml-symbol c)))
      (setf abstrcs (ocml::get-abstractor-from-solution-space sol))
      (ocml::situation-optimal-classification obs sol abstrcs (ocml::make-ocml-symbol dom)))))

;;(classify-current-situation 'emergency-gis-situations 'snow-storm-entry-situation)

(defun classify-current-situation2 (dom c obs)
  ;;(setf dd dom cc c)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology (ocml::make-ocml-symbol dom))
    (let ((sol nil)
          (abstrcs nil))
      (setf sol (ocml::get-solution-space-from-class (ocml::make-ocml-symbol c)))
      (setf abstrcs (ocml::get-abstractor-from-solution-space sol))
      (ocml::situation-optimal-classification obs sol abstrcs (ocml::make-ocml-symbol dom)))))

;;(classify-current-situation2 'emergency-gis-situations 'snow-storm-entry-situation '((ocml::snow-level 10)))


;;resources filtering
(defun select-resource (resource emergency)
 (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology 'ocml::emergency-gis-goals2)
    (let ((result nil) 
          (retrieved-slots nil)
          (expected-slots nil))
         (ocml::describe-instance resource)
         (setf retrieved-slots (find-true-slots resource))
         (setf expected-slots (find-needed-slots emergency))
         (if (intersection retrieved-slots expected-slots) (list resource)))))


(defun find-true-slots (resource)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology 'ocml::emergency-gis-goals2)
    (let ((result nil))
         (setf result (ocml::findall '?x `(ocml::HAS-SLOT-VALUE ,resource ?x 'ocml::true)))
         (first (list result)))))

(defun find-needed-slots (emergency)
  (ocml::with-ocml-thread-safety ()
   (ocml::select-ontology 'ocml::emergency-gis-goals2)
   (let ((result nil)
         (true-slots nil))
        (setf result (ocml::setofall '?x `(ocml::has-slot-value ?x 'ocml::is-suitable-for-emergency ,emergency)))
        (loop for i in result
              do (setf true-slots (append true-slots (find-true-slots i))))
        (first (list true-slots)))))



(defun internal-filter-resources (resources-item emergency)
(ocml::with-ocml-thread-safety ()
    (ocml::select-ontology 'ocml::emergency-gis-goals2)
    (let ((filtered-items nil))
          
          (loop for i in (ocml::findall '?x `(ocml::has-objects ,resources-item ?x))  
                do 
                (setf filtered-items (append filtered-items (select-resource i emergency))))
          (ocml::clear-slot-values resources-item 'ocml::has-objects)
          (if (not filtered-items) (ocml::add-slot-value resources-item 'ocml::has-objects nil))
          (loop for j in filtered-items 
                do 
                (ocml::add-slot-value resources-item 'ocml::has-objects j))
          (list resources-item))))
          
(defun filter-resources (resources-list emerg)
  ;;(setf ll resources-list)
  ;;(setf gg emerg)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology 'ocml::emergency-gis-goals2)
    (let ((emergency (ocml::make-ocml-symbol emerg))
          (resources-item (ocml::make-ocml-symbol (car resources-list))))
          (dolist (definition (cdr resources-list))
            (eval-ocml-definition definition))            
          (cons resources-item (ocml::generate-ocml-instances-source (internal-filter-resources resources-item 'ocml::snow-storm-emergency))))))


 








;;; Alessio 10/10 -> function to get date and time from the server
(defun get-date ()
(let ((date nil)
      (time-list nil))
     (setf time-list (multiple-value-bind (Second Minute Hour Date Month Year)
                            (decode-universal-time (get-universal-time) '-1) 
                            (list Second Minute Hour Date Month Year)))
     (setf date (list (fourth time-list) (fifth time-list)))))


(defun get-time ()
(let ((date nil)
      (time-list nil))
     (setf time-list (multiple-value-bind (Second Minute Hour Date Month Year)
                            (decode-universal-time (get-universal-time) '-1) 
                            (list Second Minute Hour Date Month Year)))
     (setf date (list (third time-list) (second time-list)))))


;; Alessio 25/09-> to extract method information from a kind of query

(defun get-method-from-spatial-object-query (query);;(lat long radius query)
(ocml::with-ocml-thread-safety ()
 (ocml::select-ontology 'ocml::emergency-gis-domain)
 (let ((method (ocml::the-slot-value (ocml::make-ocml-symbol query) 'ocml::has-method)))
      (first (list method)))))

;;(get-method-from-spatial-object-query 'RestCentresQuery)

(defun add-method-for-plume-radius ()
  (ocml::with-ocml-thread-safety ()
    "getPlumePolygonInRadius"))

;;(add-method-for-plume-radius)


(defun get-absolute-distance (a b)
(SQRT (+ (* (- (first a) (first b)) (- (first a) (first b))) 
         (* (- (second a) (second b)) (- (second a) (second b))))))

(defun circle-center (points)
(let ((count 0)
      (totx 0)
      (toty 0))
     (loop for i in points
           do  (setf count (+ count 1))
               (setf totx (+ totx (first i)))
               (setf toty (+ toty (second i))))
     (list (float (/ totx count)) (float (/ toty count)))))




(defun circle-center-long (points) ;;x
(first (circle-center points)))

(defun circle-center-lat (points) ;;y
(second (circle-center points)))

(defun circle-radius (points)
(let ((center (circle-center points))
      (radius 0)
      (distance 0))
     (loop for i in points 
           do (setf distance (get-absolute-distance center i))
              (if (> distance radius) (setf radius distance)))
     (first (list radius))))

(defun return-points (method points spatial-object)
(first (list points)))

;;(return-points '(getHospitalsInRadius) '((0.54160 61.7479) (0.34160 41.7479) (0.54160 41.7479) (0.34160 61.7479)) '(hospital))


;;(get-absolute-distance '(10 2) '(7 2))

;;(circle-center-long '((1 2) (10 2) (10 2)))

;;(circle-radius '((1 2) (10 2) (10 2)))

(defvar *emergency-planning-host* "dip.bat.bt.co.uk")

(defvar *emergency-planning-port* 8081)

(defun get-rest-centres-with-heating (method latitude longitude radius)
  (ocml::select-ontology 'ocml::emergency-gis-domain)
  (let* ((result
          (ocml::lift-rest-centres-results
           (utilities:http-request
            (format nil "http://~A:~A~A"
                    *emergency-planning-host*
                    *emergency-planning-port*
                    (format nil "/axis/services/eccRestCentres?method=~a&latitude=~d&longitude=~d&radiusKM=~d"
                            method latitude longitude radius)))))
         (rest-centres
          (mapcan
           #'(lambda (x)
               (when (ocml::findany '?x `(ocml::has-heating ,x ?x))
                 (list x)))
           (ocml::findany '?x `(ocml::has-rest-centres ,result ?x)))))
    (ocml::unassert1 `(ocml::has-rest-centres ,result ?x))
    (ocml::tell1 `(ocml::has-rest-centres ,result ,rest-centres))
    result))

(defun eval-ocml-definition (x)
  (let ((*package* (find-package "OCML")))
    (eval (make-ocml-object x))))

(defun gis-filter (accommodation-list)
  ;;(setf ll accommodation-list)
  (ocml::with-ocml-thread-safety ()
    ;;(ocml::select-ontology 'ocml::emergency-gis-domain)
    (ocml::select-ontology 'ocml::emergency-gis-goals2)
    (let ((accommodation-item (ocml::make-ocml-symbol (car accommodation-list))))
      ;(print (ocml::findany '?x `(ocml::instance-of ,accommodation-item ?x)))
      (dolist (definition (cdr accommodation-list))
        (eval-ocml-definition definition))
      ;;(setf p (ocml::findany '?x `(ocml::instance-of ,accommodation-item ?x)))
      ;;(format nil "~s"
              (cons accommodation-item
                    (ocml::generate-ocml-instances-source
                     (internal-gis-filter accommodation-item (ocml::findany '?x `(ocml::instance-of ,accommodation-item ?x))))))))







(defmethod internal-gis-filter (accommodation-item (type (eql 'ocml::rest-centres-results)))
  (let ((rest-centres (ocml::slot-values accommodation-item 'ocml::has-objects)));;(ocml::findany '?x `(ocml::has-objects ,accommodation-item ?x))))
    (filter-rest-centres accommodation-item rest-centres)))


;; this filters capacity
(defun filter-rest-centres (accommodation-item rest-centres)
  (setf rest-centres 
        (mapcan 
         #'(lambda (x)
             (when (web-onto::findany '?x `(and (ocml::has-capacity ,x ?x)
                                                (> ?x 100)))
               (list x)))
         rest-centres))
  (ocml::unassert1 `(ocml::has-objects ,accommodation-item ?x))
  (loop for res in rest-centres 
        do 
        (ocml::add-slot-value accommodation-item 'ocml::has-objects res))
  ;(ocml::tell1 `(ocml::has-objetcs ,accommodation-item ,rest-centres)) changed by alessio (06-11-2006)
  (list accommodation-item))

#|
;;this filter heating (very rare)
(defun filter-rest-centres (accommodation-item rest-centres)
  (setf rest-centres 
        (mapcan 
         #'(lambda (x)
             (when (web-onto::findany '?x `(ocml::has-heating ,x ?x))
               (list x)))
         rest-centres))
  (ocml::unassert1 `(ocml::has-objects ,accommodation-item ?x))
  (loop for res in rest-centres 
        do 
        (ocml::add-slot-value accommodation-item 'ocml::has-objects res))
  ;(ocml::tell1 `(ocml::has-objetcs ,accommodation-item ,rest-centres)) changed by alessio (06-11-2006)
  (list accommodation-item))
|#

(defmethod internal-gis-filter (accommodation-item (type (eql 'ocml::temp-centres-results)))
  (let ((rest-centres (ocml::slot-values accommodation-item 'ocml::has-objects)));;(ocml::findany '?x `(ocml::has-objects ,accommodation-item ?x))))
    (filter-rest-centres accommodation-item rest-centres)))





(defvar *minimum-number-of-beds* 3)

(defmethod internal-gis-filter (accommodation-item (type (eql 'ocml::hospitals-results)))
  (let ((hospitals (ocml::slot-values accommodation-item 'ocml::has-objects)));;(ocml::findany '?x `(ocml::has-objects ,accommodation-item ?x))))
    (setf hospitals 
#|
     (mapcan 
           #'(lambda (x)
               (let ((number-of-beds (web-onto::findany '?x `(ocml::has-beds ,x ?x))))
                 (when (and number-of-beds
                            (> number-of-beds *minimum-number-of-beds*))
                   (list x))))
           hospitals)
|#
          hospitals
)
    (ocml::unassert1 `(ocml::has-objects ,accommodation-item ?x))
    (loop for res in hospitals 
        do 
        (ocml::add-slot-value accommodation-item 'ocml::has-objects res))
    ;;(ocml::tell1 `(ocml::has-objects ,accommodation-item ,hospitals))
    (list accommodation-item) ;;hospitals) 
    ))


(defvar *minimum-number-of-rooms* 3)

(defun filter-accommodations (accommodations)
  (handler-case
      (mapcan 
       #'(lambda (x)
           (let ((number-of-rooms (web-onto::findany '?x `(ocml::has-rooms ,x ?x))))
             (when (and number-of-rooms
                        (or (and (numberp number-of-rooms)
                                 (> number-of-rooms *minimum-number-of-rooms*))
                            (and (string number-of-rooms)
                                 (numberp (read-from-string number-of-rooms))
                                 (> (read-from-string number-of-rooms) *minimum-number-of-rooms*))))
               (list x))))
       accommodations)
    (serious-condition 
     (c) accommodations)
    (error 
     (c) accommodations)))

(defmethod internal-gis-filter (accommodation-item (type (eql 'ocml::hotels-results)))
  (let ((hotels (ocml::slot-values accommodation-item 'ocml::has-objects)));;(ocml::findany '?x `(ocml::has-objects ,accommodation-item ?x))))
    (setf hotels (filter-accommodations hotels))
    (ocml::unassert1 `(ocml::has-objects ,accommodation-item ?x))
    (loop for res in hotels 
        do 
        (ocml::add-slot-value accommodation-item 'ocml::has-objects res))
    ;(ocml::tell1 `(ocml::has-objects ,accommodation-item ,hotels))
    (list accommodation-item))) ;;hotels)))

(defmethod internal-gis-filter (accommodation-item (type (eql 'ocml::inns-results)))
  (let ((inns (ocml::slot-values accommodation-item 'ocml::has-objects)));;(ocml::findany '?x `(ocml::has-objects ,accommodation-item ?x))))
    (setf inns (filter-accommodations inns))
    (ocml::unassert1 `(ocml::has-objects ,accommodation-item ?x))
    (loop for res in inns 
        do 
        (ocml::add-slot-value accommodation-item 'ocml::has-objects res))
    ;(ocml::tell1 `(ocml::has-objects ,accommodation-item ,inns))
    (list accommodation-item))); inns)))

(defmethod internal-gis-filter (accommodation-item (type (eql 'ocml::supermarkets-results)))
  (let ((supermarkets (ocml::slot-values accommodation-item 'ocml::has-objects)));;(ocml::findany '?x `(ocml::has-objects ,accommodation-item ?x))))
    (print supermarkets)
    (print (cons accommodation-item supermarkets))
    ))

(eval-when (eval load)
  (irs-wsmo-web-service-registration 
   emergency-gis-goals2
   gis-filter-web-service)  
  (irs-wsmo-web-service-registration
   emergency-gis-situations
   classify-current-situation-web-service)
  (irs-wsmo-web-service-registration
   emergency-gis-situations
   classify-current-situation-web-service2)
  (irs-wsmo-web-service-registration 
   emergency-gis-goals2
   resource-filter-web-service)
  (irs-wsmo-web-service-registration 
   emergency-gis-goals2
   method-from-spatial-object-query-web-service)
  (irs-wsmo-web-service-registration 
   emergency-gis-goals2
   polygon-to-circle-radius-web-service)
  (irs-wsmo-web-service-registration 
   emergency-gis-goals2
   polygon-to-circle-center-longitude-web-service)
  (irs-wsmo-web-service-registration 
   emergency-gis-goals2
   polygon-to-circle-center-latitude-web-service)
  (irs-wsmo-web-service-registration 
   emergency-gis-goals2
   get-polygon-gis-data-web-service)
  (irs-wsmo-web-service-registration 
   emergency-gis-plume
   get-plume-data-web-service)
  (irs-wsmo-web-service-registration
   met-office-goals
   get-average-web-service)
)
