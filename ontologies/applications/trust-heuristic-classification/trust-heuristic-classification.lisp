;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology trust-heuristic-classification)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-instance trust-profile application-domain)

;-------------- abstractors related with quality of service ------------------

(def-instance datafreshness-abstractor abstractor
  ((has-body '(lambda (?obs)  
                (in-environment 
                 ((?v . (observables-feature-value ?obs 'datafreshness)))
                 (cond ((>= ?v 10) 
                        (list-of 'datafreshness 'high 
                                 (list-of (list-of 'datafreshness ?v))))
                       ((and (< ?v 10) (> ?v 5))
                        (list-of 'datafreshness 'medium 
                                 (list-of (list-of 'datafreshness ?v))))
                       ((<= ?v 5)
                        (list-of 'datafreshness 'low 
                                 (list-of (list-of 'datafreshness ?v))))))))
   
   (applicability-check (kappa (?obs)
                                   (member 'datafreshness (all-features-in-observables ?obs)))))) 


(def-instance availability-abstractor abstractor
  ((has-body '(lambda (?obs)  
                (in-environment 
                 ((?v . (observables-feature-value ?obs 'availability)))
                 (cond ((>= ?v 0.8) 
                        (list-of 'availability 'high 
                                 (list-of (list-of 'availability ?v))))
                       ((and (< ?v 0.8) (> ?v 0.5))
                        (list-of 'availability 'medium 
                                 (list-of (list-of 'availability ?v))))
                       ((<= ?v 0.5)
                        (list-of 'availability 'low 
                                 (list-of (list-of 'availability ?v))))))))
   
   (applicability-check (kappa (?obs)
                                   (member 'availability (all-features-in-observables ?obs)))))) 

(def-instance execution-time-abstractor abstractor
  ((has-body '(lambda (?obs)  
                (in-environment 
                 ((?v . (observables-feature-value ?obs 'execution-time)))
                 (cond ((>= ?v 10) 
                        (list-of 'execution-time 'high 
                                 (list-of (list-of 'execution-time ?v))))
                       ((and (< ?v 10) (> ?v 5))
                        (list-of 'execution-time 'medium 
                                 (list-of (list-of 'execution-time ?v))))
                       ((<= ?v 5)
                        (list-of 'execution-time 'low 
                                 (list-of (list-of 'execution-time ?v))))))))
   
   (applicability-check (kappa (?obs)
                                   (member 'execution-time (all-features-in-observables ?obs)))))) 





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#|
;-------------- abstractors related with security  ------------------

(def-instance encryption-algorithm-abstractor abstractor
  ((has-body '(lambda (?obs)
                (in-environment 
                 ((?v . (observables-feature-value ?obs 'encryption-algorithm)))
                 (cond ((== ?v #_crypt:DES) 
                        (list-of 'encryption-algorithm 'high 
                                 (list-of (list-of 'encryption-algorithm ?v))))
                       ((== ?v #_crypt:AES)
                        (list-of 'encryption-algorithm 'medium 
                                 (list-of (list-of 'encryption-algorithm ?v))))
                       ((== ?v #_crypt:RSA)
                        (list-of 'encryption-algorithm 'low 
                                 (list-of (list-of 'encryption-algorithm ?v))))))))
   
   (applicability-check (kappa (?obs)
                               (member 'encryption-algorithm (all-features-in-observables ?obs)))))) 




(def-function pprint (?x &rest ?args)
  :lisp-fun 
  #'(lambda (x &rest args) (apply 'format t x args)))




(def-instance certification-authority-abstractor abstractor
  ((has-body '(lambda (?obs)  
                (in-environment
                 ((?v . (observables-feature-value ?obs 'certification-authority)))
                 ;;(pprint "hello ~A ~%" ?v)
                 (cond ((== ?v globalsign-austria)
                        ;;(pprint "in globalsign-austria ~%")
                        (list-of 'certification-authority 'high 
                                 (list-of (list-of 'certification-authority ?v))))
                       ((== ?v verisign)
                        (list-of 'certification-authority 'medium 
                                 (list-of (list-of 'certification-authority ?v))))
                       ((== ?v tc-trust-center)
                        (list-of 'certification-authority 'low 
                                 (list-of (list-of 'certification-authority ?v))))))))
   
   (applicability-check (kappa (?obs)
                                   (member 'certification-authority (all-features-in-observables ?obs))))))


(def-instance certification-authority-origin-abstractor abstractor
  ((has-body '(lambda (?obs)  
                (in-environment 
                 ((?v . (observables-feature-value ?obs 'certification-authority-country)))
                 ;;(pprint "in certification-authority-origin-abstractor ~%")
                 (cond ((== ?v austria) 
                        ;;(pprint "in austria ~%")
                        (list-of 'certification-authority-country 'high 
                                 (list-of (list-of 'certification-authority-country ?v))))
                       ((== ?v north-american-country)
                        (list-of 'certification-authority-country 'medium 
                                 (list-of (list-of 'certification-authority-country  ?v))))
                       ((== ?v germany)
                        (list-of 'certification-authority-country 'low 
                                 (list-of (list-of 'certification-authority-country ?v))))))))
   
   (applicability-check (kappa (?obs)
                                   (member 'certification-authority-country (all-features-in-observables ?obs))))))  

;|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-------------- classification -------------------

(defun trust-single-sol-classification (obs &optional (match-criterion 'abstraction-aware-match-criterion ))
  (unassert (task ?x))
  (heuristic-classify :solution-space '(trust-requirement)
                      :observables obs
                      :abstractors '(datafreshness-abstractor availability-abstractor execution-time-abstractor)
                      :refiners '(refinement-through-subclass-of-links)
                      :domain-name 'trust-profile
                      :task-type 'single-solution-classification-task
                      :method-type 'heuristic-admissible-sol-classifier
                      :match-criterion match-criterion
                     ; :solution-admissibility-criterion 'complete-coverage-admissibility-criterion))
                      :solution-admissibility-criterion 'DEFAULT-SOLUTION-ADMISSIBILITY-criterion))



#|
(defun trust-optimal-classification (obs &optional (match-criterion 'abstraction-aware-match-criterion ))
  (heuristic-classify :solution-space '(trust-requirement)
                      :observables obs
                      :abstractors '(datafreshness-abstractor availability-abstractor execution-time-abstractor encryption-algorithm-abstractor certification-authority-abstractor certification-authority-origin-abstractor)
                      :refiners '(refinement-through-subclass-of-links)
                      :domain-name 'trust-profile
                      :task-type 'optimal-classification-task
                      :method-type 'heuristic-optimal-sol-classifier
                       :match-criterion match-criterion
                     :solution-admissibility-criterion 'DEFAULT-SOLUTION-ADMISSIBILITY-criterion))
                      ; :solution-admissibility-criterion 'complete-coverage-admissibility-criterion))    


|#

;#|
(defun trust-optimal-classification (obs &optional (match-criterion 'abstraction-aware-match-criterion ))
  (heuristic-classify :solution-space '(trust-requirement)
                      :observables obs
                    ;  :abstractors '(encryption-algorithm-abstractor certification-authority-abstractor)
                      :abstractors '(encryption-algorithm-abstractor certification-authority-abstractor certification-authority-origin-abstractor)
                      :refiners '(refinement-through-subclass-of-links)
                      :domain-name 'trust-profile
                      :task-type 'optimal-classification-task
                      :method-type 'heuristic-optimal-sol-classifier
                      :match-criterion match-criterion
                  ;   :solution-admissibility-criterion 'DEFAULT-SOLUTION-ADMISSIBILITY-criterion))
                     :solution-admissibility-criterion 'complete-coverage-admissibility-criterion))    


;|#



(def-relation-mapping solution :up
  ((solution ?x)
   if
   (or (= ?x trust-requirement)
       (subclass-of ?x trust-requirement))))



(def-relation-mapping observable :up
  ((observable ?x)
   if
   (or (== ?X (?f ?v ?obs)) 
       (== ?x (?f ?v)))
   (and (slot-of ?f ?c)
            (or (= ?c trust-non-functional-properties)
                (subclass-of ?c trust-non-functional-properties)))))


(def-relation-mapping has-observable-feature :up
  ((has-observable-feature ?x ?f)
   if
    (or (== ?X (?f ?v ?obs)) (== ?x (?f ?v)))))


(def-relation-mapping has-observable-value :up
  ((has-observable-value ?x ?v)
   if
    (or (== ?X (?f ?v ?obs)) (== ?x (?f ?v)))))


(def-relation-mapping directly-abstracts-from :up
  ((directly-abstracts-from ?ob ?obs)
   if
   (== ?ob (?f ?v ?obs))))





;; new functions

(defun get-observables-from-participant (p)
  (let ((tp nil)
        (list-obs nil)
        (obs-pair nil) tg)
    (setf tp (first (ocml::setofall '?x `(ocml::class-slot-type ,p 'has-trust-profile ?x))))
    (setf tg (first (ocml::setofall '?y `(ocml::class-slot-type ,tp 'has-trust-guarantee ?y))))
    (loop for s in (ocml::setofall '?x `(and (ocml::slot-of ?x ,tg)
                                             (not (ocml::slot-of ?x 'non-functional-properties))))
          do (setf obs-pair (list s)) 
          (setf obs-pair (append obs-pair (ALL-CLASS-SLOT-VALUES tg s)))
          (setf list-obs (append list-obs (list obs-pair))))
    (first (list list-obs))))

;(get-observables-from-participant 'get-train-timetable-service-T2)


(defun get-requirement-from-participant (p)
 (let ((tp nil) tg)
        (setf tp (first (ocml::setofall '?x `(ocml::class-slot-type ,p 'has-trust-profile ?x))))
        (setf tg (first (ocml::setofall '?y `(ocml::class-slot-type ,tp 'has-trust-requirement ?y))))))

;;(get-requirement-from-participant 'user2)



(defun classify-from-observables (observables)
 (trust-optimal-classification observables))

;(defun classify-from-observables (observables)
;(trust-single-sol-classification observables))





;(classify-from-observables '((datafreshness 4) (availability 0.6) (execution-time 3))) 

;(classify-from-observables '((encrypion-algorithm DES) (certification-authority globalsign-austria) (certification-authority-country austria))) 
;(classify-from-observables '((encrypion-algorithm DES) (certification-authority globalsign-austria)))




(defun classify-list-ws-from-user-trust-requirement (user-requirement list-of-ws)
  (let ((list-of-good-ws nil) observable-list profile-of-ws)
    (loop for ws in list-of-ws 
          do (setf observable-list (get-observables-from-participant ws))
                  ; (print observable-list)
          (setf profile-of-ws (classify-from-observables observable-list))
          (if (eql (first profile-of-ws) user-requirement) (setf list-of-good-ws (append list-of-good-ws (list ws)))))
    (first (list list-of-good-ws))))





;(describe-class 'get-train-timetable-service-T2) 

;(classify-list-ws-from-user-trust-requirement 'requirement-USER6 '(get-train-timetable-service-T1 get-train-timetable-service-T2 get-train-timetable-service-T3))

(defun classify-list-ws-from-user-class (user list-of-ws)
 (let ((list-of-good-ws nil) user-profile observable-list profile-of-ws)
        (setf user-profile (get-requirement-from-participant user))
	(loop for ws in list-of-ws 
		do (setf observable-list (get-observables-from-participant ws))
                 ;  (print observable-list)
		   (setf profile-of-ws (classify-from-observables observable-list))
		   (if (eql (first profile-of-ws) user-profile) (setf list-of-good-ws (append list-of-good-ws (list ws)))))
	(first (list list-of-good-ws))))



(defun classify-list-ws-from-user-instance (user-instance list-of-ws)
 (let ((list-of-good-ws nil) observable-list user-profile profile-of-ws)
        (setf user-profile (get-requirement-from-participant (the-parent-class user-instance)))
            (loop for ws in list-of-ws 
                        do (setf observable-list (get-observables-from-participant ws))
                  ; (print observable-list)
                           (setf profile-of-ws (classify-from-observables observable-list))
                           (if (eql (first profile-of-ws) user-profile) (setf list-of-good-ws (append list-of-good-ws (list ws)))))
            (first (list list-of-good-ws)))) 


;(classify-list-ws-from-user-instance 'roxana '(get-train-timetable-service-T1 get-train-timetable-service-T2 get-train-timetable-service-T3 get-train-timetable-service-T4 get-train-timetable-service-T5))

;USER INSTANCES

(def-instance alessio  user1)

(def-instance roxana  user2)


; 
;#|
(def-instance dinar  user4)

(def-instance vanessa  user5)

(def-instance stefania  user6)


;|#



