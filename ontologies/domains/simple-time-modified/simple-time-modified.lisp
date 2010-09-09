;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology simple-time)

(def-class year-in-time ()?x
"A year-in-time must be an integer and integer can be a year-in-time"
:iff-def (integer ?x))

(def-class month-in-time ()?x
"A month-in-time is an integer in the interval 0-12"
:iff-def (and (integer ?x)(< ?x 13) (or (= ?X 0)(> ?x 0))))

(def-class week-in-time () ?x
  "A week must be an integer in the interval of 0-7"
        :iff-def (and (integer ?x) (or (< ?x 7) (= ?x 7)) (> ?x 0)))

(def-class day-in-time ()?x
"A day-in-time is an integer in the interval 0-31"
:iff-def (and (integer ?x)(< ?x 32) (or (= ?X 0)(> ?x 0))))

(def-class hour-in-time ()?x
"A hour-in-time is an integer in the interval 0-23"
:iff-def (and (integer ?x)(< ?x 24) (or (= ?X 0)(> ?x 0))))

(def-class second-in-time ()?x
"A second-in-time is an integer in the interval 0-59"
:iff-def (and (integer ?x)(< ?x 60) (or (= ?X 0)(> ?x 0))))

(def-class minute-in-time ()?x
"A minute-in-time is an integer in the interval 0-59"
:iff-def (and (integer ?x)(< ?x 60) (or (= ?X 0)(> ?x 0))))

(def-class temporal-thing ()
  "Like in Cyc, this is something which has a temporal extent."
  ((has-duration :type duration)
   (has-start-time :type time-point)
   (has-end-time :type time-point)
  ))

(def-class time-entity () ?te
 ((second-of :type second-in-time :default-value 0 :max-cardinality 1)
  (minute-of :type minute-in-time :default-value 0 :max-cardinality 1)
  (hour-of :type hour-in-time  :default-value 0 :max-cardinality 1)
  (day-of :type day-in-time :default-value 0 :max-cardinality 1)
  (month-of :type month-in-time :default-value 0 :max-cardinality 1)
  (year-of :type year-in-time :default-value 0 :max-cardinality 1))
 :constraint (and (not (and (month-of ?x 2)
                            (> (the ?day (day-of ?x ?day))
                               29)))
                  (not (and (member-of ?x (4 6 9 11))
                            (> (the ?day (day-of ?x ?day))
                               30))))) 

(def-class duration (time-entity) ?d
 )

(def-class time-point(time-entity) ?tp
 )

(def-class unit-of-time ()
 "This can be a second, a month, a year, a day, etc..")


(def-class calendar-date (time-point)
 "A calendar date is a time point in which month, day and year have 
  been specified"
  ((day-of :type day-in-time :cardinality 1)
   (month-of :type month-in-time :cardinality 1)
   (year-of :type year-in-time :cardinality 1)))


(def-class interval(duration)
 "An interval is a duration.  The difference is conceptual: in a duration something happens. 
  We have an interval when nothing happens")


(def-function universal-time-encoder (?tp)
"This function encodes the standard structure of time-point into universal-time structure."
 :constraint (time-point ?tp)
 :lisp-fun  '(lambda (?tp)
               (encode-universal-time (the-slot-value ?tp 'second-of)
                                      (the-slot-value ?tp 'minute-of)
                                      (the-slot-value ?tp 'hour-of)
                                      (the-slot-value ?tp 'day-of)
                                      (the-slot-value ?tp 'month-of)
                                      (the-slot-value ?tp 'year-of))))


(def-class universal-time () ?ut
 :constraint (integer ?x))


(def-function decode-time-point-from-universal-time (?ut)
 :constraint (universal-time ?ut)
 :lisp-fun '(lambda (?ut)
              (multiple-value-bind
                  (second minute hour day month year ignore1 ignore2 ignore3)
                  (decode-universal-time ?ut)
                (name 
                 (define-domain-instance (gentemp "TIME-POINT") 'time-point 
                                         `((second-of ,second)
                                           (minute-of ,minute)
                                           (hour-of ,hour)
                                           (day-of ,day)
                                           (month-of ,month)
                                           (year-of ,year)))))))


(def-function decode-duration-from-universal-time (?ut)
 :constraint (universal-time ?ut)
 :lisp-fun '(lambda (?ut)
              (multiple-value-bind
                  (second minute hour day month year ignore1 ignore2 ignore3)
                  (decode-universal-time ?ut)
                (name 
                 (define-domain-instance (gentemp "DURATION") 'duration 
                                         `((second-of ,second)
                                           (minute-of ,minute)
                                           (hour-of ,hour)
                                           (day-of ,day)
                                           (month-of ,month)
                                           (year-of ,year)))))))


(def-function time-entity-difference (?te-1 ?te-2)
"This function calculates the difference of two universal-time strctures."
 :constraint (and (time-entity ?te-1)
                  (time-entity ?te-2))
 :body (decode-time-point-from-universal-time
        (- (universal-time-encoder ?te-1) (universal-time-encoder ?te-2))))

(def-function time-entity-sum (?te-1 ?te-2)
"This function calculates the sum of two universal-time structures."
 :constraint (and (time-entity ?te-1)
                  (time-entity ?te-2))
 :body (decode-time-point-from-universal-time
        (+ (universal-time-encoder ?te-1) (universal-time-encoder ?te-2)))) 


(def-relation duration-is-less-than (?d1 ?d2)
 :constraint (and (duration ?d1)
                  (duration ?d2))
 :iff-def (< (universal-time-encoder ?d1)
             (universal-time-encoder ?d2)))


(def-class TIME-RANGE () ?tr
  ((has-start-time :type time-point :cardinality 1)
   (has-end-time :type time-point :cardinality 1)
   (has-unit-of-time :type unit-of-time))
    :iff-def (precedes (the-slot-value ?tr has-start-time)
                       (the-slot-value ?tr has-end-time)))


(def-function TIME-RANGE-DURATION (?tr) -> ?duration
  :constraint (time-range ?tr)
  :body (time-point-difference (the ?et (has-end-time ?tr ?et))
                                (the ?st (has-start-time ?tr ?st))))



(def-instance second unit-of-time)

(def-instance minute unit-of-time)

(def-instance hour unit-of-time)

(def-instance day unit-of-time)

(def-instance month unit-of-time)

(def-instance year unit-of-time)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;**;;;These are few useful relations for the Time-Ranges;;;*;;;

(def-relation precedes (?time-point-1 ?time-point-2)
 "This relation relation states that a time-point ?time-point-1 preceeds a time-point ?time-point-2."
 :constraint (and (time-point ?time-point-1)
                  (time-point ?time-point-2))
 :iff-def (< (universal-time-encoder ?time-point-1)
             (universal-time-encoder ?time-point-2)))
                

(def-relation follows (?time-point-1 ?time-point-2)
 "This relation relation states that a time-point ?time-point-2 follows a time-point ?time-point-1."
 :constraint (and (time-point ?time-point-1)
                  (time-point ?time-point-2))
 :iff-def (precedes ?time-point-2 ?time-point-1)) 
            

(def-relation time-points-equals (?time-point-1 ?time-point-2)
 :constraint (and (time-point ?time-point-1)
                  (time-point ?time-point-2))
 :iff-def (and (= (minute-of ?time-point-1)
                  (minute-of ?time-point-2))
               (= (second-of ?time-point-1)
                  (second-of ?time-point-2))
               (= (hour-of ?time-point-1)
                  (hour-of ?time-point-2))
               (= (day-of ?time-point-1)
                  (day-of ?time-point-2))
               (= (month-of ?time-point-1)
                  (month-of ?time-point-2))
               (= (year-of ?time-point-1)
                  (year-of ?time-point-2))))


;;;These are BASIC relations;;;

(def-relation before (?time-range-1 ?time-range-2)
"It means time-range-1 is before the time-range-2."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (precedes (the ?et (has-end-time ?time-range-1 ?et))
                    (the ?st (has-start-time ?time-range-2 ?st))))


(def-relation after (?time-range-1 ?time-range-2)
"It means time-range-1 is after the time-range-2."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (precedes (the ?et (has-end-time ?time-range-2 ?et))
                    (the ?st (has-start-time ?time-range-1 ?st))))


(def-relation meets (?time-range-1 ?time-range-2)
"It means that time-range-2 starts at the same time when time-range-1 ends."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (time-points-equals (the ?et (has-end-time ?time-range-1 ?et))
                              (the ?st (has-start-time ?time-range-2 ?st))))


(def-relation overlap (?time-range-1 ?time-range-2)
"It means that two time-ranges overlap with each other."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (and (precedes (the ?st (has-start-time ?time-range-1 ?st))
                         (the ?st (has-start-time ?time-range-2 ?st)))
               (precedes (the ?st (has-start-time ?time-range-2 ?st))
                         (the ?et (has-end-time ?time-range-1 ?et)))
               (precedes (the ?et (has-end-time ?time-range-1 ?et))
                         (the ?et (has-end-time ?time-range-2 ?et)))))


(def-relation job-time-ranges-overlap (?job-time-range-1 ?job-time-range-2)
"It is exclusively defined to express the overlapping of job-time-ranges."
 :constraint (and (job-time-range ?job-time-range-1)
                  (job-time-range ?job-time-range-2))
 :iff-def (and (or (precedes (the ?est (has-earliest-start-time ?job-time-range-1 ?est))
                             (the ?est (has-earliest-start-time ?job-time-range-2 ?est)))
                   (precedes (the ?lst (has-latest-start-time ?job-time-range-1 ?lst))
                             (the ?lst (has-latest-start-time ?job-time-range-2 ?lst))))
                   (precedes (the ?est (has-earliest-start-time ?job-time-range-2 ?est))
                             (the ?let (has-latest-end-time ?job-time-range-1 ?let)))
               (or (precedes (the ?eet (has-earliest-end-time ?job-time-range-1 ?eet))
                             (the ?eet (has-earliest-end-time ?job-time-range-2 ?eet)))
                   (precedes (the ?let (has-latest-end-time ?job-time-range-1 ?let))
                             (the ?let (has-latest-end-time ?job-time-range-2 ?let))))))


(def-relation starts-simultaneously (?time-range-1 ?time-range-2)
"It means that both the time-ranges starts at the same time."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (time-points-equals (the ?st (has-start-time ?time-range-1 ?st))
                              (the ?st (has-start-time ?time-range-2 ?st))))
                         


(def-relation finishes-simultaneously (?time-range-1 ?time-range-2)
"It means that both the time-ranges finishes at the same time but time-range-1 starts after time-range-2."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (time-points-equals (the ?et (has-end-time ?time-range-1 ?et))
                              (the ?et (has-end-time ?time-range-2 ?et))))


(def-relation time-range-equals (?time-range-1 ?time-range-2)
"It means that both the time-ranges starts and finsihes at the same time."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (and (time-point-equals (the ?st (has-start-time ?time-range-1 ?st))
                                  (the ?st (has-start-time ?time-range-2 ?st)))
               (time-point-equals (the ?et (has-end-time ?time-range-1 ?et))
                                  (the ?et (has-end-time ?time-range-2 ?et)))))


(def-relation is-after (?time-range-1 ?time-range-2)
"It means that time-range-2 starts after the time-range-1 is finished."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (follows (the ?st (has-start-time ?time-range-2 ?st))
                   (the ?et (has-end-time ?time-range-1 ?et))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;These are DERIVED Relations;;;

(def-relation is-during (?time-range-1 ?time-range-2)
"It means that time-range-2 is in between (during) the the start and end time of time-range-1."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (and (precedes (the ?st (has-start-time ?time-range-1 ?st))
                         (the ?st (has-start-time ?time-range-2 ?st)))
               (follows (the ?et (has-end-time ?time-range-1 ?et))
                        (the ?et (has-end-time ?time-range-2 ?et)))))



(def-relation before-or-equal (?time-range-1 ?time-range-2)
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (or (before ?time-range-1 ?time-range-2)
              (meets ?time-range-1 ?time-range2)))


(def-relation after-or-equal (?time-range-1 ?time-range-2)
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (or (after ?time-range-1 ?time-range-2)
              (meets ?time-range-1 ?time-range-2)))


(def-relation is-after-than (?time-range-1 ?time-range-2)
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (is-after ?time-range-2 ?time-range-1))


(def-relation during-or-equal (?time-range-1 ?time-range-2)
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (or (is-during ?time-range-1 ?time-range-2)
              (starts-simultaneously ?time-range-1 ?time-range-2)
              (finishes-simultaneously ?time-range-1 ?time-range-2)
              (time-range-equals ?time-range-1 ?time-range-2)))


(def-relation overlaps-or-equal (?time-range-1 ?time-range-2)
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (or (overlap ?time-range-1 ?time-range-2)
              (meets ?time-range-1 ?time-range-2)))


(def-relation starts-or-equal (?time-range-1 ?time-range-2)
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (or (starts-simultaneously ?time-range-1 ?time-range-2)
              (time-range-equals ?time-range-1 ?time-range-2)))


(def-relation finishes-or-equals (?time-range-1 ?time-range-2)
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (or (finishes-simultaneously ?time-range-1 ?time-range-2)
              (time-range-equals ?time-range-1 ?time-range-2)))


(def-relation disjoint-time-ranges (?time-range-1 ?time-range-2)
"It is true if either time-range-1 is before time-range-2 or time-range-2 is before time-range-1."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (or (before ?time-range-1 ?time-range-2)
              (before ?time-range-2 ?time-range-1)))



(def-relation time-ranges-exceed (?job-time-range ?time-range)
 :constraint (and (exists ?j (and (job ?j)
                                  (has-time-range ?j ?job-time-range)))
                  (time-range ?tr))
 :iff-def (and (precedes (the ?est (has-earliest-start-time ?job-time-range ?est))
                         (the ?et (has-end-time ?time-range ?et)))
               (follows (the ?let (has-latest-end-time ?job-time-range ?let))
                        (the ?st (has-start-time ?tr ?st)))))


(def-relation time-ranges-intersect (?jtr ?tr)
 :constraint (and (job-time-range ?jtr)
                  (time-range ?tr))
 :iff-def (and (follows (the ?est (has-earliest-start-time ?jtr ?est))
                        (the ?st (has-start-time ?tr ?st)))
               (follows (the ?lst (has-latest-start-time ?jtr ?lst))
                        (the ?st (has-start-time ?tr ?st)))
               (precedes (the ?eet (has-earliest-end-time ?jtr ?eet))
                         (the ?et (has-end-time ?tr ?et)))
               (precedes (the ?let (has-latest-end-time ?jtr ?let))
                         (the ?et (has-end-time ?tr ?et)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;This function decodes the subtracted result of time-point difference into again standard time-point structure.;;;
(defun universal-time-decoder (x y)
  (multiple-value-list (decode-universal-time (- (ocml-eval-gen `(universal-time-encoder ',x)) (ocml-eval-gen `(universal-time-encoder ',y))))))



