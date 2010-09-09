;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology simple-time-modified)

(def-class year-in-time ()?x
  "A year-in-time must be an integer and integer can be a year-in-time"
  :iff-def (integer ?x))

(def-class month-in-time ()?x
  "A month-in-time is an integer in the interval 0-12"
  :iff-def (and (integer ?x)(< ?x 13) (or (= ?X 0)(> ?x 0))))

(def-class day-in-time ()?x
  "A day-in-time is an integer in the interval 1-31"
  :iff-def (and (integer ?x)(< ?x 32) (> ?x 0)))

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

(def-class duration (pair) ?x
  "We define duration as a pair <quantity, unit-of-time>,
where unit-of-time can be day, hour, month, etc."
  :constraint (and (pair ?x)
                   (positive-real-number (first ?x))
                   (unit-of-time (second ?x)))
  :lisp-fun '(lambda (x env)
               (let ((instantiated-x (instantiate x env)))
                 (if (and (listp instantiated-x) (= (length instantiated-x) 2)
                          (holds? 'positive-real-number (car instantiated-x))
                          (holds? 'unit-of-time (second instantiated-x)))
                   (list env)
                   :fail))))

(def-class time-point() ?x
  "Something which is conceptually a point in time, even if partially specified."
  ((minute-of :type minute-in-time :max-cardinality 1 :default-value 0)
   (second-of :type second-in-time :max-cardinality 1 :default-value 0)
   (hour-of :type hour-in-time :max-cardinality 1)
   (day-of :type day-in-time :max-cardinality 1)
   (month-of :type month-in-time :max-cardinality 1)
   (year-of :type year-in-time :max-cardinality 1))
  :constraint (and (not (and (month-of ?x 2)
                        (> (the ?day (day-of ?x ?day))
                           28)))
                   (not (and (member (the ?month (month-of ?x ?month))
                                     '(4 6 9 11))
                             (> (the ?day (day-of ?x ?day))
                                30)))))

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

(def-function duration-quantity (?x)
  "A duration is a pair <quantity, unit-of-time>.
This function returns the quantity component of a duration"
  :constraint (duration ?X)
  :body (first ?x))

(def-function duration-unit (?x)
  "A duration is a pair <quantity, unit-of-time>.
This function returns the unitcomponent of a duration"
  :constraint (duration ?X)
  :body (second ?x))

(def-instance year unit-of-time)

(def-instance decade unit-of-time)

(def-instance century unit-of-time)

(def-instance month unit-of-time)

(def-instance day unit-of-time)

(def-instance hour unit-of-time)

(def-instance minute unit-of-time)

(def-instance second unit-of-time)

(def-instance millisecond unit-of-time)

(def-instance nanosecond unit-of-time)

(def-instance millennium unit-of-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;**;;;These are few useful relations for the Time-Ranges;;;*;;;

(def-class TIME-RANGE () ?tr
  ((has-start-time :type time-point :cardinality 1)
   (has-end-time :type time-point :cardinality 1)
   (has-unit-of-time :type unit-of-time :cardinality 1))
  :iff-def (< (the-slot-value ?tr has-start-time)
              (the-slot-value ?tr has-end-time)))



(def-function TIME-RANGE-DURATION (?tr) -> ?duration
  :constraint (time-range ?tr)
  :body (list-of (- (the ?et (has-end-time ?tr ?et))
                    (the ?st (has-start-time ?tr ?st))
                    (the ?ut (has-unit-of-time ?tr ?ut)))))






(def-relation before-others (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (precedes (the ?et (has-end-time ?time-range-1 ?et))
                    (the ?st (has-start-time ?time-range-2 ?st))))

(def-relation after-others (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (precedes (the ?et (has-end-time ?time-range-2 ?et))
                    (the ?st (has-start-time ?time-range-1 ?st))))

(def-relation is-after (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (follows (the ?st (has-start-time ?time-range-2 ?st))
                    (the ?et (has-end-time ?time-range-1 ?et))))

(def-relation meets (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (time-points-equals (the ?et (has-end-time ?time-range-1 ?et))
                               (the ?st (has-start-time ?time-range-2 ?st))))

(def-relation is-during-others (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (and (precedes (the ?st (has-start-time ?time-range-1 ?st))
                         (the ?st (has-start-time ?time-range-2 ?st)))
                (follows (the ?et (has-end-time ?time-range-1 ?et))
                         (the ?et (has-end-time ?time-range-2 ?et)))))

(def-relation overlaps (?time-range-1 ?time-range-2)
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

(def-relation starts (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (and (time-points-equals (the ?st (has-start-time ?time-range-1 ?st))
                                    (the ?st (has-start-time ?time-range-2 ?st)))
                (precedes (the ?et (has-end-time ?time-range-1 ?et))
                          (the ?et (has-end-time ?time-range-2 ?et)))))


(def-relation finishes (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (and (follows (the ?st (has-start-time ?time-range-1 ?st))
                         (the ?st (has-start-time ?time-range-2 ?st)))
                (time-points-equals (the ?et (has-end-time ?time-range-1 ?et))
                                    (the ?et (has-end-time ?time-range-2 ?et)))))


(def-relation before-or-equal (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (or (before-others ?time-range-1 ?time-range-2)
               (meets ?time-range-1 ?time-range2)))


(def-relation after-or-equal (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (or (after-others ?time-range-1 ?time-range-2)
               (meets ?time-range-1 ?time-range-2)))


(def-relation is-after-than (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (is-after ?time-range-2 ?time-range-1))


(def-relation during-or-equal (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (or (is-during-others ?time-range-1 ?time-range-2)
               (and 
                (starts ?time-range-1 ?time-range-2)
                (finishes ?time-range-1 ?time-range-2)
                (time-range-equals ?time-range-1 ?time-range-2)))


(def-relation overlaps-or-equal (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (or (overlaps ?time-range-1 ?time-range-2)
               (meets ?time-range-1 ?time-range-2)))


(def-relation starts-or-equal (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (or (starts ?time-range-1 ?time-range-2)
               (time-range-equals ?time-range-1 ?time-range-2)))


(def-relation finishes-or-equals (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (or (finishes ?time-range-1 ?time-range-2)
               (time-range-equals ?time-range-1 ?time-range-2)))


(def-relation disjoint-time-ranges (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (or (before-others ?time-range-1 ?time-range-2)
               (before-others ?time-range-2 ?time-range-1)))


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


(def-relation time-range-equals (?time-range-1 ?time-range-2)
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def (and (time-point-equals (the ?st (has-start-time ?time-range-1 ?st))
                                   (the ?st (has-start-time ?time-range-2 ?st)))
                (time-point-equals (the ?et (has-end-time ?time-range-1 ?et))
                                   (the ?et (has-end-time ?time-range-2 ?et)))))



(def-relation precedes (?time-point-1 ?time-point-2)
  "This relation relation states that a time-point ?time-point-1 preceeds a time-point ?time-point-2."
  :constraint (and (time-point ?time-point-1)
                   (time-point ?time-point-2))
  :iff-def (or 
            (and (year-in-time (year-of ?time-point-1))
                 (year-in-time (year-of ?time-point-2))
                 (< (year-of ?time-point-1)
                    (year-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (< (month-of ?time-point-1)          
                (month-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (= (month-of ?time-point-1)
                (month-of ?time-point-2))
             (week-in-time (week-of ?time-point-1))
             (week-in-time (week-of ?time-point-2))
             (< (week-of ?time-point-1)
                (week-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (= (month-of ?time-point-1)
                (month-of ?time-point-2))
             (week-in-time (week-of ?time-point-1))
             (week-in-time (week-of ?time-point-2))
             (= (week-of ?time-point-1)
                (week-of ?time-point-2))
             (day-in-time (day-of ?time-point-1))
             (day-in-time (day-of ?time-point-2))
             (< (day-of ?time-point-1)
                (day-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (= (month-of ?time-point-1)
                (month-of ?time-point-2))
             (week-in-time (week-of ?time-point-1))
             (week-in-time (week-of ?time-point-2))
             (= (week-of ?time-point-1)
                (week-of ?time-point-2))
             (day-in-time (day-of ?time-point-1))
             (day-in-time (day-of ?time-point-2))
             (= (day-of ?time-point-1)
                (day-of ?time-point-2))
             (hour-in-time (hour-of ?time-point-1))
             (hour-in-time (hour-of ?time-point-2))
             (< (hour-of ?time-point-1)
                (hour-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (= (month-of ?time-point-1)
                (month-of ?time-point-2))
             (week-in-time (week-of ?time-point-1))
             (week-in-time (week-of ?time-point-2))
             (= (week-of ?time-point-1)
                (week-of ?time-point-2))
             (day-in-time (day-of ?time-point-1))
             (day-in-time (day-of ?time-point-2))
             (= (day-of ?time-point-1)
                (day-of ?time-point-2))
             (hour-in-time (hour-of ?time-point-1))
             (hour-in-time (hour-of ?time-point-2))
             (= (hour-of ?time-point-1)
                (hour-of ?time-point-2))
             (minute-in-time (minute-of ?time-point-1))
             (minute-in-time (minute-of ?time-point-2))
             (< (minute-of ?time-point-1)
                (minute-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (= (month-of ?time-point-1)
                (month-of ?time-point-2))
             (week-in-time (week-of ?time-point-1))
             (week-in-time (week-of ?time-point-2))
             (= (week-of ?time-point-1)
                (week-of ?time-point-2))
             (day-in-time (day-of ?time-point-1))
             (day-in-time (day-of ?time-point-2))
             (= (day-of ?time-point-1)
                (day-of ?time-point-2))
             (hour-in-time (hour-of ?time-point-1))
             (hour-in-time (hour-of ?time-point-2))
             (= (hour-of ?time-point-1)
                (hour-of ?time-point-2))
             (minute-in-time (minute-of ?time-point-1))
             (minute-in-time (minute-of ?time-point-2))
             (= (minute-of ?time-point-1)
                (minute-of ?time-point-2))
             (second-in-time (second-of ?time-point-1))
             (second-in-time (second-of ?time-point-2))
             (< (second-of ?time-point-1)
                (second-of ?time-point-2)))))



(def-relation later-than (?time-point-1 ?time-point-2)
  :constraint (and (time-point ?time-point-1)
                   (time-point ?time-point-2))
  :iff-def (> ?time-point-2 ?time-point-1))


(def-relation earlier-than (?time-point-1 ?time-point-2)
  :constraint (and (time-point ?time-point-1)
                   (time-point ?time-point-2))
  :iff-def (< ?time-point-1 ?time-point-2)) 


(def-relation follows (?time-point-1 ?time-point-2)
  "This relation relation states that a time-point ?time-point-2 follows a time-point ?time-point-1."
  :constraint (and (time-point ?time-point-1)
                   (time-point ?time-point-2))
  :iff-def (or 
            (and (year-in-time (year-of ?time-point-1))
                 (year-in-time (year-of ?time-point-2))
                 (> (year-of ?time-point-1)
                    (year-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (> (month-of ?time-point-1)          
                (month-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (= (month-of ?time-point-1)
                (month-of ?time-point-2))
             (week-in-time (week-of ?time-point-1))
             (week-in-time (week-of ?time-point-2))
             (> (week-of ?time-point-1)
                (week-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (= (month-of ?time-point-1)
                (month-of ?time-point-2))
             (week-in-time (week-of ?time-point-1))
             (week-in-time (week-of ?time-point-2))
             (= (week-of ?time-point-1)
                (week-of ?time-point-2))
             (day-in-time (day-of ?time-point-1))
             (day-in-time (day-of ?time-point-2))
             (> (day-of ?time-point-1)
                (day-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (= (month-of ?time-point-1)
                (month-of ?time-point-2))
             (week-in-time (week-of ?time-point-1))
             (week-in-time (week-of ?time-point-2))
             (= (week-of ?time-point-1)
                (week-of ?time-point-2))
             (day-in-time (day-of ?time-point-1))
             (day-in-time (day-of ?time-point-2))
             (= (day-of ?time-point-1)
                (day-of ?time-point-2))
             (hour-in-time (hour-of ?time-point-1))
             (hour-in-time (hour-of ?time-point-2))
             (> (hour-of ?time-point-1)
                (hour-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (= (month-of ?time-point-1)
                (month-of ?time-point-2))
             (week-in-time (week-of ?time-point-1))
             (week-in-time (week-of ?time-point-2))
             (= (week-of ?time-point-1)
                (week-of ?time-point-2))
             (day-in-time (day-of ?time-point-1))
             (day-in-time (day-of ?time-point-2))
             (= (day-of ?time-point-1)
                (day-of ?time-point-2))
             (hour-in-time (hour-of ?time-point-1))
             (hour-in-time (hour-of ?time-point-2))
             (= (hour-of ?time-point-1)
                (hour-of ?time-point-2))
             (minute-in-time (minute-of ?time-point-1))
             (minute-in-time (minute-of ?time-point-2))
             (> (minute-of ?time-point-1)
                (minute-of ?time-point-2)))
            
            (and 
             (year-in-time (year-of ?time-point-1))
             (year-in-time (year-of ?time-point-2))
             (= (year-of ?time-point-1)
                (year-of ?time-point-2))
             (month-in-time (month-of ?time-point-1))
             (month-in-time (month-of ?time-point-2))
             (= (month-of ?time-point-1)
                (month-of ?time-point-2))
             (week-in-time (week-of ?time-point-1))
             (week-in-time (week-of ?time-point-2))
             (= (week-of ?time-point-1)
                (week-of ?time-point-2))
             (day-in-time (day-of ?time-point-1))
             (day-in-time (day-of ?time-point-2))
             (= (day-of ?time-point-1)
                (day-of ?time-point-2))
             (hour-in-time (hour-of ?time-point-1))
             (hour-in-time (hour-of ?time-point-2))
             (= (hour-of ?time-point-1)
                (hour-of ?time-point-2))
             (minute-in-time (minute-of ?time-point-1))
             (minute-in-time (minute-of ?time-point-2))
             (= (minute-of ?time-point-1)
                (minute-of ?time-point-2))
             (second-in-time (second-of ?time-point-1))
             (second-in-time (second-of ?time-point-2))
             (> (second-of ?time-point-1)
                (second-of ?time-point-2)))))