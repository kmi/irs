;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(def-ontology simple-time)

(in-ontology simple-time)
  
(def-class year-in-time ()?x
	"A year-in-time must be an integer and integer can be a year-in-time"
	:iff-def (integer ?x))

(def-class month-in-time ()?x
	"A month-in-time is an integer in the interval 1-12"
	:iff-def (and (integer ?x)(< ?x 13) (> ?x 0)))

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

(def-class time-entity ()
  "The constraint below ought to be extended to handle leap years"
  ((minute-of :type minute-in-time :max-cardinality 1 :default-value 0)
   (second-of :type second-in-time :max-cardinality 1 :default-value 0)
   (hour-of :type hour-in-time :max-cardinality 1 :default-value 0)
   (day-of :type day-in-time :max-cardinality 1 :default-value 1)
   (month-of :type month-in-time :max-cardinality 1 :default-value 1)
   (year-of :type year-in-time :max-cardinality 1 :default-value 0))
  :constraint  (and (not (and (month-of ?x 2)
                             (> (the ?day (day-of ?x ?day))
                                29)))
                    (not (and (member-of ?x (4 6 9 11))
                              (> (the ?day (day-of ?x ?day))
                                30)))))
                    

(def-class duration (time-entity))

(def-class time-point (time-entity))

(def-function universal-time-encoder (?tp)
"This function encodes the standard structure of time-point into universal-time structure."
 :constraint (time-entity ?tp)
 :lisp-fun  '(lambda (?tp)
  (encode-universal-time (the-slot-value ?tp 'second-of)
                         (the-slot-value ?tp 'minute-of)
                         (the-slot-value ?tp 'hour-of)
                         (the-slot-value ?tp 'day-of)
                         (the-slot-value ?tp 'month-of)
                         (the-slot-value ?tp 'year-of))))


(def-function decode-time-point-from-universal-time (?ut)
  :constraint (universal-time ?ut)
  :lisp-fun  '(lambda (?ut)
                (multiple-value-bind 
                  (second minute hour day month year ignore1 ignore2 ignore3)
                  (decode-universal-time ?ut)
                  (name 
                   (define-domain-instance (gentemp "TIME-POINT")'time-point
                     `((second-of ,second)
                       (minute-of ,minute)
                       (hour-of ,hour)
                       (day-of ,day)
                       (month-of ,month)
                       (year-of ,year)))))))


(def-function decode-duration-from-universal-time (?ut)
  :constraint (universal-time ?ut)
  :lisp-fun  '(lambda (?ut)
                (multiple-value-bind 
                  (second minute hour day month year ignore1 ignore2 ignore3)
                  (decode-universal-time ?ut)
                  (name 
                   (define-domain-instance (gentemp "DURATION")'duration
                     `((second-of ,second)
                       (minute-of ,minute)
                       (hour-of ,hour)
                       (day-of ,day)
                       (month-of ,month)
                       (year-of ,year)))))))      


(def-function TIME-ENTITY-DIFFERENCE (?tp-1 ?tp-2)
  "This function returns the difference between two time entities as a time entity"
 :constraint (and (time-entity ?tp-1)
                  (time-entity ?tp-2))
 :body (decode-duration-from-universal-time
        (- (universal-time-encoder ?tp-1) (universal-time-encoder ?tp-2))))

(def-class unit-of-time ()
	"This can be a second, a month, a year, a day, etc..")

(def-class calendar-date (time-point)
 "A calendar date is a time point in which month, day and year have 
  been specified"
  ((day-of :type day-in-time :cardinality 1)
   (month-of :type month-in-time :cardinality 1)
   (year-of :type year-in-time :cardinality 1)))


(def-instance year unit-of-time)

(def-instance month unit-of-time)

(def-instance day unit-of-time)

(def-instance hour unit-of-time)

(def-instance minute unit-of-time)

(def-instance second unit-of-time)


(def-class TIME-RANGE () ?tr
  ((has-start-time :type time-point :cardinality 1)
   (has-end-time :type time-point :cardinality 1)
   )
    :constraint (precedes (the-slot-value ?tr has-start-time)
                          (the-slot-value ?tr has-end-time)))



(def-function TIME-RANGE-DURATION (?tr) -> ?duration
  :constraint (time-range ?tr)
  :body (TIME-ENTITY-DIFFERENCE  (the ?et (has-end-time ?tr ?et))
                                 (the ?st (has-start-time ?tr ?st))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 :iff-def (= (universal-time-encoder ?time-point-1)
             (universal-time-encoder ?time-point-2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-relation duration-less-than (?d1 ?te)
  :constraint (and (duration ?d1)
                  (time-entity ?te))
  :iff-def (< (universal-time-encoder ?d1)
              (universal-time-encoder ?te)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;**;;;These are few useful relations for the Time-Ranges;;;*;;;

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
  :iff-def (before ?time-range-2 ?time-range-1)) 


(def-relation meets (?time-range-1 ?time-range-2)
"It means that time-range-2 starts at the same time when time-range-1 ends."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (time-points-equals (the ?et (has-end-time ?time-range-1 ?et))
                              (the ?st (has-start-time ?time-range-2 ?st))))


(def-relation OVERLAP (?time-range-1 ?time-range-2)
"It means that two time-ranges overlap with each other."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (and (precedes (the ?st (has-start-time ?time-range-1 ?st))
                         (the ?st (has-start-time ?time-range-2 ?st)))
               (precedes (the ?st (has-start-time ?time-range-2 ?st))
                         (the ?et (has-end-time ?time-range-1 ?et)))
               (precedes (the ?et (has-end-time ?time-range-1 ?et))
                         (the ?et (has-end-time ?time-range-2 ?et)))))



(def-relation starts-simultaneously (?time-range-1 ?time-range-2)
  "It means that both the time-ranges starts at the same time but time-range-1 ends before time-range-2."
  :constraint (and (time-range ?time-range-1)
                   (time-range ?time-range-2))
  :iff-def ;;;(and 
            (time-points-equals (the ?st (has-start-time ?time-range-1 ?st))
                                    (the ?st (has-start-time ?time-range-2 ?st))))
                ;;(precedes (the ?et (has-end-time ?time-range-1 ?et))
                ;;          (the ?et (has-end-time ?time-range-2 ?et)))))


(def-relation finishes-simultaneously (?time-range-1 ?time-range-2)
"It means that both the time-ranges finishes at the same time but time-range-1 starts after time-range-2."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def ;;;;(and 
           (follows (the ?st (has-start-time ?time-range-1 ?st))
                        (the ?st (has-start-time ?time-range-2 ?st))))

               ;;;(time-points-equals (the ?et (has-end-time ?time-range-1 ?et))
                ;;;;                   (the ?et (has-end-time ?time-range-2 ?et)))))


(def-relation time-range-equals (?time-range-1 ?time-range-2)
"It means that both the time-ranges starts and finsihes at the same time."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (and (time-point-equals (the ?st (has-start-time ?time-range-1 ?st))
                                  (the ?st (has-start-time ?time-range-2 ?st)))
               (time-point-equals (the ?et (has-end-time ?time-range-1 ?et))
                                  (the ?et (has-end-time ?time-range-2 ?et)))))


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

(def-relation disjoint-time-ranges (?time-range-1 ?time-range-2)
"It is true if either time-range-1 is before time-range-2 or time-range-2 is before time-range-1."
 :constraint (and (time-range ?time-range-1)
                  (time-range ?time-range-2))
 :iff-def (or (before ?time-range-1 ?time-range-2)
              (before ?time-range-2 ?time-range-1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

