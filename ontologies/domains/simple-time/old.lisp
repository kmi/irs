;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology simple-time)

(def-class unit-of-time ()
	"This can be a second, a month, a year, a day, etc..")

(def-class time-point()
 "Something which is conceptually a point in time, even if partially specified"
	((minute-of :type minute-in-time :max-cardinality 1)
	(second-of :type second-in-time :max-cardinality 1)
	(hour-of :type hour-in-time :max-cardinality 1)
	(day-of :type day-in-time :max-cardinality 1)
	(month-of :type month-in-time :max-cardinality 1)
	(year-of :type year-in-time :max-cardinality 1)))

(def-class duration (pair) ?x
	"We define duration as a pair <quantity, unit-of-time>,
where unit-of-time can be day, hour, month, etc."
  :iff-def (and (pair ?x)
                (positive-real-number (first ?x))
                (unit-of-time (second ?x)))
   :lisp-fun '(lambda (x env)
                (let ((instantiated-x (instantiate x env)))
                   (if (and (listp instantiated-x) (= (length instantiated-x) 2)
                            (holds? 'positive-real-number (car instantiated-x))
                            (holds? 'unit-of-time (second instantiated-x)))
                      (list env)
                      :fail))))

(def-class temporal-thing ()
  "Like in Cyc, this is something which has a temporal extent"
  ((has-duration :type duration)
   (start-time :type time-point)
   (end-time :type time-point)
  ))

(def-class minute-in-time ()?x
	"A minute-in-time is an integer in the interval 0-59"
	:iff-def (and (integer ?x)(< ?x 60) (or (= ?X 0)(> ?x 0))))

(def-class second-in-time ()?x
	"A second-in-time is an integer in the interval 0-59"
	:iff-def (and (integer ?x)(< ?x 60) (or (= ?X 0)(> ?x 0))))

(def-class hour-in-time ()?x
	"A hour-in-time is an integer in the interval 0-23"
	:iff-def (and (integer ?x)(< ?x 24) (or (= ?X 0)(> ?x 0))))

(def-class day-in-time ()?x
	"A day-in-time is an integer in the interval 0-31"
	:iff-def (and (integer ?x)(< ?x 32) (or (= ?X 0)(> ?x 0))))

(def-class month-in-time ()?x
	"A month-in-time is an integer in the interval 0-12"
	:iff-def (and (integer ?x)(< ?x 13) (or (= ?X 0)(> ?x 0))))

(def-class year-in-time ()?x
	"A year-in-time must be an integer and integer can be a year-in-time"
	:iff-def (integer ?x))

(def-class calendar-date (time-point)
 "A calendar date is a time point in which month, day and year have 
  been specified"
  ((day-of :type day-in-time :cardinality 1)
   (month-of :type month-in-time :cardinality 1)
   (year-of :type year-in-time :cardinality 1)))

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



(def-class interval(duration)
 "An interval is a duration.  The difference is conceptual: in a duration something happens. 
  We have an interval when nothing happens")