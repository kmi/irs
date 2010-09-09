;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

;;;A minimalist set of time-related definitions. 
;;;We may want to extend this in the future

(in-ontology akt-support-ontology2)

(def-class YEAR-IN-TIME (integer) ?x
	"A year-in-time must be an integer and integer can be a year-in-time"
	:iff-def (integer ?x)
        :avoid-infinite-loop t)

(def-class MONTH-IN-TIME (positive-integer)?x
	"A month-in-time is an integer in the interval 1-12"
	:iff-def (and (positive-integer ?x)(< ?x 13) )
        :avoid-infinite-loop t)

(def-class DAY-IN-TIME (positive-integer)?x
	"A day-in-time is an integer in the interval 1-31"
	:iff-def (and (positive-integer ?x)(< ?x 32) )
        :avoid-infinite-loop t)

(def-class HOUR-IN-TIME (non-negative-integer)?x
	"A hour-in-time is an integer in the interval 0-23"
	:iff-def (and (non-negative-integer ?x)(< ?x 24) )
        :avoid-infinite-loop t)

(def-class MINUTE-IN-TIME (non-negative-integer) ?x
	"A minute-in-time is an integer in the interval 0-59"
	:iff-def (and (non-negative-integer ?x)(< ?x 60) )
        :avoid-infinite-loop t)



(def-class SECOND-IN-TIME (real-number)?x
  "A second-in-time is a real number greater or equal to 0, less than 60"
  :iff-def (and (real-number ?x)(not (< ?x 0))(< ?x 60))
  :avoid-infinite-loop t)


(def-class TIME-POSITION (intangible-thing)
  "A time position is either a time interval or a time point.
   Any time position is relative to a time zone"
  ((in-timezone :default-value "+00:00" :type timezone)))

(def-class TIMEZONE (string)
  "We represent a time zone as a string with the format 
   {-/+}hh:mm ")




(def-class TIME-POINT (time-position)
  "A point in time"
  ((second-of :type second-in-time :max-cardinality 1 )
   (minute-of :type minute-in-time :max-cardinality 1 )
   (hour-of :type hour-in-time :max-cardinality 1 )
   (day-of :type day-in-time :max-cardinality 1)
   (month-of :type month-in-time :max-cardinality 1)
   (year-of :type year-in-time :max-cardinality 1 ))
  :constraint  (and (not (and (month-of ?x 2)
                             (> (the ?day (day-of ?x ?day))
                                29)))
                    (not (and (member-of ?x (4 6 9 11))
                              (> (the ?day (day-of ?x ?day))
                                30)))))






(def-class CALENDAR-DATE (time-point)
 "A calendar date is a time point in which month, day and year have 
  been specified but hour, minute and second have not"
  ((minute-of :type minute-in-time :max-cardinality 0 )
   (second-of :type second-in-time :max-cardinality 0 )
   (hour-of :type hour-in-time :max-cardinality 0 )
   (day-of :type day-in-time :cardinality 1)
   (month-of :type month-in-time :cardinality 1)
   (year-of :type year-in-time :cardinality 1)))




(def-class TIME-INTERVAL (time-position)
  "An interval is defined by two time points or a duration.  
   Classes of intervals, e.g., a day, can be defined by specifying only
   a duration.  A time interval has no gaps"

  ((begins-at-time-point :type time-point :max-cardinality 1)
   (ends-at-time-point :type time-point :max-cardinality 1)
   (has-duration :type duration :max-cardinality 1)))

(def-class DAY (time-interval)
  ((has-duration :value 24-hour-duration)))

(def-class WEEK (time-interval)
  ((has-duration :value 7-day-duration)))

(def-class MONTH (time-interval))

(def-class JANUARY (month)
  ((has-duration :value 31-day-duration)))

(def-class FEBRUARY (month)
  ((has-duration :default-value 28-day-duration)))

(def-class FEBRUARY-IN-LEAP-YEARS (february)
  ((has-duration :value 29-day-duration)))
  

(def-class MARCH (month)
  ((has-duration :value 31-day-duration)))

(def-class APRIL (month)
  ((has-duration :value 30-day-duration)))

(def-class MAY (month)
  ((has-duration :value 31-day-duration)))

(def-class JUNE (month)
  ((has-duration :value 30-day-duration)))

(def-class JULY (month)
  ((has-duration :value 31-day-duration)))

(def-class AUGUST (month)
  ((has-duration :value 31-day-duration)))

(def-class SEPTEMBER (month)
  ((has-duration :value 30-day-duration)))

(def-class OCTOBER (month)
  ((has-duration :value 31-day-duration)))

(def-class NOVEMBER (month)
  ((has-duration :value 30-day-duration)))

(def-class DECEMBER (month)
  ((has-duration :value 31-day-duration)))

(def-class YEAR (time-interval)
  ((has-duration :value 12-month-duration)))

(def-class DURATION (physical-quantity)
  "A measure of time, e.g., 5 hours"
  ((has-unit-of-measure :type time-measure)
   ))

(def-instance 24-HOUR-DURATION duration
  ((has-unit-of-measure time-measure-hour)
   (has-magnitude 24)))

(def-instance 7-DAY-DURATION duration
  ((has-unit-of-measure time-measure-day)
   (has-magnitude 7)))

(def-instance 28-DAY-DURATION duration
  ((has-unit-of-measure time-measure-day)
   (has-magnitude 28)))

(def-instance 29-DAY-DURATION duration
  ((has-unit-of-measure time-measure-day)
   (has-magnitude 29)))

(def-instance 30-DAY-DURATION duration
  ((has-unit-of-measure time-measure-day)
   (has-magnitude 30)))

(def-instance 31-DAY-DURATION duration
  ((has-unit-of-measure time-measure-day)
   (has-magnitude 31)))

(def-instance 12-MONTH-DURATION duration
  ((has-unit-of-measure time-measure-year)
   (has-magnitude 12)))

(def-class TIME-MEASURE (unit-of-measure)
  "The class of all unit of measures used to measure time,
   e.g., minute, second, hour, etc...")

(def-instance TIME-MEASURE-SECOND time-measure)

(def-instance TIME-MEASURE-MINUTE time-measure)

(def-instance TIME-MEASURE-HOUR time-measure)

(def-instance TIME-MEASURE-DAY time-measure)

(def-instance TIME-MEASURE-MONTH time-measure)

(def-instance TIME-MEASURE-YEAR time-measure)

(def-instance TIME-MEASURE-CENTURY time-measure)


(def-axiom DURATION-IS-BEGIN-TIME-MINUS-END-TIME
  "This axiom states the relation between duration, begin time 
   and end time in an interval"
  (=> (and (time-interval ?x)
           (begins-at-time-point ?x ?tp1)
           (ends-at-time-point ?x ?tp2))
      (= (has-duration ?x (time-difference 
                           (the ?tp1 (begins-at-time-point ?x ?tp1))
                           (the ?tp2 (ends-at-time-point ?x ?tp2)))))))


(def-function TIME-DIFFERENCE (?tp1 ?tp2) -> ?d
  "The duration between two time points.
   No operational definition is given here, only a spec"
  :def (and (time-point ?tp1)
            (time-point ?tp2)
            (duration ?d)))




            


      



