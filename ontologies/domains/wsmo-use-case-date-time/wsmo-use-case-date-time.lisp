;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology wsmo-use-case-date-time)


(def-class instant ())
      
(def-class interval() ?x
:documentation  "   <-
                       X memberof interval and X.start >= X.end."

:constraint (and (start ?x ?x-start)
                 (end ?x ?x-end)
                 (or (before ?x-start ?x-end)
                     (equals-to ?x-start ?x-end)))
((has-start :type instant
            :max-cardinality 1)
 (has-end :type instant
          :max-cardinality 1)))
      
(def-class date(instant) ?x
((has-dayOfMonth :type dayOfMonth
                 :max-cardinality 1)
 (has-monthOfYear :type monthOfYear
                 :max-cardinality 1)
 (has-year :type year
           :max-cardinality 1))
:documentation "<-
                              X memberof date and ( 
                               (X.dayOfMonth > 28 and X.monthOfYear = 2,
                                     not ((modulo(X.year ,4) = 0 and not modulo(X.year ,100) = 0)
                                                      or modulo(X.year ,400) = 0))
                                    or (X.dayOfMonth > 29 , X.monthOfYear = 2)
                                    or (X.dayOfMonth > 30 , X.monthOfYear = 4)
                                    or (X.dayOfMonth > 30 , X.monthOfYear = 6)
                                    or (X.dayOfMonth > 30 , X.monthOfYear = 9)
                                    or (X.dayOfMonth > 30 , X.monthOfYear = 11)
                              )."
:constraint (and (has-monthOfYear ?x ?x-monthOfYear)
                 (has-dayOfMonth ?x ?x-dayOfMonth)
                 (has-year ?x ?x-year)
                 (=> (or (= ?x-monthOfYear 4)
                         (= ?x-monthOfYear 6)
                         (= ?x-monthOfYear 9)
                         (= ?x-monthOfYear 11))
                     (< ?x-dayOfMonth 31))
                 (=> (= ?x-monthOfYear 2)
                     (< ?x-dayOfMonth 30))
                 (=> (and (= ?x-dayOfMonth 2)
                          (or (not (= (modulo ?x-year 4) 0))
                              (= (modulo ?x-year 100) 0))
                          (not (= (modulo ?x-year 400) 0)))
                     (< ?x-dayOfMonth 29))))

(def-class dayOfMonth(integer) ?x
:documentation "<- X memberof dayOfMonth and 
                              (X < 1 or X > 31)."
:constraint (and (< 0 ?x)(> 32 ?x)))
      
(def-class year(integer))

(def-class monthOfYear (integer) ?x
:documentation "<- X memberof monthOfYear and 
                        (X < 1 or X > 12)."
:constraint (and (< 0 ?x)(> 13 ?x)))
      
(def-class time()
((has-hourOfDay :type hourOfDay
                :max-cardinality 1)
 (has-minuteOfHour :type minuteOfHour
                   :max-cardinality 1)
 (has-secondOfMinute :type secondOfMinute
                     :max-cardinality 1)))
            
(def-class secondOfMinute(integer)
:documentation "<-
                    X memberof secondOfMinute and 
                    (X < 0 or X >= 60)."

:constraint (and (< 0 ?x)(> 61 ?x)))
      
(def-class minuteOfHour(integer) ?x
:documentation "<-
                    X memberof minuteOfHour and 
                   (X < 0 or X >= 60)."
:constraint (and (< 0 ?x)(> 61 ?x)))
      
(def-class hourOfDay(integer) ?x
:documentation "<-
                    X memberof hourOfDay and 
                    (X < 0 or X >= 24)."
:constraint (and (< 0 ?x)(> 25 ?x)))
      
(def-class dateAndTime(instant
((has-date :type date
           :max-cardinality 1)
 (has-time :type time
           :max-cardinality 1)))

         
;;; I am not sure if the following would be accepted by OCML?!
;
;(def-function julianDayNumber (?instant) -> ?range
;:constraint (and (instant ?instant)
;                 (integer ?range)))

(def-function julianDayNumber (?d) -> ?i
:documentation "julianDayNumber(X) = JDN <-
                           X memberof date and
                           ((
                                       X.monthOfYear < 3 and
                                       Y = X.year -1 and
                                       M = X.monthOfYear + 12
                                 ) 
                                 or 
                                 (
                                       X.monthOfYear > 2 and
                                       Y = X.year and
                                       M = X.monthOfYear 
                           ))
                           and
                           D = X.dayOfMonth and
                           A = floor(Y / 100) and
                           B = floor(A / 4) and
                           C = 2 - A + B and
                           E = floor(365.25 * (Y + 4716)) and
                           F = floor(30.6001 * (M + 1)) and
                           JDN = C + D + E + F - 1524."
:constraint (date ?d)
:body (and (has-monthOfYear ?d ?d-monthOfYear)
           (has-dayOfMonth ?d ?d-dayOfMonth)
           (has-year ?d ?d-year)
           (or (=> (< ?d-monthOfYear 3)
                   (and (= (- ?x-year 1) ?y)
                        (= (+ ?d-monthOfYear 12) ?m)))
               (=> (> ?d-monthOfYear 2)
                   (and (= ?x-year ?y)
                        (= ?d-monthOfYear ?m))))
           (= (floor (/ ?y 100))?a)
           (= (floor (/ ?a 4)) ?b)
           (= (- 2 (+ ?a ?b)) ?c)
           (= (floor (* 365.25 (+ ?y 4716))) ?e)
           (= (floor (* 30.6001 (+ ?m 1))) ?f)
           (- (+ ?c ?d-dayOfMonth ?e ?f) 1524)))


;;; I am not sure if the following would be accepted by OCML?!
;
;(def-function daysBetween (?instant1 ?instant2) -> ?range
;:constraint (and (instant ?instant1)
;                 (instant ?instant2)
;                 (integer ?range)))
;
(def-function daysBetween (?x ?y) -> ?range
:documentation "daysBetween(D1, D2) hasvalue      X <-
                        D1 memberof date and D2 memberof date and
                        X = julianDayNumber(D1) - julianDayNumber(D2).
           OR
                daysBetween(D1, D2) hasvalue X <-
                        D1 memberof dateAndTime and D2 memberof dateAndTime and
                        X = daysBetween(D1.date, D2.date)."

:constraint (or (and (date ?x)
                     (date ?y))
                (and (dateAndTime ?x)
                     (dateAndTime ?y)))

:body (or (=> (and (date ?x)
                   (date ?y))
              (and (julianDayNumber ?x ?x-julianDayNumber)
                   (julianDayNumber ?y ?y-julianDayNumber)
                   (- ?x-julianDayNumber ?y-julianDayNumber)))
          (=> (and (dateAndTime ?x)
                   (dateAndTime ?y))
              (and
                   (has-date ?x ?x-date)
                   (has-date ?y ?y-date)
                   (daysBetween ?x-date ?y-date)))))


;;; I am not sure if the following would be accepted by OCML?!
;
;(def-function secondsBetween (?instant1 ?instant2) -> ?range
;:constraint (and (instant ?instant1)
;                 (instant ?instant2)
;                 (integer ?range)))
;
(def-function secondsBetween (?x ?y) -> ?i
:documentation "secondsBetween(T1, T2) hasvalue X <-
                         T1 memberof time and T2 memberof time and
                         X = secondsFromMidnight(T1) - secondsFromMidnight(T2).
        OR
                secondsBetween(D1, D2) = X <-
                         D1 memberof dateAndTime and D2 memberof dateAndTime and
                         X = secondsFromMidnight(D1.time) + julianDayNumber(D1.date) * 24 * 60 * 60 -
                                     (secondsFromMidnight(D2.time) + julianDayNumber(D2.date) * 24 * 60 * 60)."
:constraint (or (and (time ?x)
                     (time ?y))
                (and (dateAndTime ?x)
                     (dateAndTime ?y)))
:body (or (=> (and (time ?x)
                   (time ?y)) 
              (- (secondsFromMidnight ?x) (secondsFromMidnight ?y)))
          (=> (and (dateAndTime ?x)
                   (dateAndTime ?y))
              (- (+ (secondsFromMidnight (has-time ?x))
                    (* (julianDayNumber (has-date ?x)) 86400))
                 (+ (secondsFromMidnight (has-time ?y))
                    (* (julianDayNumber (has-date ?y)) 86400))))))


(def-function secondsFromMidnight(?time) -> ?i
:documentation "secondsFromMidnight(T) hasvalue X <-
                          T memberof time and
                          X = T.secondOfMinute + (T.minuteOfHour*60) + (T.hourOfDay*60*60)."
:constraint (time ?time)
:body (+ (has-secondOfMinute ?time) (* (has-minuteOfHour ?time) 60) (* (has-hourOfDay ?time) 3600)))

(def-function floor(?x) -> ?i
:documentation "returns the integer part of a number"
:body ())

(def-function modulo(?x ?y) -> ?reminder
:documentation "returns the reminder of dividing ?x to ?y"
:body ())

(def-relation equals-to (?x ?y)
:documentation    "X = Y <-
                            Y memberof date and X memberof date and
                            X.dayOfMonth = Y.dayOfMonth and
                            X.monthOfYear = Y.monthOfYear and
                            X.year = Y.year.
       OR
                   X = Y <-
                            X memberof time and Y memberof time and
                            X.secondOfMinute = Y.secondOfMinute and
                            X.minuteOfHour = Y.minuteOfHour and 
                            X.hourOfDay = Y.hourOfDay.
       OR
                   X = Y <-
                            X memberof dateAndTime and Y memberof dateAndTime and
                            X.date = Y.date and
                            X.time = Y.time."

:sufficient (or (and (date ?x)
                     (date ?y)
                     (has-monthOfYear ?x ?x-monthOfYear)
                     (has-monthOfYear ?y ?y-monthOfYear)
                     (has-dayOfMonth ?x ?x-dayOfMonth)
                     (has-dayOfMonth ?y ?y-dayOfMonth)
                     (has-year ?x ?x-year)
                     (has-year ?y ?y-year)
                     (= ?x-monthOfYear ?y-monthOfYear)
                     (= ?x-dayOfMonth ?y-dayOfMonth)
                     (= ?x-year ?y-year))
                (and (time ?x)
                     (time ?y)
                     (has-secondOfMinute ?x ?x-secondOfMinute)
                     (has-secondOfMinute ?y ?y-secondOfMinute)
                     (has-minuteOfHour ?x ?x-minuteOfHour)
                     (has-minuteOfHour ?y ?y-minuteOfHour)
                     (has-hourOfDay ?x ?x-hourOfDay)
                     (has-hourOfDay ?y ?y-hourOfDay)
                     (= ?x-secondOfMinute ?y-secondOfMinute)
                     (= ?x-minuteOfHour ?y-minuteOfHour)
                     (= ?x-hourOfDay ?y-hourOfDay))
                (and (dateAndTime ?x)
                     (dateAndTime ?y)
                     (has-date ?x ?x-date)
                     (has-date ?y ?y-date)
                     (has-time ?x ?x-time)
                     (has-time ?y ?y-time)
                     (equals-to ?x-date ?y-date)
                     (equals-to ?x-time ?y-time))))


(def-relation before (?x ?y)
:documentation    "X < Y <-
                           Y memberof date and X memberof date and
                           ((X.dayOfMonth = Y.dayOfMonth and X.monthOfYear = Y.monthOfYear and X.year = Y.year) or
                           (X.monthOfYear < Y.monthOfYear and X.year = Y.year) or
                           (X.year < Y.year)).
      OR
                   X < Y <-
                           X memberof time and Y memberof time and
                           ((X.secondOfMinute < Y.secondOfMinute and X.minuteOfHour = Y.minuteOfHour and X.hourOfDay = Y.hourOfDay) or
                           (X.minuteOfHour < Y.minuteOfHour and X.hourOfDay = Y.hourOfDay) or
                           (X.hourOfDay < Y.hourOfDay)).
      OR
                   X < Y <-
                           X memberof dateAndTime and Y memberof dateAndTime and
                           ((X.date = Y.date and X.time < Y.time) or
                           X.date < Y.date)."

:sufficient (or (and (date ?x)
                     (date ?y)
                     (or (< (has-Year ?x) (has-Year ?y))
                         (and (= (has-Year ?x) (has-Year ?y))
                              (= (has-monthOfYear ?x) (has-monthOfYear ?y))
                              (< (has-dayOfMonth ?x) (has-dayOfMonth ?y)))
                         (and (= (has-Year ?x) (has-Year ?y))
                              (< (has-monthOfYear ?x) (has-monthOfYear ?y)))))
                (and (time ?x)
                     (time ?y)
                     (or (< (role-value ?x has-hourOfDay) (has-hourOfDay ?y))
                         (and (= (has-hourOfDay ?x) (has-hourOfDay ?y))
                              (= (has-minuteOfHour ?x) (has-minuteOfHour ?y))
                              (< (has-secondOfMinute ?x) (has-secondOfMinute ?y)))
                         (and (= (has-Year ?x) (has-Year ?y))
                              (< (has-minuteOfHour ?x) (has-minuteOfHour ?y)))))
                (and (dateAndTime ?x)
                     (dateAndTime ?y)
                     (or (< (has-date ?x) (has-date ?y))
                         (and (= (has-date ?x) (has-date ?y))
                              (< (has-time ?x) (has-time ?y)))))))


(def-relation after (?x ?y)
:documentation    "X > Y <- Y < X "
:sufficient (before ?y ?x))




(def-relation contains (?x ?y)
:documentation "contains(X, Y) <-
                        X memberof interval and Y memberof interval and
                        (X.start < Y.start or X.start = Y.start) and
                        (X.end > Y.end or X.end = Y.end).
      OR
                contains(X, Y) <-
                        X memberof interval and Y memberof instant and
                        (X.start < Y or X.start = Y) and
                        (X.end > Y or X.end = Y)."

:constraint (and (interval ?x)
                 (or (instant ?y)
                     (interval ?y)))

:sufficient (or (and (interval ?x)
                     (interval ?y)
                     (or (before (has-start ?x)(has-start ?y))
                         (equals-to (has-start ?x)(has-start ?y)))
                     (or (after (has-end ?x)(has-end ?y))
                         (equals-to (has-end ?x)(has-end ?y))))
                (and (interval ?x)
                     (instant ?y)
                     (or (before (has-start ?x)?y)
                         (equals-to (has-start ?x)?y))
                     (or (after (has-end ?x)?y)
                         (equals-to (has-end ?x)?y)))))
      

