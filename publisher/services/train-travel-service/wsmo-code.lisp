(in-package user)

(defvar *country-train-time-intervals*
  '((england 49 55) (pakistan 1 48) (northern-ireland 56 40) (gambia 46 16) (swaziland 56 46) (cambodia 27 19) (latvia 6 16) (oman 56 22) (gabon 13 28) (suriname 33 2) (wales 19 24) (burundi 6 9) (laos 18 25) (norway 15 48) (france 0 18) (sudan 3 57) (scotland 0 34) (burma 59 31) (kyrgyzstan 24 42) (nigeria 49 11) (finland 33 55) (sri-lanka 10 38) (zimbabwe 58 55) (burkina-faso 20 41) (kuwait 58 41) (niger 57 38) (fiji 20 41) (spain 44 55) (zambia 35 45) (bulgaria 27 4) (south-korea 26 5) (nicaragua 7 19) (ethiopia 22 27) (south-africa 44 14) (serbia-and-montenegro 49 40) (brunei 54 24) (north-korea 12 57) (new-zealand 55 44) (estonia 45 53) (somalia 13 0) (yemen 0 15) (brazil 4 44) (kiribati 8 2) (netherlands 19 54) (eritrea 26 13) (solomon-islands 38 29) (western-samoa 29 3) (botswana 57 7) (kenya 7 31) (nepal 42 40) (equatorial-guinea 11 54) (slovenia 56 41) (vietnam 10 40) (bosnia-and-herzegovina 4 47) (kazakstan 46 12) (nauru 29 9) (united-kingdom 38 40) (el-salvador 55 32) (slovakia 10 14) (venezuela 27 34) (jordan 13 56) (vatican-city 27 51) (bolivia 46 18) (namibia 53 7) (egypt 33 48) (singapore 32 37) (japan 25 27) (bhutan 9 51) (vanuatu 44 39) (mozambique 38 10) (ecuador 31 21) (sierra-leone 58 26) (jamaica 32 56) (benin 20 47) (uzbekistan 13 0) (morocco 12 4) (dominican-republic 30 10) (seychelles 48 17) (italy 21 48) (belize 1 22) (uruguay 43 25) (mongolia 56 43) (dominica 50 7) (senegal 2 12) (monaco 51 50) (israel 57 27) (belgium 27 46) (djibouti 15 24) (united-states 37 37) (saudi-arabia 3 39) (moldova 41 6) (ireland 53 31) (belarus 57 31) (denmark 8 15) (micronesia 16 53) (sao-tome-and-principe 54 30) (united-arab-emirates 28 7) (iraq 58 46) (barbados 44 41) (czech-republic 56 31) (san-marino 57 48) (mexico 52 39) (iran 18 57) (ukraine 59 5) (bangladesh 17 25) (cyprus 36 4) (saint-vincent-and-the-grenadines 52 16) (mauritius 11 51) (indonesia 1 41) (uganda 36 49) (bahrain 41 8) (cuba 20 47) (mauritania 45 19) (saint-lucia 4 2) (india 10 23) (tuvalu 42 54) (bahamas 26 13) (croatia 43 56) (marshall-islands 9 40) (saint-kitts-and-nevis 48 30) (iceland 49 49) (turkmenistan 3 34) (azerbaijan 2 48) (ivory-coast 13 50) (malta 15 50) (rwanda 44 49) (hungary 19 11) (austria 49 14) (turkey 5 20) (costa-rica 14 4) (mali 39 51) (russian-federation 45 12) (honduras 25 5) (australia 24 16) (tunisia 59 4) (maldives 49 56) (democratic-republic-of-congo 24 33) (romania 30 25) (armenia 31 34) (haiti 57 22) (trinidad-and-tobago 24 14) (malaysia 14 0) (congo 33 39) (qatar 36 38) (guyana 48 20) (argentina 25 39) (tonga 50 19) (malawi 43 13) (comoros 36 4) (portugal 27 19) (guinea-bissau 47 45) (antigua-and-barbuda 37 12) (togo 58 3) (madagascar 17 42) (colombia 28 50) (poland 41 6) (guinea 34 27) (angola 16 41) (thailand 2 53) (macedonia 36 39) (china 26 43) (luxembourg 18 51) (philippines 34 31) (guatemala 34 29) (tanzania 31 42) (andorra 13 0) (chile 45 1) (lithuania 28 46) (peru 39 14) (grenada 12 41) (tajikistan 29 12) (algeria 25 46) (chad 6 55) (paraguay 41 51) (liechtenstein 12 47) (greece 3 33) (taiwan 7 32) (albania 47 30) (central-african-republic 38 40) (libya 44 40) (papua-new-guinea 31 54) (ghana 7 19) (syria 53 31) (afghanistan 18 31) (cape-verde 9 20) (liberia 53 5) (panama 21 42) (germany 59 48) (switzerland 48 38) (canada 4 7) (lesotho 14 11) (palau 11 11) (georgia 7 7) (sweden 2 52) (cameroon 59 4) (lebanon 10 23)))

(defun select-travel-kb ()
  (ocml::select-ontology 'ocml::european-train-travel-application))

(defun in-region-p (city-or-town region)
  (or (ocml::holds? 'ocml::is-in-geographical-region (make-ocml-symbol city-or-town)
                    (make-ocml-symbol region))
      (progn (ocml::select-ontology 'ocml::austrian-cities-kb)
        (let ((result (ocml::holds? 'ocml::has-country-location (make-ocml-symbol city-or-town)
                                    (make-ocml-symbol region))))
          (select-travel-kb)
          result))))

(defun host-country (city-or-town)
  (ocml::findany '?x `(ocml::is-in-country ,(make-ocml-symbol city-or-town) ?x)))

(defun person-name (x)
  (or (web-onto::findany '?x `(ocml::has-name ,x ?x)) x))

(defun in-germany-p (city)
  (in-region-p city 'germany))


(defun gold-card-customer (person)
  (ocml::holds? 'ocml::gold-card-customer (make-ocml-symbol person) :true))

(defun acknowledge-error-message (error-acknowledgement-number)
  (cond ((zerop error-acknowledgement-number)
         "Date format error occurred")
        ((= error-acknowledgement-number 1)
         "Problem with departure station")
        ((= error-acknowledgement-number 2)
         "Problem with destination station")
        ((= error-acknowledgement-number 3)
         "Problem with passenger")))

(defun book-first-class-upgrade-german-train-journey (person departure destination universal-time)
  (book-region-specific-train-journey 'germany person departure destination universal-time
                                      30 'universal
                                      "First Class Booking German Rail (Die Bahn)"))

(defun book-standard-class-german-train-journey (person departure destination universal-time)
  (book-region-specific-train-journey 
   'germany person departure destination universal-time
   30 'universal
   "German Rail (Die Bahn)"))

(defun book-german-train-journey (person departure destination universal-time)
  ;;(setf ll (list person departure destination universal-time)))
  (select-travel-kb)
  (cond ((null universal-time)
         "Error: DATE-FORMAT-ERROR The time value is empty.")
        ((gold-card-customer person)
         "GOLD-CARD-UPGRADE")
        (t (book-region-specific-train-journey 'germany person departure destination universal-time
                                               30 'universal
                                               "German Rail (Die Bahn)"))))

;;N.B. cut-down version for DIP demo

(defun book-german-train (origin destination universal-time)
  (book-region-specific-train-journey 'germany 'john origin destination universal-time 
                                      30 'universal "German Rail (Die Bahn)"))

(defun book-austrian-train-journey (person departure destination universal-time)
  ;;(setf l (list person departure destination universal-time))
  (book-region-specific-train-journey 'austria person departure destination universal-time
                                       35 'universal
                                       "Austrian Rail (OBB)"))

(defun book-french-train-journey (person departure destination universal-time)
  ;;(setf l (list person departure destination universal-time))
  (book-region-specific-train-journey 'france person departure destination universal-time
                                       25 'list
                                       "SNCF"))

(defun book-english-train-journey (person departure destination universal-time)
  ;;(setf l (list person departure destination universal-time))
  (book-region-specific-train-journey 'england person departure destination universal-time
                                       150 'list
                                       "British Rail"))

(defun book-student-european-train-journey (person departure destination universal-time)
  (book-region-specific-train-journey 'europe person departure destination universal-time
                                       80 'list
                                       "European Student Rail Travel"))

(defun book-business-european-train-journey (person departure destination universal-time)
  (book-region-specific-train-journey 'europe person departure destination universal-time
                                       300 'list
                                       "Business Europe"))

(defun book-region-specific-train-journey (region person departure destination 
                                                    date-and-time price
                                                    time-type name)
  ;;(setf dd departure de destination)
  (select-travel-kb)
  (cond ((and (in-region-p departure region) (in-region-p destination region))
         (book-train-journey person departure destination price date-and-time time-type
                             name))
        ((in-region-p departure region)
         (format nil "\"Sorry ~a is not in ~:(~a~).\"" destination region))
        ((in-region-p destination region)
         (format nil "\"Sorry ~a is not in ~:(~a~).\"" departure region))
        (t (format nil "\"Sorry but neither ~a or ~a are in ~:(~a~).\"" 
                   departure destination region))))

(defun book-train-journey (person departure destination price date-and-time time-type
                                  name)
  (if (eq time-type 'universal)
      (book-universal-time-train-journey person departure destination date-and-time price
                                         name)
    (book-list-time-and-date-train-journey person departure destination date-and-time price
                                           name)))
    
(defvar *months*
  '(january february march april may june july august september october november december))

(defun get-month-name (num)
  (elt *months* (1- num)))

(defun universal-time-to-list (x)
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time x)
    (list seconds minutes hours date month year)))

(defun list-to-universal-time (x)
  (apply #'encode-universal-time x))

#|
(defun mediate-time (x)
  (if (listp (car (last x)))
      (append (butlast x) (list (list-to-universal-time (car (last x)))))
    x))
|#

(defun mediate-time (x)
  ;;(push x xs)
  (list-to-universal-time x))

;;our wsmx client currently sends back string
;;lisp universal time starts in 1900 and java universal time starts in 1970
(defun decode-wsmx-universal-time (universal-time)
  ;;(setf uu universal-time)
  (setf universal-time (read-from-string universal-time))
  (let ((java-time-start (- (encode-universal-time 0 0 0 1 1 1970)
                            (encode-universal-time 0 0 10 1 1 1900))))
    (incf universal-time java-time-start)
    (multiple-value-bind (seconds minutes hours date month year)
        (decode-universal-time universal-time)
      (values seconds minutes hours date month year))))

(defun book-universal-time-train-journey (person departure destination universal-time price
                                                 name)
  (multiple-value-bind (seconds minutes hours date month year)
      (if (stringp universal-time)
          ;;if its a string assume its coming from wsmx - a hack
          (decode-wsmx-universal-time universal-time)
        (decode-universal-time universal-time))
    (book-list-time-and-date-train-journey person departure destination 
                                            (list seconds minutes hours date month year)
                                            price name)))

(defun book-list-time-and-date-train-journey (person departure destination list-time-and-date
                                                     price name)
  (if (and (listp list-time-and-date)
           (= (length list-time-and-date) 6))
  (destructuring-bind (seconds minutes hours date month year) list-time-and-date
    (let ((train-time (first-train-after departure hours minutes seconds)))
      (format nil "\"~a: ~a is booked on the ~d going from ~a to ~a at ~d:~d, ~d, ~a ~d. The price is ~d Euros.\""
              name (person-name person) (random 1000) departure destination
              (car train-time) (second train-time)
              date (get-month-name month) year 
              (+ (random (floor (/ price 5))) price))))
    (format nil "Error: date-format-error Time and date need to be a list with 6 elements: seconds, minutes, hours, date, month and year. You sent ~a" 
            list-time-and-date)))
  


(defun region-specific-timetable-enquiry (region departure destination date)
  (select-travel-kb)
  (cond ((and (in-region-p departure region) (in-region-p destination region))
         (get-train-times departure destination date))
        ((in-region-p departure region)
         (format nil "\"Sorry ~a is not in ~:(~a~).\"" destination region))
        ((in-region-p destination region)
         (format nil "\"Sorry ~a is not in ~:(~a~).\"" departure region))
        (t (format nil "\"Sorry but neither ~a or ~a are in ~:(~a~).\"" 
                   departure destination region))))

(defun get-country-start-and-interval (x)
  (let ((result (cdr (assoc (intern (symbol-name x) (find-package "CL-USER"))
                            *country-train-time-intervals*))))
    (if result
        result
      (list 20 20))))

(defun get-train-times (departure destination date)
  (select-travel-kb)
  (let ((departure-country (host-country departure))
        (destination-country (host-country destination)))
    (if (and departure-country destination-country (not (eq departure-country :nothing))
             (not (eq destination-country :nothing)))
        (if (eq departure-country destination-country)
            (destructuring-bind (start interval)
                (get-country-start-and-interval departure-country)
              (format nil "\"Timetable of trains from ~:(~a~) to ~:(~a~) on ~d, ~:(~a~), ~d~{~%~{~d:~d~}~}\""
                      departure destination (car date) (get-month-name (second date)) (third date)
                      (internal-generate-timetable start interval)))
          (format nil "\"~:(~a~) is in ~:(~a~) and ~:(~a~) is in ~:(~a~). Both departure and destination stations should be in the same country\"" 
                  departure departure-country destination destination-country))
      (if (and departure-country (not (eq departure-country :nothing)))
          (format nil "\"Sorry couldn't find the host country of ~:(~a~)\"" destination)
        (if (and destination-country (not (eq destination-country :nothing)))
            (format nil "\"Sorry couldn't find the host country of ~:(~a~)\"" departure)
          (format nil "\"Sorry couldn't find the host country of either ~:(~a~) or ~:(~a~)\"" departure destination))))))

(defvar *number-of-times* 20)

(defvar *train-start-time* 5)

(defun generate-timetable (departure)
  (let ((host-country (host-country departure)))
    (when host-country
      (destructuring-bind (start interval)
          (get-country-start-and-interval host-country)
        (internal-generate-timetable start interval)))))

(defun first-train-after (departure hour minute second)
  (let ((time-table (generate-timetable departure)))
    (dolist (x time-table)
      (when (or (> (car x) hour)
                (and (= (car x) hour)
                     (or (> (second x) minute)
                         (and (zerop second)
                              (= minute (second x))))))
        (return-from first-train-after x)))))


(defun internal-generate-timetable (start interval)
  (let ((result nil))
    (do ((i 1 (1+ i)))
        ((> (+ *train-start-time*
               (floor (/ (+ start (* (1+ i) interval)) 60)))
            23))
      (push (gen-train-time start interval i) result))
    (reverse result)))

(defun gen-train-time (start interval i)
  (list (+ *train-start-time* (floor (/ (+ start (* i interval)) 60)))
        (mod (+ start (* i interval)) 60)))


#|

(get-train-times 'paris 'london '(18 4 2004))

(book-german-train-journey 'christoph 'berlin 'frankfurt (encode-universal-time 3 7 14 24 9 2004))

(book-austrian-train-journey 'sinuhe 'vienna 'innsbruck (encode-universal-time 9 30 17 20 9 2004))

(book-english-train-journey 'christoph 'milton-keynes 'london '(20 33 25 15 9 2004))

(book-french-train-journey 'sinuhe 'paris 'lyon '(3 4 6 18 8 2004))

(book-student-european-train-journey 'john 'london 'nice '(3 4 6 18 8 2004))

(book-business-european-train-journey 'liliana 'paris 'innsbruck '(3 4 6 18 8 2004))


|#
