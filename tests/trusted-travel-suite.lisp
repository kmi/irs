;;; Dave Lambert, 2007

;;; Testing material for the trusted virtual travel agent application.

(in-package :irs.tests)

(def-suite trusted-travel-suite
    :description "Tests for the trusted virtual travel agent application.")

(in-suite trusted-travel-suite)

(test goal-invocation-test
  (is (string-equal
       "(
 Applicable Web Services: 
<br/>GET-TRAIN-TIMETABLE-SERVICE-T3
<br/>GET-TRAIN-TIMETABLE-SERVICE-T2
<br/>GET-TRAIN-TIMETABLE-SERVICE-T1 
<br/><br/> The WS class that matches with STEFANIA trust requirements is : GET-TRAIN-TIMETABLE-SERVICE-T2
<br/><br/>The result is: 
<br/> \"Timetable of trains from Frankfurt to Berlin on 1, January, 6
6:47
7:35
8:23
9:11
9:59
10:47
11:35
12:23
13:11
13:59
14:47
15:35
16:23
17:11
17:59
18:47
19:35
20:23
21:11
21:59
22:47\")"
       (with-output-to-string (str)
	 (ip::raw-trusted-irs-achieve-goal 'ocml::stefania
					'ocml::TRUST-heuristic-classification
					'ocml::GET-TRAIN-TIMETABLE-TRUSTED-GOAL
					'((ocml::HAS-DEPARTURE-STATION ocml::FRANKFURT)
					  (ocml::HAS-DESTINATION-STATION ocml::BERLIN)
					  (ocml::HAS-DATE-AND-TIME (1 1 6)))
					str nil t)))))
