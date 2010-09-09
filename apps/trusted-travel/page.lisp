(in-package #:trusted-travel)

(defmethod initialise-application ((app (eql :trusted-travel)))
  (initialise-application :travel)
  (webonto:require-ontologies
   '(:trust-heuristic-classification :trust-profile :wsto))
  (web:register-plugin
   :trusted-travel :application
   "Trusted Travel"
   (nconc
    (mapcar (lambda (args)
	      (apply #'hunchentoot:create-regex-dispatcher args))
	    `(("/trusted-travel$" draw-top-page)
	      ("/trusted-travel/trusted-query$" draw-trusted-query-page)
	      ("/trusted-travel/trusted-response$" draw-trusted-response-page)
	      ("/trusted-travel/untrusted-query$" draw-untrusted-query-page)
	      ("/trusted-travel/untrusted-response$" draw-untrusted-response-page)
	      ("/trusted-travel/images/"
	       ,(lambda ()
			(web:send-url/file "/trusted-travel/images"
					   "irs:apps;trusted-travel;web;images;")))))
    (list
     (hunchentoot:create-static-file-dispatcher-and-handler
      "/trusted-travel/stylesheet.css"
      (make-pathname :name "stylesheet" :type "css"
		     :version nil
		     :defaults "irs:apps;trusted-travel;web;")
      "text/css")))))

(defmethod start-application ((app (eql :trusted-travel)))
  (initialise-application :travel))

;;; {{{ Constants
(defvar *images-dir* "images")

(define-constant +users+
  '("dinar" "stefania" "vanessa"))

(define-constant +cities+
  '("Accra"
    "Ahrensburg"
    "Amsterdam"
    "Andorra-La-Vella"
    "Athens"
    "Belfast"
    "Belgrade"
    "Berlin"
    "Bern"
    "Birmingham"
    "Bitburg"
    "Bordeaux"
    "Bratislava"
    "Brest"
    "Brussels"
    "Bucharest"
    "Budapest"
    "Burgdorf"
    "Cardiff"
    "Cherbourg"
    "Chisinau"
    "Cologne"
    "Copenhagen"
    "Dijon"
    "Dublin"
    "Dunkerque"
    "Edinburgh"
    "Frankfurt"
    "Freiburg"
    "Graz"
    "Grenoble"
    "Gruenstadt"
    "Hamburg"
    "Hamm"
    "Hannover"
    "Helsinki"
    "Ingolstadt"
    "Innsbruck"
    "Karlsruhe"
    "Kevelaer"
    "Kiel"
    "Kiev"
    "Klagenfurt"
    "Leverkusen"
    "Lichenberg"
    "Lille"
    "Limoges"
    "Linz"
    "Lisbon"
    "Ljubljana"
    "London"
    "Luebeck"
    "Luxembourg"
    "Lyon"
    "Madrid"
    "Magdeburg"
    "Mannheim"
    "Marseille"
    "Minsk"
    "Milton-Keynes"
    "Monaco"
    "Munich"
    "Nancy"
    "Nantes"
    "Nice"
    "Nicosia"
    "Nuremberg"
    "Orleans"
    "Oslo"
    "Paris"
    "Passau"
    "Perpignan"
    "Prague"
    "Praia"
    "Rendsburg"
    "Reykjavik"
    "Riga"
    "Rome"
    "Rouen"
    "Salzburg"
    "San-Marino"
    "Sarajevo"
    "Skopje"
    "Sofia"
    "Stockholm"
    "Strasbourg"
    "Stuttgart"
    "Tallinn"
    "Tbilisi"
    "Tirane"
    "Toulon"
    "Toulouse"
    "Ulm"
    "Vaduz"
    "Valence"
    "Valletta"
    "Vatican-City"
    "Vienna"
    "Vilnius"
    "Warsaw"
    "Weisbaden"
    "Wellington"
    "Wittenberg"
    "Wolfenbuettel"
    "Yerevan"
    "Zagreb"
    ))

(define-constant +months+
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))
;;; }}}

(defun draw-top-page ()
  (web:standard-page "Trusted Travel"
	(:p "This application demonstrates the use of reasoning in "
	    ((:a :href "http://www.kmi.open.ac.uk/projects/ocml/") "OCML")
	    " to select web services appropriate to the particular user.")
	(:p "There are two variants, with and without trust, so you can compare the behaviour:")
	(:ul
	 (:li ((:a :href "/trusted-travel/trusted-query") "With trust"))
	 (:li ((:a :href "/trusted-travel/untrusted-query") "Without trust")))))

(defun draw-trusted-query-page ()
  (web:with-html-top
    (str (draw-query-page "Trusted Travel" "images/trusted-vta-logo.png"
			  "trusted-response"))))

(defun draw-untrusted-query-page ()
  (web:with-html-top
    (str (draw-query-page "Untrusted Travel" "images/untrusted-vta-logo.png"
			   "untrusted-response"))))

(defun draw-query-page (title image action)
  (multiple-value-bind (sec min hour this-day this-month this-year x y z) (get-decoded-time)
    (declare (ignore sec min hour x y z))
    (web:with-html
    (:html
     (:head (:title (str title))
	    ((:link :href "stylesheet.css" :rel "stylesheet" :type "text/css")))
     (:body 
      ((:div :align "center")
       (:img :src image
	     :width "450" :height "150"))
      ((:table :width "480" :border "0" :align "center"
	       :cellpadding "0" :cellspacing "0")
       (:tr (:td (:img :src "images/tab.png"
		       :width "480" :height "25"))))
      ((:form :method "GET" :action action)
       ((:table :width 480 :border 1 :align "center" :cellpadding 0
		:cellspacing 0 :bordercolor "7ca5ce" :bgcolor "e5edf5" )
	(:tr ((:td :align "center")
	      ((:table :width 366 :border 0 :align "center"
		       :cellpadding 0 :cellspacing 0)
	       (:tr (:td (:br) (:br))
		    (:td "&nbsp;"))
	       (:tr ((:td :valign "top" :class "vtaoptions")
		     ((:div :align "right") "User:"))
		    ((:td :valign "bottom")
		     "&nbsp;&nbsp;"
		     ((:select :name "user")
		      (dolist (user +users+)
			(fmt "<option>~A</option>" user)))))
	       (:tr ((:td :valign "bottom" :class "vtaoptions")
		     ((:div :align "right")
		      "Depart:"))
		    ((:td :valign "bottom")
		     (:br) "&nbsp;&nbsp;"
		     ((:select :name "depart")
		      (dolist (city +cities+)
			(fmt "<option>~A</option>" city)))))
	       (:tr ((:td :valign "bottom" :class "vtaoptions")
		     ((:div :align "right")
		      "Arrive:"))
		    ((:td :valign "bottom")
		     (:br) "&nbsp;&nbsp;"
		     ((:select :name "arrive")
		      (dolist (city +cities+)
			(fmt "<option>~A</option>" city)))))
	       (:tr ((:td :valign "bottom" :class "vtaoptions")
		     ((:div :align "right")
		      "Departure date:"))
		    ((:td :valign "bottom")
		     (:br) "&nbsp;&nbsp;"
		     ((:select :name "departday")
		      (loop for day from 1 to 31
			 do (fmt "<option value=\"~A\"~A>~A</option>"
				 day
				 (if (equal day this-day)
				     (format nil " selected " day)
				     "")
				 day)))
		     "&nbsp;&nbsp;"
		     ((:select :name "departmonth")
		      (loop for month from 1 to 12
			 do (fmt "<option~A>~A</option>" 
				 (if (eq month this-month) 
				     " selected=\"true\""
				     "")
				 (nth (- month 1) +months+))))
		     "&nbsp;&nbsp;"
		     ((:select :name "departyr")
		      (loop for year from this-year to (+ 5 this-year)
			 do (fmt "<option~A>~A</option>"
				 (if (eq this-year year)
				     " selected"
				     "")
				 year)))))
	       (:tr ((:td :valign "bottom" :class "vtaoptions") "&nbsp;")
		    ((:td :valign "bottom") (:br)
		     ((:input :type "image" :src "images/submit.png"
			      :alt "Submit" :name "submit" :border 0))))))))))))))

(defun draw-untrusted-response-page ()
  (web:with-html-top
    (str (draw-response-page "Untrusted Travel" #'untrusted-buy-train-ticket))))

(defun draw-trusted-response-page ()
  (web:with-html-top
    (str (draw-response-page "Trusted Travel" #'trusted-buy-train-ticket))))

(defun draw-response-page (title func)
  (web:with-parameters (:get (user depart arrive departday departmonth departyr))
    (ocml:with-ontology ('ocml::trust-heuristic-classification)
      (let ((depart (http::make-ocml-symbol depart))
	    (user (http::make-ocml-symbol user))
	    (arrive (http::make-ocml-symbol arrive)))
	(web:with-html
	  (:html
	   (:head
	    (:title (str title))
	    ((:link :href "stylesheet.css" :rel "stylesheet"
		    :type "text/css")))
	   (:body
	    (:center (:h1 (str title)))
	    (:center ((:table :border 0 :width 750 )
		      (:tr (:td (str (get-city-flag depart)))
			   (:td (str (get-train-icon)))
			   (:td (str (get-city-flag arrive))))))
	    (fmt "<pre>~a</pre>"
		 (funcall func user depart arrive 
			  departday (month-name-to-number departmonth) departyr)))))))))

(defun host-country (city-or-town)
  (ocml::findany '?x
		 `(ocml::is-in-country
		   ,(http::make-ocml-symbol city-or-town) ?x)))

(defun get-city-flag (city)
  (get-country-flag (host-country city)))

(defun get-country-flag (country)
  (when country
    (web:with-html
      ((:img :src (format nil "~a/~(~a~)-flag.png" *images-dir* country))))))

(defun get-person-icon (user)
  (let ((type (web-onto::findany '?x `(and (ocml::person ,user) (ocml::instance-of ,user ?x)))))
    (when type
    (web:with-html
      ((:img :src (format nil "~a/~(~a~).png" *images-dir* type)))))))

(defun get-train-icon ()
  (web:with-html
    ((:img :src (format nil "~a/~a" *images-dir* "long-train.png")))))

(defun month-name-to-number (month-name)
  (1+ (position month-name trusted-travel::+months+ :test #'string=)))

(defun trusted-buy-train-ticket (user departure-station arrival-station day month year)
  "Inputs should be OCML instances or integers."
  (read-from-string
   (with-output-to-string (ostream)
     (ip::trusted-irs-achieve-goal user 
                                   'ocml::trust-heuristic-classification 
                         'ocml::get-train-timetable-trusted-goal 
                         `((ocml::has-departure-station ,departure-station) 
                           (ocml::has-destination-station ,arrival-station) 
                           (ocml::has-date-and-time ,(list day month year)))
                         ostream nil))))

(defun untrusted-buy-train-ticket (user departure-station arrival-station day month year)
  "Inputs should be OCML instances or integers."
  (declare (ignore user))
  (read-from-string
   (with-output-to-string (ostream)
     (ip::irs-achieve-goal 'ocml::trust-heuristic-classification 
			   'ocml::get-train-timetable-trusted-goal 
			   `((ocml::has-departure-station ,departure-station) 
			     (ocml::has-destination-station ,arrival-station) 
			     (ocml::has-date-and-time ,(list day month year)))
			   ostream nil))))

(defun send-image ()
)
