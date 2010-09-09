(in-package #:travel)

(defmethod irs:initialise-application ((app (eql :travel)))
  (webonto:require-ontologies '(:european-train-travel-application))
  (start-web-interface))

(defmethod irs:start-application ((app (eql :travel))))

(defun start-web-interface ()
  (web:register-plugin
   :travel :application
   "A demonstration application"
   (nconc
    (mapcar (lambda (args)
	      (apply #'hunchentoot:create-regex-dispatcher args))
	    `(("/travel$" draw-top-page)
              ("/travel/get-timetable$" get-timetable-service))))))

(defun draw-top-page ()
  (web:standard-page "Travel: A demonstration of semantic web services"
    (:p "Using REST services hosted in the IRS iteself, and the IRS's
    ontological groundings.")

    (:h2 "Services")

    (:p "The example web services are
    normal " ((:a :href "http://en.wikipedia.org/wiki/Representational_State_Transfer") "RESTful") "
    services, hosted in the IRS using
    the " (:a :href "http://weitz.de/hunchentoot/" "Hunchentoot") "
    web server.  Doing an HTTP GET operation on a suitable URL
    like " (:a :href "/travel/get-timetable?depart=HAMBURG&arrive=berlin&day=1&month=2&year=2009"
               "this one")
        " will return a timetable.")))

(defun get-timetable-service ()
  (web:with-parameters (:get (depart arrive day month year))
    (let* ((dep (intern (string-upcase depart) :ocml))
           (arr (intern (string-upcase arrive) :ocml))
           (date (mapcar #'parse-integer (list day month year)))
           (table  (cl-user::get-train-times dep arr date)))
      (web::write-http-stream "text/plain" (format nil "~A" table)))))
