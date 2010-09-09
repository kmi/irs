;;; Copyright © 2008 The Open University

(in-package #:irs.applications.math)

(defmethod initialise-application ((application (eql :math)))
  (pushnew "irs:apps;math;ontologies;" ocml:*ontology-path*)
  (webonto:require-ontologies '(:math-ontology))
  (start-web-interface))

(defmethod start-application ((application (eql :math))))

(defun start-web-interface ()
  (web:register-plugin
   :math :application
   "A demonstration application"
   (nconc
    (mapcar (lambda (args)
	      (apply #'hunchentoot:create-regex-dispatcher args))
	    `(("/math$" draw-top-page)
              ("/math/add$" addition-service))))))

(defun draw-top-page ()
  (web:standard-page "Math: A demonstration of semantic web services"
    (:p "Using REST services hosted in the IRS iteself, and the IRS's
    ontological groundings.")

    (:h2 "REST")

    (:p "The example web services are
    normal " ((:a :href "http://en.wikipedia.org/wiki/Representational_State_Transfer") "REST") "
    services, hosted in the IRS using
    the " (:a :href "http://weitz.de/hunchentoot/" "Hunchentoot") "
    web server.  The services themselves are very simple.  Doing an
    HTTP GET operation on the URL "
    ((:a :href "math/add?a=1&b=1")
     (:tt "http://an.irs:8080/math/add?a=1&b=1"))
    " will return the value "
    ((:a :href "http://en.wikipedia.org/wiki/Russell-Whitehead#Quotations")
     (:tt "2")) ".")))

(defun addition-service ()
  (web:with-parameters (:get (a b))
    (let ((an (read-from-string a))
          (bn (read-from-string b)))
      (web::write-http-stream "text/plain"
                              (format nil "~A" (+ an bn))))))
