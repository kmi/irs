;;; Copyright © 2009 The Open University

(in-package #:soa4all)

(defun start-web-interface ()
  (web:register-plugin
   :soa4all :application
   "SOA4All"
   (nconc
    (mapcar (lambda (args)
	      (apply #'hunchentoot:create-regex-dispatcher args))
	    `(("/soa4all$" draw-top-page)
              ("/soa4all/assets"
               ,(lambda () (web:send-url/file "/soa4all/assets"
                                              "irs:apps;soa4all;assets"))))))))

(defun draw-top-page ()
  (web:standard-page "SOA4All"
    (:img :src "/soa4all/assets/images/header.png" )
    (:ul (:li ((:a :href "http://www.soa4all.eu/")
	       "SOA4All project homepage"))
	 (:li ((:a :href "http://projects.kmi.open.ac.uk/soa4all/")
	       "SOA4All at the Open University")))))
