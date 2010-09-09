;;; Copyright © 2009 The Open University

(in-package #:irs.applications.demo)

(defmethod initialise-application ((application (eql :demo)))
  (load #P"irs:apps;demo;loadable-ontologies")
  (start-web-interface))

(defmethod start-application ((application (eql :demo))))

(defun start-web-interface ()
  (web:register-plugin
   :demo :application
   "A demonstration application"
   (nconc
    (mapcar (lambda (args)
	      (apply #'hunchentoot:create-regex-dispatcher args))
	    `(("/demo$" draw-top-page))))))

(defun draw-top-page ()
  (web:standard-page "demo: A portmanteu demonstrator"
    (:p "The demo application loads  several semantic
  web services demonstrators, along with many ontologies to make the browser and editor more interesting.")))
