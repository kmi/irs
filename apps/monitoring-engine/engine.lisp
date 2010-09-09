;;; Copyright © 2008 The Open University

(in-package #:monitoring-engine)

(defmethod initialise-application ((app (eql :monitoring-engine)))
  (webonto:require-ontologies
   '(:execution-history))
  (web:register-plugin
   :monitoring-engine :application
   "Monitoring engine."
   (nconc
    (mapcar (lambda (args)
	      (apply #'hunchentoot:create-regex-dispatcher args))
	    '(("/monitoring-engine$" draw-top-page))))))

(defmethod start-application ((app (eql :monitoring-engine)))
  'no-operation)

(defun draw-top-page ()
  (web:standard-page "Monitoring engine"
	(:p "This application demonstrates event monitoring in the SUPER project.")))
