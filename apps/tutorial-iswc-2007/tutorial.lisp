(defpackage :tutorial-iswc-2007
  (:use :common-lisp :cl-who :irs))

(in-package #:tutorial-iswc-2007)

(defmethod initialise-application ((app (eql :tutorial-iswc-2007)))
  (webonto:require-ontologies '(:goal-get-license
                                :ws-get-license
                                :wg-get-license
                                :goal-get-content-url
                                :ws-get-content-url
                                :wg-get-content-url
                                :content-on-demand-ontology
                                :bpmo12)))

(defmethod start-application ((app (eql :tutorial-iswc-2007)))
  (web:register-plugin
   :tutorial-iswc-2007 :application
   "ISWC 2007 Semantic Business Process Modelling Tutorial (Server)"
   (mapcar (lambda (args)
             (apply #'hunchentoot:create-regex-dispatcher args))
           '(("/tutorial-iswc-2007$" draw-top-page)))))

(defun draw-top-page ()
  (web:standard-page "ISWC 2007 Semantic Business Process Modelling Tutorial"
	(:p "This application is a server for use in tutorials demonstrating
	the creation of semantic business process models. ")))
