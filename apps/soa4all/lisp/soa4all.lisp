;;; Copyright © 2009 The Open University

(in-package #:irs.applications.soa4all)

(defmethod initialise-application ((application (eql :soa4all)))
  (start-web-interface))

(defmethod start-application ((application (eql :soa4all))))
