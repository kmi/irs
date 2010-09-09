;;; Copyright © 2008,2009 The Open University

(in-package #:irs.applications.nih)

(defmethod initialise-application ((application (eql :nih)))
  (pushnew "irs:apps;nih;ontologies;" ocml:*ontology-path*)
  (webonto:require-ontologies '(:nih-application))
  (start-web-interface))

(defmethod start-application ((application (eql :nih))))

(defun start-web-interface ()
  (web:register-plugin
   :nih :application
   "Web services Not Invented Here."
   (nconc
    (mapcar (lambda (args)
	      (apply #'hunchentoot:create-regex-dispatcher args))
	    `(("/nih$" draw-top-page))))))

(defun draw-top-page ()
  (web:standard-page "Web services Not Invented Here"
    (:p "This is a collection of web services created by Other People.
    That is, these services were created by people unknown to us, and
    whose minds were never darkened by the thought their work would be
    invoked as semantic services.")
    (:p "To try it out, go to the " ((:a :href "/irs/browser") "browser")
	" and try achieving goals in one of these ontologies:")
    (:ul
     (:li "Amazon S3")
     (:li "Flickr")
     (:li "Yahoo"))))
