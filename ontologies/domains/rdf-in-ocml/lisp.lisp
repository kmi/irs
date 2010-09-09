;;; Copyright © 2009 The Open University

(in-package #:ocml)

(in-ontology rdf-in-ocml)

(defun #_load-rdf (store url &key (format :xml))
  (let ((wilbur:*db* store))
    (case format
      ((:xml)
	 (wilbur:db-load store url
			 :parser-class 'wilbur:rdf-parser))
      ((:rdf)
	 (wilbur:db-load store url
			 :parser-class 'porky.io.turtle:turtle-parser)))))
