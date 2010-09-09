;;; Copyright © 2008 The Open University.

(defsystem :nih
  :description "Not Invented Here: Web services from other people"
  ;; The amazon-s3 ontology requires the :cl-base64 package,
  ;; because it needs the MIME package, which actually calls base64
  ;; functions.
  ;; XXX We should find a better way to manage this dependency.
  :depends-on (:irs :cl-base64)
  :components
  ((:module :lisp
	    :components
	    ((:file "defpackage")
	     (:file "nih" :depends-on ("defpackage"))))))

(defsystem :nih.tests
  :description "Tests for Not Invented Here application"
  :depends-on (:nih :irs-tests-core)
  :components
  ((:module :tests
	    :components
	    ((:file "defpackage")
	     (:file "nih-suite" :depends-on ("defpackage"))
	     (:file "amazon-s3" :depends-on ("nih-suite"))
	     (:file "flickr" :depends-on ("nih-suite"))))))
