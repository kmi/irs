;;; Copyright © 2008 The Open University

(in-package #:irs.tests)

(defvar *suites* '(api-rest-suite
		   #+:irs-use-lispweb
                   browser-interface-suite
		   #+:irs-lispworks
                   cs-invocation
                   grounding-suite
		   #+:irs-lispworks
                   monitoring-suite
                   trusted-travel-suite))

(defun setup ()
  (pushnew "irs:tests;ontologies;" ocml:*ontology-path*)
  (webonto:require-ontologies '(:dino :irs-tests))
  #+:irs-lispworks
  (irs:use-application :cs-invocation)
  (irs:use-application :trusted-travel)
  (irs:use-application :math)
  ;; Required for the namespace specific tests in the api-rest suite.
  #+:irs-lispworks
  (webonto:require-ontologies '(:international-system-units
                                :execution-history))
  (webonto:setup-library)

  ;; If account details for the NIH services are available, run the
  ;; tests.
  (when (probe-file (concatenate 'string cl-user::*irs-home*
				 "/apps/nih/ontologies/nih-application/secrets.lisp"))
    (irs:use-application :nih)
    (require :nih.tests)
    (setf *suites* (append *suites*
			   (list (intern "NIH-SUITE" :IRS.APPLICATIONS.NIH.TESTS))))))

(defun run-all-tests ()
  (format t "~%")
  (dolist (suite *suites*)
    (format t "Running ~A: " (symbol-name suite))
    (run! suite)))
