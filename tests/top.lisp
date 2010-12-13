;;; Copyright © 2008 The Open University

(in-package #:irs.tests)

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
  (webonto:setup-library))

(defun run-all-tests ()
  (format t "~%")
  (dolist (suite '(api-rest-suite
		   #+:irs-use-lispweb
                   browser-interface-suite
		   #+:irs-lispworks
                   cs-invocation
                   grounding-suite
		   #+:irs-lispworks
                   monitoring-suite
                   trusted-travel-suite))
    (format t "Running ~A: " (symbol-name suite))
    (run! suite)))
