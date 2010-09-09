;;; Copyright © 2008 The Open University

(in-package #:irs.tests)

(defun setup ()
  (pushnew "irs:tests;ontologies;" ocml:*ontology-path*)
  (webonto:require-ontologies '(:dino :irs-tests))
  (irs:use-application :cs-invocation)
  (irs:use-application :trusted-travel)
  (irs:use-application :math)
  ;; Required for the namespace specific tests in the api-rest suite.
  (webonto:require-ontologies '(:international-system-units
                                :execution-history))
  (webonto:setup-library))

(defun run-all-tests ()
  (format t "~%")
  (dolist (suite '(api-rest-suite
		   #+:irs-use-lispweb
                   browser-interface-suite
                   cs-invocation
                   grounding-suite
                   monitoring-suite
                   trusted-travel-suite))
    (format t "Running ~A: " (symbol-name suite))
    (run! suite)))
