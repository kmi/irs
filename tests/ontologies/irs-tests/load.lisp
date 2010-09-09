;;; Copyright © 2008 The Open University

(in-package #:ocml)

(def-ontology irs-tests
    "Goals and services support the IRS's test suites."
  :type :goal
  :includes (http-grounding rfc2616 wsmo)
  :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/irs-tests"
  :namespaces (("tests" irs-tests)
	       ("hg" http-grounding)
	       ("rfc2616" rfc2616))
  :allowed-editors ("carlos" "john")
  :author "dave"
  :files ("new"))
