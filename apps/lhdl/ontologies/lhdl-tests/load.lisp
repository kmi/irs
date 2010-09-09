;;; Copyright © 2008 The Open University

(in-package #:ocml)

(def-ontology lhdl-tests
    "Definitions for the LHDL test suite."
  :type :goal
  :allowed-editors ("john")
  :author "dave"
  :files ("test")
  :includes (lhdl-goals))
