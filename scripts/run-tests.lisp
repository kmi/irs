;;; Copyright © 2007,2008 The Open University

(load "scripts/irs")

(require :irs)

(require :irs-tests)

(irs:start)

(tests:setup)

(tests:run-all-tests)

(quit)
