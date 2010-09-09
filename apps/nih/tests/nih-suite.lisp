;;; Copyright © 2007, 2008 The Open University

;;;; Unit tests

(in-package #:irs.applications.nih.tests)

'(eval-when (:compile-toplevel :execute :load-toplevel)
  (ocml:register-namespace
   "lhdlA" "http://kmi.open.ac.uk/projects/lhdl/ns/application#")
  (ocml:register-namespace
   "lhdlD" "http://kmi.open.ac.uk/projects/lhdl/ns/domain#")
  (ocml:register-namespace
   "lhdlG" "http://kmi.open.ac.uk/projects/lhdl/ns/goals#"))

(def-suite nih-suite
  :description "All tests for the NIH application.")

(def-suite flickr-suite
  :description "All tests for the Flickr services."
  :in nih-suite)

(defparameter *random-string*
  (let* ((length (+ 1 (random 128)))
         (string (make-string length)))
    (dotimes (i length)
      (setf (elt string i) (code-char (+ 65 (random 26)))))
    string))
