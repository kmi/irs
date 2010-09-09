;;; Copyright © 2008 The Open University

;;; This is compatibility machinery for XSD types.

(in-package #:ocml)

(in-ontology xsd-types)

(def-class #_DateTime ()
  "The class of ‘dateTime’ datatype."
  :lisp-fun #'(lambda (dt env)
                (if (utilities:xsd-datetime-p (instantiate dt env))
                    (list env)
                    :fail)))

(def-class #_Date ()
  "The class of ‘date’ datatype."
  :lisp-fun #'(lambda (dt env)
                (if (utilities:xsd-date-p (instantiate dt env))
                    (list env)
                    :fail)))

(def-class #_Time ()
  "The class of ‘time’ datatype."
  :lisp-fun #'(lambda (dt env)
                (if (utilities:xsd-time-p (instantiate dt env))
                    (list env)
                    :fail)))
