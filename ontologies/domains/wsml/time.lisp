;;; Copyright © 2008 The Open University

(in-package #:ocml)

(in-ontology wsml)

(def-class #_DateTime ()
  "The class of XSD ‘dateTime’ datatype."
  :lisp-fun #'(lambda (dt env)
                (if (xsd-datetime-p (instantiate dt env))
                    (list env)
                    :fail)))

(def-class #_Date ()
  "The class of XSD ‘date’ datatype."
  :lisp-fun #'(lambda (dt env)
                (if (xsd-date-p (instantiate dt env))
                    (list env)
                    :fail)))

(def-class #_Time ()
  "The class of XSD ‘time’ datatype."
  :lisp-fun #'(lambda (dt env)
                (if (xsd-time-p (instantiate dt env))
                    (list env)
                    :fail)))
