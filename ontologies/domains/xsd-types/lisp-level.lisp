;;; Copyright © 2008 The Open University

(in-package #:ocml)

(in-ontology xsd-types)

(defun xsd-date (year month day)
  (let ((seconds (utilities:lisp-to-unix-epoch
                  (encode-universal-time 0 0 0 day month year))))
    (utilities:make-xsd-date :milliseconds (* seconds 1000))))

(defun year/month/day (xsd-date)
  (check-type xsd-date utilities:xsd-date)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time
       (utilities:unix-to-lisp-epoch (/ (utilities::xsd-date-milliseconds xsd-date) 1000)))
    (declare (ignore sec min hour))
    (list year month day)))
