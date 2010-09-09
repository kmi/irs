;;; Copyright © 2008 The Open University

(in-package #:utilities)

;;; WSML has primitive datatypes for date, time, and dateTime.  We
;;; create a structure to capture this, so we can dispatch on the
;;; type in the REST API used by the WSMOStudio IRS plugin.

(defstruct xsd-datetime
  ;; Milliseconds since epoch.
  milliseconds)

#+:lispworks
(deftype xsd-datetime () '(satisfies xsd-datetime-p))

(defstruct xsd-date
  milliseconds)

#+:lispworks
(deftype xsd-date () '(satisfies xsd-date-p))

;; Note that this should not take into account the actual date
(defstruct xsd-time
  milliseconds)

#+:lispworks
(deftype xsd-time () '(satisfies xsd-time-p))

(defun xsd-date (year month day)
  (let ((seconds (utilities:lisp-to-unix-epoch
                  (encode-universal-time 0 0 0 day month year))))
    (make-xsd-date :milliseconds (* seconds 1000))))

(defun year/month/day (xsd-date)
  (check-type xsd-date xsd-date)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time
       (utilities:unix-to-lisp-epoch (/ (xsd-date-milliseconds xsd-date) 1000)))
    (declare (ignore sec min hour))
    (list year month day)))
