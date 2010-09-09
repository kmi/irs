;;; Copyright © 2008, 2009 The Open University

(in-package #:ocml)

(in-ontology rfc2616)

;;; I'd prefer this were all in OCML, but I don't have time.

(defun %http-date (&optional (time (get-universal-time)))
  "Make a HTTP-suitable string showing the universal time TIME.

If TIME is not supplied, then 'now' is assumed."
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~2,'0d ~4,'0d ~2,'0d:~2,'0d:~2,'0d GMT"
            (aref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") weekday)
            day
            (aref #(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)
            year hour minute second)))

;;; Encode strings for embeddeding in a URL.
(def-function #_url-encoding (?raw-string)
  :lisp-fun #'hunchentoot:url-encode)

;;; XXX Would be nice if this were bidirectional.
(def-relation #_url-encoding (?raw ?encoded)
   :prove-by (= ?encoded (#_url-encoding ?raw)))
