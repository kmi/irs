;;; Copyright © 2008 The Open University

(in-package #:irs.grounding)

(define-condition <grounding-fault> (error)
  ((service :accessor service-of
            :initarg :service
            :initform nil)
   (service-response :accessor service-response-of
                     :initarg :service-response
                     :initform nil
                     :type (or null string))
   ;; When we catch an error in the grounding, but can't figure out
   ;; the exact cause ourselves, we set OTHER-CAUSE to that error.
   (other-cause :accessor other-cause-of
                :initarg :other-cause
                :initform nil))
  (:documentation "Indicates a grounding operation failure.")
  (:report (lambda (condition stream)
             (format stream "Failure to ground ~A.  " (service-of condition))
             (if (service-response-of condition)
              (format stream "Server response: ~S." (service-response-of condition))
              (format stream "Cause: ~A." (other-cause-of condition))))))
