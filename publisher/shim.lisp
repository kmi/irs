(in-package #:irs.publisher)

;;; These things should be in the publisher package, but are actually
;;; in cl-user.  Until we fix that, allow them to be used in the right
;;; package.

(defun start-publisher (&rest rest)
  (apply #'cl-user::start-publisher rest))

(defmacro irs-wsmo-web-service-registration (&body body)
  `(cl-user::irs-wsmo-web-service-registration ,@body))

(defun publish-all-wsmo-services (&rest rest)
  (apply #'cl-user::publish-all-wsmo-services rest))

(defun clear-all-services (&rest rest)
  (apply #'cl-user::clear-all-services rest))
