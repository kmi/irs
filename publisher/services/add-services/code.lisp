(in-package cl-user)

(defun add (x y)
  (+ x y))

(defun add-1 (x)
  (+ x 1))

(defun add-100 (x)
  (+ x 100))

(defun subtract (x y)
  (- x y))

(defun get-numbers-from (x)
  (let ((result nil))
    (dotimes (i x)
      (push i result))
    (reverse result)))

(eval-when (eval load)
  (irs-wsmo-web-service-registration orchestration-loop-test
                                     add-1-web-service)
  (irs-wsmo-web-service-registration orchestration-loop-test
                                     get-numbers-web-service))

(eval-when (eval load)
  (irs-wsmo-web-service-registration orchestration-test
                                     add-web-service)
  (irs-wsmo-web-service-registration orchestration-test
                                     add-100-web-service)
  (irs-wsmo-web-service-registration orchestration-test
                                     subtract-web-service)
  (irs-wsmo-web-service-registration orchestration-test
                                     Subtract-To-Add-100-Mediator-Web-Service)
  (irs-wsmo-web-service-registration orchestration-test
                                     Add-To-Subtract-Mediator-Goal1-Web-Service))
