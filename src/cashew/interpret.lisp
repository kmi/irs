;;; Mode: Lisp; Package: ocml

(in-package CASHEW)

(defun cl-user::execute-workflow-instance (wfi)
  (execute-workflow-instance wfi))

(defun execute-workflow-instance (wfi)
  (format t "executing ~s in ~s~%" wfi wsmo-protocol::*current-orchestration-web-service*)
  (dispatch-workflow wfi
                     #'execute-sequential
                     #'execute-concurrent
                     #'execute-interleaved
                     #'execute-int-choice
                     #'execute-ext-choice
                     #'execute-int-while
                     #'execute-ext-while
                     #'execute-int-until
                     #'execute-ext-until))

(defun execute-sequential (wfi)
  (let ((perfs (ocml::findany '?perfs `(= ?perfs (ocml::the-class-slot-value ,wfi ocml::has-perform-list)))))
    (format t "to do:~s" perfs)))

(defun execute-concurrent (wfi)
  (declare (ignore wfi))
  (format t "to do"))

(defun execute-interleaved (wfi)
  (declare (ignore wfi))
  (format t "to do"))

(defun execute-int-choice (wfi)
  (declare (ignore wfi))
  (format t "to do"))

(defun execute-ext-choice (wfi)
  (declare (ignore wfi))
  (format t "to do"))

(defun execute-int-while (wfi)
  (declare (ignore wfi))
  (format t "to do"))

(defun execute-ext-while (wfi)
  (declare (ignore wfi))
  (format t "to do"))

(defun execute-int-until (wfi)
  (declare (ignore wfi))
  (format t "to do"))

(defun execute-ext-until (wfi)
  (declare (ignore wfi))
  (format t "to do"))
