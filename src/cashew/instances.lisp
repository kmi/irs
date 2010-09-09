;;; Mode: Lisp; Package: ocml

(in-package CASHEW)

(defun cl-user::instantiate-workflow (wf)
  (instantiate-workflow wf))

(defun instantiate-workflow (wf)
  (let ((wfi (ocml::name (ocml::internal-define-anonymous-instance wf "" nil)))
        (performs (ocml::setofall '?p `(ocml::has-perform ,wf ?p))))
    (format t "instantiating ~s as ~s~%" wf wfi) 
    (ocml::tell1
     `(ocml::workflow-composes
       ,wfi 
       ,(mapcar 'instantiate-perform performs)))
    wfi))

(defun instantiate-perform (perf)
  (format t "instantiating a perform, ~s~%" perf)
  (let* ((superclasses (ocml::setofall '?c `(ocml::subclass-of ,perf ?c)))
         (object (cond ((member 'ocml::perform-goal superclasses) 
                        (let ((goal (ocml::findany '?g `(= ?g (ocml::the-class-slot-value ,perf ocml::has-goal)))))
                          (format t "it's performing a goal~%")
                          (instantiate-goal goal)))
                       ((member 'ocml::perform-workflow superclasses)
                        (let ((wf (ocml::findany '?wf `(= ?wf (ocml::the-class-slot-value ,perf ocml::has-workflow)))))
                          (format t "it's performing a workflow~%")
                          (instantiate-workflow wf)))
                       (t (format t "it's not performing a goal or a workflow~%")  perf))))
    (format t "object is ~s~%" object)
    (list perf object)))

(defun instantiate-goal (goal)
  (format t "instantiating ~s~%" goal)
  (ocml::name (ocml::internal-define-anonymous-instance goal "" nil)))

(defun get-perform-instance (wfi perf)
  (cadr (assoc perf (ocml::findany '?p `(ocml::workflow-composes ,wfi ?p)))))

(defun dispatch-workflow (wf 
                          sequential
                          concurrent
                          interleaved
                          int-choice
                          ext-choice
                          int-while
                          ext-while
                          int-until
                          ext-until)
  (let ((type (ocml::findany '?t `(direct-subclass-of ,wf ?t)))
        (args (cons wf nil)))
    (cond 
     ((equal type 'sequential) (apply sequential args))
     ((equal type 'concurrent) (apply concurrent args))
     ((equal type 'interleaved) (apply interleaved args))
     ((equal type 'int-choice) (apply int-choice args))
     ((equal type 'ext-choice) (apply ext-choice args))
     ((equal type 'int-while) (apply int-while args))
     ((equal type 'ext-while) (apply ext-while args))
     ((equal type 'int-until) (apply int-until args))
     ((equal type 'ext-until) (apply ext-until args)))))
