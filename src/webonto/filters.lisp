;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

;;;filtering out large ontologies
;;;all filters take a key and value argument and are used to filter out
;;;ocml over hash tables

(defun make-string-filter (string)
  #'(lambda (key value)
      (declare (ignore value))
      (search string (symbol-name key) :test #'string-equal)))

(defun make-not-string-filter (string)
  #'(lambda (key value)
      (declare (ignore value))
      (not (search string (symbol-name key) :test #'string-equal))))


(defun make-letter-range-filter (start-letter end-letter)
  #'(lambda (key value)
      (declare (ignore value))
      (unless (stringp start-letter)
        (setf start-letter (symbol-name start-letter)))
      (unless (stringp end-letter)
        (setf end-letter (symbol-name end-letter)))
      (and (string< start-letter (symbol-name key))
           (string< (symbol-name key) end-letter))))

(defun make-ocml-predicate-filter (predicate)
  #'(lambda (key value)
      (declare (ignore value))
      (careful-holds? predicate key)))


(defun parse-filters (filters)
  (let ((or-filters nil) (and-filters nil))
    (mapcar #'(lambda (filter)
                (case (car (last filter))
                  ((ocml::or) (push (internal-parse-file filter) or-filters))
                  ((ocml::and) (push (internal-parse-file filter) and-filters))))
            filters)
    (values or-filters and-filters)))

(defun internal-parse-file (filter)
  (case (car filter)
    ((:string) (make-string-filter (second filter)))
    ((:not-string) (make-not-string-filter (second filter)))
    ((:letter-range) (make-letter-range-filter (second filter) (third filter)))
    ((:ocml-predicate) (make-ocml-predicate-filter (second filter)))))


(defun apply-filter (filter key value)
  (funcall filter key value))

(defun filter (key value)
  (if (or *or-filters* *and-filters*)
      (let ((pass-filters nil))
        (catch 'pass-filter
          (dolist (filter *or-filters*)
            (when (apply-filter filter key value)
              (setf pass-filters t)
              (throw 'pass-filter nil))))
        ;;must satisfy at least one or filter
        ;;(so the or filters are anded with the and filters)
        (when (or (null *or-filters*) pass-filters)
          (catch 'fail-filter
            (dolist (filter *and-filters*)
              (cond ((apply-filter filter key value)
                     (setf pass-filters t))
                    (t (setf pass-filters nil)
                       (throw 'fail-filter nil))))))
        pass-filters)
      t))
