;;; Copyright © 2007-2009 The Open University

(in-package #:ocml)

(in-ontology lhdl-domain)

;;; {{{ Utilities
(defun %skyhook-instance-name (value type)
  (let* ((*package* (find-package '#:ocml))
         (i (find-current-instance (read-from-string value))))
    (if i
        (name i)
        (error "Cannot map `~A' to a known instance." value))))

(defun %skyhook-integer (value type)
  (xpm::with-ocml-ctx (:class type)
    (let ((i (xpm::o-instance))
          (val (read-from-string value)))
      (if (typep val 'integer)
          (setf val (coerce val 'integer))
          (error "Cannot elevate ~S to concept ~A." val type))
      (xpm::o-set-slot 'ocml::has-value val i)
      i)))

(defun %skyhook-boolean (value type)
  (xpm::with-ocml-ctx (:class type)
    (let ((i (xpm::o-instance)))
      (xpm::o-set-slot 'ocml::has-value (string= "true" value) i)
      i)))

(defun %skyhook-double (value type)
  (xpm::with-ocml-ctx (:class type)
    (let ((i (xpm::o-instance))
          (val (read-from-string value)))
      (if (typep val 'number)
          (setf val (coerce val 'float))
          (error "Cannot elevate ~S to concept ~A." val type))
      (xpm::o-set-slot 'ocml::has-value val i)
      i)))

(defun %skyhook-string (value type)
  (xpm::with-ocml-ctx (:class type)
    (let ((i (xpm::o-instance)))
      (xpm::o-set-slot 'ocml::has-value value i)
      i)))
;;; }}}

(define-skyhook skyhook-filename lhdl-filename
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'lhdl-filename)
      (let ((i (xpm::o-instance)))
        (xpm::o-set-slot 'ocml::has-value value i)
        i))))

(define-skyhook skyhook-lhdl-boolean lhdl-boolean
  (lambda (value)
    (cond ((string= "true" value)
           'ocml::lhdl-true)
          ((string= "false" value)
           'ocml::lhdl-false)
          (t
           (error "Cannot elevate ~S to an lhdl-double." value)))))

(defun %skyhook-3d-double (value type)
  (let ((lst (read-from-string value)))
    (if (and (listp lst)
             (= 3 (length lst))
             (every #'(lambda (x) (typep x 'number)) lst))
        (xpm::with-ocml-ctx (:class type)
          (let ((3d (xpm::o-instance)))
            (xpm::o-set-slot 'has-x (first lst) 3d)
            (xpm::o-set-slot 'has-y (second lst) 3d)
            (xpm::o-set-slot 'has-z (third lst) 3d)
            3d))
        (error "Cannot elevate ~S to a ~A." value type))))

(defun skyhook-2d-double (value type)
  (let ((val (read-from-string value)))
    (if (and (listp val)
             (= 2 (length val))
             (typep (first val) 'number)
             (typep (second val) 'number))
        (xpm::with-ocml-ctx (:class type)
          (let ((2d (xpm::o-instance)))
            (xpm::o-set-slot 'has-x (coerce (first val) 'float) 2d)
            (xpm::o-set-slot 'has-y (coerce (second val) 'float) 2d)
            2d))
        (error "Cannot elevate ~S to a ~A." value type))))

(define-skyhook skyhook-lhdl-double lhdl-double
  (lambda (value)
    (let ((val (read-from-string value)))
      (if (typep val 'number)
          (coerce val 'double-float)
          (error "Cannot elevate ~S to an lhdl-double." value)))))

(define-skyhook skyhook-lhdl-integer lhdl-integer
  (lambda (value)
    (let ((val (read-from-string value)))
      (if (typep val 'number)
          (xpm::with-ocml-ctx (:class 'lhdl-integer)
            (let ((i (xpm::o-instance)))
              (xpm::o-set-slot 'ocml::has-value (coerce val 'integer) i)
              i))
        (error "Cannot elevate ~S to an lhdl-integer." value)))))
