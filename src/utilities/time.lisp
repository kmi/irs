;;; Copyright © 2007, 2008 The Open University

(in-package :irs.utilities)

(define-constant +unix-epoch-offset+
  #.(encode-universal-time 0 0 0 1 1 1970)
  "The number of seconds between the Common Lisp epoch and the Unix
  one.")

#+:win32
(define-constant +win32-epoch-offset+
  #.(encode-universal-time 0 0 0 1 1 1601)
  "The number of seconds between the Common Lisp epoch and the Win32
  one.")

#+(and :irs-lispworks :unix)
(fli:define-c-struct timeval
    (tv_sec :long)
  (tv_usec :long))

#+(and :irs-lispworks :unix)
(fli:define-foreign-function
    (os-time "gettimeofday" :source)
    ((timeval (:pointer timeval))
     (timezone :int))
  :result-type :int
  :language :ansi-c)

#+(and :irs-lispworks :win32)
(fli:define-c-struct filetime
  (dwlowdatetime :int)
  (dwhighdatetime :int))

#+(and :irs-lispworks :win32)
(fli:define-foreign-function
    (os-time "GetSystemTimeAsFileTime" :source)
    ((ft (:pointer filetime)))
  :result-type :void
  :language :ansi-c)

(defun get-accurate-time ()
  "Return seconds and microseconds since Lisp epoch."
  #+(and :irs-lispworks :unix)
  (let* ((timeval (fli:allocate-foreign-object :type 'timeval)))
    (fli:with-coerced-pointer (timeval-ptr) timeval
      (os-time timeval-ptr 0)
      (let ((secs (fli:foreign-slot-value timeval 'tv_sec))
	    (usecs (fli:foreign-slot-value timeval 'tv_usec)))
	    (free-foreign-object timeval)
	    (values (+ +unix-epoch-offset+ secs) usecs))))
  #+(and :irs-lispworks :win32)
  (let* ((ft (fli:allocate-foreign-object :type 'filetime)))
    (fli:with-coerced-pointer (ft-ptr) ft
      (os-time ft-ptr)
      (let* ((low (fli:foreign-slot-value ft 'dwlowdatetime))
             (high (fli:foreign-slot-value ft 'dwhighdatetime))
             (microseconds (floor (/ (+ low (* high (expt 2 32))) 10)))
             (secs (+ +win32-epoch-offset+ (floor (/ microseconds 1000000))))
             (usecs (- microseconds (* secs 1000000))))
        (free-foreign-object ft)
        (values secs usecs)))))

(defun lisp-to-unix-epoch (seconds)
  "SECONDS in Lisp epoch converted to seconds in Unix epoch."
  (- seconds +unix-epoch-offset+))

(defun unix-to-lisp-epoch (seconds)
  "SECONDS in the Unix epoch converted to seconds in Lisp epoch."
  (+ seconds +unix-epoch-offset+))
