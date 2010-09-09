(in-package :irs.grounding)

;;; Eventually, more of the lifting and lowering code from xpath-mini
;;; will move here. For the moment, it's just the skyhook code and the
;;; interface to lifting and lowering from the web service invocation
;;; side that lives here.

(defvar *debug-stream* nil
  "If non-nil, the stream to which to write the content sent and
received from services. ")

(defun set-debug-stream (maybe-stream)
  (dolist (var '(*debug-stream*
                 drakma:*header-stream*))
    (setf (symbol-value var) maybe-stream)))

;;; Skyhooks are imaginary things which hold stuff up with no apparent
;;; means of support.  Skyhooks lift the strings sent by WSMO Studio
;;; or the IRS Browser on goal invocation into the OCML instances the
;;; strings name.  Although this is something of a cheat, is really
;;; just a shortcut to a point we'll need to sort out anyway: properly
;;; serializing subsets of ontologies.

(defmacro ocml::define-skyhook (name class &body exprs)
  (if (function-like? (first exprs))
      `(progn
         (ocml::tell1 '(ocml::skyhook-defined ,class ,name))
         (setf (symbol-function ',name) ,(car exprs)))))

#+lispworks
(editor:setup-indent 'ocml::define-skyhook 0 2)

(defun already-skyhooked? (value)
  (and (symbolp value)
       (string= "INSTANCE" (symbol-name (pure-name value)))))

;; This is more complicated than it need be, because we have to avoid
;; doing conversions where we shouldn't.  That's a problem of backward
;; compatibility.
(defun apply-skyhooks (slot-names ocml-classes values)
  "Convert VALUES to their equivalent OCML-CLASSES instances, if
skyhooks are defined."
  (if (every #'already-skyhooked? values)
      values
    (mapcar #'(lambda (slot-name ocml-class value)
                  (let ((skyhook-function (get-skyhook-function ocml-class)))
                    (if skyhook-function
                        (handler-case (funcall skyhook-function value)
                          (t (c)
                            (error "Error invoking skyhook for slot ~A: '~A'."
                                   slot-name c)))
                        value)))
            slot-names ocml-classes values)))

(defun skyhook-by-instance-name (ocml-type value)
  "Find an OCML instance that has name VALUE."
  (ocml::name (first (ocml::get-class-instances-named-x
                      (ocml::get-domain-class ocml-type) value))))

(defun pure-name (symbol)
  "Strip number from OCML instance names.  INSTANCE123 becomes
INSTANCE."
  (let ((string (symbol-name symbol)))
    (multiple-value-bind (int len) 
        (parse-integer (reverse string) :junk-allowed t)
      (declare (ignore int))
      (intern (subseq string 0 (- (length string) len))
              (symbol-package symbol)))))

(defun soap-type? (type)
  (member type '("sexpr" "string") :test 'string-equal))

(defun function-like? (expr)
  (or (functionp expr)
      (and (listp expr)
           (eq (first expr) 'lambda))))

;;;; General interface to elevation functions.

(defun internal-get-lift-function (class web-service)
  (web-onto::findany
   '?x `(or (and (ocml::lift-defined ,class ?service-class ?x)
                 (= ?service-class (ocml::the-parent ,web-service)))
            (ocml::lift-defined-all ,class ?x))))

(defun internal-get-lower-function (class web-service)
  (web-onto::findany
   '?x `(or (and (= ?service-class (ocml::the-parent ,web-service))
                 (ocml::lower-defined ,class ?service-class ?x))
            (ocml::lower-defined-all ,class ?x))))

(defun internal-get-skyhook-function (class &optional web-service)
  (declare (ignore web-service))
  (web-onto::findany '?x `(ocml::skyhook-defined ,class ?x)))

(defun class-supers (class)
  (ocml::ocml-eval-gen `(ocml::all-superclasses ,class)))

(defun get-lift-function (class &optional (web-service nil))
  (get-elevator 'internal-get-lift-function class web-service))

(defun get-lower-function (class &optional (web-service nil))
  (get-elevator 'internal-get-lower-function class web-service))

(defun get-skyhook-function (class)
  (get-elevator 'internal-get-skyhook-function class))

(defun get-elevator (function class &optional (web-service nil) (supers (class-supers class)))
  (or (funcall function class web-service)
      (and supers
           (get-elevator function (car supers) web-service (cdr supers)))))



