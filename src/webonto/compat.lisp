;;; Copyright ® 2009 The Open University

;;; Dummy definitions for things that are available only on Lispworks
;;; AND required at compile time.

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro fake-func (name)
    `(defun ,name (&rest rest)
       (declare (ignore rest))
       (error "Fake function ‘~A’ called." ',name)))
  (defmacro fake-macro (name)
    `(defmacro ,name (&rest rest)
       (declare (ignore rest))))
  (defmacro fake-var (name)
    `(defvar ,name)))

(fake-func http::get-package-deal-message)

(fake-macro http::define-page)

(fake-macro http::define-page2)

(fake-macro http::define-page3)

(fake-func http::color-string)

(fake-func html:html-out)

(fake-func html::applet)

(fake-func http::log)

(fake-func http::lw-debug)

(fake-func http::princ-to-binary-stream)

(fake-var http::*http-info*)

(fake-var http::*broadcasting-p*)

(fake-var http::*http-stream*)

(fake-var *task-colour*)

