(defpackage sepc-to-bpmo
  (:use :common-lisp :cl-who :asdf :irs))

(in-package :sepc-to-bpmo)

(defsystem :sepc-to-bpmo
    :description "Translates sEPC models to BPMO ones."
    :depends-on (:irs)
    :components
    ((:file "translate-sepc")))
