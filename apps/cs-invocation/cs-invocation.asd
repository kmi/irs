(defpackage cs-invocation
  (:use :common-lisp :cl-who :asdf :irs))

(in-package :cs-invocation)

(defsystem :cs-invocation
    :description "A new goal invocation based on CS"
    :depends-on (:irs)
    :components
    ((:file "services")))
