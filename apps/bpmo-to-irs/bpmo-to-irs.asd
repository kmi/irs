(defpackage bpmo-to-irs
  (:use :common-lisp :asdf :irs)
  (:nicknames "B2I"))

(in-package :bpmo-to-irs)

;;(irs:use-application :bpmo-to-irs)

(defsystem :bpmo-to-irs
    :description "Translates BPMO models to executable IRS."
    :depends-on (:irs)
    :components
    ((:file "generate-wsmo")))
