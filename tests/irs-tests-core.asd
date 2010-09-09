;;; Copyright © 2009 The Open University.

(defsystem :irs-tests-core
  :description "IRS tests"
  :depends-on (:drakma :fiveam :irs)
  :components
  ((:file "defpackage")
   (:file "tools" :depends-on ("defpackage"))))
