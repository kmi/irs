;;; Copyright © 2008 The Open University

(defsystem :math
  :description "Demo IRS application."
  :depends-on (:irs)
  :components
  ((:file "defpackage")
   (:file "math" :depends-on ("defpackage"))))
