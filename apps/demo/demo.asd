;;; Copyright © 2009 The Open University

(defsystem :demo
  :description "Portmanteau of IRS demos."
  :depends-on (:irs)
  :components
  ((:file "defpackage")
   (:file "demo" :depends-on ("defpackage"))))
