;;; Copyright © 2007, 2009 The Open University.

(defsystem :irs-tests
  :description "IRS tests"
  :depends-on (:irs-tests-core :trusted-travel)
  :components
  ((:file "api-rest")
   #+:irs-use-lispweb
   (:file "browser-interface")
   (:file "cs-invocation")
   (:file "grounding")
   (:file "monitoring-suite")
   (:file "top")
   (:file "trusted-travel-suite")))
