;;; Dave Lambert, 2007.

(defsystem :irs-publisher
  :description "IRS Publisher"
  ;; :version  ;; It's the same as the IRS server, I guess"3.3.10"
  :depends-on (:irs)
  :components
  ((:file "defpackage")
   (:file "def-irs-soap-interface" :depends-on ("defpackage"))
   (:file "client-support" :depends-on ("defpackage"))
   (:file "function-as-web-service" :depends-on ("defpackage"))
   (:file "publisher-server" :depends-on ("defpackage"))
   (:file "publisher-support" :depends-on ("defpackage"))
   (:file "shim" :depends-on ("defpackage"))
   (:file "start-publisher" :depends-on ("defpackage"))
   (:file "wsmo" :depends-on ("defpackage"))))
