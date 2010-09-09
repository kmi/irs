;;; Dave Lambert, 2007.

(defsystem :trusted-travel
    :description "Trusted travel."
    :depends-on (:irs :travel)
    :components
    ((:file "defpackage")
     (:file "page" :depends-on ("defpackage"))))
