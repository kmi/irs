;;; Dave Lambert, 2007.

(defsystem :travel
    :description "Travel."
    :depends-on (:irs :travel-services)
    :components
    ((:file "defpackage")
     (:file "startup" :depends-on ("defpackage"))))
