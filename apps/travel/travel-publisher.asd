;;; Copyright © 2008 The Open University

(defsystem :travel-publisher
    :description "Travel application's web publisher."
    :depends-on (:irs-publisher :travel :travel-services)
    :components
    ((:file "publisher")))
