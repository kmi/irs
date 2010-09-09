;;; Copyright © 2008 The Open University

(defsystem :monitoring-engine
    :description "Monitoring engine."
    :depends-on (:irs)
    :components
    ((:file "defpackage")
     (:file "engine" :depends-on ("defpackage"))))
