;;; Copyright © 2009 The Open University.

(defsystem :soa4all
    :description "SOA4All"
    :depends-on (:irs)
    :components
    ((:module :lisp
      :components
      ((:file "defpackage")
       (:file "soa4all" :depends-on ("defpackage"))
       (:file "web" :depends-on ("defpackage"))))))
