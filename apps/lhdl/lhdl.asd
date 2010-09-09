;;; Copyright © 2007,2008 The Open University.

(defsystem :lhdl
    :description "Living Human Digital Library"
    :depends-on (:irs :nih)
    :components
    ((:module :lisp
      :components
      ((:file "defpackage")
       (:file "gui" :depends-on ("defpackage"))
       (:file "lhdl" :depends-on ("defpackage"))
       (:file "web" :depends-on ("defpackage"))))))
