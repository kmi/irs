;;; Dave Lambert, 2007.

(in-package :tutorial-iswc-2007-system)

(defsystem :tutorial-iswc-2007
    :description "Semantic Business Process Modelling tutorial-iswc-2007."
    :depends-on (:irs :bpmo-to-irs)
    :components ((:file "tutorial")))
