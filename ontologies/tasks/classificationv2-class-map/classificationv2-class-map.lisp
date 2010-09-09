;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML; -*- 

(in-package "OCML")

(in-ontology classificationv2-class-map)




;;;ALL-FEATURES-IN-SOLUTION
;;;Redefined here assuming that a solution is defined in terms of its slots
(def-function all-features-in-solution (?sol)
  :body (setofall ?f  (member ?f (all-class-slots ?sol))))

;;;SOLUTION-FEATURE-SPEC
;;;Redefined here assuming that teh spec of a solution feature, say f, is teh value of slot f.
(def-function solution-feature-spec (?sol ?f)
  :lisp-fun #'(lambda (sol f) (sol-feature-spec-from-class-slot sol f)))

(defun sol-feature-spec-from-class-slot (sol f)
  (let ((values (all-class-slot-values sol f)))
    (if values
      `(kappa (?v) (member ?v ,values))
      :nothing)))



            