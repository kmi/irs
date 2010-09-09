;;; Copyright © 2009 The Open University

(in-package #:ocml)

(in-ontology utilities)

;;; Functions (or relations) common in the functional programming
;;; world.

(def-rule #_zip
    "Each element in the third list will be a pair of the
corresponding elements in the first two lists."
  ((#_zip () () ()))
  ((#_zip (?x . ?xs) (?y . ?ys) ((?x ?y) . ?zs)) if
   (#_zip ?xs ?ys ?zs)))
