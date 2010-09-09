(in-package user)


(defun test (x y)
  (+ x y))

(def-irs-corba-interface test_ontology  ;;this is the ontology name
  test  ;;this is the method name
  ((x "long")
   (y "long")
   )
  "long"
   test   ;;this is the lisp function
  )