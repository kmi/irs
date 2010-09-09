;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology classificationv2  task 
                   "OCML:LIBRARY;tasks;classificationv2;load.lisp"))

(def-ontology classificationv2-class-map
  :includes (classificationv2)
  :type :task 
  
  :author "wenjin"
  :allowed-editors ("enrico")
  
  :files ("classificationv2-class-map"
          "new"
          ))