;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(eval-when (eval load)
  (ensure-ontology classificationv2 task
                   "OCML:LIBRARY;tasks;classificationv2;load.lisp"))


(def-ontology heuristic-classification
  :includes (classificationv2)

  :type :method 
  
  :author "wenjin"
  :allowed-editors ("enrico")
  
  :files ("heuristic-classifier"
          "new"
          ))