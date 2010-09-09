;;; Mode: Lisp; Package: http

;;; Author: John Domingue

;;; The Open University

(in-package "HTTP")


(http::define-page ("Refined Results Page" :func-name web-onto::refine-lois-results
                                           :class :user
                                           :bgcolor
                                           web-onto::*default-page-colour*
                                           :Header-p nil
                                           ) 
    ()
  (with-field-value (operator variable values ontology-name)
    (html-out (header 2 "Refined Results Page"))
    (let* ((*package* (find-package "OCML"))
           (operator (read-from-string (string-upcase operator)))
           (variable (read-from-string (string-upcase variable)))
           (values (read-from-string (string-upcase values)))
           (ontology-name (read-from-string (string-upcase ontology-name)))
           (variable-values (cdr (assoc variable values)))
           (ocml::*current-ontology* ocml::*current-ontology*)
           (ocml::*current-ontologies* ocml::*current-ontologies*)
           (ocml::*defined-relations* ocml::*defined-relations*)
           (ocml::*axioms* ocml::*axioms*)
           (ocml::*defined-functions* ocml::*defined-functions*)
           (ocml::*bc-rules* ocml::*bc-rules*)
           (ocml::*domain-classes* ocml::*domain-classes*))
      (declare (special ocml::*current-ontology* ocml::*current-ontologies*
                        ocml::*defined-relations* ocml::*axioms*
                        ocml::*defined-functions* ocml::*bc-rules*
                        ocml::*domain-classes*))
      (ocml::select-ontology ontology-name)
      (cond ((null variable-values)
             (html-out (format nil "Sorry couldn't find any values for the variable ~a"
                               variable)))
            (t (html-out
                (format nil "The results of applying the operator ~(~a~) for the values of the variable ~(~a~) are:<p>~a"
                        operator variable
                        (internal-insert-ocml-links
                         (format nil "~a"
                                 (apply-lois-query-refinement-operator
                                  operator
                                  variable variable-values))
                         #'ocml-lookup-current-word
                         ontology-name))))))))


(defmethod apply-lois-query-refinement-operator ((operator (eql 'ocml::most-common-value))
                                                 variable variable-values)
  (multiple-value-bind (most-common-elements number)
      (ocml::most-common-elements variable-values)
    (format nil "The most common element ~:[were~;was~] ~(~{~a ~}~). ~d ~:[were~;was~] found."
            (= (length most-common-elements) 1) most-common-elements
            number (= number 1))))

(defmethod apply-lois-query-refinement-operator ((operator (eql 'ocml::categorise))
                                                 variable variable-values)
  (ocml::common-ancestor variable-values nil))


(defmethod apply-lois-query-refinement-operator ((operator (eql 'ocml::most-common-type))
                                                 variable variable-values)
  (multiple-value-bind (most-common-elements number)
      (ocml::most-common-elements
       (mapcar #'(lambda (x) (ocml::name (class-of (ocml::find-current-instance x))))
               variable-values))
    (format nil "The most common type of element ~:[were~;was~] ~(~{~a ~}~). ~d ~:[were~;was~] found."
            (= (length most-common-elements) 1) most-common-elements
            number (= number 1))))
  
  

