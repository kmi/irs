(in-package irs-utilities)

(defun ocml::generate-ocml-instance-source (instance-name &optional
                                                    (ontology-name (ocml::name ocml::*current-ontology*)))
  (ocml::select-ontology ontology-name)
  (let* ((class-name (web-onto::findany '?x `(ocml::instance-of ,instance-name ?x)))
         (class (and class-name (ocml::get-domain-class class-name))))
    (when class
      `(ocml::def-instance ,instance-name ,class-name
         ,(mapcan
           #'(lambda (x) 
               (let ((values (ocml::careful-setofall '?x `(,x ,instance-name ?x))))
                 (when values
                   `((,x ,@values)))))
           (ocml::ocml-most-specific-class-slots class))))))

(defun ocml::generate-ocml-instances-source (instance-names &optional
                                                (ontology-name (ocml::name ocml::*current-ontology*)))
  (mapcar #'(lambda (x) (ocml::generate-ocml-instance-source x ontology-name))
          instance-names))