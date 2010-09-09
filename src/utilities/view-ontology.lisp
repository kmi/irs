(in-package irs-utilities)

;; Modified Carlos 25-10-2006
;; Introduced include-instances and recursive-retrieval parameters in between
;; Nothing but wsmo-protocol/soap-post-handlers was aparently calling this method
(defun generate-class-descriptions (&optional
                                    (ontology-name (ocml::name ocml::*current-ontology*))
                                    (view-only-items-in-current-ontology t)
                                    include-base-ontology-p
                                    (include-instances t)
                                    (recursive-retrieval nil)
                                    (number-of-items 0)
                                    subclass-of (not-subclass-of web-onto::*classes-to-hide*) root-classes-only-p
                                    include-wsmo-base-ontology-p)
  (ocml::select-ontology ontology-name)
  (let* ((ontology (ocml::get-ontology ontology-name))
         (current-ontologies
          (ocml::current-ontologies ontology
                                    view-only-items-in-current-ontology
                                    include-base-ontology-p include-wsmo-base-ontology-p))
         (number-of-classes number-of-items) (ok-p t)
         class-descriptions)
    (when (and subclass-of (symbolp subclass-of))
      (setf subclass-of (list subclass-of)))
    (when (and not-subclass-of (symbolp not-subclass-of))
      (setf not-subclass-of (list not-subclass-of)))
    (catch 'over-limit
      (maphash
       #'(lambda (key value)
           ;;(setf aa key bb value)
           (when (web-onto::filter key value)
             (when (and (ocml::limited-p ocml::*list-length-limit*)
                        (> number-of-classes ocml::*list-length-limit*))
               (setf ok-p nil)
               (throw 'over-limit nil))
             (when (and (find (iu::home-ontology value) current-ontologies :test #'eq)
                        (or (null subclass-of)
                            (find (ocml::name value) subclass-of)
                            (intersection 
                             subclass-of
                             (mapcar #'ocml::name 
                                     (ocml::domain-superclasses value))))
                        (or (null not-subclass-of)
                            (and
                             (not (find (ocml::name value) not-subclass-of))
                             (not (intersection not-subclass-of
                                                (mapcar #'ocml::name 
                                                        (ocml::domain-superclasses 
                                                         value))))))
                        (or (null root-classes-only-p)
                            (null (ocml::direct-domain-superclasses value))))
               (incf number-of-classes)
               (push (generate-class-description (ocml::name (iu::home-ontology value)) key value include-instances recursive-retrieval) class-descriptions))))
       ocml::*domain-classes*))
    (values class-descriptions ok-p number-of-classes)))

;; Carlos 25-10-2006
;; Serialize just the class definition
(defun generate-class-description (ontology-name class-name class-structure &optional (include-instances t) (recursive-retrieval nil))
  (if include-instances
      (let ((instances (mapcar #'ocml::name (ocml::get-current-direct-instances class-structure)))
            (slots (get-domain-slot-names class-structure)))
        (format nil "~(~a~%~a~)"
                (serialize-class-definition ontology-name class-name class-structure recursive-retrieval)
                (generate-instances-information ontology-name instances slots)))
      (serialize-class-definition ontology-name class-name class-structure recursive-retrieval)))

(defun serialize-class-definition (ontology-name class-name class-structure &optional recursive-retrieval)
  (ocml:with-ontology (ontology-name)
    (let* ((subclasses (mapcar #'ocml::name (ocml::current-direct-subclasses class-structure)))
           (superclasses (mapcar #'ocml::name (ocml::direct-domain-superclasses class-structure)))
           (local-slots (get-local-slot-names class-structure)))
      (format nil "<class><name>~a</name>
<superclasses>~{~{~%<superclass><name>~a</name>
<ontology_name>~a</ontology_name>
<ontology_namespace_url>~A</ontology_namespace_url>
</superclass>~}~}</superclasses><subclasses>~{~%<subclass>~a</subclass>~}</subclasses>~%<slots>~{~{~%<slot><slot_name>~a</slot_name><slot_types>~{~%<slot_type>~a</slot_type>~}</slot_types><slot_min_cardinality>~a</slot_min_cardinality><slot_max_cardinality>~a</slot_max_cardinality><slot_fixed_values>~{<slot_fixed_value>~a</slot_fixed_value>~}</slot_fixed_values><slot_default_values>~{<slot_default_value>~a</slot_default_value>~}</slot_default_values></slot>~}~}</slots></class>"
              (extern-ocml-sym class-name)
              (mapcar #'(lambda (superclass)
                          (let* ((struct (ocml::get-domain-class superclass))
                                 (ont (ocml::home-ontology struct)))
                            (list superclass (extern-ocml-sym ont)
                                  (ocml::namespace-uri-of ont))))
                      superclasses)
              subclasses
              (mapcar #'(lambda (slot) (list slot
                                             (obtain-slot-types class-structure slot)
                                             (ocml::get-min-cardinality class-structure slot)
                                             (ocml::get-max-cardinality class-structure slot)
                                             (obtain-slot-fixed-values class-structure slot recursive-retrieval)
                                             (obtain-slot-default-values class-structure slot recursive-retrieval)))
                      local-slots)))))

;;; {{{ Symbol representation translation
(defun extern-ocml-sym (ocml-object)
  "Return string representing OCML-OBJECT.

OCML-OBJECT may be an object, or the name of an object."
  (format nil "~A"
          (if (symbolp ocml-object)
              ocml-object
              (ocml::name ocml-object))))
;;; }}}

;; Get slot names for a class (includes inherited slots)
(defun get-domain-slot-names (class-structure)

  (let ((obsolete-names (ocml::obsolete-names class-structure)))
    (mapcan #'(lambda (slot-info) 
                (let ((slot-name (if (listp slot-info)
                                     (car slot-info)
                                   slot-info)))
                  (unless (find slot-name obsolete-names)
                    (list slot-name))))
            (ocml::domain-slots class-structure)))
)

;; Get local slot names for a class
(defun get-local-slot-names (class-structure)

  (let ((obsolete-names (ocml::obsolete-names class-structure)))
    (mapcan #'(lambda (slot-info) 
                (let ((slot-name (if (listp slot-info)
                                     (car slot-info)
                                   slot-info)))
                  (unless (find slot-name obsolete-names)
                    (list slot-name))))
            (ocml::local-slots class-structure)))
)

;; Added by Carlos 28-9-06
;; Retrieves the slot values for instances but retrieving the ontology
;; where instance values are defined. Required for supporting namespaces.
(defun generate-instances-information (ontology-name instances slots)
  
  (format nil "<instances>~{~%~a~}</instances>"
          (mapcar #'(lambda (instance) 
                        (generate-instance-information ontology-name instance slots))       
                        instances)))

;; Added by Carlos 28-9-06 to retrieve instances with the correct slots values
;; which include ontologies for instances
;; Optional parameter for recursively retrieving the instances definition
(defun generate-instance-information (ontology-name instance slots &optional (recursive-retrieval nil))

  (ocml::select-ontology ontology-name)

  (format nil "<instance>~a<instance_slots>~{~{~%<instance_slot><instance_slot_name>~a</instance_slot_name><instance_slot_values>~{~%<instance_slot_value>~a</instance_slot_value>~}</instance_slot_values></instance_slot>~}~}</instance_slots></instance>"
          (generate-instance-signature ontology-name instance)
          
          (mapcar #'(lambda (slot) 
                      (let ((values (ocml::setofall '?x `(,slot ,instance ?x))))
                        (when values
                          (list slot 
                                (generate-slot-values values recursive-retrieval)))))             
                  slots)))

(defun generate-instance-signature (ontology-name instance)

  (ocml::select-ontology ontology-name)
  (let ((instance-struct (ocml::find-current-instance instance))
        (inst-class (ocml::get-ocml-class (ocml::findany '?c `(= ?c (ocml::the-parent ,instance))))))

    (format nil "<instance_name>~a</instance_name><ontology_name>~a</ontology_name>~a"
            instance
            (ocml::name (iu::home-ontology instance-struct))
            (generate-class-signature inst-class)))
)

(defun generate-class-signature (class-struct)
  (format nil "<class><class_name>~a</class_name><ontology_name>~a</ontology_name></class>"
          (ocml::name class-struct)
          (ocml::name (iu::home-ontology class-struct)))
)

(defun generate-instance-slot-values (instances slots)
  (mapcar #'(lambda (instance) 
              (list instance 
                    (mapcan #'(lambda (slot) 
                                (let ((values (ocml::setofall '?x `(,slot ,instance ?x))))
                                  (when values
                                    (list (list slot values)))))
                            slots)))
          instances))

;; Generate a slot values 
;; Optional parameter for recursively retrieving the instances definition
(defun generate-slot-values (slot-values &optional recursive-retrieval)
  (when slot-values
    (mapcar #'(lambda (slot-value)                 
                (let* ((slot-value-instance (ocml::find-current-instance slot-value))
                      (slot-value-class (ocml::get-ocml-class slot-value)))
                  
                  (if slot-value-instance
                      (let ((inst-class (ocml::get-ocml-class (ocml::findany '?c `(= ?c (ocml::the-parent ,slot-value)))))
                            (ontology-name (ocml::name (iu::home-ontology slot-value-instance))))

                        (if recursive-retrieval
                            (generate-instance-information ontology-name slot-value (get-domain-slot-names inst-class) recursive-retrieval)

                          (generate-instance-signature ontology-name (ocml::name slot-value-instance))))

                    (if slot-value-class
                        (generate-class-signature slot-value-class)
                        
                      (format nil "~a"
                              (insert-html-characters slot-value))))))
          slot-values)))

;; Added by Carlos 28-9-06
;; Optional parameter for recursively retrieving the instances definition
(defun obtain-slot-default-values (class-structure slot &optional recursive-retrieval)
  (multiple-value-bind (fixed-values default-values)
      (ocml::get-slot-values-from-class-structure class-structure slot)
    (declare (ignore fixed-values))
    (generate-slot-values default-values recursive-retrieval)))

;; Added by Carlos 28-9-06
;; Optional parameter for recursively retrieving the instances definition
(defun obtain-slot-fixed-values (class-structure slot &optional recursive-retrieval)
  (multiple-value-bind (fixed-values default-values)
      (ocml::get-slot-values-from-class-structure class-structure slot)
    (declare (ignore default-values))
    (generate-slot-values fixed-values recursive-retrieval)))

;; Added by Carlos 23-2-07
;; Include all the types definitions
;; Find better way to handle the lists
(defun obtain-slot-types (class-structure slot)
  (mapcan #'(lambda (slot-type)
              (if (listp slot-type)
                  (mapcar #'(lambda (conj-slot-type)
                           (format nil "<type_name>~a</type_name><type_ontology>~a</type_ontology>"
                                   conj-slot-type
                                   (ocml::name (iu::home-ontology (ocml::get-ocml-class conj-slot-type)))))
                          (cdr slot-type))

                (list (format nil "<type_name>~a</type_name><type_ontology>~a</type_ontology>"
                        slot-type
                        (ocml::name (iu::home-ontology (ocml::get-ocml-class slot-type)))))))
          (ocml::get-slot-type class-structure slot))
)
