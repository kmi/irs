(in-package :kmi.vt.xpath-mini)

#|

(ocml::load-ontology 'profile 'domain)

(ocml::in-ontology ocml::profile)

(ocml::def-instance OCML::county-buckinghamshire ocml::county)

(ocml::find-current-instance 'ocml::county-buckinghamshire)

(ocml::def-instance ocml::your-domicile ocml::address
  ((ocml::has-county ocml::county-buckinghamshire)
   (ocml::has-postal-code "MK6 3PA")
   (ocml::has-locality "Milton Keynes")
   (ocml::has-postal-address "7 Woodley")
   (ocml::has-country ocml::country-UK)))

(setf rt 'ocml::your-domicile)

(ocml::describe-instance rt)

(ocml::slot-values rt 'ocml::has-postal-code)
(ocml::clear-slot-values rt 'ocml::has-postal-code)
(ocml::add-slot-value rt 'ocml::has-postal-code "MK6 3PA")

(ocml::get-slot-values (ocml::find-current-instance rt) 'ocml::has-postal-code)
(ocml::get-slot-values (ocml::find-current-instance rt) 'ocml::has-county)

(ocml::domain-slots (ocml::find-current-instance rt))

|#

(defun f-o (s)
  "FOR-OCML: produces a symbol usable by ocml functions, from a string"
  (cond ((stringp s) (if (and (>= (length s) (length "ocml::")) (string= (subseq s 0 6) "ocml::")) 
                         (read-from-string s) 
                       (read-from-string (concatenate 'string "ocml::" s))))
        ((symbolp s) s)
        (t (error "only symbols or strings to generate ocml names"))))

;(f-o "toto")
;(f-o "ocml::toto")
;(f-o 'toto)
;(f-o 'ocml::toto)

(defmacro with-ocml-ctx ((&optional &key ontology class instance) &body body)
  
  `(let* ((xpm::*ocml-ctx-ontology* (cond (,ontology (f-o ,ontology))
                                  (t (ocml::name ocml::*current-ontology*))))
         (xpm::*ocml-ctx-class* (when ,class (f-o ,class)))
         (xpm::*ocml-ctx-instance* (when ,instance (f-o ,instance))))
     `(ocml::in-ontology ,*ocml-ctx-ontology*)
     ,@body))

(defmacro upd-ocml-ctx ((&optional &key ontology class instance) &body body)
  `(with-ocml-ctx (:ontology (if ,ontology ,ontology *ocml-ctx-ontology*)
                  :class (if ,class ,class *ocml-ctx-class*)
                  :instance (if ,instance ,instance *ocml-ctx-instance*))
    ,@body))

;(macroexpand '(with-ocml-ctx (:ontology 'profile :class 'address) ()))
;(with-ocml-ctx (:ontology 'profile :class 'address ) 
;  (list 'toto)
;  (upd-ocml-ctx (:instance 'toto)
;    (list 'toto))))


(defun o-instance (&optional parent name)
  (let ((res-inst nil)
        (parent (if parent parent *ocml-ctx-class*)))
    (setf res-inst (if name
        (ocml::define-domain-instance (f-o name) (f-o parent))
        ;;new-instance does not add OCML:: to the name so we do not use it
        ;(ocml::new-instance (f-o parent))))
      (ocml::define-domain-instance (f-o (GENTEMP "INSTANCE")) (f-o parent))))
   (ocml::name res-inst)))

;(setf rt (o-instance 'address 'his-domicile))
;(setf rt (o-instance 'address ))
;(ocml::find-current-instance rt)

(defun o-slot-types (slot &optional class)
  (let ((class (if class class *ocml-ctx-class*)))
    (ocml::ocml-eval-gen `(ocml::ALL-CLASS-SLOT-TYPES ,(f-o class) ,(f-o slot)))))

;(o-slot-types 'HAS-COUNTRY 'address)

(defun o-slot-type (slot &optional class)
  (let ((class (if class class *ocml-ctx-class*)))
    (first ;can a slot have more than one type? when?
     (o-slot-types slot class))))

;(o-slot-type 'HAS-COUNTRY 'address)
;(o-slot-type 'HAS-POSTAL-ADDRESS 'address)
;(with-ocml-ctx (:ontology profile :class address)
;  (o-slot-type 'HAS-COUNTRY)
;  (o-slot-type 'HAS-POSTAL-ADDRESS))

(defun o-set-slot (slot the-value &optional instance)
  (let* ((instance (if instance instance *ocml-ctx-instance*))
        (oslot (f-o slot))
        (slot-type (car (eval `(ocml::ocml-eval (ocml::ALL-CLASS-SLOT-TYPES ,*ocml-ctx-class*  ,oslot)))))
        (slot-type-supertypes (eval `(ocml::ocml-eval (ocml::ALL-superclasses ,slot-type)))))
     ;; add instead of set by alessio
     ;;vt: modified to accept multiple values
     (let ((vals (if (listp the-value) the-value (list the-value))))
       (loop for value in vals 
             do 
             (ocml::add-slot-value instance oslot 
                                   (if (or (eql 'string slot-type) (member 'string slot-type-supertypes))
                                       value
                                     (if (stringp value)
                                         (read-from-string value)
                                       value)))))))

;(setf rt (o-instance 'address ))
;(ocml::find-current-instance rt)
;(ocml::describe-instance rt)
;(o-set-slot 'has-locality "Milton Keynes" rt)
;(o-set-slot 'has-county (f-o 'county-buckinghamshire) rt)
;(o-xp "/has-county" (list rt))


(defun o-types (slot &optional class)
  (let ((class (if class (f-o class) *ocml-ctx-class*)))
    (ocml::ocml-eval-gen `(ocml::ALL-CLASS-SLOT-TYPES ,class ,(f-o slot)))))

;(o-types  "has-street-number" "address")
;(o-types "has-test-slot" "county" )

