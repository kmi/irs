
(setf (logical-pathname-translations "transform")
       '(("ROOT;*"        "C:\\users\\jbd2\\code\\ocml\\library\\v5-0\\domains\\profile\\transform\\")))

(load "transform:root;transform.lisp")
(load "transform:root;help-xml-gen.lisp")


(in-package "OCML")
;(in-package cl-user)


;(setf *print-escape* t)
;(setf *print-case* :downcase)

(defun xs-maxLength-cstr (cls slot ind)
  (let ((val (first (md-get-par-value `'(transform ocml-xsd struc classes ,cls ,slot maxLength)))))
    (when val (xml-with "xs" "maxLength" ind
                        (list (m-a "value" val))))))

(defun xs-maxInclusive-cstr (cls slot ind)
  (let ((val (first (md-get-par-value `'(transform ocml-xsd struc classes ,cls ,slot maxInclusive)))))
    (when val (xml-with "xs" "maxInclusive" ind
                        (list (m-a "value" val))))))

;(xs-maxLength-cstr 'profile-fam 'firstname 10)
;(xs-maxLength-cstr 'profile-fam 'sex 10)

(defun xs-enumeration-cstr (cls slot ind)
  (let ((enum (md-get-par-value `'(transform ocml-xsd struc classes ,cls ,slot enumeration))))
    (when enum (enum-helper enum ind))))

;(md-get-par-value `'(transform ocml-xsd struc classes 'PROFILE-FAM 'SEX enumeration))

;(xs-enumeration-cstr 'PROFILE-FAM 'SEX 0)

(defun get-data-adaptation (cls slot types ind)
  (util-list-to-string (mapcar #'(lambda (type)    (case type
                                ((maxLength) (xs-maxlength-cstr cls slot ind))
                                ((enumeration) (xs-enumeration-cstr cls slot ind))
                                ((maxInclusive) (xs-maxInclusive-cstr cls slot ind))
                                (otherwise nil)))
                          types))
  )

;(get-data-adaptation 'profile-fam 'firstname '(maxLength enumeration) 0)
;(get-data-adaptation 'profile-fam 'sex '(maxLength enumeration) 10)
;(get-data-adaptation 'exchange-rate-goal 'has_amount '(maxLength maxInclusive enumeration) 10)

(defvar *current-class* nil
  "current class being parsed (used to construct paths for constraint retrival)")

(defun xs-simpletype (name base ind)
  "<!ELEMENT xs:simpleType (xs:restriction?)>"
  (xml-with "xs" "simpleType" ind nil
            (xml-with "xs" "restriction" (+ ind 2) 
                      (list (m-a "base" base))
                      
                      (get-data-adaptation *current-class*  name '(maxlength maxInclusive enumeration) (+ ind 4))
                      )))

(defun xs-simple-element (name base ind)
  (xml-with "xs" "element" ind 
            (list (m-a "name" name))
            (xs-simpletype name base (+ 2 ind))))

;(xs-simple-element "toto" "xs:string" 2)

(defun xs-cardinality (name)
  "checks the original class cardinality cstr and express them in xsd"
  (eval (ocml::ocml-eval (ocml::all-class-slots name)))
  )

;(eval (ocml::ocml-eval (ocml::the-class-min-slot-cardinality 'profile-fam 'altnames)))
;(eval (ocml::ocml-eval (ocml::the-class-max-slot-cardinality 'profile-fam 'altnames)))

(defun xs-typed-element (name type ind)
  "<!ELEMENT xs:element (xs:annotation|xs:complexType|xs:simpleType)*>"
  ;(let ((sup (first (eval `(ocml-eval (ALL-SUPERCLASSES ,type))))))
    ;(if sup
        ;(xs-typed-element name sup ind)
        (xml-with "xs" "element" ind 
            ;(get-attr-list-for-el name)
                  (list 
                   (m-a "name" name)
                   (m-a "type" type))));))

;(xs-typed-element "toto" "Relative" 2)

(defun get-local-slots (cls)
  "returns the slots that belong only to this class, not to its superclass"
  (let ((sup (first (eval `(ocml::ocml-eval (ocml::ALL-SUPERCLASSES ,cls))))))
    (if sup
        (let ((locslots (eval `(ocml::ocml-eval (ocml::ALL-CLASS-SLOTS ,cls))))
              (supslots (eval `(ocml::ocml-eval (ocml::ALL-CLASS-SLOTS ,sup)))))
          (util-filter #'(lambda (x) (not (member x supslots))) locslots))
      (eval `(ocml::ocml-eval (ocml::ALL-CLASS-SLOTS ,cls))))
    )
  )

;(in-ontology profile-fam)
;(get-local-slots 'profile-fam)
;(get-local-slots 'RelativeProfile)

;(in-ontology wsmo-exchange-rate)
;(get-local-slots 'goal)

(defun xs-sequence (cls ind &optional (slots nil))
  "<!ELEMENT xs:sequence (xs:element+)>"
  (let ((locslots (if (not slots) (get-local-slots cls) slots)))
    ;(princ locslots)
    (xml-with "xs" "sequence" ind 
              nil
              (util-list-to-string 
               (mapcar 
                #'(lambda (slt)
                    "distinguished between inlied simpletypes and typed elements"
                    (let ((stype (car (eval `(ocml::ocml-eval (ocml::ALL-CLASS-SLOT-TYPES ,cls ,slt ))))))
                      (if (member stype (md-ask-value 'list-nodes '(transform ocml-xsd struc types)))
                          (xs-simple-element slt (first (md-get-par-value `'(transform ocml-xsd struc types ,stype))) (+ 2 ind));it is a simpleType
                        (xs-typed-element slt stype (+ 2 ind)))));it is a complexType
                locslots)))))

;(xs-sequence 'profile-fam 0) 
;(xs-sequence 'profile-fam 0 '(birth sex))
;(xs-sequence 'Relativeprofile 0)


(defun xs-complextype (name ind)
  "<!ELEMENT xs:complexType (xs:sequence,xs:attribute*)>"
  (progn
    (setf *current-class* name)
    (xml-with "xs" "complexType" ind 
              (list (m-a "name" name)) 
              (xs-sequence name (+ ind 2))
              )
  ))

;(xs-complextype 'profile-fam 0)
;(xs-complextype 'relativeprofile 0)

(defun util-flatten-list (l)
  (let ((el (first l)))
    (when el
      (if (atom el) 
          (append (list el) (util-flatten-list (rest l)))
        (append (util-flatten-list el) (util-flatten-list (rest l)))))))

;(util-flatten-list '(1 2 (3 (4) 5) (((6)))))

(defun get-used-classes-helper (root types-mdroot)
  "all classes used by root which are not basic types"
  (let ((btypes (md-ask-value 'list-nodes types-mdroot ))
        (slts (eval `(ocml-eval (ALL-CLASS-SLOTS ,root)))))
    (when slts (remove nil (mapcar #'(lambda (slt) (let ((stype (car (eval `(ocml-eval (ALL-CLASS-SLOT-TYPES ,root ,slt ))))))
                              (when (not (member stype btypes)) (append (list stype)  (get-used-classes stype)))))
            slts))))
  )

(defun get-used-classes (root)
  (util-flatten-list (get-used-classes-helper root '(transform ocml-xsd struc types))))


;(md-ask-value 'list-nodes '(transform ocml-xsd struc types))
;(get-used-classes 'profile-fam)
;(get-used-classes 'RelativeProfile)
;(get-used-classes 'birthinfo)
;(get-used-classes 'altnamesc)

(defun xs-schema (root)
  (concatenate 'string
                (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
                (format nil "<!-- Generated from OCML class ~A -->~%" root)
                (xml-with "xs" "schema" 0 
                          (list (m-a "elementFormDefault" "qualified")
                                (m-a "attributeFormDefault" "unqualified")
                                (m-a "xmlns:xs" "http://www.w3.org/2001/XMLSchema"))
                          (util-list-to-string
                           (mapcar #'(lambda (cls) (when (get-local-slots cls) (xs-complextype cls 0 )))
                                   (cons root (get-used-classes root)))))
                          ))


;(with-ocml-thread-safety ()
;(ocml::select-ontology 'ocml::profile)
  
  
  (defparameter *md-db*
    '((transform ((ocml-xsd ((struc ((classes ((profile-fam
                                                ((Firstname 
                                                  ((maxLength ((120)))
                                                   (minLength ((10)))))
                                                 (Sex 
                                                  ((enumeration (((F |Female|)(M |Male|))))))))))
                                     (types ((string (("xs:string")))
                                             (integer (("xs:integer")))))))))))))
  
(in-ontology profile-fam)
;(princ (xs-schema 'profile-fam))

