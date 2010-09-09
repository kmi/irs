;;; Copyright © 2008 The Open University

(in-package #:ocml)

(in-ontology xml)

;;; We (attempt to) model only well-formed XML, without validation.

(def-class #_Document ()
  ((#_rootElement :type #_Element)))

(def-class #_Element ()
  ((#_tag :type string)
   (#_attributes :type #_Attributes)
   (#_contents :type #_Contents)))

(def-class #_Attributes () ?attributes
  :iff-def (and (listp ?attributes)
                (every ?attributes #_Attribute)))

(def-class #_Attribute ()
  ((#_name :type string)
   (#_value :type string)))

(def-class #_Contents () ?contents
  :iff-def (and (listp ?contents)
                (every ?contents (or #_Element #_Text))))

(def-class #_Text ()
  ((#_value :type string)))

;;; {{{ Constructors
(defun #_new (type value)
  (let ((instance (new-instance type `((#_value ,value)))))
    (name instance)))

(def-function #_Document (root)
    :lisp-fun #'(lambda (root)
                  (let ((instance (new-instance '#_Document
                                                `((#_rootElement ,root)))))
                    (name instance))))

(def-function #_Text (text)
    :lisp-fun #'(lambda (text)
                  (#_new '#_Text text)))

(def-function #_Element (tag attributes &rest contents)
    :lisp-fun #'(lambda (tag attributes &rest contents)
                  (let ((instance (new-instance '#_Element
                                                `((#_tag ,tag)
                                                  (#_attributes ,attributes)
                                                  (#_contents ,contents)))))
                    (name instance))))

(def-function #_ElementList (tag attributes contents)
    :lisp-fun #'(lambda (tag attributes contents)
                  (let ((instance (new-instance '#_Element
                                                `((#_tag ,tag)
                                                  (#_attributes ,attributes)
                                                  (#_contents ,contents)))))
                    (name instance))))

;;; }}}
