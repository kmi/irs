
(eval-when (compile load eval)
  (defpackage :irs.loli
    (:use :common-lisp :xml-sax)
  ;(:export :with-html-output)
    )
)

(in-package :irs.loli)

(setf doc (xml-sax::simple-xml-parse
 "<list><person><name>john</name><works-at>kmi</works-at></person><organistation><name>Knowledge
Media Institute</name><url>kmi.open.ac.uk</url></organisation></list>"))

(setf doc (xml-sax::simple-xml-parse
 "<instance ontology=\"profile\" class=\"address\" name=\"my-domicile\"/>"))

(setf doc (xml-sax::simple-xml-parse
 "<instance ontology=\"profile\" class=\"address\" name=\"my-domicile\">
<def-instance ontology=\"profile\" class=\"address\" name=\"my-domicile\"/>
	<slot name=\"has-county\"><def-instance ontology=\"profile\" class=\"county\" name=\"county- buckinghamshire\"/></slot>
<slot name=\"has-postal-code\">MK6 3PA</slot>
<slot name=\"has-locality\">Milton Keynes</slot>
<slot name=\"has-postal-address\">7 Woodley</slot>
<slot name=\"has-country\"><instance ontology=\"profile\" class=\"country\" name=\"country-UK\"/></slot>
</instance>
"))

(xml-sax::print-as-tree doc)

(print "tot\"\"o")

(pprint (format nil "toto\"\""))

(

(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than amount."))

(defmethod withdraw((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))

;;(defclass bank-account 


(defun iota (n) (loop for i from 1 to n collect i))       ;helper
(destructuring-bind ((a &optional (b 'bee)) one two three)
     `((alpha 12)  ,@(iota 3))
   (list a b three two one)) =>  (ALPHA BEE 3 2 1)


(defun parse-cons-form (sexp)
  (if (consp (first sexp))
    (parse-explicit-attributes-sexp sexp)
    (parse-implicit-attributes-sexp sexp)))

(defun parse-explicit-attributes-sexp (sexp)
  (destructuring-bind ((tag &rest attributes) &body body) sexp
    (values tag attributes body)))

(defun parse-implicit-attributes-sexp (sexp)
  (loop with tag = (first sexp)
     for rest on (rest sexp) by #'cddr
     while (and (keywordp (first rest)) (second rest))
     when (second rest)
       collect (first rest) into attributes and
       collect (second rest) into attributes
     end
     finally (return (values tag attributes rest))))

(parse-cons-form '(:p :id "x" :style "foo" "Foo"))
(parse-cons-form '((:p :id "x" :style "foo") "Foo"))

;(in-package :xml-utils)

;(xml-utils:

;;(defVar *dm* (parse-document #4P"xml:documentation;howto;howto.xml"))
;(defVar *dm* (parse-document #4P"xml:documentation;howto;howto.xml"))
