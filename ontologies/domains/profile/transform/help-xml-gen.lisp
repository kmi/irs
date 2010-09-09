
(in-package :ocml)

;;;;IMPORTANT, JOHN'S SOLUTION FOR MAPPING
;;;; (def-rule family-name-mapping-rule 
;;;;   ((has-family-name ?inst ?x) if 
;;;;    (haslastname ?inst ?x)))

;;;; (def-rule full-name-mapping-rule 
;;;;   ((has-full-name ?inst ?full-name) if 
;;;;    (haslastname ?inst ?x)
;;;;    (hasfirstname ?inst ?y)
;;;;    (= ?full-name))


(defstruct (attr (:conc-name a-)
                 (:print-function print-attr))
  "represents a xml attribute with name and value"
  n
  v)

(defun print-attr (a stream depth)
  "printing function of the attribute structure"
  (format stream " ~a=\"~a\" " (a-n a) (a-v a)))

;(princ (make-attr :n 'toto :v 'tata))

(defmacro m-a (name value)
  "macro for easy attribute creation"
  `(make-attr :n ,name :v ,value))

;(princ (m-a "toto" "tata"))

(defun attr-list-to-string (a-lst)
  "generates a xml suitable string from a list of attributes structures"
  (reduce #'(lambda (a s) (concatenate 'string (print-attr a nil 0)   s))
          a-lst
          :from-end t
          :initial-value ""))
  
(attr-list-to-string (list 
                      (m-a "titi" "tutu")
                      (m-a "toto" "tata")))

(defun util-list-flatten (a-list)
  "flattens a list of nested lists"
  (if (not a-list) 
      ()
    (let ((f (first a-list))
          (r (rest a-list)))
      (if (listp f) 
          (append (util-list-flatten f) (util-list-flatten r))
        (cons f (util-list-flatten r))))))

;(util-list-flatten '((("b") "a") ((("c"))"d")))
;(util-list-flatten '(1 2))

(defun util-filter (filter-func list-to-filter)
  (remove nil (mapcar #'(lambda (x) (when (apply filter-func (list x)) x)) list-to-filter)))

;(util-filter #'(lambda (x) (> x 2)) '(3 1 4 0))

(defun util-list-to-string (a-lst)
  "general utility function, concatenates the string elements of a list after flatening it"
  (let ((f-lst (util-list-flatten a-lst)))
    (reduce #'(lambda (s1 s2) (concatenate 'string s1  s2)) 
            f-lst
            :from-end t
            :initial-value "")))

;(util-list-to-string '("a" ("b" (("c"))) "d"))


(defstruct el-info
  "list of restriction strings"
  attr-list
  restriction)

(el-info-restriction (make-el-info :restriction t))

(defun util-spaces-s (n)
  (make-string n :initial-element (coerce " " 'character)))

(defun indent-add (ind s)
  "indents a string with ind spaces"
  (concatenate 'string (util-spaces-s ind) s))

(defun indent-princ (ind s)
  "prints a string with added indentation"
  (princ (indent-add ind s)))

;(indent-princ 10 "toto")

(defun enum-helper (enum-list ind)
  "builds a list of enumeration strings from list of values or tuples value annotation"
  (util-list-to-string (mapcar #'(lambda (val)
                                   (if (listp val) ;we have a value with annotation
                                       (concatenate 'string
                                                    (indent-add ind (format nil "<xs:enumeration value=\"~A\">~%" (car val)))
                                                    (indent-add (+ ind 2) (format nil "<xs:annotation>~%" ))
                                                    (indent-add (+ ind 4) (format nil "<xs:documentation>~A</xs:documentation>~%" (cadr val)))
                                                    (indent-add (+ ind 2) (format nil "</xs:annotation>~%" ))
                                                    (indent-add ind (format nil  "</xs:enumeration>~%"))
                                                    )
                                     (indent-add ind (format nil "<xs:enumeration value=\"~A\"/>~%" val))
                                     ))
                               enum-list)))

;(enum-helper '(FR DE EN (UK Angleterre)) 10)



  
  
(defmacro xml-with (ns tag i attrlist &rest body)
  "xml tag generation with nesting"
  `(concatenate 'string
                (indent-add ,i (if ',ns (format nil "<~A:~A" ',ns ',tag) (format nil "<~A" ',tag)))
                (attr-list-to-string ,attrlist)
                (if (not (eql ',@body nil))
                    (concatenate 'string
                                 (format nil ">")
                                 ,@body
                                 (if ',ns (format nil "</~A:~A>" ',ns ',tag) (format nil "</~A>" ',tag)))
                  (format nil "/>"))))

;(xml-with "xs" "testel" 0 nil)
;(xml-with "xs" "testel" 0 nil "toto")
;(xml-with "xs" "element" 
;          0 nil (xml-with "xs" "restriction" 3 (list
;                                                (m-a "base" "xs:string")) "toto" ))
;(xml-with "xs" "element" 
;          0 nil (xml-with "xs" "restriction" 3 (list
;                                                (m-a "base" "xs:string")) "toto" ))
