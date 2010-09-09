

(in-package "OCML")

(setf (logical-pathname-translations "transform")
       '(("ROOT;*"        "C:\\users\\jbd2\\code\\ocml\\library\\v5-0\\domains\\profile\\transform\\")))

(load "transform:root;transform.lisp")
(load "transform:root;help-xml-gen.lisp")
;(load "transform:root;transform-genonto-xsd.lisp")

(defvar *current-class* nil
  "current class being parsed (used to construct paths for constraint retrival)")

;;(ocml::in-ontology ocml::profile)

;(defun foo ()
;  (with-ocml-thread-safety ()
;    (ocml::select-ontology 'ocml::profile)


;(princ *md-db*)
;(md-get-value '(cls-path))

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

(defun util-flatten-list (l)
  (let ((el (first l)))
    (when el
      (if (atom el) 
          (append (list el) (util-flatten-list (rest l)))
        (append (util-flatten-list el) (util-flatten-list (rest l)))))))


(defun get-used-classes-helper (root types-mdroot)
  "all classes used by root which are not basic types"
  (let ((btypes (md-ask-value 'list-nodes types-mdroot ))
        (slts (eval `(ocml-eval (ALL-CLASS-SLOTS ,root)))))
    (when slts (remove nil (mapcar #'(lambda (slt) (let ((stype (car (eval `(ocml-eval (ALL-CLASS-SLOT-TYPES ,root ,slt ))))))
                              (when (not (member stype btypes)) (append (list stype)  (get-used-classes stype)))))
            slts))))
  )

(defun get-used-classes (root)
  (util-flatten-list (get-used-classes-helper root '(transform ocml types))))

;(get-used-classes 'ADDRESSCARD)

(defun mddb-slot-get (slot key)
  (let* ((path (append (md-get-value '(cls-path)) (list *current-class* slot key)))
         (val (md-get-value path)))
    (if val
        (if (> (length val) 1)
            val
          (first val))
      )))

;(setf *current-class* 'address)
;(mddb-slot-get 'has-county 'label)
;(mddb-slot-get 'has-county 'enumeration)
;(mddb-slot-get 'has-county 'help)

(defun mddb-type-get (type)
  (let* ((path (append (md-get-value '(typ-path)) (list type))))
    (first (md-get-value path))))

;(setf *current-class* 'address)
;(mddb-slot-get 'has-county 'optional)
;(mddb-slot-get 'has-county 'enumeration)
;(mddb-type-get 'domicile)
;(mddb-type-get 'toto)

(defun util-remove-ontology-prefix-from-slot (s)
  (let* ((name (format nil "~a" s))
         (beg (subseq name 0 4)))
    (cond ((equal beg "HAS-") (subseq name 4))
          (t name))))

(defun util-pretty-print-ontology-name (s)
  (string-capitalize (substitute  #\Space #\- s)))

;(util-remove-ontology-prefix-from-slot 'has-county)

(defun gen-fd (s stype)
  "field from slot and type"
  (let ((enumeration (mddb-slot-get s 'enumeration))
        (label (mddb-slot-get s 'label))
        (optional (mddb-slot-get s 'optional))
        (help (mddb-slot-get s 'help))
        (value (mddb-slot-get s 'value)))
    (xml-with "fd" "field" 0
      (list (m-a "id" s)
            (m-a "required" (if optional "false" "true")))
      (xml-with "fd" "label" 2 () (if label label (util-pretty-print-ontology-name (util-remove-ontology-prefix-from-slot s))))
      (xml-with "fd" "datatype" 2 (list (m-a "base" (mddb-type-get stype))) ())
      (when enumeration
        (xml-with "fd" "selection-list" 2 ()
           (util-list-to-string (mapcar #'(lambda (e) (xml-with "fd" "item" 4 (list (m-a "value" (format nil "~a" e))) ())) enumeration))))
      (when help
        (xml-with "fd" "help" 2 () help ))
     )
   )
)


;(setf *current-class* 'address)
;(mddb-slot-get 'has-county 'enumeration)
;(princ (gen-fd 'has-county 'string))

(defun xs-cform-definition-for-admin-goal (classes &optional admg)
  (setf *current-class* nil)
  (concatenate 'string
    (format nil "<!-- Generated from OCML classes ~A -->~%" classes)
    (xml-with "fd" "form" 0 
      (list (m-a "xmlns:fd" "http://apache.org/cocoon/forms/1.0#definition")
            (m-a "xmlns:i18n" "http://apache.org/cocoon/i18n/2.1"))
      (xml-with "fd" "widgets" 0
        ()
        (util-list-to-string           
         (mapcar #'(lambda (cur-class) 
                     (mapcar #'(lambda (cls) 
                                 (mapcar #'(lambda (s) 
                                             (setf *current-class* cls)
                                             (princ cls)
                                             (let ((stype (car (eval `(ocml::ocml-eval (ALL-CLASS-SLOT-TYPES ,cls ,s )))))
                                                   (cl-list (eval `(ocml::ocml-eval (get-admg-elements ,admg)))))
                                               (if (not admg)
                                                   (when (mddb-type-get stype) (gen-fd s stype))
                                                 (when (and (mddb-type-get stype) (util-filter #'(lambda (tup) (and (eql cls (first tup)) (eql s (second tup)))) cl-list))
                                                (gen-fd s stype))
                                               )))
                                         (get-local-slots cls)))
                             (cons cur-class (get-used-classes cur-class))))
                 classes))))))

(defun filter-admg (admg)
  "gets rid of non profile elements from admg"
  (util-filter #'(lambda (tup) (NOT ;we don't want fields not in profile
                                                     (or 
                                                      (member 'ADMG tup ) 
                                                      (member 'TRANSACTION-ID tup ))) ) 
                                               (eval `(ocml::ocml-eval (get-admg-elements2 ,admg)))))

;(filter-admg 'life-event-change-address)
        
(defun xs-cform-definition-for-admin-goal2 (admg)
  "modified since admg are wsmo goals"
  (setf *current-class* nil)
  (concatenate 'string
    (format nil "<!-- Generated from ADMG ~A -->~%" admg)
    (xml-with "fd" "form" 0 
      (list (m-a "xmlns:fd" "http://apache.org/cocoon/forms/1.0#definition")
            (m-a "xmlns:i18n" "http://apache.org/cocoon/i18n/2.1"))
      (xml-with "fd" "widgets" 0
        ()
        (util-list-to-string
         ;cl-list will contain all our goal profile related input roles
         (append
         (let ((cl-list (filter-admg admg)))
           (mapcar #'(lambda (cur-tuple) 
                       ;(princ (length cl-list))
                       (let* ((cls (first cur-tuple))
                              (s (second cur-tuple))
                              (stype (car (eval `(ocml::ocml-eval (ALL-CLASS-SLOT-TYPES ,cls ,s ))))))
                         (setf *current-class* cls)
                         ;(princ *current-class*)
                         (gen-fd s stype)
))
                   cl-list))
         (list (xml-with "fd" "messages" 12 (list (m-a "id" "COLLECTS-FOR")) 
                         (xml-with "fd" "label" 12 nil "Collects data for"))
               (xml-with "fd" "messages" 12 (list (m-a "id" "ERROR-MESSAGES")) 
                         (xml-with "fd" "label" 12 nil "Error"))
               (xml-with "fd" "action" 12 (list (m-a "id" "NEXT-AGENCY") (m-a "action-command" "next-agency-action"))
                         (xml-with "fd" "label" 12 nil "Next Agency")
                         (xml-with "fd" "hint" 12 nil "Use information for next agency without submitting to this one."))
               (xml-with "fd" "action" 12 (list (m-a "id" "CANCEL-ALL") (m-a "action-command" "cancel-all-action"))
                         (xml-with "fd" "label" 12 nil "Cancel Process")
                         (xml-with "fd" "hint" 12 nil "Cancel the whole process (note: not for agencies already notified)")))
         )
         )))))

;(xs-cform-definition-for-admin-goal2 'life-event-change-address-profile)
;(princ (xs-cform-definition-for-admin-goal2 'life-event-change-address-profile))

(defun filter-class (l clsname)
  (util-filter #'(lambda (l) (eql (first l) clsname)) 
                l))

;(filter-class (ocml-eval (get-admg-elements admg-change-address profile (all-class-slots profile))) 'address)

(defun gen-template (s stype ind &optional admg)
  "field from slot and type"
  (if (mddb-type-get stype)
      (xml-with "ft" "widget" ind
        (list (m-a "id" s))
        nil)
    (xs-cform-template-group stype (+ ind 2) admg))
      ;(xml-with "fd" "label" 2 () (if label label (util-remove-ontology-prefix-from-slot s)))
      ;(xml-with "fd" "datatype" 2 (list (m-a "base" (mddb-type-get stype))) ())
      ;(when enumeration
      ;  (xml-with "fd" "selection-list" 2 ()
      ;     (util-list-to-string (mapcar #'(lambda (e) (xml-with "fd" "item" 4 (list (m-a "value" (format nil "~a" e))) ())) enumeration))))
     ;)   
)

(defun xs-cform-template-group (cur-class ind &optional admg)
  (xml-with "fi" "group" ind nil 
            (concatenate 'string
                         (xml-with "fi" "styling" (+ ind 2) (list (m-a "type" "fieldset") (m-a "layout" "columns")) nil)
                         (xml-with "fi" "label" (+ ind 2) nil (format nil "~a" cur-class))
                         (xml-with "fi" "items" (+ ind 2) nil 
                                   (util-list-to-string 
                                    (mapcar #'(lambda (s) 
                                                (setf *current-class* cur-class)
                                                (let ((stype (car (eval `(ocml::ocml-eval (ALL-CLASS-SLOT-TYPES ,cur-class ,s ))))))
                                                  (if (not admg)
                                                      (gen-template s stype (+ ind 2))
                                                    (let ((cl-list (eval `(ocml::ocml-eval (get-admg-elements ,admg )))))
                                                      (when (util-filter #'(lambda (tup) (and (eql cur-class (first tup)) (eql s (second tup)))) cl-list)
                                                        (gen-template s stype (+ ind 2) admg)))
                                                      )
                                                  )
                                                )
                                            (get-local-slots cur-class))
                                    )
                                   )
                         )
            )
  )


(defun xs-cform-template-for-admin-goal (classes &optional admg)
  (setf *current-class* nil)
  (concatenate 'string
    (format nil "<!-- Generated from OCML classes ~A -->~%" classes)
    (xml-with nil "page" 0 
      (list (m-a "xmlns:ft" "http://apache.org/cocoon/forms/1.0#template")
            (m-a "xmlns:fi" "http://apache.org/cocoon/forms/1.0#instance"))
      
       (xml-with nil "title" 0 () "Administrative goal")
       (xml-with nil "content" 0
        ()
        (xml-with "ft" "form-template" 2
          (list (m-a "action" "#{$continuation/id}.continue")
                (m-a "method" "POST"))
          (xml-with nil "table" 4
            (list (m-a "align" "center"))
            (xml-with nil "tr" 6 ()
              (xml-with nil "td" 8 ()
                (util-list-to-string 
                  (append
              
                       (mapcar #'(lambda (c) 
                                   (if (not admg)
                                       (xs-cform-template-group c 10 admg)
                                     (let ((cl-list (eval `(ocml::ocml-eval (get-admg-elements ,admg )))))
                                       (when (util-filter #'(lambda (tup) (eql c (first tup))) cl-list)
                                         (xs-cform-template-group c 10 admg))
                                       )
                                     )
                                   )
                               classes)
                       (list "<input type=\"submit\"/>")))))
))))))

;(princ (xs-cform-template-for-admin-goal 'admg-change-address))
;(princ (xs-cform-template-for-admin-goal '(addresscard)))
;(princ (xs-cform-template-for-admin-goal '(addresscard) 'admg-change-address))

(defun gen-template2 (s stype ind &optional admg)
  "field from slot and type"
  ;(if (mddb-type-get stype)
      (xml-with "ft" "widget" ind
        (list (m-a "id" s))
        nil)
    ;(xs-cform-template-group stype (+ ind 2) admg))
      ;(xml-with "fd" "label" 2 () (if label label (util-remove-ontology-prefix-from-slot s)))
      ;(xml-with "fd" "datatype" 2 (list (m-a "base" (mddb-type-get stype))) ())
      ;(when enumeration
      ;  (xml-with "fd" "selection-list" 2 ()
      ;     (util-list-to-string (mapcar #'(lambda (e) (xml-with "fd" "item" 4 (list (m-a "value" (format nil "~a" e))) ())) enumeration))))
     ;)   
)

(defun xs-cform-template-group2 (cur-class ind &optional admg)
  "modified sincs WSMO admgs"
  (xml-with "fi" "group" ind nil 
            (concatenate 'string
                         (xml-with "fi" "styling" (+ ind 2) (list (m-a "type" "fieldset") (m-a "layout" "columns")) nil)
                         (xml-with "fi" "label" (+ ind 2) nil (format nil "~a" cur-class))
                         (xml-with "fi" "items" (+ ind 2) nil 
                                   (util-list-to-string 
                                    (mapcar #'(lambda (s) 
                                                (setf *current-class* cur-class)
                                                (let ((stype (car (eval `(ocml::ocml-eval (ALL-CLASS-SLOT-TYPES ,cur-class ,s ))))))
                                                  (if (not admg)
                                                      (gen-template s stype (+ ind 2))
                                                    (let ((cl-list (eval `(ocml::ocml-eval (get-admg-elements ,admg )))))
                                                      (when (util-filter #'(lambda (tup) (and (eql cur-class (first tup)) (eql s (second tup)))) cl-list)
                                                        (gen-template s stype (+ ind 2) admg)))
                                                      )
                                                  )
                                                )
                                            (get-local-slots cur-class))
                                    )
                                   )
                         )
            )
  )

(defun pretty-print-goal (admg)
  (string-upcase (substitute  #\Space #\- (subseq (format nil "~A" admg) (length "LIFE-EVENT-")))))

;(pretty-print-goal 'life-event-change-address-profile)

(defun xs-cform-template-for-admin-goal2 (admg)
  "modified since admin goal is wsmo"
  (setf *current-class* nil)
  (concatenate 'string
    (format nil "<!-- Generated from admg ~A -->~%" admg)
    (xml-with nil "page" 0 
      (list (m-a "xmlns:ft" "http://apache.org/cocoon/forms/1.0#template")
            (m-a "xmlns:fi" "http://apache.org/cocoon/forms/1.0#instance"))
      
       ;(xml-with nil "title" 0 () "Administrative goal")
       (xml-with nil "content" 0
        ()
        (xml-with "ft" "form-template" 2
          (list (m-a "action" "#{$continuation/id}.continue")
                (m-a "method" "POST"))
          (xml-with nil "table" 4
            (list (m-a "align" "center"))
            (xml-with nil "tr" 6 ()
              (xml-with nil "td" 8 ()
                (util-list-to-string 
                  (append
                   ;(list (xml-with nil "title" 0 () "Administrative goal"))
                   (list (xml-with nil "title" 0 () (pretty-print-goal admg)))
                   (list (xml-with "ft" "widget" 12 (list (m-a "id" "COLLECTS-FOR")) nil))
                   (list (xml-with "ft" "widget" 12 (list (m-a "id" "ERROR-MESSAGES")) nil))
                   (list (let ((cl-list (filter-admg admg)))
                     (xml-with "fi" "group" 10 nil 
                               (concatenate 'string
                                            (xml-with "fi" "styling" 12 (list (m-a "type" "fieldset") (m-a "layout" "columns")) nil)
                                            (xml-with "fi" "label" 12 nil (pretty-print-goal admg))
                                            (xml-with "fi" "items" 12 nil 
                                                      (util-list-to-string
                                                       (append
                                                        ;(list (xml-with "ft" "widget" 12 (list (m-a "id" "ERROR-MESSAGES")) nil))
                                                        (mapcar #'(lambda (cur-tuple) 
                                                                   (let*  ((cls (first cur-tuple))
                                                                           (s (second cur-tuple))
                                                                           (stype (car (eval `(ocml::ocml-eval (ALL-CLASS-SLOT-TYPES ,cls ,s ))))))
                                   ;(if (not admg)
                                   ;    (xs-cform-template-group (first tup) 10 admg)
                                   ;  (let ((cl-list (eval `(ocml::ocml-eval (get-admg-elements ,admg )))))
                                   ;    (when (util-filter #'(lambda (tup) (eql c (first tup))) cl-list)
                                   ;      (xs-cform-template-group c 10 admg))
                                   ;    )
                                   ;  )
                                                                     (gen-template2 s stype 14 admg)
                                                                   ))
                                                               cl-list)))
                                                      )))))
                                            (list "<input type=\"submit\" value=\"Submit Data\"/>")
                                            (list (xml-with "ft" "widget" 12 (list (m-a "id" "NEXT-AGENCY")) nil)
                                                  (xml-with "ft" "widget" 12 (list (m-a "id" "CANCEL-ALL")) nil))
                                            ;(xml-with "ft" "widget" 12 (list (m-a "id" "cancel-process")) nil)
                                            ))))))))))



;(princ (xs-cform-template-for-admin-goal2 'life-event-change-address-profile))

;NESTED FUNCTIONS
;;(defun toto (a b)
;;  (labels ((add (x y) (+ x y)))
;;    (add a b)))

;;(toto 10 24)


;(in-ontology profile)


;(defparameter *addresscard-def* (xs-cform-definition-for-admin-goal '(addresscard)))
;(defparameter *addresscard-tmp* (xs-cform-template-for-admin-goal '(addresscard)))

;(princ *addresscard-def*)
;(princ *addresscard-tmp*)

