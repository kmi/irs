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

(ocml::describe-instance 'ocml::your-domicile)

(setf *tree* (list 'ocml::your-domicile))

(setf rt (first *tree*))

(ocml::slot-values 'ocml::your-domicile 'ocml::has-postal-code)
(ocml::clear-slot-values 'ocml::your-domicile 'ocml::has-postal-code)
(ocml::add-slot-value 'ocml::your-domicile 'ocml::has-postal-code "MK6 3PA")

(ocml::get-slot-values (ocml::find-current-instance rt) 'ocml::has-postal-code)
(ocml::get-slot-values (ocml::find-current-instance rt) 'ocml::has-county)

(ocml::domain-slots (ocml::find-current-instance rt))

|#

(defun %ocml-get-slots-named (condition cl)
    (setf cl (if (symbolp cl) (ocml::find-current-instance cl) cl))
    (flet ((inner-condition (el condition)
             (cond 
              ((string= condition "*") t)
              (t (eql el (read-from-string (concatenate 'string "ocml::" condition)))))))
      (loop for val in ;outer loop to remove nils and lists
            (loop for slt in (ocml::domain-slots cl) 
                  when (inner-condition slt condition) collect ;if the slot name corresponds to the condition
                  (loop for val in (ocml::get-slot-values cl slt)
                        when val
                        collect val))
            ;;vt 11/10/2006 to get all values when multiple
            ;;when val collect (first val))))
            when val collect val)))

;PBL WITH THE TRAILING OCML:: SHOULDN'T BE DEFINED IN THE CLASS (EX: COUNTY-BUCKING...) ENLEVER LE OCML DE DEBUT!!!!!

#|
(%ocml-get-slots-named "has-country" rt)
(ocml::in-ontology ocml::swift-services-datatypes)
(%ocml-get-slots-named "has-address-key" (f-o 'instance2284))
(%ocml-get-slots-named "has-county" rt)
(%ocml-get-slots-named "HAS-LOCALITY" rt)
|#

(defun ocml-get-slots-named (name context)
  (if (listp context)
      (let ((res ())) (dolist (el context res) (setf res (append res (%ocml-get-slots-named name el)))))
    (%ocml-get-slots-named name context)))

#|
(ocml-get-slots-named "has-country" rt)
(ocml-get-slots-named "has-county" (list rt rt))
|#

(defun %ocml-get-attributes-named (attrtest elem)
  (ocml-get-slots-named attrtest elem))

(defun ocml-get-attributes-named (attrtest context)
  (if (listp context)
      (let ((res ())) (dolist (el context res) (setf res (append res (%ocml-get-attributes-named attrtest el)))))
    (%ocml-get-attributes-named attrtest context)))

(defun %ocml-get-text-values (cl)
  (let ((cl (ocml::find-current-instance cl)))
    (loop for val in ;outer loop to remove nils and lists
          (loop for slt in (ocml::domain-slots cl) 
                collect
                (loop for val in (ocml::get-slot-values cl slt)
                      when (not (ocml::find-current-instance val)) ;text or integer value
                      collect val))
          when val collect  (first val))))

#|
(%ocml-get-text-values  rt)
|#

(defun ocml-get-text-values (context)
  (if (listp context)
      (let ((res ())) (dolist (el context res) (setf res (append res (%ocml-get-text-values el)))))
    (%ocml-get-text-values context)))
#|
(ocml-get-text-values  rt)
(ocml-get-text-values  (list rt rt))
|#

(defun ocml-APPLY-LEADING-/ (sequence)
  ;a OCML root is one classe so the sequence is supposed to have a single element
  (first sequence))

(defun ocml-APPLY-CHILD (nodetest sequence)
  (cond
   ;;;the text() operator just extracts the text from the list
   ((string= nodetest "text()") (first sequence))
   (t (ocml-get-slots-named nodetest sequence))))

(defun ocml-APPLY-ATTRIBUTE (attrtest sequence)
  (declare (ignore attrtest sequence))
  (error "no attributes in OCML!"))

(defun o-xp  (xpath &optional (sequence nil))
  (let ((*APPLY-LEADING-/* 'ocml-APPLY-LEADING-/)
        (*APPLY-CHILD* 'ocml-APPLY-CHILD)
        (*APPLY-ATTRIBUTE* 'ocml-APPLY-ATTRIBUTE))
    (let ((what (if sequence sequence *context*)))      
      (xp xpath  (if (consp what)  what (list what))))))

#|


(o-xp "/*"  (list rt))
(o-xp "/has-country"  (list rt))
(o-xp "/has-country/*"  (list rt))
(o-xp "/has-locality" (list rt))
(o-xp "/has-postal-code/text()" (list rt))

(let ((*context* (list rt)))
  (print (o-xp "/has-postal-code")))

|#

