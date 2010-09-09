(in-package :kmi.vt.xpath-mini)

(defun %xml-get-children-named (condition elem)
  (loop for child in (children elem)
     when (and (dom:element-p child)
               (or (string= condition "*")
                   (string= (dom:local-name child) condition)))
     collect child))

(defun xml-get-children-named (name context)
  (if (listp context)
      (let ((res ())) (dolist (el context res) (setf res (append res (%xml-get-children-named name el)))))
    (%xml-get-children-named name context)))

(defun %xml-get-attributes-named (attrtest elem)
  (loop for attr in (dom:attributes elem)
     ;; XXX Not sure the name getting of the attribute will work here.
     when (string= (dom:local-name attr) attrtest)
     collect attr))

(defun xml-get-attributes-named (attrtest context)
  (if (listp context)
      (let ((res ())) (dolist (el context res) (setf res (append res (%xml-get-attributes-named attrtest el)))))
    (%xml-get-attributes-named attrtest context)))

(defun %xml-get-text-values (el)
  (loop for child in (children el)
     when (dom:text-node-p child)
     collect (dom:node-value child)))

(defun xml-get-text-values (context)
  (if (listp context)
      (let ((res ()))
        (dolist (el context res)
          (setf res (append res (%xml-get-text-values el)))))
      (%xml-get-text-values context)))

(defmacro with-xml-ctx ((&key sequence) &body body)
  `(let ((*xml-ctx-sequence* ,sequence))        
     ,@body))

(defun xml-APPLY-LEADING-/ (sequence)
  sequence)

(defun xml-apply-child (nodetest sequence)
  (if (string= nodetest "text()")
      (let ((res (map 'list #'string-on-one-line (xml-get-text-values sequence))))
	(if (eql (length res) 1)
	    (car res)
	    res))
      (xml-get-children-named nodetest sequence)))

(defun xml-APPLY-ATTRIBUTE (attrtest sequence)
  (xml-get-attributes-named attrtest sequence))

(defun x-xp  (xpath &optional sequence)
  (let ((*APPLY-LEADING-/* 'xml-APPLY-LEADING-/)
        (*APPLY-CHILD* 'xml-APPLY-CHILD)
        (*APPLY-ATTRIBUTE* 'xml-APPLY-ATTRIBUTE)
        (sequ (if sequence (if (stringp sequence)
                               (document-parser sequence)
                             sequence)
                *xml-ctx-sequence*)))
    (xp xpath sequ)))
