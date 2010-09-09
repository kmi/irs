;;; Copyright © 2008,2009 The Open University

(in-package #:ocml)

(in-ontology xml)

;;; Deals with the serialisation of the ontological XML to text, and
;;; parsing it back again.

(defun call-with-xml-wrapping (thunk)
  (let* ((stream (flexi-streams:make-in-memory-output-stream
                  :element-type '(unsigned-byte 8)))
         (sink (cxml:make-octet-stream-sink stream :indentation nil :canonical t)))
    (cxml:with-xml-output sink
      (funcall thunk))
    (flexi-streams:octets-to-string
     (flexi-streams:get-output-stream-sequence stream))))

(defun to-xml-toplevel (ocml)
  (call-with-xml-wrapping (lambda () (to-xml ocml))))

(defun resolve (name)
  (let ((instance (find-all-current-instances-named-x name)))
    (if (= 1 (length instance))
        (setf instance (first instance))
        (error "Symbol '~A' resolves to ~A, not a unique instance." name instance))))

(defun to-xml (name)
  ;; Returns a string of XML from the OCML XML representation.
  (let* ((instance (resolve name))
	 (class (name (class-of instance))))
    (ecase class
      ((#_Element)
	 (let ((tag (the-slot-value name '#_tag))
	       (attributes (the-slot-value name '#_attributes))
	       (contents (the-slot-value name '#_contents)))
	   (cxml:with-element tag
	     (dolist (a attributes)
	       (to-xml a))
	     (dolist (c contents)
	       (to-xml c)))))
      ((#_Attribute)
	 (cxml:attribute (the-slot-value name '#_name)
			 (the-slot-value name '#_value)))
      ((#_Text)
	 (let ((string (the-slot-value name '#_value)))
	   (cxml:text string)))
      ((#_Document)
	 (let ((root (the-slot-value name '#_rootElement)))
	   (to-xml root))))))

(defun from-xml-toplevel (xml-string)
  (let ((dom (cxml:parse (flexi-streams:string-to-octets xml-string)
			 (cxml-dom:make-dom-builder))))
    (from-xml dom)))

(defun from-xml (node)
  (let ((xml-children (api-soap-old::dom-children node)))
    (etypecase node
      (rune-dom::document
	 (name (new-instance '#_Document `((#_rootElement ,(from-xml (first xml-children)))))))
      (rune-dom::element
	 (let* ((kids (mapcar #'from-xml xml-children))
		(tag (dom:tag-name node))
		(attributes (dom:items (dom:attributes node)))
		(attrs (mapcar (lambda (at)
				 (let ((name (dom:name at))
				       (value (dom:node-value (first (api-soap-old::dom-children at)))))
				   (name (new-instance '#_Attribute
						       `((#_name ,name)
							 (#_value ,value))))))
			       attributes)))
	   (name (new-instance '#_Element `((#_contents ,kids)
					    (#_tag ,tag)
					    (#_attributes ,attrs ))))))
      (rune-dom::text
	 (let ((text (dom:node-value node)))
	   (name (new-instance '#_Text `((#_value ,text)))))))))

;;; Holds if ?ocml serialises to ?xml, or ?xml can be parsed to ?ocml.
(def-relation #_serialiseXml (?ocml ?xml)
    :lisp-fun
  #'(lambda (?ocml ?xml env)
      (let* ((ocml-bound? (not (unbound-variable? ?ocml env)))
	     (xml-bound? (or (stringp ?xml) (not (unbound-variable? ?xml env))))
	     (serialised (if ocml-bound?
			     (to-xml-toplevel (lookup-or-self ?ocml env))
			     nil)))
	(cond (ocml-bound?
	       (cond (xml-bound?
		      (if (string= serialised (lookup-or-self ?xml env))
			  (list env)
			  :fail))
		     ((not xml-bound?)
		      (let ((newenv (unify ?xml serialised env)))
			(if newenv
			    (list newenv)
			    :fail)))))
	      (xml-bound?
	       ;; ?ocml must not be bound, so bind it.
	       (let* ((deserialised (from-xml-toplevel (lookup-or-self ?xml env)))
		      (newenv (unify ?ocml deserialised env)))
		 (if newenv
		     (list newenv)
		     :fail)))
	      (t
	       (error "Neither ~A nor ~A are bound." ?ocml ?xml))))))
