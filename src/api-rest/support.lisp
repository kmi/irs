;;; Copyright © 2008 The Open University

(in-package #:irs.api.rest)

;;; {{{ XML stuff
(defun elements-by-name (top name)
  (coerce (dom:get-elements-by-tag-name-ns top #"*" name) 'list))

(defun get-first-text-child (element &optional (error t))
  (if (and (not element) (not error))
      nil
      (let* ((children (api-soap-old::dom-children element))
             (text (first children)))
        (if (and (= 1 (length children))
                 (member (type-of text)
                         '(rune-dom::text rune-dom::cdata-section)))
            (dom:node-value text)
            (when error
              (error "Cannot extract a data value from ~A." element))))))

(defun unpack-element (xml name)
  (let ((elements (dom:get-elements-by-tag-name-ns xml #"*" name)))
    (if (> (length elements) 0)
        (get-first-text-child (elt elements 0))
        nil)))
;;; }}}

;;; {{{ Lexical bits

;;; XXX Should probably be in OCML.
(defun intern-ocml-iri (iri)
  "IRI is a string which is turned into a symbol."
  ;; If the namespace is an assumed one, we have to lose it, to put it
  ;; in the global OCML space.
  (assert (stringp iri))
  (let ((hash (position #\# iri)))
    ;; XXX This is horrible.  Should have a clean way of determining
    ;; assumed namespaces.
    (intern (if (search "assumed" iri)
                (subseq iri (+ 1 hash))
                iri)
            (find-package :ocml))))

(defun must-read-integer (string)
  (let ((n (read-from-string string)))
    (if (integerp n)
        n
        (error "Cannot parse `~A' as an integer." string))))

(defun intern-value (lexical-class value-string)
  ""
  (case lexical-class
    ((string) value-string)
    ((number)
     (let ((n (read-from-string value-string)))
       (if (numberp n)
           n
           (error "Cannot parse `~A' as a number." value-string))))
    ((sexp)
     (let ((*package* (find-package :ocml)))
       (read-from-string value-string)))
    ((symbol)
     (intern-ocml-iri value-string))
    ((datetime)
     (utilities:make-xsd-datetime :milliseconds (must-read-integer value-string)))
    ((date)
     (utilities:make-xsd-date :milliseconds (must-read-integer value-string)))
    ((time)
     (utilities:make-xsd-time :milliseconds (must-read-integer value-string)))
    (t
     (error "intern-value called with unspecified lexical class `~A' for `~A'."
            lexical-class value-string))))

(defun extern-value (val)
  (etypecase val
    (symbol (symbol-name val))
    (string val)
    ((or fixnum bignum single-float) (format nil "~A" val))
    (simple-string (format nil "~A" val))
    (cons (format nil "~S" val))
    (utilities:xsd-datetime
       (format *error-output* "extern-value: called on ~S~%" val)
       (format nil "~A" (utilities::xsd-datetime-milliseconds val)))
    (utilities:xsd-time
       (format *error-output* "extern-value: called on ~S~%" val)
       (format nil "~A" (utilities::xsd-time-milliseconds val)))
    (utilities:xsd-date
       (format *error-output* "extern-value: called on ~S~%" val)
       (format nil "~A" (utilities::xsd-date-milliseconds val)))
    (null "NIL")))

(defun output-ltype-attribute (value)
  (let ((ltype (typecase value
                 (number "number")
                 (symbol "symbol")
                 (string "string")
                 (utilities:xsd-datetime "datetime")
                 (utilities:xsd-time "time")
                 (utilities:xsd-date "date")
                 (t "expr"))))
    (cxml:attribute "tns:ltype" ltype)))

(defun get-ltype-from-value-dom (value-dom)
  "Read the `ltype' attribute."
  (let ((ltype (dom:get-attribute value-dom "tns:ltype")))
    (cond ((string= ltype "string") 'string)
          ((string= ltype "number") 'number)
          ((string= ltype "symbol") 'symbol)
          ((string= ltype "expr") 'sexp)
          ((string= ltype "datetime") 'datetime)
          ((string= ltype "date") 'date)
          ((string= ltype "time") 'time)
          (t (error "Bad ltype `~A'." ltype)))))

(defun iri-of (ocml-object)
  (if (symbolp ocml-object)
      ;; If it's a symbol, then it will be a slot name, and therefore
      ;; a local slot.  That puts it in the namespace of the current
      ;; ontology, so we use its IRI to compute the IRI for the
      ;; symbol.
      (iri-from-symbol ocml::*current-ontology* ocml-object)
      (iri-from-symbol (ocml::home-ontology ocml-object)
                       (ocml::name ocml-object))))

(defun iri-from-symbol (ontology symbol)
  (if (position #\# (symbol-name symbol))
      (symbol-name symbol)
      (format nil "~A~A" (ocml::namespace-uri-of ontology) symbol)))

(defun maybe-parse-number (string-or-nil)
  (check-type string-or-nil (or string null))
  (if string-or-nil
      (parse-integer string-or-nil)
      nil))

;;; }}}

;;; {{{ Object order canonicalisation
(defun canonicalise (things style)
  (let ((things* (copy-seq things)))
    (ecase style
      ((:classes :instances) (sort things* #'string< :key #'ocml::name))
      ((:symbols) (sort things* #'string<)))))

(defun order-subclass-lattice (classes)
  "Order CLASSES so no subclass precedes its own superclasses."
  (labels ((shake (classes)
             (if (null classes)
                 '()
                 (let ((c (ocml::subclass-of* (first classes) (rest classes))))
                   (if c
                       (shake (append (rest classes) (list (first classes))))
                       (cons (first classes) (shake (rest classes))))))))
    (shake (canonicalise classes :classes))))
;;; }}}

;;; {{{ Miscellaneous

(defun ontology-iri (obj)
  (format nil "~A~A" (ocml::namespace-uri-of obj) (ocml::name obj)))

(defun get-ontology-iri-from-namespace ()
  ""
  (web:with-parameters (:get (namespace))
    (let* ((onto (find-ontology namespace)))
      (if onto
          (irs.web:write-http-stream
           "text/plain" (ontology-iri onto))
          (error "No ontology identified by ~A." namespace)))))

(defun namespacify-symbol ()
  ""
  (web:with-parameters (:get (ontology type symbol))
    (let* ((onto (find-ontology ontology)))
      (if onto
          (ocml:with-ontology (onto)
            (let ((type (intern (string-upcase type) :keyword))
                  (symbol (intern-ocml-symbol symbol)))
              (irs.web:write-http-stream
               "text/plain" (iri-of (lookup-symbol-by-type type symbol)))))
          (error "No ontology identified by ~A." ontology)))))

(defun lookup-symbol-by-type (type symbol)
  (ecase type
    ((:class) (ocml::get-domain-class symbol))
    ((:function) (ocml::get-function symbol))
    ((:instance) (ocml::find-current-instance symbol))
    ((:relation) (ocml::get-relation symbol))
    ((:rule) (ocml::get-rule symbol))))

;;; }}}
