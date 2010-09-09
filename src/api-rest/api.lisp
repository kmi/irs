;;; Copyright © 2008 The Open University

(in-package #:irs.api.rest)

;;; {{{ URLs for schemas, namespaces etc
(define-constant +ocml-xml-namespace-iri+
  "http://kmi.open.ac.uk/ocml/xsd/20081105/OcmlModel"
  "XML namespace for OCML XML scheme.")

(define-constant +ocml-xml-xsi-schema-location+
  "http://kmi.open.ac.uk/ocml/xsd/20081105/OcmlModel OcmlModel.xsd"
  "Tell XSI systems what file to use for the OCML schema.")

(define-constant +xsi-schema-iri+
  "http://www.w3.org/2001/XMLSchema-instance")
;;; }}}

(defun initialise ()
  (irs.web:register-plugin
   :api-rest :api
   "API for REST calls."
   (nconc (mapcar (lambda (args)
                    (apply #'hunchentoot:create-regex-dispatcher args))
                  '(("/api-rest/achieve-goal" achieve-goal)
                    ("/api-rest/get-ontology$" get-ontology)
                    ("/api-rest/get-ontology-iri-from-namespace$"
                     get-ontology-iri-from-namespace)
                    ("/api-rest/monitor-events$" monitor-events)
                    ("/api-rest/namespacify-symbol$" namespacify-symbol)
                    ("/api-rest/ontology-list$" send-ontology-list-data)
                    ("/api-rest/query$" do-query)
                    ("/api-rest/retrieve-instance$" retrieve-instance)
                    ("/api-rest/send-class$" send-class)
                    ("/api-rest/send-instance$" send-instance)
                    ("/api-rest/send-ontology$" send-ontology)
                    ("/api-rest/delete-ontology$" del-ontology)
                    ("/api-rest/delete-class$" del-class))))))

;;; {{{ Achieve goal
(defun achieve-goal ()
  (web:with-parameters (:get (ontology goal))
    (let* ((o (intern-ocml-symbol ontology))
           (g (with-ontology (o)
                (intern-ocml-symbol goal))))
      (ocml:with-ontology (o)
        (let* ((roles (wp::input-roles g)))
          (let ((roles+values (mapcar #'(lambda (slot)
                                          (list slot
                                                (let ((*package* (find-package :ocml)))
                                                  (hunchentoot:get-parameter (extern-ocml-symbol slot)))))
                                      roles)))
            (irs.web:write-http-stream "text/javascript"
                               (with-output-to-string (ostream)
                                 (ip::irs-achieve-goal o g roles+values ostream nil)))))))))
;;; }}}
;;; {{{ Query
(defun do-query ()
  (web:with-parameters (:get (f o q))
    (let* ((*package* (find-package '#:ocml))
           (ontology (find-ontology (hunchentoot:url-decode o)))
           (query (read-from-string (hunchentoot:url-decode q))))
        (irs.web:write-http-stream
         (cond ((string= f "xml") "text/xml")
               ;; XXX JSON should be sent as "application/json", but
               ;; Firefox refuses to view JSON, making debugging
               ;; harder.
               ((string= f "json") "text/plain")
               (t "text/plain"))
         (do-query* f ontology query)))))

(defun do-query* (f o query)
  (let ((answers (ocml:with-ontology (o)
                   (eval (list 'ocml::ocml-eval query)))))
  (cond ((string= f "xml")
         (format-xml-query-result query answers))
        ((string= f "json")
         (format-json-query-result query answers))
        (t
         (format nil "~S" answers)))))

;;; XXX This has implications for the types of queries we ask.  Do all
;;; query toplevels place variables in the first position?

(defun query-variables (exp)
  (ecase (first exp)
    ((ocml::setofall) (second exp))))

(defun format-xml-query-result (query answers)
  ;; Grab the variables from query.  If it's not a list, make it one
  ;; and wrap the answers, too.
  (let ((variables (query-variables query)))
    (unless (listp variables)
      (setf variables (list variables))
      (setf answers (mapcar #'list answers)))
    (with-output-to-string (str)
      (format str "<?xml version='1.0' encoding='UTF-8'?>~%")
      (format str "<tns:ResultSet xmlns:tns='http://kmi.open.ac.uk/irs/query/20080215/result'
    xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'
    xsi:schemaLocation='http://kmi.open.ac.uk/irs/query/20080215/result QueryResultSet.xsd'>~%")
      (dolist (answer answers)
        (format str "    <tns:Result>~%")
        (mapcar #'(lambda (var val)
                    (format str "        <tns:Binding Variable='~A' Value='~A'/>~%"
                            var val))
                variables answer)
        (format str "    </tns:Result>~%"))
      (format str "</tns:ResultSet>")
      str)))

(defun format-json-query-result (query answers)
  ;; Grab the variables from query.  If it's not a list, make it one
  ;; and wrap the answers, too.
  (let ((variables (query-variables query)))
    (unless (listp variables)
      (setf variables (list variables))
      (setf answers (mapcar #'list answers)))
    (irs.api.javascript::serialise-json
     (coerce (mapcar #'(lambda (answer)
                         (irs.api.javascript::js-object
                          "result"
                          (coerce (mapcar #'(lambda (var val)
                                              (irs.api.javascript::js-object
                                               "variable" var
                                               "value" val))
                                          variables answer)
                                  'vector)))
                     answers)
             'vector))))

;;; }}}
;;; {{{ XML support
(defun call-with-xml-wrapping (thunk)
  (let* ((stream (flexi-streams:make-in-memory-output-stream
                  :element-type '(unsigned-byte 8)))
         (sink (cxml:make-octet-stream-sink stream :indentation nil :canonical t)))
    (cxml:with-xml-output sink
      (cxml:with-namespace ("tns" +ocml-xml-namespace-iri+)
        (cxml:with-namespace ("xsi" +xsi-schema-iri+)
          (funcall thunk))))
    (flexi-streams:octets-to-string
     (flexi-streams:get-output-stream-sequence stream))))
;;; }}}
;;; {{{ get ontology
(defun get-ontology ()
  "Take ontology identifier (IRI) and return serialisation of it as ..."
  (web:with-parameters (:get (ontology))
    (let* ((*package* (find-package :ocml))
           (onto (find-ontology ontology)))
      (if onto
          (irs.web:write-http-stream
           "text/xml" (serialise-ontology onto))
          (error "No ontology identified by ~A." ontology)))))

(defun serialise-ontology (ontology)
  (call-with-xml-wrapping
   (lambda ()
     (ocml:with-ontology (ontology)
       (cxml:with-element "tns:OcmlOntology"
         (cxml:attribute "xsi:schemaLocation" +ocml-xml-xsi-schema-location+)
         (cxml:with-element "tns:Iri"
           (cxml:text (ontology-iri ocml::*current-ontology*)))
         (dolist (o (ocml::ontology-includes ocml::*current-ontology*))
           (cxml:with-element "tns:Import"
             (cxml:with-element "tns:Iri"
               (cxml:text (ontology-iri o)))))
         (serialise-classes)
         (serialise-instances))))))

(defun serialise-classes ()
  (dolist (c (order-subclass-lattice
              (remove-if (lambda (c)
                           (not (eq (ocml::home-ontology c)
                                    ocml::*current-ontology*)))
                         (ocml::all-ocml-classes t))))
    (cxml:with-element "tns:OcmlClass"
      (cxml:with-element "tns:Iri"
        (cxml:text (iri-of c)))
      (dolist (super (canonicalise (ocml::direct-domain-superclasses c) :classes))
        (cxml:with-element "tns:SuperClass"
          (cxml:text (iri-of super))))
      (serialise-class-slots c)
      (let ((doc (ocml::ocml-documentation c)))
        (when doc
          (cxml:with-element "tns:Documentation"
            (cxml:text doc)))))))

(defun serialise-slot-type (c slot)
  (let ((type (ocml::get-slot-type c slot)))
    (cond ((null type)
           ;; An undefined type.  Don't mention it.
           nil)
          ((= 1 (length type))
           (cxml:with-element "tns:SlotTypeIri"
             (cxml:text (iri-of (ocml::get-domain-class (first type))))))
          (t
           (error "Slot ~A in class ~A has weird value ~A."
                  (iri-of c) (iri-of slot) type)))))

(defun serialise-class-slots (c)
  (dolist (slot (canonicalise (ocml::local-slots c) :symbols))
    (cxml:with-element "tns:Slot"
      (cxml:with-element "tns:Iri" (cxml:text (iri-of slot)))
      (serialise-slot-type c slot)
      ;; Inheritace mode
      (case (ocml::find-option-value c slot :inheritance)
        ((:merge) (cxml:with-element "tns:InheritanceMode"
                    (cxml:text "merge")))
        ((:supercede) (cxml:with-element "tns:InheritanceMode"
                        (cxml:text "supersede"))))
      ;; Cardinality
      (let ((min (ocml::find-option-value c slot :min-cardinality))
            (max (ocml::find-option-value c slot :max-cardinality)))
        (when min
          (cxml:with-element "tns:MinCardinality"
            (cxml:text (format nil "~A" min))))
        (when max
          (cxml:with-element "tns:MaxCardinality"
            (cxml:text (format nil "~A" max)))))
      ;; Values
      (multiple-value-bind (fixed default)
          (ocml::get-slot-values-from-class-structure c slot)
        (dolist (f fixed)
          (cxml:with-element "tns:FixedValue"
            (output-ltype-attribute f)
            (cxml:text (extern-value f))))
        (dolist (d default)
          (cxml:with-element "tns:DefaultValue"
            (output-ltype-attribute d)
            (cxml:text (extern-value d)))))
      ;; Documentation.
      (let ((doc (ocml::find-slot-documentation c slot)))
        (when doc
          (cxml:with-element "tns:Documentation"
            (cxml:text doc)))))))

(defun all-instances-defined-by-this-ontology ()
  "All instances defined by this ontology."
  (let ((classes (ocml::all-ocml-classes t)))
    (remove-duplicates
     (remove-if (lambda (instance)
                  (not (eq (ocml::home-ontology instance)
                           ocml::*current-ontology*)))
                (mapcan (lambda (c) (ocml::get-current-direct-instances c))
                        classes)))))

(defun serialise-instances ()
  (dolist (i (canonicalise (all-instances-defined-by-this-ontology) :instances))
    (serialise-instance i)))

(defun serialise-instance (i)
  (cxml:with-element "tns:OcmlInstance"
    (cxml:with-element "tns:Iri"
      (cxml:text (iri-of i)))
    (cxml:with-element "tns:ClassIri"
      (cxml:text (iri-of (ocml::parent-class i))))
    (let ((doc (ocml::ocml-documentation i)))
      (when doc
        (cxml:with-element "tns:Documentation"
          (cxml:text doc))))
    ;; Write out slots with their values.
    (let ((c (class-of i)))
      (dolist (s (canonicalise (ocml::domain-slots i) :symbols))
        (let ((values (ocml::setofall '?x `(,s ,(ocml::name i) ?x))))
          (when values
            (cxml:with-element "tns:SlotValue"
              (cxml:with-element "tns:Iri"
                (cxml:text (iri-of s)))
              (serialise-slot-type c s)
              (dolist (v (canonicalise values :symbols))
                (cxml:with-element "tns:Value"
                  (output-ltype-attribute v)
                  (cxml:text (extern-value v)))))))))))

;;; }}}
;;; {{{ monitor-events
(defun monitor-events ()
  (let ((stream (irs.web:setup-multipart)))
    (irs.monitoring:create-monitoring-observer
     'a-rest-api-session-id (monitor-events-callback stream))))

(defun monitor-events-callback (stream)
  (lambda (monitor event-instance)
    (handler-case
        (let ((xml (call-with-xml-wrapping
                    (lambda () (serialise-instance event-instance)))))
          (irs.web:send-multipart stream "text/xml" xml))
      (error (c)
        ;; Something went wrong so lets get rid of the
        ;; monitoring-observer.
        (irs.monitoring:delete-monitoring-observer monitor)))))

;;; }}}
;;; {{{ ontology-list
(defun send-ontology-list-data ()
  (web:write-http-stream "text/xml" (ontology-list-xml)))

(defun ontology-list-xml ()
  (call-with-xml-wrapping
   (lambda ()
     (cxml:with-element "tns:OcmlOntologyList"
       (dolist (o (ocml::sorted-ontologies))
         (cxml:with-element "tns:OcmlOntologyName"
           (cxml:text (ontology-iri (find-ontology o)))))))))
;;; }}}
;;; {{{ retrieve-instance
(defun retrieve-instance ()
  (web:with-parameters (:get (iri))
    (irs.web:write-http-stream "text/xml" (retrieve-instance* iri))))

(defun find-instance-by-name (name)
  (let ((instances (ocml::find-all-current-instances-named-x name)))
    (cond ((= 1 (length instances))
           (first instances))
          ((= 0 (length instances))
           (error "No instance named ~A found in ontology ~A."
                  name ocml::*current-ontology*))
          (t
           (error "More than one instance named ~A found in ontology ~A."
                  name ocml::*current-ontology*)))))

(defun retrieve-instance* (iri)
  (let ((ontology (find-ontology (hunchentoot:url-decode iri))))
    (ocml:with-ontology (ontology)
      (let* ((symbol (intern iri (find-package :ocml)))
             (instance (find-instance-by-name symbol)))
        (call-with-xml-wrapping
         (lambda () (serialise-instance instance)))))))
;;; }}}
;;; {{{ send-class
(defun send-class ()
  (web:with-parameters (:post (ontology class))
    (irs.web:write-http-stream "text/xml" (send-class* ontology class))))

(defun send-class* (ontology class-xml)
  (send-class-dom ontology
                  (first (api-soap-old::dom-children
                          (api-soap-old::soap-xml-parse class-xml)))))

(defun send-class-dom (ontology-name top)
  (let* ((class-name (intern-ocml-iri (unpack-element top "Iri")))
         (ontology (find-ontology ontology-name)))
    (unless ontology
      (error "Cannot identify ontology for ~A." ontology-name))
    (ocml:with-ontology (ontology)
      (let* ((supers (mapcar #'intern-ocml-iri
                             (mapcar #'get-first-text-child
                                     (elements-by-name top "SuperClass"))))
             (documentation (get-first-text-child
                             (first (elements-by-name top "Documentation"))
                             nil))
             (slots (extract-class-slots top)))
        (ocml:define-class class-name :superclasses supers
                           :documentation documentation
                           :class-slots slots)))))

(defun extract-class-slots (class-dom)
  (mapcar (lambda (slot)
            (let* ((slotname (intern-ocml-iri (unpack-element slot "Iri")))
                   (type (let ((type0 (unpack-element slot "SlotTypeIri")))
                           (if type0
                               (intern-ocml-iri type0))))
                   (fixed-values
                    (mapcar #'(lambda (v)
                                (intern-value (get-ltype-from-value-dom v)
                                              (get-first-text-child v)))
                            (elements-by-name slot "FixedValue")))
                   (default-values
                    (mapcar #'(lambda (v)
                                (intern-value (get-ltype-from-value-dom v)
                                              (get-first-text-child v)))
                            (elements-by-name slot "DefaultValue")))
                   (min-cardinality
                    (maybe-parse-number
                     (unpack-element slot "MinCardinality")))
                   (max-cardinality
                    (maybe-parse-number
                     (unpack-element slot "MaxCardinality")))
                   (modcons (append
                             (if type
                                 (list :type type))
                             (if min-cardinality
                                 (list :min-cardinality min-cardinality))
                             (if max-cardinality
                                 (list :max-cardinality max-cardinality))
                             (if default-values
                                 (apply #'append
                                        (mapcar #'(lambda (v) (list :default-value v))
                                                default-values)))
                             (if fixed-values
                                 (apply #'append
                                        (mapcar #'(lambda (v) (list :value v))
                                                fixed-values))))))
              (list* slotname modcons)))
          (elements-by-name class-dom "Slot")))
;;; }}}
;;; {{{ send-instance
(defun send-instance ()
  (web:with-parameters (:post (instance))
    (irs.web:write-http-stream "text/xml" (send-instance* instance))))

(defun extract-instance-slots (instance-dom)
  (mapcar (lambda (slot)
            (let* ((slotname (intern-ocml-iri (unpack-element slot "Iri")))
                   (values (coerce (dom:get-elements-by-tag-name-ns
                                    slot #"*" "Value") 'list)))
              (list* slotname
                     (mapcar (lambda (xml-value)
                               (intern-value (get-ltype-from-value-dom xml-value)
                                             (get-first-text-child xml-value)))
                             values))))
          (coerce (dom:get-elements-by-tag-name-ns
                   instance-dom #"*" "SlotValue")
                  'list)))

(defun send-instance* (instance-xml)
  (send-instance-dom (first (api-soap-old::dom-children
                             (api-soap-old::soap-xml-parse instance-xml)))))

(defun send-instance-dom (top)
  (let* ((instance-name (unpack-element top "Iri"))
         (ontology (find-ontology instance-name)))
    (unless ontology
      (error "Cannot identify ontology for ~A." instance-name))
    (ocml:with-ontology (ontology)
      (let* ((class-name (intern-ocml-iri (unpack-element top "ClassIri")))
             (documentation (unpack-element top "Documentation"))
             (slots (extract-instance-slots top)))
        (apply #'ocml::define-domain-instance
               (intern-ocml-iri instance-name) class-name
               (if documentation
                   (list documentation slots)
                   (list slots)))))))
;;; }}}
;;; {{{ send-ontology
(defun send-ontology ()
  (web:with-parameters (:post (ontology))
    (irs.web:write-http-stream "text/xml" (send-ontology* ontology))))

(defun send-ontology* (ontology-xml)
  (send-ontology-dom (first (api-soap-old::dom-children
                      (api-soap-old::soap-xml-parse ontology-xml)))))

(defun send-ontology-dom (top)
  (let ((ontology-iri (unpack-element top "Iri")))
    (multiple-value-bind (ocml-name namespace-iri)
        (irs::identify-ontology-by-wsml-iri ontology-iri)
      (let* ((import-iris (mapcar #'(lambda (el)
                                      (get-first-text-child
                                       (first (elements-by-name el "Iri"))))
                                  (elements-by-name top "Import")))
             (imports (mapcar #'irs::identify-ontology-by-wsml-iri
                              import-iris))
             (classes (elements-by-name top "OcmlClass"))
             (instances (elements-by-name top "OcmlInstance"))
             (continuation (lambda ()
                             (dolist (dom classes)
                               (send-class-dom ocml-name dom) classes)
                             (dolist (dom instances)
                               (send-instance-dom dom)))))
        (ocml:define-ontology ocml-name :includes imports
                              :namespace-iri namespace-iri
                              :continuation continuation)))))

;;; }}}
;;; {{{ delete-ontology
(defun del-ontology ()
  (web:with-parameters (:get (ontology))
    (irs.web:write-http-stream "text/plain" (del-ontology* ontology))))

(defun del-ontology* (ont-name)
  (ocml::delete-ontology (intern ont-name :ocml)))

;;; }}}
;;; {{{ delete-class
(defun del-class ()
  (web:with-parameters (:get (clazz))
    (irs.web:write-http-stream "text/plain" (del-class* clazz))))

(defun del-class* (class-name)
  (ocml::remove-ocml-class (intern class-name :ocml)))
;;; }}}
