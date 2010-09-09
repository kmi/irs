;;; Copyright (C) 2008 The Open University

(in-package #:irs.api.soap-old)

;;; Bits of this API are scattered across the soap-irs-protocol,
;;; soap-irs-utilities and wsmo-protocol modules.  They will gradually
;;; move in here, as we clean up the code.

(defun initialise ()
  (irs.web:register-plugin
   :api-soap-old :api "API for legacy SOAP calls on port 3000." nil)
  #+:irs-use-lispweb
  (http::start-tcp-services))

;;; The main user of these functions seems to be the method
;;; http::http-reply ((method (eql :post)) request) in
;;; code/soap-irs-protocol/soap.lisp

(defun soap-xml-parse (string)
  "Parse the XML in STRING to a DOM object."
  (cxml:parse (flexi-streams:string-to-octets string)
              (cxml-dom:make-dom-builder)))

(defun dom-children (node)
  "Get children of NODE as a list."
  (let ((children '()))
    (dom:do-node-list (child (dom:child-nodes node))
      (push child children))
    (nreverse children)))

(defun get-msg (xml-doc)
  "Return the XML element of XML-DOC that's the SOAP operation/method."
  (let* ((body (elt (dom:get-elements-by-tag-name-ns xml-doc #"*" "Body") 0))
         (kids (dom-children body)))
    (find-if #'dom:element-p kids)))

(defun filter-out-text (nodes)
  "Remove text nodes from NODES."
  (remove-if (lambda (node)
               (equal 'RUNE-DOM::TEXT (type-of node)))
             nodes))

(defun xml-name (thing)
  (dom:local-name thing))

(defun get-soapvalues (xml-msg)
  "Get the parsed SOAP values from XML-MSG."
  (mapcar (lambda (node)
            (let* ((attributes (dom:attributes node))
                   (type (dom:node-value (first (dom:items attributes))))
                   (raw-value (dom:node-value (first (dom-children node)))))
              (read-soap-type type raw-value)))
          (filter-out-text (dom-children xml-msg))))

(defun read-soap-type (type value)
  (cond ((string= type "int")
         (parse-integer value))
        ((string= type "sexpr")
         (let ((*package* (find-package :ocml)))
           (read-from-string value)))
        ((string= type "string")
         value)
        (t
         (error "Unrecognised XML type ~A." type))))

(defvar *xml-soap-types* '(xml xml-array))

(defun xml-soap-type-p (x)
  (find x *xml-soap-types*))

(defvar *string-soap-types* '(string xml xml-array))

(defun string-soap-type-p (x)
  (find x *string-soap-types*))
