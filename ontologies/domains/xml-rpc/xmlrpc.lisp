;;; Copyright © 2008 The Open University

(in-package #:ocml)

(in-ontology xml-rpc)

;;; Simple cross-platform distributed computing, based on the
;;; standards of the Internet.

;;; An XML-RPC message is an HTTP-POST request. The body of the
;;; request is in XML. A procedure executes on the server and the
;;; value it returns is also formatted in XML.

;;; Procedure parameters can be scalars, numbers, strings, dates,
;;; etc.; and can also be complex record and list structures.

;;; This is always the same for each XML-RPC request.
(def-rule prepare-http-request
    ((prepare-http-request ?request ?url ?xmlrpc) if
     (serialise ?xmlrpc ?content)
     (#_http:set-content ?request ?content)
     (#_http:set-header ?request "Content-Type" "text/xml")))

;;; The <methodCall> must contain a <methodName> sub-item.
(def-class #_MethodCall ()
    ((#_methodName :type #_MethodName :cardinality 1)
     (#_params :type #_ListOfParam)))

(def-class #_ListOfParam () ?list
  :iff-def (and (listp ?list)
                (every ?list #_Param)))

;;; The <methodCall> must contain a <MethodName> sub-item, a string,
;;; containing the name of the method to be called. The string may
;;; only contain identifier characters, upper and lower-case A-Z, the
;;; numeric characters, 0-9, underscore, dot, colon and slash. It's
;;; entirely up to the server to decide how to interpret the
;;; characters in a MethodName.
(def-class #_MethodName ()
    ((#_value :type string)))

(def-class #_Params ())

(def-class #_Param ()
    ((#_value :type #_Value)))

(def-class #_Value ())

(def-class #_scalarValue (#_Value))

(def-class #_I4 (#_scalarValue)
    ((#_value :type integer)))

(def-class #_Boolean (#_scalarValue)
    ((#_value :type boolean)))

(def-class #_String (#_scalarValue)
    ((#_value :type string)))

(def-class #_Double (#_scalarValue)
    ((#_value :type float)))

(def-class #_DataTimeIso8601 (#_scalarValue)
    ((#_value :type string)))

(def-class #_Base64 (#_scalarValue)
    ((#_value :type string)))

(def-class #_Struct (#_Param)
    ())

(def-class #_Member ()
    ((#_name :type string)
     (#_value :type #_Param)))

(def-class #_Array (#_Param)
    ;; Where the list is a list of #_Param's.
    ((#_values :type list)))

;;; {{{ Constructors
(defun #_new (type value)
  (let ((instance (new-instance type `((#_value ,value)))))
    (name instance)))

(def-function #_Param (value)
  :lisp-fun (lambda (value)
              (#_new '#_Param value)))

(def-function #_I4 (integer)
  :lisp-fun (lambda (integer)
              (#_new '#_I4 integer)))

(def-function #_String (string)
  :lisp-fun (lambda (string)
              (#_new '#_String string)))

(def-function #_Boolean (bool)
  :lisp-fun (lambda (bool)
              (#_new '#_Boolean bool)))

(def-function #_Double (double)
  :lisp-fun (lambda (double)
              (#_new '#_Double double)))

(def-function #_Base64 (base64)
  :lisp-fun (lambda (base64)
              (#_new '#_Base64 base64)))

(def-function #_Member (name value)
  :lisp-fun (lambda (name value)
              (let ((instance (new-instance '#_Member `((#_name ,name)
                                                       (#_value ,value)))))
                (name instance))))

(def-function #_Struct (&rest values)
  :lisp-fun (lambda (values)
              (#_new '#_Struct values)))

(def-function #_MethodCall (method-name &rest params)
  :lisp-fun (lambda (method-name &rest params)
              (let ((instance (new-instance '#_MethodCall
                                            `((#_methodName ,method-name)
                                              (#_params ,params)))))
                (name instance))))

;;; }}}
