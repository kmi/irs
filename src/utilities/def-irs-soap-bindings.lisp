(in-package irs-utilities)


(defvar *methods-hash-table* (make-hash-table :test #'equal))

(defun add-to-methods-hash-table (ontology method-name input-roles 
                                           output &optional lisp-function)
  (setf (gethash (List ontology method-name)
                 *methods-hash-table*)
        (list  input-roles 
               output lisp-function)))

(defun get-methods-definition (ontology method-name)
  (gethash (List ontology method-name)
           *methods-hash-table*))

(defun remove-method-definition (ontology method-name)
  (remhash (List ontology method-name)
           *methods-hash-table*))

(defun get-task-input-roles (ontology task-name)
  (car (get-methods-definition ontology task-name)))

(defun get-task-output-role (ontology task-name)
  (second (get-methods-definition ontology task-name)))

;;;have to re-write this to indicate that this is a tasks table
;;;and not a methods table
(defun get-task-definition (ontology task-name)
  (get-methods-definition ontology task-name))

;;;moved to the ocml package so that the definitions look nicer
;;;in ocml files
(defmacro ocml::def-irs-soap-bindings (task-name ontology input-roles output)
  `(progn 
     (ocml::ocml-record-source-file ',task-name 
                                    'ocml::def-irs-soap-bindings
                                    ',ontology)
     (add-to-methods-hash-table ',ontology 
                                ',task-name ',input-roles ',output)))


#+:lispworks 
(editor::setup-indent 'ocml::def-irs-soap-bindings 0 2)

#+:lispworks4.3
(dspec:define-dspec-class ocml::def-irs-soap-bindings nil "def-irs-soap-bindings"
  :pretty-name "Def-Irs-Soap-Bindings"
  :Canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec))
                          (ontology (third dspec)))
                      `(ocml::def-irs-soap-bindings ,name ,ontology)))
  :definedp #'(lambda (x) (and (listp x) (get-methods-definition (car x) (second x)))))