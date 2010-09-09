(in-package #:ocml)

(in-ontology json)

;;Here we define the functionality of converting from a JSON string to
;;an OCML-ised representation of that string, and then back again.

;{{{ TO-JSON

(defun to-json-toplevel (ocml)
;; Returns a JSON string from the OCML-ised JSON representation
  (json:bind-custom-vars
   ;Good idea to treat JSON arrays as Lisp vectors
   (:array-type 'VECTOR)
   (json:encode-json-to-string
    (to-json ocml))))

(defun to-json (name)
  ;; Returns a CL-JSON representation from the OCML-ised JSON
  ;; representation

  (etypecase name
    (string
     name)

    (number
     name)

    (boolean
     name)

    (symbol
     (let* ((instance (resolve name))
	    (class (name (class-of instance))))
       (ecase class
	 ((#_Pair)
	  (cons
	   (intern 
	    (json:camel-case-to-lisp
	     (the-slot-value name '#_key))
	    :keyword)
	   (to-json (the-slot-value name '#_value))))
	 ((#_Object)
	  (mapcar #'to-json (the-slot-value name '#_members)))
	 ((#_Array)
	  (map 'vector #'to-json (the-slot-value name
     '#_elements))))))))

;Find the OCML instance called 'name'
(defun resolve (name)
  (let ((instance (find-all-current-instances-named-x name)))
    (if (= 1 (length instance))
	(setf instance (first instance))
      (error "Symbol '~A' resolves to ~A, not a unique instance." name
  instance))))

;}}} 

;{{{ FROM-JSON

(defun from-json-toplevel (json-string)
  (json:bind-custom-vars
   ;Good idea to treat JSON arrays as Lisp vectors
   (:array-type 'VECTOR)
   (let ((json-structure (json:decode-json-from-string json-string)))
     (from-json json-structure))))

(defun from-json (value)

  (etypecase value

    ;typecheck for string must come before check for vector
    ;since string is a type of vector
    (string
     value)

    (number
     value)

    ;typecheck for boolean must come before check for list
    ;since NIL (which is used for 'false') is also of type
    ;list
    (boolean
     value)

    (vector
     (let ((elements (map 'list #'from-json value)))
       (name (new-instance '#_Array
			   `((#_elements ,elements))))))
    (list
     (let ((members (mapcar #'(lambda (x)
				(let ((key 
				       ;CL-JSON parses "keyString"
				       ;into "KEY-STRING". So want to
				       ;convert back to the original
				       ;for the OCML instance	       
				       (json:lisp-to-camel-case
					(symbol-name (car x))))
				      (value (from-json (cdr x))))
				  (name (new-instance '#_Pair
						      `((#_key ,key)
							(#_value ,value))))))
			    value)))
       (name (new-instance '#_Object
			   `((#_members ,members))))))))
;}}}

;;Now we define an OCML relation that brings these serialisation
;;functions together.
(def-relation #_serialiseJson (?ocml ?json)
"The relation holds if ?ocml serialises to ?json or ?json can be
parsed to ?ocml"
  :lisp-fun
  #'(lambda (?ocml ?json env)
      (let* ((ocml-bound? (not (unbound-variable? ?ocml env)))
	     (json-bound? (or (stringp ?json)
			      (not (unbound-variable? ?json env))))
	     (serialised (if ocml-bound?
			     (to-json-toplevel (lookup-or-self ?ocml env))
			   nil)))
	(cond (ocml-bound?
	       (cond (json-bound?
		      (if (string= serialised (lookup-or-self ?json
							      env))
			  (list env)
			:fail))
		     ((not json-bound?)
		      (let ((newenv (unify ?json serialised env)))
			(if newenv
			    (list newenv)
			  :fail)))))
	      (json-bound?
	       ;; ?ocml must not be bound, so bind it.
	       (let* ((deserialised (from-json-toplevel (lookup-or-self ?json
							       env)))
		      (newenv (unify ?ocml deserialised env)))
		 (if newenv
		     (list newenv)
		   :fail)))
	      (t
	       (error "Neither ~A nor ~A are bound." ?ocml ?json))))))
