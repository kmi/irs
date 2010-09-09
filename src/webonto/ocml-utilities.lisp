;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

;;;used for the limit on the number of displayable items
;;;negative means unlimited - this is used in the java code
(defun unlimited-p (x)
  (< x 0))

(defun limited-p (x)
  (>= x 0))

(defmacro map-over-hash-table-with-limit (function hash-table limit
                                                   &optional (start 0))
  (let ((counter (gensym)) (ok-var (gensym)) (result-var (gensym)))
    `(let (,result-var  (,counter ,start) (,ok-var t))
       (catch 'over-limit
         (maphash #'(lambda (key value)
                      (when (and (limited-p ,limit) (>= ,counter ,limit))
                        (setf ,ok-var nil)
                        (throw 'over-limit nil))
		      (push (funcall ,function key value)
			    ,result-var)
                      (incf ,counter))
	          ,hash-table))
       (values (reverse ,result-var) ,ok-var ,counter))))

(defmacro mapcan*-over-hash-table (function hash-table)
  `(let (result)
     (maphash #'(lambda (key value)
		  (setf result
                        (append result (funcall ,function key value))))
	      ,hash-table)
     result))

(defmacro mapcan*-over-hash-table-with-limit (function hash-table limit
                                                       &optional (start 0))
  (let ((counter (gensym)) (ok-var (gensym)) (result-var (gensym)))
    `(let (,result-var  (,counter ,start) (,ok-var t))
       (catch 'over-limit
         (maphash #'(lambda (key value)
                      (when (and (limited-p ,limit) (>= ,counter ,limit))
                        (setf ,ok-var nil)
                        (throw 'over-limit nil))
		      (setf ,result-var
                            (append ,result-var (funcall ,function key value)))
                      (incf ,counter))
	          ,hash-table))
       (values ,result-var ,ok-var ,counter))))

(defun compile-ontology (name)
  #+:irs-lispworks
  (system::compile-system name :load t))

(defun compile-ocml-files (files logical-directory)
  (dolist (file files)
    (let ((full-file (translate-logical-pathname
                 (format nil "~a~a"
                         logical-directory file))))
      (compile-file full-file)
      (load full-file))))

(defvar *list-length-limit* -1
  "-1 taken as unlimited (has to be a number in java)")

(defun watch-length (list &optional (include-ellipsis-p t))
  (if (and (limited-p *list-length-limit*)
           (> (length list) *list-length-limit*))
      (if include-ellipsis-p
          (append (subseq list 0 *list-length-limit*)
                  (list +ellipsis+))
          (subseq list 0 *list-length-limit*))
      list))

(defmethod make-ocml-info-string (x)
  (if x
      (format nil "~(~a~)%" x)
      "nil%"))


(defmethod make-ocml-info-string ((list list))
  (let ((*package* (find-package "OCML"))
        (ocm-info-list (remove nil list)))
    (if ocm-info-list
        (if (and (limited-p *list-length-limit*)
                 (> (length list) *list-length-limit*))
            (format nil "~{~(~s~)%~}...%" (subseq list 0 *list-length-limit*))
            (format nil "~{~(~s~)%~}" ocm-info-list))
        "nil%")))

(defmethod make-ocml-info-string ((bc-clause bc-clause))
  (format nil "BC Clause}Consequent ~a}Antecedents ~a%"
          (bc-clause-consequent bc-clause)
          (bc-clause-antecedents bc-clause)))

;;;get pprint to NOt print (function x) as '#
(defun ocml-pprint (list stream)
  (let ((function-plist (symbol-plist 'function)))
    (setf (symbol-plist 'function) nil)
    (pprint list stream)
    (setf (symbol-plist 'function) function-plist)))

(defun make-ocml-function-body-string (list)
  (format nil "~a%"
	  (convert-multiple-line-string
           (string-downcase
            (with-output-to-string (stream)
	      (ocml-pprint list stream))))))

(defun convert-multiple-line-string (string)
  (substitute web-onto::*ocml-line-separator*
	      #\return
	      (substitute web-onto::*ocml-line-separator*
			  #\linefeed string)))

(defun insert-returns (string)
  (substitute #\return web-onto::*ocml-line-separator* string))

(defun insert-linefeeds (string)
  (substitute #\linefeed web-onto::*ocml-line-separator* string))

(defun make-ocml-option-string (list)
  (apply #'concatenate 'string
         (mapcar #'make-single-ocml-option-string list)))

(defun make-lisp-ocml-option-string (list)
  (apply #'concatenate 'string
         (mapcar #'make-lisp-single-ocml-option-string list)))

;;;must report this to enrico - if we redefine a class parent the options
;;;get screwed up
(defun clean-up-slot-options (slot-options)
  (mapcar #'(lambda (option)
              (if (listp (car option))
                  (car option)
                  option))
          slot-options))

(defun make-single-ocml-option-string (option)
  (string-downcase
   (format nil "~a}~{~{~a ~s}~}~}%" (car option) 
           (remove-duplicates (clean-up-slot-options
                               (cdr option))))))

(defun make-lisp-single-ocml-option-string (option)
  (string-downcase
   (format nil "(~a ~{~{~a ~s}~}~})%" (car option) 
           (remove-duplicates (clean-up-slot-options
                               (cdr option))))))

(defun make-ocml-slot-info-string (list)
  (apply #'concatenate 'string
         (mapcar #'make-single-slot-info-string list)))

(defun make-single-slot-info-string (slot-info)
  (let* ((name (car slot-info))
         (slot-info-structure (cdr slot-info))
         (ocml-slot-value (ocml-slot-value slot-info-structure))
         (ocml-slot-default-value (ocml-slot-default-value slot-info-structure))
         (ocml-slot-type (remove-duplicates (ocml-slot-type slot-info-structure))))
    (string-downcase
     (format nil "~a}~:[~*~;min-cardinality: ~a}~]~:[~*~;max-cardinality: ~a}~]~:[~*~*~;value: ~{~s, ~}~a}~]~:[~*~*~;default-value: ~{~s, ~}~a}~]~:[~*~;inheritance: ~a}~]~:[~*~*~;type: ~{~s, ~}~a}~]%"
             name
             (ocml-slot-min-cardinality slot-info-structure)
             (ocml-slot-min-cardinality slot-info-structure)
             (ocml-slot-max-cardinality slot-info-structure)
             (ocml-slot-max-cardinality slot-info-structure)
             ocml-slot-value
             (butlast ocml-slot-value) (car (last ocml-slot-value))
             ocml-slot-default-value
             (butlast ocml-slot-default-value) (car (last ocml-slot-default-value))
             (ocml-slot-inheritance slot-info-structure)
             (ocml-slot-inheritance slot-info-structure)
             ocml-slot-type
             (butlast ocml-slot-type) (car (last ocml-slot-type))))))

(defun make-lisp-ocml-slot-info-string (slot-info)
  (let* ((name (car slot-info))
         (slot-info-structure (cdr slot-info))
         (ocml-slot-value (ocml-slot-value slot-info-structure))
         (ocml-slot-default-value (ocml-slot-default-value slot-info-structure))
         (ocml-slot-type (remove-duplicates (ocml-slot-type slot-info-structure))))
    (string-downcase
     (format nil "(~a ~:[~*~;(min-cardinality: ~a)~]~:[~*~;(max-cardinality: ~a)~]~:[~*~*~;(value: ~{~s ~}~a)~]~:[~*~*~;(default-value: ~{~s ~}~a)~]~:[~*~;(inheritance: ~a)~]~:[~*~*~;(type: ~{~s ~}~a)~])%"
             name
             (ocml-slot-min-cardinality slot-info-structure)
             (ocml-slot-min-cardinality slot-info-structure)
             (ocml-slot-max-cardinality slot-info-structure)
             (ocml-slot-max-cardinality slot-info-structure)
             ocml-slot-value
             (butlast ocml-slot-value) (car (last ocml-slot-value))
             ocml-slot-default-value
             (butlast ocml-slot-default-value) (car (last ocml-slot-default-value))
             (ocml-slot-inheritance slot-info-structure)
             (ocml-slot-inheritance slot-info-structure)
             ocml-slot-type
             (butlast ocml-slot-type) (car (last ocml-slot-type))))))

(defun make-ocml-rules-info-string (rules)
  (apply #'concatenate 'string
         (mapcar #'make-single-ocml-rule-info-string rules)))

(defun make-single-ocml-rule-info-string (rule)
  (with-slots ((direction ocml::direction)) rule
    (string-downcase
     (format nil
             "~a}Direction: ~a}~{~a}~}%"
             (name rule) direction
             (make-clauses-info-string rule)))))

(defun make-clause-info-string (clause)
  (with-slots ((consequent ocml::consequent) (antecedents ocml::antecedents)) clause
    (string-downcase
     (format nil "Consequent: ~a}Antecedents:|~{~a|~}"
             consequent antecedents))))

(defun make-clauses-info-string (rule)
  (mapcar #'(lambda (clause) (make-clause-info-string clause))
          (clauses rule)))
 
(defun make-clause-info-string2 (clause)
  (with-slots ((consequent ocml::consequent) (antecedents ocml::antecedents)) clause
    (string-downcase
     (format nil "Consequent:}~a%Antecedents:}~{~a}~}%"
             consequent antecedents))))

(defun make-clauses-info-string2 (rule)
  (mapcar #'(lambda (clause) (make-clause-info-string2 clause))
          (clauses rule)))

          
(defun maybe-ocml-name (x)
  (if (and (slot-exists-p x 'ocml-name) (slot-boundp x 'ocml-name))
      (slot-value x 'ocml-name)
      (internal-name x)))

  ;;(slot-value x 'ocml-name))

  ;(if (and (slot-exists-p x 'ocml-name) (slot-boundp x 'ocml-name))
;      (slot-value x 'ocml-name)))
;
;
;      (if (and (slot-exists-p x 'clos::name) (slot-boundp x 'clos::name))
;          (slot-value x 'clos::name)))))
     

(defun list-class-info (name &optional structure)
  (unless structure
    (setf structure (get-ocml-class name)))
  (append (list (string-downcase (format nil "~a" name)))
          (list-class-structure-info structure)))

(defun list-detailed-class-info (name &optional structure)
  (unless structure
    (setf structure (get-ocml-class name)))
  (append (list (string-downcase (symbol-name name)))
          (list-detailed-class-structure-info structure)
          (non-arity-detailed-relation-stucture-info (get-relation name))))

(defun list-detailed-axiom-info (name &optional structure)
  (unless structure
    (setf structure (get-axiom name)))
  (list (string-downcase (symbol-name name))
        (convert-multiple-line-string
         (ocml-documentation structure))
        (convert-multiple-line-string
         (format nil "~(~:w~)" (axiom-expression structure)))))

(defun get-ocml-function (name)
  (gethash name *defined-functions*))


(defun list-detailed-relation-info (name &optional structure)
  (unless structure
    (setf structure (get-ocml-relation name)))
  (append (list (string-downcase (symbol-name name)))
          (list-detailed-relation-structure-info structure)))

;;;enrico says
;(arity :initarg :arity :initform nil :accessor arity)
;   (schema :initarg :schema :initform nil :accessor schema )
;   (constraint :initarg :constraint :initform nil)
;   (sufficient :initarg :sufficient  :initform nil :accessor sufficient)
;   (iff-def :initarg :iff-def :initform nil :accessor iff-def)
;   (prove-by  :initarg :prove-by :initform nil :accessor prove-by)
;   (no-op :initarg :no-op :initform nil)
;   (axiom-def :initarg :axiom-def  :initform nil)

;;slot-of

;;"relevant rules" defined-by-rule

;;mappings
;; upward
;; downward
;;   add
;;   remove

(defun simple-format (x)
  (string-downcase (format nil "~a%" x)))


(defun list-detailed-relation-structure-info (structure)
  (append (list (convert-multiple-line-string (documentation-info structure))
                (make-ocml-info-string (list (arity structure))))
          (non-arity-detailed-relation-stucture-info structure)))

(defun generate-relation-setofall-expression (structure)
  (let ((args nil))
    (dotimes (i (arity structure))
      (push (intern (string (gensym "?")) (find-package "OCML")) args))
  (cons (name structure) args)))

(defun get-relation-instance-expression (structure)
  (with-slots (original-form) structure
    original-form))

(defun get-relation-instances (structure)
  (let ((relation-expression (generate-relation-setofall-expression structure)))
    ;;(setofall relation-expression relation-expression)
    (careful-setofall relation-expression `(asserted ,relation-expression))))

;;use this version because can get errors if
;;an ontology uses another ontology that has been altered in the
;;;current editing session.
(defun safe-get-relation-instances (structure)
  (handler-case (get-relation-instances structure)
    ;;use serious-condition to catch stack overflows
    (serious-condition (c)
           (format t "Got an error in get-relation-instances ~a" (name structure))
           (format nil "Had problems getting relation instances ~a" c))))

(defun non-arity-detailed-relation-stucture-info (structure)
  (let* ((relation-instances (safe-get-relation-instances structure))
         (local-slot-of (remove-duplicates
                         (mapcar #'name (local-slot-of structure))))
         (slot-of (set-difference (remove-duplicates
                                   (mapcar #'name (slot-of structure)))
                                  local-slot-of)))
    (with-slots (constraint printable-lisp-fun) structure
      (list (simple-format (schema structure))
            ;;(make-ocml-info-string (sufficient structure))
            (if (sufficient structure)
                (if (listp (sufficient structure))
                    (simple-format (sufficient structure))
	          (simple-format
                   (car (bc-clause-antecedents (sufficient structure)))))
	        "nil%")
            (if (iff-def structure)
                (if (listp (iff-def structure))
                    (simple-format (iff-def structure))
                  (simple-format (car (bc-clause-antecedents (iff-def structure)))))
                "nil%")
            (if (prove-by structure)
                (if (listp (prove-by structure))
                    (simple-format (prove-by structure))
                  (simple-format (car (bc-clause-antecedents (prove-by structure)))))
                "nil%")
            ;;(make-ocml-info-string (relation-instances structure))
            (make-ocml-info-string local-slot-of)
            (make-ocml-info-string slot-of)
            (if (defined-by-rule structure)
	        (make-ocml-rules-info-string (defined-by-rule structure))
	        "nil%")
            (simple-format constraint)
            (if printable-lisp-fun
                (make-ocml-function-body-string printable-lisp-fun)
                "nil%")
            (if relation-instances
                (make-ocml-info-string relation-instances)
                "nil%")))))

(defun list-detailed-function-info (name &optional structure)
  (unless structure
    (setf structure (get-ocml-function name)))
  (append (list (string-downcase (symbol-name name)))
          (list-detailed-function-structure-info structure)))

(defun list-detailed-function-structure-info (structure)
  (with-slots (schema definition constraint body printable-lisp-fun)
    structure
    (list (convert-multiple-line-string (documentation-info structure))
	  (make-ocml-info-string (list (arity structure)))
	  (make-ocml-info-string (list schema))
          (make-ocml-info-string (list definition))
          (if constraint
	      (make-ocml-function-body-string constraint)
	    "nil%")
          (if body
              (make-ocml-function-body-string body)
              "nil%")
          (if printable-lisp-fun
              (make-ocml-function-body-string printable-lisp-fun)
              "nil%"))))

(defun list-detailed-rule-info (name &optional structure)
  (unless structure
    (setf structure (find-bc-rule name)))
  (let ((clauses (make-clauses-info-string2 structure)))
    (with-slots ((direction ocml::direction)) structure    
      (list (string-downcase (symbol-name name))
	    (convert-multiple-line-string (documentation-info structure))
	    direction
	    (if clauses
		(format nil "~{~a ~}" clauses)
		"nil%")))))



(defun list-detailed-instance-info (name class-name &optional structure)
  (unless structure
    (setf structure (find-current-direct-instance name class-name)))
  (list (string-downcase (symbol-name name))
	(convert-multiple-line-string (documentation-info structure))
        ;;(string-downcase (name (class-of structure)))
	(list-detailed-instance-structure-info structure)
        (list-instance-renaming-chains structure)))

(defun list-instance-renaming-chains (structure)
  (let ((renaming-chains (get-renaming-chains (class-of structure))))
    (if renaming-chains
        (format nil "~{~{~(~a~) ~}%~}"
                renaming-chains)
        "nil%")))

(defun list-detailed-instance-structure-info (structure)
  (make-ocml-instance-slots-info structure))


(defun list-detailed-ontology-info (name &optional structure)
  (unless structure
    (setf structure (get-ontology name)))
  (append (list (string-downcase (symbol-name name)))
	  (list-detailed-ontology-structure-info structure)))

(defun list-detailed-ontology-structure-info (structure)
  (cons (convert-multiple-line-string(documentation-info structure))
	(list-ontology-structure-info structure t)))

(defun cleanup-null-values (x)
  (if (null x) (list "no value")
      x))

(defun make-ocml-instance-slots-info (structure)
  (let ((name (name structure)))
    (convert-multiple-line-string
     (string-downcase
      (format nil "~{~{~a}~{~s, ~}~s}~}%~}%"
              (mapcar #'(lambda (slot)
                       ;(let ((value (car (slot-value structure slot))))
;		         (list slot (butlast value) (car (last value))))

                          (let ((value (cleanup-null-values
                                        (careful-setofall '?x `(,slot ,name  ?x)))
                                       ))
                            (list slot (butlast value) (car (last value)))))
	              (ocml-most-specific-class-slots (class-of structure))))))))

(defun pretty-structure-name (structure)
  (string-downcase (format nil "~a" (name structure))))

(defun pretty-string (x)
  (string-downcase (format nil "~a" x)))

(defun relation-info (structure)
  (list (pretty-structure-name structure)
        (pretty-structure-name (home-ontology structure))))

(defun function-info (structure)
  (list (pretty-structure-name structure)
        (pretty-structure-name (home-ontology structure))))

;(methods :initform nil :accessor methods)
;   (domain-slots :accessor domain-slots)            ;;;all domain slots (local + inherited)
;   (local-slots :accessor local-slots :initform nil);;;all local slots
;   (ocml-options :accessor ocml-options)
;   (slot-info-alist :initform nil :accessor slot-info-alist)))

(defun has-local-slots-p (class-name)
  (and (get-ocml-class class-name) (local-slots (get-ocml-class class-name))))


;(defun used-in-slot-info (class-name type instance-name)
;  (let* ((class-structure (get-ocml-class class-name))
;         (local-slots (local-slots class-structure))
;         (slot-info-alist (slot-info-alist class-structure)))
;    (mapcan #'(lambda (slot)
;                (let ((slot-info (assoc slot slot-info-alist)))
;                  ;;(format t "~a ~a~%" (car slot-info) (get-slot-info-from-structure type (cdr slot-info)))
;                  (when (and slot-info
;                             (eq (car (get-slot-info-from-structure type (cdr slot-info))) instance-name))
;		    (list class-name (car slot-info)))))
;            local-slots)))

(defun used-in-slot-info (class-name type instance-name)
  (let ((class-structure (get-ocml-class class-name)))
    (when class-structure
      (mapcan #'(lambda (option)
                  (let* ((option-name (car option))
                         (option-info (assoc type (cdr option)))
                         (option-info-value (second option-info)))
                    (when (eq option-info-value instance-name)
		      (list class-name option-name))))
              (ocml-options class-structure)))))

;(defun slot-info-uses (class-name type use-set)
;  (let* ((class-structure (get-ocml-class class-name))
;         (local-slots (local-slots class-structure))
;         (slot-info-alist (slot-info-alist class-structure)))
;    (mapcan #'(lambda (slot)
;                (let* ((slot-info (assoc slot slot-info-alist))
;                       (slot-info-value (and slot-info
;                                             (car (get-slot-info-from-structure type (cdr slot-info))))))
;                  (when (and slot-info-value (find slot-info-value use-set))
;		    (list slot-info-value (car slot-info)))))
;            local-slots)))

(defun slot-info-uses (class-name type use-set)
  (let ((class-structure (get-ocml-class class-name)))
    (when class-structure
      (mapcan #'(lambda (option)
                  (let* ((option-name (car option))
                         (option-info (assoc type (cdr option)))
                         (option-info-value (second option-info)))
                    (when (and option-info-value (find option-info-value use-set))
		      (list option-info-value option-name))))
              (ocml-options class-structure)))))

(defun default-value-of (instance-name)
  (mapcan*-over-hash-table
   #'(lambda (class-name class)
       (declare (ignore class))
       (used-in-slot-info class-name :default-value instance-name))
  *domain-classes*))

(defun default-value-of-class (class-names instance-name)
  (mapcan #'(lambda (class-name)
              (used-in-slot-info class-name :default-value instance-name))
          class-names))

(defun type-of-class (class-names type-of-class-name)
  (mapcan #'(lambda (class-name)
              (used-in-slot-info class-name :type type-of-class-name))
          class-names))

(defun class-default-values (instance-names class-name)
  (slot-info-uses class-name :default-value instance-names))

(defun class-types (class-names class-name)
  (slot-info-uses class-name :type class-names))

(defun instance-of-class (instance-name)
  (let ((instance (find-current-instance instance-name)))
    (when instance
      (name (class-of instance)))))

(defun documentation-info (structure)
  (let ((doc (ocml-documentation structure)))
    (if (string= doc "")
	"nil"
      (convert-multiple-line-string doc))))

(defun get-class-info-for-new-instance (name)
  (let ((structure (get-domain-class name)))
    (when structure
      (internal-get-class-info-for-instance structure))))

(defun instance-structures (instance-name)
  (let* ((class-name (instance-of-class instance-name))
         (class-structure (get-domain-class class-name))
         (instance-structure (find-current-instance instance-name class-name)))
    (values class-structure instance-structure class-name)))


(defun get-instance-info-for-edit (instance-name)
  (multiple-value-bind (class-structure instance-structure)
      (instance-structures instance-name)
    (when class-structure
      (internal-get-class-info-for-instance class-structure instance-structure))))

(defun all-subclass-names (class-name)
  (remove-duplicates (mapcar #'name (subclasses (get-domain-class class-name)))))

(defun all-current-subclass-names (class-name)
  (remove-duplicates
   (mapcar #'name (current-subclasses (get-domain-class class-name)))))

(defun all-superclass-names (class-name)
  (remove-duplicates
   (mapcar #'name (domain-superclasses (get-domain-class class-name)))))
       
 ;;;some classes may not exist if from a different ontology
(defun filter-unaccessible-classes (class-names)
  (mapcan #'(lambda (x)
              (when (get-domain-class x)
                (list x)))
          class-names))


(defun filter-unaccessible-slot-types (types)
  (mapcan #'(lambda (x)
              (when (or (listp types) (get-domain-class x))
                (list x)))
          types))

(defun parse-type-name (type-name)
  (if (and (listp type-name) (eq (car type-name) 'or))
      (cdr type-name)
      (list type-name)))

(defun sorted-descendent-slot-type-names (single-slot-info-alist)
  (let ((type-names (ocml-slot-type (cdr single-slot-info-alist))))
    (remove-duplicates
     (append
      (mapcan #'(lambda (type-name)
                  (if (listp type-name)
                      ;;either or or and
                      (copy-list (cdr type-name))
                      (list type-name)))
              type-names)
      (sort
       (mapcan #'(lambda (type-name)
                   (process-slot-type-name type-name)
                   )
               type-names)
       #'string<))
     :from-end t)))

(defun get-slot-default-values (class-name slot-name)
  (let ((structure (get-domain-class class-name)))
    (when structure      
      (multiple-value-bind (values defaults)
          (get-slot-values-from-class-structure
           structure slot-name)
        (declare (ignore values))
        defaults))))

;(defun careful-all-instances (name)
;  (when (get-ocml-class name)
;    (all-instances name)))

(defun or-type-p (type)
  (and (listp type) (eq (car type) 'or)))

(defun and-type-p (type)
  (and (listp type) (eq (car type) 'and)))

(defun careful-all-instances-from-type (type)
  (remove-duplicates (careful-instances-from-type type)))

;;;mapcan included because the class thing in the akt-support-ontology had
;;;some spurious looking instances

;;(#:?A174006174012 . #:?B174007174013) 
;;(:SET . #:?ELEMENTS173919) 
;;(:SET . #:?ELEMENTS173899) 

(defun careful-generalised-all-instances-from-type (type)
  (mapcan #'(lambda (x) (when (and (atom x) (symbolp x))
                          (list x)))
          (careful-setofall '?x (list type '?x))))

(defun careful-direct-instances-from-type (type)
  (remove-duplicates (careful-instances-from-type type t)))

(defun careful-setofall (expression goal &optional env)
  (handler-case
      (setofall expression goal env)
    (serious-condition (c) nil)))

(defun careful-instances-from-type (type &optional direct-p)
  (cond ((or-type-p type)
         (remove-duplicates
          (mapcan #'(lambda (type)
                      (copy-list (careful-instances-from-type type)))
                  (cdr type))))
        ((and-type-p type)
         (remove-duplicates
          (apply #'intersection
                 (mapcar #'careful-instances-from-type
                         (cdr type)))))
        (t (when (get-ocml-class type)
             (let ((result (if direct-p
                               (all-current-direct-instances type)
                               (all-current-instances type))))
               (if result
                   result
                   (get-instances-by-inference type)))))))

(defun get-instances-by-inference (type)
  (remove-duplicates
   (apply #'append
          (mapcar #'careful-instances-from-type
                  (mapcan #'(lambda (instance-name)
                              (when (find-current-instance instance-name)
                                (list (find-current-instance instance-name))))
                          (careful-setofall '?x (list type '?x)))))))

(defun process-slot-type-name (type-name) 
  (cond ((listp type-name)
         ;;either or or and
         (if (eq (car type-name) 'or)
             (mapcan #'process-slot-type-name (cdr type-name))
             (list type-name)))
        ((get-ocml-class type-name)
         (cons type-name
                 (copy-list
                  (filter-unaccessible-classes
                   (all-subclass-names
                    type-name)))))))

(defun internal-get-class-info-for-instance (structure &optional instance-structure)
  (let* ((obsolete-names (obsolete-names structure))
         ;;(class-name (name structure))
         (slot-info
          (if obsolete-names
              (mapcan #'(lambda (slot)
                          (unless (find (car slot) obsolete-names)
                            (list slot)))
                      (slot-info-alist structure))
              (slot-info-alist structure))))
    (when slot-info
      (mapcar #'(lambda (slot)
		  (let* ((type-names
                          (filter-unaccessible-slot-types
                           (ocml-slot-type (cdr slot))))
			 (sorted-type-names
                          (when type-names
                            (cons (car type-names)
				  (remove
                                   (car type-names)
                                   (sort
                                    (remove-duplicates
                                     (mapcan #'process-slot-type-name
                                             type-names))
                                    #'string<)))))
                         (all-instances
                          (when type-names
                            ;;(mapcar #'name
                            ;;(careful-all-instances-from-type
                            (web-onto::sorted-careful-generalised-all-instances-from-type
                             (car sorted-type-names)))))
                    (format nil "~a{~:[~a~;~{~a%~}~]{~{~s%~}{~{~a%~}"
                            (car slot)
                            type-names sorted-type-names
                            (if instance-structure
                                (let ((values
                                       ;;(get-slot-values instance-structure (car slot))
                                       ;;now use only the local slot values
                                       ;;because default values could accidentally
                                       ;;become values
                                       (get-local-slot-values instance-structure
                                                              (car slot))))
                                  (if values
                                      (check-instance-values-for-dialog values)
                                      (list 'null-value)))
                                ;;do not return default values now
                                ;;because they can accidentally become values
                                '(null-value)
                                ;(or (all-class-slot-values class-name
;                                                           (car slot))
;                                          ;;(ocml-slot-default-value (cdr slot))
;                                          ;;(ocml-slot-value (cdr slot))
;                                          '(null-value))

                                )
                            (if all-instances
                                all-instances
                                (list nil)))))
              slot-info))))

(defun check-instance-values-for-dialog (values)
  (mapcar #'(lambda (value)
              (if (stringp value)
                  (convert-multiple-line-string value)
                value))
          values))

(defun get-renaming-chains (structure)
  (remove-duplicates (renaming-chains structure)
                     :test #'equal))

(defun internal-slot-options (structure)
  (let* ((local-slots (ocml-most-specific-class-local-slots structure))
         (ocml-options (ocml-most-specific-options structure))
         (inherited-slots
          (set-difference
           (domain-slots structure)
           local-slots))
         (inherited-slot-info
          (mapcan #'(lambda (x)
                      (when (find (car x)  inherited-slots)
		        (list (copy-list x))))
                  (slot-info-alist structure))))
    (concatenate 'string
                  (if ocml-options
                      (convert-multiple-line-string
                       (make-ocml-option-string ocml-options))
                    "")
                  (if inherited-slot-info
                      (convert-multiple-line-string
                       (make-ocml-slot-info-string inherited-slot-info))
                    ""))))

#+:irs-lispworks
(defun list-detailed-class-structure-info (structure)
  (let* ((direct-superclasses (direct-domain-superclasses structure))
         (renaming-chains (get-renaming-chains structure))
         (local-slots (ocml-most-specific-class-local-slots structure))
         (ocml-options (ocml-most-specific-options structure))
         (inherited-slots
          (set-difference ;;(ocml-most-specific-class-slots structure)
                          ;;get the renamed slots as well because these
                          ;;have slot info
                          (domain-slots structure)
                          local-slots))
         (direct-instances (mapcar #'name ;;(get-direct-instances structure)
                                   (all-current-direct-instances (name structure))))
         (inherited-slot-info
          (mapcan #'(lambda (x)
                      (when (find (car x)  inherited-slots)
		        (list (copy-list x))))
                  (slot-info-alist structure))))
    (with-slots ((direct-subclasses clos::direct-subclasses))
      structure
      (list (convert-multiple-line-string (documentation-info structure))
	    (if direct-superclasses
		(make-ocml-info-string (remove-duplicates
                                        (mapcar #'maybe-ocml-name
					        direct-superclasses)
                                        :test #'string-equal))
		"nil%")
	    (if direct-subclasses
		(make-ocml-info-string (remove-duplicates
                                        (mapcar #'maybe-ocml-name
					        direct-subclasses)
                                        :test #'string-equal))
		"nil%")
            (if inherited-slots
                (make-ocml-info-string inherited-slots)
                "nil%")
            (if local-slots
                (make-ocml-info-string local-slots)
                "nil%")
            ;;;details on local slots
            (if ocml-options
                (convert-multiple-line-string
                 (make-ocml-option-string ocml-options))
                "nil%")
            ;;inherited slot details
            (if inherited-slot-info
                (convert-multiple-line-string
                 (make-ocml-slot-info-string inherited-slot-info))
                "nil%")
            (if direct-instances
                 (make-ocml-info-string direct-instances)
                "nil%")
            (if renaming-chains
                (format nil "~{~{~(~a~) ~}%~}"
                        renaming-chains)
                "nil%")
            ))))

(defun list-ontology-structure-info (structure
                                     &optional include-author-editors-type-p)
  (append (list (if (ontology-includes structure)
                    (make-ocml-info-string (mapcar #'name
                                                   (ontology-includes structure)))
	            "None")
                (if (ontology-included-by structure)
                    (make-ocml-info-string (mapcar #'name
                                                   (ontology-included-by structure)))
                    "None"))
	  (when include-author-editors-type-p
	    ;;ontology-author should always have a value
	    (list (ontology-author structure)
	          (if (ontology-allowed-editors structure)
                      (ontology-allowed-editors structure)
                      "None")
                  (format nil "~:(~a~)" (ontology-type structure))))))

#+:irs-lispworks
(defun list-class-parents-and-children (structure)
  (let ((direct-superclasses (direct-domain-superclasses structure)))
    (with-slots ((direct-subclasses clos::direct-subclasses))
      structure
      (list (if direct-superclasses
		(make-ocml-info-string (remove-duplicates
                                        (mapcar #'maybe-ocml-name
					        direct-superclasses)
                                        :test #'string-equal))
		"nil%")
	    (if direct-subclasses
		(make-ocml-info-string (remove-duplicates
                                        (mapcar #'maybe-ocml-name
					        direct-subclasses)
                                        :test #'string-equal))
		"nil%")))))

(defun list-class-structure-info (structure)
  (append (list-class-parents-and-children structure)
	  (list (pretty-structure-name (home-ontology structure)))))

(defun list-tasks (view-only-items-in-current-ontology
                   include-base-ontology-p
                   &optional (number-of-items 0)
                   (ontology *current-ontology*)
                   describe-p name-only structure-only include-wsmo-base-ontology-p)
  (list-classes view-only-items-in-current-ontology
                include-base-ontology-p
                number-of-items ontology describe-p name-only structure-only
                'task 'problem-solving-method
                nil include-wsmo-base-ontology-p))

(defun list-problem-solving-methods (view-only-items-in-current-ontology
                                     include-base-ontology-p
                                     &optional (number-of-items 0)
                                     (ontology *current-ontology*)
                                     describe-p
                                     name-only structure-only
                                     include-wsmo-base-ontology-p)
  (list-classes view-only-items-in-current-ontology
                include-base-ontology-p
                number-of-items
                ontology describe-p name-only structure-only 'problem-solving-method
                nil nil include-wsmo-base-ontology-p))

(defvar *wsmo-base-ontology-name* 'ocml::wsmo)


;;now exclude wsmo ontology unless otherwise specified
(defun current-ontologies (ontology view-only-items-in-current-ontology
                                    include-base-ontology-p
                                    &optional include-wsmo-base-ontology-p)
  (cons ontology
        (unless view-only-items-in-current-ontology
          (if include-wsmo-base-ontology-p
              (internal-current-ontologies ontology include-base-ontology-p)
            (remove (get-ontology *wsmo-base-ontology-name*)
                    (internal-current-ontologies ontology include-base-ontology-p))))))

(defun internal-current-ontologies (ontology include-base-ontology-p)
  (if include-base-ontology-p
      (ontology-ancestors ontology)
    (remove (get-ontology *base-ontology-name*)
            (ontology-ancestors ontology))))
                  


(defun list-classes (view-only-items-in-current-ontology
                     include-base-ontology-p
                     &optional (number-of-items 0)
                     (ontology *current-ontology*)
                     describe-p name-only structure-only
                     subclass-of not-subclass-of root-classes-only-p
                     include-wsmo-base-ontology-p)
  (let ((current-ontologies
         (current-ontologies ontology
                             view-only-items-in-current-ontology
                             include-base-ontology-p include-wsmo-base-ontology-p))
        (number-of-classes number-of-items) (ok-p t)
        class-items)
    (when (and subclass-of (symbolp subclass-of))
      (setf subclass-of (list subclass-of)))
    (when (and not-subclass-of (symbolp not-subclass-of))
      (setf not-subclass-of (list not-subclass-of)))
    (catch 'over-limit
      (maphash
       #'(lambda (key value)
           (when (web-onto::filter key value)
             (when (and (limited-p *list-length-limit*)
                        (> number-of-classes *list-length-limit*))
               (setf ok-p nil)
               (throw 'over-limit nil))
             (when (and (find (home-ontology value) current-ontologies :test #'eq)
                        (or (null subclass-of)
                            (find (name value) subclass-of)
                            (intersection 
                             subclass-of
                             (mapcar #'name 
                                     (domain-superclasses value))))
                        (or (null not-subclass-of)
                            (and
                             (not (find (name value) not-subclass-of))
                             (not (intersection not-subclass-of
                                                (mapcar #'name 
                                                        (domain-superclasses 
                                                         value))))))
                        (or (null root-classes-only-p)
                            (null (domain-superclasses value))))
               (incf number-of-classes)
               (let ((new-class-item (cond (name-only key)
                                           (structure-only value)
                                           (t (if describe-p
                                                  (cons (list-class-info key value)
                                                        (describe value))
                                                (list-class-info key value))))))
                 (when new-class-item
                   (push new-class-item class-items))))))
       *domain-classes*))
    (values class-items ok-p number-of-classes)))

(defun list-axioms (view-only-items-in-current-ontology
                     include-base-ontology-p
                     &optional (number-of-items 0)
                     (ontology *current-ontology*)
                     include-wsmo-base-ontology-p)
  (let ((current-ontologies
         (current-ontologies ontology
                             view-only-items-in-current-ontology
                             include-base-ontology-p 
                             include-wsmo-base-ontology-p))
        (number-of-axioms number-of-items) (ok-p t)
        axiom-items)
    (catch 'over-limit
      (maphash
       #'(lambda (key value)
           (when (web-onto::filter key value)
             (when (and (limited-p *list-length-limit*)
                        (> number-of-axioms *list-length-limit*))
               (setf ok-p nil)
               (throw 'over-limit nil))
             (when (find (home-ontology value) current-ontologies :test #'eq)
               (incf number-of-axioms)
               (push (list (string-downcase (symbol-name key))
                           (string-downcase
                                (symbol-name (name (home-ontology value)))))
                     axiom-items))))
       *axioms*))
    (values axiom-items ok-p number-of-axioms)))

(defun list-instance-info (instance-structure)
  (list (pretty-structure-name instance-structure)
        (pretty-structure-name (class-of instance-structure))
        (pretty-structure-name (home-ontology instance-structure))))

(defun list-instances (view-only-items-in-current-ontology
                       include-base-ontology-p
                       &optional (number-of-items 0)
                       (ontology *current-ontology*) describe-p
                       include-wsmo-base-ontology-p)
  (declare (ignore describe-p))
  (let ((current-ontologies
         (current-ontologies ontology
                             view-only-items-in-current-ontology
                             include-base-ontology-p include-wsmo-base-ontology-p))
        (instances nil) (number-of-instances number-of-items) (ok-p t))
    (catch 'over-limit
      (maphash
       #'(lambda (key value)
           (declare (ignore value))
           (mapc #'(lambda (instance)
                     (when (and (limited-p *list-length-limit*)
                                (> number-of-instances *list-length-limit*))
                       (setf ok-p nil)
                       (throw 'over-limit nil))
                     (when (and (find (home-ontology instance)
                                      current-ontologies :test #'eq)
                                (web-onto::filter (name instance) instance))
                       (setf instances (cons instance instances))
                       (incf number-of-instances)))
                 (all-current-direct-instances key)))
       *domain-classes*))
    (values (mapcar #'list-instance-info instances)
            ok-p number-of-instances)))

(defun list-rule-info (rule-structure)
  (list (pretty-structure-name rule-structure)
        (pretty-structure-name (home-ontology rule-structure))))
        

(defun list-rules (view-only-items-in-current-ontology
                   include-base-ontology-p
                   &optional (number-of-items 0)
                   (ontology *current-ontology*) describe-p
                   include-wsmo-base-ontology-p)
  (let ((current-ontologies
         (current-ontologies ontology
                             view-only-items-in-current-ontology
                             include-base-ontology-p 
                             include-wsmo-base-ontology-p))
        (number-of-rules number-of-items) (rules nil)
        (ok-p t))
    (catch 'over-limit
      (maphash
       #'(lambda (key value)
           (when (web-onto::filter key value)
             (when (and (limited-p *list-length-limit*)
                        (> number-of-rules *list-length-limit*))
               (setf ok-p nil)
               (throw 'over-limit nil))
             (when (find (home-ontology value) current-ontologies :test #'eq)
               (incf number-of-rules)
               (push (list-rule-info value) rules))))
       *bc-rules*))
    (values rules ok-p number-of-rules)))

(defun list-functions (view-only-items-in-current-ontology
                       include-base-ontology-p
                       &optional (number-of-items 0)
                       (ontology *current-ontology*)
                       include-wsmo-base-ontology-p)
  (let ((current-ontologies
         (current-ontologies ontology
                             view-only-items-in-current-ontology
                             include-base-ontology-p include-wsmo-base-ontology-p))
        (number-of-functions number-of-items) (functions nil)
        (ok-p t))
    (catch 'over-limit
      (maphash
       #'(lambda (key value)
           (when (web-onto::filter key value)
             (when (and (limited-p *list-length-limit*)
                        (> number-of-functions *list-length-limit*))
               (setf ok-p nil)
               (throw 'over-limit nil))
             (when (and (not (procedure-p value))
                        (find (home-ontology value) current-ontologies
                              :test #'eq))
               (incf number-of-functions)
               (push (function-info value) functions))))
       *defined-functions*))
    (values functions ok-p number-of-functions)))

(defun list-procedures (view-only-items-in-current-ontology
                        include-base-ontology-p
                        &optional (number-of-items 0)
                        (ontology *current-ontology*)
                        include-wsmo-base-ontology-p)
  (let ((current-ontologies
         (current-ontologies ontology
                             view-only-items-in-current-ontology
                             include-base-ontology-p 
                             include-wsmo-base-ontology-p))
        (number-of-procedures number-of-items) (procedures nil)
        (ok-p t))
    (catch 'over-limit
      (maphash
       #'(lambda (key value)
           (when (web-onto::filter key value)
             (when (and (limited-p *list-length-limit*)
                        (> number-of-procedures *list-length-limit*))
               (setf ok-p nil)
               (throw 'over-limit nil))
             (when (and (procedure-p value)
                        (find (home-ontology value) current-ontologies
                              :test #'eq))
               (incf number-of-procedures)
               (push (function-info value) procedures))))
       *defined-functions*))
    (values procedures ok-p number-of-procedures)))

(defun list-relations (view-only-items-in-current-ontology
                       include-base-ontology-p
                       &optional (number-of-items 0)
                       (ontology *current-ontology*)
                       include-wsmo-base-ontology-p)
  (let ((current-ontologies
         (current-ontologies ontology
                             view-only-items-in-current-ontology
                             include-base-ontology-p include-wsmo-base-ontology-p))
        (number-of-relations number-of-items) (relations nil)
        (ok-p t))
    (catch 'over-limit
      (maphash
       #'(lambda (key value)
           (when (web-onto::filter key value)
             (when (and (limited-p *list-length-limit*)
                        (> number-of-relations *list-length-limit*))
               (setf ok-p nil)
               (throw 'over-limit nil))
             (when (and (find (home-ontology value) current-ontologies
                              :test #'eq)
                        (not (get-domain-class key)))
               (incf number-of-relations)
               (push (relation-info value) relations))))
       *defined-relations*))
    (values relations ok-p number-of-relations)))

(defun relation-instance-predicate (relation-instance)
  (with-slots (predicate) relation-instance
    predicate))

(defun list-relation-instances (view-only-items-in-current-ontology
                                include-base-ontology-p
                                &optional (number-of-items 0)
                                (ontology *current-ontology*)
                                include-wsmo-base-ontology-p)
  (let ((current-ontologies
         (current-ontologies ontology
                             view-only-items-in-current-ontology
                             include-base-ontology-p include-wsmo-base-ontology-p))
        (number-of-relation-instances number-of-items) (relation-instances nil)
        (ok-p t))
    (catch 'over-limit
      (maphash
       #'(lambda (key value)
           (unless (get-domain-class key)
             (let ((instances (get-direct-relation-instances value)))
               (mapc #'(lambda (instance)
                         (when (web-onto::filter key instance)
                           (when (and (limited-p *list-length-limit*)
                                      (> number-of-relation-instances *list-length-limit*))
                             (setf ok-p nil)
                             (throw 'over-limit nil))
                           (when (find (home-ontology instance) current-ontologies
                                       :test #'eq)
                             (incf number-of-relation-instances)
                             (setf relation-instances
                                   (cons (list
                                          (format nil "(~(~a~) ~(~{~a ~}~{~a~}~))"
                                                  (relation-instance-predicate instance)
                                                  (butlast (args instance))
                                                  (last (args instance)))
                                          (string-downcase (symbol-name
                                                            (name
                                                             (home-ontology instance)))))
                                         relation-instances)))))
                     instances))))
       *defined-relations*))
    (values relation-instances ok-p number-of-relation-instances)))

;;;*defined-relations* not (get-domain-class relation)

 ;; (list-hash-table *domain-classes*))


 ; (Let ((dir (ontology-directory *current-ontology*)))
;    (setf (ontology-relations dir) *defined-relations*)
;    (setf (ontology-expressions dir) *expressions*)
;    (setf (ontology-functions dir) *defined-functions*)
;    ;;;(setf (ontology-operators dir) *operators*)
;    (setf (ontology-bc-rules dir) *bc-rules*)
;   ;;;; (setf (ontology-fc-rule-packets dir) *rule-packets*)
;    
;    
;    (setf (ontology-classes dir) *domain-classes*)))


(defun web-onto::get-ontology (name)
  (get-ontology name))


(defun web-onto::loaded-ontology-p (name)
  (assoc name *all-ontologies*))


(defun ontology-ancestors (ontology &optional collected)
  (let ((includes (set-difference (ontology-includes ontology)
                                  collected)))
    (setf collected (append includes collected))
    (cond (includes
           (mapc #'(lambda (include)
                     (setf collected (ontology-ancestors include collected)))
                 includes)
           collected)
          (t collected))))
  

(defun get-home-ontology-from-dspec (dspec)
  (case (car dspec)
    ((def-class) (home-ontology (get-ocml-class (second dspec))))
    ((def-instance) (home-ontology (find-current-instance (second dspec))))
    ((def-relation) (home-ontology (get-ocml-relation (second dspec))))
    ((def-function) (home-ontology (get-ocml-function (second dspec))))
    ((def-procedure) (home-ontology (get-ocml-function (second dspec))))
    ((def-rule) (home-ontology (get-rule (second dspec))))
    ((def-ontology) (home-ontology (get-ontology (second dspec))))))

(defun class-structure-from-query-result (x)
  (get-domain-class (cdar x)))

;;(ask-top-level `(tackles-task-type ?x ,task-name) :all t :query-mode nil)))
(defun task-children (task-name)
  (if (web-onto::executable-task-p task-name)
      (get-task-subtasks task-name)
      (mapcar #'get-domain-class
              (careful-setofall '?x `(tackles-task-type ?x ,task-name)))))

(defun get-generic-subtasks (x)
  (let ((result (findany '?x `(has-generic-subtasks ,x ?x))))
    (if (or (null result) (eq result :fail) (eq result :nothing))
	nil
        (if (listp result)
            (mapcar #'get-domain-class result)
            (list (get-domain-class result))))))

(defun get-task-subtasks (x)
  (get-generic-subtasks x))
  
(defun psm-children (psm-name)
  (get-generic-subtasks psm-name))

(defun task-or-psm-children (structure)
  (let ((name (name structure)))
    (if (web-onto::task-p name)
        (task-children name)
        (psm-children name))))


(defun psm-parents (psm-name)
  (mapcar #'get-domain-class
          (careful-setofall '?x `(tackles-task-type ,psm-name ?x))))

(defun task-parents (task-name)
  (mapcar #'get-domain-class
          (careful-setofall '?x
                            `(and (has-generic-subtasks ?x ?l)
                                  (member ,task-name ?l)))))
  
  ;(let ((result (ask-top-level `(has-generic-subtasks ?x ,task-name) :all t :query-mode nil)))
;    (if (eq result :fail)
;        nil
;        (mapcar #'class-structure-from-query-result result))))

(defun task-or-psm-parents (structure)
  (let ((name (name structure)))
    (if (web-onto::task-p name)
        (task-parents name)
        (psm-parents name))))

(defun list-task-psm-parents-and-children (structure)
  (let ((parents (task-or-psm-parents structure))
        (children (task-or-psm-children structure)))
    (list (if parents
	      (make-ocml-info-string (remove-duplicates
				      (mapcar #'maybe-ocml-name
					      parents)
				      :test #'string-equal))
	      "nil%")
	  (if children
	      (make-ocml-info-string (remove-duplicates
				      (mapcar #'maybe-ocml-name
					      children)
				      :test #'string-equal))
	      "nil%"))))


(defun get-methods-applicable-to-task (task-structure)
  (let* ((task-type (name task-structure))
         (home-ontology (home-ontology task-structure))
         (ontologies (cons home-ontology
                           (mapcar #'cdr
                                   (filter *all-ontologies*
                                           #'(lambda (ontology)
                                               (and (eq (ontology-type (cdr ontology)) :method)
                                                    (member home-ontology
			                                    (ontology-includes (cdr ontology))))))))))
    (remove-duplicates
     (mapcan #'(lambda (ontology)
                 (select-ontology (name ontology))
                 (let ((methods
                        (careful-setofall '?x `(tackles-task-type ?x ,task-type))))
                   (when methods
                     (mapcar #'get-domain-class methods))))
             ontologies))))

(defun construct-task-method-hierarchy-from-task (task-structure)
  (let ((methods (get-methods-applicable-to-task task-structure)))
    (cons task-structure
          (mapcar #'(lambda (method)
                      (construct-task-method-hierarchy-from-method method))
                  methods))))


(defun construct-task-method-hierarchy-from-method (method-structure)
  (let ((tasks (get-method-subtasks method-structure)))
    (cons method-structure
          (mapcar #'(lambda (task)
                      (construct-task-method-hierarchy-from-task task))
                  tasks))))

(defun get-method-subtasks (method-structure)
  (let ((method-name (name method-structure)) result)
    (select-ontology (name (home-ontology method-structure)))
    (setf result (findany '?x `(has-generic-subtasks ,method-name ?x)))
    (if (eq result :nothing)
        nil
        (mapcar #'get-domain-class result))))


(defun convert-type-to-defining-type-name (name)
  (case name
    ((classes class tasks task
                    problem_solving_methods problem_solving_method) 'def-class)
    ((functions function) 'def-function)
    ((procedures procedure) 'def-procedure)
    ((relations relation) 'def-relation)
    ((rules rule) 'def-rule)
    ((instances instance) 'def-instance)
    ((axioms axiom) 'def-axiom)
    ((relation_instance relation_instances) 'def-relation-instances)
    ((soap-binding) 'def-irs-soap-bindings)
    ((all) 'unknown)))

(defun convert-defining-type-name-to-type (name)
  (case name
    ((def-class) 'class)
    ((def-function) 'function)
    ((def-procedure) 'procedure)
    ((def-relation) 'relation)
    ((def-rule) 'rule)
    ((def-instance) 'instance)
    ((def-relation-instances) 'relation_instance)
    ((def-axiom) 'axiom)))

(defun get-unknown-type (name)
  (cond ((get-ocml-class name)
	 'class)
	((get-relation name)
	 'relation)
	((get-function name)
	 (if (procedure-p (get-function name))
	     'procedure
	   'function))
	((find-current-instance name)
	 'instance)
        ((get-axiom name)
         'axiom)
        ((get-relation-instance name)
         'relation_instance)
	((get-rule name)
	 'rule)))

(defun delete-ocml-object (type name &optional instance-class-name)
  (case type
    ((classes class tasks task
	      problem_solving_methods problem_solving_method)
     (maybe-remove-class name))
    ((functions function procedures procedure) (maybe-remove-function name))
    ((relations relation)
     (maybe-remove-relation name))
    ((rules rule) (maybe-remove-rule name))
    ((instances instance) (maybe-remove-instance name instance-class-name))
    ((relation_instance relation_instances)
     (maybe-remove-relation-instance name))
    ((axiom axioms)
     (maybe-remove-axiom name))
    ((soap-binding) (values t nil))
    ((all) 
     (cond ((get-ocml-class name)
	    (maybe-remove-class name))
	   ((get-relation name)
	    (maybe-remove-relation name))
	   ((get-function name)
	    (maybe-remove-function name))
	   ((find-current-instance name instance-class-name)
	    (maybe-remove-instance name instance-class-name))
           ((get-axiom name)
            (maybe-remove-axiom name))
           ((get-relation-instance name)
            (maybe-remove-relation-instance name))
	   ((get-rule name)
	    (maybe-remove-rule name))))))

(defun maybe-remove-relation-instance (name)
  (let ((relation-instance (get-relation-instance name)))
    (if relation-instance
        (cond ((not (eq (home-ontology relation-instance)
                        *current-ontology*))
               (values
                nil
                (format nil
                        "The relation instance ~s has been defined in ontology ~s, which is different from the 
                         currently selected ontology, ~s.  Imported definitions cannot be renamed  
                         in a sub-ontology"
                        name (home-ontology relation-instance) *current-ontology*)))
              (t
               (progn (unassert1 name) t)))
        (values nil (format nil "Couldn't find the relation instance ~s"
                            name)))))
        

(defun maybe-remove-axiom (name)
  (let ((axiom (get-axiom name)))
    (if axiom
        (cond ((not (eq (home-ontology axiom)
                        *current-ontology*))
               (values
                nil
                (format nil
                        "Axiom ~s has been defined in ontology ~s, which is different from the 
                         currently selected ontology, ~s.  Imported definitions cannot be renamed  
                         in a sub-ontology"
                        name (home-ontology axiom) *current-ontology*)))
              (t
               (progn (remove-axiom-in-all-ontologies name axiom) t)))
        (values nil (format nil "Couldn't find the axiom ~s"
                            name )))))

(defun maybe-remove-function (name)
  (if (get-function name)
      (progn (remove-function name) t)
    (values nil (format nil "Couldn't find the function ~(~a~)" name))))

(defun maybe-remove-instance (instance-name &optional instance-class-name)
  (let ((instance (find-current-instance instance-name instance-class-name)))
    (if instance
        (let ((class-name (or instance-class-name
                              (name (class-of instance)))))
	  (progn (ocml-eval-gen `(unassert (,class-name ,instance-name))) t))
        (values nil (format nil "Couldn't find the instance ~(~a~)" instance-name)))))

(defun maybe-remove-class (name)
  (let ((class (get-ocml-class name)))
    (if class 
        (cond ((not (eq (home-ontology class) *current-ontology*))
               (values nil
                       (format nil
                               "Class ~(~s~) has been defined in ontology ~(~s~), which is different from the 
                         currently selected ontology, ~(~s~).  Imported definitions cannot be deleted."
                               name (home-ontology class) *current-ontology*)))
              ((subclasses  class)
               (values nil
                       (format nil
                               "Class ~(~s~) cannot be deleted: subclasses exist for this class" 
                               name)))
              (t
               (let ((warning "")
                     (action nil)
                     (current-instances (get-current-direct-instances class)))
                 (when current-instances
                   (let ((instance-names (mapcar #'name current-instances)))
                     (setf warning
                           (format nil
                                   "The existing instances ~{~(~a~), ~}~(~a~) of the class ~(~s~) have been deleted"
                                   (butlast instance-names)
                                   (car (last instance-names))
                                   name)
                           action
                           (format nil
                                   "delete-instances~a~(~a~)[~{~(~(~a~)[~)~}"
                                   web-onto::*ocml-line-separator*
                                   name
                                   instance-names))))
                 (remove-class-in-all-ontologies name class (get-relation name))
                 (values t warning action))))
        (values nil (format nil "Couldn't find the class ~(~a~)" name)))))

(defun maybe-remove-relation (name)
  (let ((relation (get-relation name)))
    (if relation
	(with-slots (name relation-instances upward-mapping?
                          downward-add-exp downward-remove-exp 
                          slot-of defined-by-rule fc-nodes
                          home-ontology)
          relation
          (let ((class (get-ocml-class name)))
            (cond ((not (eq home-ontology *current-ontology*))
                   (values nil
                           (format nil "Relation ~(~s~) has been defined in ontology ~(~s~), which is different from the 
                         currently selected ontology, ~(~s~).  Imported definitions cannot be deleted."
                                   name home-ontology *current-ontology*)))
                  ((and class
                        (not (can-remove-class?  class)))
                   (values nil
                           (format nil "Cannot delete relation ~(~s~): associated class cannot be deleted"
                                   name)))
                  
                  (slot-of
                   (values nil
                           (format nil 
                                   (string-append "Cannot delete relation ~(~s~), which is a slot of classes ~(~s~)."
                                                  " These classes need to be deleted first")
                                   name slot-of)))
                  
                  (fc-nodes
                   (values nil
                           (format nil "Cannot delete relation ~(~s~): associated forward rules must be removed first"
                                   name)))
                  (t (let ((warning "") (action nil))
                       (when relation-instances
                         (setf warning
                               (concatenate
                                'string
                                warning
                                (format nil "Deleting relation ~(~s~): associated facts will be lost"
                                        name))
                               action
                           (format nil
                                   "delete-relation-instances~a[~{~(~(~a~)[~)~}"
                                   web-onto::*ocml-line-separator*
                                   (mapcar #'get-relation-instance-expression
                                           relation-instances))))
                       (when (or upward-mapping? downward-add-exp downward-remove-exp)
                         (setf warning
                               (concatenate
                                'string
                                warning
                                (format nil
                                        "Removing mapping information associated with relation ~(~s~)....."
                                        name))))
                       (when defined-by-rule
                         (setf warning
                               (concatenate
                                'string
                                warning
                                (format nil
                                        "Removing rules information associated with relation ~(~s~)....."
                                        name))))
                       (remove-relation-aux name class relation defined-by-rule)
                       (values t warning action))))))
        (values nil (format nil "Couldn't find the relation ~(~a~)" name)))))

	  

(defun maybe-remove-rule (name)
  (let ((rule-structure (get-rule name)))
    (if rule-structure
	(progn (remove-rule rule-structure) t)
      (values nil (format nil "Couldn't find the rule ~(~a~)" name)))))


(defun sorted-all-ontology-names ()
  (sort (mapcar #'car *all-ontologies*)
        #'string<))

(defun sorted-ontologies-owned-by (name &optional (type :all))
  (sorted-accessible-ontologies name nil nil type))

(defun sorted-editable-ontologies (name &optional (type :all))
  (sorted-accessible-ontologies name t nil type))

(defun sorted-ontologies (&optional (type :all))
  (sorted-accessible-ontologies "" t t type))

(defun all-ontology-type-p (x)
  (eq x :all))

(defun ontology-of-type-p (ontology type)
  (or (all-ontology-type-p type)
      (if (listp type)
          (find (ocml::ontology-type ontology) type)
        (eq (ocml::ontology-type ontology) type))))

(defun sorted-accessible-ontologies (name &optional 
                                          include-editable-ontologies
                                          include-all-ontologies
                                          type)
  (sort (if (or include-all-ontologies
                (string= name web-onto::*root-user-name*))
            (mapcan #'(lambda (name-and-ontology)
                        (when (ontology-of-type-p
                               (cdr name-and-ontology)
                               type)
                          (list (car name-and-ontology))))
                    *all-ontologies*)
          (mapcan #'(lambda (name-and-ontology)
                      (when (and (ontology-of-type-p 
                                  (cdr name-and-ontology)
                                  type)
                                 (or (string= name
                                              (ontology-author
                                               (cdr name-and-ontology)))
                                     (and include-editable-ontologies
                                          (web-onto::ok-to-edit-p
                                           (cdr name-and-ontology) name))))
                        (list (car name-and-ontology))))
                  *all-ontologies*))
        #'string<))


(defun safe-direct-instances (class)
  (when (get-ocml-class class)
    (direct-instances class)))

(defun safe-all-instances (class)
  (when (get-ocml-class class)
    (remove-duplicates (all-instances class))))

(defun ocml-most-specific-class-slots (class)
  (let ((obsolete-names (obsolete-names class)))
    (set-difference (domain-slots class)
                    obsolete-names)))

(defun ocml-most-specific-class-local-slots (class)
  (let ((obsolete-names (obsolete-names class)))
    (set-difference (local-slots class)
                    obsolete-names)))

(defun ocml-most-specific-options (class)
  (let ((obsolete-names (obsolete-names class))
        (ocml-options (ocml-options class)))
    (if obsolete-names
        (mapcan #'(lambda (option)
                    (unless (find (car option) obsolete-names)
                      (list option)))
                ocml-options)
        ocml-options)))

;;;names overwritten by a renaming chain
(defun obsolete-names (class)
  (mapcan #'(lambda (x) (copy-list (cdr x)))
          (renaming-chains class)))


(defun get-all-classes ()
  (let ((classes nil) (names nil))
    (map-over-hash-table
     #'(lambda (key value)
         (push key names)
         (push value classes))
     *domain-classes*)
    (values names classes)))


(defun count-items-in-ontology (ontology &optional included-inherited-items-p
                                         types)
  (unless (typep ontology 'ocml::ocml-ontology)
    (setf ontology (get-ontology ontology)))
  (let ((directory (ontology-directory ontology))
        (count 0))
    (select-ontology (name ontology))
    (mapc #'(lambda (table)
              (incf count (count-items-in-table table ontology
                                                included-inherited-items-p)))
          (append
           (when (or (null types) (find 'relations types))
             (list (ontology-relations directory)))
           (when (or (null types) (find 'axioms types))
             (list (ontology-axioms directory)))
           (when (or (null types) (find 'functions types))
             (list (ontology-functions directory)))))
    (maphash #'(lambda (name value)
                 (when (and (eq ontology (home-ontology value))
                            (or (null types) (find 'classes types)))
                       (incf count))
                   (when (or (null types) (find 'instances types))                     
                     (mapc #'(lambda (instance)
                               (when (or included-inherited-items-p
                                         (eq ontology (home-ontology instance)))
                                 (incf count)))
                           (all-current-direct-instances name))))
             (ontology-classes directory))
    count))

(defun count-items-in-table (table ontology &optional included-inherited-items-p)
  (let ((count 0))
    (maphash #'(lambda (name item)
                 (declare (ignore name))
                 (when (or included-inherited-items-p
                           (eq ontology (home-ontology item)))
                   (incf count)))
             table)
    count))

(defun number-of-items-in-library ()
  (let ((count 0))
    (mapc #'(lambda (ontology)
              (incf count (count-items-in-ontology ontology)))
          (mapcar #'cdr *all-ontologies*))
    count))

(defun unassert-old-relation-instance (defining-name)
  (unassert1 defining-name))

(defun get-instance-values (instance-name)
   (multiple-value-bind (class-structure instance-structure)
       (instance-structures instance-name)
    (when class-structure
      (internal-get-instance-values class-structure instance-structure))))

(defun internal-get-instance-values (class-structure instance-structure)
  (let* ((obsolete-names (obsolete-names class-structure))
         (slot-info
          (if obsolete-names
              (mapcan #'(lambda (slot)
                          (unless (find (car slot) obsolete-names)
                            (list slot)))
                      (slot-info-alist class-structure))
              (slot-info-alist class-structure)))
         (all-values nil))
    (values
     (when slot-info
       (mapcan #'(lambda (slot)
                   (when instance-structure
                     (let ((values
                            ;;this includes default values
                            (get-slot-values instance-structure (car slot))
                            ;; use the form below to just get
                            ;;the set values
                            ;;(get-local-slot-values instance-structure
                            ;;                     (car slot))))
                            ))
                       (when values
                         (setf all-values
                               (append values all-values))                              
                         (list (cons (car slot) values))))))
               slot-info))
     all-values)))

(defun get-all-connected-instances (instance-name max-depth &optional (depth 0)
                                                  (inline-p t))
  (if inline-p
      (get-all-connected-instances-inline instance-name max-depth depth)
      (remove-duplicates
       (get-all-connected-instances-not-inline instance-name max-depth depth)
       :test #'equal)))

(defun get-all-connected-instances-inline (instance-name max-depth depth)
  (cond ((and (find-current-instance instance-name)
              (< depth max-depth))
         (cons instance-name
               (mapcar #'(lambda (slot-name-and-slot-value)
                           (let ((slot-name (car slot-name-and-slot-value))
                                 (slot-values (cdr slot-name-and-slot-value)))
                             (append (list slot-name)
                                     (mapcar
                                      #'(lambda (slot-value)
                                          (get-all-connected-instances-inline slot-value max-depth
                                                                              (1+ depth)))
                                      slot-values))))
                       (get-instance-values instance-name))))
        (t instance-name)))

(defun get-all-connected-instances-not-inline (instance-name max-depth depth)
  (multiple-value-bind (slot-values all-values)
      (get-instance-values instance-name)
    (let ((instance-values (filter all-values
                                   #'find-instance)))
      (cond ((and instance-values (< depth max-depth))
             (cons (cons instance-name slot-values)
                   (mapcan
                    #'(lambda (instance-value)
                        (get-all-connected-instances-not-inline instance-value max-depth
                                                                (1+ depth)))
                    instance-values)))
            (t (list (cons instance-name slot-values)))))))


(defun generate-ontolingua (ontology-name &optional (stream *standard-output*)
                                          (line-separator #\linefeed))
  (let ((ontology (get-ontology ontology-name)))
    (cond (ontology
           (format stream "OK[")
           (generate-ontolingua-from-files
            (default-ontology-load-file  
              ontology-name
              (ontology-type ontology))
            (mapcar #'(lambda (file)
                        (merge-pathnames 
                         (ontology-pathname ontology)
                         (make-pathname :name file
                                         :type *lisp-suffix*)))
                    (ontology-files ontology))
            stream
            line-separator))
          
          (t
           (format stream "~s is not a known ontology" ontology-name)))))

(defun generate-ontolingua-from-files (load-file files stream line-separator)    
  (loop for file in (cons load-file files)
        do
        (generate-ontolingua-from-file file stream line-separator)))



(defun generate-ontolingua-from-file (source-pathname stream line-separator)
  (with-open-file (ifile source-pathname
                         :direction :input)
    (format stream "~a;;;Automatically translated from OCML file ~(~s~)"
            line-separator source-pathname)
    (loop with new-form
          for form = (read  ifile  nil :eof-value)
          until (eq form :eof-value)
          do
          (setf new-form (translate-ocml-form-into-ontolingua  form))
          (unless (eq new-form *skip-form-flag*)
            (format stream "~a" line-separator)
            (princ (substitute line-separator #\formfeed
                               (substitute line-separator #\linefeed 
                                           (substitute line-separator
                                                       #\return new-form)))
                   stream))
          (format stream "~a" line-separator))))

;;;useful for printing instance slots in a format string e.g.
;;;(format nil "~(~{(~a ~a ~a~%~/kmi-db::pretty-print-instance-slots/~})~)"
;;;                          (create-new-kmi-member first-name last-name
;;;                                                 ocml-name table-name database-name))

(defun pretty-print-instance-slots (stream arg modifier-p atsign-p)
  (declare (ignore modifier-p atsign-p))
  (format stream "  (~s~{~%   ~s~})" 
          (car arg) (cdr arg)))


(defun cl-user::select-ontology (name)
  (select-ontology name))