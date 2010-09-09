;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue based on Enrico Motta's code

;;; The Open University

(in-package "OCML")

(defvar *html-class-colour*
  "GREEN")

(defvar *html-instance-colour*
  "#1063B5")

(defvar *html-relation-colour*
  "#6E8c9c")

(defvar *html-function-colour*
  "#BB0004")

(defvar *html-procedure-colour*
  "#BB0009")

(defvar *html-rule-colour*
  "RED")

(defvar *ontology-type-colour-mapping*)

(defun pretty-format (&rest args)
  (String-Capitalize (apply #'format nil args)))

(defun html-describe-class-info (name &optional 
                                      (lookup-function 'http::lookup-ocml-thing)
                                      (ontology (name *current-ontology*)))
  (http::html-out
   (http::internal-insert-ocml-links
    (with-output-to-string (http::html-stream)
      (Let* ((class (get-ocml-class name))
	     (supers (mapcar #'name (domain-superclasses class)))
	     (subs (remove-duplicates (mapcar #'name (current-subclasses class)))))
	(when class
	  (http::html-out (http::header 1 (pretty-format "Description of Class ~S" name)))
	  (when supers
	    (http::html-out (http::header 2 "Superclasses"))
	    (http::html-out (pretty-format "~S~{, ~S~}"
					   (car supers)
					   (cdr supers))))
	  (when subs
	    (http::html-out (http::header 2 "Subclasses"))
	    (http::html-out (pretty-format "~S~{, ~S~}"
					   (car subs)
					   (cdr subs))))
	  (when (ocml-most-specific-class-slots class)
	    (http::html-out (http::header 2 "Slots"))
	    (http::html-out "<dl>")
	    (loop for slot in (ocml-most-specific-class-slots class)
		  do
		  (http::html-out (pretty-format "<dt><b>~s</b></dt>" slot))
		  (multiple-value-bind (values defaults)
		      (get-slot-values-from-class-structure
		       class slot )
		    (if values
			(http::html-out (pretty-format "<br><dd> <b>~S:</b> ~S~{, ~S~}</dd>"
						       :value
						       (car values)
						       (cdr values)))
			(when defaults
			  (http::html-out (pretty-format "<br><dd>   <b>~S:</b> ~S~{, ~S~}</dd>"
							 :default-value
							 (car defaults)
							 (cdr defaults))))))
		  (let ((type-info (remove-duplicates
                                    (find-option-value class slot :type))))
		    (when type-info
		      (http::html-out (pretty-format "<br><dd>   <b>~S:</b> ~S~{, ~S~}</dd>"
						     :type 
						     (car type-info)
						     (cdr type-info)))))
		  (loop for option in '(:min-cardinality
				       :max-cardinality
				       :inheritance)
			for value = (find-option-value class slot option)
			when value
			do
			(http::html-out (pretty-format "<br><dd>  <b>~S:</b> ~S</dd>"
						       option value)))
		  (let ((doc (find-slot-documentation class slot)))
		    (when doc
		      (http::html-out "<br><dd>  <b>~S:</b> ~S</dd>"
				      :documentation doc)))
		  (http::html-out "<br><Br>"))
	    (http::html-out "</dl>  "))
          (when (renaming-chains class)
            (http::html-out (http::header 2 "Renaming Chains"))
            (http::html-out "<dl>")
            (loop for chain in (remove-duplicates (renaming-chains class))
		  do
		  (http::html-out (pretty-format "<dt><b>~{~a renames~} ~a</b></dt>"
                                                 (butlast chain) (car (last chain)))))
            (http::html-out "</dl><p>")))))
    lookup-function ontology)))

(defun html-describe-instance2 (name &optional 
                                      (lookup-function 'http::lookup-ocml-thing)
                                      (ontology (name *current-ontology*)))
  (http::html-out
   (http::internal-insert-ocml-links
    (with-output-to-string (http::html-stream)
      (declare (special http::html-stream))
      (Let ((instances (find-all-current-instances-named-x name)))
        (when instances
          (mapc #'(lambda (inst)
                    (describe-single-instance inst name))
                instances))))
    lookup-function ontology)))

(defun html-describe-instance (name &optional 
                                      (lookup-function 'http::lookup-ocml-thing)
                                      (ontology (name *current-ontology*)))  
  (http::html-out
   (http::internal-insert-ocml-links
    (with-output-to-string (http::html-stream)
      (declare (special http::html-stream))
      (Let ((instances (find-all-current-instances-named-x name)))
        (when instances
          (mapc #'(lambda (inst)
                    (describe-single-instance inst name))
                instances))))
    lookup-function ontology)))

(defun describe-single-instance (inst name)
  (let ((parent (parent-class inst)))
    (http::html-out (http::header 1 (pretty-format "~%Instance ~s of class ~S~%"
                                                   name (name parent))))
    (when (ocml-most-specific-class-slots parent)          
      (http::html-out (http::header 2 "Slots"))
      (http::html-out
       (http::in-table (:border 0)
         (loop for slot in (ocml-most-specific-class-slots parent)
               for values = (get-slot-values inst slot)
               when values 
               do
               (progn
                 (http::html-out 
                  (format nil "<tr><td>~(~a~):</td> <td>~(~S~)~{, ~(~S~)~}</td></tr>"
                          slot
;                                     (pretty-format "~a" slot) nil ontology)
                          (car values)(cdr values))))))))))

(defun clean-up-info-string (string &optional (char1 #\space) (char2 #\%))
  (substitute char1 char2 string))

(defun html-code (code)
  (http::code
   (http::preformatted
    (format nil "~a"
            (string-downcase
	     (with-output-to-string (stream)
	       (ocml-pprint code stream)))))))

(defun html-describe-relation (name &optional 
				    (lookup-function 'http::lookup-ocml-thing)
				    (ontology (name *current-ontology*)))
  (http::html-out
   (http::internal-insert-ocml-links
    (with-output-to-string (http::html-stream)
      (let ((structure (get-ocml-relation name)))
	(when structure
	  (with-slots (constraint printable-lisp-fun) structure
	    (http::html-out (http::header 1 (pretty-format "Relation ~s" name)))
	    (http::html-out (http::header 2 "Arity"))
	    (http::html-out (format nil "~a" (arity structure)))
	    (http::html-out (http::header 2 "Schema"))
	    (http::html-out (pretty-format "(~{~A ~})" (schema structure)))
	    (when (slot-of structure)
	      (http::html-out (http::header 2 "Slot of"))
	      (mapc #'(lambda (slot) (http::html-out (pretty-format "~a<br>" (name slot))))
		    (slot-of structure)))
	    (when (sufficient structure)
	      (http::html-out (http::header 2 "Sufficient"))
	      (http::html-out (simple-format
			       (car (bc-clause-antecedents (sufficient structure))))))
	    (when (iff-def structure)
	      (http::html-out (http::header 2 "Iff Definition"))
	      (http::html-out (simple-format (car (bc-clause-antecedents (iff-def structure))))))
	    (when (prove-by structure)
	      (http::html-out (http::header 2 "Prove by"))
	      (http::html-out (simple-format (car (bc-clause-antecedents (prove-by structure))))))
	    (when (defined-by-rule structure)
	      (http::html-out (http::header 2 "Defined by Rule"))
	      (http::html-out (make-single-ocml-rule-html-output
			       (car (defined-by-rule structure)))))
	    (when constraint
	      (http::html-out (http::header 2 "Constraint"))
	      (http::html-out (simple-format constraint)))
	    (when printable-lisp-fun
	      (http::html-out (http::header 2 "Lisp Function"))
	      (http::html-out (html-code printable-lisp-fun)))))))
    lookup-function ontology)))

(defun make-single-ocml-rule-html-output (rule)
  (with-slots ((direction ocml::direction)) rule
     (format nil
             "<b>Direction:</b> ~a<br>~{~a<br>~}"
             direction
             (make-clauses-html-output rule))))

(defun make-clause-html-output (clause)
  (with-slots ((consequent ocml::consequent) (antecedents ocml::antecedents))
	      clause
    (string-downcase
     (format nil "<b>Consequent:</b> ~a<br><b>Antecedents:</b>~{~a~}"
             consequent antecedents))))

(defun make-clauses-html-output (rule)
  (mapcar #'(lambda (clause) (make-clause-html-output clause))
          (clauses rule)))

(defun html-describe-function (name &optional 
				    (lookup-function 'http::lookup-ocml-thing)
				    (ontology (name *current-ontology*)))
  (http::html-out
   (http::internal-insert-ocml-links
    (with-output-to-string (http::html-stream)
      (let ((structure (get-ocml-function name)))
        (when structure
          (with-slots (schema definition output-var constraint body printable-lisp-fun)
            structure
            (http::html-out (http::header 1 (pretty-format "~:[Function~;Procedure~] ~s"
                                                           (procedure-p structure) name)))
            (http::html-out (http::header 2 "Arity"))
            (http::html-out (format nil "~a" (arity structure)))
            (http::html-out (http::header 2 "Schema"))
            (http::html-out (pretty-format "(~{~A ~})" schema))
            (when constraint
              (http::html-out (http::header 2 "Constraint"))
              (http::html-out (format nil "~a" constraint)))
            (http::html-out (http::header 2 "Constraint"))
            (when definition
              (http::html-out (http::header 2 "Definition"))
              (http::html-out (format nil "~a" definition)))
            (when body
              (http::html-out (http::header 2 "Body"))
              (http::html-out (html-code body)))
            (when printable-lisp-fun
              (http::html-out (http::header 2 "Lisp Function"))
              (http::html-out (format nil "~a" (html-code printable-lisp-fun))))))))
    lookup-function ontology))) 


(defun make-html-clause-info (clause)
  (with-slots (consequent antecedents) clause
    (string-downcase
     (format nil "<br><b>Consequent:</b>~a<br><b>Antecedents:</b>~{~a<br>~}"
             consequent antecedents))))

(defun make-html-clauses-info (rule)
  (mapcar #'(lambda (clause) (make-html-clause-info clause))
          (clauses rule)))

(defun html-describe-rule (name &optional 
				(lookup-function 'http::lookup-ocml-thing)
				(ontology (name *current-ontology*)))
  (http::html-out
   (http::internal-insert-ocml-links
    (with-output-to-string (http::html-stream)
      (let ((structure (get-ocml-function name)))
        (when structure
          (let ((clauses (make-html-clauses-info structure)))
            (with-slots (direction) structure
              (http::html-out (http::header 1 (pretty-format "Rule ~a" name)))
              (http::html-out (format nil "Direction: ~a" direction))
              (http::html-out (http::header 2 "Clauses"))
              (mapcar #'http::html-out clauses))))))
    lookup-function ontology)))

(defun ocml-lookup (name &optional (ontology 'kmi-planet-kb)
                         check-relation-defined-from-def-relation-p)
  (select-ontology ontology)
  (let ((result nil) (type nil) (class nil) (colour nil))
    (cond ((setf result (get-ocml-class name))
           (setf colour *html-class-colour*
                 type :class))
          ((setf result (find-current-instance name))
           (setf colour *html-instance-colour*
                 type :instance
                 class 
                 (name (class-of result))))
          ((setf result (find-bc-rule name))
           (setf colour *html-rule-colour*
                 type :rule))
          ((and (setf result (get-ocml-relation name))
                (or (not check-relation-defined-from-def-relation-p)
                    (ocml::defined-from-def-relation result)))
           (setf colour *html-relation-colour*
                 type :relation))
          ((setf result (get-ocml-function name))
           (if (procedure-p name)
               (setf colour *html-procedure-colour*
                     type :procedure)
             (setf colour *html-function-colour*
                   type :function)))
          ((setf result (get-ontology name))
           (setf colour *html-ontology-colour*
                 type :ontology)))
    (if (eq type :ontology)
        (values result
                (when result (eq ontology (name result)))
                colour
                (when result (name result))
                type
                class)
      (values result
              (when result (and (home-ontology result)
                                (eq ontology (name (home-ontology result)))))
              colour
              (when result (name (home-ontology result)))
              type
              class))))

(defun ocml-definition (name &optional (ontology 'kmi-planet-kb) instance-class)
  (select-ontology ontology)
  (cond ((get-ocml-class name)
         (web-onto::get-source-string (list 'def-class name ontology)))
        ((find-current-instance name instance-class)
         (web-onto::get-source-string (list 'def-instance name instance-class ontology)))
        ((find-bc-rule name)
         (web-onto::get-source-string (list 'def-rule name ontology)))
        ((get-ocml-relation name)
         (web-onto::get-source-string (list 'def-relation name ontology)))
        ((and (get-ocml-function name) (not (procedure-p (get-ocml-function name))))
         (web-onto::get-source-string (list 'def-function name ontology)))
        ((get-ocml-function name)
         (web-onto::get-source-string (list 'def-procedure name ontology)))))

(defun html-ocml-ask (expression)
  (internal-ocml-eval expression  #'(lambda (expression)
                                     (ocml::ask-top-level expression :all t :query-mode t)))) 

(defun html-ocml-eval (expression)
  (internal-ocml-eval expression #'(lambda (expression)
                                     (ocml::ocml-eval-gen expression nil))))

(defun internal-ocml-eval (expression function &optional (ontology 'kmi-planet-kb))
  (select-ontology ontology)
  (let ((*package* (find-package "OCML"))
        output (result nil))
    (setf output
	  (handler-case
	      (with-output-to-string (*standard-output*)
                (let ((input (read-from-string (string-upcase expression))))
		  (setf result
                        (if input
                            (format nil "~a" (funcall function input))
                            "No Input"))))
            ;;use serious-condition to catch stack overflows
	    (serious-condition (c) (format nil "When evaluating ~a got an error of type ~a" expression c))))
    
    (http::font
     (http::preformatted (if result
                             (concatenate 'string output "<p>" result )
                             output))
     :size 10)))
