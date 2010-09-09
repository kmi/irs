;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defvar *database-file-name* "database.db")

(defvar *database-table-header* "")

(defvar *database-table-footer* (format nil "end~%"))

(defvar *object-number-table* nil)

(defvar *current-object-number* 0)

(defun clear-object-number-table ()
  (setf *object-number-table* nil *current-object-number* 0))

(defun find-object-number (object)
  (cdr (assoc object *object-number-table* :test #'eq)))

(defun find-no-x-object-number (object string x)
  (if (name-starts object string)
      x
    (cdr (assoc object *object-number-table* :test #'eq))))

(defun find-no-12-ecol-object-number (object)
  (find-no-x-object-number object "ecol" 12))

(defun name-starts (object string)
  (let ((name (ocml-name object)))
    (and (>= (length name) (length string))
	 (string= string (subseq name 0 (length string))))))

(defun no-x-object-number (x)
  (if (= *current-object-number* (1- x))
      (setf *current-object-number* (+ *current-object-number* 2))
    (incf *current-object-number*)))

(defun no-12-x-object-number ()
  (no-x-object-number 12))

(defun new-object-number (object)
  (incf *current-object-number*)
  (push (cons object *current-object-number*) *object-number-table*)
  *current-object-number*)

(defun new-no-12-object-number (object)
  (no-12-x-object-number)
  (push (cons object *current-object-number*) *object-number-table*)
  *current-object-number*)

(defun no-12-ecol-object-number (object)
  (let ((object-number (find-no-12-ecol-object-number object)))
    (if object-number
	object-number
      (new-no-12-object-number object))))

(defun object-number (object)
  (let ((object-number (find-object-number object)))
    (if object-number
	object-number
      (new-object-number object))))

(defun generate-database-table-on-remote-host (stream upcase-string)
  (handler-case
   (progn 
     (clear-object-number-table)
     (with-input-from-string (istream upcase-string)
			     (read istream)   ;;;the generate-database-table bit
			     (let* ((*package* (find-package "OCML"))
				    (ontology-name (read istream))	    
				    (address (read istream))
				    (port (read istream))
				    (types-and-names (read istream))
				    ocml-objects)
			       (ocml::select-ontology ontology-name)
			       (setf ocml-objects
				     (mapcar
				      #'(lambda (type_and_name)
					  (get-ocml-object (car type_and_name) (second type_and_name)))
				      types-and-names))
			       (generate-database-table-on-remote-host-from-ocml-objects
				address port ocml-objects)
			       (http::princ-to-binary-stream
				(format nil
					"Successfully generated a database on ~a ~a~%" address port)
				stream))))
   (error (c)
	  (http::princ-to-binary-stream
	   (format nil
		   "Error ~a in generating table on remote host ~a~%" c upcase-string)
	   stream))))
  ;;(setq s-b (make-socket :remote-host "machine-a" :remote-port port-b))

(defun generate-database-table-file (stream upcase-string)
  (handler-case
   (progn 
     (clear-object-number-table)
     (with-input-from-string (istream upcase-string)
			     (read istream)   ;;;the generate-database-table bit
			     (let* ((*package* (find-package "OCML"))
				    (ontology-name (read istream))
				    (types-and-names (read istream))
				    ocml-objects
				    (ontology (get-ontology ontology-name)))
			       (ocml::select-ontology ontology-name)
			       (setf ocml-objects
				     (mapcar
				      #'(lambda (type_and_name)
					  (get-ocml-object (car type_and_name) (second type_and_name)))
				      types-and-names))
			       (generate-database-table-from-ocml-objects ontology
									  ocml-objects)))
     (http::princ-to-binary-stream
	   (format nil
		   "Successfully generated a database file~%")
	   stream))
   (error (c)
	  (http::princ-to-binary-stream
	   (format nil
		   "Error ~a in generating a database table in a file  ~a~%" c upcase-string)
	   stream))))


(defun get-ocml-object (ocml-type name)
  (case ocml-type
    ((ocml::classes) (ocml::get-ocml-class name))
    ((ocml::functions) (ocml::get-ocml-function name))
    ((ocml::procedures) (ocml::get-ocml-function name))
    ((ocml::relations) (ocml::get-ocml-relation name))
    ((ocml::rules) (ocml::get-rule name))
    ((ocml::instances) (ocml::find-current-instance name))))

(defun make-remote-output-stream (host port)
  #+lispworks
  (comm::open-tcp-stream host port :direction :output)
  #-:lispworks
  (usocket:socket-connect host port))

(defun generate-database-table-on-remote-host-from-ocml-objects (address port ocml-objects)
  (let ((ostream (make-remote-output-stream address port)))
    (unwind-protect 
	(progn (format ostream *database-table-header*)
	       (mapc #'(lambda (ocml-object)
			 (format ostream (ocml-object-db-row ocml-object ocml-objects)))
		     ocml-objects)
	       (format ostream *database-table-footer*)
	       (finish-output ostream))
      (close ostream))))
    

(defun generate-database-table-from-ocml-objects (ontology ocml-objects)
  (let ((db-file (make-pathname :directory
				(pathname-directory
				 (ontology-directory ontology))
				:name *database-file-name*)))
    (when (probe-file db-file)
      (delete-file db-file))
    (with-open-file (ostream db-file :direction :output :if-does-not-exist :create)
      (format ostream *database-table-header*)
      (mapc #'(lambda (ocml-object)
		(format ostream (ocml-object-db-row ocml-object ocml-objects)))
	    ocml-objects)
      (format ostream *database-table-footer*))))

(defmethod ocml-object-db-row (ocml-object ocml-objects)
  (declare (ignore ocml-object ocml-objects)))

(defmethod ocml-object-db-row ((ocml-class ocml::basic-domain-class) ocml-objects)
  (let ((parents (intersection (ocml-class-parents ocml-class) ocml-objects)))
    (format nil "~{~a ~}~a ~a~%"
	    (if parents
		(mapcar #'(lambda (parent) (no-12-ecol-object-number parent));;(object-number parent))
			parents)
	      '(null))
	    (no-12-ecol-object-number ocml-class) ;;(object-number ocml-class)
	    (ocml-name ocml-class))))
