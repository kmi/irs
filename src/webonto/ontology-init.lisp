;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package #:webonto)

(defun cl-user::read-in-ontology-names ()
  (read-in-ontology-names))

(defun deprecated (format-control &rest args)
  "Issue a warning about deprecated feature."
  (warn "Deprecated: ~A" (apply #'format nil format-control args)))

(defun read-in-ontology-names ()
  (if (file-exists? *ontology-names-file*)
      (let ((*package* (find-package "OCML")))
        (deprecated "use of ontology names file ~A." *ontology-names-file*)
        (with-open-file (istream *ontology-names-file* :direction :input)
          ;; a final t argument to read makes the pc version crash
          (do ((form (read istream nil nil) (read istream nil nil))
               (forms nil (push form forms)))
              ((null form) (setf *ontologies* forms)))))
        (setf *ontologies* '())))

(defun read-in-file-string (filename &optional variable (skip-blank-lines-p t))
  (with-open-file (istream filename :direction :input)
          ;;;a final t argument to read-line  makes the pc version crash
    (do ((line (read-line istream nil nil) (read-line istream nil nil))
	 (lines nil (if (and skip-blank-lines-p (string= line "")) lines
                        (push (format nil "~a~%" line) lines))))
        ((null line) (if variable
                         (set variable
			      (concatenate* (reverse lines)))
                         (concatenate* (reverse lines)))))))

(defun save-file-string (filename string)
  (when (probe-file filename)
    (delete-file filename))
  (with-open-file (ostream filename :direction :output :if-does-not-exist :create)
    (princ string ostream)))

(defun save-ontologies-string ()
  (save-file-string *ontology-names-file* *ontologies-string*))

(defun save-ontology-home-pathnames-string ()
  (save-file-string *ontology-home-directories-file* *ontology-home-pathnames-string*))

(defun cl-user::read-in-ontology-names-string ()
  (read-in-ontology-names-string))

(defun read-in-ontology-names-string ()
  (if (file-exists? *ontology-names-file*)
      (progn
        (deprecated "use of ontology names file ~A." *ontology-names-file*)
        (read-in-file-string *ontology-names-file* '*ontologies-string*))
      (setf *ontologies-string* "")))

(defun read-in-ontology-home-pathnames-string ()
  (read-in-file-string *ontology-home-directories-file* '*ontology-home-pathnames-string*))

(defun read-in-ontology-home-pathnames ()
  (let ((*package* (find-package "OCML")))
    (with-open-file (istream *ontology-home-directories-file* :direction :input)
      (do ((ontology-name (read istream nil nil t) (read istream nil nil t))
           (logical-directory (read istream nil nil t) (read istream nil nil t))
           (filename (read istream nil nil t) (read istream nil nil t))
           (ontology-pathnames nil
                               (push (list ontology-name
                                           (merge-pathnames
                                            (translate-logical-pathname logical-directory)
                                            filename))
                                     ontology-pathnames)))                                           
          ((null ontology-name) (setf *ontology-home-pathnames* ontology-pathnames))))))
