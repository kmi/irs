(in-package wsmo-protocol)

(defvar *lifting-and-lowering-filename* "lilo.lisp")

(defun get-lifting-and-lowering-code (ontology-name)
  (let* ((ontology (ocml::get-ontology ontology-name))
         (ontology-directory-pathname (ocml::ontology-pathname ontology))
         (lifting-and-lowering-file 
          (translate-logical-pathname
           (concatenate 'string ontology-directory-pathname *lifting-and-lowering-filename*))))
    (values 
     (if (probe-file lifting-and-lowering-file)
        (get-file-contents lifting-and-lowering-file)
      "") t)))

(defun get-file-contents (filename)
  (let ((contents ""))
    (with-open-file (istream filename :direction :input)
      (do ((line (read-line istream nil nil t) (read-line istream nil nil t)))
          ((null line) contents)
        (setf contents (format nil "~a~%~a" contents line))))))
    
(defun save-lifting-and-lowering-code (ontology-name new-code)
 ;; (setf my-onto ontology-name)
 ;; (setf my-code new-code)

  (let* ((ontology (ocml::get-ontology ontology-name))
         (ontology-directory-pathname (ocml::ontology-pathname ontology))
         (lifting-and-lowering-file 
          (translate-logical-pathname
           (concatenate 'string ontology-directory-pathname *lifting-and-lowering-filename*))))
    (when (probe-file lifting-and-lowering-file)
      (delete-file lifting-and-lowering-file))
    (save-file-contents lifting-and-lowering-file new-code)
    (values (load lifting-and-lowering-file) t)))

(defun save-file-contents (filename new-code)
  (with-open-file (ostream filename :direction :output :if-does-not-exist :create)
      (princ new-code ostream)))