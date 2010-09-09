;;; Copyright Open University, 2007

(in-package :webonto)

(defun file-exists? (pathname)
  (probe-file (translate-logical-pathname pathname)))

#+(and :lispworks :win32)
(defun delete-non-empty-directory (directory)
  (let ((files (directory directory)))
    (dolist (file files)
      (delete-file file))
    (cl-user::delete-directory directory)))

(defun delete-directory (directory)
  (when (probe-file directory)
    #+(and :lispworks :unix) (foreign::call-system (format nil "rm -r ~a" directory))
    #+(and :lispworks :win32)
    (delete-non-empty-directory directory)
    #+allegro (excl::run-shell-command (format nil "rm -r ~a" directory))))
