;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defvar *users-file* nil)

(defvar *users-string* "")

(defun print-all-users (&optional (order-by :group))
  (format t "~{~{~a ~}~%~}" (sort *users* #'string< :key
                                  (if (eq order-by :group)
                                      #'third
                                      #'car))))

(defun cl-user::read-in-users-string ()
  (read-in-users-string))

(defun read-in-users-string ()
  (read-in-file-string *users-file* '*users-string*))

(defun merge-user-files (output-file &rest input-files)
  (when (probe-file output-file)
    (delete-file output-file))
  (let ((users nil))
  (with-open-file (ostream output-file :direction :output :if-does-not-exist :create)
    (dolist (input-file input-files)
        (with-open-file (istream input-file :direction :input)
          (do ((name (read-line istream nil nil t) (read-line istream nil nil t))
               (password (read-line istream nil nil t) (read-line istream nil nil t))
               (groups (read-line istream nil nil t) (read-line istream nil nil t)))
              ((null name))
            (unless (find name users :test #'equal)
              (push name users)
              (format ostream "~a~%" name)
              (format ostream "~a~%" password)
              (format ostream "~a~%" groups))))))))


(defun cl-user::read-in-users ()
  (read-in-users))

(defun read-in-users ()
  (with-input-from-string (istream *users-string*)
    (do ((name (read-line istream nil nil t) (read-line istream nil nil t))
         (password (read-line istream nil nil t) (read-line istream nil nil t))
         (groups (read-line istream nil nil t) (read-line istream nil nil t))
         (users nil (push (list name password groups) users)))
        ((null name) (check-users users) (setf *users* users)))))

(defun check-users (users)
  (cond ((null users))
        ((find (car users) (cdr users)
               :test #'(lambda (x y)
                         (string= (car x) (car y))))
         (error "The user ~a and ~a have the same name"
                (car users) (member (car users) (cdr users)
                                    :test #'(lambda (x y)
                                              (string= (car x) (car y))))))
        (t (check-users (cdr users)))))


(defun check-new-user (user)
  (not (find user *users*
               :test #'(lambda (x y)
                         (string= (car x) (car y))))))

(defun warn-about-user (user)
  (format t
          "The user ~a and ~a have the same name"
          user (car (member user *users*
                            :test #'(lambda (x y)
                                      (string= (car x) (car y)))))))

(defun add-user (name password &optional (groups ""))
  (let ((new-user (list name password groups)))
    (cond ((check-new-user new-user)
           (setf *users-string*
                 (concatenate 'string
                              (format nil "~a~%~a~%~a~%" name password groups)
                              *users-string*)
                 *users* (cons new-user *users*))
           (save-users-string))
          (t (warn-about-user new-user)))))

(defun add-users (name-start password number &optional (groups ""))
  (dotimes (i number)
    (add-user (concatenate 'string name-start "_" (princ-to-string (1+ i)))
              password groups)))

(defun save-users-string ()
  (when (probe-file *users-file*)
    (delete-file *users-file*))
  (with-open-file (ostream *users-file* :direction :output :if-does-not-exist :create)
    (format ostream *users-string*)))


(defun registered-user (name)
  (assoc name *users* :test #'string=))

(defun login (stream request-string)
  (with-input-from-string (istream request-string)
    (read istream) ;;login
    (let* ((name (read istream))
           (password (read istream))
           (stored-name (assoc name *users* :test #'string=)))
      (if (and stored-name (string= (second stored-name) password))
          (http::princ-to-binary-stream (format nil "OK~%") stream)
          (http::princ-to-binary-stream (format nil "NOT_OK~%") stream)))))

(defun change-password (stream request-string)
  (with-input-from-string (istream request-string)
    (read istream) ;;change_password
    (let* ((name (read istream))
           (new-password (read istream)))
      (cond ((assoc name *users* :test #'equal)
             (setf (second (assoc name *users* :test #'equal)) new-password)
             (write-and-save-users-string *users*)
             (http::princ-to-binary-stream (format nil "OK~%") stream))
            (t (http::princ-to-binary-stream (format nil "NOT_OK~%") stream))))))

(defun write-and-save-users-string (users)
  (let ((string ""))
    (mapc #'(lambda (name-password-groups)
              (setf string (concatenate 'string (format nil "~a~%~a~%~a~%" (car name-password-groups)
                                                        (second name-password-groups)
                                                        (third name-password-groups))
                                        string)))
          users)
    (setf *users-string* string)
    (save-users-string)))

(defun user-groups (user-name)
  (third (assoc user-name *users* :test #'string=)))
