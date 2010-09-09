;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defun get-ontology-of-type (stream request-string)
  (with-input-from-string (request-string-stream request-string)
    (read request-string-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read request-string-stream))
           (ontology  (get-ontology ontology-name)))           
      (cond (ontology
             (when stream               
               (http::princ-to-binary-stream
                (format nil "~(~A~)~%"
                        (ocml::ontology-type ontology))
                stream)))
            (t (http::princ-to-binary-stream
                (format nil "Sorry, the ontology ~a does not exists~%" ontology-name)
                stream))))))

(defun get-ontology-properties (stream request-string)
  (with-input-from-string (request-string-stream request-string)
    (read request-string-stream)
    (let* ((*package* (find-package "OCML"))
           (user-name (read request-string-stream))
           (ontology-name (read request-string-stream))
           (ontology  (get-ontology ontology-name)))           
      (cond (ontology
             (cond ((or (root-user-p user-name)
                        (ontology-owner-p ontology user-name))
                    (let ((ontology-includes
                          ; (remove (get-ontology ocml::*base-ontology-name*)
;                                   (ocml::ontology-includes ontology))
                           (ocml::ontology-includes ontology))
                          (ontology-allowed-editors (ocml::ontology-allowed-editors ontology)))
                      (when stream               
                        (http::princ-to-binary-stream
                         (format nil "~:[~* ~;~{~(~a~) ~}~][~:[~* ~;~{~a ~}~][~a[~:(~a~)[~%"
                                 ontology-includes
                                 (mapcar #'ocml::name ontology-includes)
                                 ontology-allowed-editors
                                 ontology-allowed-editors
                                 (ocml::ontology-author ontology)
                                 (ocml::ontology-type ontology))
                         stream))))
                   (t (http::princ-to-binary-stream
                       (format nil "Sorry, the ontology ~a is owned by ~a~%" ontology-name
                               (ocml::ontology-author ontology))
                       stream))))
            (t (http::princ-to-binary-stream
                (format nil "Sorry, the ontology ~a does not exists~%" ontology-name)
                stream))))))


(defun undefined-used-ontology (ontologies)
  (mapcan #'(lambda (ontology-name)
              (unless (get-ontology ontology-name)
                (list ontology-name)))
          ontologies))

(defun update-ontology-properties (stream request-string)
  (with-input-from-string (request-string-stream request-string)
    (read request-string-stream)
    (let* ((*package* (find-package "OCML"))
           (old-ontology-name (read request-string-stream))
           (new-ontology-name (read request-string-stream))
           (ontology-type (read request-string-stream))
           (ontology-uses (read request-string-stream))
           (user-name (read request-string-stream))
           (ontology-new-author (read request-string-stream))
           (ontology-editors (read request-string-stream))
           (ontology  (get-ontology old-ontology-name))
           (undefined-used-ontology (undefined-used-ontology ontology-uses)))
      (when (string= ontology-editors "")
        (setf ontology-editors nil))
      (cond (ontology
             (cond ((not (registered-user ontology-new-author))
                    (http::princ-to-binary-stream
                     (format nil "Sorry, ~a is not a registered user~%" ontology-new-author)
                     stream))
                   (undefined-used-ontology
                    (http::princ-to-binary-stream
                     (format nil "The ontolog~@p ~{~(~a~) ~}~:[is~;are~] not defined~%"
                             (length undefined-used-ontology) undefined-used-ontology
                             (> (length undefined-used-ontology) 1))
                     stream))
                   ((or (root-user-p user-name)
                        (ontology-owner-p ontology user-name))
                    (internal-update-ontology-properties stream
                                                         ontology new-ontology-name ontology-new-author
                                                         (intern (symbol-name ontology-type)
                                                                 (find-package "KEYWORD"))
                                                         ontology-uses ontology-editors))
                   (t (http::princ-to-binary-stream
                       (format nil "Sorry, the ontology ~a is owned by ~a~%" old-ontology-name
                               (ocml::ontology-author ontology))
                       stream))))
            (t (http::princ-to-binary-stream
                (format nil "Sorry, the ontology ~a does not exists~%" old-ontology-name)
                stream))))))

(defun current-directory (ontology)
  (let ((directory-pathname (ontology-directory ontology)))
    (make-pathname :directory
                   (pathname-directory directory-pathname)
                   :host 
                   (pathname-host directory-pathname))))

(defun current-new-source-location (ontology)
  (ocml::ontology-new-source-location ontology))

(defun current-load-file (ontology)
  (let ((directory-pathname (ontology-directory ontology)))
    (make-pathname :directory
                   (pathname-directory directory-pathname)
                   :name ocml:*load-filename*
                   :host (pathname-host directory-pathname))))

(defun reset-load-file (ontology-name ontology-type ontology-uses ontology-author allowed-editors)
  (let* ((logical-ontology-directory
	  (ontology-directory-name-from-type ontology-type ontology-name))
         (ontology-directory (translate-logical-pathname logical-ontology-directory)))
    (create-load-file ontology-directory
                      ontology-name ontology-type ontology-uses ontology-author allowed-editors)))

(defun reset-definitions-file (ontology-directory old-ontology-name new-ontology-name filename)
  (let* ((edits-file (merge-pathnames ontology-directory filename))
         (file-contents (read-in-file-string edits-file nil nil))
         (old-in-ontology-string (format nil "(in-ontology ~(~a~))" old-ontology-name))
         (ontology-line-position (search old-in-ontology-string file-contents)))
    (when (probe-file edits-file)
      (delete-file edits-file))
    (with-open-file (ostream edits-file :direction :output :if-does-not-exist :create)
      (format ostream *edits-file-header*)
      (format ostream "~%~%(in-ontology ~(~a~))~%~%" new-ontology-name)
      (format ostream
              (subseq file-contents (+ ontology-line-position (length old-in-ontology-string)))))))

(defun reset-definitions-files (ontology-type old-ontology-name new-ontology-name)
  (let ((ontology-directory
         (translate-logical-pathname
	  (ontology-directory-name-from-type ontology-type new-ontology-name))))
  (reset-new-definitions-file ontology-directory old-ontology-name new-ontology-name)
  (reset-second-definitions-file ontology-directory old-ontology-name new-ontology-name)))

(defun reset-new-definitions-file (ontology-directory old-ontology-name new-ontology-name)
  (reset-definitions-file ontology-directory old-ontology-name new-ontology-name
                          *webonto-edits-filename*))

(defun reset-second-definitions-file (ontology-directory old-ontology-name new-ontology-name)
  (reset-definitions-file ontology-directory old-ontology-name new-ontology-name
                          (format nil "~(~a~).lisp" new-ontology-name)))

(defun pc-move-pathname (old-pathname new-pathname)
  (ensure-directories-exist new-pathname)
  (let ((old-files (directory old-pathname)))
    (dolist (old-file old-files)
      (copy-file old-file
                 (merge-pathnames (format nil "~a.lisp" (pathname-name old-file))
                                  new-pathname)))
    (delete-non-empty-directory old-pathname)))

(defun move-pathname (old-pathname new-pathname)
  (if (probe-file old-pathname)
      (progn 
	 #+(and :lispworks :unix)
         (foreign::call-system (format nil "mv ~a ~a" old-pathname new-pathname))
         #+(and :lispworks :win32) 
         (pc-move-pathname old-pathname new-pathname)
	 #+allegro
	 (excl::run-shell-command (format nil "mv ~a ~a" old-pathname new-pathname))
	)
      (http::log (format nil "Failed to find ~a when moving directories to ~a"
                         old-pathname new-pathname))))

(defun move-ontology-directories-for-new-type-and-name (ontology new-ontology-type new-ontology-name)
   (let* ((old-ontology-name (ocml::name ontology))
          ;;(old-ontology-type (ocml::ontology-type ontology))
          (old-ontology-directory
           (current-directory ontology)
          ; (translate-logical-pathname
;	    (ontology-directory-name-from-type old-ontology-type old-ontology-name))
           )
          (new-ontology-directory
           (translate-logical-pathname
	    (ontology-directory-name-from-type new-ontology-type new-ontology-name))))
     (move-pathname old-ontology-directory new-ontology-directory)
     (unless (eq old-ontology-name new-ontology-name)
       (move-pathname (merge-pathnames new-ontology-directory
                                       (format nil "~(~a~).lisp" old-ontology-name))
                      (merge-pathnames new-ontology-directory
                                       (format nil "~(~a~).lisp" new-ontology-name))))))     
  
(defun internal-update-ontology-properties (stream ontology new-ontology-name new-author
						   new-ontology-type new-ontology-uses
						   new-ontology-editors)
  (let ((old-ontology-name (ocml::name ontology)))
    (unless (and (eq (ocml::ontology-type ontology)
                     new-ontology-type)
                 (eq old-ontology-name
                     new-ontology-name))
      (move-ontology-directories-for-new-type-and-name ontology new-ontology-type new-ontology-name)
      (delete-from-list-of-ontology-names stream old-ontology-name)
      (ocml::delete-ontology old-ontology-name))
    (reset-load-file new-ontology-name new-ontology-type new-ontology-uses new-author
                     new-ontology-editors)
    (unless (eq old-ontology-name
		new-ontology-name)
      (reset-definitions-files new-ontology-type old-ontology-name new-ontology-name))
    (load-ontology-by-name new-ontology-name)
    (unless (and (eq (ocml::ontology-type ontology)
                     new-ontology-type)
                 (eq old-ontology-name
                     new-ontology-name))
      (add-to-list-of-ontology-names new-ontology-name)
      ;;(add-to-list-of-ontology-home-pathnames new-ontology-name new-ontology-type)
      )
    (http::princ-to-binary-stream
     (format nil "OK~%")
     stream)))
    