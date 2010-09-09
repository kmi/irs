(in-package web-onto)

(defun add-code-to-ontology (ontology code &optional overwrite-p)
  (let ((new-source-file 
         (ocml::ontology-new-source-location (ocml::get-ontology ontology))))
    (if overwrite-p 
        (internal-overwrite-code-in-ontology ontology code new-source-file)
      (internal-append-code-to-ontology code new-source-file))
    (load new-source-file)))

(defun user-name-and-password-ok-p (name password)
  (let ((stored-name (assoc name *users* :test #'string=)))
    (and stored-name (string= (second stored-name) password))))

(defun upload-ontology (stream request-string)
  (with-input-from-string (istream request-string)
    (read istream)
    (let (user-name user-password ontology-name code)
    (setf user-name (read istream)
          user-password (read istream)
          code (read istream)
          ontology-name (intern (string-upcase (read istream))
                                (find-package "OCML")))
    (cond ((user-name-and-password-ok-p user-name user-password)
           (let ((ontology (ocml::get-ontology ontology-name)))
             (if ontology
                 (cond ((or (root-user-p user-name)
                            (ontology-owner-p ontology user-name))
                        (add-code-to-ontology ontology-name code t)
                        (format stream "OK~%"))
                       (t (format stream "NOT_ONTOLOGY_OWNER")))
               (format stream "ONTOLOGY-DOES-NOT-EXIST"))))
          (t (format stream "INVALID-USER-NAME-OR-PASSWORD"))))))

