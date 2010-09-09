;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defun delete-ontology (stream request-string)
  (with-input-from-string (request-string-stream request-string)
    (read request-string-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read request-string-stream))
           (user (read request-string-stream))
           (ontology  (get-ontology ontology-name)))
      (cond (ontology
             (internal-delete-ontology stream ontology-name ontology user))
            (t (http::princ-to-binary-stream
                (format nil "Sorry, the ontology ~a does not exist~%" ontology-name)
                stream))))))

(defun ontology-owner-p (ontology user)
  (string= (ocml::ontology-author ontology) user))

(defun allowed-editor-p (ontology user)
  (find user (ocml::ontology-allowed-editors ontology) :test #'string=))

(defun root-user-p (ontology-name)
  (string= ontology-name *root-user-name*))

(defun internal-delete-ontology (stream ontology-name ontology user)
  (cond ((or (root-user-p user)
             (ontology-owner-p ontology user))
         (when (and (delete-ontology-files ontology-name ontology)
	            (delete-from-list-of-ontology-names stream ontology-name)
	            ;;(delete-from-list-of-ontology-home-pathnames stream ontology-name ontology)
                    )
           (ocml::delete-ontology ontology-name)
	   (http::princ-to-binary-stream
	    (format nil "OK~%")
	    stream)))
        (t (http::princ-to-binary-stream
                (format nil "Sorry, the ontology ~a is owned by ~a~%" ontology-name
                        (ocml::ontology-author ontology))
                stream))))

;(defun delete-from-list-of-ontology-home-pathnames (stream ontology-name ontology)
;  (let* ((logical-directory (ontology-directory-name-from-type
;                            (ocml::ontology-type ontology)
;			    ontology-name))
;        (pathnames-string (format nil "~%~(~a~)~% ~s~% ~s ~%" ontology-name
;                               logical-directory
;                               *webonto-edits-filename*))
;        (position (search pathnames-string *ontology-home-pathnames-string*)))
;    (cond (position
;           (setf *ontology-home-pathnames-string*
;                 (concatenate 'string (subseq *ontology-home-pathnames-string* 0 position)
;                              (subseq *ontology-home-pathnames-string* (+ (length pathnames-string)
;                                                                          position)))
;                 *ontology-home-pathnames*
;                 (delete (assoc ontology-name *ontology-home-pathnames*)
;                         *ontology-home-pathnames*
;                         :test #'equal))
;           (save-ontology-home-pathnames-string)
;           t)
;          (t (http::princ-to-binary-stream
;                (format nil "Sorry, couldn't find the ontology ~a's home pathname~%" ontology-name)
;                stream)
;             nil))))

(defun delete-from-list-of-ontology-names (stream ontology-name)
  (let* ((string-name (format nil "~%~(~a~)" ontology-name))
         (position (search string-name *ontologies-string*)))
    (cond (position
           (setf *ontologies-string*
                 (concatenate 'string (subseq *ontologies-string* 0 position)
                              (subseq *ontologies-string* (+ position (length string-name))))
                 *ontologies*
                 (delete ontology-name *ontologies*))
           (save-ontologies-string)
           t)
          (t (http::princ-to-binary-stream
                (format nil "Had some problems finding ~a in the internal list of ontologies~%"
                        ontology-name)
                stream)
             nil))))

(defun delete-ontology-files (ontology-name ontology)
  (let* ((logical-ontology-directory
	  (ontology-directory-name-from-type (ocml::ontology-type ontology) ontology-name))
         (ontology-directory (translate-logical-pathname logical-ontology-directory)))
    (delete-directory ontology-directory)
    t))
