;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defvar *diagram-extension* "drg")

(defun get-diagrams (stream request-string)
  (with-input-from-string (info-stream request-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (ontology (get-ontology ontology-name)) diagrams)
      (cond (ontology 
             (setf diagrams
                   (directory
		    (merge-pathnames
                     (ontology-directory ontology)
                     (format nil "*.~a" *diagram-extension*))))
             (if diagrams
                 (http::princ-to-binary-stream
                   (format nil "~{~a[~}~%"
                           (sort (mapcar #'pathname-name diagrams) #'string<))
                   stream)
                 (http::princ-to-binary-stream
                   (format nil "nil~%" )
                   stream)))
            (t (http::princ-to-binary-stream
                   (format nil "ontology_does_not_exist~%")
                   stream))))))

(defun open-diagram (stream request-string)
  (with-input-from-string (info-stream request-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (file-name (read info-stream))
           (ontology (get-ontology ontology-name)) file)
      (cond (ontology
             (setf file  
		    (merge-pathnames
                     (ontology-directory ontology)
                     (format nil "~(~a~).~a" file-name *diagram-extension*)))
             (if (probe-file file)
                 (send-file file stream)
                 (http::princ-to-binary-stream
                   (format nil "diagram_does_not_exist~%")
                   stream)))
            (t (http::princ-to-binary-stream
                   (format nil "ontology_does_not_exist~%")
                   stream))))))

(defun save-diagram (stream request-string)
  (let* ((nodes-string-end (position *ocml-line-separator*
                                     request-string))
         (nodes-string (subseq request-string 0 nodes-string-end))
         (extra-info (if (> (length request-string) nodes-string-end)
                         (subseq request-string (1+ nodes-string-end)
                                 (position *ocml-line-separator*
                                           request-string :start
                                           (1+ nodes-string-end)))
                         "")))
    (with-input-from-string (info-stream nodes-string)
      ;;;the action bit
      (read info-stream)
      (let* ((*package* (find-package "OCML"))
             (ontology-name (read info-stream))
             (file-name (read info-stream))
             (file-name-string (format nil "~a" file-name))
             (ontology (get-ontology ontology-name)))
        (cond (ontology
               (save-file
                file-name
                (concatenate 'string
                             (subseq nodes-string
                                     (+ (search file-name-string nodes-string)
                                        (length file-name-string)))
                             (format nil "~a~%~%" *ocml-line-separator*)
                             extra-info)                             
	        (merge-pathnames
                 (ontology-directory ontology)
		 (format nil "~(~a~).~a" file-name *diagram-extension*))
                stream))
              (t (http::princ-to-binary-stream
		  (format nil "ontology_does_not_exist~%")
		  stream)))))))

(defun send-file (file stream)
  (let ((source (get-file-contents file nil)))
    (http::princ-to-binary-stream
     source
     ;;(format nil "~a~%" source)
     stream)))

(defun save-file (diagram-name diagram file stream)
  (when (probe-file file)
    (delete-file file))
  (with-open-file (ostream file :direction :output :if-does-not-exist :create)
    (format ostream "~(~a~)~%"  diagram))
  (http::princ-to-binary-stream (format nil "succesfully saved the diagram ~(~a~)~%" diagram-name) stream))
    
