(in-package wsmo-protocol)

(defun generate-ontology-page (ontology &optional (insert-hyper-links t) (representation-language :ocml))
  (let ((ontology-structure (ocml::get-ontology ontology)))
    (when ontology-structure
      (ocml:with-ocml-thread-safety
	(ocml:with-ontology (ontology)
	  (let* ((files (ocml::ontology-files ontology-structure))
		 (ontology-type (ocml::ontology-type ontology-structure))
		 (load-file web-onto::*default-ocml-load-file-name*)
		 (documentation
		  (slot-value ontology-structure 'ocml::documentation))
		 (source-directory (ocml::ontology-pathname ontology-structure)))
	    (http::html-out web-onto::*published-ontology-header*)
	    (http::html-out "<h1>Ontology: ~:(~a~); Representation Language: ~a</h1>" 
			    ontology representation-language)
	    (mapc 
	     #'(lambda (file)
		 (generate-ontology-file-definitions
		  ontology 
		  documentation source-directory file insert-hyper-links representation-language))
	     (cons load-file files))
	    (http::html-out web-onto::*published-ontology-footer*)))))))

(defvar *default-root-url* "/")

(defun generate-ontology-file-definitions (ontology documentation source-directory file
					   &optional (insert-hyper-links t)
					   (representation-language :ocml)
					   (source-file-extension cl-user::*lisp-suffix*))
  (let ((ifile (merge-pathnames 
                (make-pathname :name file 
                               :type 
                               source-file-extension)
                source-directory))
	(link-url "/irs/ontology/ocml/~a?format=raw#~(~a~)"))
    (with-open-file (istream ifile :direction :input)
      (http::html-out "<h2>File: ~a</h2>" file)
      (when documentation
        (http::html-out "<p>~a</p>" documentation))
      (http::html-out "<pre>")
      (do ((line (read-line istream nil nil)
                 (read-line istream nil nil)))
          ((null line))
        (if insert-hyper-links
            (http::html-out "~a~%" 
                            (http::internal-insert-ocml-links 
                             (maybe-insert-definition-anchor 
                              representation-language
                              line)
                             #'irs-uri-lookup-current-word
                             ontology
                             t
                             link-url))
          (http::html-out "~a~%" (maybe-insert-definition-anchor 
                                  representation-language
                                  line))))
      (http::html-out "</pre>"))))

(defun irs-uri-lookup-current-word (current-word current-word-length ontology
                                              bold-home-ontology-p link-url)
  (let* ((current-word-string (coerce (subseq current-word 0 current-word-length)
                                      'string))
         (reverse-current-word-string (reverse current-word-string))
         (word-to-lookup (intern (string-upcase current-word-string)
                                 (find-package "OCML"))))
    (multiple-value-bind (found-p home-ontology-p colour home-ontology)
        (ocml::ocml-lookup word-to-lookup ontology)
      (if found-p
          (add-irs-uri-link word-to-lookup current-word-string t home-ontology
                         (and bold-home-ontology-p home-ontology-p)
                         colour link-url)
          reverse-current-word-string))))

(defun add-irs-uri-link (word-to-lookup current-word &optional (reverse-p t)
                                        (ontology 'ocml::wsmo)
                                        bold-p colour link-url)
  (when colour
    (setf current-word (http::font current-word :color colour)))
  (when bold-p
    (setf current-word (http::bold current-word)))
  (if reverse-p 
      (reverse (http::anchor (format nil link-url
                                     ontology word-to-lookup)
                             current-word))
    (http::anchor (format nil link-url
                          ontology word-to-lookup)
                  current-word)))
(defun create-standard-item-name-string (item-name)
  (string-downcase
   (if (symbolp item-name)
       (symbol-name item-name)
     (format nil "~a" item-name))))

(defun create-definition-anchor (string)
  (concatenate 'string
               "<a name=\""
               string
               "\"></a>"))

(defun make-uri-compatible (string)
  (let ((result nil) (last-char nil))
    (dolist (char (coerce string 'list))
      (cond ((char= char #\-))          
            ((and last-char (char= last-char #\-))
             (push (char-upcase char) result))
            (t (push char result)))
      (setf last-char char))
    (coerce (reverse result) 'string)))

(defun create-definition-anchors (item-name)
  (let ((item-string-name (create-standard-item-name-string item-name)))
    (concatenate 'string
                 (create-definition-anchor item-string-name)
                 (create-definition-anchor (make-uri-compatible item-string-name)))))
    

(defun maybe-insert-definition-anchor 
       (representation-language line)
  (let ((ocml-defining-item (web-onto::find-ocml-defining-item 
                             representation-language line)))
    (if ocml-defining-item
        (let ((item-name (web-onto::get-item-name line)))
          (if item-name
              (concatenate 'string (create-definition-anchors item-name)
                           line)
            line))
      line)))