(in-package web-onto)


(defvar *ocml-model-types*
  '(:basic :domain :task :method :application))

(defvar *published-ontology-header*
  (format nil 
          "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">~%
   <HTML>~%<!-- Page produced automatically by WebOnto --><body>"))

(defvar *published-ontology-footer*
  (format nil 
          "<h2>Contact Point</h2>
Email: <a href=\"mailto:j.b.domingue@open.ac.uk\">John Domingue (j.b.domingue@open.ac.uk)</a>
<hr>
<TABLE width=100%  BORDER=\"0\" CELLSPACING=\"2\" CELLPADDING=\"1\">
<TR valign=top><TD align=left><A HREF=\"http://kmi.open.ac.uk/\" TARGET=\"x\"><IMG align=left SRC=\"http://kmi.open.ac.uk/logos/imgs/kmi.gif\" BORDER=\"0\"></A>
</TD>
<TD align=right><A HREF=\"http://www.open.ac.uk/\" TARGET=\"x\"><IMG align=right SRC=\"http://kmi.open.ac.uk/logos/imgs/ou_hor.gif\" BORDER=\"0\"></A>
</TD></TR>
</TABLE></body></HTML>"))

(defvar *published-ontology-root-url*
  (concatenate 'string 
               web-onto::*default-host*
               "/ocml/"))

(defvar *goto-webonto-image-path*
 (concatenate 'string 
               web-onto::*default-host*
               "/ocml-image/goto-webonto-image.JPG"))

(defvar *published-ontology-page-file-extension*
  "html")

(defvar *root-published-ontology-directory*
  "ocml:web;")

(defvar *index-page-name-only*
  "index")

(defvar *index-page-name*
  (concatenate 'string
               *index-page-name-only* "." 
               *published-ontology-page-file-extension*))

(defvar *published-ontology-type-name-separator*
  "-")

(defvar *default-ocml-load-file-name*
  "load")

(defun publish-ontology (ontology &optional 
                                  (root-directory
                                   *root-published-ontology-directory*))
  (let ((ontology-structure (ocml::get-ontology ontology)))
    (when ontology-structure
      (ocml::with-ocml-thread-safety ()
        (ocml::select-ontology ontology)
        (let* ((files (ocml::ontology-files ontology-structure))
               (ontology-type (ocml::ontology-type ontology-structure))
               (load-file *default-ocml-load-file-name*)
               (documentation
                (slot-value ontology-structure 'ocml::documentation))
               (web-directory 
                (translate-logical-pathname
                 (ontology-directory-name-from-type 
                  ontology-type ontology
                  root-directory)))
               (source-directory 
                (translate-logical-pathname
                 (ontology-directory-name-from-type 
                  ontology-type ontology))))
          (ensure-directories-exist web-directory)
          (generate-published-ontology-index-page :ocml
                                                  web-directory
                                                  ontology 
                                                  ontology-type 
                                                  documentation 
                                                  (cons load-file files))
          (mapc 
           #'(lambda (file)
               (generate-published-ontology-page
                ontology :ocml 
                documentation web-directory source-directory file))
           (cons load-file files))
          (publish-ontolingua-ontology ontology ontology-type
                                       documentation 
                                       source-directory load-file 
                                       files))))))

(defun ontology-url-from-type (root-url ontology-type)
  (format nil "~a~a/"
          root-url
          (ontology-url-path-from-type ontology-type)))

(defun ontology-url-path-from-type (ontology-type)
  (string-downcase
   (symbol-name 
    (ontology-type-to-library-directory ontology-type))))
    

(defun generate-ontology-file-url (root-url ontology ontology-type file)
  (if (basic-type-p ontology-type)
      (format nil
              "~a~a.~a"
              (ontology-url-from-type root-url ontology-type)
              file
              *published-ontology-page-file-extension*)
    (format nil
            "~a~(~a~)/~a.~a"
            (ontology-url-from-type root-url ontology-type)
            ontology
            file
            *published-ontology-page-file-extension*)))

(defun generate-ontology-index-file-url (root-url ontology ontology-type)
  (generate-ontology-file-url root-url ontology ontology-type 
                              *index-page-name-only*))

(defun generate-published-ontology-index-page (representation-language
                                               directory
                                               ontology ontology-type
                                               documentation
                                               files
                                               &optional 
                                               (root-url 
                                                *published-ontology-root-url*))
  (let ((file (merge-pathnames *index-page-name* directory)))
    (when (probe-file file)
      (delete-file file))
    (with-open-file (ostream file :direction :output 
                             :if-does-not-exist :create)
      (format ostream *published-ontology-header*)
      (format ostream "<h1>Ontology ~:(~a~)</h1>" ontology)
      (when documentation
        (format ostream "<p>~a</p>" documentation))
      (format ostream 
              "<p>The Ontology ~:(~a~) contains the following files represented in ~a.</p><ul>" 
              ontology representation-language)
      (mapc #'(lambda (file)
                (format ostream 
                        "<li><a href=\"~a\">~a</a></li>"
                        (generate-ontology-file-url 
                         root-url
                         ontology 
                         ontology-type file)
                        file))
            files)
      (format ostream "~%</ul>~&")
      (format ostream *published-ontology-footer*))))

(defun generate-library-index-page (representation-language 
                                    ontology-names
                                    root-directory
                                    root-url)
  (let ((index-file 
         (merge-pathnames *index-page-name* root-directory)))
    (when (probe-file index-file)
      (delete-file index-file))
    (with-open-file (ostream index-file :direction :output 
                             :if-does-not-exist :create)
      (format ostream *published-ontology-header*)
      (format ostream "<h1>Index Page for the WebOnto Library</h1>")
      (format ostream 
              "The library contains the following knowledge models represented in ~a." representation-language)
      (mapc #'(lambda (type)
                (generate-type-specific-index-page-links ostream
                 (sort (knowledge-models-of-type ontology-names type)
                       #'string<)
                 type
                 root-url))
            *ocml-model-types*)
      (format ostream *published-ontology-footer*))))

(defun knowledge-models-of-type (ontology-names type)
  (ocml::filter ontology-names
		#'(lambda (x)
		    (eq (ocml::ontology-type (ocml::get-ontology x))
			type))))
      
(defun generate-type-specific-index-page-links (ostream 
                                                ontology-names 
                                                ontology-type
                                                root-url)
  (format ostream "<h2>~:(~a~) Models</h2>" ontology-type)
  (mapc #'(lambda (ontology)
            (format ostream 
                    "<li><a href=\"~a\">~:(~a~)</a></li>"
                    (generate-ontology-index-file-url root-url
                                                      ontology
                                                      ontology-type)
                    ontology))
        ontology-names))

(defun generate-published-ontology-page (ontology 
                                         representation-language
                                         documentation
                                         web-directory 
                                         source-directory
                                         file
                                         &optional
                                         (source-file-extension
                                          cl-user::*lisp-suffix*)
                                         (root-url 
                                          *published-ontology-root-url*))
  (let ((ofile (merge-pathnames 
                (make-pathname :name 
                               file 
                               :type 
                               *published-ontology-page-file-extension*)
                web-directory))
        (ifile (merge-pathnames 
                (make-pathname :name file 
                               :type 
                               source-file-extension)
                source-directory)))
    (when (probe-file ofile)
      (delete-file ofile))
    (with-open-file (ostream ofile :direction :output 
                             :if-does-not-exist :create)
      (with-open-file (istream ifile :direction :input)
        (format ostream *published-ontology-header*)
        (format ostream "<h1>Ontology: ~:(~a~); Representation Language: ~a; File: ~a</h1>" 
                ontology representation-language file)
        (when documentation
          (format ostream "<p>~a</p>" documentation))
        (format ostream "<pre>")
        (do ((line (read-line istream nil nil)
                   (read-line istream nil nil)))
            ((null line))
          (format ostream "~a~%" 
                  (http::internal-insert-ocml-links 
                   (maybe-insert-published-item-definition-anchor 
                    representation-language
                    ontology line)
                   #'(lambda (current-word current-word-length
                                           ontology 
                                           bold-home-ontology-p link-url)
                       (lookup-word-for-published-ontology
                        root-url
                        current-word current-word-length ontology
                        bold-home-ontology-p link-url))
                   ontology
                   t)))
        (format ostream "</pre>")
        (format ostream *published-ontology-footer*)))))

(defun trim-defining-string (line)
  (string-trim '(#\space #\tab #\return #\linefeed #\()
                           line))

(defmethod get-approved-defs ((representation-language (eql :ocml)))
  *approved-ocml-defs*)

(defvar *approved-ontolingua-defs*
  '(ocml::define-frame ocml::define-individual 
                       ocml::define-axiom 
                       ocml::define-function ocml::define-relation))


(defvar *ontolingua-defs-to-types*
  '((ocml::define-frame :class)
    (ocml::define-axiom :axiom)
    (ocml::define-relation :relation)
    (ocml::define-function :function)
    (ocml::define-individual :instance)))


(defmethod get-approved-defs ((representation-language (eql :ontolingua)))
  *approved-ontolingua-defs*)

(defun find-ocml-defining-item (representation-language line)
  (setf line (string-upcase (trim-defining-string line)))
  (unless (zerop (length line))
    (dolist (ocml-def (get-approved-defs representation-language))
      (let ((ocml-string-def (symbol-name ocml-def)))
        (when (and (>= (length line) (length ocml-string-def))
                   (string= ocml-string-def 
                            (subseq line 0 (length ocml-string-def))))
          (return-from find-ocml-defining-item
            ocml-def))))))

(defmethod defining-item-to-type 
           ((representation-language (eql :ocml)) 
            ocml-defining-item)
  (second (assoc ocml-defining-item *ocml-defs-to-types*)))

(defmethod defining-item-to-type 
           ((representation-language (eql :ontolingua)) 
            ocml-defining-item)
  (second (assoc ocml-defining-item *ontolingua-defs-to-types*)))

(defun get-item-name (line)
  (setf line (trim-defining-string line))
  (let ((*package* (find-package "OCML")))
    (handler-case 
        (with-input-from-string (istream line)
          (read istream)
          (read istream))
      (serious-condition (c)
	(declare (ignore c))))))

(defvar *no-webonto-anchor-p* nil)

(defun no-webonto-anchor ()
  (setf *no-webonto-anchor-p* t))

(defun insert-webonto-anchor ()
  (setf *no-webonto-anchor-p* nil))

(defun maybe-insert-published-item-definition-anchor 
       (representation-language 
        ontology line)
  (let ((ocml-defining-item (find-ocml-defining-item 
                             representation-language line)))
    (if ocml-defining-item
        (let ((item-name (get-item-name line)))
          (if item-name
              (if *no-webonto-anchor-p*
                  (concatenate 'string
                               "<a name=\""
                               (symbol-name 
                                (defining-item-to-type
                                 representation-language
                                 ocml-defining-item))
                               *published-ontology-type-name-separator*
                               (if (symbolp item-name)
                                   (symbol-name item-name)
                                 (format nil "~a" item-name))
                               "\"></a>"
                               line)                
                (concatenate 'string
                             "<a name=\""
                             (symbol-name 
                              (defining-item-to-type
                               representation-language
                               ocml-defining-item))
                             *published-ontology-type-name-separator*
                             (if (symbolp item-name)
                                 (symbol-name item-name)
                               (format nil "~a" item-name))
                             "\"></a>"
                             line
                             " "
                             (webonto-anchor representation-language
                                             ontology 
                                             ocml-defining-item item-name)))
            line))
      line)))

(defun convert-defining-type-name-to-type2 (defining-type)
  (intern 
   (symbol-name 
    (ocml::convert-defining-type-name-to-type defining-type))
   (find-package "KEYWORD")))


(defun webonto-anchor (representation-language 
                       ontology ocml-defining-item item-name)
  (let ((item-type 
         ;;(convert-defining-type-name-to-type2 ocml-defining-item)))
         (defining-item-to-type representation-language
                                ocml-defining-item)))
    (http::anchor 
     (format nil "http://~a:~d/webonto?ontology=~a&name=~a&type=~a"
             ;;(tcp::full-hostname)
             "plainmoor.open.ac.uk" http::*http-port*
             ontology 
             item-name
             item-type)
     (html:inline-image *goto-webonto-image-path*
                        :remote t :align "BOTTOM"
                        :border "0" 
                        :alt 
                        (format nil 
                                "Show the ~(~a~) ~(~a~) in WebOnto" 
                                item-type item-name)))))

(defun lookup-word-for-published-ontology 
       (root-url current-word current-word-length ontology
                     bold-home-ontology-p link-url)
  (declare (ignore link-url))
  (let* ((current-word-string 
          (coerce (subseq current-word 0 current-word-length)
                                      'string))
         (reverse-current-word-string (reverse current-word-string))
         (word-to-lookup (intern (string-upcase current-word-string)
                                 (find-package "OCML"))))
    (multiple-value-bind (found-p home-ontology-p colour
                                  home-ontology type class)
        (ocml::ocml-lookup word-to-lookup ontology t)
      (if found-p
          (add-published-ontology-link
           root-url word-to-lookup current-word-string type
                         (and bold-home-ontology-p home-ontology-p)
                         colour home-ontology class)
          reverse-current-word-string))))

(defun add-published-ontology-link (root-url word-to-lookup current-word
                                                   type
                                                   bold-p colour 
                                                   home-ontology
                                                   &optional class)
  (when colour
    (setf current-word (http::font current-word :color colour)))
  (when bold-p
    (setf current-word (http::bold current-word)))
  (reverse (http::anchor 
            (generate-published-item-anchor 
             root-url word-to-lookup type home-ontology class)
            current-word)))

(defun generate-published-item-anchor (root-url
                                       word-to-lookup type 
                                       home-ontology 
                                                      &optional class)
  (let* ((ontology-type 
          (ocml::ontology-type 
           (ocml::get-ontology home-ontology)))
         (full-pathname
          (ocml-item-source-file word-to-lookup type
                                 home-ontology class))
         (file (when (and full-pathname (not (eq full-pathname :unknown)))
                 (pathname-name full-pathname)))
         (file-url (when file
                     (generate-ontology-file-url root-url
                      home-ontology ontology-type file))))
    (concatenate 'string
                 file-url
                 "#"
                 (symbol-name type)
                 *published-ontology-type-name-separator*
                 (symbol-name word-to-lookup))))

(defun publish-all-ontologies (&optional (ontology-names (mapcar #'car ocml::*all-ontologies*)))
  (generate-library-index-page :ocml ontology-names
                               (translate-logical-pathname
                                *root-published-ontology-directory*)
                               *published-ontology-root-url*)
  (generate-library-index-page
   :ontolingua ontology-names
   (translate-logical-pathname
    *root-published-ontolingua-ontology-directory*)
   *published-ontolingua-ontology-root-url*)
  (mapc #'(lambda (ontology-name )
            (publish-ontology ontology-name))
        ontology-names))
      
      


                                                         