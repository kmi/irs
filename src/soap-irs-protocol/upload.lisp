(in-package ip)

(defun cleanup-code (string)
  (remove #\return string))

(defun upload (knowledge-model-name knowledge-model-uses
               knowledge-model-type knowledge-model-format
               url-string upload-mode author password allowed-editors
               html-stream)
  (if (password-ok-p author password)
      (multiple-value-bind (result code)
          (utilities:http-request url-string)
        (if (eq code 200)
            (internal-upload knowledge-model-name knowledge-model-uses
                             knowledge-model-type knowledge-model-format
                             result upload-mode author password
                             allowed-editors html-stream)
            (format html-stream "HTTP error ~A~%" knowledge-model-uses)))))

(defun internal-upload (knowledge-model-name knowledge-model-uses knowledge-model-type knowledge-model-format 
                                             code upload-mode author  password  allowed-editors html-stream)
  (if (password-ok-p author password)
      (let ((ocml-code (cleanup-code code)))
        (when (string= allowed-editors "")
          (setf allowed-editors nil))
        (case upload-mode
          ((ocml::new new) 
           (add-new-knowledge-model knowledge-model-name 
                                    knowledge-model-uses 
                                    knowledge-model-type 
                                    knowledge-model-format
                                    author allowed-editors
                                    ocml-code html-stream))
          ((ocml::append append) (append-knowledge-model 
                                  knowledge-model-name 
                                  knowledge-model-type 
                                  knowledge-model-format
                                  author
                                  ocml-code html-stream))
          ((ocml::overwrite overwrite) 
           (overwrite-knowledge-model knowledge-model-name 
                                      knowledge-model-uses 
                                      knowledge-model-type 
                                      knowledge-model-format
                                      author allowed-editors
                                      ocml-code html-stream))
          (t (format html-stream "error unknown-upload-mode ~a~%"
                     upload-mode)))
        ;;new code for ISWC 2007 tutorial
        (when (find upload-mode '(ocml::new new ocml::append append ocml::overwrite overwrite))
          (maybe-translate-bpmo-to-wsmo 
           author knowledge-model-name knowledge-model-uses html-stream)))
    (if (web-onto::registered-user author)
        (format html-stream "error password-mistake ~a~%" password)
      (format html-stream "error unknown-user ~a~%" author))))


;;new code for ISWC 2007 tutorial

(defun mixed-case-to-hyphenated-strings (string)
  (when (string string)
    (let ((result nil))
      (dolist (char (coerce string 'list))
        (cond ((upper-case-p char)
               (push #\- result)
               (push (char-downcase char) result))
              (t (push char result))))
      (coerce (reverse result) 'string))))

(defun mixed-case-make-ocml-symbol (string)
  (intern (string-upcase (mixed-case-to-hyphenated-strings string))
          (find-package "OCML")))

(defvar *bpmo-ontology* 'ocml::bpmo12)

;;; XXX This is defined in the bpmo-to-irs application.  I'd move of
;;; this code there, but I don't understand the entry point, so I
;;; daren't touch it.
(defun generate-irs-code-for-all-processes ()
  (error "Dummy function.  Real one lives in the bpmo-to-irs application."))

(defun maybe-translate-bpmo-to-wsmo (author knowledge-model-name knowledge-model-uses
                                            html-stream)
  ;;(setf xx (list author knowledge-model-name knowledge-model-uses))
  (when (find *bpmo-ontology* knowledge-model-uses)
    (generate-irs-code-for-all-processes author knowledge-model-name
                                         html-stream)))

(defun password-ok-p (name password)
  (string= (second (web-onto::registered-user name)) password))

(defun add-new-knowledge-model (knowledge-model-name 
                                knowledge-model-uses 
                                knowledge-model-type 
                                knowledge-model-format
                                author allowed-editors
                                ocml-code html-stream)
  (let ((undefined-knowledge-models
         (web-onto::get-undefined-ontology-uses knowledge-model-uses))
        (knowledge-model (ocml::get-ontology knowledge-model-name)))
    (cond (knowledge-model
           (format html-stream "error knowledge-model-exist ~a~%" 
                   knowledge-model-name))
          (undefined-knowledge-models
           (format html-stream 
                   "error undefined-knowledge-models-in-use-list undefined-ontologies ~{~a ~}~%" undefined-knowledge-models))
          (t (web-onto::define-new-ontology 
              html-stream knowledge-model-name knowledge-model-type
              knowledge-model-uses
              author allowed-editors)
             (append-code-to-knowledge-model knowledge-model-name 
                                             knowledge-model-type 
                                             knowledge-model-format
                                             ocml-code html-stream)))))

(defun append-knowledge-model (knowledge-model-name 
                               knowledge-model-type 
                               knowledge-model-format
                               author
                               ocml-code html-stream)
  (let ((knowledge-model (ocml::get-ontology knowledge-model-name)))
    (cond (knowledge-model
           (if (or (web-onto::root-user-p author)
                   (web-onto::allowed-editor-p knowledge-model author)
                   (web-onto::ontology-owner-p knowledge-model author))
               (append-code-to-knowledge-model knowledge-model-name 
                                               knowledge-model-type
                                               knowledge-model-format
                                               ocml-code html-stream)
             (format html-stream 
                     "error not-allowed-to-edit ~a ~a~%" 
                     knowledge-model-name author)))
          (t 
           (format html-stream 
                   "error knowledge-model-does-not-exist ~a~%" 
                   knowledge-model-name)))))

(defun overwrite-knowledge-model (knowledge-model-name 
                                  knowledge-model-uses 
                                  knowledge-model-type 
                                  knowledge-model-format
                                  author allowed-editors
                                  ocml-code html-stream)
      (cond ((ocml::get-ontology knowledge-model-name)
             (web-onto::internal-delete-ontology
              html-stream knowledge-model-name 
              (ocml::get-ontology knowledge-model-name) author)
             (add-new-knowledge-model knowledge-model-name 
                                knowledge-model-uses 
                                knowledge-model-type 
                                knowledge-model-format
                                author allowed-editors
                                ocml-code html-stream))
            (t (format html-stream 
                       "error knowledge-model-does-not-exist  ~a~%" 
                       knowledge-model-name))))

(defun rdf-format-type-p (knowledge-model-format)
  (eq knowledge-model-format 'ocml::rdf))

(defun append-code-to-knowledge-model (knowledge-model-name 
                                       knowledge-model-type 
                                       knowledge-model-format
                                       ocml-code html-stream)
  (when (rdf-format-type-p knowledge-model-format)
    (error "No way to translate RDF to OCML."))
  (setf ocml-code 
        (substitute  #\linefeed
                     web-onto::*ocml-line-separator*                   
              ocml-code))
  (let* ((logical-ontology-directory
	  (web-onto::ontology-directory-name-from-type knowledge-model-type 
                                                       knowledge-model-name))
         (knowledge-model-directory 
          (translate-logical-pathname logical-ontology-directory))
         (webonto-edits-file 
          (merge-pathnames knowledge-model-directory
                           web-onto::*webonto-edits-filename*)))
    (with-open-file (ostream webonto-edits-file :direction :output 
                             :if-exists :append)
      (princ ocml-code ostream)
      (terpri ostream))
    (handler-case 
        (progn 
          (load webonto-edits-file)
          (format html-stream "OK~%"))
      (serious-condition 
       (c)
       (format html-stream "error error-loading-file~%")))))