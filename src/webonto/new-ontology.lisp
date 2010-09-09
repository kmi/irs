;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")


(defun get-undefined-ontology-uses (ontologies)
  (mapcan #'(lambda (ontology)
              (unless (get-ontology ontology)
                (list ontology)))
          ontologies))

(defun new-ontology (stream request-string)
  (with-input-from-string (request-string-stream request-string)
    (read request-string-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read request-string-stream))
           (ontology-type (read request-string-stream))
           (ontology-uses (read request-string-stream))
           (ontology-author (read request-string-stream))
           (allowed-editors (read request-string-stream))
           (ontology  (get-ontology ontology-name))
           (undefined-ontologies (get-undefined-ontology-uses ontology-uses)))
      (when (string= allowed-editors "")
        (setf allowed-editors nil))
      (cond (ontology
             (when stream               
               (http::princ-to-binary-stream
                (format nil "Sorry, the ontology ~a already exists~%" ontology-name)
                stream)))
            (undefined-ontologies
             (when stream
               (http::princ-to-binary-stream
                (format nil "Sorry, the ontolog~@p~{ ~(~a~)~} ~:[do~;does~] not exist~%"
                        (length undefined-ontologies) undefined-ontologies
                        (= (length undefined-ontologies) 1))
                stream)))
            (t (define-new-ontology stream ontology-name ontology-type
                                    ontology-uses ontology-author allowed-editors))))))

(defun define-new-ontology (stream ontology-name ontology-type ontology-uses ontology-author
                                   allowed-editors)
  (cond ((not (registered-user ontology-author))
         (http::princ-to-binary-stream
          (format nil "Sorry, ~a is not a registered user~%" ontology-author)
          stream))
        (t (add-ontology-files ontology-name ontology-type ontology-uses ontology-author allowed-editors)
           (load-new-ontology ontology-name)
	   (add-to-list-of-ontology-names ontology-name)
	   ;;(add-to-list-of-ontology-home-pathnames ontology-name ontology-type)
           (http::princ-to-binary-stream
            (format nil "OK~%")
            stream))))

(defun add-ontology-files (ontology-name ontology-type ontology-uses ontology-author allowed-editors)
  (let* ((logical-ontology-directory
	  (ontology-directory-name-from-type ontology-type ontology-name))
         (ontology-directory (translate-logical-pathname logical-ontology-directory)))
    (create-directory ontology-directory)
    (create-load-file ontology-directory
                      ontology-name ontology-type ontology-uses ontology-author allowed-editors)
    (create-new-definitions-file ontology-directory ontology-name)
    (create-second-definitions-file ontology-directory ontology-name)))

(defun create-directory (directory)
  (unless (probe-file directory)
    #+lispworks
    (ensure-directories-exist directory)
    ;;(foreign::call-system (format nil "mkdir ~a" directory))
    ;;(io::io-mkdir 
     ;;(if (pathnamep directory)
       ;;  (namestring directory)
       ;;directory))
    #+allegro (excl::run-shell-command (format nil "mkdir ~a" directory))))

(defun create-new-definitions-file (ontology-directory ontology-name)
  (create-definitions-file ontology-directory ontology-name *webonto-edits-filename*))

(defun create-second-definitions-file (ontology-directory ontology-name)
  (create-definitions-file ontology-directory ontology-name (format nil "~(~a~).lisp" ontology-name)))

(defun create-definitions-file (ontology-directory ontology-name filename)
  (let ((edits-file (merge-pathnames ontology-directory filename)))
    (when (probe-file edits-file)
      (delete-file edits-file))
    (with-open-file (ostream edits-file :direction :output :if-does-not-exist :create)
      (format ostream *edits-file-header*)
      (format ostream "~%~%(in-ontology ~(~a~))~%~%" ontology-name))))

(defvar *load-files*
    '((ocml::sisyphus1 . "load2.lisp"))
  "All the ontology load files which are not load.lisp are stored here")

;(defun setup-load-files ()
;  (mapc #'(lambda (ontology-and-load-file)
;            (setf (ocml::ontology-load-filename (get-ontology (car ontology-and-load-file)))
;                  (cdr ontology-and-load-file)))
;        *load-files*))

(defun create-load-file (ontology-directory ontology-name
					    ontology-type ontology-uses ontology-author allowed-editors)
  (let ((load-file (merge-pathnames ontology-directory ocml:*load-filename*))
        (parsed-allowed-editors
         (parse-allowed-editors allowed-editors)))
    (when (probe-file load-file)
      (delete-file load-file))
    (with-open-file (ostream load-file :direction :output :if-does-not-exist :create)
      (format ostream *load-file-header*)
      (cond (ontology-uses
             (format ostream "~%~%(eval-when (eval load)")
             (mapc #'(lambda (used-ontology)
                       (let ((ontology-structure (ocml::get-ontology used-ontology)))
                         (format ostream "~%  (ensure-ontology ~(~a~) ~(~a~) \"~(~a~)\" )"
                                 used-ontology
                                 (ocml::ontology-type ontology-structure)
                                 (ocml::ontology-logical-load-filename
                                  ontology-structure))))
                   ontology-uses)
             (format ostream ")")
             (format ostream
                     "~%~%(def-ontology ~(~a~) :includes ~(~a~)~
                                        ~%~14t:type ~(~s~) ~
                                        ~%~14t:author ~s ~:[~*~;:allowed-editors (~{~s ~}~s)~])"
                     ontology-name
                     ontology-uses
                     (make-ontology-type-name ontology-type)
                     ontology-author allowed-editors
                     (butlast parsed-allowed-editors)
                     (car (last parsed-allowed-editors))))
	     (t (format ostream
		        "~%~%(def-ontology ~(~a~) :type ~(~s~)~
                                         ~%~14t:author ~s ~:[~*~;:allowed-editors (~{~s ~}~s)~])"
                        ontology-name
                        (make-ontology-type-name ontology-type)
                        ontology-author allowed-editors
                        (butlast parsed-allowed-editors)
                        (car (last parsed-allowed-editors))))))))

(defun parse-allowed-editors (string)
  (multiple-value-list (parse-string-by-spaces string)))
            
    

(defun load-new-ontology (ontology-name)
  (internal-load-single-ontology ontology-name))

(defun add-to-list-of-ontology-names (ontology-name)
  (push ontology-name *ontologies*)
  (setf *ontologies-string*
        (concatenate 'string *ontologies-string*
                     (format nil "~%~(~a~)" ontology-name)))
  (save-ontologies-string))

;(defun add-to-list-of-ontology-home-pathnames (ontology-name ontology-type)
;  (let* ((logical-directory (ontology-directory-name-from-type ontology-type ontology-name))
;         (new-source-location (pathname
;                               (format nil "~a~a" (translate-logical-pathname logical-directory)
;				       *webonto-edits-filename*))))
;    (setf *ontology-home-pathnames-string*
;          (concatenate 'string *ontology-home-pathnames-string*
;                       (format nil "~%~(~a~)~% ~s~% ~s ~%~%" ontology-name
;                               logical-directory
;                               *webonto-edits-filename*)))
;    (setf *ontology-home-pathnames*
;          (cons (list ontology-name new-source-location) *ontology-home-pathnames*))
;    (setf (ocml::ontology-new-source-location (get-ontology ontology-name))
;          new-source-location)
;    (save-ontology-home-pathnames-string)))

(defun basic-type-p (ontology-type)
  (eq ontology-type :basic))

(defun ontology-directory-name-from-type (ontology-type 
                                          ontology-name
                                          &optional
                                          (root "ocml:library;"))
  (if (basic-type-p ontology-type)
      (format nil "~a~(~a~);"
              root
              (ontology-type-to-library-directory ontology-type))
    (format nil "~a~(~a~);~(~a~);"
            root
            (ontology-type-to-library-directory ontology-type)
            ontology-name)))

(defun ontology-type-to-library-directory (ontology-type)
  (case ontology-type
    ((application :application ocml::application) 'applications)
    ((domain :domain ocml::domain) 'domains)
    ((method :method ocml::method) 'methods)
    ((task :task ocml::task) 'tasks)
    ((goal :goal ocml::goal) 'goals)
    ((web-service :web-service ocml::web-service) 'web-services)
    ((mediator :mediator ocml::mediator) 'mediators)
    ((:basic basic ocml::basic) 'basic)))

(defun make-ontology-type-name (ontology-type)
  (intern (symbol-name ontology-type) (find-package "KEYWORD")))