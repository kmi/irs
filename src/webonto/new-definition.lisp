;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defvar *planet-home-pathnames*
  `((ocml::kmi-planet-ontology
     ,(merge-pathnames
       "ocml:library;domains;kmi-planet-ontology;"
       "new.lisp"))
    (ocml::kmi-planet-kb
     ,(merge-pathnames
       "ocml:library;domains;kmi-planet-kb;"
       "new.lisp"))
    (ocml::kmi-new
     ,(merge-pathnames
       "ocml:library;domains;kmi-new;"
       "new.lisp"))
    (ocml::event-ontology
     ,(merge-pathnames
       "ocml:library;domains;event-ontology;"
       "new.lisp"))
    (ocml::news-ontology
     ,(merge-pathnames
       "ocml:library;domains;news-ontology;"
       "new.lisp"))
    (ocml::persons-and-organizations
     ,(merge-pathnames
       "ocml:library;domains;persons-and-organizations;"
       "new.lisp"))))


(defun setup-ontology-home-pathnames ()
  (mapc #'(lambda (ontology-home-pathname)
            (setf (ocml::ontology-new-source-location (ocml::get-ontology (car ontology-home-pathname)))
                  (second ontology-home-pathname)))
        *ontology-home-pathnames*))

(defun setup-planet-home-pathnames ()
  (mapc #'(lambda (ontology-home-pathname)
            (setf (ocml::ontology-new-source-location (ocml::get-ontology (car ontology-home-pathname)))
                  (second ontology-home-pathname)))
        *planet-home-pathnames*))

#+lispworks
(defun add-new-definition-source (ontology-name new-source &optional name type)
  (declare (ignore name type))
  ;;;name and type used by allegro version
  (let* ((ontology (get-ontology ontology-name))
	 (new-source-location ;;(translate-logical-pathname
                              (ocml::ontology-new-source-location ontology))
	 (buffer (editor::find-file-buffer new-source-location))
         (epoint (editor::buffers-end buffer))
         (spoint (editor::copy-point epoint)))
    (ocml::select-ontology ontology-name)
    (editor:insert-string epoint (format nil "~%~%~a" new-source))
    (editor::character-offset spoint (- (length new-source)))
    ;;      (setf a (editor:points-to-string spoint epoint))
    ;;;make the same for unix and pc compilation not needed says enrico 
    ;;(editor::region-compile (editor:point-buffer spoint) spoint epoint)
    (if (deliver-mode-p)
        (delivery-eval-and-save 
         (editor:point-buffer spoint) 
         new-source-location)
      (eval-and-save (editor:point-buffer spoint) 
                     new-source-location
                     spoint epoint))))

(defun class-type (x)
  (eq x 'ocml::def-class))

(defun instance-type (x)
  (eq x 'ocml::def-instance))

(defun superclass-of? (class-x class-y)
  (find class-x (ocml::domain-superclasses class-y)))

(defun higher-defining-type-p (dspec-a dspec-b)
  (<= (position (car dspec-a) *approved-ocml-defs*)
               (position (car dspec-b) *approved-ocml-defs*)))


;;split classes into those who have not parents in the set and those that do
(defun split-dspecs (dspecs)
  (let ((classes (mapcan #'(lambda (dspec)
                             (when (and (class-type (car dspec))
                                        (ocml::get-ocml-class (second dspec)))
                               (list (ocml::get-ocml-class (second dspec)))))
                         dspecs))
        no-parents with-parents non-class-dspecs)
    (mapc #'(lambda (dspec)
              (cond ((and (class-type (car dspec))
                          (ocml::get-ocml-class (second dspec)))
                     (if (intersection
                          (ocml::direct-domain-superclasses
                           (ocml::get-ocml-class (second dspec)))
                          classes)
                         (push dspec with-parents)
                         (push dspec no-parents)))
                    (t (push dspec non-class-dspecs))))
          dspecs)
    (values no-parents with-parents non-class-dspecs)))


(defun order-dspecs-by-sets (dspecs)
  (multiple-value-bind (ordered-class-dspecs with-parents non-class-dspecs)
      (split-dspecs dspecs)
    (do ()
      ((null with-parents)
       (append ordered-class-dspecs
               (sort non-class-dspecs #'higher-defining-type-p)))
    (multiple-value-bind (no-parents new-with-parents)
        (split-dspecs with-parents)
      (setf ordered-class-dspecs
            (append ordered-class-dspecs no-parents)
            with-parents new-with-parents)))))
  

(defun ocml-definition-p (definition)
  (find (car definition) *approved-ocml-defs*))

(defun get-file-dspecs (file)
  (let ((*package* (find-package "OCML")))
    (with-open-file (istream file :direction :input)
      (do ((next-def (read istream nil nil nil) (read istream nil nil nil))
           (definitions nil (if (ocml-definition-p next-def)
                                (cons (list (car next-def) (second next-def)) definitions)
                                definitions)))
          ((null next-def) (if (ocml-definition-p next-def)
                              (cons (list (car next-def) (second next-def)) definitions)
                              definitions))))))

(defvar *start-of-buffer-string*
    ";;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package \"OCML\")

(in-ontology ~a)

")

#+lispworks
(defun ontology-buffer-definitions (ontology-name)
  (ocml::select-ontology ontology-name)
  (let* ((ontology (get-ontology ontology-name))
	 (new-source-location ;;(translate-logical-pathname
                              (ocml::ontology-new-source-location ontology))
	 (buffer (editor::find-file-buffer new-source-location))
         (buffer-definitions (copy-list
			      (editor::buffer-definitions buffer)))
         )
    buffer-definitions))

(defun remove-bad-definitions (buffer-definitions)
  (let ((good-definitions nil)
        (bad-definitions nil))
    (mapc #'(lambda (dspec)
                (if (and (class-type (car dspec))
                         (not (ocml::get-ocml-class (second dspec))))
                    (push dspec bad-definitions)
                    (push dspec good-definitions)))
            buffer-definitions)
    (values (reverse good-definitions) (reverse bad-definitions))))


(defun new-source-location (ontology)
  ;;(translate-logical-pathname
  (ocml::ontology-new-source-location ontology))

(defun generate-new-source-location (ontology new-name)
  (let ((directory-pathname (ontology-directory ontology)))
    (make-pathname :directory
                   (pathname-directory directory-pathname)
                   :name new-name
                   :host (pathname-host directory-pathname))))

(defun old-source-location (ontology)
  (generate-new-source-location ontology "old.lisp"))

(defun saved-source-location (ontology)
  (generate-new-source-location ontology "saved-new.lisp"))

(defun copy-file (file1 file2)
  (with-open-file (istream file1 :direction :input)
    (when (probe-file file2)
      (delete-file file2))
    (with-open-file (ostream file2 :direction :output :if-does-not-exist :create)
      (do ((line (read-line istream nil nil) (read-line istream nil nil)))
          ((null line))
        (write-line line ostream)))))

(defun save-current-ontology-version (ontology)
  (let ((new-source-location (new-source-location ontology))
        (saved-source-location (saved-source-location ontology)))
    (copy-file new-source-location saved-source-location)))


;(defun reorder-new-definitions-save-and-compile (ontology-name)
;  (ocml::select-ontology ontology-name)
;  (let* ((ontology (get-ontology ontology-name))
;	 (new-source-location (new-source-location ontology))
;         (old-source-location (old-source-location ontology))
;         (buffer (editor::find-file-buffer new-source-location))
;	 (buffer-definitions (editor::buffer-definitions buffer)))
;    ;;(editor::goto-buffer buffer t)
;    (when buffer-definitions
;      (editor::use-buffer buffer
;        (copy-file new-source-location old-source-location)
;        (multiple-value-bind (buffer-definitions bad-definitions)
;            (remove-bad-definitions
;	     (copy-list buffer-definitions))
;          (let* ((buffer-top-classes (top-classes buffer-definitions))
;                 (ordered-buffer-defintions (append buffer-top-classes
;					            (sort (set-difference buffer-definitions
;                                                                          buffer-top-classes
;                                                                          :test #'equal)
;                                                          #'higher-defining-type-p)))
;                 (buffer-sources (mapcar #'(lambda (definition)
;                                             (let ((source-string (get-source-string definition)))
;                                               (when (null source-string)
;                                                 (error "Null source string for ~a" definition))
;                                               (format nil "~a~%~%" source-string)))
;                                         ordered-buffer-defintions))
;                 (current-point (editor::current-point))
;                 ;;(current-mark (editor::current-mark nil t))
;                 (epoint (editor::buffers-end buffer))
;                 (spoint (editor::buffers-start buffer)))
;            (editor::move-point current-point spoint)
;            ;;(editor::move-point current-mark epoint)
;            (editor::set-current-mark epoint)
;            (editor::kill-region-command current-point)
;            (editor::move-point current-point (editor::buffers-end buffer))
;            (editor:insert-string current-point (format nil *start-of-buffer-string*
;                                                        (string-downcase (format nil "~a" ontology-name))))
;            (mapc #'(lambda (buffer-source)
;                      (editor:insert-string current-point buffer-source))
;                  buffer-sources)
;            (when bad-definitions
;              (editor:insert-string
;               current-point
;	       (format nil "~%~%;;;Had problems with these definitions~%~%#|~%~{~a~%~}~%|#"
;		       (mapcar #'(lambda (definition)
;				   (list definition (get-source-string definition)))
;			       bad-definitions))))
;            (EDITOR::%SET-BUFFER-MODIFIED buffer)
;            ;;(editor:buffer-modified buffer)
;            ;;(EDITOR::RESET-BUFFER-MODIFIED buffer)
;            ;;(EDITOR::NOTICE-BUFFER-MODIFIED buffer nil)
;            (editor::write-da-file buffer new-source-location)
;        
;            ;;(compile-file new-source-location)
;            ))))))

#+lispworks
(defun reorder-new-definitions-save-and-compile (ontology-name)
  (ocml::select-ontology ontology-name)
  (let* ((ontology (get-ontology ontology-name))
	 (new-source-location (new-source-location ontology))
         (old-source-location (old-source-location ontology))
         (buffer (editor::find-file-buffer new-source-location))
         #+lispworks3.2
         (dspecs (editor::buffer-definitions buffer))
         #+lispworks4
	 (dspecs (editor:buffer-dspecs buffer)))
    ;;(editor::goto-buffer buffer t)
    (when dspecs
      (editor::use-buffer buffer
        (copy-file new-source-location old-source-location)
        (multiple-value-bind (dspecs bad-definitions)
            (remove-bad-definitions
	     (copy-list dspecs))
	  (let* ((ordered-dspecs (order-dspecs-by-sets dspecs))
                 (buffer-sources (mapcar #'(lambda (definition)
                                             (let ((source-string
                                                    (get-source-string
                                                     (append definition
                                                             (list ontology-name)))))
                                               (when (null source-string)
                                                 (error "Null source string for ~a" definition))
                                               (format nil "~a~%~%" source-string)))
                                         ordered-dspecs))
                 (current-point (editor::current-point))
                 ;;(current-mark (editor::current-mark nil t))
                 (epoint (editor::buffers-end buffer))
                 (spoint (editor::buffers-start buffer)))
            (editor::move-point current-point spoint)
            ;;(editor::move-point current-mark epoint)
            (editor::set-current-mark epoint)
            (editor::kill-region-command current-point)
            (editor::move-point current-point (editor::buffers-end buffer))
            (editor:insert-string current-point (format nil *start-of-buffer-string*
                                                        (string-downcase (format nil "~a" ontology-name))))
            (mapc #'(lambda (buffer-source)
                      (editor:insert-string current-point buffer-source))
                  buffer-sources)
            (when bad-definitions
              (editor:insert-string
               current-point
	       (format nil "~%~%;;;Had problems with these definitions~%~%#|~%~{~a~%~}~%|#"
		       (mapcar #'(lambda (definition)
				   (list definition
                                         (get-source-string
                                                     (append definition
                                                             (list ontology-name)))))
			       bad-definitions))))
            ;;new for pc vers
            ;;(EDITOR::%SET-BUFFER-MODIFIED buffer)
            (setf (editor::buffer-modified buffer) t)
            
            ;;(editor:buffer-modified buffer)
            ;;(EDITOR::RESET-BUFFER-MODIFIED buffer)
            ;;(EDITOR::NOTICE-BUFFER-MODIFIED buffer nil)
            (editor::write-da-file buffer new-source-location)
        
            ;;(compile-file new-source-location)
            ))))))

  
      


  