;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

;; Strip out the ontology name which is now included in the dspec
(defun dspec-type-and-name (dspec)
  (if (eq (car dspec) 'ocml::def-instance)
      (list (car dspec) (second dspec) (third dspec))
      (list (car dspec) (second dspec))))

(defun dspec-ontology (dspec)
  (third dspec))

(defun instance-def-class-name (instance-source-code)
  (third instance-source-code))

(defun find-definition-in-buffer (dspec buffer)
  #+:lispworks
  (editor:find-dspec-in-buffer dspec buffer (editor:buffer-point buffer)))

(defun get-source-file (dspec)
  #+(and :lispworks (not :lispworks-dspec))
  (compiler::source-file dspec)
  #+:lispworks-dspec
  (list dspec ;;;from previous version pre-lispworks (dspec:find-dspec-locations dspec))))
        ;;john domingue 3/9/2007
        (second (second (dspec:find-dspec-locations dspec)))))

(defun ocml-item-source-file (name type ontology &optional class-name)
  (second 
   (get-source-file
    (dspec-from-name-type-ontology name type ontology
                                   class-name))))

(defun dspec-from-name-type-ontology (name type ontology
                                      &optional class-name)
  (case type
    ((:class) `(ocml::def-class ,name ,ontology))
    ((:relation) `(ocml::def-relation ,name ,ontology))
    ((:function) `(ocml::def-function ,name ,ontology))
    ((:procedure) `(ocml::def-procedure ,name ,ontology))
    ((:axiom) `(ocml::def-axiom ,name ,ontology))
    ((:rule) `(ocml::def-rule ,name ,ontology))    
    ((:instance) `(ocml::def-instance ,name ,class-name ,ontology))))



#+lispworks
(defun get-source-string (dspec-with-ontology)
  (system::when-let (dspec-and-file (get-source-file dspec-with-ontology))
    (let ((dspec (dspec-type-and-name
                  (if (eq (caar dspec-and-file) 'harlequin-common-lisp:subfunction)
                      (second (car dspec-and-file))
                      (car dspec-and-file))))
          (file (second dspec-and-file)))
      (system::when-let (buffer (editor:find-file-buffer file))
        (system::when-let (point (find-definition-in-buffer
                                  dspec
                                  buffer))
          (editor:with-point ((spoint point) 
                              (epoint point)) 
            (editor:form-offset epoint 1) 
            (editor:points-to-string spoint epoint)))))))

#+lispworks
(defun get-dspec-buffer (dspec) 
  (system::when-let (dspec-and-file (get-source-file dspec)) 
    (let ((dspec (first dspec-and-file)) 
          (file (second dspec-and-file)))
      (when (and file (pathname-name file))
        ;;check that there's a real file here and not just a directory
        (editor:find-file-buffer file)))))

#+lispworks
(defun get-source-string-via-Find-Dspec (dspec)
  (let ((dspec-type-and-name (dspec-type-and-name dspec)))
    (system::when-let (point (find-definition-in-buffer dspec-type-and-name
                                                           (get-dspec-buffer dspec)))
      (editor:with-point ((spoint point)
                          (epoint point))
        (editor:form-offset epoint 1)
        (editor:points-to-string spoint epoint)))))

(defvar cl-user::*deliver-mode*)

(defun deliver-mode-p ()
  (boundp 'cl-user::*deliver-mode*))

#+lispworks
(defun replace-source-string-via-Find-Dspec (old-dspec new-dspec new-source
                                                       &optional (save-old-source-p t))
  (let ((old-type-and-name (dspec-type-and-name old-dspec))
        spoint epoint insert-point)
    (system::when-let (point (find-definition-in-buffer 
                              old-type-and-name
                              (get-dspec-buffer old-dspec)))

        (setf epoint (editor::copy-point point)
              spoint (editor::copy-point point)
              insert-point (editor::copy-point point))
        (let ((old-form (editor:points-to-string spoint epoint)))
          (editor:form-offset epoint 1)
          (editor:delete-between-points spoint epoint)
          (editor:insert-string insert-point new-source)
          (editor:form-offset spoint -1)
          (if (deliver-mode-p)
              (delivery-eval-and-save 
               (editor:point-buffer spoint) 
               (editor:buffer-pathname
                (editor:point-buffer spoint))
               new-dspec
               old-form save-old-source-p)
            (eval-and-save (editor:point-buffer spoint) 
                           (editor:buffer-pathname
                          (editor:point-buffer spoint))
                           spoint epoint new-dspec old-form save-old-source-p))
	  old-form))))

(defun delivery-eval-and-save (buffer buffer-pathname &optional
                                      new-dspec old-form save-old-source-p)
  (when save-old-source-p
    (save-old-source new-dspec old-form (dspec-ontology new-dspec)))
  (editor::write-da-file buffer buffer-pathname)
  ;;(editor:fast-save-all-buffers)
  (load buffer-pathname :verbose nil :print nil)
  )

(defun eval-and-save (buffer buffer-pathname spoint epoint &optional new-dspec old-form save-old-source-p)
  (editor::region-eval buffer spoint epoint)
  (when save-old-source-p
    (save-old-source new-dspec old-form (dspec-ontology new-dspec)))
  (editor::write-da-file buffer buffer-pathname))

#+lispworks
(defun delete-source (buffer defining-term defining-name
                             ontology
                             &optional (save-old-source-p t)
                             instance-class-name)
;;  (setf xx (list buffer defining-term defining-name
  ;;                           ontology))
  (let* ((type-and-name (if instance-class-name
                            (list defining-term defining-name instance-class-name)
                            (list defining-term defining-name))))
    (when buffer
      (system::when-let (point (find-definition-in-buffer type-and-name buffer))
        (editor:with-point ((spoint point)
                            (epoint point))      
          (editor:form-offset epoint 1)
          (let ((old-form (editor:points-to-string spoint epoint)))
            (editor:delete-between-points spoint epoint)
	    (when save-old-source-p
	      (save-old-source type-and-name old-form ontology))
	    (editor::write-da-file (editor:point-buffer spoint)
			           (editor:buffer-pathname
				    (editor:point-buffer spoint)))))))))

(defvar *old-source-directory* nil)

(defun start-editing-session (ontology)
  (let ((file (merge-pathnames *old-source-directory*
                                (ocml::pretty-structure-name ontology))))
    (save-current-ontology-version ontology)
  (when (probe-file file)
    (delete-file file))))

(defun end-editing-session (ontology)
  (reorder-new-definitions-save-and-compile ontology)
  (publish-ontology ontology))

(defun save-old-source (dspec old-source home-ontology-name)
  (let ((old-source-pathname
         (merge-pathnames *old-source-directory*
                          (ocml::pretty-string home-ontology-name))))
    (with-open-file (ostream old-source-pathname
                             :direction :output :if-exists :append
                             :if-does-not-exist :create)
      (print (list dspec old-source) ostream))))

(defun current-old-source-file ()
  (merge-pathnames *old-source-directory*
                   (ocml::pretty-structure-name ocml::*current-ontology*)))

(defun undo-source-changes (&optional count)
  (let ((*package* (find-package "OCML"))
        (old-source-file (current-old-source-file)))
    (when (probe-file old-source-file)
      (with-open-file (istream old-source-file :direction :input)
        (do ((next-form (read istream nil nil) (read istream nil nil))
	     (forms nil (cons next-form forms)))
            ((null next-form)
             (let ((forms-to-process (if count (subseq forms 0 count) forms)))
               (write-new-old-source-file old-source-file
                                          (reverse (when count
                                                     (subseq forms count))))
               (if forms-to-process
                   (process-source-undos forms-to-process)
                   "Nothing to undo"))))))))

(defun write-new-old-source-file (old-source-file new-forms)
  ;;;always delete file - overwriting is not safe
  (delete-file old-source-file)
  (when new-forms
    (with-open-file (ostream old-source-file :direction :output :if-exists
			     :overwrite
			     :if-does-not-exist :create)
      (dolist (new-form new-forms)
	(print new-form ostream)))))

(defun definition-dspec (form)
  (car form))

(defun definition-source-code (form)
  (second form))

(defun process-source-undos (definition-forms)
  (let ((undo-results nil))
    (dolist (definition-form definition-forms)
      (replace-source-string-via-Find-Dspec (definition-dspec definition-form)
                                            (definition-source-code definition-form)
                                            nil)
      (if (ocml-eval-from-string (definition-source-code definition-form))
          (push (format nil "Evaluated ~a~a"
                        (second (definition-dspec definition-form))
                        *ocml-line-separator*)
                undo-results)
          (push (format nil "Had trouble evaluating ~a~a"
                        (ocml::convert-multiple-line-string
                         (definition-source-code definition-form))
                        *ocml-line-separator*)
                undo-results)))
    undo-results))

(defvar *approved-ocml-defs*
  '(ocml::def-class ocml::def-axiom ocml::def-relation ocml::def-function
                      ocml::def-procedure ocml::def-rule ocml::def-instance ocml::def-anonymous-instance
                      ocml::def-domain-instance ocml::def-relation-instances
                      ocml::def-ontology ocml::def-pattern ocml::def-subclass-template 
                      ocml::def-new-instance-template
                      ))

(defvar *ocml-defs-to-types*
  '((ocml::def-class :class)
    (ocml::def-axiom :axiom)
    (ocml::def-relation :relation)
    (ocml::def-function :function)
    (ocml::def-procedure :procedure)
    (ocml::def-rule :rule)
    (ocml::def-instance :instance)
    (ocml::def-domain-instance :instance)
    (ocml::def-relation-instances :relation-instance)
    (ocml::def-ontology :ontology)
    (ocml::def-pattern :pattern)
    (ocml::def-subclass-template :sublcass-template) 
    (ocml::def-new-instance-template :instance-template)))

(defun instance-def-p (x)
  (eq x 'ocml::def-instance))

(defun relation-instance-def-p (x)
  (eq x 'ocml::def-relation-instances))

(defun process-new-source (defining-string source-code stream)
  (setf defining-string (string-upcase defining-string))
  (let* ((*package* (find-package "OCML")) defining-term defining-name
         instance-class-name home-ontology)
    (with-input-from-string (istream defining-string)
      (setf home-ontology (read istream) defining-term (read istream)
            defining-name (read istream) instance-class-name (read istream)))
    (ocml::select-ontology home-ontology)
    (internal-process-new-source defining-term defining-name source-code stream
                                 home-ontology instance-class-name)))

(defun internal-process-new-source (defining-term defining-name source-code stream
                                                  &optional
                                                  (ontology
                                                   (ocml::name
                                                    ocml::*current-ontology*))
                                                  instance-class-name)
  (handler-case
      (let* ((*package* (find-package "OCML"))
             ;;(downcase-code (string-downcase source-code))
             (upcase-code (string-upcase source-code))
	     (code (read-from-string upcase-code))
             output
             )
        (if code
            (cond ((find defining-term *approved-ocml-defs*)
                   (setf output
                         (process-line-for-sending
                          (with-output-to-string (*standard-output*)
                           
                            ;;can't eval code if name has changed - need to eval from
                            ;;;buffer (eval code)
                            (if (instance-def-p defining-term)
                                (replace-source-string-via-Find-Dspec
                                 (list defining-term defining-name
                                       instance-class-name
                                       ontology)
                                 (list (car code) (second code)
                                       (third code) ontology) source-code)
                              (replace-source-string-via-Find-Dspec
                                       
                               (list defining-term defining-name
                                     ontology)
                               (list (car code) (second code)
                                     ontology) source-code)))))
                   (when (relation-instance-def-p defining-term)
                     (ocml::unassert-old-relation-instance
                      defining-name))
                   (cond ((eq defining-name (second code))
                          (http::princ-to-binary-stream
                           (if (zerop (length output))
                               (if (relation-instance-def-p defining-term)
                                   (format nil
                                           "redefined Successfully change ~(~a~) to ~(~a~).~%"
                                           defining-name (second code))
                                 (format nil "redefined Successfully redefined ~(~a~).~%"
                                         defining-name))
                             (format nil "redefined Successfully redefined ~(~a~).~aWarnings were ~a~%"
                                     defining-name *ocml-line-separator* output))
                           stream))
                         (t  (ocml::delete-ocml-object
                              (ocml::convert-defining-type-name-to-type defining-term)
                              defining-name instance-class-name)
                             (http::princ-to-binary-stream
                              (format nil "new_name Successfully redefined ~a.~aWarnings were ~a~%"
                                      defining-name *ocml-line-separator* output)
                              stream))))
                  (t 
                   (http::princ-to-binary-stream
                    (format nil
                            "unknown_definition_type ~a Source ~a starts with a non-approved defining name.~
                             It must be one of ~{~a ~}~%"
                            defining-term source-code *approved-ocml-defs*)
                    stream)))               
          (http::princ-to-binary-stream
           (format nil "unable_to_find_souce Sorry couldn't find the source ~a~%" source-code)
           stream)))
    ;;use serious-condition to catch stack overflows
    (serious-condition 
            (c)
      (format *terminal-io*
              "~%Error ~a in processing new source with ~a" c source-code)
      (http::princ-to-binary-stream
       (format nil
               "Error ~a in processing new source with ~a~%" c source-code)
       stream))))

(defun instance-of-iff-def-class-p (ontology-string-name defining-term code)
  (when (instance-type defining-term)
    (let ((*package* (find-package "OCML")))
      (ocml::select-ontology (read-from-string ontology-string-name))
      (ocml::iff-def (ocml::get-relation (instance-def-class-name code))))))

(defun new-definition-result-type (defining-term)
  (if (eq defining-term 'ocml::def-new-instance-template)
      'template
      'new_definition))
  

(defun process-new-definition (ontology-string-name source-code stream)
  (handler-case
      (let* ((*package* (find-package "OCML"))
             ;;(downcase-code (string-downcase source-code))
             (upcase-code (string-upcase source-code))
	     (code (read-from-string upcase-code))
             (defining-term (car code))
             (defining-name (second code)) output
             result-type)
	(cond ((find defining-term *approved-ocml-defs*)
               (cond ((instance-of-iff-def-class-p ontology-string-name
                                                 defining-term code)
                      (http::princ-to-binary-stream
                       (format nil
                               "instance_of_iff_def_class The class ~(~a~) is defined by an iff-def, so the instance ~(~a~) was not created.~%" (instance-def-class-name code)
                               defining-name)
                       stream))
                     (t (setf output
                              (process-line-for-sending
                               (with-output-to-string (*standard-output*)
                                 (add-new-definition-source
                                  (read-from-string ontology-string-name)
                                  source-code defining-name defining-term)))
                              result-type
                              (new-definition-result-type defining-term))
                     
	                (http::princ-to-binary-stream
                         (if (zerop (length output))
                             (format nil
                                     "~(~a~) Successfully defined ~(~a~).~%"
			             result-type defining-name)
		             (format nil
                                     "~(~a~) Successfully defined ~(~a~). ~a Warnings were ~a~%"
			             result-type defining-name *ocml-line-separator* output))
		         stream))))
	      (t 
	       (http::princ-to-binary-stream
		(format nil
			"unknown_definition_type Source ~a starts with a non-approved defining name.~
                             It must be one of ~{~a ~}~%"
			source-code *approved-ocml-defs*)
		stream))))
    ;;use serious-condition to catch stack overflows
    (serious-condition
     (c)
     (format *terminal-io*
	     "~%Error ~a in processing new source with ~a" c source-code)
     (http::princ-to-binary-stream
      (format nil
	      "Error ~a in processing new source with ~a~%" c source-code)
      stream))))

(defun ocml-eval-from-string (source-code)
  (handler-case
      (let* ((*package* (find-package "OCML"))
	     (code (read-from-string (string-upcase source-code))))
        (when (and code (find (car code) *approved-ocml-defs*))
          (eval code)
          t))
    (error
     (c)
     (declare (ignore c))
     (format *terminal-io*
	     "~%Error in evaling new source ~a" source-code)
     nil)))
