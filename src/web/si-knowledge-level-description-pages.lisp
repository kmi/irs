(in-package http)

(defvar *irs-spec-languages* '("UPML" "OCML" "DAML-S"))

(defvar *ontology-types* '("application" "domain" "method" "task"))

(defvar *kl-definition-area-width* 80)

(defvar *kl-definition-area-height* 20)

(defvar *default-number-of-ontologies-used* 1)

(defvar *kl-spec-page-text-field-size* 20)

(define-page ("IRS Knowledge Level Publishers Page" :func-name irs-kl-publish
			                            :class :user 
                                                    :bgcolor 
                                                    web-onto::*default-page-colour*
                                                    ) 
    ()
  (with-field-value (realname email-name email-host new-ontology existing-ontology
                              number-of-ontologies-used ontology-type definitions-string 
                              documentation language)
    (cond ((and (form-value-p realname) 
                (form-value-p definitions-string))   
           (add-new-service-knowledge-level-spec
            realname (concatenate 'string email-name "@" email-host)
            (or (and (form-value-p new-ontology) new-ontology)
                existing-ontology) ontology-type
            number-of-ontologies-used definitions-string language documentation))
          (t (irs-publish-kl-form realname email-name email-host
                                  new-ontology existing-ontology
                                  number-of-ontologies-used 
                                  ontology-type definitions-string 
                                  language documentation)))))

(defun irs-publish-kl-form (realname email-name email-host
                                     new-ontology existing-ontology 
                                     number-of-ontologies-used 
                                     ontology-type definitions-string 
                                     language documentation)
  (if number-of-ontologies-used
      (setf number-of-ontologies-used (read-from-string number-of-ontologies-used))
    (setf number-of-ontologies-used *default-number-of-ontologies-used*))
  (html-out (center (header 1 "IRS Knowledge Specification Page")))
  (html-out (center 
             (anchor 
              "http://www.swi.psy.uva.nl/projects/ibrow/home.html"
              (format nil "~a"
                      (inline-image *irs-image-url*
                                    :remote t)))))
  (html-out (center (italic "<p>Welcome to the IRS Knowledge Specification Page")))
  (html-out (in-form (:action (url 'irs-kl-publish))
              (html-out "<hr>")
              (html-out "Please enter your contact details:<P>~%")
              (html-out
               (in-table (:border 0)
                 (html-out
                  (table-row 
                   (table-cell (bold "Your name:"))
                   (table-cell (bold (text-field "realname" :value realname)))))
                 (html-out
                  (table-row
                   (table-cell (bold "Your e-mail address:"))
                   (table-cell (text-field "email-name" :value email-name))
                   (table-cell (bold "@"))
                   (table-cell (text-field "email-host" :value email-host))))))
              (html-out "<hr>")
              (html-out 
               "Please enter the knowledge level specification of your service:<P>~%")
              (html-out (in-table (:border 0)
                          (html-out
                           (table-row
                            (table-cell (bold "Ontology:"))
                            (table-cell 
                             (menu-select "existing-ontology"
                                          (all-ontologies)
                                          :value existing-ontology))
                            (table-cell 
                             (text-field "new-ontology" :value new-ontology
                                         :size 
                                         *kl-spec-page-text-field-size*))))
                          (html-out
                           (table-row
                            (table-cell (bold "Ontology Type:"))
                            (table-cell "")
                            (table-cell 
                             (menu-select "ontology-type"
                                          *ontology-types*
                                          :value ontology-type))))
                          (html-out 
                           (table-row
                            (table-cell (bold "Ontology Uses:"))
                            (generate-cells number-of-ontologies-used 
                                            'ontology-uses-cell)))
                          (html-out 
                           (table-row
                            (table-cell (bold "Number of ontologies used:"))
                            (table-cell 
                             (text-field "number-of-ontologies-used" 
                                         :value number-of-ontologies-used
                                         :size 
                                         4))))
                          (html-out
                           (table-row
                            (table-cell (bold "Language:"))
                            (table-cell 
                             (menu-select "language"
                                          *irs-spec-languages*
                                          :value language))))))
              (html-out "<p>")
              (html-out (in-table (:border 0)
                          (html-out
                           (table-row
                            (table-cell (bold "Definition:"))
                            (table-cell  (text-area "definitions-string" 
                                                    *kl-definition-area-height*
                                                    *kl-definition-area-width*
                                                    :contents definitions-string))))
                          (html-out
                           (table-row
                            (table-cell (bold "Any Documentation:"))
                            (table-cell (text-area "Documentation" 10 30
                                                   :contents Documentation))))))
              (html-out "<HR>  ~a  ~a~%" (submit-button "Submit") 
                        (reset-button "Reset")))))

(defvar *ontology-uses-cell-control-string*
  "ontology-uses~d")

(defun generate-ontology-uses-cell-name (i)
  (format nil *input-role-name-cell-control-string* i))

(defun all-ontologies ()
  (mapcar #'(lambda (x) (string-downcase (symbol-name x)))
                         (ocml::sorted-ontologies)))

(defun ontology-uses-cell (i)
  (let* ((cell-name  (generate-ontology-uses-cell-name i))
         (cell-value (get-irs-cell-form-value cell-name)))
    (menu-select cell-name (all-ontologies)
                 :value cell-value)))

(defun find-definition-in-ontology (ontology definition)
  (webonto::get-source-file 
   (if (instance-definition-p definition)
       (append (subseq definition 0 3) (list ontology))
       (append (subseq definition 0 2) (list ontology)))))

(defun add-definitions-to-existing-ontology (ontology-string-name ontology-name
                                                                  definitions)
  (mapc #'(lambda (definition) 
            (add-definition-to-existing-ontology ontology-string-name ontology-name
                                                 definition))
        definitions))

(defun instance-definition-p (definition)
  (eq (car definition) 'ocml::def-instance))

(defun add-definition-to-existing-ontology (ontology-string-name ontology-name
                                                                 definition)
  (let ((exists-p (find-definition-in-ontology ontology-name definition)))
    (cond (exists-p
           (ocml::select-ontology ontology-name)
           (web-onto::internal-process-new-source 
            (car definition) (second definition) (format nil "~(~:w~)" definition)
            *standard-output*
            ontology-name
            (when (instance-definition-p definition)
              (third definition))))             
          (t (web-onto::process-new-definition
              (string-upcase ontology-string-name)
              (format nil "~(~:w~)" definition)
              *standard-output*)))))

(defun get-definitions (definitions-string)
  (let ((*package* (find-package "OCML")))
    (with-input-from-string (istream definitions-string)
      (do ((definition (read istream nil nil) (read istream nil nil))
           (definitions nil (cons definition definitions)))
          ((null definition) (reverse definitions))))))

(defun get-kl-irs-form-ontology-uses (number-of-ontologies-used-string)
  (let ((results nil))
    (dotimes (i (read-from-string number-of-ontologies-used-string))
      (push (get-irs-cell-form-value (generate-ontology-uses-cell-name i))
            results))
    results))

(defun add-new-service-knowledge-level-spec (realname email
                                                      ontology-string-name
                                                      ontology-type
                                                      number-of-ontologies-used-string 
                                                      definitions-string language
                                                      documentation)
  (declare (ignore email language documentation))
  (let* ((*package* (find-package "OCML"))
         (ontology-name (read-from-string (string-upcase ontology-string-name)))
         (definitions (get-definitions definitions-string))
         (ontology-type (read-from-string (string-upcase ontology-type)))
         (ontology-uses  (get-kl-irs-form-ontology-uses number-of-ontologies-used-string)))
    (unless (ocml::get-ontology ontology-name)
      (web-onto::define-new-ontology *standard-output*
                                     ontology-name ontology-type
                                     ontology-uses realname nil))
    (add-definitions-to-existing-ontology ontology-string-name ontology-name
                                          definitions)
    
    (html-out 
     "<p>~a your knowledge level specification has successfully been published in the ontology ~:(~a~)."
     realname ontology-string-name)))

