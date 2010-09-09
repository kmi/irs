(in-package http)

(defun generate-irs-publish-url-file-name (ontology psm)
  (merge-pathnames
   (format nil 
           "run-~(~a~)_~(~a~).lisp"
           ontology psm)
   (translate-logical-pathname "irs:src;irs-server-generated-code;")))

(defun generate-irs-publish-url-function-name (ontology psm)
  (intern (format nil "RUN-~A_~A" ontology psm)
          (find-package "CL-USER")))

(defun within-server-method-registration (ontology method-name 
                                                   input-roles
                                                   output
                                                   publisher-ip-address
                                                   publisher-port)
  (ip::add-method-to-ontology ontology method-name input-roles)
  (ip::create-post-url-for-psm-soap-definition method-name output)
  (visualiser:refresh))
                 
(defun irs-create-publish-url-code (host port path ontology psm 
                                         variables)
  (let ((*package* (find-package "OCML")))
    (setf ontology (read-from-string (string-upcase ontology))
          psm (read-from-string (string-upcase psm))
          port (read-from-string (string-upcase port)))
    (internal-irs-create-publish-url-code host port path ontology psm variables)))

(defun internal-irs-create-publish-url-code (host port path ontology psm 
                                                  string-variables)
  (let* ((*package* (find-package "CL-USER"))
         (variables
          (read-from-string
           (concatenate 'string 
                        "(" (string-upcase string-variables) ")")))
         (function-name 
          (generate-irs-publish-url-function-name ontology psm))
         (file-name
          (generate-irs-publish-url-file-name ontology psm))
         (code (generate-irs-publish-url-code
                function-name host port path 
                variables string-variables))
         (file-start (format nil "(in-package cl-user)~%~%")))
    (when (probe-file file-name)
      (delete-file file-name))
    (with-open-file (istream file-name
                             :direction :output
                             :if-does-not-exist :create)
      (format istream file-start)
      ;;don't downcase the code - because it may contain strings so we leave the case alone
      ;;e.g. urls for web applications
      (format istream "~:w~%~%"
              code))
    (load file-name)
    (cond ((ocml::get-ontology ontology)
           (ocml::with-ocml-thread-safety
             (ocml::select-ontology ontology)
             (multiple-value-bind (input-roles output)
                 (ip::internal-get-task-info ontology psm)
               (within-server-method-registration 
                ontology
                (intern (format nil "~a-~a"
                                ontology psm)
                        (find-package "CL-USER"))
                input-roles output
                host port)))))))
     
(defvar *problem-variable-names*
  '(t nil))

(defun clean-up-problem-variable-names (variables)
  (dolist (problem-variable-name *problem-variable-names*)
    (when (find problem-variable-name variables )
      (let ((new-name (intern (symbol-name (gensym (symbol-name problem-variable-name)))
                              (find-package "HTTP"))))
        (setf variables
              (substitute new-name problem-variable-name variables)))))
  variables)

(defvar *use-proxy* nil)

(defun use-proxy-p ()
  *use-proxy*)

(defun use-proxy ()
  (setf *use-proxy* t))

(defun dont-use-proxy ()
  (setf *use-proxy* nil))

(defun generate-list-from-space-separated-string (string &optional result)
  (setf string (string-right-trim '(#\space #\tab) string))
  (cond ((zerop (length string)) (reverse result))
        (t (multiple-value-bind (first-item rest-of-string)
               (split-string string #\space)
             (generate-list-from-space-separated-string 
              rest-of-string
              (push first-item result))))))

(defun split-string (string char)
  (if (find char string)
      (let ((pos (position char string)))
        (values (subseq string 0 pos)
                (string-left-trim (list char) (subseq string pos))))
    (values string "")))

(defvar *irs-get-url-timeout* 10000)

;;;need to change

;;("xml.amazon.com" 80 "/onca/xml3" COMMON-LISP-USER::AMAZON-SEARCH-PSM-ONTOLOGY COMMON-LISP-USER::SEARCH-BOOK-PSM "t dev-t AuthorSearch mode type page f")

(defun generate-irs-publish-url-code (function-name host port path
                                      variables string-variables)
  (let ((clean-variables (clean-up-problem-variable-names variables))
        (string-variables-list
         (generate-list-from-space-separated-string string-variables)))
    `(defun ,function-name ,clean-variables
       (let ((cl-user::result
              (utilities:http-request
               (format nil "http://~A:~A" ,host ,port
                       (format nil ,(format nil "~a?~{~a=~~a&~}~{~a=~~a~}" path
                                            (butlast string-variables-list)
                                            (last string-variables-list))
                               ,@clean-variables))
               :proxy (if irs:*proxy-host*
                          (format nil "http://~A:~A/" irs:*proxy-host* irs:*proxy-port*)))))
         cl-user::result))))

(define-page ("Publish URL Page" :func-name irs-publish-url
                                 :class :user 
                                 :bgcolor 
                                 web-onto::*default-page-colour*
                                 ) 
    ()
  (with-field-value (host port path ontology psm 
                          variables)
    (cond ((and (form-value-p host) 
                (form-value-p port)
                (form-value-p path) 
                (form-value-p variables)
                )   
           (with-output-to-string (html-stream)
             (irs-create-publish-url-code host port path ontology psm 
                                          variables))
           (html-out "The service at url ~a:~d~a has been published against the problem solving method ~a in ontology ~a" 
                     host port path psm ontology))
          (t (irs-publish-url-form host port path ontology psm 
                                   variables)))))

(defvar *irs-publish-url-form-text-cell-size* 
  30)

(defun irs-publish-url-form (host port path ontology psm 
                                  variables)
  (html-out (center (header 1 "IRS Publish URL Page")))
  (html-out (center 
             (anchor 
              "http://kmi.open.ac.uk/projects/irs/"
              (format nil "~a"
                      (inline-image *irs-image-url*
                                    :remote t)))))
  (html-out (center (italic "<p>Welcome to the IRS Publish URL Page")))
  (html-out (in-form (:action (url 'irs-publish-url))
              (html-out "<hr>")
              (html-out "Please enter the details of your service below:<P>~%")
              (html-out
               (in-table (:border 0)
                 (html-out
                  (table-row 
                   (table-cell (bold "Host:"))
                   (table-cell 
                    (bold (text-field 
                           "host" :value host
                           :size 
                           *irs-publish-url-form-text-cell-size*)))
                   
                   (table-cell (bold "Variables:"))
                   (table-cell 
                    (text-field "variables" :value variables
                                :size
                                *irs-publish-url-form-text-cell-size*))))
                 (html-out
                  (table-row
                   (table-cell (bold "Port:"))
                   (table-cell
                    (text-field "port" :value port
                                :size 
                                *irs-publish-url-form-text-cell-size*))
                   (table-cell (bold "Path"))
                   (table-cell 
                    (text-field 
                     "path" :value path
                     :size
                     *irs-publish-url-form-text-cell-size*))))))
              (html-out "<hr>")
              (html-out 
               "Please specify which Problem Solving Method your service implements:<P>~%")
              (html-out (in-table (:border 0)
                          (html-out
                           (table-row
                            (table-cell (bold "Ontology:"))
                            (table-cell 
                             (menu-select "ontology"
                                          (all-ontologies)
                                          :value ontology)))
                           (html-out (table-row
                            (table-cell (bold "Problem Solving Method:"))
                            (table-cell 
                             (text-field "psm" :value psm)))))))
                          
              (html-out "<HR>  ~a  ~a~%" (submit-button "Submit") 
                        (reset-button "Reset")))))