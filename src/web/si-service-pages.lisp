(in-package http)

(defvar *input-role-text-area-size* 20)

(defvar *corba-types* '("float" "long" "int" "string"))

(defvar *irs-languages* '("Lisp" "Java"))

(defvar *service-page-text-field-size* 40)

(defvar *default-number-of-input-roles* 5)

(defvar *irs-image-url* "http://137.108.24.105/IRS/images/irs.gif")

(defun form-value-p (x)
  (and x (not (zerop (length 
                      (string-trim '(#\tab #\space #\linefeed #\return #\formfeed) x))))))

#|
(defun get-host-ip (&optional (http-info *http-info*))
  (dolist (x (http::http-info-headers http-info)) 
    (when (eq (header-field-name x) :host)
      (let ((string (car (header-field-value x))))
        (return 
         (if (find #\: string)
             (subseq string 0 (position #\: string))
           string))))))
|#

(defvar *host-name-translations*
  '(("pckm070.open.ac.uk" . "137.108.24.105")
    ("localhost" . "137.108.24.105")
    ("pckm002.open.ac.uk" . "137.108.24.129")))

(defun translate-host-name (host-name)
  (setf host-name (string-downcase host-name))
  (or (cdr (assoc host-name *host-name-translations* :test #'string=))
      host-name))

(defun generate-cells (number-of-cells cell-function)
  (let ((cells ""))
    (dotimes (i number-of-cells)
      (setf 
       cells 
       (concatenate 
        'string
        cells
        (table-cell (funcall cell-function i)))))
    cells))

(defvar *input-role-name-cell-control-string* "input-role~d")

(defvar *input-role-type-cell-control-string* "input-role-type~d")

(defvar *input-role-translator-cell-control-string* "input-role-translator~d")

(defun get-irs-cell-form-value (cell-name &optional read-value-p)
  (let ((value (form-value (intern (string-upcase cell-name) :keyword))))
    (if read-value-p
        (when (form-value-p value)
          (read-from-string (string-upcase value)))
      value)))

(defun generate-input-role-cell-name (i)
  (format nil *input-role-name-cell-control-string* i))

(defun generate-input-role-type-cell-name (i)
  (format nil *input-role-type-cell-control-string* i))

(defun generate-input-role-translator-cell-name (i)
  (format nil *input-role-translator-cell-control-string* i))

(defun get-irs-form-input-roles (number-of-input-roles-string)
  (let ((results nil))
    (dotimes (i (read-from-string number-of-input-roles-string))
      (let ((input-role-name 
             (get-irs-cell-form-value 
              (generate-input-role-cell-name i) t))
            (input-role-translator
             (get-irs-cell-form-value 
              (generate-input-role-translator-cell-name i) t))
            (input-role-type
             (get-irs-cell-form-value 
              (generate-input-role-type-cell-name i))))
        (push (append (list input-role-name) 
                      (list input-role-type)
                      (when input-role-translator
                        (list input-role-translator)))
              results)))
    (reverse results)))
      
  

(defun input-role-text-field-cell  (cell-name)
  (let ((cell-value (get-irs-cell-form-value cell-name)))
    (text-field cell-name
                :size *input-role-text-area-size*
                :value cell-value)))
  
(defun input-role-cell (i)
  (input-role-text-field-cell (generate-input-role-cell-name i)))

(defun input-role-type-cell (i)
  (let* ((cell-name  (generate-input-role-type-cell-name i))
         (cell-value (get-irs-cell-form-value cell-name)))
    (menu-select cell-name
                 *corba-types*
                 :value cell-value)))

(defun input-role-translator-cell  (i)
  (input-role-text-field-cell (generate-input-role-translator-cell-name i)))

(defun irs-publish-form (realname email-name email-host ontology method-name  
                                  number-of-input-roles output-type
                                  function language ip-address ior-string
                                  documentation)
  (if number-of-input-roles
      (setf number-of-input-roles (read-from-string number-of-input-roles))
    (setf number-of-input-roles *default-number-of-input-roles*))
  (html-out (center (header 1 "IRS Publishers Page")))
  (html-out (center 
             (anchor 
              "http://www.swi.psy.uva.nl/projects/ibrow/home.html"
              (format nil "~a"
                      (inline-image *irs-image-url*
                                    :remote t)))))
  (html-out (center (italic "<p>Welcome to the IRS Publisher's Page.</p>")))
  (html-out (in-form (:action (url 'irs-publish))
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
              (html-out "Please enter the details on your service:<P>~%")
              (html-out (in-table (:border 0)
                          (html-out
                           (table-row
                            (table-cell (bold "Ontology:"))
                            (table-cell (text-field "ontology" :value ontology
                                                    :size 
                                                    *service-page-text-field-size*))))
                          (html-out 
                           (table-row
                            (table-cell (bold "Method name:"))
                            (table-cell (text-field "method-name" :value method-name
                                                    :size 
                                                    *service-page-text-field-size*))))
                          #|(html-out 
                           (table-row
                            (table-cell (bold "Function:"))
                            (table-cell (text-field "function" 
                                                    :value function
                                                    :size 
                                                    *service-page-text-field-size*))))|#))
              (html-out "<p>")
              (html-out (in-table (:border 0)
                          (html-out 
                           (table-row
                            (table-cell (bold "Input Role Names:"))
                            (generate-cells number-of-input-roles 'input-role-cell)))
                          (html-out 
                           (table-row
                            (table-cell (bold "Input Role Types:"))
                            (generate-cells number-of-input-roles 'input-role-type-cell)))
                          (html-out 
                           (table-row
                            (table-cell (bold "Input Role Translators:"))
                             (generate-cells number-of-input-roles 
                                             'input-role-translator-cell)))
                          (html-out
                           (table-row 
                            (table-cell (bold "Number of Input Roles:"))
                            (table-cell (text-field "number-of-input-roles"
                                                    :value number-of-input-roles
                                                    :size *input-role-text-area-size*))))
                          (html-out "<p>")
                          (html-out 
                           (table-row
                            (table-cell (bold "Output Type:"))
                            (table-cell (menu-select "output-type" *corba-types*
                                                     :value output-type))))))
              (html-out
               "<hr><p>Finally, please enter some implementation specific data.<p>")
              (html-out (in-table (:border 0)
                          
                          (html-out 
                           (table-row
                            (table-cell (bold "Language:"))
                            (table-cell (menu-select "language" 
                                                     *irs-languages*
                                                    :value language))))
                         #| (html-out 
                           (table-row
                            (table-cell (bold "IP Address:"))
                            (table-cell (text-field "ip-address" :value ip-address
                                                    :size 
                                                    *service-page-text-field-size*))))|#

                          (html-out
                           (table-row
                            (table-cell (bold "IOR string:"))
                            (table-cell  (text-area "ior-string" 10 30
                                                    :contents ior-string))))
                          (html-out
                           (table-row
                            (table-cell (bold "Any Documentation:"))
                            (table-cell (text-area "Documentation" 10 30
                                                   :contents Documentation))))))
              (html-out "<HR>  ~a  ~a~%" (submit-button "Submit") 
                        (reset-button "Reset")))))

#|
(defun dummy (ontology method-name input-roles output-type function 
                       publisher-ip-address ior-string documentation email)
  (setf a (list ontology method-name
                input-roles output-type function 
                publisher-ip-address ior-string documentation email)))
|#

(defun clean-up-ior-string (ior-string)
  (remove #\space 
          (remove #\tab 
                  (remove #\return 
                          (remove #\linefeed
                                  (remove #\formfeed ior-string ))))))

(defmacro with-ocml-item-name-and-ontology ((info name ontology) &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,name (cl-user::get-decoded-form-value ,info :name))
          (,ontology (cl-user::get-decoded-form-value ,info :ontology)))
    ,@body))

#+lispworks
(editor::setup-indent 'with-ocml-item-name-and-ontology 0 2)

(define-page ("Describe OCML Object Page" :func-name ocml-describe
			                  :class :user :bgcolor 
                                          web-onto::*default-page-colour*
                                          ) 
    (&rest info)
  (when info
    (setf info (car info)))
  (with-ocml-item-name-and-ontology (info name ontology)
    (if (ocml::get-ontology ontology)
    (ocml::with-ocml-thread-safety ()
      (ocml::select-ontology ontology)
      (let ((result
             (with-output-to-string (stream)
               (cond ((ocml::get-ocml-class name)
                      (ocml::irs-html-describe-class 
                       stream name
                       nil 
                       #'ocml::irs-lookup-current-word-insert-link))
                     ((ocml::get-ontology name)
                      (ocml::irs-html-describe-ontology 
                       stream name
                       #'ocml::irs-lookup-current-word-insert-link))
                     ((ocml::find-current-instance name) 
                      (ocml::irs-html-describe-instance
                       stream name
                       :lookup-function 
                       #'ocml::irs-lookup-current-word-insert-link))
                     ((ocml::get-relation name) 
                      (ocml::irs-html-describe-relation 
                       stream name
                       #'ocml::irs-lookup-current-word-insert-link))
                     ((ocml::get-function name) 
                      (ocml::irs-html-describe-function 
                       stream name
                       #'ocml::irs-lookup-current-word-insert-link))
                     (t (format stream 
                                "Sorry couldn't find ~(~a~) in ontology ~(~a~)"
                                name ontology))))))
        (html-out result)))
      (html-out "Sorry, the ontology ~(~a~) is not defined" ontology))))