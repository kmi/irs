(in-package ocml)

(defun superclasses-in-current-ontology (class)
  (remove-if #'(lambda (superclass)
                 (not (eq (home-ontology superclass) *current-ontology*)))
             (domain-superclasses class)))

(defun local-top-classes (ontology)
  (ocml:with-ontology (ontology)
    (let ((local-top-classes nil))
      (maphash
       #'(lambda (name class)
           (when (and (eq (home-ontology class) *current-ontology*)
                      (not (superclasses-in-current-ontology class)))
             (push name local-top-classes)))
       *domain-classes*)
      local-top-classes)))

(defun irs-html-dictionary-query-from-tree (stream upcase-string)
  (format stream "<html>~%<body>~%")
  (with-input-from-string (istream upcase-string)
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (type (read istream))
           (name (read istream))
           documentation)
      (if (eql type ':TASKS) 
          (select-ontology (find-ontology name (all-tasks)))
        (when (get-ontology name)
          (select-ontology name)))
      (cond
       ((get-ocml-class name) 
        ;;(format stream "~a~%" "end-of-documentation")
        (irs-html-describe-class stream name))
       ((get-ontology name)
        (if (member (get-ontology name) (sub-ontologies *current-ontology*)) (select-ontology name))
        (setf documentation (ocml-documentation (get-ontology name)))
        (when documentation
          (format stream "<i>~a</i><p>" documentation))
        ;;(format stream "~a~%" "end-of-documentation")
        (irs-html-describe-ontology stream name))
       (t (format stream "No definition of ~:(~A~) is found in current ontology ~:(~A~)~%" name 
                  (name *current-ontology*))))))
  (format stream "</body></html>~%"))

(defun irs-html-describe-class (stream name &optional local-defs-only?
                                       (lookup-function
                                        #'irs-lookup-current-word))
  (if local-defs-only?
      (irs-describe-class-local-info stream name)
    (irs-html-describe-class-info stream name
                                  lookup-function)))

;;chocolate = new Color((float)0.823528, (float)0.411764, (float)0.1176469),
;;MediumBrown = new Color((float)0.99, (float)0.95, (float)0.85),
;;#(:RGB 0.721567s0 0.52549s0 0.0431372s0) DarkGoldenrod
(defvar *html-ontology-colour*
  #+:irs-use-lispweb
  (http::color-string :red 0.721567 :green 0.52549 :blue 0.0431372))


;;DarkSlateGray = new Color((float)0.1843133, (float)0.309803, (float)0.309803),
;;DarkSlateGrey = new Color((float)0.1843133, (float)0.309803, (float)0.309803),
;;DimGray = new Color((float)0.411764, (float)0.411764, (float)0.411764),
;;DimGrey = new Color((float)0.411764, (float)0.411764, (float)0.411764),
;;SlateGray = new Color((float)0.4392157, (float)0.501961, (float)0.564705),
;;SlateGrey = new Color((float)0.4392157, (float)0.501961, (float)0.564705),
;;LightSlateGray = new Color((float)0.466666, (float)0.533333, (float)0.599998),
;;LightSlateGrey = new Color((float)0.466666, (float)0.533333, (float)0.599998),
;;chocolate = new Color((float)0.823528, (float)0.411764, (float)0.1176469),

(defvar *html-slot-option-colour* 
  ;;SlateGray (http::color-string :red 0.4392157 :green 0.501961 :blue 0.564705))
  #+:irs-use-lispweb
  (http::color-string :red 0.1843133 :green 0.309803 :blue 0.309803))

(defun add-html-class-colour (x)
  (http::font x :color *html-class-colour*))

(defun add-html-instance-colour (x)
  (http::font x :color *html-instance-colour*))

(defun add-html-ontology-colour (x)
  (http::font x :color *html-ontology-colour*))

(defun add-html-relation-colour (x)
  (http::font x :color *html-relation-colour*))

(defun add-html-function-colour (x)
  (http::font x :color *html-function-colour*))

(defun add-html-slot-option-colour (x)
  (http::font x :color *html-slot-option-colour*))

(defun insert-spaces (number)
  (let ((string ""))
    (dotimes (i number)
      (setf string (concatenate 'string string "&nbsp;")))
    string))

(defun irs-html-describe-class-info (stream 
                                     name
                                     &optional
                                     (lookup-function
                                      #'irs-lookup-current-word))
  ;;now use the one in mnm-server
  (html-describe-class stream name lookup-function))

#|
  (Let* ((class (get-ocml-class name))
         (supers (mapcar #'name (direct-domain-superclasses class)))
         (subs (mapcar #'name (current-direct-subclasses class)))
         documentation
         (ontology (name *current-ontology*)))
    (when class     
      (format stream
              (http::internal-insert-ocml-links
               (format nil
                       "<h2>~a ~:(~a~) (~:(~a~))</h2>" 
                       (get-class-title-type name)
                       (add-html-class-colour name)
                       (add-html-ontology-colour (name (home-ontology class))))
               lookup-function
               ontology))
      (setf documentation (ocml-documentation class))
      (when documentation
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "<i>~a</i><p>" documentation)
                 lookup-function
                 ontology)))
      (let ((publisher-location
             (cl-user::get-psm-publisher-location 
              name
              (name (home-ontology class)))))
        (when publisher-location
          (format stream "<b>Associated with web service:</b> ~a<p>" 
                  publisher-location)))
      (when supers
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "<b>~a:</b> ~:(~a~)~{, ~:(~a~)~}<br>"
                         (get-class-superclass-title-type name)
                         (add-html-class-colour (car supers))
                         (mapcar #'add-html-class-colour (cdr supers)))
                 lookup-function ontology)))
      (when subs
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "<b>~a:</b> ~:(~a~)~{, ~:(~a~)~}<br>"
                         (get-class-subclass-title-type name)
                         (add-html-class-colour (car subs))
                         (mapcar #'add-html-class-colour (cdr subs)))
                 lookup-function ontology)))
      (format stream "~A" (http::bold "Slots:"))
      (loop for slot in (domain-slots class)
            do
            (format stream 
                    (http::internal-insert-ocml-links
                     (format nil "<br>~a<b>~:(~a~)</b>" 
                             (insert-spaces 4) 
                             (add-html-relation-colour slot))
                     lookup-function ontology))
            (multiple-value-bind (values defaults)
                (get-slot-values-from-class-structure
                 class slot )
              (when values
                (format stream 
                        (http::internal-insert-ocml-links
                         (format nil "<br>~a<b><i>~:(~a~):</i></b>~{~a~}"
                                 (insert-spaces 5)
                                 (add-html-slot-option-colour :value)
                                 (mapcar #'(lambda (x) (format-ocml-value x 7))
                                         values))
                         lookup-function
                         ontology)))
              (when defaults
                (format stream
                        (http::internal-insert-ocml-links
                         (format nil "<br>~a<b><i>~:(~a~):</i></b>~{~a~}"
                                 (insert-spaces 5)
                                 (add-html-slot-option-colour :default-value)
                                 (mapcar #'(lambda (x) (format-ocml-value x 7))
                                         defaults))
                         lookup-function
                         ontology))))
            (let ((type-info (remove-duplicates
                              (find-option-value class slot :type))))
              (when type-info
                (format stream
                        (http::internal-insert-ocml-links
                         (format nil "<br>~a<b><i>~:(~a~):</i></b>~{~a~}"
                                 (insert-spaces 5)
                                 (add-html-slot-option-colour :type)
                                 (mapcar #'(lambda (x) (format-ocml-value x 7))
                                         type-info))
                         lookup-function
                         ontology))))
            (loop for option in '(:min-cardinality
                                  :max-cardinality
                                  :inheritance)
                  for value = (find-option-value class slot option)
                  when value
                  do
                  (format stream
                          (http::internal-insert-ocml-links
                           (format nil "<br>~a<b><i>~:(~a~):</i></b> ~:(~a~)"
                                   (insert-spaces 5)
                                   (add-html-slot-option-colour option) value)
                           lookup-function
                           ontology)))
            (let ((doc (find-slot-documentation class slot)))
              (when doc
                (format stream
                        (http::internal-insert-ocml-links
                         (format nil "<br>~a<b><i>~:(~a~):</i></b> <i>~a</i>"
                                 (insert-spaces 5) 
                                 (add-html-slot-option-colour :documentation) doc)
                         lookup-function
                         ontology)))))
      (let ((own-slots (own-slots class)))
        (when own-slots
          (format stream "<br>~A" (http::bold "Own Slots:"))
          (loop for slot-name-and-value in own-slots
                do
                (let ((slot (car slot-name-and-value))
                      (slot-value (second slot-name-and-value)))
                  (format stream "<br>~a<b>~:(~a~)</b>" 
                          (insert-spaces 4) (add-html-relation-colour slot))
                  (when slot-value
                    (format stream 
                            (http::internal-insert-ocml-links
                             (format nil "<br>~a<b><i>~:(~a~):</i></b>~a"
                                     (insert-spaces 5)
                                     (add-html-slot-option-colour :value)
                                     (format-ocml-value slot-value 7))
                             lookup-function
                             (name *current-ontology*))))))))
      (let ((instances 
             (mapcar 
              #'(lambda (x)
                  (add-html-instance-colour
                   (name x)))
              (get-limited-current-instances class))))
        (when instances
          (format stream "<br>~A" (http::bold "All Instances:"))
          (format stream 
                  (http::internal-insert-ocml-links
                   (format nil "~(~{~a ~}~)"
                           instances)
                   lookup-function
                   ontology)))))))
|#



(defun irs-lookup-current-word (current-word current-word-length ontology
                                              bold-home-ontology-p link-url)
  (declare (ignore link-url bold-home-ontology-p))
  (let* ((current-word-string (coerce (subseq current-word 0 current-word-length)
                                      'string))
         (reverse-current-word-string (reverse current-word-string))
         (word-to-lookup (intern (string-upcase current-word-string)
                                 (find-package "OCML"))))
    (multiple-value-bind (found-p home-ontology-p colour)
        (ocml::ocml-lookup word-to-lookup ontology)
      (declare (ignore home-ontology-p))
      (if found-p
          (reverse (http::font current-word-string :color colour))
          reverse-current-word-string))))

(defun irs-lookup-current-word-insert-link
       (current-word current-word-length ontology
                     bold-home-ontology-p link-url)
  (declare (ignore link-url bold-home-ontology-p))
  (let* ((current-word-string (coerce (subseq current-word 0 current-word-length)
                                      'string))
         (reverse-current-word-string (reverse current-word-string))
         (word-to-lookup (intern (string-upcase current-word-string)
                                 (find-package "OCML"))))
    (multiple-value-bind (found-p home-ontology-p colour)
        (ocml::ocml-lookup word-to-lookup ontology)
      (declare (ignore home-ontology-p))
      (if found-p
          (reverse 
           (http::anchor 
            (format nil "/ocml-describe?name=~a&ontology=~a"
                         word-to-lookup ontology)
            (http::font current-word-string 
                        :color colour)))
          reverse-current-word-string))))

(defun irs-html-describe-instance (stream name &key class-name 
                                          deduce-all-values?
                                          (lookup-function 
                                           #'irs-lookup-current-word))
   "If deduce-all-values? is T then we use all inference methods
    at our disposal to find slot values"
     (Let ((inst (find-current-instance name class-name))
           (unbound-slots)
            (parent))
       (when inst
         (setf parent (parent-class inst))
         (format stream "<h2>Instance ~:(~a~) of class ~:(~a~) (~:(~a~)) </h2>"
                 (add-html-instance-colour name)
                 (add-html-class-colour (name parent))
                 (add-html-ontology-colour 
                                     (name (home-ontology parent))))
         (loop for slot in (domain-slots parent)
               for values = (if deduce-all-values?
                              (setofall '?x `(,slot ,name ?x))
                              (get-slot-values inst slot))
               ;;;;;when values
               do
               (cond (values  
                      (format stream "<br><b>~:(~a~):</b>" 
                              (add-html-relation-colour slot))
                      (format stream
                        (http::internal-insert-ocml-links
                      (format nil ;;"<pre>~% ~:(~:w~)</pre>~{<pre>~% ~:(~:w~)</pre>~}<br>"
                              "~{~a~}" (mapcar #'(lambda (x) (format-ocml-value x 1))
                                               values))
                      lookup-function
                      (name *current-ontology*))))
                     (t (push slot unbound-slots)))
               finally
               (when unbound-slots
                 (format stream
                         (http::internal-insert-ocml-links
                          (format 
                           nil 
                           "<br>The following slots have no value: ~:(~a~)~{, ~:(~a~)~}~%"
                           (car unbound-slots)
                           (cdr unbound-slots))
                          lookup-function
                          (name *current-ontology*)))))
         (values))))


(defun irs-html-describe-relation (stream name
                                          &optional
                                     (lookup-function
                                      #'irs-lookup-current-word))
  (Let* ((rel (get-relation name)))
    (if rel
        (irs-html-describe-relation-internal stream name rel
                                             lookup-function)
      (format stream "~:(~a~) is not a relation." name))))

(defun irs-html-describe-relation-internal (stream 
                                            name rel
                                            &optional
                                            (lookup-function
                                             #'irs-lookup-current-word))
  (format stream "<h2>Relation ~:(~a~) (~:(~a~)) </h2>" 
          (add-html-relation-colour name) 
          (add-html-ontology-colour (name (home-ontology rel))))
  (format stream "<b>Schema:</b> ~:(~a~)" (schema rel))
  ;;(format stream "<br><b>Ontology:</b> ~:(~a~)" 
  ;;      (add-html-ontology-colour (name (home-ontology rel))))
  (when (ocml-documentation rel)
    (format stream "<br><b>Documentation:</b> <i>~:(~a~)</i>" 
            (http::internal-insert-ocml-links
             (ocml-documentation rel)
             lookup-function
             (name *current-ontology*))))

  (when (local-slot-of rel)
    (format stream "<br><b>Local slot of:</b> ")
    (format stream 
            (http::internal-insert-ocml-links
             (format nil 
                     "~:(~a~)~{, ~:(~a~)~}"
                     (name (car (local-slot-of rel)))
                     (mapcar #'name  (cdr (local-slot-of rel))))
             lookup-function
             (name *current-ontology*))))
  (when (slot-of rel)
    (Let ((non-local-slots (set-difference (slot-of rel) (local-slot-of rel))))
      (when non-local-slots
        (format stream "<br><b>Also slot of:</b> ")
        (format stream
                (http::internal-insert-ocml-links
                 (format nil
                         "~:(~a~)~{, ~:(~a~)~}"
                         (name (car non-local-slots))
                         (mapcar #'name (cdr non-local-slots)))
                 lookup-function
                 (name *current-ontology*))))))
  (when (constraint rel)
    (format stream "<br><b>Constraints:</b> ")
    (format stream 
            (http::internal-insert-ocml-links
             (format-ocml-value (constraint rel) 2)
             lookup-function
             (name *current-ontology*))))
  (when (iff-def rel)
    (format stream "<br><b>Iff-def:</b> ")
    (http::internal-insert-ocml-links
     (format-ocml-value (iff-def rel) 1)
     ;;(car (bc-clause-antecedents (iff-def rel)))
     )
    lookup-function
    (name *current-ontology*))
  (when (sufficient rel)
    (format stream "<br><b>Sufficient:</b> ")
    (format stream
            (http::internal-insert-ocml-links
             (format-ocml-value (car (bc-clause-antecedents (sufficient rel))) 2)
             lookup-function
             (name *current-ontology*))))
  (when (defined-by-rule rel)
    (format stream "<br><b>Associated rules:</b> ")
    (format stream
            (http::internal-insert-ocml-links
             (format nil
                     "~:(~a~)~{, ~:(~a~)~}"
                     (name (car (defined-by-rule rel)))
                     (mapcar #'name
                             (cdr (defined-by-rule rel))))
             lookup-function
             (name *current-ontology*))))
  (when (prove-by rel)
    (format stream "<br><b>Prove by:</b> ")
    (format stream 
            (http::internal-insert-ocml-links
             (format-ocml-value (car (bc-clause-antecedents
                                      (prove-by rel))) 2)
             lookup-function
             (name *current-ontology*))))
  (when (lisp-fun rel)
    (format stream "<br><b>Prove by:</b> ")
    (format stream
            (http::internal-insert-ocml-links
             (format-ocml-value (lisp-fun rel) 2)
             lookup-function
             (name *current-ontology*))))
  (when (relation-instances rel)
    (format stream "<br><b>Relation Instances:</b> ")
    (format stream 
            (http::internal-insert-ocml-links
             (format nil
                     "~:(~a~)~{, ~:(~a~)~}"
                     (args (car (relation-instances rel)))
                     (mapcar #'args (cdr (relation-instances rel))))
             lookup-function
             (name *current-ontology*)))))



(defun irs-html-describe-function (stream name
                                          &optional
                                          (lookup-function
                                           #'irs-lookup-current-word))
  (Let* ((f (get-function name)))
    (if f
        (irs-html-describe-function-internal stream name f
                                             lookup-function)
      (format stream "~:(~a~) is not a function." name))))

(defun irs-html-describe-function-internal (stream 
                                            name f
                                            &optional
                                            (lookup-function
                                             #'irs-lookup-current-word))
  (format stream "<h2>Function ~:(~a~) (~:(~a~))</h2>"  
          (add-html-function-colour name) 
          (add-html-ontology-colour (name (home-ontology f))))
  (format stream "<b>Schema</b>: ~:(~a~)" (schema f))
  (when (ocml-documentation f)
    (format stream "<br><b>Documentation:</b> <i>~a</i>" 
            (http::internal-insert-ocml-links
             (ocml-documentation f)
             lookup-function
             (name *current-ontology*))))
  (when (constraint f)
    (format stream "<br><b>Constraints:</b> ")
    (format stream
            (http::internal-insert-ocml-links
             (format-ocml-value (constraint f) 2)
             lookup-function
             (name *current-ontology*))))
  (when (definition f)
    (format stream "<br><b>Def:</b> ")
    (format stream
            (http::internal-insert-ocml-links
             (format-ocml-value (definition f) 2)
             lookup-function
             (name *current-ontology*))))
  (when (body f)
    (format stream "<br><b>Body:</b> ")
    (format stream 
            (http::internal-insert-ocml-links
             (format-ocml-value (body f) 2)
             lookup-function
             (name *current-ontology*))))
  (when (lisp-fun f)
    (format stream "<br><b>Lisp attachment:</b> ")
    (http::internal-insert-ocml-links 
     (format-ocml-value (lisp-fun f) 2)
     lookup-function
     (name *current-ontology*))))


(defun irs-html-describe-ontology (stream name
                                          &optional
                                          (lookup-function
                                           #'irs-lookup-current-word))
  "If deduce-all-values? is T then we use all inference methods
    at our disposal to find slot values"
  (Let ((ontology (get-ontology name)) 
        (local-top-classes nil))
    (when ontology 
      (setf local-top-classes (local-top-classes name))
      (when (slot-value ontology 'NAME) 
        (format stream "<h2>Ontology ~:(~a~)</h2>" 
                (add-html-ontology-colour (slot-value ontology 'NAME))))
      (when (slot-value ontology 'includes) 
        (format stream "<b>Includes:</b> ")
        (format stream 
                (http::internal-insert-ocml-links
                 (format nil "~{~:(~a ~)~}~%" 
                         (mapcar #'name (slot-value ontology 'includes)))
                 lookup-function
                 (name *current-ontology*))))
      (when (slot-value ontology 'INCLUDED-BY)
        (format stream "<br><b>Included By:</b> ")
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "~{~:(~a ~)~}~%" 
                         (mapcar #'name (slot-value ontology 'INCLUDED-BY)))
                 lookup-function
                 (name *current-ontology*))))
      (when (slot-value ontology 'ONTOLOGY-TYPE)
        (format stream "<br><b>Ontology Type:</b> ")
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "~:(~a~)" (slot-value ontology 'ONTOLOGY-TYPE))
                 lookup-function
                 (name *current-ontology*))))
      (when (slot-value ontology 'AUTHOR)
        (format stream "<br><b>Author:</b> ")
        (format stream 
                (http::internal-insert-ocml-links
                 (format nil "~:(~a~)"
                         (read-from-string (slot-value ontology 'AUTHOR)))
                 lookup-function
                 (name *current-ontology*))))
      (when (slot-value ontology 'ALLOWED-EDITORS)
        (format stream "<br><b>Allowed Editors:</b> ")
        (format stream
                (http::internal-insert-ocml-links
                 (format nil "~{~:(~a ~)~}" 
                         (mapcar #'read-from-string
                                 (slot-value ontology 'ALLOWED-EDITORS)))
                 lookup-function
                 (name *current-ontology*))))
         ;(format stream "~%PATHNAME: ~:(~a~)~%" (slot-value ontology 'PATHNAME))
      (when (slot-value ontology 'ONTOLOGY-FILES)
        (format stream "<br><b>Ontology Files:</b> ")
        (format stream 
                (http::internal-insert-ocml-links
                 (format nil "~{~:(~a ~)~}" 
                         (mapcar #'read-from-string
                                 (slot-value ontology 'ONTOLOGY-FILES)))
                 lookup-function
                 (name *current-ontology*))))
      (when (slot-value ontology 'DOCUMENTATION)
        (format stream "<br><b>Documentation:</b> <i>~:(~a~)</i>" 
                (http::internal-insert-ocml-links
                 (ocml-documentation ontology)
                 lookup-function
                 (name *current-ontology*))))
      (when local-top-classes
        (format stream "<br><b>Top Classes:</b> ~a" 
                (http::internal-insert-ocml-links
                 (format nil "~:(~{~a ~}~)" local-top-classes)
                 lookup-function
                 (name *current-ontology*)))))))


