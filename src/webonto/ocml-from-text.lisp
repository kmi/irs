;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(defmethod generate-new-code ((ocml-class ocml-metaclass) variables values template)
  (eval `(let ,(mapcar #'(lambda (variable value)
                           `(,variable ',value)) variables values)
           ,template)))

(defun get-template-info (ocml-class operation)
  (let ((class-precedence-list (class-precedence-list ocml-class))
        (result nil))
    (dolist (class class-precedence-list result)
      (setf result (funcall operation class))
      (when result (return result)))))

(defun get-instance-template-variables (ocml-class)
  (get-template-info ocml-class 'instance-template-variables))

(defun get-subclass-template-variables (ocml-class)
  (get-template-info ocml-class 'subclass-template-variables))

(defun get-subclass-template (ocml-class)
  (get-template-info ocml-class 'subclass-template-variables))

(defun get-instance-template (ocml-class)
  (get-template-info ocml-class 'instance-template))

(defmethod generate-new-instance-code ((ocml-class ocml-metaclass) values)
  (generate-new-code ocml-class (get-instance-template-variables ocml-class)
                     (cons (intern (symbol-name (gensym (symbol-name (name ocml-class)))))
                           (cons (name ocml-class) values))
                     (get-instance-template ocml-class)))

(defmethod generate-subclass-code ((ocml-class ocml-metaclass) values)
  (generate-new-code ocml-class (get-subclass-template-variables ocml-class)
                     (cons (name ocml-class) values) (get-subclass-template ocml-class)))


(defmethod generate-new-instance ((ocml-class ocml-metaclass) values)
  (eval (generate-new-instance-code ocml-class values)))

(defmethod generate-subclass ((ocml-class ocml-metaclass) values)
  (eval (generate-subclass-code ocml-class values)))

(defun ocml-definition-from-text (stream request-string)
  (with-input-from-string (istream request-string)
    ;;;the ocml-definition-from-text bit
    (read istream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read istream))
           (template-type (read istream))
           (values (read istream))
           (matched-type (read istream))
           ;;need to change this for instances
           (selected-class-name (read istream))
           (string (insert-linefeeds (read istream)))
           (new-instance-info nil))
      (select-ontology ontology-name)
      (let* ((code (funcall (intern (format nil "GENERATE-~a-CODE" template-type)
                                       (find-package "OCML"))
                               ;;need to change this for instances
                               (get-domain-class matched-type)
                               values))
            (instance-name (second code))
            (instance-class-name (third code)))
        (push (list instance-name instance-class-name) new-instance-info)
        (if (ocml::find-current-instance instance-name instance-class-name)
            (web-onto::process-new-source
             (format nil "~a DEF-INSTANCE ~a ~a"
                     (symbol-name ontology-name)
                     instance-name instance-class-name)
             (format nil "~(~w~)" code)
             *standard-output*)               
             (web-onto::process-new-definition 
              (symbol-name ontology-name)
              (format nil "~(~w~)" code)
              *standard-output*))
      (http::princ-to-binary-stream
         (if new-instance-info
             (format nil "OK ~(~{~{~a ~a ~}~}~)~%" new-instance-info)
             (format nil "Couldn't identify any entities OK~%"))
         stream)))))

(defvar *odd-parsing-chars* 
  '(#\. #\tab #\return #\linefeed))

(defvar *parsing-trim-chars*
  '(#\space))

(defun clean-string-for-parsing (string)
  (dolist (char *odd-parsing-chars*)
    (setf string (remove char string)))
  string)

(defmethod parse-text-for-ocml (parse-type class-name string))
  
(defmethod parse-text-for-ocml ((parse-type (eql 'organization-ontology)) class-name string)
  (parse-text-for-ocml 'observatory-kb2 class-name string))

(defmethod parse-text-for-ocml ((parse-type (eql 'observatory-kb2)) class-name string)
  (do ((string (clean-string-for-parsing string)
               (string-trim *parsing-trim-chars*
                            (subseq string (1+ (position #\, string)))))
       (substrings nil (cons (string-trim *parsing-trim-chars*
                                          (subseq string 0 (position #\, string)))
                             substrings)))
      ((not (find #\, string))
       (create-ocml-types-and-value-lists
        class-name
        (cons (string-trim *parsing-trim-chars* 
                           (subseq string 0 (position #\, string)))
              substrings)))))

(defvar *all-ocml-entity-patterns* nil)

(defun create-ocml-types-and-value-lists (class-name strings)
  (let ((types nil) (value-lists nil))
    (mapc #'(lambda (string)
              (dolist (pattern (get-all-class-patterns class-name))
                (when (match-pattern pattern string)
                  (multiple-value-bind (type values)
                      (create-ocml-type-and-values pattern string)
                    (push type types)
                    (push values value-lists)))))
          strings)
    (values types value-lists)))

(defun get-all-class-patterns (class-name)
  (let ((all-subclasses (all-current-subclass-names class-name)))
    (mapcan #'(lambda (subclass)
                (let ((class-patterns
                       (class-patterns (get-domain-class subclass))))
                  (when class-patterns
                    (list (list subclass class-patterns)))))
            (cons class-name all-subclasses))))

(defun match-pattern (pattern string)
  (let ((pattern-string (second pattern)))
    (search pattern-string string)))

(defun create-ocml-type-and-values (pattern string)
  (values (car pattern)
          (list (create-ocml-value string))))

(defun create-ocml-value (string)
  (intern (substitute #\- #\space (string-upcase string))))
          

(defun reset-new-instance-template (ocml-class new-variables-string raw-template-string)
  (let ((*package* (find-package "OCML")))
    (setf (instance-template-variables ocml-class)
          (read-from-string new-variables-string)
          (instance-template ocml-class)
          (create-template-from-string raw-template-string))))

(defun reset-subclass-template (ocml-class new-variables-string raw-template-string)
  (let ((*package* (find-package "OCML")))
    (setf (subclass-template-variables ocml-class)
          (read-from-string new-variables-string)
          (subclass-template ocml-class)
          (create-template-from-string raw-template-string))))

(defmacro def-new-instance-template (class-name (&rest variables) template)
  `(let ((ocml-class (get-domain-class ',class-name)))
     (ocml-record-source-file ',class-name 'def-new-instance-template)
     (setf (instance-template-variables ocml-class)
           ',(cons 'generate-name 'class-name variables)
           (instance-template ocml-class)
           ',(create-template-from-list template))))
           


(defmacro def-subclass-template (class-name (&rest variables) template)
  `(let ((ocml-class (get-domain-class ',class-name)))
     (ocml-record-source-file ',class-name 'def-new-subclass-template)
     (setf (subclass-template-variables ocml-class)
           ',(cons 'class-name variables)
           (subclass-template ocml-class)
           ',(create-template-from-list template))))

(defmacro def-pattern (class-name matching-string &optional values-string)
  `(let ((ocml-class (get-domain-class ',class-name)))
     (ocml-record-source-file ',class-name 'def-pattern)
     (setf (class-patterns ocml-class) (list ,matching-string ,values-string))))

#+lispworks
(editor::setup-indent 'def-pattern 0 2)

#+lispworks
(editor::setup-indent 'def-new-instance-template 0 2)


#+lispworks
(editor::setup-indent 'def-new-subclass-template 0 2)


#+(or :lispworks4.3 :lispworks4.4)
(dspec:define-dspec-class def-pattern nil "def-pattern"
  :pretty-name "def-pattern"
  :Canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec)))
                      `(def-pattern ,name))))


#+(or :lispworks4.3 :lispworks4.4)
(dspec:define-dspec-class def-new-instance-template nil "def-new-instance-template"
  :pretty-name "def-new-instance-template"
  :Canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec)))
                      `(def-new-instance-template ,name))))


#+(or :lispworks4.3 :lispworks4.4)
(dspec:define-dspec-class def-new-subclass-template nil "def-new-subclass-template"
  :pretty-name "def-new-subclass-template"
  :Canonicalize #'(lambda (dspec)
		    (let ((name (cadr dspec)))
                      `(def-new-subclass-template ,name))))


(defun create-template-from-list (raw-template)
  (create-template-from-string (format nil "~a" raw-template)))
  
(defun create-template-from-string (raw-template-string)
  (read-from-string (concatenate 'string "`" (substitute #\, #\$ raw-template-string))))


(defun get-all-patterns (ontology-name)
  (let* ((*current-ontology* *current-ontology*)
        (*current-ontologies* *current-ontologies*)
        (*defined-relations* *defined-relations*)
        (*axioms* *axioms*)
        (*defined-functions* *defined-functions*)
        (*bc-rules* *bc-rules*)
        (*domain-classes* *domain-classes*)
        (ontology (get-ontology ontology-name))
        (current-ontologies (cons ontology (ontology-ancestors ontology)))
        (all-patterns nil))
    (declare (special *current-ontology* *current-ontologies*
                      *defined-relations* *axioms*
                      *defined-functions* *bc-rules*
                      *domain-classes*))
    (select-ontology ontology-name)
    (setf ontology 
          (maphash
           #'(lambda (key value)
               (when (and (find (home-ontology value) current-ontologies :test #'eq)
                          (class-patterns value))
                 (push (cons key (class-patterns value)) all-patterns)))
           *domain-classes*))
    all-patterns))

(defun ontology-pattern-javascript (ontology-name)
  (let ((function-names nil)
        (function-codes nil))
    (mapcar
     #'(lambda (pattern)
         (multiple-value-bind (function-name function-code)
             (generate-pattern-javascript pattern)
           (push function-name function-names)
           (push function-code function-codes)))
     (get-all-patterns ontology-name))
    (apply #'concatenate
           'string
           (format nil
                   "function find_pattern(text) {~%  ~a(text);~{ ~a(text);~}~%  return true;~%}~%~%"
                   (car function-names) (cdr function-names))
           function-codes)))

(defun generate-javascript-name (x)
  (string-downcase (substitute #\_ #\- (symbol-name x))))

(defun generate-javascript-find-function-name (x)
  (concatenate 'string "find_" (generate-javascript-name x)))

(defun generate-pattern-javascript (pattern)
  (let ((string-name (generate-javascript-find-function-name (car pattern))))
    (values
     string-name
     (format nil "function ~a(text) {~%  var i;~%~a~%  return true;~%}~%~%"
             string-name
             (generate-single-pattern-javascript (car pattern) (second pattern) (third pattern))))))

(defvar *pattern-split-char* "¬")

(defun generate-single-pattern-javascript (ocml-entity-name matching-pattern values-pattern)
  (if (and values-pattern (> (length values-pattern) 0))
      ;;need to write this one for multipve values for a template
      (format nil "~%  var regexp = /~a/g;~%  var splitexp = /~a/;~%  if (!(text.search(regexp) == -1)) {~%    var finds = text.match(regexp);~%    for (i = 0; i < finds.length; i++) {~%      var splitresults = finds[i].split(splitexp);~%      alert(\"Finds \"+finds[i]);~%      for (j = 0; j < splitresults.length; j++) {~%        alert(\"split \"+splitresults[j]);~%      }~%      webonto.annotate(text, splitresults.join(\"~a\"),\"~(~a~)\");~%      }~%  }"
              matching-pattern
              values-pattern
              *pattern-split-char*
              ocml-entity-name)
      ;;assume here that the whole match is a single value
      (format nil "~%  var regexp = /~a/g;~%  if (!(text.search(regexp) == -1)) {~%    var finds = text.match(regexp);~%    for (i = 0; i < finds.length; i++) {~%      alert(finds[i]);~%      webonto.annotate(text, finds[i],\"~(~a~)\");~%    }~%  }"
              matching-pattern
              ocml-entity-name)))



 