;;; Copyright © 2009 The Open University

(in-package #:irs-utilities)

(defvar cl-user::*current-pathname* nil)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defun base-char-p (object)
  #+:lispworks
  (lw:base-char-p object)
  #+:sbcl
  (sb-kernel:base-char-p object))

(defun compose (&rest functions)
  (lambda (x)
    (reduce #'funcall functions :from-end t :initial-value x)))

;;may have to get the class of the instance
(defun get-service-name (x)
  (string-capitalize (if (ocml::get-domain-class x)
			 (symbol-name x)
			 (symbol-name (ocml::instance-of-class x)))))

(defun make-irs-method-id (method-name ontology
                                       &optional (package-name "OCML"))
  (intern (concatenate 'string
                       (symbol-name ontology) "_" 
                       (symbol-name method-name))
          (find-package package-name)))



(defun cl-user::make-ocml-symbol (x)
  (if (and (atom x) (symbolp x))
      (intern (symbol-name x)
              (find-package "OCML"))
    x))

(defun cl-user::make-ocml-item (x)
  (read-in-sexpr-input (format nil "~s" x) t))

#|
(defun read-in-sexpr-input (value &optional return-instance-name-p)
  (let* ((*package* (find-package "OCML"))
         (current-value nil))
    (with-input-from-string (istream value)
      (do ((current-form (read istream nil nil) (read istream nil nil)))
          ((null current-form) current-value)
        (setf current-value (read-in-one-form current-form return-instance-name-p))))))
|#

(defun read-in-sexpr-input (Value &Optional return-instance-name-p)
  (let* ((*package* (find-package "OCML"))
         current-value
         (all-forms (read-from-string value)))
    (if (and (listp all-forms) (eq (car all-forms) 'ocml::instances))
        (dolist (form (cdr all-forms))
          (setf current-value (read-in-one-form form return-instance-name-p)))
      (setf current-value (read-in-one-form all-forms return-instance-name-p)))
    (if return-instance-name-p
        current-value
      all-forms)))

(defun read-in-one-form (form return-instance-name-p)
  (cond ((ocml-definition-p form)
         (let ((structure (eval form)))
           (if return-instance-name-p
               (ocml::name structure)
             form)))
        (t form)))

(defun ocml-definition-p (x)
  (and (listp x) (web-onto::ocml-definition-p x)))

(defun insert-html-char-replacement (replacement-html old-char string)
  (if (find old-char string)
      (let ((position (position old-char string)))
        (concatenate 'string (subseq string 0 position)
                     replacement-html
                     (insert-html-char-replacement
                      replacement-html old-char 
                      (subseq string (max (1+ position)) (length string)))))
    string))
 

(defvar *html-chars-and-replacements*
  `((#\" "&quot;") 
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")))
    

(defun insert-html-characters (string)
  (when (not (stringp string))
    (let ((out-val (make-string-output-stream)))
      (princ string out-val)
      (setf string (get-output-stream-string out-val))))
  (dolist (html-char-and-replacement *html-chars-and-replacements*)
    (setf string 
          (insert-html-char-replacement (second html-char-and-replacement) (car html-char-and-replacement)
                                        string)))
  string)

;; Carlos 16-10-2006
;; Added for handling types which do not have any predefined class
(defun home-ontology (class-structure)
  (if class-structure
      (ocml::home-ontology class-structure)
    (ocml::get-ontology 'ocml::top-level-ontology)))

(defun http-request (url &rest rest &key parameters &allow-other-keys)
  ;; DRAKMA does some funky nonsense with URL components.  We want to
  ;; fully construct URLs, and pass them to DRAKMA, without it
  ;; sticking its nose in.  To get around it, we decompose the URL
  ;; here, and pass it to DRAKMA the way it likes it: a base URL, and
  ;; an alist of query parameters and values.
  (labels ((url-components (url)
	     (cl-ppcre:split "[&?=]" url))
	   (pair-up (list)
	     (if (null list)
		 '()
		 (cons (cons (hunchentoot:url-decode (first list))
			     (hunchentoot:url-decode (second list)))
		       (pair-up (cddr list))))))
    (let ((bits (url-components url))
	  (proxy (proxy-for url)))
      (if (and parameters (rest bits))
	  (error "Query-containing URL supplied with other parameters.")
	  (apply #'drakma:http-request (first bits)
		 :proxy proxy
		 :parameters (or parameters
				 (pair-up (rest bits)))
		 rest)))))

(defun proxy-for (url)
  "Compute HTTP proxy for passing to Drakma.

If URL is a local address, do not use a proxy, even if one is
defined."
  (if irs:*proxy-host*
      (let ((puri (puri:uri url)))
	(if (member (puri:uri-host puri) '("localhost" "127.0.0.1")
		    :test 'string=)
	    nil
	    (list *proxy-host* *proxy-port*)))))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))
