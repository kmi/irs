;;; Mode: Lisp; Package: http

;;; Author: John Domingue

;;; The Open University

(in-package "HTTP")

(defvar *planet-onto-page-colour*
  #+:irs-use-lispweb
  (http::color-string :red 255 :blue 255 :green 255))

(defvar *last-input-values* nil)

(defvar *last-original-news-items-displayed* nil)

(defvar *last-augmented-news-items-displayed* nil)

(defvar *last-ocml-expression* "")

(defvar *evaluate-ocml-expression* nil )

(defvar *last-ocml-ask-expression* "")

(defvar *last-ocml-evaluation-result*)

(defvar *last-ocml-ask-result*)

(defvar *last-search-string* "")

(defvar *last-case-sensitive-value* nil )

(defstruct (planet-onto-input (:type list))
  storytypes people technology project)

(define-page ("Planet Onto Page" :func-name planet-onto
			      :class :user :bgcolor *planet-onto-page-colour*
			      ) 
    ()
  (html-out (header 1 "Planet Onto"))
  (html-out "Filter KMi Planet stories by selecting from the menus below")
  (with-field-value (storytypes people technology project)
    (html-out (in-form (:action (url 'planet-onto-internal))
	        (html-out "Show me KMi Planet stories about ~a involving ~a using ~a in the context of ~a<P>~%"
                          (menu-select "storytypes" (cons "AnyStoryType" (ocml::kmi-planet-story-types)))
                                       
                          (menu-select "people" (cons "AnyPerson" (ocml::kmi-people)))
                          (menu-select "technology" (cons "AnyTechnology" (ocml::kmi-technologies)))
                          (menu-select "project" (cons "AnyProject" (ocml::kmi-projects))))
                (html-out "~a<HR><p>"
                          (submit-button "Find Matching Stories"))))))


(define-page ("Planet Onto Page" :func-name planet-onto-internal
			         :class :user :bgcolor *planet-onto-page-colour*
			         ) 
    ()
  (html-out (header 1 "Planet Onto"))
  (html-out "Filter KMi Planet stories by selecting from the menus below")
  (with-field-value (storytypes people technology project)
    (unless (or storytypes people technology project)
      (setf storytypes (planet-onto-input-storytypes *last-input-values*)
	    people (planet-onto-input-people *last-input-values*)
	    technology (planet-onto-input-technology *last-input-values*)
	    project (planet-onto-input-project *last-input-values*)))    
    (html-out (in-form (:action (url 'planet-onto-internal))
		(html-out "Show me KMi Planet stories about ~a involving ~a using ~a in the context of ~a<P>~%"
			  (menu-select "storytypes" (cons "AnyStoryType"
							  (ocml::kmi-planet-story-types))
				       :value storytypes)
			  (menu-select "people" (cons "AnyPerson" (ocml::kmi-people))
				       :value people)
			  (menu-select "technology" (cons "AnyTechnology" (ocml::kmi-technologies))
				       :value technology)
			  (menu-select "project" (cons "AnyProject" (ocml::kmi-projects))
				       :value project))
		(html-out "~a<HR><p>"
			  (submit-button "Find Matching Stories"))
		(Find-Matching-Stories storytypes people technology project)))))


(define-page ("Onto Eval Page" :func-name planet-onto-ocml-eval
			      :class :user :bgcolor *planet-onto-page-colour*
			      ) 
    ()
  (html-out (header 1 "Planet Onto OCML Interface"))
  (html-out "Evaluate OCML Expressions")
  (with-field-value (ocml-expression)
    (html-out (in-form (:action (url 'planet-onto-ocml-eval-internal))
                (html-out "~a" (text-field "ocml-expression" :size 100))
                (html-out "<p>~a <HR><p>"
                          (submit-button "Evaluate Expression"))))))

(define-page ("Onto Eval Page" :func-name planet-onto-ocml-eval-internal
			      :class :user :bgcolor *planet-onto-page-colour*
			      ) 
    ()
  (html-out (header 1 "Planet Onto OCML Interface"))
  (html-out "Evaluate OCML Expressions")
  (with-field-value (ocml-expression)
    (html-out (in-form (:action (url 'planet-onto-ocml-eval-internal))
                (html-out "~a" (text-field "ocml-expression" :size 100 :value ocml-expression))
                (html-out "<p>~a <HR><p>"
                          (submit-button "Evaluate Expression"))
                (evaluate-ocml-expression ocml-expression)
                (setf *last-ocml-expression* ocml-expression)))))

(define-page ("Onto Ask Page" :func-name planet-onto-ocml-ask
			      :class :user :bgcolor *planet-onto-page-colour*
			      ) 
    ()
  (html-out (header 1 "Planet Onto OCML Ask Interface"))
  (html-out "Ask OCML Expressions")
  (with-field-value (ocml-expression)
    (html-out (in-form (:action (url 'planet-onto-ocml-ask-internal))
                (html-out "~a" (text-field "ocml-expression" :size 100))
                (html-out "<p>~a <HR><p>"
                          (submit-button "OCML Ask"))))))

(define-page ("Onto Ask Page" :func-name planet-onto-ocml-ask-internal
			      :class :user :bgcolor *planet-onto-page-colour*
			      ) 
    ()
  (html-out (header 1 "Planet Onto OCML Ask Interface"))
  (html-out "Ask OCML Expressions")
  (with-field-value (ocml-expression)
    (html-out (in-form (:action (url 'planet-onto-ocml-ask-internal))
                (html-out "~a" (text-field "ocml-expression" :size 100 :value ocml-expression))
                (html-out "<p>~a <HR><p>"
                          (submit-button "OCML Ask"))
                (ocml-ask ocml-expression)
                (setf *last-ocml-ask-expression* ocml-expression)))))

(defun evaluate-ocml-expression (ocml-expression)
  (cond ((string= ocml-expression "")
         (html-out (header 2 "No Input in expression window")))
        ((string= ocml-expression *last-ocml-expression*)
         (html-out (header 2 "Output from Evaluation"))
         (html-out *last-ocml-evaluation-result*))
        (t (html-out (header 2 "Output from Evaluation"))
	   (web-onto::select-planet-kb-ontology)
           (html-out (setf *last-ocml-evaluation-result*
                           (internal-insert-ocml-links (ocml::html-ocml-eval ocml-expression)
                                                       #'ocml-lookup-current-word))))))

(defun ocml-ask (ocml-expression)
  (cond ((string= ocml-expression "")
         (html-out (header 2 "No Input in expression window")))
        (t (html-out (header 2 "Output from Ask"))
           (cond ((string= ocml-expression *last-ocml-ask-expression*)
		  (html-out *last-ocml-ask-result*))
		 (t (web-onto::select-planet-kb-ontology)
		    (html-out (setf *last-ocml-ask-result*
				    (internal-insert-ocml-links
                                     (ocml::html-ocml-ask ocml-expression)
                                     #'ocml-lookup-current-word))))))))

(defun Find-Matching-Stories (storytypes people technology project)
  (cond ((equal *last-input-values*
		(list storytypes people technology project))
	 (display-augmented-news-items *last-original-news-items-displayed*
				       *last-augmented-news-items-displayed*))
	(t (web-onto::select-planet-kb-ontology)
	   (setf *last-input-values* (list storytypes people technology project))
           (let* ((stories (ocml::stories-related-to
			    (intern (string-upcase storytypes) (find-package "OCML"))
			    (intern (string-upcase people) (find-package "OCML"))
			    (intern (string-upcase technology) (find-package "OCML"))
			    (intern (string-upcase project) (find-package "OCML"))))
		  original-news-items augmented-news-items)
	     (cond (stories                           
		    (setf original-news-items
			  (mapcar #'(lambda (story) (ocml::news-items-from-story-name story))
				  stories)
			  augmented-news-items
			  (mapcar #'insert-ocml-links original-news-items)
			  *last-original-news-items-displayed* original-news-items
			  *last-augmented-news-items-displayed* augmented-news-items)
		    (display-augmented-news-items original-news-items augmented-news-items))
		   (t (html-out
		       (header 2 "Sorry No KMi Planet stories match your request"))))))))

(defun insert-contents-page (original-news-items)
  (html-out (dotted-list 
	     (mapcar #'(lambda (news-item)
			 (anchor (format nil "#~d" (cl-user::news-item-number news-item))
				 (cl-user::news-item-headline news-item)))
		     original-news-items))))

(defun display-augmented-news-items (original-news-items augmented-news-items)
  (html-out (header 2 "KMi Planet stories which match your request"))
  (insert-contents-page original-news-items)
  (html-out "<hr>")
  (mapc #'(lambda (original-news-item augmented-news-item)
	    (cl-user::write-news-item augmented-news-item
				      (cl-user::news-item-number original-news-item) t)
	      (html-out "<hr>"))
        original-news-items augmented-news-items))

(defmacro with-ocml-name-and-ontology ((info ocml-name ocml-ontology) &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,ocml-name (cl-user::get-decoded-form-value ,info :ocml-name))
          (,ocml-ontology (cl-user::get-decoded-form-value ,info :ocml-ontology)))
    ,@body))

#+lispworks
(editor::setup-indent 'with-ocml-name-and-ontology 0 2)

(defun back-to-planet-onto-page ()
  (html-out (anchor "/planet-onto-internal" "Back to Planet Onto")))

(defun back-to-planet-onto-eval-page ()
  (html-out (anchor "/planet-onto-ocml-eval-internal" "Back to Planet Onto Evaluate Page")))

(defun back-to-planet-onto-ask-page ()
  (html-out (anchor "/planet-onto-ocml-ask-internal" "Back to Planet Onto Ask Page")))

(define-page ("Onto Inspect Page" :func-name ocml-definition
			      :class :user :bgcolor *planet-onto-page-colour*
			      )
    (info)
  (with-ocml-name-and-ontology (info ocml-name ontology)
    (generate-ocml-definition-html ocml-name ontology)
    ;(html-out (hrule))
;    (back-to-planet-onto-page)
;    (html-out "<br>")
;    (back-to-planet-onto-eval-page)
;    (html-out "<br>")
;    (back-to-planet-onto-ask-page)
    ))

(defmacro test-html-form (&rest forms)
  `(let ((html-stream *standard-output*))
     (declare (special html-stream))
     ,@forms))

(defun generate-ocml-definition-html (ocml-name &optional (ontology 'ocml::kmi-planet-kb))
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((ok-to-lookup-definition-p t) (definition nil))      
      (cond ((ocml::get-ocml-class ocml-name)
	     (ocml::html-describe-class-info ocml-name #'ocml-lookup-current-word ontology))
	    ((ocml::find-current-instance ocml-name)
	     (ocml::html-describe-instance2
              ocml-name #'ocml-lookup-current-word ontology))
	    ((ocml::find-bc-rule ocml-name)
	     (ocml::html-describe-rule ocml-name #'ocml-lookup-current-word ontology))
	    ((ocml::get-ocml-relation ocml-name)
	     (setf ok-to-lookup-definition-p nil)
	     (ocml::html-describe-relation ocml-name #'ocml-lookup-current-word ontology))
	    ((ocml::get-ocml-function ocml-name)
	     (ocml::html-describe-function ocml-name #'ocml-lookup-current-word ontology)))
      (when ok-to-lookup-definition-p
        (setf definition (ocml::ocml-definition ocml-name ontology)))
      (when definition
        (html-out (header 1 (format nil "Source code of ~a" ocml-name)))
        (html-out (internal-insert-ocml-links
                   (with-output-to-string (html-stream)
                     (html-out (font (bold (preformatted (code definition))) :size 10)))
                   #'ocml-lookup-current-word
                   ontology))))))
                            
                              
(defun html-class-info (ocml-name string)
  (html-out (header 1 (format nil "Internal Description of class ~a" ocml-name)))
  (with-input-from-string (istream string)
    (html-ocml-object-internals istream)))

(defun html-ocml-object-internals (istream)
  (do ((line (read-line istream nil nil) (read-line istream nil nil)))
      ((null line))
    (cond ((find #\space line)
               (html-out (bold (concatenate 'string
                                            (string-capitalize (subseq line 0 (position #\space line)))
                                            ":")))
               (html-out (concatenate 'string (subseq line (position #\space line)) "<p>")))
              (t (html-out line)))))


(defun html-instance-info (string)
  (with-input-from-string (istream string)
    (let ((name (read istream))
          (class (read istream)))
      (html-out (header 1 (format nil "Internal Description of ~a of class ~a" name class)))
      (html-ocml-object-internals istream))))

(defun word-boundary-char-p (char)
  (find char '(#\space #\return #\linefeed #\newline #\tab #\( #\) #\. #\' #\" #\$ #\, #\:) :test #'char=))

(defvar *current-word* (make-array '(300) :element-type 'character))

;(defun internal-insert-ocml-links (string)
;  (if (zerop (length string))
;      ""
;      (do* ((i 0 (1+ i)) (string-length (length string)) (new-string "")
;            (ch (elt string i) (elt string i))
;            (last-char nil (elt string (1- i)))
;            (last-but-one-char nil) (inside-href-p nil)
;            (current-word "")
;            (inside-left-angle nil))
;           ((>= (1+ i) string-length) (setf new-string (add-current-word ch current-word new-string))
;            (reverse new-string))
;        (when (and last-char (char= last-char #\<) (or (char= ch #\a) (char= ch #\A)))
;          (setf inside-href-p t))
;        (when (and last-char last-but-one-char
;                   (char= last-but-one-char #\<) (char= last-char #\/) (or (char= ch #\A) (char= ch #\a)))
;          (setf inside-href-p nil))
;        (when (char= ch #\<)
;          (setf new-string (add-current-word nil current-word new-string)
;	        current-word "")
;          (push t inside-left-angle))
;        (cond ((or inside-left-angle inside-href-p)
;               (setf new-string (add-char ch new-string)))
;              ((word-boundary-char-p ch)
;               (setf new-string (add-current-word ch current-word new-string)
;                     current-word ""))
;              (t (setf current-word (add-char ch current-word))))
;        (when (char= ch #\>)
;          (pop inside-left-angle))
;        (setf last-but-one-char last-char))))


(defun internal-insert-ocml-links (string
                                   &optional
                                   (lookup-function #'ocml-lookup-current-word)
                                   (ontology 'ocml::kmi-planet-kb)
                                   bold-home-ontology-p
                                   (link-url
                                    "/ocml-definition?ocml-name=~a?ocml-ontology=~a"))
  (if (zerop (length string))
      ""
    (do* ((i 0 (1+ i)) (string-length (length string)) (new-string "")
          (ch (elt string i) (elt string i))
          (last-char nil (elt string (1- i)))
          (last-but-one-char nil) (inside-href-p nil)
          (current-word-length 0)
          (inside-left-angle nil))
         ((>= (1+ i) string-length)
          (cond ((char= ch #\>)
                 (concatenate 'string (reverse new-string) ">"))
                (t (unless (word-boundary-char-p ch)
                     (add-char-to-current-word ch *current-word*
                                               current-word-length)
                     (incf current-word-length)
                     (setf ch nil))
                   (setf new-string (add-current-word ch *current-word*
                                                      current-word-length new-string
                                                      lookup-function ontology
                                                      bold-home-ontology-p
                                                      link-url))
                   (reverse new-string))))
      (when (and last-char (char= last-char #\<) (or (char= ch #\a) (char= ch #\A)))
        (setf inside-href-p t))
      (when (and last-char last-but-one-char
                 (char= last-but-one-char #\<) (char= last-char #\/)
                 (or (char= ch #\A) (char= ch #\a)))
        (setf inside-href-p nil))
      (when (char= ch #\<)
        (setf new-string (add-current-word nil *current-word*
                                           current-word-length new-string
                                           lookup-function ontology
                                           bold-home-ontology-p
                                           link-url)
              current-word-length  0)
        (push t inside-left-angle))
      (cond ((or inside-left-angle inside-href-p)
             (setf new-string (add-char ch new-string)))
            ((word-boundary-char-p ch)
             (setf new-string (add-current-word ch *current-word*
                                                current-word-length new-string
                                                lookup-function ontology
                                                bold-home-ontology-p
                                                link-url)
                   current-word-length  0))
            (t (add-char-to-current-word 
                ch 
                *current-word* current-word-length)
               (incf current-word-length)))
      (when (char= ch #\>)
        (pop inside-left-angle))
      (setf last-but-one-char last-char))))

(defun add-char-to-current-word (ch current-word current-word-length)
  (setf (elt current-word current-word-length) ch))

(defun add-char (char string)
  (if (null char)
      string
      (concatenate 'string (string char) string)))

(defun add-strings (string1 string2)
  (concatenate 'string string1 string2))

(defun add-current-word (ch current-word current-word-length new-string
                            lookup-function ontology bold-home-ontology-p
                            link-url)
  (cond ((zerop current-word-length)
         (add-char ch new-string))
	(t (add-char ch (add-strings
                         (funcall lookup-function current-word current-word-length
                                  ontology bold-home-ontology-p link-url)
                         new-string)))))

(defun ocml-lookup-current-word (current-word current-word-length ontology
                                              bold-home-ontology-p link-url)
  (let* ((current-word-string (coerce (subseq current-word 0 current-word-length)
                                      'string))
         (reverse-current-word-string (reverse current-word-string))
         (word-to-lookup (intern (string-upcase current-word-string)
                                 (find-package "OCML"))))
    (multiple-value-bind (found-p home-ontology-p colour)
        (ocml::ocml-lookup word-to-lookup ontology)
      (if found-p
          (add-ocml-link word-to-lookup current-word-string t ontology
                         (and bold-home-ontology-p home-ontology-p)
                         colour link-url)
          reverse-current-word-string))))

(defun add-ocml-link (word-to-lookup current-word &optional (reverse-p t)
                                     (ontology 'kmi-planet-kb)
                                     bold-p colour link-url)
  (when colour
    (setf current-word (font current-word :color colour)))
  (when bold-p
    (setf current-word (bold current-word)))
  (if reverse-p 
      (reverse (anchor (format nil link-url
                               word-to-lookup ontology)
                       current-word))
      (anchor (format nil link-url
                      word-to-lookup ontology)
              current-word)))

(defun insert-ocml-links (news-item &optional (ontology 'ocml::kmi-planet-kb))
  (let ((new-news-item (cl-user::copy-news-item news-item)))
    (setf (cl-user::news-item-contents new-news-item)
          (internal-insert-ocml-links (cl-user::news-item-contents new-news-item)
                                      #'ocml-lookup-current-word
                                      ontology
                                      ))
    new-news-item))

(define-page ("Planet Onto Search Page" :func-name planet-onto-search
			      :class :user :bgcolor *planet-onto-page-colour*
			      ) 
    ()
  (html-out (header 1 "Planet Onto Search Page"))
  (html-out "Type in a string to search for")
  (with-field-value (string case-sensitive)
    (html-out (in-form (:action (url 'planet-onto-search-internal))
                (html-out "~a<p>" (text-field "string" :size 100))
                (html-out "Case Sensitive: ~a<p>" (checkbox "case-sensitive" :value case-sensitive))
                (html-out "<p>~a <HR><p>"
                          (submit-button "Find Stories Containing String"))))))

(define-page ("Planet Onto Search Page" :func-name planet-onto-search-internal
			      :class :user :bgcolor *planet-onto-page-colour*
			      ) 
    ()
  (html-out (header 1 "Planet Onto Search Page"))
  (html-out "Type in a string to search for")
  (with-field-value (string case-sensitive)
    (html-out (in-form (:action (url 'planet-onto-search-internal))
                (html-out "~a<p>" (text-field "string" :size 100))
                (html-out "Case Sensitive: ~a<p>" (checkbox "case-sensitive" :value case-sensitive))
                (html-out "<p>~a <HR><p>"
                          (submit-button "Find Stories Containing String"))
                (find-stories-containing string case-sensitive)
                (setf *last-search-string* string *last-case-sensitive-value* case-sensitive)))))

;(defun insert-search-links (news-item search-string case-sensitive-p)
; (let ((new-news-item (cl-user::copy-news-item news-item)))
;   (setf (cl-user::news-item-contents new-news-item)
;         (internal-insert-search-links (cl-user::news-item-contents new-news-item)
;                                       search-string case-sensitive-p)
;         (cl-user::news-item-headline news-item)
;         (internal-insert-search-links (cl-user::news-item-headline new-news-item)
;                                       search-string case-sensitive-p)
;         )
;   new-news-item))

(defun initialise-news-items-word-list (&optional (ontology 'ocml::kmi-planet-kb)
                                                  (news-items (cl-user::news-items)))
  (ocml::select-ontology ontology)
  (mapc #'(lambda (news-item)
            (let ((word-list nil))
              (internal-insert-ocml-links
               (cl-user::news-item-contents news-item)
               #'(lambda (current-word current-word-length ontology
                                       bold-home-ontology-p link-url)
                   (declare (ignore ontology bold-home-ontology-p link-url))
                   (let ((word-string (coerce (subseq current-word 0 current-word-length) 'string)))
                     (push word-string word-list)
                     (reverse word-string)))
               ontology)
              (internal-insert-ocml-links
               (cl-user::news-item-headline news-item)
               #'(lambda (current-word current-word-length ontology
                                       bold-home-ontology-p
                                       link-url)
                   (declare (ignore ontology bold-home-ontology-p link-url))
		   (let ((word-string (coerce (subseq current-word 0 current-word-length) 'string)))
                     (push word-string word-list)
                     (reverse word-string)))
               ontology)
              (setf (cl-user::news-item-word-list news-item)
                    (remove-duplicates word-list :test #'string=))))
        news-items))

(defun get-words-from-html (current-word current-word-length
                                                  search-string case-sensitive-p)
  (let* ((current-word-string )
         (reverse-current-word-string (reverse current-word-string)))
    (if case-sensitive-p
        (if (string= current-word-string search-string)
            (progn (setf *found-one-word-match* t) (add-search-link current-word-string))
            reverse-current-word-string)
        (if (string= (string-downcase current-word-string)
                     (string-downcase search-string))
            (progn (setf *found-one-word-match* t) (add-search-link current-word-string))
            reverse-current-word-string))))
             

(defun insert-search-links (news-item search-string case-sensitive-p)
  (let ((new-news-item (cl-user::copy-news-item news-item)))
    (setf (cl-user::news-item-contents new-news-item)
          (internal-insert-ocml-links (cl-user::news-item-contents new-news-item)
                                      #'(lambda (current-string current-string-length
                                                                ontology
                                                                bold-home-ontology-p
                                                                link-url)
                                          (declare (ignore ontology bold-home-ontology-p
                                                           link-url))
                                          (internal-insert-search-links
                                           current-string current-string-length
                                           search-string case-sensitive-p)))
          (cl-user::news-item-headline news-item)
          (internal-insert-ocml-links
           (cl-user::news-item-headline news-item)
           #'(lambda (current-string current-string-length ontology bold-home-ontology-p
                                     link-url)
               (declare (ignore ontology bold-home-ontology-p link-url))
               (internal-insert-search-links
                current-string current-string-length
                search-string case-sensitive-p))))
    new-news-item))

(defun find-all-string-matches (string search-string case-sensitive-p &optional (start 0) positions)
  (let ((position (search search-string string :start2 start)))
    (cond (position
	   (if (< position (length string))
	       (find-all-string-matches string search-string case-sensitive-p
					(1+ position) (cons position positions))
	       (reverse (cons position positions))))
	  (t (reverse positions)))))

(defvar *html-red* "#f00000")

(defun generate-string-sections (string positions search-string)
  (let ((search-string-length (length search-string)))
    (append (mapcan #'(lambda (position1 position2)
                        (list (subseq string position1 position2)
                              (font (bold search-string) :color *html-red*)))
                    (cons 0 (mapcar #'(lambda (x) (+ x search-string-length))
                                    positions)) positions)
            (list (subseq string (+ search-string-length (car (last positions))))))))

;(defun internal-insert-search-links (string search-string case-sensitive-p)
;  (let ((positions (find-all-string-matches (if case-sensitive-p
;                                                string
;                                                (string-downcase string))
;                                            (if case-sensitive-p
;                                                search-string
;                                                (string-downcase search-string))
;                                            case-sensitive-p)))
;    (if positions
;        (apply #'concatenate
;               (cons 'string (generate-string-sections string positions search-string)))
;        string)))

(defun internal-insert-search-links (current-word current-word-length
                                                  search-string case-sensitive-p)
  (let* ((current-word-string (coerce (subseq current-word 0 current-word-length) 'string))
         (reverse-current-word-string (reverse current-word-string)))
    (if case-sensitive-p
        (if (string= current-word-string search-string)
            (add-search-link current-word-string)
            reverse-current-word-string)
        (if (string= (string-downcase current-word-string)
                     (string-downcase search-string))
            (add-search-link current-word-string)
            reverse-current-word-string))))

(defun add-search-link (current-word)
  (reverse (font (bold current-word) :color *html-red*)))

;(or (search string (cl-user::news-item-headline news-item))
;                        (search string (cl-user::news-item-contents news-item)))

(defun stories-containing (string case-sensitive-p)
  (unless case-sensitive-p
    (setf string (string-downcase string)))
  (mapcan #'(lambda (news-item)
              (when (find string (cl-user::news-item-word-list news-item)
                          :test #'string=
                          :key (if case-sensitive-p #'identity #'string-downcase))
		(list news-item)))
          (cl-user::news-items)))
              
(defun find-stories-containing (string case-sensitive-p)
  (cond ((and (string= *last-search-string* string)
              (eq *last-case-sensitive-value* case-sensitive-p))
	 (display-augmented-news-items *last-original-news-items-displayed*
				       *last-augmented-news-items-displayed*))
	(t (setf *last-search-string* string *last-case-sensitive-value* case-sensitive-p)
           (let ((original-news-items
                  (stories-containing string case-sensitive-p))
		 (augmented-news-items nil))
             (cond (original-news-items
	            (setf augmented-news-items
		          (mapcar #'(lambda (news-item)
			              (insert-search-links news-item string case-sensitive-p))
			          original-news-items)
	                  *last-original-news-items-displayed* original-news-items
			  *last-augmented-news-items-displayed* augmented-news-items)
	            (display-augmented-news-items original-news-items augmented-news-items))
	           (t (html-out
		       (header 2 "Sorry No KMi Planet stories match your request"))))))))

(define-page ("Planet Story Submission Page" :func-name planet-submit
			                     :class :user :bgcolor *planet-onto-page-colour*
			                     ) 
    ()
  (html-out (header 1 "Submit a Story to KMi Planet"))
  (with-field-value (storytypes people technology project)
    (html-out (in-form (:action (url 'planet-submit-internal))
		          (html-out "<b>Email Address:</b> ~a<p>" (text-field "email" :size 50))
		          (html-out "<b>Story Title:</b>~a<p>" (text-field "story-title" :size 50))
		          (html-out "<b>Contents</b><P>~a<p>" (text-area "story-contents" 15 60))
		          (html-out "<b>Attachment Location</b>:~a<p>"
                                    (text-field "attached-image" :size 50))
                          (html-out "~a<HR><p>"
                                    (submit-button "Submit Story"))))))


(define-page ("Planet Story Submission Page" :func-name planet-submit-internal
			      :class :user :bgcolor *planet-onto-page-colour*
			      ) 
    ()
  (html-out (header 1 "Thanks for your Story to KMi Planet"))
  (html-out "You will receive notification soon"))