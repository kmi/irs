;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defvar *lois-java-file*
  "johnd.lois.lois.class")

(defvar *lois-archive-file* "lois.zip")

(defvar *default-page-colour*
  #+:irs-use-lispweb
  (http::color-string :red 255 :blue 255 :green 255))

(defvar *default-codebase* (format nil "~a/" *default-host*))

(defvar *result-file* "/lois-results")

(defvar *results* nil)

(defvar *default-ontology* 'kmi-planet-kb)

(defvar *end-of-proof* "%%%proof-finished")

(defvar *in-lois-query*)

(defvar *end-of-proof* "%%%proof-finished")

(defvar *in-lois-query*)

;(defun lois-indexes (stream request-string)
;  (with-input-from-string (info-stream request-string)
;    ;;;the action bit
;    (read info-stream)
;    (let* ((*package* (find-package "OCML"))
;           (ontology-name (read info-stream)) index-class-and-real-names index-names
;           index-aspects index-types index-instances items index-pretty-names)      
;      (ocml::select-ontology ontology-name)
;      (setf index-class-and-real-names (index-names)
;            index-names (mapcar #'car index-class-and-real-names)
;            index-pretty-names (mapcar #'cdr index-class-and-real-names)
;            index-aspects (index-aspects index-names)
;            index-types (index-types index-names (mapcar #'car index-aspects))
;            index-instances (index-instances (mapcar #'car index-types))
;            items (mapcar '(lambda (index-name index-pretty-name
;                                               index-aspects index-types index-instances)
;                             (list index-pretty-name index-name index-aspects index-types index-instances))
;			  index-names index-pretty-names index-aspects index-types index-instances))
;       (http::princ-to-binary-stream
;	(format nil "~{~{~a{~a{~{~(~a~)%~}{~{~(~a~)%~}{~{~(~a~)%~}{[~}~}~%" items)
;	stream))))

;(defun lois-indexes (stream request-string)
;  (with-input-from-string (info-stream request-string)
;    ;;;the action bit
;    (read info-stream)
;    (let* ((*package* (find-package "OCML"))
;           (ontology-name (read info-stream)) index-class-and-real-names items
;           top-index-name top-index-aspects top-index-types top-index-instances)      
;      (ocml::select-ontology ontology-name)
;      (setf index-class-and-real-names (index-names)
;            top-index-name (caar index-class-and-real-names)
;            top-index-aspects (index-aspect top-index-name)
;            top-index-types (cons top-index-name (ocml::all-current-subclass-names top-index-name))
;            top-index-instances (single-type-index-instances top-index-name)
;            items (list index-class-and-real-names top-index-aspects top-index-types
;                        top-index-instances))
;      (http::princ-to-binary-stream
;       (format nil "~{~{~{~(~a~)%~a%~}~}{~{~(~a~)%~}{~{~(~a~)%~}{~{~(~a~)%~}{~}~%" items)
;       stream))))

(defun lois-indexes (stream request-string)
  (with-input-from-string (info-stream request-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream)))      
      (ocml::select-ontology ontology-name)
      (http::princ-to-binary-stream
       (format nil "~{~{~(~a~)[~a[~}~}~%" (index-names ontology-name))
       stream))))

(defun return-index-aspects (stream request-string)
  (with-input-from-string (info-stream request-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (index-name (read info-stream)))
      (ocml::select-ontology ontology-name)
      (http::princ-to-binary-stream (format nil "~{~(~a~)[~}~%" (index-aspect index-name))
                                    stream))))

           
(defvar *kmi-planet-kb-index-names*
  '((ocml::event  "Story Event Type")
    (ocml::kmi-technology  "KMi Technology")
    (ocml::kmi-project   "KMi Project")
    (ocml::kmi-member  "Member of KMi")
    (ocml::organization "Organisation")))

(defvar *default-index-name-limit* 5)

;;;default the first 5 classes
(defmethod index-names (ontology)
  (let ((result nil))
    (maphash
     #'(lambda (name class)
         (unless (or (>= (length result) *default-index-name-limit*)
                     (not (eq (ocml::home-ontology class)
                              ocml::*current-ontology*)))
           (push (list name (format nil "~:(~a~)" name)) result)))
     ocml::*domain-classes*)
    (unless (= (length result) *default-index-name-limit*)
      (maphash
       #'(lambda (name class)
           (declare (ignore class))
           (unless (or (>= (length result) *default-index-name-limit*)
                       (assoc name result))
             (push (list name (format nil "~:(~a~)" name)) result)))
       ocml::*domain-classes*))
    result))

(defmethod index-names ((ontology (eql 'ocml::kmi-planet-kb)))
  *kmi-planet-kb-index-names*)

(defmethod index-names ((ontology (eql 'ocml::kmi-planet-kb-drafts-220799)))
  *kmi-planet-kb-index-names*)

(defun slot-names (name)
  (let ((structure (ocml::get-domain-class name)))
    (when structure
      (ocml::ocml-most-specific-class-slots structure))))

(defun sorted-slot-names (index-name)
  (sort (slot-names index-name) #'string<))
        

(defun index-part-slot-names (names &optional include-type-p)
  (mapcar #'(lambda (name)
              (if include-type-p
                  (cons 'dont-care (cons *lois-sub-type-name*
                                         (cons *lois-super-type-name*
                                               (slot-names name))))
                  (slot-names name)))
          names))

(defun index-aspects (index-names)
  (index-part-slot-names index-names t))

(defvar *lois-sub-type-name* 'ocml::concept-sub-type)

(defvar *lois-super-type-name* 'ocml::concept-super-type)

(defun get-virtual-slots (class-name)
  (let ((structure (ocml::get-domain-class class-name)))
    (when structure
      (ocml::current-virtual-slots structure))))

(defun get-virtual-slots* (class-name)
  (let ((structure (ocml::get-domain-class class-name)))
    (when structure
      (ocml::current-virtual-slots* structure))))

(defun index-aspect (index-name)
  (cons *lois-super-type-name*
        (cons *lois-sub-type-name*
              (sort (append (slot-names index-name)
                            (get-virtual-slots* index-name))
                    #'string<))))

(defun get-virtual-slot-subclasses (class-structure virtual-slot-name)
  (let ((virtual-slot-type
         (ocml::type-of-current-virtual-slot* class-structure virtual-slot-name)))
    (when virtual-slot-type
      (cons virtual-slot-type
            (sort (ocml::all-current-subclass-names virtual-slot-type)
                  #'string<)))))
         
(defun index-aspect-types (class-name slot-name)
  (let ((class-structure (ocml::get-domain-class class-name))
        slot-info-alist)
    (when class-structure
      (setf slot-info-alist
            (ocml::slot-info-alist class-structure))
      (cond ((assoc slot-name slot-info-alist)
             (ocml::sorted-descendent-slot-type-names
              (assoc slot-name slot-info-alist)))
            ((find slot-name (get-virtual-slots* class-name))
             (get-virtual-slot-subclasses class-structure slot-name))))))

(defun index-types (index-names index-aspects)
  (mapcar #'(lambda (index-name index-aspect)
              (if (eq index-aspect *lois-sub-type-name*)
                  (sort (ocml::all-current-subclass-names index-name) #'string<)
                  (if (eq index-aspect *lois-super-type-name*)
                      (sort (ocml::all-superclass-names index-name) #'string<)
                      (if (eq index-aspect 'dont-care)
                          nil
                          (index-aspect-types index-name index-aspect)))))
          index-names index-aspects))

(defun index-instances (index-types)
  (mapcar #'(lambda (index-type)
              (when index-type
	        (mapcar #'ocml::name (ocml::direct-instances index-type))))
          index-types))

(defun single-type-index-instances (index-type)
  (mapcar #'ocml::name (ocml::all-instances index-type)))

(defun process-line-for-sending (line)
  (substitute *ocml-line-separator*
              #\return
              (substitute *ocml-line-separator*
                          #\linefeed
                          line)))

(defun failure-p (x)
  (eq x :fail))

(defun new-result-name ()
  (intern (symbol-name (gensym)) (find-package "WEB-ONTO")))

(defun lois-query (stream request-string)
  (with-input-from-string (info-stream request-string)
    ;;;the action bit
    (read info-stream)
    (let* ((*package* (find-package "OCML"))
           (ontology-name (read info-stream))
           (query (read info-stream))
           (parsed-query (parse-lois-query query))
           (all-solutions-p (eq (read info-stream) 'ocml::true))
           (result-name (new-result-name))           
           (ocml::*current-ontology* ocml::*current-ontology*)
           (ocml::*current-ontologies* ocml::*current-ontologies*)
           (ocml::*defined-relations* ocml::*defined-relations*)
           (ocml::*axioms* ocml::*axioms*)
           (ocml::*defined-functions* ocml::*defined-functions*)
           (ocml::*bc-rules* ocml::*bc-rules*)
           (ocml::*domain-classes* ocml::*domain-classes*))
      (declare (special ocml::*current-ontology* ocml::*current-ontologies*
                        ocml::*defined-relations* ocml::*axioms*
                        ocml::*defined-functions* ocml::*bc-rules*
                        ocml::*domain-classes*))
      (ocml::select-ontology ontology-name)
      (multiple-value-bind (result all-query-variables raw-results)
          (evaluate-ocml-query query parsed-query all-solutions-p)
      (http::princ-to-binary-stream
       (format nil "~a~%" result-name)
       stream)
      (push (create-html-result result-name result raw-results
                                query parsed-query all-query-variables
                                all-solutions-p)
            *results*)))))

(defun contents-part (result result-name)
  (let ((i 0))
    (with-output-to-string (http::html-stream)
      (declare (special http::html-stream))
      (http::html-out (http::dotted-list 
	         (mapcar #'(lambda (news-item)
                             (declare (ignore news-item))
                             (incf i)
			     (http::anchor (format nil "lois-results?name=~a#~d" result-name i)
				     (format nil "Solution ~d" i)))
		         result))))))

(defun create-html-result (result-name result raw-results
                                       query parsed-query all-query-variables
                                       all-solutions-p)
  (cons result-name
        (concatenate
         'string
         "<h2>Original Query</h2>"
         (format nil
                 "The original query was:<p>~(~a~)<p> which in OCML is:<p>~(~a~)"
                 query parsed-query)
         "<h2>Solutions Found To Your Query</h2>"
         (if (failure-p result)
             (format nil "No solution was found to the query.")
             (if all-solutions-p
                 (format nil
                         "There ~:[was~;were~] ~d solution~p to the query.<p>~a<p> ~a ~{~{<p><A NAME=\"~d\"><h2>Solution ~d</h2></a> ~{<p>~{~a ~a~}~}~}~}"
                         (> (length result) 1)
                         (length result)
                         (length result)
                         (contents-part result result-name)
                         (refine-query-html all-query-variables raw-results) result)
                 (format nil
                         "The results were:<p>~{~{~s ~s<p>~}~}"
                         result))))))

(defvar *lois-variable-operators*
  '(most-common-value most-common-type categorise))

(defun integrate-vars-with-results (variables results)
  (cond ((null variables) nil)
        (t (cons (cons (car variables) (mapcar #'car results))
                 (integrate-vars-with-results (cdr variables)
                                              (mapcar #'cdr results))))))

(defun refine-query-html (all-query-variables raw-results)
  (with-output-to-string  (http::html-stream)
    (declare (special http::html-stream))
    (http::html-out
     (http::in-form (:action (http::url 'refine-lois-results))
       (http::html-out (http::header 2 "Refining the Query"))
       (http::html-out
        "You can filter the results query by selecting an operator and variable.<p> ~(~a~) ~(~a~) ~a ~a" 
          (http::menu-select "Operator" *lois-variable-operators*)
          (http::menu-select "Variable" all-query-variables)
          (http::hidden-field "values" (format nil "~a"
                                         (integrate-vars-with-results
                                                 all-query-variables raw-results)))
          (http::hidden-field "ontology-name" (format nil "~a" (current-ontology-name)))
          
          )
       (http::html-out "<p>~a" (http::submit-button "Analyse Query Variable"))))))

(defun all-variables (query)
  (remove-duplicates (internal-all-variables query)))

(defun internal-all-variables (query)
  (cond ((null query) nil)
        ((atom query)
         (when (ocml::variable? query)
           (list query)))
        (t (append (internal-all-variables (car query))
                   (internal-all-variables (cdr query))))))

(defun kmi-technology-value (kmi-technology)
  (let (result output)
    (setf output
          (with-output-to-string (*standard-output*)
            (setf result (ocml::setofall
                          '?e
                          `(ocml::event-involves-technology
                            ?e ,kmi-technology)))))
    (kmi-index-value kmi-technology
                     result output)))

(defun kmi-project-value (kmi-project)
  (let (result output)
    (setf output
          (with-output-to-string (*standard-output*)
            (setf result
                  (ocml::setofall
                   '?e
                   `(ocml::event-involves-project ?e ,kmi-project)))))
    (kmi-index-value kmi-project result output)))

(defun kmi-organization-value (organization)
  (let (result output)
    (setf output
          (with-output-to-string (*standard-output*)
            (setf result
                  (ocml::setofall
                   '?e
                   `(ocml::event-involves-organization ?e ,organization)))))
    (kmi-index-value organization result output)))

(defun kmi-member-value (kmi-member)
  (let (result output)
    (setf output
          (with-output-to-string (*standard-output*)
            (setf result
                  (ocml::setofall
                   '?x
                   `(ocml::event-involves-kmi-member ?x ,kmi-member)))))
    (kmi-index-value kmi-member result output)))

(defun next-output-segment (output)
  (let ((end-of-proof-position (search ocml::*end-of-proof* output)))
    (unless end-of-proof-position
      (error "Couldn't find the end marker for a proof"))
    (subseq output 0 end-of-proof-position)))
          

(defun kmi-index-value (index related-events output)
  (let* (news-items associated-news-items event-for-display)
    (setf associated-news-items
          (remove-duplicates
           (mapcan #'(lambda (event)
                       (let* ((news-anchor (news-anchor-for-event event))
                              (news-item (news-item-for-event event))
                              (next-output-segment
                               (next-output-segment output))
                              (rationale 
                               (format nil
                                       "~aThe story ~a relates the event ~(~a~)<br>"
                                       next-output-segment
                                       (mark-headline news-item)
                                       event)))
                         (when news-item
                           (push news-item news-items))
                         (setf output
                               (subseq output
                                       (+ (length next-output-segment)
                                          (length ocml::*end-of-proof*))))
                         (when news-anchor
                           (list (list news-anchor
                                       (make-html-rationale-anchor
                                        rationale))))))
                   related-events)
           :test #'(lambda (x y) (string= (car x) (car y))))
          event-for-display (generate-lois-result-string index))
    (if associated-news-items
        (values 
         (format nil "~a. Associated news items ~{~{~a ~a ~}~}."
                 event-for-display associated-news-items)
         news-items)
      (no-associated-items-found (ocml::name ocml::*current-ontology*)
                                 index))))

(defun make-html-rationale-anchor (output)
  (with-output-to-string (http::html-stream)
    (declare (special http::html-stream))
    (http::html-out 
     (http::anchor (format nil "lois-rationale?rationale=~a"
                           output)
                   "(rationale)"
                   :target "Lois-Rationale"))))

(defun careful-holds? (relation-name value)
  (and (ocml::get-relation relation-name)
       (ocml::holds? relation-name value)))

(defmethod lois-value (ontology x)
  (generate-lois-result-string x))

;(defmethod lois-value (x)
;  x)
;
;(defmethod lois-value ((kmi-event ocml::kmi-event))
;  (lois-story-value kmi-event))

(defun generate-lois-result-string (x)
  (http::internal-insert-ocml-links
   (format nil "~(~a~) " x)
   #'http::ocml-lookup-current-word
   (current-ontology-name)))

(defun news-anchor-for-event (event)
  (let ((news-item 
         (ocml::news-items-from-story-name
                   (car (ocml::ocml-eval-gen
                         `(ocml::setofall ?x
                                          (ocml::kmi-planet-story
                                           ?x ocml::relates-events
                                           ,event))))
                   (current-ontology-name))))
    (when news-item
      (create-story-anchor news-item))))

(defun news-item-for-event (event)
  (ocml::news-items-from-story-name
   (car (ocml::setofall '?x
                        `(ocml::kmi-planet-story
                          ?x ocml::relates-events
                          ,event)))
   (current-ontology-name)))

(defun create-story-anchor (news-item)
  (with-output-to-string (http::html-stream)
    (declare (special http::html-stream))
    (http::html-out
     (http::anchor (format nil "lois-story?story-number=~d"
                     (cl-user::news-item-number news-item))
             (cl-user::news-item-headline news-item)
             :target "Lois-Story"))))
 

(defun augmented-html (news-item)
  (let ((augmented-news-item
         (http::insert-ocml-links news-item
                                  (current-ontology-name))))
    (with-output-to-string (http::html-stream)
      (declare (special http::html-stream))
      (cl-user::write-news-item
       augmented-news-item
       (cl-user::news-item-number news-item) t))))

(defvar *headline-marker*
  "%%%")

(defun mark-headline (news-item)
  (concatenate 'string
               *headline-marker*
               (remove #\' (remove #\" (cl-user::news-item-headline news-item)))
               *headline-marker*))

(defmethod get-associated-story (ontology x)
  (no-associated-items-found ontology  x))

(defmethod no-associated-items-found (ontology x)
  (format nil "Sorry no associated items were found for ~a in the knowledge model ~a"
          x ontology))

(defun lois-story-value (event)
  (let ((news-anchor-for-event (news-anchor-for-event event))
        (news-item (news-item-for-event event))
        (event-for-display (generate-lois-result-string event)))
    (if news-anchor-for-event
        (format nil "Associated news item ~a ~a"
                news-anchor-for-event
                (make-html-rationale-anchor
                 (format nil
                         "The story ~a relates the event ~(~a~)<br>"
                         (mark-headline news-item)
                         event)))
        (no-associated-items-found (ocml::name ocml::*current-ontology*)
                                   event))))

(defmacro with-result-name ((info &optional name) &rest body)
  `(let* ((,name (intern (symbol-name 
                          (http::get-decoded-form-value ,info :name))
                         (find-package "WEB-ONTO"))))
    ,@body))

(defmacro with-lois-value-and-ontology ((info &optional value ontology) &rest body)
  `(let* ((,value (intern (symbol-name 
                           (http::get-decoded-form-value ,info :value))
                          (find-package "OCML")))
          (,ontology (intern (symbol-name 
                              (http::get-decoded-form-value ,info :ontology))
                             (find-package "OCML"))))
     ,@body))


#+lispworks
(editor::setup-indent 'with-result-name 0 2)


(defmacro with-lois-story-number ((info &optional story-number) &rest body)
  `(let* ((,story-number (http::get-decoded-form-value ,info :story-number)))
    ,@body))

#+lispworks
(editor::setup-indent 'with-lois-story-number 0 2)


#+lispworks
(editor::setup-indent 'with-lois-value-and-ontology 0 2)



(defmacro with-lois-rationale ((info &optional rationale) &rest body)
  `(let* ((,rationale (http::get-decoded-form-string-value ,info :rationale)))
    ,@body))

#+lispworks
(editor::setup-indent 'with-lois-rationale 0 2)


(defmacro with-lois-query-output ((info &optional output ontology) &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,output (http::get-decoded-form-string-value ,info :output))
          (,ontology (read-from-string (http::get-decoded-form-string-value ,info :ontology))))
    ,@body))

#+lispworks
(editor::setup-indent 'with-lois-query-output 0 2)

(http::define-page ("Associated Story Page" :func-name associated-story
                                            :class :user :bgcolor *default-page-colour*
                                            :Header-p nil
                                            ) 
    (&rest info)
  (when info
    (setf info (car info)))
  (with-lois-value-and-ontology (info value ontology)
    (let ((ocml::*current-ontology* ocml::*current-ontology*)
          (ocml::*current-ontologies* ocml::*current-ontologies*)
          (ocml::*defined-relations* ocml::*defined-relations*)
          (ocml::*axioms* ocml::*axioms*)
          (ocml::*defined-functions* ocml::*defined-functions*)
          (ocml::*bc-rules* ocml::*bc-rules*)
          (ocml::*domain-classes* ocml::*domain-classes*))
      (declare (special ocml::*current-ontology* ocml::*current-ontologies*
                        ocml::*defined-relations* ocml::*axioms*
                        ocml::*defined-functions* ocml::*bc-rules*
                        ocml::*domain-classes*))
      (ocml::select-ontology ontology)
      (http::html-out (http::center (http::header 1 "Lois Associated Stories Page")))
      (http::html-out (get-associated-story ontology value)))))

(http::define-page ("Lois Results Page" :func-name lois-results
                           :class :user :bgcolor *default-page-colour*
                           :Header-p nil
                           ) 
    (&rest info)
  (when info
    (setf info (car info)))
  (with-result-name (info result-name)
    (http::html-out (http::center (http::header 1 "Lois Results Page")))
    (http::html-out (get-lois-result result-name))))


(http::define-page ("Lois Story" :func-name lois-story
                           :class :user :bgcolor *default-page-colour*
                           :Header-p nil
                           ) 
    (&rest info)
  (when info
    (setf info (car info)))
  (with-lois-story-number (info story-number)
    (http::html-out (http::center (http::header 1 "Lois Story")))
    (let ((news-item (cl-user::news-item-from-number story-number)))
      (if news-item
          (http::html-out (augmented-html news-item))
          (http::html-out
           (format nil "Sorry, couldn't find the news item ~d" story-number))))))


(defun replace-headline-marker (rationale headline-marker-position html-string)
  (concatenate 'string
               (subseq rationale 0 headline-marker-position)
               html-string
               (subseq rationale
                       (+ headline-marker-position
                          (length *headline-marker*)))))

(defun maybe-html-headline (rationale)
  (let ((headline-marker-position (search *headline-marker* rationale)))
    (cond (headline-marker-position
           (setf rationale
                 (replace-headline-marker rationale headline-marker-position "<i>"))
           (setf headline-marker-position (search *headline-marker* rationale)
                 rationale
                 (replace-headline-marker rationale headline-marker-position "</i>")))
          (t rationale))))

(defvar *statement-terminator* "<br>")

(defun reverse-rationale-statements (rationale)
  (let ((statements nil))
    (do ((statement-terminator-length (length *statement-terminator*))
         (position (search *statement-terminator* rationale)
                   (search *statement-terminator* rationale)))
        ((null position)
         (apply #'concatenate (cons 'string statements)))
      (push (subseq rationale 0 (+ statement-terminator-length position))
            statements)
      (setf rationale (subseq rationale (+ statement-terminator-length position))))))

(http::define-page ("Lois Rationale" :func-name lois-rationale
                                     :class :user :bgcolor *default-page-colour*
                                     :Header-p nil
                                     ) 
    (&rest info)
  (when info
    (setf info (car info)))
  (with-lois-rationale (info rationale)
    (setf rationale (maybe-html-headline (reverse-rationale-statements rationale)))
    (http::html-out (http::center (http::header 1 "Lois Rationale")))
    (http::html-out
     (http::internal-insert-ocml-links
      rationale
      #'http::ocml-lookup-current-word
      (current-ontology-name)))))

(http::define-page ("Lois Query Output" :func-name lois-query-output
                                     :class :user :bgcolor *default-page-colour*
                                     :Header-p nil
                                     ) 
    (&rest info)
  (when info
    (setf info (car info)))
  (with-lois-query-output (info output ontology)
    (http::html-out (http::center (http::header 1 "Explanation of Results")))
    (http::html-out
     (http::internal-insert-ocml-links
      output
      #'http::ocml-lookup-current-word
      ontology))))

(defun get-lois-result (name)
  (cdr (assoc name *results*)))

(defun ask-with-output (query &key all query-mode)
  (let (output result)
    (setf output
          (with-output-to-string (*standard-output*)
            (setf result (ocml::ask-top-level
                          query
                          :all all :query-mode query-mode))))
    (values result output)))

(defun current-ontology-name ()
  (ocml::name ocml::*current-ontology*))

;(defun evaluate-ocml-query (query parsed-query all-solutions-p)
;  (let ((all-variables (all-variables query)) (i 0)
;        (envs (ocml::ask-top-level
;                parsed-query
;                :all all-solutions-p :query-mode nil)))
;    (when (listp envs)
;      (setf envs (remove-duplicates envs :test #'equal)))
;    (if (eq envs :fail)
;        :fail
;        (if all-solutions-p
;            (remove-duplicates
;             (mapcar #'(lambda (env)
;                         (incf i)
;                         (list i i (mapcar #'(lambda (var)
;                                               (list (format nil "~(~a~)" var)
;                                                     (lois-value
;                                                      (ocml::name
;                                                       ocml::*current-ontology*)
;                                                      (ocml::instantiate var env))))
;                                           all-variables)))
;                     envs)
;             :test #'equal)
;            (mapcar #'(lambda (var)
;                        (list (format nil "~(~a~)" var)
;                              (lois-value
;                               ocml::*current-ontology*
;                               (ocml::instantiate var envs))))
;                    all-variables)))))

(defun findany (all-variables parsed-query)
  (let ((result (ocml::findany all-variables parsed-query)))
    (if (eq result :nothing)
        nil
        result)))
        
(defun findall (exp goal &optional env)
  (Let ((envs (ocml::ask-top-level goal :all t :env env)))
    (if (eq envs :fail)
        nil
	(mapcar #'(lambda (env)
                  (ocml::instantiate exp env))
              envs))))

(defvar *query-output*)

(defun break-up-proof-outputs (results string)
  (internal-break-up-proof-outputs results string))

(defun find-result-proof-with-output (result results-and-output)
  (assoc result results-and-output :test #'equal))

(defun add-new-output-to-proof (result output results-and-output)
  (setf (cdr (assoc result results-and-output :test #'equal))
        (append (cdr (assoc result results-and-output :test #'equal))
                (list output)))
  results-and-output)
  
(defun internal-break-up-proof-outputs (results string
                                                &optional
                                                (previous-results-and-output nil))
  (let ((end-of-proof-position 
         (and (>= (length string) (length ocml::*end-of-proof*))
              (search ocml::*end-of-proof* string))))
    (cond (end-of-proof-position
           (cond ((find-result-proof-with-output (car results)
                                                 previous-results-and-output)
                  (internal-break-up-proof-outputs
                   (cdr results)
                   (subseq string 
                           (+ end-of-proof-position (length ocml::*end-of-proof*)))
                   (add-new-output-to-proof (car results)
                                            (subseq string 0 end-of-proof-position)
                                            previous-results-and-output)))
                 (t (internal-break-up-proof-outputs
                     (cdr results)
                     (subseq string 
                             (+ end-of-proof-position (length ocml::*end-of-proof*)))
                     (cons (list (car results) (subseq string 0 end-of-proof-position))
                           previous-results-and-output)))))
          ((and (car results) (> (length string) 0))
           (cons (list (car results) string)
                 previous-results-and-output))
          (t previous-results-and-output))))

(defun process-query-output (output-strings)
  (setf output-strings (remove-duplicates output-strings :test #'string=))
  (cond ((<= (length output-strings) 1)
         (car output-strings))
        (t (let ((i 0))
             (format nil "The query was proved in ~d ways.<p>~{~a~}"
                     (length output-strings)
                     (mapcar #'(lambda (output)
                                 (incf i)
                                 (create-query-rationale output i))
                             output-strings))))))

(defun create-query-rationale (output i)
  (format nil "Proof ~d<br> ~a.<p>" i output))

;;; This function no longer works.  It relied on a hacked version of
;;; #'ocml::*prove-aux which printed webonto::*end-of-proof* after
;;; each successful proof.  This broke when the OCML version was
;;; reworked.  I'm not hacking the hack.  The *correct* ways to fix
;;; this include:
;;;
;;; 1. A better way of handling interactive proof in OCML, offering
;;;    more control and information to the calling function.
;;;
;;; 2. A hook to be called by *prove-aux when it completes a proof,
;;;    this hook printing the webonto::*end-of-proof* string.
(defun evaluate-ocml-query (query parsed-query all-solutions-p)
  (error "Broken.  See webonto::evaluate-ocml-query for details.")
  (let ((all-variables (all-variables query)) (i 0)
        results (raw-results nil)
        (*query-output*))
    (declare (special *query-output*))
    (setf *query-output*
          (with-output-to-string (*standard-output*)
            (setf results (if all-solutions-p
                              (let ((ocml::*in-lois-query* t))
                                (declare (special ocml::*in-lois-query*))
                                ;;ocml::setofall removes duplicates. But sometimes
                                ;;we can have the same results for different reasons
                                ;;(so the output is different). So we now use findall and
                                ;;remove duplicates carefully
                                (ocml::findall
                                 all-variables
                                 parsed-query))
                              (findany
                               all-variables parsed-query)))))
;;;    (setf nnn results o (ocml::name ocml::*current-ontology*))
    (values
     (if (null results)
         :fail
         (if all-solutions-p
             (mapcan #'(lambda (values-with-output)
                         (let ((values (car values-with-output))
                               (output (process-query-output (cdr values-with-output))))
                           (push values raw-results)
                           (list (let ((*query-output* output))
                                   (declare (special *query-output*))
                                   (incf i)
                                   (list i i (mapcar #'(lambda (var value)
                                                         (list (format nil "~(~a~)" var)
                                                               (lois-value
                                                                (ocml::name
                                                                 ocml::*current-ontology*)
                                                                value)))
                                                     all-variables values))))))
                     (break-up-proof-outputs (reverse results)
                                             *query-output*))
             (mapcar #'(lambda (var value)
                         (list (format nil "~(~a~)" var)
                               (lois-value
                                (ocml::name
                                 ocml::*current-ontology*)
                                value)))
                     all-variables results)))
     all-variables
     (if all-solutions-p
         (reverse raw-results)
         results))))

(defun reduce-depth (list)
  (cond ((and (listp list)
           (find (car list) '(or and))
           (= 2 (length list)))
         (reduce-depth (second list)))
        ((= (length list) 1)
         (reduce-depth (car list)))
        (t list)))
      
(defun maybe-remove-first-relation (query)
  (if (and (= (length query) 2) (find (car query) '(or and)))
      (second query)
      query))

(defun parse-lois-query (lois-query)
  (let ((initial-parse (internal-parse-lois-query lois-query)))
    (maybe-remove-first-relation
     (remove-duplicates
     (if (= (length initial-parse) 1)
        (car initial-parse)
        (cons 'or initial-parse))
     :test #'equal))))

(defun internal-parse-lois-query (lois-query)
  (let ((current-and-bundle nil) (and-bundles nil))
    (dolist (expression (butlast lois-query))
      (case (car expression)
        ((or)
         (setf current-and-bundle
               (append current-and-bundle
                       (parse-lois-query-expression
                        (cdr expression) nil)))
         (setf and-bundles
               (append
                and-bundles
                (list (if (> (length current-and-bundle) 1)
                          (cons 'and current-and-bundle)
                          (car current-and-bundle))))
               current-and-bundle nil))
        ((and) (setf current-and-bundle
                     (append current-and-bundle
                             (parse-lois-query-expression
                              (cdr expression) t))))))
    (setf current-and-bundle
          (append current-and-bundle
                  (parse-lois-query-expression
                   (cdr (car (last lois-query))) t)))
    (append
     and-bundles
     (list (if (> (length current-and-bundle) 1)
               (cons 'and current-and-bundle)
               (car current-and-bundle))))))

(defun typed-variable-expression (x)
  (when (listp x)
    (list x)))

(defun relation-argument-expression (x)
  (if (listp x)
      (second x)
      x))

#|
(defun parse-lois-query-expression (lois-query-expression and-p)
  (cond ((find (second lois-query-expression)
               (list *lois-sub-type-name* *lois-super-type-name*))
         `((,(relation-argument-expression (third lois-query-expression))
            ,(relation-argument-expression (car lois-query-expression)))))
        ((eq (second lois-query-expression) 'ocml::is)
         (if and-p
             `((ocml::=
                ,(relation-argument-expression (car lois-query-expression))
                ,(relation-argument-expression (third lois-query-expression)))
               ;;,@(typed-variable-expression (car lois-query-expression))
               ;;,@(typed-variable-expression (third lois-query-expression))
               )
             `((and (ocml::=
                     ,(relation-argument-expression (car lois-query-expression))
                     ,(relation-argument-expression (third lois-query-expression)))
                    ;;,@(typed-variable-expression (car lois-query-expression))
                    ;;,@(typed-variable-expression (third lois-query-expression))
                    ))))
        (t (if and-p
               `(,@(typed-variable-expression (car lois-query-expression))
                 ,@(typed-variable-expression (third lois-query-expression))
                 (,(second lois-query-expression)
                  ,(relation-argument-expression (car lois-query-expression))
                  ,(relation-argument-expression (third lois-query-expression))))
               `((and ,@(typed-variable-expression (car lois-query-expression))
                      ,@(typed-variable-expression (third lois-query-expression))
                      (,(second lois-query-expression)
                       ,(relation-argument-expression (car lois-query-expression))
                       ,(relation-argument-expression (third lois-query-expression)))))))))
|#

;;put the typing forms at the end because they are slow
(defun parse-lois-query-expression (lois-query-expression and-p)
  (cond ((find (second lois-query-expression)
               (list *lois-sub-type-name* *lois-super-type-name*))
         `((,(relation-argument-expression (third lois-query-expression))
            ,(relation-argument-expression (car lois-query-expression)))))
        ((eq (second lois-query-expression) 'ocml::is)
         (if and-p
             `((ocml::=
                ,(relation-argument-expression (car lois-query-expression))
                ,(relation-argument-expression (third lois-query-expression)))
               ;;,@(typed-variable-expression (car lois-query-expression))
               ;;,@(typed-variable-expression (third lois-query-expression))
               )
             `((and (ocml::=
                     ,(relation-argument-expression (car lois-query-expression))
                     ,(relation-argument-expression (third lois-query-expression)))
                    ;;,@(typed-variable-expression (car lois-query-expression))
                    ;;,@(typed-variable-expression (third lois-query-expression))
                    ))))
        (t (if and-p
               `((,(second lois-query-expression)
                  ,(relation-argument-expression (car lois-query-expression))
                  ,(relation-argument-expression (third lois-query-expression)))
                 ,@(typed-variable-expression (car lois-query-expression))
                 ,@(typed-variable-expression (third lois-query-expression))
                 )
               `((and (,(second lois-query-expression)
                       ,(relation-argument-expression (car lois-query-expression))
                       ,(relation-argument-expression (third lois-query-expression)))
                      ,@(typed-variable-expression (car lois-query-expression))
                      ,@(typed-variable-expression (third lois-query-expression))
                      ))))))

(defmacro with-lois-info ((info &optional ontology) &rest body)
  `(let* ((*package* (find-package "OCML"))
          (,ontology (http::get-decoded-form-value ,info :ontology)))
    ,@body))

#+lispworks
(editor::setup-indent 'with-lois-info 0 2)

(http::define-page ("Lois Page" :func-name lois
                                :class :user :bgcolor
                                *default-page-colour*
                                :base "/tadzebao/lois.html"
                                ;;:external-base 
                                ;;(format nil "~alois.html"
                                  ;;      *default-codebase*)
                                :header-p nil
                                ) 
    (&rest info)
  (when info
    (setf info (car info)))
  (with-lois-info (info ontology)
    (http::html-out (http::center (http::header 1 "Lois")))
    (http::html-out (http::center
                     (http::inline-image
                      *kmi-image-url*
                      :remote t)))
    (http::html-out
     (http::italic
      (format nil "<p>Welcome to Lois part of Planet Onto, constructed within the ~a, ~a and ~a projects. This system was designed by ~a and ~a and implemented by ~a.  You can find a paper on Planet Onto in ~a, ~a and ~a formats<p></p>"
              (http::anchor "http://kmi.open.ac.uk/projects/hcrema/"
                            "HCREMA")
              (http::anchor "http://kmi.open.ac.uk/projects/enrich/"
                            "Enrich")
              (http::anchor "http://kmi.open.ac.uk/projects/ibrow/"
                            "IBROW")
              (http::anchor "http://kmi.open.ac.uk/~john/"
                            "John Domingue")
              (http::anchor "http://kmi.open.ac.uk/~enrico/"
                            "Enrico Motta")
              (http::anchor "http://kmi.open.ac.uk/~john/"
                            "John Domingue")
              (http::anchor "http://kmi.open.ac.uk/~enrico/papers/ekaw99/planet_onto.ps"
                            "postscript")
              (http::anchor "http://kmi.open.ac.uk/~enrico/papers/ekaw99/planet_onto.ps.gz"
                            "gzipped postscript")
              (http::anchor "http://kmi.open.ac.uk/~enrico/papers/ekaw99/planet_onto.pdf"
                            "PDF"))))
    (http::html-out
     (format nil
             "<title>LOIS</title>
<!-- Changed by: John Domingue,  5-Feburary-1999 -->
<hr>
<applet code=\"~a\" codebase=\"~a\" width=0 height=0>
<param name=port value=\"~d\">
<param name=ontology value=\"~(~a~)\">
<param name=result_file value=\"~a\"></applet>"
             *lois-java-file*
             *default-codebase*
             ;;*lois-archive-file*
             *default-port*
             (or ontology *default-ontology*)
             *result-file*))))
   


(defvar *scholarly-knowledge-index-names*
  '((ocml::skc-software "Software")
    (ocml::skc-methodology "Methodology")
    (ocml::skc-language "Language")
    (ocml::skc-theory-model   "Theory Model")
    (ocml::skc-phenomenon  "Phenomenon")
    (ocml::skc-idea "Idea")
    (ocml::skc-problem "Problem")))


(defmethod index-names ((ontology (eql 'ocml::scholarly-knowledge)))
  *scholarly-knowledge-index-names*)
