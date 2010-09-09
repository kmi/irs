;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defun java-true-p (x)
  (eq x 'ocml::true))

(defun send-find-object-result (stream result)
  (http::princ-to-binary-stream
   (if result
       (format nil
               "~(~a~)[~{~{~(~a~){~{~(~a~){~{~(~a~)%~}{~}[~}~}~%"
               (ok-p-result (or (ocml::unlimited-p ocml::*list-length-limit*)
                                (< *number-of-keys-found* ocml::*list-length-limit*)))
               (mapcar #'(lambda (ontology-find)
                           (let ((ontology-name (ocml::name (cdr ontology-find)))
                                 (finds-in-ontology (car ontology-find)))
                             (cons ontology-name
                                   (list finds-in-ontology))))
                       result))
       (format nil
               "nothing-found[~%"))
   stream))

(defvar *number-of-keys-found*)

(defvar *all-ontologies-name* 'ocml::all-ontologies)

(defun find-object (stream request-string)
  (with-input-from-string (get-find-object-stream request-string)
    ;;;the find_object bit
    (read get-find-object-stream)
    (let ((*package* (find-package "OCML")))
      (multiple-value-bind (or-filters and-filters)
          (parse-filters (read get-find-object-stream))
        (let* ((*or-filters* or-filters)
               (*and-filters* and-filters)
               (ocml::*list-length-limit* (read get-find-object-stream))
               (string (read get-find-object-stream))
               (ontology-name (read get-find-object-stream))
               (home-ontology-only? (java-true-p (read get-find-object-stream)))
               (include-classes? (java-true-p (read get-find-object-stream)))
               (include-relations? (java-true-p (read get-find-object-stream)))
               (include-axioms? (java-true-p (read get-find-object-stream)))
               (include-functions? (java-true-p (read get-find-object-stream)))
               (include-bc-rules? (java-true-p (read get-find-object-stream)))
               (include-instances? (java-true-p (read get-find-object-stream)))
               (*number-of-keys-found* 0)
               (all-ontologies-p (eq ontology-name *all-ontologies-name*)))
          (if all-ontologies-p
              (send-find-object-result
               stream
               (remove nil
                       (ocml::collect-names-matching-substring-in-all-ontologies
                        string
                        :home-ontology-only? home-ontology-only?
                        :include-classes? include-classes?
                        :include-relations? include-relations?
                        :include-axioms? include-axioms?
                        :include-functions? include-functions?
                        :include-bc-rules? include-bc-rules?
                        :include-instances? include-instances?)))
              (let ((ontology (ocml::get-ontology ontology-name)))
                (if ontology
                    (send-find-object-result
                     stream
                     (let ((result 
                            (ocml::collect-names-matching-substring-in-ontology
                             string
                             ontology
                             :home-ontology-only? home-ontology-only?
                             :include-classes? include-classes?
                             :include-relations? include-relations?
                             :include-axioms? include-axioms?
                             :include-functions? include-functions?
                             :include-bc-rules? include-bc-rules?
                             :include-instances? include-instances?)))
                       (if result
                           (list result)
                           nil)))
                    (http::princ-to-binary-stream
                     (format nil "unknown-ontology~a~%"
                             *ocml-line-separator*)
                     stream)))))))))


;;;i redefine these two functions in ocml/describe to include
;;;filters
(defun ocml::collect-all-keys-containing-substring (hash-table 
                                                    ontology 
                                                    string home-ontology-only? 
                                                    &aux result)
  (catch 'over-limit
    (maphash #'(lambda (key value)
                 (when (and (ocml::limited-p ocml::*list-length-limit*)
                            (> *number-of-keys-found* ocml::*list-length-limit*))
                   (throw 'over-limit nil))
                 (when (filter key value)
                   (let ((key-string (if (and (atom key) (symbolp key))
                                         (symbol-name key)
                                         (format nil "~A" key ))))
                     (when (search string key-string :test #'string-equal)
                       (unless (and home-ontology-only?
                                    (not (eq (ocml::home-ontology value) ontology)))
                         (incf *number-of-keys-found*)
                         (push key result))))))
             hash-table))
  result)

(defun ocml::collect-all-keys-containing-substring-over-lists
       (keys values
             ontology 
             string home-ontology-only? 
             &aux result)
  (catch 'over-limit
    (mapc #'(lambda (key value)
              (when (and (ocml::limited-p ocml::*list-length-limit*)
                         (> *number-of-keys-found* ocml::*list-length-limit*))
                (throw 'over-limit nil))
              (when (filter key value)
                (let ((key-string (if (and (atom key) (symbolp key))
                                      (symbol-name key)
                                      (format nil "~A" key ))))
                  (when (search string key-string :test #'string-equal)
                    (unless (and home-ontology-only?
                                 (not (eq (ocml::home-ontology value) ontology)))
                      (incf *number-of-keys-found*)
                      (push key result))))))
          keys values))
  result)