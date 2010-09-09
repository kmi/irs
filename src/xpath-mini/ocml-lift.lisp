(in-package :kmi.vt.xpath-mini)

;;------- added by alessio in order to manage the general case of soap messages: extracting from soap envelope and body, following href link to the multiRef list (17-02-2006)

;;; allows to clean an xml code adopting an xpath call, the result is
;;; a xml code
(defun filter-xml (xml query)
  (with-xml-ctx (:sequence (document-parser xml))
    (element-to-string (first (x-xp query)))))

;; extracts the core of the soap message, the result is a node-element
(defun filter-soap-body (xml)
  (with-xml-ctx (:sequence (document-parser xml))
    (if (first (x-xp "Envelope/Body/*"))
        (first (x-xp "Envelope/Body/*"))
        nil)))

;; extracts the list of multirefs enclosed in the soap message
(defun get-multiRef-list (xml)
  (with-xml-ctx (:sequence (document-parser xml))
    (x-xp "Envelope/Body/multiRef")))

;; returns the list of elements enclosed in a multiref element with a specific id
(defun find-ref (id xml-ref)
  (first (loop for ref-elem in xml-ref
            when (string= (first (children (first (%xml-get-attributes-named "id" ref-elem)))) id)
            collect (remove-if #'stringp  (children ref-elem)))))

;;; Recursive fun that filters an element; if the element links to a
;;; multirefs element, it uses the above fun to obtain the nested
;;; elements.
(defun create-filtered-element (elem xml-ref)
  (let ((kids (if (string= (dom:local-name (first (dom:attributes elem))) "href")
                  (loop for el in (find-ref (remove #\# (first (children (first (dom:attributes elem))))) xml-ref)
                     ;; seek in the multiRef list the elements to add
                     collect (create-filtered-element el xml-ref))
                  (loop for child in (children elem)
                     collect (if (dom:element-p child)
                                 ;; collects some nested elements
                                 (create-filtered-element child xml-ref)
                                 ;; collects the value of the element
                                 (eval child))))))
    (let ((node (dom:create-element *dom* (dom:local-name elem))))
      (dolist (k kids)
        (dom:append-child node k))
      node)))

(defvar *dom*)

;;; filters an xml code parsing it and creating a new xml code without
;;; namespaces, href, etc.
(defun filter (xml)
  (with-xml-ctx (:sequence (document-parser xml))
    (let  ((xml-ref (get-multiRef-list xml))
           (xml-body (filter-soap-body xml)))
      (progn
        (if (x-xp "Envelope/Body") ;; check if I have to filter
            (filter-xml ;; used only for deleting the header  
             (element-to-string
              (let* ((*dom* (rune-dom:create-document))
                     (root (dom:create-element *dom* (dom:local-name xml-body))))
                (loop for el in (x-xp (concatenate 'string "Envelope/Body/"
                                                   (dom:local-name xml-body) "/*"))
                   collect (create-filtered-element el xml-ref))
                dom))
             "*")
            (eval xml))))))

;; ----------added and changed by alessio for managing a lifting with multi-elements of the same type (15-02-2006) 

(defun element-to-string (el)
  ;; Acutally, EL has probably got to be a DOM.
  (let* ((stream (flexi-streams:make-in-memory-output-stream
                  :element-type '(unsigned-byte 8)))
         (sink (cxml:make-octet-stream-sink stream :indentation nil :canonical 1)))
    (dom:map-document sink el)
    (flexi-streams:octets-to-string
     (flexi-streams:get-output-stream-sequence stream))))


(defun path-extract2 (expr arg)
  (append (list (first expr)) arg))        

(defun path-extract (expr)
  (list (first (second (second expr)))))  

(defun result-keyword-extract (expr)
  " extracts keyword :
  :AS-LIST
  :AS-VALUES
  from expressions such as:
  (\"has-location\" (lift-plume-location '(\"getPlumePolygonInRadiusReturn/coordinates\") :AS-VALUES))"
  (when (and (listp expr) (> (length expr) 2)) (third expr)))

(defun path-cut (path)
 (subseq path 0 (position #\/ path :from-end t)))

(defun internal-lift (xml ontology class exprs)
  (with-xml-ctx (:sequence (document-parser (filter xml))) ;; changed by alessio (17-02-2006): added the filter for general cases
    (with-ocml-ctx (:ontology ontology :class class :instance (o-instance))
      (loop for expr in exprs
         do (let ((slot (first expr))
                  (value (cond
                           ((stringp (second expr)) (x-xp (second expr))) ;it is an xpath
                           ((eql (car (second expr)) 'EVAL) (eval (cadr (second expr)))) ;; it is a function
                           ;;it is a list eval expression, e.g. 
                           ;;("has-location" (lift-plume-location '("getPlumePolygonInRadiusReturn/coordinates")))
                           ;;if the :AS-LIST keyword by default, but can also use :AS-VALUES
                           (t
                            (let* ((res ())
                                   (extr-keyword (result-keyword-extract (second expr)))
                                   (keyword (if extr-keyword
                                                extr-keyword
                                                :as-list)))
                              (loop for el in (x-xp (first (path-extract (second expr))))
                                 do (setf res (append res (list (eval (path-extract2 (second expr) (list (element-to-string el))))))))
                              (cond ((eq keyword :as-list)  (list res))
                                    ((eq keyword :as-values) res)))))))
              (o-set-slot slot value)))
      *ocml-ctx-instance*)))

(defun ocml::lift-instance-name ()
  (symbol-name *ocml-ctx-instance*))

(defmacro ocml::deflift
    (name class (&key (web-services '())
                      (ontology '(ocml::name ocml::*current-ontology*)))
     &body exprs)
  `(progn
     ,(if web-services
          `(dolist (ws ,web-services)
             (ocml::tell1 (list 'ocml::lift-defined ',class ws ',name)))
          `(ocml::tell1 '(ocml::lift-defined-all ,class ,name)))
     ,(if (or (functionp (car exprs))
              (and (listp (car exprs)) (eq (caar exprs) 'lambda)))
          `(setf (symbol-function ',name) ,(car exprs))
          `(defun ,name (xml)
             (internal-lift xml ,ontology ',class ',(first exprs))))))
