;;; Copyright © 2007,2008 The Open University

(in-package #:irs.api.javascript)

(defun initialise ()
  (run-keep-alive-thread)
  (irs.web:register-plugin
   :api-javascript :api
   "API for Javascript clients."
   (nconc (mapcar (lambda (args)
                    (apply #'hunchentoot:create-regex-dispatcher args))
                  '(("/api-javascript/events/services$" services-info)
                    ("/api-javascript/events/events-info$" events-info)
                    ("/api-javascript/goal/slots" send-goal-slots-json)
                    ("/api-javascript/graph/data" send-graph-data)
                    ("/api-javascript/class$" send-class-data)
                    ("/api-javascript/class-tree$" send-class-tree-data)
                    ("/api-javascript/function$" send-function-data)
                    ("/api-javascript/function-list$" send-function-list-data)
                    ("/api-javascript/instance$" send-instance-data)
                    ("/api-javascript/instance-list$" send-instance-list-data)
                    ("/api-javascript/ontology-list$" send-ontology-list-data)
                    ("/api-javascript/ontology$" send-ontology-data)
                    ("/api-javascript/relation$" send-relation-data)
                    ("/api-javascript/relation-list$" send-relation-list-data)
                    ("/api-javascript/rule$" send-rule-data)
                    ("/api-javascript/rule-list$" send-rule-list-data)
                    ("/api-javascript/wsmo-entities-tree" send-wsmo-tree-data))))))

(defun send-goal-slots-json ()
  (web:with-parameters (:get (ontology concept))
    (let* ((o (intern-ocml-symbol ontology))
           (c (ocml:with-ontology (o)
                (intern-ocml-symbol concept))))
      (ocml:with-ontology (o)
        (let* ((roles (wp::input-roles c))
               (json (with-output-to-string (s)
                       (format s "[")
                       (dolist (role roles)
                         (format s "{name: \"~(~A~)\", ocmltype: \"~(~A~)\"}," role
                                 (wsmo-protocol::get-input-role-type c role)))
                       (format s "]"))))
          (irs.web:write-http-stream "text/javascript" json))))))

(defun send-graph-data ()
  (web:with-parameters (:get (ontology concept))
    (graph-json (intern (string-upcase ontology) :ocml)
                (intern (string-upcase concept) :ocml))))

(defun graph-json (ontology concept)
  (web:write-http-stream "text/plain" (raw-graph-json ontology concept)))

(defun raw-graph-json (ontology concept)
  (let ((webonto::*number-of-tree-items* 0))
    (ocml:with-ontology (ontology)
      (list-to-json-array
              (webonto::js-tree-draw-classes (ocml::get-domain-class concept) nil nil :vertical)))))


(defun list-to-json-array (list)
  (with-output-to-string (str)
    (format str "[")
    (dolist (thing list)
          (format str "~A," thing ))
    (format str "]")))

(defun filters-as-ontologies (ontology filters)
  "Return the list of ontology structures to filter."
  (let* ((base-ontology (ocml::get-ontology 'ocml::base-ontology))
         (wsmo-ontology (ocml::get-ontology 'ocml::wsmo))
         (imported (if (first filters)
                       (remove base-ontology
                               (remove wsmo-ontology
                                       (ocml::get-included-ontologies ontology)))))
         (base (if (second filters)
                   (list base-ontology)))
         (wsmo (if (third filters)
                   (list wsmo-ontology))))
    (funcall #'append base wsmo imported)))

;;; {{{ Function list
(defun send-function-list-data ()
  (web:with-parameters (:get (ontology filters))
    (web:write-http-stream
     "text/plain" (function-list-json (intern-ocml-symbol ontology)
                                      (read-filters filters)))))

(defun function-list-json (ontology filters)
  (ocml::with-ontology (ontology)
  ;new mapcar functionality added by Neil B. so that home-ontology info can be returned with class tree (to make client-side filtering in IRS Editor possible)
    (list-to-json-array (mapcar #'(lambda (func-home-pair)
                                    (format nil "{name: ~S, homeOntology: ~S}" 
											(ocml::extern-ocml-symbol (first func-home-pair)) 
											(ocml::extern-ocml-symbol (second func-home-pair))))
                                (function-list ontology filters)))))

(defun function-list (ontology filters)
  (let ((home-ontologies-to-zap (filters-as-ontologies ontology filters)))
    (ocml:with-ontology (ontology)
      (sort (mapcar ;new mapcar functionality added by Neil B. so that home-ontology info can be returned with class tree (to make client-side filtering in IRS Editor possible)
					#'(lambda (r)
						(list (ocml::name r) (ocml::name (ocml::home-ontology r))))
                    (remove-if (lambda (r)
                                 (member (ocml::home-ontology r)
                                         home-ontologies-to-zap))
                               (ocml::all-functions t)))
			;new :key parameter added by Neil B. so that home-ontology info can be returned with class tree (to make client-side filtering in IRS Editor possible)				   
            #'string-lessp :key #'first))))
;;; }}}
;;; {{{ Ontology list
(defun send-ontology-list-data ()
  (web:write-http-stream "text/plain" (ontology-list-json)))

(defun ontology-list-json ()
  (list-to-json-array (mapcar #'(lambda (ont)
                                  (format nil "{name: ~S}" (extern-ocml-symbol ont)))
                              (ocml::sorted-ontologies))))
;;; }}}
;;; {{{ Relation list
(defun send-relation-list-data ()
  (web:with-parameters (:get (ontology filters))
    (web:write-http-stream
     "text/plain" (relation-list-json (intern-ocml-symbol ontology)
                                      (read-filters filters)))))

(defun relation-list-json (ontology filters)
  (ocml:with-ontology (ontology)
  ;new mapcar functionality added by Neil B. so that home-ontology info can be returned with class tree (to make client-side filtering in IRS Editor possible)
    (list-to-json-array (mapcar #'(lambda (rel-home-pair)
                                    (format nil "{name: ~S, homeOntology: ~S}" 
											(ocml::extern-ocml-symbol (first rel-home-pair)) 
											(ocml::extern-ocml-symbol (second rel-home-pair))))
                                (relation-list ontology filters)))))

(defun relation-list (ontology filters)
  (let ((home-ontologies-to-zap (filters-as-ontologies ontology filters)))
    (ocml:with-ontology (ontology)
      (sort (mapcar ;new mapcar functionality added by Neil B. so that home-ontology info can be returned with class tree (to make client-side filtering in IRS Editor possible)
					#'(lambda (r)
						(list (ocml::name r) (ocml::name (ocml::home-ontology r))))
                    (remove-if (lambda (r)
                                 (member (ocml::home-ontology r)
                                         home-ontologies-to-zap))
                               (ocml::all-relations t)))
			;new :key parameter added by Neil B. so that home-ontology info can be returned with class tree (to make client-side filtering in IRS Editor possible)				   
            #'string-lessp :key #'first))))
;;; }}}
;;; {{{ Rule list
(defun send-rule-list-data ()
  (web:with-parameters (:get (ontology filters))
    (web:write-http-stream
     "text/plain" (serialise-json
                   (rule-list-json* ontology (read-filters filters))))))

;;; XXX How do we deal with filtering of rules?
(defun rule-list-json* (ontology filters)
  (declare (ignore filters))
  (let* ((o (intern-ocml-symbol ontology)))
    (ocml:with-ontology (o)
      (vectorise (mapcar (lambda (name)
                           (js-object "name" (extern-ocml-symbol name)
				      "homeOntology" (extern-ocml-symbol (ocml::name (ocml::home-ontology (ocml::get-rule (ocml::intern-ocml-symbol name)))))))
                         (ocml::list-hash-table ocml::*bc-rules*))))))
;;; }}}
;;; {{{ WSMO entities tree
(defun send-wsmo-tree-data ()
  "Send Javascript JSON about the WSMO entities."
  (web:with-parameters (:get (ontology filters))
    (web:write-http-stream
     "text/plain" (wsmo-tree-json (intern-ocml-symbol ontology)
                                  (read-filters filters)))))

(defun subs-we-want (superclass-name ignored)
  "Return names of subclasses of SUPERCLASS-NAME that are not homed in IGNORED."
  (let* ((superclass (ocml::get-ocml-class superclass-name))
         (all-subclasses (and superclass (ocml::current-subclasses superclass))))
    (remove-duplicates (mapcar (compose #'ocml:extern-ocml-symbol #'ocml::name)
             (remove-if (lambda (subclass)
                          (member (ocml::home-ontology subclass) ignored))
                        all-subclasses)))))

(defun all-instances-in-this-ontology (ignored)
  "All instances in this ontology."
  (let ((classes (ocml::all-ocml-classes t)))
    (mapcar (lambda (instance)
	      (list "name" (ocml::extern-ocml-symbol (ocml::name instance))
		    "homeOntology" (ocml::extern-ocml-symbol (ocml::name (ocml::home-ontology instance)))))
            (remove-duplicates
             (remove-if (lambda (instance)
                          (member (ocml::home-ontology instance) ignored))
                        (mapcan (lambda (c)
                                  (ocml::get-current-direct-instances c))
                                classes))))))

(defun wsmo-tree-json (ontology filters)
  ;; ONTOLOGY is a symbol.
  (let ((ignored (filters-as-ontologies ontology filters)))
    (ocml:with-ontology (ontology)
      (let* ((goals (sort (subs-we-want 'ocml::goal ignored) #'string<))
             (services (sort (subs-we-want 'ocml::web-service ignored) #'string<))
             (mediators (sort (subs-we-want 'ocml::mediator ignored) #'string<))
             (concepts (sort (set-difference (subs-we-want 'ocml::ocml-thing ignored)
                                             (append goals services mediators))
                             #'string<))
             (instances (sort (all-instances-in-this-ontology ignored)
                         #'string< :key #'second))) ;;Added a :key parameter to the sort in order to accommodate new instance data returned by 'all-instances-in-this-ontology'
        (with-output-to-string (str)
          (labels ((oot (objects label)
                     (format str "~A: [" label)
                     (dolist (obj objects)
					 ;amended by Neil B. so that homeOntology info can be returned with instance list
						(if (string= label "instances")
							(format str "{name: '~A', homeOntology: '~A'}," (second obj) (fourth obj))
                          (format str "{name: '~A', homeOntology: '~A'}, " obj (ocml::extern-ocml-symbol (ocml::name (ocml::home-ontology (ocml::get-ocml-class (ocml::intern-ocml-symbol obj))))))))
                     (format str "],")))
            (format str "[{")
            (oot goals "goals")
            (oot services "services")
            (oot mediators "mediators")
            (oot concepts "concepts")
            (oot instances "instances")
            (format str "}]")))))))

;;; }}}
;;; {{{ Instance list
(defun send-instance-list-data ()
  (web:with-parameters (:get (ontology filters))
    (web:write-http-stream
     "text/plain" (serialise-json
                   (instance-list-json* ontology (read-filters filters))))))

(defun instance-list-json* (ontology filters)
  (let* ((o (intern-ocml-symbol ontology))
         (ignored (filters-as-ontologies o filters)))
    (ocml:with-ontology (o)
      (coerce
       (mapcar (lambda (o)
		 (apply #'irs.api.javascript::js-object o))
	       (all-instances-in-this-ontology ignored))
       'vector))))

;;; }}}
;;; {{{ Class tree
(defun read-filters (string)
  (with-input-from-string (stream (substitute #\space #\comma string))
    (let ((*package* (find-package :keyword)))
      (loop for n from 1 to 3
         collect (not (eq (read stream) :false))))))

(defun send-class-tree-data ()
  (web:with-parameters (:get (ontology filters))
    (web:write-http-stream
     "text/plain" (class-tree-to-json
                   (class-tree (intern ontology :ocml) (read-filters filters))))))

(defun class-tree-to-json (tree)
  (labels ((forest (forest)
             (format t "[")
             (dolist (node forest)
               (node node)
               (format t ","))
             (format t "]"))
           (node (node)
		   ;;homeOntology property added by Neil B. so that home-ontology info can be returned with class tree to enable client-side filtering in IRS Editor
             (format t "{name: ~S, homeOntology: ~S, children: " (first node) (second node)) 
             (forest (third node))
             (format t "}")))
    (with-output-to-string (str)
      (let ((*standard-output* str))
        (forest tree)))))

(defun class-tree (ontology filters)
  "Build a tree (actually a forest!) of classes and their subclasses."
  (let* ((included-stuff (irs.browser::figure-out-what-to-include ontology filters))
	 (ontologies (append included-stuff (list (ocml::get-ontology ontology))))
	 (topclasses (apply #'append
			    (mapcar #'(lambda (ont)
					(ocml:with-ontology (ont)
					  (sort (ocml::local-top-classes (ocml::name ont))
						#'string-lessp)))
				    ontologies))))
    (labels ((calctree (class)
               (let ((classname (ocml::name class))
                     (subclasses (sort (ocml::current-direct-subclasses class)
                                       #'string-lessp :key #'ocml::name)))
                 (setf topclasses (delete classname topclasses))
                 (cons (ocml::extern-ocml-symbol classname) 
				 ;new line added by Neil B. so that home-ontology info can be returned with class tree (to make client-side filtering in IRS Editor possible)
					(cons (ocml::extern-ocml-symbol (ocml::name (ocml::home-ontology class))) 
                       (list (mapcar #'calctree subclasses))))))
             (iter ()
               (if topclasses
                   (let ((res (calctree (ocml::get-ocml-class (first topclasses)))))
                     (cons res (iter)))
                   '())))
      (ocml:with-ontology (ontology)
        (sort (iter) #'string-lessp :key #'first)))))

;;; }}}
;;; {{{ Class introspection
(defun send-class-data ()
  (web:with-parameters (:get (ontology class))
    (let* ((o (intern-ocml-symbol ontology))
           (c (ocml:with-ontology (o)
                (intern-ocml-symbol class))))
      (irs.web:write-http-stream
       "text/javascript" (serialise-json (class-json* o c))))))

(defun slot-json* (class slot)
  (js-object
   "name" (extern-ocml-symbol slot)
   "type" (extern-ocml-symbol (first (ocml::find-option-value class slot :type)))
   "cardinalityMin" (ocml::find-option-value class slot :min-cardinality)
   "cardinalityMax" (ocml::find-option-value class slot :max-cardinality)
   "documentation" (ocml::find-slot-documentation class slot)
   "values" (vectorise (mapcar #'value-mapper
                               (ocml::get-slot-values-from-class-structure class slot)))))

(defun class-json* (ontology classname)
  (ocml:with-ontology (ontology)
    (let* ((class (ocml::get-domain-class classname))
           (slots (mapcar (lambda (slotname)
                            (slot-json* class slotname))
                          (ocml::domain-slots class))))
      (js-object
       "name" (extern-ocml-symbol (ocml::name class))
            "superClasses" (vectorise (mapcar (compose #'ocml:extern-ocml-symbol
                                                       #'ocml::name)
                                              (ocml::domain-superclasses class)))
            "subClasses" (vectorise (mapcar (compose #'ocml:extern-ocml-symbol
                                                       #'ocml::name)
                                            (ocml::current-subclasses class)))
            "homeOntology" (ocml::name (ocml::home-ontology class))
            "documentation" (ocml::ocml-documentation class)
            "slots" (vectorise slots)
            ))))

(defun vectorise (list)
  (coerce list 'simple-vector))

;;; }}}
;;; {{{ Function introspection
(defun send-function-data ()
  (web:with-parameters (:get (ontology function))
    (let* ((o (intern-ocml-symbol ontology))
           (r (ocml:with-ontology (o)
                (intern-ocml-symbol function))))
      (irs.web:write-http-stream
       "text/javascript" (serialise-json (function-json* o r))))))

(defun function-json* (ontology function)
  (ocml:with-ontology (ontology)
    (let ((f (ocml::get-function function)))
      (js-object "name" (extern-ocml-symbol (ocml::name f))
            "homeOntology" (ocml::name (ocml::home-ontology f))
            "documentation" (ocml::ocml-documentation f)
            "schema" (vectorise (ocml::schema f))
            "constraint" (if (ocml::constraint f)
                             (format nil "~A" (ocml::constraint f)))
            "definition" (if (ocml::definition f)
                             (format nil "~A" (ocml::definition f))
                             nil)
            "body" (let ((b (ocml::body f)))
                     (if b
                         (format nil "~A" b)))
            "lispFun" (if (ocml::lisp-fun f)
                          (format nil "~A" (ocml::lisp-fun f)))))))

;;; }}}
;;; {{{ Instance introspection
(defun send-instance-data ()
  (web:with-parameters (:get (ontology instance))
    (let* ((o (intern-ocml-symbol ontology))
           (i (ocml:with-ontology (o)
                (intern-ocml-symbol instance))))
      (irs.web:write-http-stream
       "text/javascript" (serialise-json (instance-json* o i))))))

(defun instance-json* (ontology instance)
  (ocml:with-ontology (ontology)
    (let ((i (ocml::find-current-instance instance)))
      (js-object "name" (ocml:extern-ocml-symbol (ocml::name i))
            "homeOntology" (ocml::name (ocml::home-ontology i))
            "class" (extern-ocml-symbol (ocml::name (ocml::class-of i)))
            "documentation" (ocml::ocml-documentation i)
            "slots" (vectorise (instance-slot-values i))))))

(defun instance-slot-values (instance)
  (mapcar (lambda (slot)
            (js-object "name" (extern-ocml-symbol slot)
                  "values"
                  (vectorise
                   (mapcar #'value-mapper
                           (ocml::setofall '?x `(,slot ,(ocml::name instance) ?x))))))
          (ocml::domain-slots instance)))

;;; }}}
;;; {{{ Ontology introspection
(defun send-ontology-data ()
  (web:with-parameters (:get (ontology))
    (let ((o (intern-ocml-symbol ontology)))
      (irs.web:write-http-stream
       "text/javascript" (serialise-json (ontology-json* o))))))

(defun ontology-json* (ontology)
  (ocml:with-ontology (ontology)
    (let ((o ocml::*current-ontology*))
      (js-object "name" (ocml::name o)
            "documentation" (ocml::ocml-documentation o)
            "namespaceUri" (ocml::namespace-uri-of o)
            "includes" (vectorise (mapcar #'ocml::name (ocml::ontology-includes o)))
            "includedBy" (vectorise
                          (mapcar #'ocml::name (ocml::ontology-included-by o)))
            "namespacePrefixes"
            (vectorise
             (mapcar (lambda (mapping)
                       (js-object "prefix" (car mapping)
                                  "namespace" (cdr mapping)))
                     ocml::*namespace-prefixes*))
            "type" (ocml::ontology-type o)
            "author" (ocml::ontology-author o)
            "editors" (vectorise (ocml::ontology-allowed-editors o))
            "files" (vectorise (ocml::ontology-files o))))))

;;; }}}
;;; {{{ Relation introspection
(defun send-relation-data ()
  (web:with-parameters (:get (ontology relation))
    (let* ((o (intern-ocml-symbol ontology))
           (r (ocml:with-ontology (o)
                (intern-ocml-symbol relation))))
      (irs.web:write-http-stream
       "text/javascript" (serialise-json (relation-json* o r))))))

(defun relation-json* (ontology relation)
  (ocml:with-ontology (ontology)
    (let ((r (ocml::get-relation relation)))
      (js-object
       "name" (extern-ocml-symbol (ocml::name r))
            "homeOntology" (ocml::name (ocml::home-ontology r))
            "documentation" (ocml::ocml-documentation r)
            "schema" (vectorise (ocml::schema r))
            "localSlotOf" (vectorise (remove-duplicates
                                      (mapcar (compose #'extern-ocml-symbol
                                                       #'ocml::name)
                                              (ocml::local-slot-of r))))
            "slotOf" (vectorise (remove-duplicates
                                 (mapcar (compose #'extern-ocml-symbol #'ocml::name)
                                         (ocml::slot-of r))))
            "constraint" (if (ocml::constraint r)
                             (format nil "~A" (ocml::constraint r)))
            "iffDef" (let ((iffdef (ocml::iff-def r)))
                       (if iffdef
                           (format nil "~A"
                                   (if (ocml::bc-clause? iffdef)
                                       (car (ocml::bc-clause-antecedents iffdef))
                                       iffdef))
                           nil))
            "sufficient" (if (ocml::sufficient r)
                             (format nil "~A" (car (ocml::bc-clause-antecedents
                                                (ocml::sufficient r))))
                             nil)
            "associatedRules" (let ((rules (ocml::defined-by-rule r)))
                                (if rules
                                    (vectorise (mapcar (lambda (x)
                                                         (extern-ocml-symbol (ocml::name x)))
                                                       rules))
                                    (vectorise nil)))
            "proveBy" (if (ocml::prove-by r)
                          (format nil "~A" (car (ocml::bc-clause-antecedents (ocml::prove-by r)))))
            "lispFun" (if (ocml::lisp-fun r)
                          (format nil "~A" (ocml::lisp-fun r)))
            "instances" (vectorise
                         (mapcar #'ocml::name
                                 (rest (ocml::relation-instances r))))))))
;;; }}}
;;; {{{ Rule introspection
(defun send-rule-data ()
  (web:with-parameters (:get (ontology rule))
    (let* ((o (intern-ocml-symbol ontology))
          (r (ocml:with-ontology (o)
               (intern-ocml-symbol rule))))
      (irs.web:write-http-stream
       "text/javascript" (serialise-json (rule-json* o r))))))

(defun rule-json* (ontology rule)
  (ocml:with-ontology (ontology)
    (let ((r (ocml::get-rule rule)))
      (apply #'js-object
             (append
              (list "name" (extern-ocml-symbol (ocml::name r))
                    "homeOntology" (extern-ocml-symbol
                                    (ocml::name (ocml::home-ontology r)))
                    "direction" (ocml::rule-direction r)
                    "documentation" (ocml::ocml-documentation r))
              (if (eq :backward (ocml::rule-direction r))
                  (list "definesRelation"
                        (extern-ocml-symbol (ocml::defines-relation r)))))))))
;;; }}}
