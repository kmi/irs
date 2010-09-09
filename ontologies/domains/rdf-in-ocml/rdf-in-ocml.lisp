;;; Copyright © 2009 The Open University

(in-package #:ocml)

(in-ontology rdf-in-ocml)

(def-function #_newRdfStore ()
   "Return a new #_RdfStore, for storing RDF..."
  :lisp-fun (lambda ()
	      (make-instance 'porky.io.turtle:turtle-db)))

(def-class #_RdfStore () ?thing
    :lisp-fun (lambda (x env)
		(if (typep (instantiate x env) 'wilbur:db)
		    (list env)
		    :fail)))

(def-relation #_loadRdf (?store ?url)
   "Add triples found at ?URL to ?STORE."
  :lisp-fun (lambda (store url env)
	      (#_load-rdf (instantiate store env)
			  (instantiate url env)
			  :format :xml)
	      (list env)))

(def-relation #_loadRdfFormat (?store ?url ?format)
   "Add to ?STORE the triples found at ?URL, in format ?FORMAT."
  :lisp-fun
  (lambda (?store ?url ?format env)
    (#_load-rdf (instantiate ?store env)
		(instantiate ?url env)
		:format (intern (symbol-name (instantiate ?format env))
				:keyword))
    (list env)))

(def-relation #_triples (?store ?triples)
   "?TRIPLES---in the form (?subject ?predicate ?object)---is the set
of all triples in ?STORE."
  :lisp-fun (lambda (?store ?triples env)
	      (let* ((store (instantiate ?store env))
		     (ts (loop for trip in (wilbur:db-triples store)
			       collect (list (wilbur:triple-subject trip)
					     (wilbur:triple-predicate trip)
					     (wilbur:triple-object trip))))
		     (unification (unify ?triples ts env)))
		(if unification
		    (list unification)
		    :fail))))

(def-rule #_triple
    "Holds if a triple (?SUBJECT ?PREDICATE ?OBJECT) is in ?STORE."
    ((#_triple ?store ?subject ?predicate ?object) if
     (#_triples ?store ?triples)
     (member (?subject ?predicate ?object) ?triples)))

(def-class #_Node () ?thing
    :lisp-fun (lambda (x env)
		(if (eq 'wilbur:node (type-of (instantiate x env)))
		    (list env)
		    :fail)))

(def-class #_Literal () ?thing
    :lisp-fun (lambda (x env)
		(if (eq 'wilbur:literal (type-of (instantiate x env)))
		    (list env)
		    :fail)))

(def-function #_literalString (literal)
  :lisp-fun #'wilbur:literal-string)

(def-function #_literalDatatype (literal)
  :lisp-fun #'wilbur::literal-datatype)

(def-function #_literalValue (literal)
  :lisp-fun #'wilbur::literal-value)

(def-function #_literalLanguage (literal)
  :lisp-fun #'wilbur:literal-language)
