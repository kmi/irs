;;; Copyright © 2009 The Open University

(in-package #:ocml)

(in-ontology flickr)

(def-relation #_new (?name ?type ?slots)
    :lisp-fun
    #'(lambda (?name ?type ?slots env)
	(if (unbound-variable? ?name env)
	    (let* ((type (lookup-or-self ?type env))
		   (slots (lookup-or-self ?slots env))
		   (name (gentemp "INSTANCE"))
		   (class (get-domain-class type))
		   (new (new-domain-instance-internal name type class "" slots))
		   (newenv (unify ?name name env)))
	      (if newenv
		  (list newenv)
		  :fail))
	    :fail)))