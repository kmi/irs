;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology akt-support-ontology2)

;;;Here we introduce a number of definitions, which provide
;;;the basic representational layer to define entities in the ontology.
;;;Here we include basic data types, such 
;;;as strings, lists, sets and numbers, as well as basic logical concepts, such 
;;;as FUNCTION and RELATION.  It also provides equality constructs and a meta-level
;;;relation HOLDS, which takes a rel, say ?rel, and a number of args, say ?args
;;;and it is satisfied iff ?rel is satisfied by ?args.
;;;The advantage of expliciting including here the representational layer
;;;for the set of AKT ontologies is that these become completely self-contained:
;;;all the notions required to specify any concept in the ontology are themselves
;;;to be found in the ontologies

;;;BASIC UNIFICATION MECHANISMS


;;;RELATION = 
(def-relation = (?x ?y)
   "True if ?x and ?y do unify"
   :lisp-fun #'(lambda ( x y env)
                 (Let ((result (unify x y env)))
                   (if (eq result :fail)
                       :fail
                       (List result)))))

;;;RELATION ==
(def-relation == (?x ?y)
   "True if ?x and ?y do unify and they also have the same structure.
    This means that either they are both atoms, or they are lists with 
    the same structure"
   :lisp-fun #'(lambda ( x y env)
                 (Let ((result (unify-strong x y env)))
                   (if (eq result :fail)
                       :fail
                       (List result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-class LIST (Intangible-Thing) ?x
   "A  class representing lists."
   :iff-def (or (= ?x nil)
                (= ?x (?a . ?b)))
   :prove-by (or (= ?x nil)
                (and (variable-bound ?x)
                     (= ?x (?a . ?b))))
   :no-proofs-by (:iff-def))


(def-instance NIL list
  "The empty list")

(def-relation NULL (?l)
   "True if ?l is the empty list"
   :iff-def (= ?l nil))

(def-function FIRST (?l)
   "Takes the first element of a list.  If the list is empty
    the function returns :nothing"
  :constraint (list ?l)
  :body (if (== ?l (?a . ?b))
            ?a
            :nothing))

(def-function REST (?l)
   "Takes a list as argument, say ?l, removes the first element of ?s
    and returns the resulting list.  If ?l = nil, then :nothing is returned"
  :constraint (list ?l)
  :body (if (== ?l (?a . ?b))
            ?b
            :nothing))

(def-function LIST-OF (&rest ?els)
   "This is the primitive list constructor.  It is implemented in terms of
    the underlying LISP list construction primitive, LIST"
   :lisp-fun #'(lambda (&rest els)
                 (apply #'list els)))

(def-function APPEND (?l1 &rest ?ls)
  "Appends together a number of lists. I cannot be bothered giving its operational
   spec...so you only get a lisp attachment"
  :constraint (and (list ?l1)(every  ?ls list))
   :lisp-fun #'append)


(def-function CONS (?el ?l)
  "Adds ?el to the beginning of list ?l.  
   Note that this cons is less generic than the lisp function with the same name.
   Here the 2nd argument must be a list (in lisp, it can also be an atom)."
  :constraint (list ?l)
  :body (append (list-of ?el)
                ?l))


(def-function LENGTH (?l)
  "Computes the number of elements in a list"
  :constraint (list ?l)
  :body (if (= ?l nil)
          0
          (if (== ?l (?first . ?rest))
            (+ 1
               (length ?rest)))))

(def-function REMOVE1 (?el ?l)
  "Removes the first occurrence of ?el in ?l and returns the resulting list.
   If ?el is not a member of ?l then the result is ?l"
  :constraint (list ?l)
  :body (if (= ?l nil)
          ?l
          (if (== ?l (?el . ?rest))
            ?rest
            (if (== ?l (?first . ?rest))
              (cons ?first
                    (remove1 ?el ?rest))))))

(def-function REMOVE (?el ?l)
  "Removes all occurrences of ?el in ?l and returns the resulting list.
   If ?el is not a member of ?l then the result is ?l"
  :constraint (list ?l)
  :body (if (= ?l nil)
          ?l
          (if (== ?l (?el . ?rest))
            (remove ?el ?rest)
            (if (== ?l (?first . ?rest))
              (cons ?first
                    (remove ?el ?rest))))))
          
          


(def-relation MEMBER (?el ?list)
  "A relation to check whether something is a member of a list"
  :constraint (list ?list)
  :iff-def (or (== ?list (?el . ?rest))
               (and (== ?list (?first . ?rest))
                    (member ?el ?rest))))

(def-relation EVERY (?l ?rel)
  "True if for each term in ?l, say ?term, (holds ?rel ?term) is true.
   For instance, (every '(1 2 3) 'number) is satisfied, while 
   (every '(1 2 pippo) 'number) is not"
  :constraint (unary-relation ?rel)
  :iff-def (or (= ?l nil)
               (and (== ?l (?first . ?rest))
                    (holds ?rel ?first)
                    (every ?rest ?rel))))

;;;FUNCTION BUTLAST
(def-function BUTLAST (?list)
   "Returns all the element of ?list, except the last one.
    If ?list = NIL, then :nothing is returned.  If ?list has
    length 1, then nil is returned"
   :constraint (list ?l)
   :body (cond ((null ?list) :nothing)
               ((null (rest ?list)) nil)
               ((true)
                (cons (first ?list) (butlast (rest ?list))))))


;;;FUNCTION LAST
(def-function LAST (?list)
   "Returns the last element of a list.  If ?list is empty
    then :nothing is returned"
  :constraint (list ?list)
  :body (cond ((null ?list) :nothing)
              ((null (rest ?list)) (first ?list))
              ((true) (last (rest ?list)))))


;;;;;;;;;;;;;;;;;;;;;;;;


;;;SET
(def-class SET (Intangible-Thing) 
  "A set is something which is not an individual. In cyc sets are distinguished
   from collections.  Here we just use the term generically to refer to 
   something which denotes a collection of elements, whether abstract or
   concrete.
   Functions and relations represent sets. ")

(def-axiom BASIC-SET-TYPES-ARE-DISJOINT
  "There are three basic types of sets in our ontology, functions
   relations and enumerated sets and these do not intersect"
  (subclass-partition set (set-of function relation enumerated-set)))


(def-function SET-OF (&rest ?args)
  "This is the basic set constructor to create a set by enumerating its elements.
   For instance, (setof 1 2) denotes the set {1 2}.
   We represent such a set as a list whose first item is :set"
  :body (cons :set ?args))


(def-class ENUMERATED-SET (set) ?x
  "A set represented as (:set-of el1 el_2...el_n), where no el_i is repeated"
  :constraint (list ?x)
  :iff-def (and (= ?x (:set . ?elements))
                (not (exists ?el
                             (and (member ?el ?elements)
                                  (member ?el (remove1 ?el ?elements))))))
  :prove-by (and (variable-bound ?x)
                 (= ?x (:set . ?elements))
                 (not (exists ?el
                              (and (member ?el ?elements)
                                   (member ?el (remove1 ?el ?elements))))))
  :no-proofs-by (:iff-def))

(def-relation ELEMENT-OF (?el ?set)
  "A relation to check whether something is an element of a set.  
   Note that because functions and relations are sets, we can prove
   whether a tuple satisfies a relation or a function using ELEMENT-OF.
   For instance, (element-of '(1 2 3) '+) is satisfied because 
   1+2=3 - see definitions below for an operationalization of this approach")

(def-rule ELEMENT-OF-ENUMERATED-SET
  "An operationalization of element-of, which works with sets represented as lists."
  ((element-of ?el ?l) if
   (enumerated-set ?l)
   (member ?el (rest ?l))))

(def-rule ELEMENT-OF-SET-AS-RELATION
  "A tuple, say <t1 t2 t3> is an element of relation r iff
   (holds r ti t2 t3) is satisfied"
  ((element-of ?tuple ?rel) if
   (relation ?rel)
   (List ?tuple)
   (sentence-holds (cons ?rel ?tuple))))

(def-rule ELEMENT-OF-SET-AS-FUNCTION
  "A tuple, say <t1 t2 t3> is an element of function f iff
   (= (apply f ti t2) t3) is satisfied"
  ((element-of ?el ?fun) if
   (function ?fun)
   (list ?el)
   (= (last ?el) (apply ?fun (butlast ?el)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;NUMBER
(def-class NUMBER (Quantity)
  "The class of all numbers"
  :lisp-fun  #'(lambda (x env)
                 (let ((y (unbound-variable? x env)))
                   (if y ;;if y is unbound we return a 'sample' number
                     (list (cons (cons y 0) env))
                     (if (numberp (instantiate x env)) ;;;make sure to instantiate x
                       (list env)
                       :fail)))))

(def-class REAL-NUMBER (number))

(def-class INTEGER (real-number))
   

;;; RELATION <
(def-relation < (?x ?y)
   "A predicate to test whether a number is less than another"
   :constraint (and (number ?x)(number ?y))
   :lisp-fun #'(lambda (x y env)
                 (if (< (instantiate x env)
                        (instantiate y env))
		     (List env)
		     :fail)))
;;;RELATION >
(def-relation > (?x ?y)
   "A predicate to test whether a number is greater than another"
   :constraint (and (number ?x)(number ?y))
   :lisp-fun #'(lambda (x y env)
                 (if (> (instantiate x env)
                        (instantiate y env))
		     (List env)
		     :fail)))


(def-class POSITIVE-NUMBER (number) ?x
  :iff-def (and (number ?x)
                (> ?x 0)))

(def-class NEGATIVE-NUMBER (number) ?x
  :iff-def (and (number ?x)
                (< ?x 0)))

(def-class NON-NEGATIVE-NUMBER (number) ?x
   :iff-def (and (number ?x)
                 (not (negative-number ?x))))
  

(def-axiom POS-NEG-NUMBERS-EXHAUSTIVE-PARTITION
  (exhaustive-subclass-partition number (set-of positive-number negative-number)))

           
(def-class POSITIVE-INTEGER (integer) ?x
  :iff-def (and (integer ?x)
                (> ?x 0)))

(def-class NEGATIVE-INTEGER (integer) ?x
  :iff-def (and (integer ?x)
                (< ?x 0)))

(def-class NON-NEGATIVE-INTEGER (integer) ?x
  :iff-def (and (integer ?x)
                (not (negative-integer ?x))))


(def-function + (?X &rest ?y)
  "Adds numbers"
  :constraint (and (number ?x) (number ?y))
  :lisp-fun #'+)

(def-function * (?X ?y)
  "Multiplies numbers"
  :constraint (and (number ?x) (number ?y))
   :lisp-fun #'*)

(def-function SQUARE (?X)
  :constraint  (number ?x)  
  :body (* ?X ?X))

(def-function - (?X &rest ?y)
  "Subtracts numbers"
  :constraint (and (number ?x) (number ?y))
  :lisp-fun #'(lambda (x &rest y)
                (apply #'- x y)))

(def-function / (?X ?y)
  "Divides numbers"
  :constraint (and (number ?x) (number ?y))
  :lisp-fun #'/)

;;;;;;;;;;;;;;;;;;;;;

(def-class STRING (individual intangible-thing)
   "A primitive class representing strings"
   :lisp-fun  #'(lambda (x env)
                  (let ((y (unbound-variable? x env)))
                   (if y
                     (list (cons (cons y "SAMPLE-STRING") env))
                     (if (stringp (instantiate x env)) 
                       (list env)
                       :fail)))))

;;;;;;;;;;;;;;;;;;;;;

(def-class RELATION (set) 
  "The class of defined relations.  We assume fixed arity"
  :lisp-fun #'(lambda (x env)
                 (let ((y (unbound-variable? x env)))
                   (if y
                     (mapcar #'(lambda (rel)
                                   (cons (cons y rel) env))
                             (all-relations))
                     (if (get-relation (instantiate x env)) ;;;make sure to instantiate x
                       (list env)
                       :fail)))))
  

(def-class UNARY-RELATION (relation) ?r
  :iff-def (and (relation ?r)
                (= (arity ?r) 1)))
                

(def-class BINARY-RELATION (relation) ?r
  :iff-def (and (relation ?r)
                (= (arity ?r) 2)))

(def-relation TRUE ()
  "This is always satisfied"
   :lisp-fun #'(lambda (env) (list env)))

(def-relation FALSE ()
  "This is never satisfied"
   :lisp-fun #'(lambda (env) :fail))


(def-function ARITY (?x)
  "The arity of a function or relation.  If a function or relation
   has variable arity, then we treat the last argument as if it were
   a sequence variable.  For instance the argument list for + is
   (?x &rest ?y).  In this case we say that + has arity 2.
   It is important to note that OCML does not support relations with variable
   arity.  The only exception is HOLDS, which has variable arity and is built in
   the OCML proof system"

  :constraint (or (function ?X)(relation ?X))
  :body  (in-environment 
          ((?l . (the-schema ?x))
           (?n . (length ?l)))
          (if (every ?l variable)
            ?n
            ;we assume that the only non-var can be &rest
            (- ?n 1))))

(def-relation VARIABLE (?x) 
  "True of an OCML variable"
  :lisp-fun #'(lambda (x env)
                (if
                  (variable? (instantiate x env))
                  (list env)
                  :fail)))

(def-relation VARIABLE-BOUND (?var)
  "True if ?var is  bound in teh current environment"
  :lisp-fun #'(lambda (x env)
                (if
                  (unbound-variable? x env)
                  :fail
                  (list env))))
  
(def-function THE-SCHEMA (?x)
  "The schema of a function or relation"
  :constraint (or (function ?X)(relation ?X))
  :body (if (relation ?x)
          (relation-schema ?x)
          (if (function ?x)
            (function-schema ?x)
            :nothing)))

(def-function FUNCTION-SCHEMA (?f)
  :constraint (function ?f)
   :lisp-fun #'(lambda (x)
                  (rename-variables (schema (get-function x)))))

(def-function RELATION-SCHEMA (?rel)
  :constraint (relation ?rel)
  :lisp-fun #'(lambda (x)
                 (rename-variables (schema (get-relation x)))))
  


(def-relation HOLDS (?rel &rest ?args)
  "A meta-level relation which is true iff the sentence (?rel . ?args) is true.
   The length of ?args must be consistent with the arity of the relation"
  :constraint (and (relation ?r)
                   (= (arity ?rel)
                      (length ?args))))

(def-relation SENTENCE-HOLDS (?sentence)
  "The same as HOLDS, but takes only one argument, a sentence whose truth
   value is to be checked"
  :constraint (and (== ?sentence (?rel . ?args ))
                   (relation ?rel)
                   (= (arity ?rel)
                      (length ?args)))
  :lisp-fun #'(lambda (sent env)
                (ask-top-level 
                 (cons 'holds (instantiate sent env)) 
                 :env env
                 :all t)))
                
  
                
(def-class FUNCTION (set)  
  "The class of all defined functions"
  :lisp-fun #'(lambda (x env)
                 (let ((y (unbound-variable? x env)))
                   (if y
                     (mapcar #'(lambda (rel)
                                   (cons (cons y rel) env))
                             (all-functions))
                     (if (ocml-function? 
                          (instantiate x env)) 
                       (list env)
                       :fail)))))




(def-function APPLY (?f ?args)
  "(apply f (arg1 .....argn)) is the same as 
   (f arg1 ....argn)"
  :constraint (and (function ?f)
                   (list ?args)))
  
  