;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology akt-support-ontology2)

;;;here I have defined a few basic notions we need to have to talk about frame-based models

(def-class CLASS (unary-relation) 
  "The class of all classes.  We consider a class as a unary relation, true for all its instances"
  :lisp-fun  #'(lambda (x env)
               (let ((y (unbound-variable? x env)))
                   (if y
                     (mapcar #'(lambda (rel)
                                 (cons (cons y rel) env))
                             (all-ocml-classes))
                     (if (get-ocml-class 
                          (instantiate x env))
                       (list env)
                       :fail)))))

(def-class CLASS-PARTITION (enumerated-set) ?set-of-classes
  "A set of mutually disjoint classes.  Disjointness of
   classes is a special case of disjointness of sets."
  :constraint (enumerated-set ?set-of-classes)
  :iff-def (and (enumerated-set ?set-of-classes)
                (forall ?C
                        (=> (element-of ?C ?set-of-classes)
                            (class ?C)))
                (forall (?C1 ?C2)
                        (=> (and (element-of ?C1 ?set-of-classes)
                                 (element-of ?C2 ?set-of-classes)
                                 (not (= ?C1 ?C2)))
                            (forall (?i)
                                    (=> (instance-of ?i ?C1)
                                        (not (instance-of ?i ?C2)))))))
  :avoid-infinite-loop t)


(def-relation SUBCLASS-PARTITION (?C ?class-partition)
   "A subclass-partition of a class C is a set of
    subclasses of C that are mutually disjoint."
   :iff-def (and (class ?C)
                 (class-partition ?class-partition)
                 (forall ?subclass
                         (=> (element-of ?subclass ?class-partition)
                             (subclass-of ?subclass ?C)))))

(def-relation EXHAUSTIVE-SUBCLASS-PARTITION (?C ?class-partition)
   "A subrelation-partition of a class C is a set of
    mutually-disjoint classes (a subclass partition) which covers C.
    Every instance of C is is an instance of exactly one of the subclasses
    in the partition."
   :iff-def (and (subclass-partition ?C ?class-partition)
                 (forall ?instance
                         (=> (instance-of ?instance ?C)
                             (exists ?subclass
                                     (and (element-of ?subclass ?class-partition)
                                          (instance-of ?instance ?subclass)))))))

(def-relation INSTANCE-OF (?x ?c)
  "This definition relates the notion of 'being an instance' to the notion
   of satisfying a relation: ?I is an instance of a class ?c, 
   iff (holds ?I ?c) is true"
   :constraint (class ?c)
   :iff-def (and (class ?c)
                 (holds ?c ?x)))

(def-relation SUBCLASS-OF (?sub ?c)
  "?sub is a subclass of ?c if every instance of ?sub is also an instance of 
   ?c.  Note that according to this definition every class is a subclass of itself"

  :constraint (and (class ?sub)(class ?c))
  
  :prove-by (or (and (variable-bound ?sub)
                      (member ?c (all-superclasses ?sub)))
                 (and (variable-bound ?c)
                      (member ?sub (all-subclasses ?c)))
                 (and 
                      (not (variable-bound ?c))
                      (not (variable-bound ?sub))
                      (class ?sub1)(class ?super1)
                      (subclass-of ?sub1 ?super1)))

  :iff-def (and (class ?sub) (class ?c)
                (forall ?inst
                        (=> (instance-of ?inst ?sub)
                            (instance-of ?inst ?c))))
  :no-proofs-by (:iff-def))

(def-function ALL-SUPERCLASSES (?class) -> ?supers
  "returns all superclasses of a class"
  :constraint (class ?class)
  :def (forall ?super (<=> (member ?super ?supers)
                           (subclass-of ?class ?super)))
  :lisp-fun  #'(lambda (class)
                 (let ((class-s (get-ocml-class class)))
                   (if class-s
                     (mapcar #'name (domain-superclasses class-s))))))

(def-function ALL-SUBCLASSES (?class) -> ?subs
    "returns all subclasses of a class"
  :constraint (class ?class)
   :def (forall ?sub (<=> (member ?sub ?subs)
                           (subclass-of ?sub ?class)))
   :lisp-fun  #'(lambda (class)
                  (let ((class-s (get-ocml-class class)))
                   (if class-s
                     (mapcar #'name (current-subclasses class-s))))))




                
