;;; Mode: Lisp; Package: ocml

;; Time Ontology
;;
;; Time Ontology brings support for reasoning about time. It is based on Allen's Interval
;; Temporal Algebra and has been extended with Marc Vilain's instant-interval relations
;; and polimorphism in the relations. 
;;
;; Explained in more detail in:
;; C. Pedrinaci, J. Domingue, and A. K. Alves de Medeiros. A Core Ontology for Business 
;; Process Analysis. In 5th European Semantic Web Conference, 2008.
;;
;; Based on:
;; J. F. Allen. Maintaining knowledge about temporal intervals. Communications of the ACM, 26(11):832-843, 1983.
;;
;; M. B. Vilain. A system for reasoning about time. In AAAI, pages 197-201, 1982.
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 2.0-SNAPSHOT

;; Comments:
;; Includes a dependency with Units Manipulation for computing Durations
;; Remove Duration related definitions and the dependency can be removed

;; TODO: 
;; 2.- Fix Relations names to fit new case sensitivity?

(in-package "OCML")

(in-ontology time-ontology)

(def-class #_TemporalEntity ()
  "Like in Cyc, this is something which has a temporal extent.")

(def-class #_TimeInterval () ?int
  "A period of time, as opposed to an instant."
  ((#_hasStartTime :type #_XSD:DateTime)
   (#_hasEndTime :type #_XSD:DateTime)))

;; Breaks the reasoning since we apparently do not return the results in the proper shape
;; We need to return the env properly created. Check that out
;;  :iff-def (#_dateTimeBefore (the-slot-value ?int #_hasStartTime)
;;                             (the-slot-value ?int #_hasEndTime)))

(def-class #_InstantaneousEntity (#_TemporalEntity)
  "Something that occurs instantaneously"
  ((#_occursAt :type #_XSD:DateTime :cardinality 1)))

(def-class #_TimeSpanningEntity (#_TemporalEntity)
  "Something that occurs or holds during some period, i.e., not instantaneous"
  ((#_spansInterval :type #_TimeInterval :cardinality 1)))

;; We should add a constraint so that only those of Temporal Dimension are Duration
(def-class #_Duration (#_PHYS-Q:ConstantQuantity) ?x
  "We define Duration as a Constant Quantity (see Physical Quantities)")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;**;;;These are few useful relations for the Intervals or Temporal Entities;;;*;;;
;;;These are BASIC relations taken from Allen's ITL;;;

(defun get-time-from-instant (i)
  (if (holds? '#_InstantaneousEntity i)
      (the-slot-value i '#_occursAt)
    i))

;; Extract start time from TimeIntervals or TimeSpanning entities
(defun get-start-from-interval (itl)
  (if (holds? '#_TimeInterval itl)
      (the-slot-value itl '#_hasStartTime)
    (the-slot-value (the-slot-value itl '#_spansInterval) '#_hasStartTime)))

(defun get-end-from-interval (itl)
  (if (holds? '#_TimeInterval itl)
      (the-slot-value itl '#_hasEndTime)
    (the-slot-value (the-slot-value itl '#_spansInterval) '#_hasEndTime)))


(defun prove-before (itl1 itl2)
  (let ((itl1-s (get-start-from-interval itl1))
         (itl1-e (get-end-from-interval itl1))
         (itl2-s (get-start-from-interval itl2))
         (itl2-e (get-end-from-interval itl2)))
    
    (if (and (utilities:xsd-datetime-p itl1-s)
             (utilities:xsd-datetime-p itl1-e)
             (utilities::xsd-datetime-p itl2-s)
             (utilities::xsd-datetime-p itl2-e)
             (datetime-before itl1-e itl2-s))
        T
      NIL)))

(def-relation #_before (?itl-1 ?itl-2)
  "Primitive relation between intervals. ITL-1 is 'before' ITL2 if it ends before ITL-2 starts."
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))
  
  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-before itl1* itl2*)
                      (list env)
                    :fail))))


;; After doesn't need these rules for it is defined as the inverse of before.
(def-relation #_after (?itl-1 ?itl-2)
  "It means interval-1 is after the interval-2."
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-before itl2* itl1*)
                      (list env)
                    :fail))))
                
;; Prove the meets relation in lisp for further performance
(defun prove-meets (itl1 itl2)
  (let ((itl1-s (get-start-from-interval itl1))
         (itl1-e (get-end-from-interval itl1))
         (itl2-s (get-start-from-interval itl2))
         (itl2-e (get-end-from-interval itl2)))
    
    (if (and (utilities:xsd-datetime-p itl1-s)
             (utilities:xsd-datetime-p itl1-e)
             (utilities::xsd-datetime-p itl2-s)
             (utilities::xsd-datetime-p itl2-e)
             (datetime-equals itl1-e itl2-s))
        T
      NIL)))

;; Function that proves the relation 'meets' over two primitive intervals
(def-relation #_meets (?itl-1 ?itl-2)
  "It means interval-1 finishes right when interval-2 starts."
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-meets itl1* itl2*)
                      (list env)
                    :fail))))


;; Doesn't need the rules
(def-relation #_metBy (?itl-1 ?itl-2)
  "Inverse of meets."
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-meets itl2* itl1*)
                      (list env)
                    :fail))))


(defun prove-during (itl1 itl2)
  (let ((itl1-s (get-start-from-interval itl1))
         (itl1-e (get-end-from-interval itl1))
         (itl2-s (get-start-from-interval itl2))
         (itl2-e (get-end-from-interval itl2)))
    
    (if (and (utilities:xsd-datetime-p itl1-s)
             (utilities:xsd-datetime-p itl1-e)
             (utilities::xsd-datetime-p itl2-s)
             (utilities::xsd-datetime-p itl2-e)
             (datetime-after itl1-s itl2-s)
             (datetime-before itl1-e itl2-e))
        T
      NIL)))

(def-relation #_during (?itl-1 ?itl-2)
  "Interval 1 is between (during) the start and enf time of interval 2"
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-during itl1* itl2*)
                      (list env)
                    :fail))))


(def-relation #_contains (?itl-1 ?itl-2)
  "Inverse of during"
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-during itl2* itl1*)
                      (list env)
                    :fail))))

(defun prove-overlaps (itl1 itl2)
  (let ((itl1-s (get-start-from-interval itl1))
         (itl1-e (get-end-from-interval itl1))
         (itl2-s (get-start-from-interval itl2))
         (itl2-e (get-end-from-interval itl2)))
    
    (if (and (utilities:xsd-datetime-p itl1-s)
             (utilities:xsd-datetime-p itl1-e)
             (utilities::xsd-datetime-p itl2-s)
             (utilities::xsd-datetime-p itl2-e)
             (datetime-before itl1-s itl2-s)
             (datetime-before itl2-s itl1-e)
             (datetime-before itl1-e itl2-e))
        T
      NIL)))


(def-relation #_overlaps (?itl-1 ?itl-2)
  "It means that two intervals overlap with each other: interval 1 starts before interval 2 and it finishes after the beginning of interval 2 but before the end of interval 2."
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-overlaps itl1* itl2*)
                      (list env)
                    :fail))))


(def-relation #_overlappedBy (?itl-1 ?itl-2)
  "Inverse of overlaps"
  :constraint  (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                    (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))
  
  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-overlaps itl2* itl1*)
                      (list env)
                    :fail))))


(defun prove-finishes (itl1 itl2)
  (let ((itl1-s (get-start-from-interval itl1))
         (itl1-e (get-end-from-interval itl1))
         (itl2-s (get-start-from-interval itl2))
         (itl2-e (get-end-from-interval itl2)))
    
    (if (and (utilities:xsd-datetime-p itl1-s)
             (utilities:xsd-datetime-p itl1-e)
             (utilities::xsd-datetime-p itl2-s)
             (utilities::xsd-datetime-p itl2-e)
             (datetime-before itl2-s itl1-s)
             (datetime-equals itl1-e itl2-e))
        T
      NIL)))


(def-relation #_finishes (?itl-1 ?itl-2)
  "It means that both the intervals finish at the same time but interval-1 starts after interval-2."
  :constraint  (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                    (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))
  
  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-finishes itl1* itl2*)
                      (list env)
                    :fail))))

(Def-relation #_finishedBy (?itl-1 ?itl-2)
  "Inverse of finishes"
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-finishes itl2* itl1*)
                      (list env)
                    :fail))))


(defun prove-starts (itl1 itl2)
  (let ((itl1-s (get-start-from-interval itl1))
         (itl1-e (get-end-from-interval itl1))
         (itl2-s (get-start-from-interval itl2))
         (itl2-e (get-end-from-interval itl2)))
    
    (if (and (utilities:xsd-datetime-p itl1-s)
             (utilities:xsd-datetime-p itl1-e)
             (utilities::xsd-datetime-p itl2-s)
             (utilities::xsd-datetime-p itl2-e)
             (datetime-equals itl2-s itl1-s)
             (datetime-before itl1-e itl2-e))
        T
      NIL)))


(def-relation #_starts (?itl-1 ?itl-2)
   "It means that both the intervals starts at the same time but interval-1 ends before interval-2."
  :constraint  (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                    (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))
  
  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-starts itl1* itl2*)
                      (list env)
                    :fail))))

(def-relation #_startedBy (?itl-1 ?itl-2)
   "Inverse of starts."
  :constraint  (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                    (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))
  
  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-starts itl2* itl1*)
                      (list env)
                    :fail))))


(defun prove-interval-equals (itl1 itl2)
  (let ((itl1-s (get-start-from-interval itl1))
         (itl1-e (get-end-from-interval itl1))
         (itl2-s (get-start-from-interval itl2))
         (itl2-e (get-end-from-interval itl2)))
    
    (if (and (utilities:xsd-datetime-p itl1-s)
             (utilities:xsd-datetime-p itl1-e)
             (utilities::xsd-datetime-p itl2-s)
             (utilities::xsd-datetime-p itl2-e)
             (datetime-equals itl1-s itl2-s)
             (datetime-equals itl1-e itl2-e))
        T
      NIL)))


(def-relation #_intervalEquals (?itl-1 ?itl-2)
  "It means that both the intervals starts and finsihes at the same time."
  :constraint  (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                    (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-interval-equals itl1* itl2*)
                      (list env)
                    :fail))))

;;;
;; Additional relations on top of Allen's 
;;;

;; Defines the rule Temporally-Coincide between two intervals or Time spanning entities
;; In fact is like Interval-equals but can also deal with Time Spanning entities.
(def-relation #_temporallyCoincide (?itl-1 ?itl-2)
  "The two intervals or time-spanning entities, or two instants or instantaneous entities coincide in time."  
  :constraint  (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                    (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-interval-equals itl1* itl2*)
                      (list env)
                    :fail))))

(def-relation #_temporallyDisjoint (?itl-1 ?itl-2)
  "It is true if either itl-1 is before itl-2 or itl-2 is before itl-1. Intervals can coincide in the limits and still be temporally disjoints."
  :constraint  (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                    (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :iff-def (or (#_before ?itl-1 ?itl-2)
               (#_before ?itl-2 ?itl-1)
               (#_meets ?itl-1 ?itl-2)
               (#_meets ?itl-2 ?itl-1)))


(def-relation #_temporallyConcurrent (?itl-1 ?itl-2)
  "True if there is an interval (i.e. not just an instant), which is shared between both intervals. It is the inverse of temporally-disjoint"
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))
  
  :iff-def (not (#_temporallyDisjoint ?itl-1 ?itl-2)))

(def-relation #_beforeOrMeets (?itl-1 ?itl-2)
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))
  
  :iff-def (or (#_before ?itl-1 ?itl-2)
               (#_meets ?itl-1 ?itl-2)))

(def-relation #_afterOrMetBy (?itl-1 ?itl-2)
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))
  
  :iff-def (or (#_after ?itl-1 ?itl-2)
               (#_metBy ?itl-1 ?itl-2)))


(defun prove-starts-before (itl1 itl2)
  (let ((itl1-s (get-start-from-interval itl1))
         (itl1-e (get-end-from-interval itl1))
         (itl2-s (get-start-from-interval itl2))
         (itl2-e (get-end-from-interval itl2)))
    
    (if (and (utilities:xsd-datetime-p itl1-s)
             (utilities:xsd-datetime-p itl1-e)
             (utilities::xsd-datetime-p itl2-s)
             (utilities::xsd-datetime-p itl2-e)
             (datetime-before itl1-s itl2-s))
        T
      NIL)))

(def-relation #_startsBefore (?itl-1 ?itl-2)
  "True if itl-1 starts before itl-2"
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-starts-before itl1* itl2*)
                      (list env)
                    :fail))))

(def-relation #_startsAfter (?itl-1 ?itl-2)
  "True if itl-1 starts after itl-2"
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))
  
  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-starts-before itl2* itl1*)
                      (list env)
                    :fail))))


(defun prove-finishes-before (itl1 itl2)
  (let ((itl1-s (get-start-from-interval itl1))
         (itl1-e (get-end-from-interval itl1))
         (itl2-s (get-start-from-interval itl2))
         (itl2-e (get-end-from-interval itl2)))
    
    (if (and (utilities:xsd-datetime-p itl1-s)
             (utilities:xsd-datetime-p itl1-e)
             (utilities::xsd-datetime-p itl2-s)
             (utilities::xsd-datetime-p itl2-e)
             (datetime-before itl1-e itl2-e))
        T
      NIL)))

(def-relation #_finishesBefore (?itl-1 ?itl-2)
  "True if itl-1 finishes before itl-2"
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))

  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-finishes-before itl1* itl2*)
                      (list env)
                    :fail))))


(def-relation #_finishesAfter (?itl-1 ?itl-2)
  "True if itl-1 finishes after itl-2"
  :constraint (and (or (#_TimeInterval ?itl-1) (#_TimeSpanningEntity ?itl-1))
                   (or (#_TimeInterval ?itl-2) (#_TimeSpanningEntity ?itl-2)))
 
  :lisp-fun #'(lambda (itl1 itl2 env)
                (let ((itl1* (instantiate itl1 env))
                      (itl2* (instantiate itl2 env)))
                  (if (prove-finishes-before itl2* itl1*)
                      (list env)
                    :fail))))

;; Function for obtaining the first to start
(def-function #_firstToStart (?itl-list) -> ?itl
  "Given a set of intervals or time-spanning entities, it returns the one that started first."
  ;;:constraint TODO: We should say that they are all either intervals or tse
  ;;:def TODO
  :lisp-fun #'(lambda (itl-list)
                 (most itl-list #'#_startsBefore)))

(def-relation #_followedBy (?tse-1 ?tse-2 ?class)
  "Time spanning entity 2 is the next time spanning entity of type ?class after tse 1."
  :constraint (and (#_TimeSpanningEntity ?tse-1)
                   (#_TimeSpanningEntity ?tse-2)
                   (class ?class))

  :iff-def (and (instance-of ?tse-2 ?class)
                (#_beforeOrMeets ?tse-1 ?tse-2)
                (not (exists ?x (and (instance-of ?x #_TimeSpanningEntity) 
                                     (instance-of ?x ?class)
                                     (#_beforeOrMeets ?tse-1 ?x)
                                     (#_startsBefore ?x ?tse-2))))))

(def-relation #_precededBy (?tse-1 ?tse-2)
  "Tse-1 is the tse preceding tse-2"
  :constraint (and (#_TimeSpanningEntity ?tse-1)
                   (#_TimeSpanningEntity ?tse-2))

  :iff-def (#_followedBy ?tse-2 ?tse-1))


;;;
;; Relations between Instants or Instantaneous Entities and Intervals or Time Spanning Entities
;;;
(defun prove-instant-before (i itl)
  (let ((itl-s (get-start-from-interval itl))
        (instant (get-time-from-instant i)))
    
    (if (and (utilities:xsd-datetime-p itl-s)
             (utilities:xsd-datetime-p instant)
             (datetime-before instant itl-s))
        T
      NIL)))

(def-relation #_instantBefore (?instant ?itl)
  "The instant is before the tse"
  :constraint (and (or (#_TimeSpanningEntity ?itl) (#_TimeInterval ?itl))
                   (or (#_InstantaneousEntity ?instant) (#_XSD:DateTime ?instant)))
 
  :lisp-fun #'(lambda (instant itl env)
                (let ((itl* (instantiate itl env))
                      (instant* (instantiate instant env)))
                  (if (prove-instant-before instant* itl*)
                      (list env)
                    :fail))))

(defun prove-instant-after (i itl)
  (let ((itl-e (get-end-from-interval itl))
        (instant (get-time-from-instant i)))
    
    (if (and (utilities:xsd-datetime-p itl-e)
             (utilities:xsd-datetime-p instant)
             (datetime-after instant itl-e))
        T
      NIL)))

(def-relation #_instantAfter (?instant ?itl)
  "The instant is after the tse"
  :constraint (and (or (#_TimeInterval ?itl)
                       (#_TimeSpanningEntity ?itl))
                   (or (#_XSD:DateTime ?instant)
                       (#_InstantaneousEntity ?i)))

  :lisp-fun #'(lambda (instant itl env)
                (let ((itl* (instantiate itl env))
                      (instant* (instantiate instant env)))
                  (if (prove-instant-after instant* itl*)
                      (list env)
                    :fail))))


;; INSTANT DURING
(defun prove-instant-during (i itl)
  (let ((itl-s (get-start-from-interval itl))
        (itl-e (get-end-from-interval itl))
        (instant (get-time-from-instant i)))
    
    (if (and (utilities:xsd-datetime-p itl-s)
             (utilities:xsd-datetime-p itl-e)
             (utilities:xsd-datetime-p instant)
             (datetime-after instant itl-s)
             (datetime-before instant itl-e))
        T
      NIL)))

(def-relation #_instantDuring (?i ?itl)
    "The instant takes place during the interval or time spanning entity"
  :constraint (and (or (#_TimeInterval ?itl)
                       (#_TimeSpanningEntity ?itl))
                   (or (#_XSD:DateTime ?i)
                       (#_InstantaneousEntity i)))

  :lisp-fun #'(lambda (instant itl env)
                (let ((itl* (instantiate itl env))
                      (instant* (instantiate instant env)))
                  (if (prove-instant-during instant* itl*)
                      (list env)
                    :fail))))

(defun prove-instant-starts (i itl)
  (let ((itl-s (get-start-from-interval itl))
        (instant (get-time-from-instant i)))
    
    (if (and (utilities:xsd-datetime-p itl-s)
             (utilities:xsd-datetime-p instant)
             (datetime-equals instant itl-s))             
        T
      NIL)))

(def-relation #_instantStarts (?instant ?itl)
  "The instant coincides with the start of the tse"
  :constraint (and (or (#_TimeInterval ?itl)
                       (#_TimeSpanningEntity ?itl))
                   (or (#_XSD:DateTime ?instant)
                       (#_InstantaneousEntity ?instant)))

  :lisp-fun #'(lambda (instant itl env)
                (let ((itl* (instantiate itl env))
                      (instant* (instantiate instant env)))
                  (if (prove-instant-starts instant* itl*)
                      (list env)
                    :fail))))

;;
;; INSTANT FINISHES
;; Prove the instant-finishes relation in lisp for further performance
(defun prove-instant-finishes (i itl)
  (let ((itl-e (get-end-from-interval itl))
        (instant (get-time-from-instant i)))
    
    (if (and (utilities:xsd-datetime-p itl-e)
             (utilities:xsd-datetime-p instant)
             (datetime-equals instant itl-e))             
        T
      NIL)))

(def-relation #_instantFinishes (?instant ?itl)
  "The instant coincides with the end of the tse"
  :constraint (and (or (#_TimeInterval ?itl)
                       (#_TimeSpanningEntity ?itl))
                   (or (#_XSD:DateTime ?instant)
                       (#_InstantaneousEntity ?instant)))

  :lisp-fun #'(lambda (instant itl env)
                (let ((itl* (instantiate itl env))
                      (instant* (instantiate instant env)))
                  (if (prove-instant-finishes instant* itl*)
                      (list env)
                    :fail))))

;;;
;; Definition of all the tse related relations and functions
;; over happenings
;;;

;;;;
;; BASIC Instant Related Relations and Functions
;;;;

;; Prove the precedes relation in lisp for further performance
(defun prove-precedes (t1 t2)
  (let ((i1 (get-time-from-instant t1))
        (i2 (get-time-from-instant t2)))
    
    (if (and (utilities:xsd-datetime-p i1)
             (utilities:xsd-datetime-p i2)
             (datetime-before i1 i2))             
        T
      NIL)))

(def-relation #_precedes (?t1 ?t2)
  "Precedes relation between Instants and Instantaneous Entities. One instant precedes another if it occurs before. Implemented in lisp for further performance."
  :constraint (and (or (#_XSD:DateTime ?t1) (#_InstantaneousEntity ?t1))
                   (or (#_XSD:DateTime ?t2) (#_InstantaneousEntity ?t2)))
  
  :lisp-fun #'(lambda (t1 t2 env)
                (let ((t1* (instantiate t1 env))
                      (t2* (instantiate t2 env)))
                  (if (prove-precedes t1* t2*)
                      (list env)
                    :fail))))

;; Prove the follows relation in lisp for further performance
(def-relation #_follows (?t1 ?t2)
    "Follows relation between Instants and Instantaneous Entities. One instant follows another if it occurs later. Implemented in lisp for further performance."
    :constraint (and (or (#_XSD:DateTime ?t1) (#_InstantaneousEntity ?t1))
                     (or (#_XSD:DateTime ?t2) (#_InstantaneousEntity ?t2)))

  :lisp-fun #'(lambda (t1 t2 env)
                (let ((t1* (instantiate t1 env))
                      (t2* (instantiate t2 env)))
                  (if (prove-precedes t2* t1*)
                      (list env)
                    :fail))))
                  

(defun prove-instants-coincide (t1 t2)
  (let ((i1 (get-time-from-instant t1))
        (i2 (get-time-from-instant t2)))
    
    (if (and (utilities:xsd-datetime-p i1)
             (utilities:xsd-datetime-p i2)
             (datetime-equals i1 i2))              
        T
      NIL)))

(def-relation #_instantsCoincide (?t1 ?t2)
  "Primitive relation instants coincide over temporal entities and intervals. Implemented in lisp for further performance"
  :constraint (and (or (#_XSD:DateTime ?t1) (#_InstantaneousEntity ?t1))
                   (or (#_XSD:DateTime ?t2) (#_InstantaneousEntity ?t2)))
  
  :lisp-fun #'(lambda (t1 t2 env)
                (let ((t1* (instantiate t1 env))
                      (t2* (instantiate t2 env)))
                  (if (prove-instants-coincide t1* t2*)
                      (list env)
                    :fail))))
                
;;
;; Primitive Relations over dates, times and dateTimes
;;

;; Primitive lisp functions

(defun date-after (i1 i2)
  (> (utilities::xsd-date-milliseconds i1)
     (utilities::xsd-date-milliseconds i2)))

(defun date-before (i1 i2)
  (date-after i2 i1))

(defun time-after (i1 i2)
  (> (utilities::xsd-time-milliseconds i1*)
     (utilities::xsd-time-milliseconds i2*)))

(defun time-before (i1 i2)
  (time-after i2 i1))

(defun datetime-after (i1 i2)    
  (> (utilities::xsd-datetime-milliseconds i1)
     (utilities::xsd-datetime-milliseconds i2)))

(defun datetime-before (i1 i2)
  (datetime-after i2 i1))

(defun create-milliseconds-quantity (millis)
  (let ((instance-structure
         (new-instance '#_PHYS-Q:ConstantQuantity
                       `((#_PHYS-Q:hasMagnitude ,millis)
                         (#_PHYS-Q:hasUnitOfMeasure #_SI:Millisecond)))))
    (ocml::name instance-structure)))

(defun compute-difference (i1 i2)
    (- (utilities::xsd-datetime-milliseconds i1)
       (utilities::xsd-datetime-milliseconds i2)))

(defun datetime-equals (i1 i2)
  (= (utilities::xsd-datetime-milliseconds i1)
     (utilities::xsd-datetime-milliseconds i2)))

;; Primitive Relations

(def-relation #_dateBefore (?i1 ?i2)
  "Succeeds if ?i1 comes before ?i2."
  :constraint (and (#_XSD:Date ?i1)
                   (#_XSD:Date ?i2))

  :lisp-fun #'(lambda (i1 i2 env)
                (let ((i1* (instantiate i1 env))
                      (i2* (instantiate i2 env)))
                  (if (and (utilities:xsd-date-p i1*)
                           (utilities::xsd-date-p i2*)
                           (date-before i1* i2*))
                      (list env)
                    :fail))))

(def-relation #_dateAfter (?i1 ?i2)
  "Succeeds if ?i1 comes after ?i2."
  :constraint (and (#_XSD:Date ?i1)
                   (#_XSD:Date ?i2))

  :lisp-fun #'(lambda (i1 i2 env)
                (let ((i1* (instantiate i1 env))
                      (i2* (instantiate i2 env)))
                  (if (and (utilities:xsd-date-p i1*)
                           (utilities::xsd-date-p i2*)
                           (date-after i1* i2*))
                      (list env)
                    :fail))))

(def-relation #_timeBefore (?i1 ?i2)
  "Succeeds if ?i1 comes before ?i2."
  :constraint (and (#_XSD:Time ?i1)
                   (#_XSD:Time ?i2))
  
  :lisp-fun #'(lambda (i1 i2 env)
                (let ((i1* (instantiate i1 env))
                      (i2* (instantiate i2 env)))
                  (if (and (utilities:xsd-time-p i1*)
                           (utilities::xsd-time-p i2*)
                           (time-before i1* i2*))
                      (list env)
                    :fail))))

(def-relation #_timeAfter (?i1 ?i2)
  "Succeeds if ?i1 comes after ?i2."
  :constraint (and (#_XSD:Time ?i1)
                   (#_XSD:Time ?i2))

  :lisp-fun #'(lambda (i1 i2 env)
                (let ((i1* (instantiate i1 env))
                      (i2* (instantiate i2 env)))
                  (if (and (utilities:xsd-time-p i1*)
                           (utilities::xsd-time-p i2*)
                           (time-after i1* i2*))
                      (list env)
                    :fail))))

(def-relation #_dateTimeBefore (?i1 ?i2)
  "Succeeds if ?i1 comes before ?i2."
  :constraint (and (#_XSD:DateTime ?i1)
                   (#_XSD:DateTime ?i2))

  :lisp-fun #'(lambda (i1 i2 env)
                (let ((i1* (instantiate i1 env))
                      (i2* (instantiate i2 env)))
                  (if (and (utilities::xsd-datetime-p i1*)
                           (utilities::xsd-datetime-p i2*)
                           (datetime-before i1* i2*))
                      (list env)
                    :fail))))

(def-relation #_dateTimeAfter (?i1 ?i2)
  "Succeeds if ?i1 comes after ?i2."
  :constraint (and (#_XSD:DateTime ?i1)
                   (#_XSD:DateTime ?i2))

  :lisp-fun #'(lambda (i1 i2 env)
                (let ((i1* (instantiate i1 env))
                      (i2* (instantiate i2 env)))
                  (if (and (utilities::xsd-datetime-p i1*)
                           (utilities::xsd-datetime-p i2*)
                           (datetime-after i1* i2*))
                      (list env)
                    :fail))))


(def-function #_dateTimesDifference (?instant-1 ?instant-2)
  "This function computes the different in milliseconds between two dateTimes."
  :constraint (and (#_XSD:DateTime ?instant-1)
                   (#_XSD:DateTime ?instant-2))
  
  :lisp-fun #'(lambda (i1 i2)
                (create-milliseconds-quantity (compute-difference i1 i2))))


(def-relation #_dateTimeEquals (?i1 ?i2)
"This relation is true is both dateTimes are identical, i.e., their value is the same number of milliseconds since epoch."
  :constraint (and (#_XSD:DateTime ?i1)
                   (#_XSD:DateTime ?i2))

  :lisp-fun #'(lambda (i1 i2 env)
                (let ((i1* (instantiate i1 env))
                      (i2* (instantiate i2 env)))
                  (if (and (utilities::xsd-datetime-p i1*)
                           (utilities::xsd-datetime-p i2*)                             
                           (datetime-equals i1* i2*))
                      (list env)
                  :fail))))

;;
;; Useful functions for obtaining current date and/or time
;;

(def-function #_getCurrentDate () -> ?currentDate
  "Returns the current Date instance"
  :lisp-fun #'(lambda ()
              (multiple-value-bind (sec min hour day month year)
                  (get-decoded-time)
                (declare (ignore sec min hour))
                (xsd-date year month day))))

(def-function #_getCurrentDateTime () -> ?currentDateTime
  "Returns the current dateTime instance"
  :lisp-fun #'(lambda ()
                (multiple-value-bind
                    (unix-seconds microseconds)
                    (irs.utilities::get-accurate-time)
                  (irs.utilities::make-xsd-datetime :milliseconds (+ (* unix-seconds 1000) 
                                                                     (/ microseconds 1000))))))

;; 
;; TODO: Implement getCurrentTime
;; Note that time should ignore the current day!
;;

#|
(def-function #_intervalDuration (?interval) -> ?seconds
"Returns the duration of an interval in seconds. If the upper-bound limit is not set the resulting value is based on using the current instant."
  :constraint (#_TimeInterval ?interval)
              
  :lisp-fun #'(lambda (interval)
                (let* ((i1 (ocml-eval-fun `(the-slot-value ,interval #_hasStartTime)))
                       (i2 (ocml-eval-fun `(the-slot-value ,interval #_hasEndTime))))
                  
                (if (holds? '#_XSD:DateTime i1)
                    (if (holds? '#_XSD:DateTime i2)
                        (ocml-eval-fun `(#_dateTimesDifference ,i2 ,i1))
                      (ocml-eval-fun `(#_dateTimesDifference ,(ocml-eval-fun `(#_getCurrentDateTime)) ,i1)))
                  0))))
                    
(def-function #_getDuration (?ti) -> ?d
  "Returns the duration of the interval or time spanning entity"
  :constraint (or (#_TimeInterval ?ti)
                  (#_TimeSpanningEntity ?ti))

  :lisp-fun #'(lambda (ti)
                (let ((interval (if (holds? '#_TimeInterval ti)
                                    ti
                                  (ocml-eval-fun `(the-slot-value ,ti #_spansInterval)))))
                  (name (new-instance '#_Duration
                                      `((#_PHYS-Q:hasMagnitude 
                                         ,(ocml-eval-fun `(#_intervalDuration ,interval)))

                                        (#_PHYS-Q:hasUnitOfMeasure #_SI:Second)))))))                                          
|#