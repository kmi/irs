;;; Mode: Lisp; Package: ocml

;;; Tests for the Time Ontology

(in-package "OCML")

(in-ontology time-ontology)

(setf i0 (ocml-eval-fun `(#_getCurrentDateTime)))
(setf i1 (ocml-eval-fun `(#_getCurrentDateTime)))
(setf i2 (ocml-eval-fun `(#_getCurrentDateTime)))
(setf i3 (ocml-eval-fun `(#_getCurrentDateTime)))
(setf i4 (ocml-eval-fun `(#_getCurrentDateTime)))
(setf i5 (ocml-eval-fun `(#_getCurrentDateTime)))
(setf i6 (ocml-eval-fun `(#_getCurrentDateTime)))

(defun create-instantaneous-entity (name time)
  (let ((instance-structure
         (define-domain-instance name 
                                 '#_InstantaneousEntity
                                 ""
                                 `((#_occursAt ,time)))))
    (ocml::name instance-structure)))

(defun create-interval (name start end)
  (let ((instance-structure
         (define-domain-instance name 
                                 '#_TimeInterval
                                 ""
                                 `((#_hasStartTime ,start)
                                   (#_hasEndTime ,end)))))
    (ocml::name instance-structure)))


(create-instantaneous-entity 'Instantaneous-0 i0)
(create-instantaneous-entity 'Instantaneous-1 i1)
(create-instantaneous-entity 'Instantaneous-2 i2)
(create-instantaneous-entity 'Instantaneous-3 i3)
(create-instantaneous-entity 'Instantaneous-4 i4)
(create-instantaneous-entity 'Instantaneous-5 i5)
(create-instantaneous-entity 'Instantaneous-6 i6)

(create-interval 'Interval-A i0 i1)
(create-interval 'Interval-B i1 i2)
(create-interval 'Interval-C i0 i2)
(create-interval 'Interval-D i2 i3)
(create-interval 'Interval-E i0 i3)
(create-interval 'Interval-F i1 i4)
(create-interval 'Interval-G i1 i4)

(def-instance TimeSpanning-A #_TimeSpanningEntity
  ((#_spansInterval Interval-A)))

(def-instance TimeSpanning-B #_TimeSpanningEntity
  ((#_spansInterval Interval-B)))

(def-instance TimeSpanning-C #_TimeSpanningEntity
  ((#_spansInterval Interval-C)))

(def-instance TimeSpanning-D #_TimeSpanningEntity
  ((#_spansInterval Interval-D)))

(def-instance TimeSpanning-E #_TimeSpanningEntity
  ((#_spansInterval Interval-E)))

(def-instance TimeSpanning-F #_TimeSpanningEntity
  ((#_spansInterval Interval-F)))

(def-instance TimeSpanning-G #_TimeSpanningEntity
  ((#_spansInterval Interval-G)))


;; (ocml-eval-fun-term '#_dateTimesDifference `(,i1 ,i2))

;; i1 is not after i2
;;(holds? '#_dateTimeAfter i1 i2)

;; i2 is after i1
;;(holds? '#_dateTimeAfter i2 i1)

;; i2 is not after i2
;;(holds? '#_dateTimeAfter i2 i2)

;; i2 is not before i1
;;(holds? '#_dateTimeBefore i2 i1)

;; i1 is not before i1
;;(holds? '#_dateTimeBefore i1 i1)

;; i1 is before i2
;;(holds? '#_dateTimeBefore i1 i2)

;; i2 equals i2
;;(holds? '#_dateTimeEquals i2 i2)

;; i1 and i2 are not equal
;;(holds? '#_dateTimeEquals i1 i2)

;; Instants Coincide
;; i1 and i2 do not coincide
;;(holds? '#_instantsCoincide i1 i2)

;; i1 and a coincide
;;(holds? '#_instantsCoincide i1 i1)

;; TODO: Test for coincides InstantaneousEntities

;; i1 does not follow i2
;;(holds? '#_follows i1 i2)

;; i2 follows i1
;;(holds? '#_follows i2 i1)

;; i2 does not follow i2
;;(holds? '#_follows i2 i2)

;;(holds? '> 4 3)

;; Follows
;;(holds? '#_follows 'Instantaneous-2 'Instantaneous-1)

;; Does not follow 
;;(holds? '#_follows 'instantaneous-1 'instantaneous-2)

;; Precedes

;; i1 precedes i2
;;(holds? '#_precedes i1 i2)

;; i2 does not precede i1
;;(holds? '#_precedes i2 i1)

;; i2 does not precede i2
;;(holds? '#_precedes i2 i2)

;; Does not precede
;;(holds? '#_precedes 'instantaneous-1 'instantaneous-0)

;; precedes 
;;(holds? '#_precedes 'instantaneous-0 'instantaneous-1)

;; Duration

;; Instant Finishes
;; i0 does not finish Interval A
;;(holds? '#_instantFinishes i0 'Interval-A)

;; i1 finishes Interval A
;;(holds? '#_instantFinishes i1 'Interval-A)

;; i2 finishes Interval B
;;(holds? '#_instantFinishes i2 'Interval-B)

;; i0 does not finish time spanning A
;;(holds? '#_instantFinishes i0 'TimeSpanning-A)

;; i1 finishes time spanning A
;;(holds? '#_instantFinishes i1 'TimeSpanning-A)

;; instantaneous-2 finishes Interval B
;;(holds? '#_instantFinishes 'instantaneous-2 'Interval-B)

;; instantaneous-2 does not finish Interval A
;;(holds? '#_instantFinishes 'instantaneous-2 'Interval-A)

;; instantaneous-2 finishes TimeSpanning B
;;(holds? '#_instantFinishes 'instantaneous-2 'TimeSpanning-B)

;; Instant Starts
;; i0 starts Interval A
;;(holds? '#_instantStarts i0 'Interval-A)

;; i1 does not start Interval A
;;(holds? '#_instantStarts i1 'Interval-A)

;; i1 starts Interval B
;;(holds? '#_instantStarts i1 'Interval-B)

;; i2 does not start Interval B
;;(holds? '#_instantStarts i2 'Interval-B)

;; instantaneous-1 starts Interval B
;;(holds? '#_instantStarts 'instantaneous-1 'Interval-B)

;; instantaneous-2 does not start Interval B
;;(holds? '#_instantStarts 'instantaneous-2 'Interval-B)

;; Instant During
;; i0 is not during Interval A
;;(holds? '#_instantDuring i0 'Interval-A)

;; instantaneous-0 is not during Interval A
;;(holds? '#_instantDuring 'instantaneous-0 'Interval-A)

;; i0 is not during TimeSpanning-A
;;(holds? '#_instantDuring i0 'TimeSpanning-A)

;; i0 is not during Interval A
;;(holds? '#_instantDuring 'instantaneous-0 'TimeSpanning-A)

;; i1 is during Interval C
;;(holds? '#_instantDuring i1 'Interval-C)

;; instantaneous-1 is during Interval C
;;(holds? '#_instantDuring 'instantaneous-1 'Interval-C)

;; i1 is during TimeSpanning-C
;;(holds? '#_instantDuring i1 'TimeSpanning-C)

;; instantaneous-1 is not during Interval C
;;(holds? '#_instantDuring 'instantaneous-1 'TimeSpanning-C)


;; Instant After
;; i0 is not after Interval A
;;(holds? '#_instantAfter i0 'Interval-A)

;; instantaneous-0 is not after Interval A
;;(holds? '#_instantAfter 'instantaneous-0 'Interval-A)

;; i0 is not after TimeSpanning-A
;;(holds? '#_instantAfter i0 'TimeSpanning-A)

;; i0 is not after Interval A
;;(holds? '#_instantAfter 'instantaneous-0 'TimeSpanning-A)

;; i1 is not after Interval A
;;(holds? '#_instantAfter i1 'Interval-A)

;; i2 is after Interval A
;;(holds? '#_instantAfter i2 'Interval-A)

;; instantaneous-2 is after Interval A
;;(holds? '#_instantAfter 'instantaneous-2 'Interval-A)

;; i2 is after TimeSpanning-A
;;(holds? '#_instantAfter i2 'TimeSpanning-A)

;; instantaneous-2 is not after Interval A
;;(holds? '#_instantAfter 'instantaneous-2 'TimeSpanning-A)


;; Instant Before
;; i0 is not before Interval A
;;(holds? '#_instantBefore i0 'Interval-A)

;; i0 is not before Interval A
;;(holds? '#_instantBefore 'instantaneous-0 'Interval-A)

;; i0 is not before Interval A
;;(holds? '#_instantBefore i0 'TimeSpanning-A)

;; i0 is not before Interval A
;;(holds? '#_instantBefore 'instantaneous-0 'TimeSpanning-A)

;; i0 is before Interval B
;;(holds? '#_instantBefore i0 'Interval-B)

;; i0 is not before Interval A
;;(holds? '#_instantBefore 'instantaneous-0 'Interval-B)

;; i0 is not before Interval A
;;(holds? '#_instantBefore i0 'TimeSpanning-B)

;; i0 is not before Interval A
;;(holds? '#_instantBefore 'instantaneous-0 'TimeSpanning-B)


;; First to start
;; (ocml-eval (#_firstToStart `(Interval-A Interval-B Interval-C)))

;; Interval-A does not finish After Interval A
;;(holds? '#_finishesAfter 'Interval-A 'Interval-A)

;; Interval-A does not finish After Interval B
;;(holds? '#_finishesAfter 'Interval-A 'Interval-B)

;; Interval-A does not finish After Interval A
;;(holds? '#_finishesAfter 'TimeSpanning-A 'TimeSpanning-A)

;; Interval-A does not finish After Interval A
;;(holds? '#_finishesAfter 'TimeSpanning-A 'TimeSpanning-B)

;; Interval-B finishes After Interval A
;;(holds? '#_finishesAfter 'TimeSpanning-B 'TimeSpanning-A)

;; Interval-B finishes After Interval A
;;(holds? '#_finishesAfter 'Interval-B 'TimeSpanning-A)


;; Interval-B does not finish Before Interval A
;;(holds? '#_finishesBefore 'Interval-B 'TimeSpanning-A)

;; Interval-B does not finish Before Interval A
;;(holds? '#_finishesBefore 'TimeSpanning-B 'TimeSpanning-A)

;; Interval-A finishes Before Interval B
;;(holds? '#_finishesBefore 'Interval-A 'TimeSpanning-B)

;; Interval-A finishes Before Interval B
;;(holds? '#_finishesBefore 'TimeSpanning-A 'TimeSpanning-B)


;; Interval-A does not start After Interval B
;;(holds? '#_startsAfter 'TimeSpanning-A 'TimeSpanning-B)

;; Interval-A does not start After Interval B
;;(holds? '#_startsAfter 'Interval-A 'Interval-B)

;; Interval-A does not start After Interval B
;;(holds? '#_startsAfter 'Interval-A 'TimeSpanning-B)

;; Interval-A does not start After Interval B
;;(holds? '#_startsAfter 'TimeSpanning-A 'Interval-B)

;; Interval-B starts After Interval A
;;(holds? '#_startsAfter 'TimeSpanning-B 'TimeSpanning-A)

;; Interval-B starts After Interval A
;;(holds? '#_startsAfter 'Interval-B 'Interval-A)

;; Interval-B starts After Interval A
;;(holds? '#_startsAfter 'Interval-B 'TimeSpanning-A)

;; Interval-B starts After Interval A
;;(holds? '#_startsAfter 'TimeSpanning-B 'Interval-A)


;; Interval-A starts Before Interval B
;;(holds? '#_startsBefore 'TimeSpanning-A 'TimeSpanning-B)

;; Interval-A starts Before Interval B
;;(holds? '#_startsBefore 'Interval-A 'TimeSpanning-B)

;; Interval-A starts Before Interval B
;;(holds? '#_startsBefore 'TimeSpanning-A 'Interval-B)

;; Interval-A starts Before Interval B
;;(holds? '#_startsBefore 'Interval-A 'Interval-B)

;; Interval-B does not start Before Interval A
;;(holds? '#_startsBefore 'TimeSpanning-B 'TimeSpanning-A)

;; Interval-B does not start Before Interval A
;;(holds? '#_startsBefore 'Interval-B 'TimeSpanning-A)

;; Interval-B does not start Before Interval A
;;(holds? '#_startsBefore 'TimeSpanning-B 'Interval-A)

;; Interval-B does not start Before Interval A
;;(holds? '#_startsBefore 'Interval-B 'Interval-A)



;; Interval-A is not before Interval B
;;(holds? '#_before 'Interval-A 'Interval-B)

;; Interval-A is not before Interval B
;;(holds? '#_before 'Interval-A 'TimeSpanning-B)

;; Interval-A is not before Interval B
;;(holds? '#_before 'TimeSpanning-A 'TimeSpanning-B)

;; Interval-A is not before Interval B
;;(holds? '#_before 'TimeSpanning-A 'Interval-B)

;; Interval-A is not before Interval A
;;(holds? '#_before 'TimeSpanning-A 'Interval-A)

;; Interval-A is before Interval D
;;(holds? '#_before 'TimeSpanning-A 'Interval-D)

;; Interval-A is before Interval D
;;(holds? '#_before 'Interval-A 'Interval-D)

;; Interval-A is before Interval D
;;(holds? '#_before 'TimeSpanning-A 'TimeSpanning-D)

;; Interval-A is before Interval D
;;(holds? '#_before 'Interval-A 'TimeSpanning-D)


;; Interval-A is not after Interval B
;;(holds? '#_after 'Interval-A 'Interval-B)

;; Interval-A is not after Interval B
;;(holds? '#_after 'Interval-A 'TimeSpanning-B)

;; Interval-A is not after Interval B
;;(holds? '#_after 'TimeSpanning-A 'TimeSpanning-B)

;; Interval-A is not after Interval B
;;(holds? '#_after 'TimeSpanning-A 'Interval-B)

;; Interval-A is not after Interval A
;;(holds? '#_after 'TimeSpanning-A 'Interval-A)

;; Interval-D is after Interval A
;;(holds? '#_after 'TimeSpanning-D 'Interval-A)

;; Interval-D is after Interval A
;;(holds? '#_after 'Interval-D 'Interval-A)

;; Interval-D is after Interval A
;;(holds? '#_after 'TimeSpanning-D 'TimeSpanning-A)

;; Interval-D is after Interval A
;;(holds? '#_after 'Interval-D 'TimeSpanning-A)



;; Interval-A meets Interval-B
;;(holds? '#_meets 'Interval-A 'TimeSpanning-B)

;; Interval-A meets Interval-B
;;(holds? '#_meets 'Interval-A 'Interval-B)

;; Interval-A meets Interval-B
;;(holds? '#_meets 'TimeSpanning-A 'TimeSpanning-B)

;; Interval-A meets Interval-B
;;(holds? '#_meets 'TimeSpanning-A 'Interval-B)

;; Interval-C meets Interval-D
;;(holds? '#_meets 'Interval-C 'TimeSpanning-D)

;; Interval-C meets Interval-D
;;(holds? '#_meets 'Interval-C 'Interval-D)

;; Interval-C meets Interval-D
;;(holds? '#_meets 'TimeSpanning-C 'TimeSpanning-D)

;; Interval-C meets Interval-D
;;(holds? '#_meets 'TimeSpanning-C 'Interval-D)

;; Interval-C does not meet Interval-B
;;(holds? '#_meets 'TimeSpanning-C 'Interval-B)

;; Interval-A does not meet Interval-D
;;(holds? '#_meets 'TimeSpanning-A 'Interval-D)

;; Interval-A is not metby Interval-D
;;(holds? '#_metBy 'TimeSpanning-A 'Interval-D)

;; Interval-A is not metby Interval-B
;;(holds? '#_metBy 'TimeSpanning-A 'Interval-B)

;; Interval-B is metby Interval-A
;;(holds? '#_metBy 'TimeSpanning-B 'Interval-A)



;; Interval-B is not during Interval-A
;;(holds? '#_during 'TimeSpanning-B 'Interval-A)

;; Interval-B is during Interval-E
;;(holds? '#_during 'TimeSpanning-B 'Interval-E)

;; Interval-B is during Interval-E
;;(holds? '#_during 'Interval-B 'Interval-E)

;; Interval-B is during Interval-E
;;(holds? '#_during 'Interval-B 'TimeSpanning-E)

;; Interval-B is during Interval-E
;;(holds? '#_during 'TimeSpanning-B 'Interval-E)

;; Interval-B is during Interval-E
;;(holds? '#_during 'TimeSpanning-B 'TimeSpanning-E)


;; Interval-B does not contain Interval-E
;;(holds? '#_contains 'TimeSpanning-B 'TimeSpanning-E)

;; Interval-E does not contain Interval-B
;;(holds? '#_contains 'TimeSpanning-E 'TimeSpanning-B)


;; Interval-E does not overlap with Interval-B
;;(holds? '#_overlaps 'TimeSpanning-E 'TimeSpanning-B)

;; Interval-C overlaps with Interval-F
;;(holds? '#_overlaps 'TimeSpanning-C 'TimeSpanning-F)

;; Interval-C overlaps with Interval-F
;;(holds? '#_overlaps 'Interval-C 'TimeSpanning-F)

;; Interval-E overlaps with Interval-F
;;(holds? '#_overlaps 'Interval-E 'Interval-F)

;; Interval-E is not overlappedBY with Interval-F
;;(holds? '#_overlappedBy 'Interval-E 'Interval-F)

;; Interval-F is not overlappedBY with Interval-E
;;(holds? '#_overlappedBy 'Interval-F 'Interval-E)

;; Interval-F is not overlappedBY with Interval-E
;;(holds? '#_overlappedBy 'Interval-F 'Interval-C)


;; Interval-F does not finish Interval-C
;;(holds? '#_finishes 'Interval-F 'Interval-C)

;; Interval-B finishes Interval-C
;;(holds? '#_finishes 'TimeSpanning-B 'TimeSpanning-C)

;; Interval-D finishes Interval-E
;;(holds? '#_finishes 'TimeSpanning-D 'Interval-E)


;; Interval-E finishedBy Interval-D
;;(holds? '#_finishedBy 'TimeSpanning-E 'Interval-D)

;; Interval-E finishedBy Interval-D
;;(holds? '#_finishedBy 'TimeSpanning-C 'Interval-B)


;;Interval-A starts Interval-C
;;(holds? '#_starts 'TimeSpanning-A 'Interval-C)

;; Interval-C starts Interval-E
;;(holds? '#_starts 'Interval-C 'TimeSpanning-E)

;; Interval-C does not start Interval-B
;;(holds? '#_starts 'Interval-C 'TimeSpanning-B)


;; Interval-E startedBy Interval-C
;;(holds? '#_startedBy 'Interval-E 'TimeSpanning-C)

;; Interval-B is not startedBy Interval-C
;;(holds? '#_startedBy 'Interval-B 'TimeSpanning-C)


;; Interval-B is not equal to Interval-C
;;(holds? '#_intervalEquals 'Interval-B 'TimeSpanning-C)

;; Interval-F is not equal to Interval-G
;;(holds? '#_intervalEquals 'Interval-F 'TimeSpanning-G)


;; Interval-C is temporallyDisjoint with Interval-D
;;(holds? '#_temporallyDisjoint 'Interval-C 'TimeSpanning-D)


