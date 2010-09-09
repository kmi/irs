;;; Mode: Lisp; Package: ocml

;;; Tests for the Events Analysis Ontology

(in-package "OCML")

(in-ontology events-analysis-ontology)

(def-instance instant-t0 instant
  ((microsecond-of 10))
)

(def-instance instant-t1 instant
  ((microsecond-of 30))
)

(def-instance instant-t2 instant
  ((microsecond-of 50))
)

(def-instance instant-t3 instant
  ((microsecond-of 70))
)

(def-instance instant-t4 instant
  ((microsecond-of 20))
)

(def-instance user-process-A user-process
  ((has-start-time instant-t0)
   (has-end-time instant-t1))
)

(def-instance user-process-B user-process
  ((has-start-time instant-t2)
   (has-end-time instant-t3))
)

(def-instance user-process-C user-process
  ((has-start-time instant-t4)
   (has-end-time instant-t3))
)

(def-instance user-process-D user-process
  ((has-start-time instant-t4)
   (has-end-time instant-t1))
)

(def-instance user-process-E user-process
  ((has-start-time instant-t0)
   (has-end-time instant-t2))
)

(def-instance user-process-F user-process
  ((has-start-time instant-t1)
   (has-end-time instant-t2))
)

(def-instance user-process-G user-process
  ((has-start-time instant-t1)
   (has-end-time instant-t3))
)

(def-instance user-process-H user-process
  ((has-start-time instant-t0)
   (has-end-time instant-t3))
)

(def-instance user-process-I user-process
  ((has-start-time instant-t0)
   (has-end-time instant-t4))
)

;;Check get-process-time-range
;;
;;(describe-instance 'ocml::user-process-A)

;; That looks like it is the way
;;(ocml-eval-gen `(ocml::get-process-time-range ocml::user-process-a))

;;
;;(findany '?x '(and (= ?x (get-proc-or-activ-time-range user-process-b)) (time-range ?x)))

;;;
;; Process Intervals Relations
;;
;; A before B
;;(holds? 'entity-before-entity 'user-process-A 'user-process-B)

;; B after A
;;(holds? 'entity-after-entity 'user-process-B 'user-process-A)

;; A meets G
;;(holds? 'entity-meets-entity 'user-process-A 'user-process-G)

;;(holds? 'instants-equal 'instant-t0 'instant-t0)

;; H is followed-by F
;;(holds? 'entity-followed-by-entity 'user-process-H 'user-process-F)

;; H is not followed-by B because there is F
;;(holds? 'entity-followed-by-entity 'user-process-H 'user-process-B)








