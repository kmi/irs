;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*- 

;;; DOLCE ontology.

 ;;; Translated from owl with OWL2OCML Translator (Author: Baldassarre Claudio)


(in-package "OCML")

(in-ontology DOLCE-Lite)



;-_-_-_-_-_-_-_-_-_-_-_-_-_- DEFINITION OF RELATIONS AND RULES-_-_-_-_-_-_-_-_-_-_-_-_-_-


(def-relation temporary-participant-in (?x ?y)
   :iff-def (temporary-participant ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-participant (?x ?y)
   :iff-def (temporary-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-participant-in-participant-in
 ((temporary-participant-in ?x ?y)
 if (participant-in ?x ?y)))

(def-relation spatio-temporal-presence-of (?x ?y)
   :iff-def (spatio-temporally-present-at ?y ?x)
   :avoid-infinite-loop t)

(def-relation spatio-temporally-present-at (?x ?y)
   :iff-def (spatio-temporal-presence-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule spatio-temporal-presence-of-exact-location-of
 ((spatio-temporal-presence-of ?x ?y)
 if (exact-location-of ?x ?y)))

(def-relation part-of (?x ?y)
   :iff-def (part ?y ?x)
   :avoid-infinite-loop t)

(def-relation part (?x ?y)
   :iff-def (part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule part-of-immediate-relation-i
 ((part-of ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation part-of (?x ?z)
   :sufficient (and (part-of ?x ?y)
                   (part-of ?y ?z)))

(def-relation t-inherent-in (?x ?y)
   :iff-def (has-t-quality ?y ?x)
   :avoid-infinite-loop t)

(def-relation has-t-quality (?x ?y)
   :iff-def (t-inherent-in ?y ?x)
   :avoid-infinite-loop t)

(def-rule t-inherent-in-inherent-in
 ((t-inherent-in ?x ?y)
 if (inherent-in ?x ?y)))

(def-relation partly-compresent (?x ?y)
   :iff-def (partly-compresent ?y ?x)
   :avoid-infinite-loop t)

(def-relation partly-compresent (?x ?y)
   :iff-def (partly-compresent ?y ?x)
   :avoid-infinite-loop t)

(def-rule partly-compresent-mediated-relation
 ((partly-compresent ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation specific-constant-constituent-of (?x ?y)
   :iff-def (specific-constant-constituent ?y ?x)
   :avoid-infinite-loop t)

(def-relation specific-constant-constituent (?x ?y)
   :iff-def (specific-constant-constituent-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule specific-constant-constituent-of-immediate-relation-i
 ((specific-constant-constituent-of ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation q-location (?x ?y)
   :iff-def (q-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation q-location-of (?x ?y)
   :iff-def (q-location ?y ?x)
   :avoid-infinite-loop t)

(def-rule q-location-immediate-relation
 ((q-location ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation proper-part-of (?x ?y)
   :iff-def (proper-part ?y ?x)
   :avoid-infinite-loop t)

(def-relation proper-part (?x ?y)
   :iff-def (proper-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule proper-part-of-part-of
 ((proper-part-of ?x ?y)
 if (part-of ?x ?y)))

(def-relation proper-part-of (?x ?z)
   :sufficient (and (proper-part-of ?x ?y)
                   (proper-part-of ?y ?z)))

(def-relation total-temporary-participant (?x ?y)
   :iff-def (total-temporary-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation total-temporary-participant-in (?x ?y)
   :iff-def (total-temporary-participant ?y ?x)
   :avoid-infinite-loop t)

(def-rule total-temporary-participant-temporary-participant
 ((total-temporary-participant ?x ?y)
 if (temporary-participant ?x ?y)))

(def-relation life-of (?x ?y)
   :iff-def (life ?y ?x)
   :avoid-infinite-loop t)

(def-relation life (?x ?y)
   :iff-def (life-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule life-of-constant-participant
 ((life-of ?x ?y)
 if (constant-participant ?x ?y)))

(def-relation total-temporary-participant-in (?x ?y)
   :iff-def (total-temporary-participant ?y ?x)
   :avoid-infinite-loop t)

(def-relation total-temporary-participant (?x ?y)
   :iff-def (total-temporary-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-rule total-temporary-participant-in-temporary-participant-in
 ((total-temporary-participant-in ?x ?y)
 if (temporary-participant-in ?x ?y)))

(def-relation has-quality (?x ?y)
   :iff-def (inherent-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation inherent-in (?x ?y)
   :iff-def (has-quality ?y ?x)
   :avoid-infinite-loop t)

(def-rule has-quality-immediate-relation-i
 ((has-quality ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation part (?x ?y)
   :iff-def (part-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation part-of (?x ?y)
   :iff-def (part ?y ?x)
   :avoid-infinite-loop t)

(def-rule part-immediate-relation
 ((part ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation part (?x ?z)
   :sufficient (and (part ?x ?y)
                   (part ?y ?z)))

(def-relation quale-of (?x ?y)
   :iff-def (has-quale ?y ?x)
   :avoid-infinite-loop t)

(def-relation has-quale (?x ?y)
   :iff-def (quale-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule quale-of-q-location-of
 ((quale-of ?x ?y)
 if (q-location-of ?x ?y)))

(def-relation q-location-of (?x ?y)
   :iff-def (q-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation q-location (?x ?y)
   :iff-def (q-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule q-location-of-immediate-relation-i
 ((q-location-of ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation q-present-at (?x ?y)
   :iff-def (time-of-q-presence-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation time-of-q-presence-of (?x ?y)
   :iff-def (q-present-at ?y ?x)
   :avoid-infinite-loop t)

(def-rule q-present-at-mediated-relation
 ((q-present-at ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation specific-constant-dependent (?x ?y)
   :iff-def (specifically-constantly-dependent-on ?y ?x)
   :avoid-infinite-loop t)

(def-relation specifically-constantly-dependent-on (?x ?y)
   :iff-def (specific-constant-dependent ?y ?x)
   :avoid-infinite-loop t)

(def-rule specific-constant-dependent-immediate-relation
 ((specific-constant-dependent ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation overlaps (?x ?y)
   :iff-def (overlaps ?y ?x)
   :avoid-infinite-loop t)

(def-relation overlaps (?x ?y)
   :iff-def (overlaps ?y ?x)
   :avoid-infinite-loop t)

(def-rule overlaps-mediated-relation
 ((overlaps ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation total-constant-participant-in (?x ?y)
   :iff-def (total-constant-participant ?y ?x)
   :avoid-infinite-loop t)

(def-relation total-constant-participant (?x ?y)
   :iff-def (total-constant-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-rule total-constant-participant-in-constant-participant-in
 ((total-constant-participant-in ?x ?y)
 if (constant-participant-in ?x ?y)))

(def-relation inherent-in (?x ?y)
   :iff-def (has-quality ?y ?x)
   :avoid-infinite-loop t)

(def-relation has-quality (?x ?y)
   :iff-def (inherent-in ?y ?x)
   :avoid-infinite-loop t)

(def-rule inherent-in-immediate-relation
 ((inherent-in ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation mediated-relation-i (?x ?y)
   :iff-def (mediated-relation ?y ?x)
   :avoid-infinite-loop t)

(def-relation mediated-relation (?x ?y)
   :iff-def (mediated-relation-i ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-proper-part (?x ?y)
   :iff-def (temporary-proper-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-proper-part-of (?x ?y)
   :iff-def (temporary-proper-part ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-proper-part-proper-part
 ((temporary-proper-part ?x ?y)
 if (proper-part ?x ?y)))

(def-rule temporary-proper-part-temporary-part
 ((temporary-proper-part ?x ?y)
 if (temporary-part ?x ?y)))

(def-relation spatio-temporally-present-at (?x ?y)
   :iff-def (spatio-temporal-presence-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation spatio-temporal-presence-of (?x ?y)
   :iff-def (spatio-temporally-present-at ?y ?x)
   :avoid-infinite-loop t)

(def-rule spatio-temporally-present-at-exact-location
 ((spatio-temporally-present-at ?x ?y)
 if (exact-location ?x ?y)))

(def-relation life (?x ?y)
   :iff-def (life-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation life-of (?x ?y)
   :iff-def (life ?y ?x)
   :avoid-infinite-loop t)

(def-rule life-constant-participant-in
 ((life ?x ?y)
 if (constant-participant-in ?x ?y)))

(def-relation strong-connection (?x ?y)
   :iff-def (strong-connection ?y ?x)
   :avoid-infinite-loop t)

(def-relation strong-connection (?x ?y)
   :iff-def (strong-connection ?y ?x)
   :avoid-infinite-loop t)

(def-rule strong-connection-mediated-relation
 ((strong-connection ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation temporary-atomic-part (?x ?y)
   :iff-def (temporary-atomic-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-atomic-part-of (?x ?y)
   :iff-def (temporary-atomic-part ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-atomic-part-temporary-proper-part
 ((temporary-atomic-part ?x ?y)
 if (temporary-proper-part ?x ?y)))

(def-relation exact-location (?x ?y)
   :iff-def (exact-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation exact-location-of (?x ?y)
   :iff-def (exact-location ?y ?x)
   :avoid-infinite-loop t)

(def-rule exact-location-generic-location
 ((exact-location ?x ?y)
 if (generic-location ?x ?y)))

(def-relation physical-location (?x ?y)
   :iff-def (physical-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation physical-location-of (?x ?y)
   :iff-def (physical-location ?y ?x)
   :avoid-infinite-loop t)

(def-rule physical-location-exact-location
 ((physical-location ?x ?y)
 if (exact-location ?x ?y)))

(def-relation exact-location-of (?x ?y)
   :iff-def (exact-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation exact-location (?x ?y)
   :iff-def (exact-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule exact-location-of-generic-location-of
 ((exact-location-of ?x ?y)
 if (generic-location-of ?x ?y)))

(def-relation boundary (?x ?y)
   :iff-def (boundary-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation boundary-of (?x ?y)
   :iff-def (boundary ?y ?x)
   :avoid-infinite-loop t)

(def-rule boundary-proper-part
 ((boundary ?x ?y)
 if (proper-part ?x ?y)))

(def-relation mediated-relation (?x ?y)
   :iff-def (mediated-relation-i ?y ?x)
   :avoid-infinite-loop t)

(def-relation mediated-relation-i (?x ?y)
   :iff-def (mediated-relation ?y ?x)
   :avoid-infinite-loop t)

(def-relation host-of (?x ?y)
   :iff-def (host ?y ?x)
   :avoid-infinite-loop t)

(def-relation host (?x ?y)
   :iff-def (host-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule host-of-specific-constant-dependent
 ((host-of ?x ?y)
 if (specific-constant-dependent ?x ?y)))

(def-relation constant-participant (?x ?y)
   :iff-def (constant-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation constant-participant-in (?x ?y)
   :iff-def (constant-participant ?y ?x)
   :avoid-infinite-loop t)

(def-rule constant-participant-participant
 ((constant-participant ?x ?y)
 if (participant ?x ?y)))

(def-relation has-t-quality (?x ?y)
   :iff-def (t-inherent-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation t-inherent-in (?x ?y)
   :iff-def (has-t-quality ?y ?x)
   :avoid-infinite-loop t)

(def-rule has-t-quality-has-quality
 ((has-t-quality ?x ?y)
 if (has-quality ?x ?y)))

(def-relation temporary-atomic-part-of (?x ?y)
   :iff-def (temporary-atomic-part ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-atomic-part (?x ?y)
   :iff-def (temporary-atomic-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-atomic-part-of-temporary-proper-part-of
 ((temporary-atomic-part-of ?x ?y)
 if (temporary-proper-part-of ?x ?y)))

(def-relation mereologically-coincides (?x ?y)
   :iff-def (mereologically-coincides ?y ?x)
   :avoid-infinite-loop t)

(def-relation mereologically-coincides (?x ?y)
   :iff-def (mereologically-coincides ?y ?x)
   :avoid-infinite-loop t)

(def-rule mereologically-coincides-temporary-part
 ((mereologically-coincides ?x ?y)
 if (temporary-part ?x ?y)))

(def-relation generic-location-of (?x ?y)
   :iff-def (generic-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation generic-location (?x ?y)
   :iff-def (generic-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule generic-location-of-mediated-relation-i
 ((generic-location-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-relation weak-connection (?x ?y)
   :iff-def (weak-connection ?y ?x)
   :avoid-infinite-loop t)

(def-relation weak-connection (?x ?y)
   :iff-def (weak-connection ?y ?x)
   :avoid-infinite-loop t)

(def-rule weak-connection-immediate-relation
 ((weak-connection ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation identity-c (?x ?y)
   :iff-def (identity-c ?y ?x)
   :avoid-infinite-loop t)

(def-relation identity-c (?x ?y)
   :iff-def (identity-c ?y ?x)
   :avoid-infinite-loop t)

(def-rule identity-c-immediate-relation
 ((identity-c ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation identity-c (?x ?z)
   :sufficient (and (identity-c ?x ?y)
                   (identity-c ?y ?z)))

(def-relation atomic-part-of (?x ?y)
   :iff-def (atomic-part ?y ?x)
   :avoid-infinite-loop t)

(def-relation atomic-part (?x ?y)
   :iff-def (atomic-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule atomic-part-of-part-of
 ((atomic-part-of ?x ?y)
 if (part-of ?x ?y)))

(def-relation temporary-proper-part-of (?x ?y)
   :iff-def (temporary-proper-part ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-proper-part (?x ?y)
   :iff-def (temporary-proper-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-proper-part-of-temporary-part-of
 ((temporary-proper-part-of ?x ?y)
 if (temporary-part-of ?x ?y)))

(def-rule temporary-proper-part-of-proper-part-of
 ((temporary-proper-part-of ?x ?y)
 if (proper-part-of ?x ?y)))

(def-relation temporary-part (?x ?y)
   :iff-def (temporary-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-part-of (?x ?y)
   :iff-def (temporary-part ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-part-partly-compresent
 ((temporary-part ?x ?y)
 if (partly-compresent ?x ?y)))

(def-rule temporary-part-part
 ((temporary-part ?x ?y)
 if (part ?x ?y)))

(def-relation generic-constituent-of (?x ?y)
   :iff-def (generic-constituent ?y ?x)
   :avoid-infinite-loop t)

(def-relation generic-constituent (?x ?y)
   :iff-def (generic-constituent-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule generic-constituent-of-immediate-relation-i
 ((generic-constituent-of ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation generic-constituent (?x ?y)
   :iff-def (generic-constituent-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation generic-constituent-of (?x ?y)
   :iff-def (generic-constituent ?y ?x)
   :avoid-infinite-loop t)

(def-rule generic-constituent-immediate-relation
 ((generic-constituent ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation abstract-location-of (?x ?y)
   :iff-def (abstract-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation abstract-location (?x ?y)
   :iff-def (abstract-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule abstract-location-of-exact-location-of
 ((abstract-location-of ?x ?y)
 if (exact-location-of ?x ?y)))

(def-relation temporary-part-of (?x ?y)
   :iff-def (temporary-part ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-part (?x ?y)
   :iff-def (temporary-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-part-of-partly-compresent
 ((temporary-part-of ?x ?y)
 if (partly-compresent ?x ?y)))

(def-rule temporary-part-of-part-of
 ((temporary-part-of ?x ?y)
 if (part-of ?x ?y)))

(def-relation r-location-of (?x ?y)
   :iff-def (r-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation r-location (?x ?y)
   :iff-def (r-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule r-location-of-immediate-relation-i
 ((r-location-of ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation identity-n (?x ?y)
   :iff-def (identity-n ?y ?x)
   :avoid-infinite-loop t)

(def-relation identity-n (?x ?y)
   :iff-def (identity-n ?y ?x)
   :avoid-infinite-loop t)

(def-rule identity-n-immediate-relation
 ((identity-n ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation identity-n (?x ?z)
   :sufficient (and (identity-n ?x ?y)
                   (identity-n ?y ?z)))

(def-relation temporary-participant (?x ?y)
   :iff-def (temporary-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-participant-in (?x ?y)
   :iff-def (temporary-participant ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-participant-participant
 ((temporary-participant ?x ?y)
 if (participant ?x ?y)))

(def-relation host (?x ?y)
   :iff-def (host-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation host-of (?x ?y)
   :iff-def (host ?y ?x)
   :avoid-infinite-loop t)

(def-rule host-specifically-constantly-dependent-on
 ((host ?x ?y)
 if (specifically-constantly-dependent-on ?x ?y)))

(def-relation proper-part (?x ?y)
   :iff-def (proper-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation proper-part-of (?x ?y)
   :iff-def (proper-part ?y ?x)
   :avoid-infinite-loop t)

(def-rule proper-part-part
 ((proper-part ?x ?y)
 if (part ?x ?y)))

(def-relation proper-part (?x ?z)
   :sufficient (and (proper-part ?x ?y)
                   (proper-part ?y ?z)))

(def-relation participant-in (?x ?y)
   :iff-def (participant ?y ?x)
   :avoid-infinite-loop t)

(def-relation participant (?x ?y)
   :iff-def (participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-rule participant-in-immediate-relation-i
 ((participant-in ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation constant-participant-in (?x ?y)
   :iff-def (constant-participant ?y ?x)
   :avoid-infinite-loop t)

(def-relation constant-participant (?x ?y)
   :iff-def (constant-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-rule constant-participant-in-participant-in
 ((constant-participant-in ?x ?y)
 if (participant-in ?x ?y)))

(def-relation immediate-relation (?x ?y)
   :iff-def (immediate-relation-i ?y ?x)
   :avoid-infinite-loop t)

(def-relation immediate-relation-i (?x ?y)
   :iff-def (immediate-relation ?y ?x)
   :avoid-infinite-loop t)

(def-relation r-location (?x ?y)
   :iff-def (r-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation r-location-of (?x ?y)
   :iff-def (r-location ?y ?x)
   :avoid-infinite-loop t)

(def-rule r-location-immediate-relation
 ((r-location ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation generically-dependent-on (?x ?y)
   :iff-def (generic-dependent ?y ?x)
   :avoid-infinite-loop t)

(def-relation generic-dependent (?x ?y)
   :iff-def (generically-dependent-on ?y ?x)
   :avoid-infinite-loop t)

(def-rule generically-dependent-on-immediate-relation-i
 ((generically-dependent-on ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation immediate-relation-i (?x ?y)
   :iff-def (immediate-relation ?y ?x)
   :avoid-infinite-loop t)

(def-relation immediate-relation (?x ?y)
   :iff-def (immediate-relation-i ?y ?x)
   :avoid-infinite-loop t)

(def-relation specific-constant-constituent (?x ?y)
   :iff-def (specific-constant-constituent-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation specific-constant-constituent-of (?x ?y)
   :iff-def (specific-constant-constituent ?y ?x)
   :avoid-infinite-loop t)

(def-rule specific-constant-constituent-immediate-relation
 ((specific-constant-constituent ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation time-of-q-presence-of (?x ?y)
   :iff-def (q-present-at ?y ?x)
   :avoid-infinite-loop t)

(def-relation q-present-at (?x ?y)
   :iff-def (time-of-q-presence-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule time-of-q-presence-of-mediated-relation-i
 ((time-of-q-presence-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-relation sibling-part (?x ?y)
   :iff-def (sibling-part ?y ?x)
   :avoid-infinite-loop t)

(def-relation sibling-part (?x ?y)
   :iff-def (sibling-part ?y ?x)
   :avoid-infinite-loop t)

(def-rule sibling-part-mediated-relation
 ((sibling-part ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation has-quale (?x ?y)
   :iff-def (quale-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation quale-of (?x ?y)
   :iff-def (has-quale ?y ?x)
   :avoid-infinite-loop t)

(def-rule has-quale-q-location
 ((has-quale ?x ?y)
 if (q-location ?x ?y)))

(def-relation physical-location-of (?x ?y)
   :iff-def (physical-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation physical-location (?x ?y)
   :iff-def (physical-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule physical-location-of-exact-location-of
 ((physical-location-of ?x ?y)
 if (exact-location-of ?x ?y)))

(def-relation atomic-part (?x ?y)
   :iff-def (atomic-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation atomic-part-of (?x ?y)
   :iff-def (atomic-part ?y ?x)
   :avoid-infinite-loop t)

(def-rule atomic-part-part
 ((atomic-part ?x ?y)
 if (part ?x ?y)))

(def-relation generic-location (?x ?y)
   :iff-def (generic-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation generic-location-of (?x ?y)
   :iff-def (generic-location ?y ?x)
   :avoid-infinite-loop t)

(def-rule generic-location-mediated-relation
 ((generic-location ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation boundary-of (?x ?y)
   :iff-def (boundary ?y ?x)
   :avoid-infinite-loop t)

(def-relation boundary (?x ?y)
   :iff-def (boundary-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule boundary-of-proper-part-of
 ((boundary-of ?x ?y)
 if (proper-part-of ?x ?y)))

(def-relation abstract-location (?x ?y)
   :iff-def (abstract-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation abstract-location-of (?x ?y)
   :iff-def (abstract-location ?y ?x)
   :avoid-infinite-loop t)

(def-rule abstract-location-exact-location
 ((abstract-location ?x ?y)
 if (exact-location ?x ?y)))

(def-relation participant (?x ?y)
   :iff-def (participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation participant-in (?x ?y)
   :iff-def (participant ?y ?x)
   :avoid-infinite-loop t)

(def-rule participant-immediate-relation
 ((participant ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation total-constant-participant (?x ?y)
   :iff-def (total-constant-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation total-constant-participant-in (?x ?y)
   :iff-def (total-constant-participant ?y ?x)
   :avoid-infinite-loop t)

(def-rule total-constant-participant-constant-participant
 ((total-constant-participant ?x ?y)
 if (constant-participant ?x ?y)))

(def-relation generic-dependent (?x ?y)
   :iff-def (generically-dependent-on ?y ?x)
   :avoid-infinite-loop t)

(def-relation generically-dependent-on (?x ?y)
   :iff-def (generic-dependent ?y ?x)
   :avoid-infinite-loop t)

(def-rule generic-dependent-immediate-relation
 ((generic-dependent ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation specifically-constantly-dependent-on (?x ?y)
   :iff-def (specific-constant-dependent ?y ?x)
   :avoid-infinite-loop t)

(def-relation specific-constant-dependent (?x ?y)
   :iff-def (specifically-constantly-dependent-on ?y ?x)
   :avoid-infinite-loop t)

(def-rule specifically-constantly-dependent-on-immediate-relation-i
 ((specifically-constantly-dependent-on ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation sibling-part (?x ?y)
   :iff-def (sibling-part ?y ?x)
   :avoid-infinite-loop t)

(def-rule life-of-constant-participant
 ((life-of ?x ?y)
 if (constant-participant ?x ?y)))

(def-relation part-of (?x ?y)
   :iff-def (part ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-part-part
 ((temporary-part ?x ?y)
 if (part ?x ?y)))

(def-rule has-quality-immediate-relation-i
 ((has-quality ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-rule abstract-location-exact-location
 ((abstract-location ?x ?y)
 if (exact-location ?x ?y)))

(def-relation q-present-at (?x ?y)
   :iff-def (time-of-q-presence-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation proper-part (?x ?y)
   :iff-def (proper-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule overlaps-mediated-relation
 ((overlaps ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation immediate-relation-i (?x ?y)
   :iff-def (immediate-relation ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-proper-part (?x ?y)
   :iff-def (temporary-proper-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule has-quale-q-location
 ((has-quale ?x ?y)
 if (q-location ?x ?y)))

(def-relation proper-part (?x ?z)
   :sufficient (and (proper-part ?x ?y)
                   (proper-part ?y ?z)))

(def-relation specific-constant-constituent-of (?x ?y)
   :iff-def (specific-constant-constituent ?y ?x)
   :avoid-infinite-loop t)

(def-rule atomic-part-part
 ((atomic-part ?x ?y)
 if (part ?x ?y)))

(def-relation has-quale (?x ?y)
   :iff-def (quale-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation immediate-relation (?x ?y)
   :iff-def (immediate-relation-i ?y ?x)
   :avoid-infinite-loop t)

(def-relation host (?x ?y)
   :iff-def (host-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation generically-dependent-on (?x ?y)
   :iff-def (generic-dependent ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-proper-part-temporary-part
 ((temporary-proper-part ?x ?y)
 if (temporary-part ?x ?y)))

(def-relation partly-compresent (?x ?y)
   :iff-def (partly-compresent ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-proper-part-of-temporary-part-of
 ((temporary-proper-part-of ?x ?y)
 if (temporary-part-of ?x ?y)))

(def-relation generic-location-of (?x ?y)
   :iff-def (generic-location ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-part-partly-compresent
 ((temporary-part ?x ?y)
 if (partly-compresent ?x ?y)))

(def-relation total-constant-participant (?x ?y)
   :iff-def (total-constant-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation overlaps (?x ?y)
   :iff-def (overlaps ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-part-of (?x ?y)
   :iff-def (temporary-part ?y ?x)
   :avoid-infinite-loop t)

(def-rule participant-immediate-relation
 ((participant ?x ?y)
 if (immediate-relation ?x ?y)))

(def-rule temporary-atomic-part-temporary-proper-part
 ((temporary-atomic-part ?x ?y)
 if (temporary-proper-part ?x ?y)))

(def-rule temporary-participant-participant
 ((temporary-participant ?x ?y)
 if (participant ?x ?y)))

(def-rule boundary-proper-part
 ((boundary ?x ?y)
 if (proper-part ?x ?y)))

(def-relation strong-connection (?x ?y)
   :iff-def (strong-connection ?y ?x)
   :avoid-infinite-loop t)

(def-relation total-temporary-participant (?x ?y)
   :iff-def (total-temporary-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation q-location-of (?x ?y)
   :iff-def (q-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation specific-constant-dependent (?x ?y)
   :iff-def (specifically-constantly-dependent-on ?y ?x)
   :avoid-infinite-loop t)

(def-rule specific-constant-dependent-immediate-relation
 ((specific-constant-dependent ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation exact-location (?x ?y)
   :iff-def (exact-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule generic-location-mediated-relation
 ((generic-location ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation identity-n (?x ?y)
   :iff-def (identity-n ?y ?x)
   :avoid-infinite-loop t)

(def-rule constant-participant-participant
 ((constant-participant ?x ?y)
 if (participant ?x ?y)))

(def-relation physical-location (?x ?y)
   :iff-def (physical-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule exact-location-generic-location
 ((exact-location ?x ?y)
 if (generic-location ?x ?y)))

(def-relation proper-part-of (?x ?y)
   :iff-def (proper-part ?y ?x)
   :avoid-infinite-loop t)

(def-rule weak-connection-immediate-relation
 ((weak-connection ?x ?y)
 if (immediate-relation ?x ?y)))

(def-rule abstract-location-of-exact-location-of
 ((abstract-location-of ?x ?y)
 if (exact-location-of ?x ?y)))

(def-rule inherent-in-immediate-relation
 ((inherent-in ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation abstract-location (?x ?y)
   :iff-def (abstract-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule atomic-part-of-part-of
 ((atomic-part-of ?x ?y)
 if (part-of ?x ?y)))

(def-rule total-temporary-participant-temporary-participant
 ((total-temporary-participant ?x ?y)
 if (temporary-participant ?x ?y)))

(def-relation part-of (?x ?z)
   :sufficient (and (part-of ?x ?y)
                   (part-of ?y ?z)))

(def-rule partly-compresent-mediated-relation
 ((partly-compresent ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation inherent-in (?x ?y)
   :iff-def (has-quality ?y ?x)
   :avoid-infinite-loop t)

(def-rule r-location-immediate-relation
 ((r-location ?x ?y)
 if (immediate-relation ?x ?y)))

(def-rule host-of-specific-constant-dependent
 ((host-of ?x ?y)
 if (specific-constant-dependent ?x ?y)))

(def-relation abstract-location-of (?x ?y)
   :iff-def (abstract-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation r-location-of (?x ?y)
   :iff-def (r-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation generic-location (?x ?y)
   :iff-def (generic-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule temporary-atomic-part-of-temporary-proper-part-of
 ((temporary-atomic-part-of ?x ?y)
 if (temporary-proper-part-of ?x ?y)))

(def-relation r-location (?x ?y)
   :iff-def (r-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule total-temporary-participant-in-temporary-participant-in
 ((total-temporary-participant-in ?x ?y)
 if (temporary-participant-in ?x ?y)))

(def-relation temporary-proper-part-of (?x ?y)
   :iff-def (temporary-proper-part ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-atomic-part (?x ?y)
   :iff-def (temporary-atomic-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation q-location (?x ?y)
   :iff-def (q-location-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation specifically-constantly-dependent-on (?x ?y)
   :iff-def (specific-constant-dependent ?y ?x)
   :avoid-infinite-loop t)

(def-relation atomic-part (?x ?y)
   :iff-def (atomic-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule specifically-constantly-dependent-on-immediate-relation-i
 ((specifically-constantly-dependent-on ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-rule specific-constant-constituent-immediate-relation
 ((specific-constant-constituent ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation time-of-q-presence-of (?x ?y)
   :iff-def (q-present-at ?y ?x)
   :avoid-infinite-loop t)

(def-relation constant-participant (?x ?y)
   :iff-def (constant-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-rule total-constant-participant-constant-participant
 ((total-constant-participant ?x ?y)
 if (constant-participant ?x ?y)))

(def-relation participant (?x ?y)
   :iff-def (participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation identity-c (?x ?z)
   :sufficient (and (identity-c ?x ?y)
                   (identity-c ?y ?z)))

(def-rule host-specifically-constantly-dependent-on
 ((host ?x ?y)
 if (specifically-constantly-dependent-on ?x ?y)))

(def-rule sibling-part-mediated-relation
 ((sibling-part ?x ?y)
 if (mediated-relation ?x ?y)))

(def-rule has-t-quality-has-quality
 ((has-t-quality ?x ?y)
 if (has-quality ?x ?y)))

(def-rule part-immediate-relation
 ((part ?x ?y)
 if (immediate-relation ?x ?y)))

(def-rule total-constant-participant-in-constant-participant-in
 ((total-constant-participant-in ?x ?y)
 if (constant-participant-in ?x ?y)))

(def-rule quale-of-q-location-of
 ((quale-of ?x ?y)
 if (q-location-of ?x ?y)))

(def-rule temporary-proper-part-proper-part
 ((temporary-proper-part ?x ?y)
 if (proper-part ?x ?y)))

(def-rule q-location-immediate-relation
 ((q-location ?x ?y)
 if (immediate-relation ?x ?y)))

(def-rule life-constant-participant-in
 ((life ?x ?y)
 if (constant-participant-in ?x ?y)))

(def-rule boundary-of-proper-part-of
 ((boundary-of ?x ?y)
 if (proper-part-of ?x ?y)))

(def-relation spatio-temporally-present-at (?x ?y)
   :iff-def (spatio-temporal-presence-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule generic-constituent-of-immediate-relation-i
 ((generic-constituent-of ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-rule generic-dependent-immediate-relation
 ((generic-dependent ?x ?y)
 if (immediate-relation ?x ?y)))

(def-rule temporary-proper-part-of-proper-part-of
 ((temporary-proper-part-of ?x ?y)
 if (proper-part-of ?x ?y)))

(def-rule specific-constant-constituent-of-immediate-relation-i
 ((specific-constant-constituent-of ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation constant-participant-in (?x ?y)
   :iff-def (constant-participant ?y ?x)
   :avoid-infinite-loop t)

(def-rule mereologically-coincides-temporary-part
 ((mereologically-coincides ?x ?y)
 if (temporary-part ?x ?y)))

(def-relation boundary-of (?x ?y)
   :iff-def (boundary ?y ?x)
   :avoid-infinite-loop t)

(def-relation mediated-relation (?x ?y)
   :iff-def (mediated-relation-i ?y ?x)
   :avoid-infinite-loop t)

(def-rule physical-location-exact-location
 ((physical-location ?x ?y)
 if (exact-location ?x ?y)))

(def-relation life-of (?x ?y)
   :iff-def (life ?y ?x)
   :avoid-infinite-loop t)

(def-rule generic-constituent-immediate-relation
 ((generic-constituent ?x ?y)
 if (immediate-relation ?x ?y)))

(def-rule temporary-part-of-part-of
 ((temporary-part-of ?x ?y)
 if (part-of ?x ?y)))

(def-rule generic-location-of-mediated-relation-i
 ((generic-location-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-rule generically-dependent-on-immediate-relation-i
 ((generically-dependent-on ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation physical-location-of (?x ?y)
   :iff-def (physical-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation mediated-relation-i (?x ?y)
   :iff-def (mediated-relation ?y ?x)
   :avoid-infinite-loop t)

(def-relation spatio-temporal-presence-of (?x ?y)
   :iff-def (spatio-temporally-present-at ?y ?x)
   :avoid-infinite-loop t)

(def-rule physical-location-of-exact-location-of
 ((physical-location-of ?x ?y)
 if (exact-location-of ?x ?y)))

(def-rule part-of-immediate-relation-i
 ((part-of ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation host-of (?x ?y)
   :iff-def (host ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-participant (?x ?y)
   :iff-def (temporary-participant-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation proper-part-of (?x ?z)
   :sufficient (and (proper-part-of ?x ?y)
                   (proper-part-of ?y ?z)))

(def-relation total-constant-participant-in (?x ?y)
   :iff-def (total-constant-participant ?y ?x)
   :avoid-infinite-loop t)

(def-relation boundary (?x ?y)
   :iff-def (boundary-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule constant-participant-in-participant-in
 ((constant-participant-in ?x ?y)
 if (participant-in ?x ?y)))

(def-rule temporary-part-of-partly-compresent
 ((temporary-part-of ?x ?y)
 if (partly-compresent ?x ?y)))

(def-relation participant-in (?x ?y)
   :iff-def (participant ?y ?x)
   :avoid-infinite-loop t)

(def-relation generic-constituent-of (?x ?y)
   :iff-def (generic-constituent ?y ?x)
   :avoid-infinite-loop t)

(def-relation quale-of (?x ?y)
   :iff-def (has-quale ?y ?x)
   :avoid-infinite-loop t)

(def-rule t-inherent-in-inherent-in
 ((t-inherent-in ?x ?y)
 if (inherent-in ?x ?y)))

(def-relation t-inherent-in (?x ?y)
   :iff-def (has-t-quality ?y ?x)
   :avoid-infinite-loop t)

(def-rule q-location-of-immediate-relation-i
 ((q-location-of ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation part (?x ?y)
   :iff-def (part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule identity-c-immediate-relation
 ((identity-c ?x ?y)
 if (immediate-relation ?x ?y)))

(def-rule participant-in-immediate-relation-i
 ((participant-in ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-rule time-of-q-presence-of-mediated-relation-i
 ((time-of-q-presence-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-relation part (?x ?z)
   :sufficient (and (part ?x ?y)
                   (part ?y ?z)))

(def-rule spatio-temporally-present-at-exact-location
 ((spatio-temporally-present-at ?x ?y)
 if (exact-location ?x ?y)))

(def-rule identity-n-immediate-relation
 ((identity-n ?x ?y)
 if (immediate-relation ?x ?y)))

(def-relation mereologically-coincides (?x ?y)
   :iff-def (mereologically-coincides ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-part (?x ?y)
   :iff-def (temporary-part-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule proper-part-part
 ((proper-part ?x ?y)
 if (part ?x ?y)))

(def-relation generic-constituent (?x ?y)
   :iff-def (generic-constituent-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule exact-location-of-generic-location-of
 ((exact-location-of ?x ?y)
 if (generic-location-of ?x ?y)))

(def-rule spatio-temporal-presence-of-exact-location-of
 ((spatio-temporal-presence-of ?x ?y)
 if (exact-location-of ?x ?y)))

(def-relation generic-dependent (?x ?y)
   :iff-def (generically-dependent-on ?y ?x)
   :avoid-infinite-loop t)

(def-relation identity-c (?x ?y)
   :iff-def (identity-c ?y ?x)
   :avoid-infinite-loop t)

(def-rule strong-connection-mediated-relation
 ((strong-connection ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation total-temporary-participant-in (?x ?y)
   :iff-def (total-temporary-participant ?y ?x)
   :avoid-infinite-loop t)

(def-rule r-location-of-immediate-relation-i
 ((r-location-of ?x ?y)
 if (immediate-relation-i ?x ?y)))

(def-relation identity-n (?x ?z)
   :sufficient (and (identity-n ?x ?y)
                   (identity-n ?y ?z)))

(def-rule temporary-participant-in-participant-in
 ((temporary-participant-in ?x ?y)
 if (participant-in ?x ?y)))

(def-rule proper-part-of-part-of
 ((proper-part-of ?x ?y)
 if (part-of ?x ?y)))

(def-relation has-quality (?x ?y)
   :iff-def (inherent-in ?y ?x)
   :avoid-infinite-loop t)

(def-rule q-present-at-mediated-relation
 ((q-present-at ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation exact-location-of (?x ?y)
   :iff-def (exact-location ?y ?x)
   :avoid-infinite-loop t)

(def-relation life (?x ?y)
   :iff-def (life-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation has-t-quality (?x ?y)
   :iff-def (t-inherent-in ?y ?x)
   :avoid-infinite-loop t)

(def-relation atomic-part-of (?x ?y)
   :iff-def (atomic-part ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-atomic-part-of (?x ?y)
   :iff-def (temporary-atomic-part ?y ?x)
   :avoid-infinite-loop t)

(def-relation temporary-participant-in (?x ?y)
   :iff-def (temporary-participant ?y ?x)
   :avoid-infinite-loop t)

(def-relation specific-constant-constituent (?x ?y)
   :iff-def (specific-constant-constituent-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation weak-connection (?x ?y)
   :iff-def (weak-connection ?y ?x)
   :avoid-infinite-loop t)



;-_-_-_-_-_-_-_-_-_-_-_-_-_- DEFINITION OF ANONYMOUS ITEMS-_-_-_-_-_-_-_-_-_-_-_-_-_-



;-_-_-_-_-_-_-_-_-_-_-_-_-_- DEFINITION OF CLASSES-_-_-_-_-_-_-_-_-_-_-_-_-_-


(def-class Thing () 
 "Comment Here"
)

(def-class particular () 
 "AKA 'entity'.Any individual in the DOLCE domain of discourse. The extensional coverage of DOLCE is as large as possible, since it ranges on 'possibilia', i.e all possible individuals that can be postulated by means of DOLCE axioms. Possibilia include physical objects, substances, processes, qualities,  conceptual regions, non-physical objects, collections and even arbitrary sums of objects.The class 'particular' features a covering partition that includes: endurant, perdurant, quality, and abstract. There are also some subclasses defined as unions of subclasses of 'particular' for special purposes: spatio-temporal-particular (any particular except abstracts)- physical-realization (any realization of an information object, defined in the ExtendedDnS ontology)."
)

(def-class abstract (particular) ?c
 "The main characteristic of abstract entities is that  they do not have spatial nor temporal qualities, and they are not qualities themselves.  The only class of abstract entities we consider in the present version of the upper  ontology is that of quality regions (or simply regions). Quality spaces are special  kinds of quality regions, being mereological sums of all the regions related to a certain quality type. The other examples of abstract entities (sets and facts) are only  indicative."
:iff-def (and  (exists ?a
 (and (has-quality ?c ?a)
	(not ( temporal-location_q ?a  ))))
 (exists ?b
 (and (has-quality ?c ?b)
	(not ( spatial-location_q ?b  ))))
)
:constraint  (and ( (not ( perdurant ?c ))
 (not ( quality ?c ))
))
)

(def-class region (abstract) 
 "We distinguish between a quality (e.g., the color  of a specific rose), and its value (e.g., a particular shade of red). The latter  is called quale, and describes the position of an individual quality within a certain  conceptual space (called here quality space) Gardenfors (2000). So when we say that  two roses have (exactly) the same color, we mean that their color qualities, which  are distinct, have the same position in the color space, that is they have the same  color quale."
((part :type region)
)
)

(def-class temporal-region (region) 
 "A region at which only temporal qualities can be  directly located. It assumes a metrics for time."
((part :type temporal-region)
(q-location-of :type temporal-quality)
)
)

(def-class time-interval (temporal-region) 
 "A temporal region, measured according to a calendar."
)

(def-class spatio-temporal-particular (particular) ?a
"Dummy class for optimizing some property universes. It includes all entities that are not reifications of universals ('abstracts'), i.e. those entities that are in space-time."
:iff-def   ( and ( or ( endurant ?a  )
( perdurant ?a  )
( quality ?a  )
)
( particular ?a   )
)

:avoid-infinite-loop t
 )

(def-class perdurant (spatio-temporal-particular) ?b
 "Perdurants (AKA occurrences) comprise what are variously called events, processes, phenomena, activities and states. They can have temporal parts or spatial parts. For instance, the first movement of (an execution of) a symphony is a temporal part of the symphony. On the other hand, the play performed by the left side of the orchestra is a spatial part. In both cases, these parts are occurrences themselves. We assume that objects cannot be parts of occurrences, but rather they participate in them. Perdurants extend in time by accumulating different temporal parts, so that, at any time they are present, they are only partially present, in  the sense that some of their proper temporal parts (e.g., their previous or future phases) may be not present. E.g., the piece of paper you are reading now is wholly present, while some temporal parts of your reading are not present yet, or any more. Philosophers say that endurants are entities that are in time, while lacking temporal parts (so to speak, all their parts flow with them in time). Perdurants, on the contrary, are entities that happen in time, and can have temporal parts (all their parts are fixed in time)."
((participant :min-cardinality 1)
(part :type perdurant)
(specific-constant-constituent :type perdurant)
(has-quality :min-cardinality 1 :type temporal-quality)
)
:iff-def (and  (exists ?a
 (and (has-quality ?b ?a)
	(temporal-location_q ?a)))
 (exists ?a
 (and (participant ?b ?a)
	(endurant ?a)))
)
:constraint  ( (not ( quality ?b ))
)
)

(def-class stative (perdurant) 
 "An occurrence-type is stative or eventive according  to whether it holds of the mereological sum of two of its instances, i.e. if it is  cumulative or not. A sitting occurrence is stative since the sum of two sittings  is still a sitting occurrence."
)

(def-class process (stative) 
 "Within stative occurrences, we distinguish between states and processes     according to homeomericity: sitting is classified as a state but running     is classified as a process, since there are (very short) temporal parts of     a running that are not themselves runnings. In general, processes differ     from situations because they are not assumed to have a description from     which they depend. They can be sequenced by some course, but they do not     require a description as a unifying criterion. On the other hand, at any     time, one can conceive a description that asserts the constraints by which     a process of a certian type is such, and in this case, it becomes a     situation. Since the decision of designing an explicit description that     unifies a perdurant depends on context, task, interest, application, etc.,     when aligning an ontology do DLP, there can be indecision on where to     align a process-oriented class. For example, in the WordNet alignment, we     have decided to put only some physical processes under 'process', e.g.     'organic process', in order to stress the social orientedness of DLP. But     whereas we need to talk explicitly of the criteria by which we conceive     organic processes, these will be put under 'situation'. Similar     considerations are made for the other types of perdurants in DOLCE. A     different notion of event (dealing with change) is currently investigated     for further developments: being 'achievement', 'accomplishment', 'state',     'event', etc. can be also considered 'aspects' of processes or of parts of     them. For example, the same process 'rock erosion in the Sinni valley' can     be conceptualized as an accomplishment (what has brought the current state     that e.g. we are trying to explain), as an achievement (the erosion     process as the result of a previous accomplishment), as a state (if we     collapse the time interval of the erosion into a time point), or as an     event (what has changed our focus from a state to another). In the erosion     case, we could have good motivations to shift from one aspect to another:     a) causation focus, b) effectual focus, c) condensation d) transition     (causality). If we want to consider all the aspects of a process together,     we need to postulate a unifying descriptive set of criteria (i.e. a     'description'), according to which that process is circumstantiated in a     'situation'. The different aspects will arise as a parts of a same situation."
)

(def-class state (stative) 
 "Within stative occurrences, we distinguish between  states and processes according to homeomericity: sitting is classified as a state  but running is classified as a process, since there are (very short) temporal parts  of a running that are not themselves runnings.In general, states differ from situations because they are not assumed to have a description from which they depend. They can be sequenced by some course, but they do not require a description as a unifying criterion.On the other hand, at any time, one can conceive a description that asserts the constraints by which a state of a certian type is such, and in this case, it becomes a situation.Since the decision of designing an explicit description that unifies a perdurant depends on context, task, interest, application, etc., when aligning an ontology do DLP, there can be indecision on where to align a state-oriented class. For example, in the WordNet alignment, we have decided to put only some physical states under 'state', e.g. 'turgor', in order to stress the social orientedness of DLP. But whereas we need to talk explicitly of the criteria by which we conceive turgor states, these will be put under 'situation'.Similar considerations are made for the other types of perdurants in DOLCE.A different notion of event (dealing with change) is currently investigated for further developments: being 'achievement', 'accomplishment', 'state', 'event', etc. can be also considered 'aspects' of processes or of parts of them. For example, the same process 'rock erosion in the Sinni valley' can be conceptualized as an accomplishment (what has brought the current state that e.g. we are trying to explain), as an achievement (the erosion process as the result of a previous accomplishment), as a state (if we collapse the time interval of the erosion into a time point), or as an event (what has changed our focus from a state to another).In the erosion case, we could have good motivations to shift from one aspect to another: a) causation focus, b) effectual focus, c) condensation d) transition (causality).If we want to consider all the aspects of a process together, we need to postulate a unifying descriptive set of criteria (i.e. a 'description'), according to which that process is circumstantiated in a 'situation'. The different aspects will arise as a parts of a same situation."
)

(def-class endurant (spatio-temporal-particular) ?b
 "The main characteristic of endurants is that all of them are independent essential wholes. This does not mean that the corresponding property (being an endurant) carries proper unity, since there is  no common unity criterion for endurants. Endurants can 'genuinely' change in time,  in the sense that the very same endurant as a whole can have incompatible properties at different times. To see this, suppose that an endurant - say 'this paper' - has a  property at a time t 'it's white', and a different, incompatible property at time t'  'it's yellow': in both cases we refer to the whole object, without picking up any  particular part of it. Within endurants, we distinguish between physical and non-physical  endurants, according to whether they have direct spatial qualities. Within physical  endurants, we distinguish between amounts of matter, objects, and features."
((specific-constant-constituent :type endurant)
(part :type endurant)
(participant-in :min-cardinality 1)
)
:iff-def  (exists ?a
 (and (participant-in ?b ?a)
	(perdurant ?a)))

:constraint  (and ( (not ( perdurant ?b ))
 (not ( abstract ?b ))
 (not ( quality ?b ))
))
)

(def-class physical-endurant (endurant) ?c
 "An endurant having a direct physical (at least spatial) quality."
((specific-constant-constituent :type physical-endurant)
(part :type physical-endurant)
(has-quality :min-cardinality 1 :type physical-quality :min-cardinality 1)
)
:iff-def (and  (exists ?a
 (and (has-quality ?c ?a)
	(spatial-location_q ?a)))
 (exists ?b
 (and (has-quality ?c ?b)
	(physical-quality ?b)))
)
:constraint  (and ( (not ( non-physical-endurant ?c ))
 (not ( arbitrary-sum ?c ))
))
)

(def-class feature (physical-endurant) ?a
 "Features are 'parasitic entities', that exist insofar their host exists. Typical examples of features are holes, bumps, boundaries, or spots of color. Features may be relevant parts of their host, like a bump or an edge, or dependent regions like a hole in a piece of cheese, the underneath of a table, the front of a house, or the shadow of a tree, which are not parts of their host. All features are essential wholes, but no common unity criterion may exist for all of them. However, typical features have a topological unity, as they are singular entities.Here only features of physical endurants are considered."
((host :min-cardinality 1)
)
:iff-def  (exists ?a
 (and (host ?a ?a)
	(physical-endurant ?a)))

:constraint  ( (not ( amount-of-matter ?a ))
)
)

(def-class event (perdurant) 
 "An occurrence-type is stative or eventive according  to whether it holds of the mereological sum of two of its instances, i.e. if it is cumulative or not. A sitting occurrence is stative since the sum of two sittings is still a sitting occurrence.In general, events differ from situations because they are not assumed to have a description from which they depend. They can be sequenced by some course, but they do not require a description as a unifying criterion.On the other hand, at any time, one can conceive a description that asserts the constraints by which an event of a certian type is such, and in this case, it becomes a situation.Since the decision of designing an explicit description that unifies a perdurant depends on context, task, interest, application, etc., when aligning an ontology do DLP, there can be indecision on where to align an event-oriented class. For example, in the WordNet alignment, we have decided to put only some physical events under 'event', e.g. 'discharge', in order to stress the social orientedness of DLP. But whereas we need to talk explicitly of the criteria by which we conceive discharge events, these will be put under 'situation'.Similar considerations are made for the other types of perdurants in DOLCE.A different notion of event (dealing with change) is currently investigated for further developments: being 'achievement', 'accomplishment', 'state', 'event', etc. can be also considered 'aspects' of processes or of parts of them. For example, the same process 'rock erosion in the Sinni valley' can be conceptualized as an accomplishment (what has brought the current state that e.g. we are trying to explain), as an achievement (the erosion process as the result of a previous accomplishment), as a state (if we collapse the time interval of the erosion into a time point), or as an event (what has changed our focus from a state to another).In the erosion case, we could have good motivations to shift from one aspect to another: a) causation focus, b) effectual focus, c) condensation d) transition (causality).If we want to consider all the aspects of a process together, we need to postulate a unifying descriptive set of criteria (i.e. a 'description'), according to which that process is circumstantiated in a 'situation'. The different aspects will arise as a parts of a same situation."
)

(def-class accomplishment (event) 
 "Eventive occurrences (events) are called achievements if they are atomic, otherwise they are accomplishments.Further developments: being 'achievement', 'accomplishment', 'state', 'event', etc. can be also considered 'aspects' of processes or of parts of them. For example, the same process 'rock erosion in the Sinni valley' can be seen as an accomplishment (what has brought the current state that e.g. we are trying to explain), as an achievement (the erosion process as the result of a previous accomplishment), as a state (collapsing the time interval of the erosion into a time point), as an event (what has changed our focus from a state to another).In the erosion case, we could have good motivations to shift from one aspect to another: a) causation focus, b) effectual focus, c) condensation d) transition (causality)."
)

(def-class physical-object (physical-endurant) ?a
 "The main characteristic of physical objects is that  they are endurants with unity. However, they have no common unity criterion, since  different subtypes of objects may  have different unity criteria. Differently from  aggregates, (most) physical objects change some of their parts while keeping their  identity, they can have therefore temporary parts. Often physical objects (indeed,  all endurants) are ontologically independent from occurrences (discussed below).  However, if we admit that every object has a life, it is hard to exclude a mutual  specific constant dependence between the two. Nevertheless, we may still use the  notion of dependence to (weakly) characterize objects as being not specifically  constantly dependent on other objects."
:constraint  (and ( (not ( feature ?a ))
 (not ( amount-of-matter ?a ))
))
)

(def-class quality (spatio-temporal-particular) ?a
 "Qualities can be seen as the basic entities we can  perceive or measure: shapes, colors, sizes, sounds, smells, as well as weights, lengths,  electrical charges... 'Quality' is often used as a synonymous of 'property', but this is  not the case in this upper ontology: qualities are particulars, properties are universals.  Qualities inhere to entities: every entity (including qualities themselves) comes with  certain qualities, which exist as long as the entity exists."
((inherent-in :min-cardinality 1)
)
:iff-def  (exists ?a
 (and (inherent-in ?a ?a)
	(particular ?a)))

)

(def-class temporal-quality (quality) ?a
 "A quality inherent in a perdurant."
((inherent-in :min-cardinality 1)
(q-location :type temporal-region)
(has-quality :type temporal-quality)
)
:iff-def  (exists ?a
 (and (inherent-in ?a ?a)
	(perdurant ?a)))

:constraint  (and ( (not ( abstract-quality ?a ))
 (not ( physical-quality ?a ))
))
)

(def-class temporal-location_q (temporal-quality) 
 "A temporal location quality."
)

(def-class physical-quality (quality) ?a
 "A quality inherent in a physical endurant."
((inherent-in :min-cardinality 1)
(q-location :type physical-region)
(has-quality :type physical-quality)
)
:iff-def  (exists ?a
 (and (inherent-in ?a ?a)
	(physical-endurant ?a)))

:constraint  ( (not ( abstract-quality ?a ))
)
)

(def-class spatial-location_q (physical-quality) 
 "A physical quality, q-located in (whose value is given within) ordinary spaces (geographical coordinates, cosmological positions, anatomical axes, etc.)."
)

(def-class amount-of-matter (physical-endurant) 
 "The common trait of amounts of matter is that they are endurants with no unity (according to Gangemi et a. 2001 none of them is an essential  whole). Amounts of matter - 'stuffs' referred to by mass nouns like 'gold', 'iron', 'wood',  'sand', 'meat', etc. - are mereologically  invariant, in the sense that they change their  identity when they change some parts."
)

(def-class dependent-place (feature) 
 "A feature that is not part of its host, like a hole in a piece of cheese, the underneath of a table, the front of a house, or the shadow of a tree."
)

(def-class quale (region) ?a
"An atomic region."
((atomic-part-of :min-cardinality 1)
)
:iff-def   ( and ( region ?a   )
( exists ?a
 (and (atomic-part-of ?a ?a)
	(region ?a))
 )
)

:avoid-infinite-loop t
 )

(def-class physical-region (region) ?a
 "A region at which only physical qualities can be  directly located. It assumes some metrics for physical properties."
((part :type physical-region)
(q-location-of :type physical-quality)
)
:constraint  (and ( (not ( temporal-region ?a ))
 (not ( abstract-region ?a ))
))
)

(def-class space-region (physical-region) 
 "An ordinary space: geographical, cosmological, anatomical, topographic, etc."
((part :type space-region)
(q-location-of :type spatial-location_q)
)
)

(def-class spatio-temporal-region (space-region) 
 "Any region resulting from the composition of a space region with a temporal region, i.e. being present in region r at time t."
)

(def-class set (abstract) 
 "A mathematical set."
)

(def-class achievement (event) 
 "Eventive occurrences (events) are called achievements  if they are atomic, otherwise they are accomplishments.Further developments: being 'achievement', 'accomplishment', 'state', 'event', etc. can be also considered 'aspects' of processes or of parts of them. For example, the same process 'rock erosion in the Sinni valley' can be seen as an accomplishment (what has brought the current state that e.g. we are trying to explain), as an achievement (the erosion process as the result of a previous accomplishment), as a state (collapsing the time interval of the erosion into a time point), as an event (what has changed our focus from a state to another).In the erosion case, we could have good motivations to shift from one aspect to another: a) causation focus, b) effectual focus, c) condensation d) transition (causality)."
)

(def-class relevant-part (feature) 
 "Features that are relevant parts of their host, like a bump or an edge."
)

(def-class arbitrary-sum (endurant) ?a
 "AKA arbitrary-collection.The mereological sum of any two or more endurants (physical or not). Arbitrary sums have no unity criterion (they are 'extensional')."
((part :min-cardinality 1)
)
:iff-def  (exists ?a
 (and (part ?a ?a)
	(endurant ?a)))

:constraint  ( (not ( non-physical-endurant ?a ))
)
)

(def-class non-physical-endurant (endurant) 
 "An endurant with no mass, generically constantly depending on some agent. Non-physical endurants can have physical constituents (e.g. in the case of members of a collection)."
((part :type non-physical-endurant)
(has-quality :type abstract-quality)
)
)

(def-class non-physical-object (non-physical-endurant) ?a
 "Formerly known as description. A unitary endurant with no mass (non-physical), generically constantly depending on some agent, on some communication act, and indirectly on some agent participating in that act. Both descriptions (in the now current sense) and concepts are non-physical objects."
((part :type non-physical-object)
(generically-dependent-on :min-cardinality 1)
)
:iff-def  (exists ?a
 (and (generically-dependent-on ?a ?a)
	(physical-endurant ?a)))

)

(def-class abstract-quality (quality) ?b
 "A quality inherent in a non-physical endurant."
((inherent-in :min-cardinality 1)
(q-location :type abstract-region)
(has-quality :type abstract-quality)
)
:iff-def  (exists ?a
 (and (inherent-in ?b ?a)
	(non-physical-endurant ?a)))

)

(def-class abstract-region (region) ?a
 "A region at which only abstract qualities can be directly located. It assumes some metrics for abstract (neither physical nor temporal) properties."
((part :type abstract-region)
(q-location-of :type abstract-quality)
)
:constraint  ( (not ( temporal-region ?a ))
)
)

(def-class proposition (abstract) 
 "The abstract content of a proposition. Abstract content is purely combinatorial: from this viewpoint, any content that can be generated by means of combinatorial rules is assumed to exist in the domain of quantification (reified abstracts)."
)

(def-class quality-space (region) ?b
"A quality space is a topologically maximal region. The constraint of maximality cannot be given completely in OWL, but a constraint is given that creates a partition out of all quality spaces (e.g. no two quality spaces can overlap mereologically)."
:iff-def   ( and ( region ?b   )
( exists ?a
 (and (overlaps ?b ?a)
	(not ( quality-space ?a  )))
 )
)

:avoid-infinite-loop t
 )



;-_-_-_-_-_-_-_-_-_-_-_-_-_- DEFINITION OF INDIVIDUALS-_-_-_-_-_-_-_-_-_-_-_-_-_-




;-_-_-_-_-_-_-_-_-_-_-_-_-_- DEFINITION OF AXIOMS-_-_-_-_-_-_-_-_-_-_-_-_-_-


(def-axiom Axiom-temporary-participant-in-temporary-participant
   (Inverse temporary-participant-in temporary-participant ))

(def-axiom Axiom-temporary-participant-in-participant-in
(subrelation-of temporary-participant-in participant-in))

(def-axiom Axiom-spatio-temporal-presence-of-spatio-temporally-present-at
   (Inverse spatio-temporal-presence-of spatio-temporally-present-at ))

(def-axiom Axiom-spatio-temporal-presence-of-exact-location-of
(subrelation-of spatio-temporal-presence-of exact-location-of))

(def-axiom Axiom-part-of-part
   (Inverse part-of part ))

(def-axiom Axiom-part-of-immediate-relation-i
(subrelation-of part-of immediate-relation-i))

(def-axiom part-of
  (transitive part-of))

(def-axiom Axiom-t-inherent-in-has-t-quality
   (Inverse t-inherent-in has-t-quality ))

(def-axiom Axiom-t-inherent-in-inherent-in
(subrelation-of t-inherent-in inherent-in))

(def-axiom Axiom-partly-compresent-partly-compresent
   (Inverse partly-compresent partly-compresent ))

(def-axiom Axiom-partly-compresent-mediated-relation
(subrelation-of partly-compresent mediated-relation))

(def-axiom Axiom-specific-constant-constituent-of-specific-constant-constituent
   (Inverse specific-constant-constituent-of specific-constant-constituent ))

(def-axiom Axiom-specific-constant-constituent-of-immediate-relation-i
(subrelation-of specific-constant-constituent-of immediate-relation-i))

(def-axiom Axiom-q-location-q-location-of
   (Inverse q-location q-location-of ))

(def-axiom Axiom-q-location-immediate-relation
(subrelation-of q-location immediate-relation))

(def-axiom Axiom-proper-part-of-proper-part
   (Inverse proper-part-of proper-part ))

(def-axiom Axiom-proper-part-of-part-of
(subrelation-of proper-part-of part-of))

(def-axiom proper-part-of
  (transitive proper-part-of))

(def-axiom Axiom-total-temporary-participant-total-temporary-participant-in
   (Inverse total-temporary-participant total-temporary-participant-in ))

(def-axiom Axiom-total-temporary-participant-temporary-participant
(subrelation-of total-temporary-participant temporary-participant))

(def-axiom Axiom-life-of-life
   (Inverse life-of life ))

(def-axiom Axiom-life-of-constant-participant
(subrelation-of life-of constant-participant))

(def-axiom Axiom-total-temporary-participant-in-total-temporary-participant
   (Inverse total-temporary-participant-in total-temporary-participant ))

(def-axiom Axiom-total-temporary-participant-in-temporary-participant-in
(subrelation-of total-temporary-participant-in temporary-participant-in))

(def-axiom Axiom-has-quality-inherent-in
   (Inverse has-quality inherent-in ))

(def-axiom Axiom-has-quality-immediate-relation-i
(subrelation-of has-quality immediate-relation-i))

(def-axiom Axiom-part-part-of
   (Inverse part part-of ))

(def-axiom Axiom-part-immediate-relation
(subrelation-of part immediate-relation))

(def-axiom part
  (transitive part))

(def-axiom Axiom-quale-of-has-quale
   (Inverse quale-of has-quale ))

(def-axiom Axiom-quale-of-q-location-of
(subrelation-of quale-of q-location-of))

(def-axiom Axiom-q-location-of-q-location
   (Inverse q-location-of q-location ))

(def-axiom Axiom-q-location-of-immediate-relation-i
(subrelation-of q-location-of immediate-relation-i))

(def-axiom Axiom-q-present-at-time-of-q-presence-of
   (Inverse q-present-at time-of-q-presence-of ))

(def-axiom Axiom-q-present-at-mediated-relation
(subrelation-of q-present-at mediated-relation))

(def-axiom Axiom-specific-constant-dependent-specifically-constantly-dependent-on
   (Inverse specific-constant-dependent specifically-constantly-dependent-on ))

(def-axiom Axiom-specific-constant-dependent-immediate-relation
(subrelation-of specific-constant-dependent immediate-relation))

(def-axiom Axiom-overlaps-overlaps
   (Inverse overlaps overlaps ))

(def-axiom Axiom-overlaps-mediated-relation
(subrelation-of overlaps mediated-relation))

(def-axiom Axiom-total-constant-participant-in-total-constant-participant
   (Inverse total-constant-participant-in total-constant-participant ))

(def-axiom Axiom-total-constant-participant-in-constant-participant-in
(subrelation-of total-constant-participant-in constant-participant-in))

(def-axiom Axiom-inherent-in-has-quality
   (Inverse inherent-in has-quality ))

(def-axiom Axiom-inherent-in-immediate-relation
(subrelation-of inherent-in immediate-relation))

(def-axiom Axiom-mediated-relation-i-mediated-relation
   (Inverse mediated-relation-i mediated-relation ))

(def-axiom Axiom-temporary-proper-part-temporary-proper-part-of
   (Inverse temporary-proper-part temporary-proper-part-of ))

(def-axiom Axiom-temporary-proper-part-proper-part
(subrelation-of temporary-proper-part proper-part))

(def-axiom Axiom-temporary-proper-part-temporary-part
(subrelation-of temporary-proper-part temporary-part))

(def-axiom Axiom-spatio-temporally-present-at-spatio-temporal-presence-of
   (Inverse spatio-temporally-present-at spatio-temporal-presence-of ))

(def-axiom Axiom-spatio-temporally-present-at-exact-location
(subrelation-of spatio-temporally-present-at exact-location))

(def-axiom Axiom-life-life-of
   (Inverse life life-of ))

(def-axiom Axiom-life-constant-participant-in
(subrelation-of life constant-participant-in))

(def-axiom Axiom-strong-connection-strong-connection
   (Inverse strong-connection strong-connection ))

(def-axiom Axiom-strong-connection-mediated-relation
(subrelation-of strong-connection mediated-relation))

(def-axiom Axiom-temporary-atomic-part-temporary-atomic-part-of
   (Inverse temporary-atomic-part temporary-atomic-part-of ))

(def-axiom Axiom-temporary-atomic-part-temporary-proper-part
(subrelation-of temporary-atomic-part temporary-proper-part))

(def-axiom Axiom-exact-location-exact-location-of
   (Inverse exact-location exact-location-of ))

(def-axiom Axiom-exact-location-generic-location
(subrelation-of exact-location generic-location))

(def-axiom Axiom-physical-location-physical-location-of
   (Inverse physical-location physical-location-of ))

(def-axiom Axiom-physical-location-exact-location
(subrelation-of physical-location exact-location))

(def-axiom Axiom-exact-location-of-exact-location
   (Inverse exact-location-of exact-location ))

(def-axiom Axiom-exact-location-of-generic-location-of
(subrelation-of exact-location-of generic-location-of))

(def-axiom Axiom-boundary-boundary-of
   (Inverse boundary boundary-of ))

(def-axiom Axiom-boundary-proper-part
(subrelation-of boundary proper-part))

(def-axiom Axiom-mediated-relation-mediated-relation-i
   (Inverse mediated-relation mediated-relation-i ))

(def-axiom Axiom-host-of-host
   (Inverse host-of host ))

(def-axiom Axiom-host-of-specific-constant-dependent
(subrelation-of host-of specific-constant-dependent))

(def-axiom Axiom-constant-participant-constant-participant-in
   (Inverse constant-participant constant-participant-in ))

(def-axiom Axiom-constant-participant-participant
(subrelation-of constant-participant participant))

(def-axiom Axiom-has-t-quality-t-inherent-in
   (Inverse has-t-quality t-inherent-in ))

(def-axiom Axiom-has-t-quality-has-quality
(subrelation-of has-t-quality has-quality))

(def-axiom Axiom-temporary-atomic-part-of-temporary-atomic-part
   (Inverse temporary-atomic-part-of temporary-atomic-part ))

(def-axiom Axiom-temporary-atomic-part-of-temporary-proper-part-of
(subrelation-of temporary-atomic-part-of temporary-proper-part-of))

(def-axiom Axiom-mereologically-coincides-mereologically-coincides
   (Inverse mereologically-coincides mereologically-coincides ))

(def-axiom Axiom-mereologically-coincides-temporary-part
(subrelation-of mereologically-coincides temporary-part))

(def-axiom Axiom-generic-location-of-generic-location
   (Inverse generic-location-of generic-location ))

(def-axiom Axiom-generic-location-of-mediated-relation-i
(subrelation-of generic-location-of mediated-relation-i))

(def-axiom Axiom-weak-connection-weak-connection
   (Inverse weak-connection weak-connection ))

(def-axiom Axiom-weak-connection-immediate-relation
(subrelation-of weak-connection immediate-relation))

(def-axiom Axiom-identity-c-identity-c
   (Inverse identity-c identity-c ))

(def-axiom Axiom-identity-c-immediate-relation
(subrelation-of identity-c immediate-relation))

(def-axiom identity-c
  (transitive identity-c))

(def-axiom Axiom-atomic-part-of-atomic-part
   (Inverse atomic-part-of atomic-part ))

(def-axiom Axiom-atomic-part-of-part-of
(subrelation-of atomic-part-of part-of))

(def-axiom Axiom-temporary-proper-part-of-temporary-proper-part
   (Inverse temporary-proper-part-of temporary-proper-part ))

(def-axiom Axiom-temporary-proper-part-of-temporary-part-of
(subrelation-of temporary-proper-part-of temporary-part-of))

(def-axiom Axiom-temporary-proper-part-of-proper-part-of
(subrelation-of temporary-proper-part-of proper-part-of))

(def-axiom Axiom-temporary-part-temporary-part-of
   (Inverse temporary-part temporary-part-of ))

(def-axiom Axiom-temporary-part-partly-compresent
(subrelation-of temporary-part partly-compresent))

(def-axiom Axiom-temporary-part-part
(subrelation-of temporary-part part))

(def-axiom Axiom-generic-constituent-of-generic-constituent
   (Inverse generic-constituent-of generic-constituent ))

(def-axiom Axiom-generic-constituent-of-immediate-relation-i
(subrelation-of generic-constituent-of immediate-relation-i))

(def-axiom Axiom-generic-constituent-generic-constituent-of
   (Inverse generic-constituent generic-constituent-of ))

(def-axiom Axiom-generic-constituent-immediate-relation
(subrelation-of generic-constituent immediate-relation))

(def-axiom Axiom-abstract-location-of-abstract-location
   (Inverse abstract-location-of abstract-location ))

(def-axiom Axiom-abstract-location-of-exact-location-of
(subrelation-of abstract-location-of exact-location-of))

(def-axiom Axiom-temporary-part-of-temporary-part
   (Inverse temporary-part-of temporary-part ))

(def-axiom Axiom-temporary-part-of-partly-compresent
(subrelation-of temporary-part-of partly-compresent))

(def-axiom Axiom-temporary-part-of-part-of
(subrelation-of temporary-part-of part-of))

(def-axiom Axiom-r-location-of-r-location
   (Inverse r-location-of r-location ))

(def-axiom Axiom-r-location-of-immediate-relation-i
(subrelation-of r-location-of immediate-relation-i))

(def-axiom Axiom-identity-n-identity-n
   (Inverse identity-n identity-n ))

(def-axiom Axiom-identity-n-immediate-relation
(subrelation-of identity-n immediate-relation))

(def-axiom identity-n
  (transitive identity-n))

(def-axiom Axiom-temporary-participant-temporary-participant-in
   (Inverse temporary-participant temporary-participant-in ))

(def-axiom Axiom-temporary-participant-participant
(subrelation-of temporary-participant participant))

(def-axiom Axiom-host-host-of
   (Inverse host host-of ))

(def-axiom Axiom-host-specifically-constantly-dependent-on
(subrelation-of host specifically-constantly-dependent-on))

(def-axiom Axiom-proper-part-proper-part-of
   (Inverse proper-part proper-part-of ))

(def-axiom Axiom-proper-part-part
(subrelation-of proper-part part))

(def-axiom proper-part
  (transitive proper-part))

(def-axiom Axiom-participant-in-participant
   (Inverse participant-in participant ))

(def-axiom Axiom-participant-in-immediate-relation-i
(subrelation-of participant-in immediate-relation-i))

(def-axiom Axiom-constant-participant-in-constant-participant
   (Inverse constant-participant-in constant-participant ))

(def-axiom Axiom-constant-participant-in-participant-in
(subrelation-of constant-participant-in participant-in))

(def-axiom Axiom-immediate-relation-immediate-relation-i
   (Inverse immediate-relation immediate-relation-i ))

(def-axiom Axiom-r-location-r-location-of
   (Inverse r-location r-location-of ))

(def-axiom Axiom-r-location-immediate-relation
(subrelation-of r-location immediate-relation))

(def-axiom Axiom-generically-dependent-on-generic-dependent
   (Inverse generically-dependent-on generic-dependent ))

(def-axiom Axiom-generically-dependent-on-immediate-relation-i
(subrelation-of generically-dependent-on immediate-relation-i))

(def-axiom Axiom-immediate-relation-i-immediate-relation
   (Inverse immediate-relation-i immediate-relation ))

(def-axiom Axiom-specific-constant-constituent-specific-constant-constituent-of
   (Inverse specific-constant-constituent specific-constant-constituent-of ))

(def-axiom Axiom-specific-constant-constituent-immediate-relation
(subrelation-of specific-constant-constituent immediate-relation))

(def-axiom Axiom-time-of-q-presence-of-q-present-at
   (Inverse time-of-q-presence-of q-present-at ))

(def-axiom Axiom-time-of-q-presence-of-mediated-relation-i
(subrelation-of time-of-q-presence-of mediated-relation-i))

(def-axiom Axiom-sibling-part-sibling-part
   (Inverse sibling-part sibling-part ))

(def-axiom Axiom-sibling-part-mediated-relation
(subrelation-of sibling-part mediated-relation))

(def-axiom Axiom-has-quale-quale-of
   (Inverse has-quale quale-of ))

(def-axiom Axiom-has-quale-q-location
(subrelation-of has-quale q-location))

(def-axiom Axiom-physical-location-of-physical-location
   (Inverse physical-location-of physical-location ))

(def-axiom Axiom-physical-location-of-exact-location-of
(subrelation-of physical-location-of exact-location-of))

(def-axiom Axiom-atomic-part-atomic-part-of
   (Inverse atomic-part atomic-part-of ))

(def-axiom Axiom-atomic-part-part
(subrelation-of atomic-part part))

(def-axiom Axiom-generic-location-generic-location-of
   (Inverse generic-location generic-location-of ))

(def-axiom Axiom-generic-location-mediated-relation
(subrelation-of generic-location mediated-relation))

(def-axiom Axiom-boundary-of-boundary
   (Inverse boundary-of boundary ))

(def-axiom Axiom-boundary-of-proper-part-of
(subrelation-of boundary-of proper-part-of))

(def-axiom Axiom-abstract-location-abstract-location-of
   (Inverse abstract-location abstract-location-of ))

(def-axiom Axiom-abstract-location-exact-location
(subrelation-of abstract-location exact-location))

(def-axiom Axiom-participant-participant-in
   (Inverse participant participant-in ))

(def-axiom Axiom-participant-immediate-relation
(subrelation-of participant immediate-relation))

(def-axiom Axiom-total-constant-participant-total-constant-participant-in
   (Inverse total-constant-participant total-constant-participant-in ))

(def-axiom Axiom-total-constant-participant-constant-participant
(subrelation-of total-constant-participant constant-participant))

(def-axiom Axiom-generic-dependent-generically-dependent-on
   (Inverse generic-dependent generically-dependent-on ))

(def-axiom Axiom-generic-dependent-immediate-relation
(subrelation-of generic-dependent immediate-relation))

(def-axiom Axiom-specifically-constantly-dependent-on-specific-constant-dependent
   (Inverse specifically-constantly-dependent-on specific-constant-dependent ))

(def-axiom Axiom-specifically-constantly-dependent-on-immediate-relation-i
(subrelation-of specifically-constantly-dependent-on immediate-relation-i))

(def-axiom axiom-abstract-perdurant
   (disjoint abstract perdurant))

(def-axiom axiom-abstract-quality
   (disjoint abstract quality))

(def-axiom axiom-perdurant-quality
   (disjoint perdurant quality))

(def-axiom axiom-endurant-perdurant
   (disjoint endurant perdurant))

(def-axiom axiom-endurant-abstract
   (disjoint endurant abstract))

(def-axiom axiom-endurant-quality
   (disjoint endurant quality))

(def-axiom axiom-physical-endurant-non-physical-endurant
   (disjoint physical-endurant non-physical-endurant))

(def-axiom axiom-physical-endurant-arbitrary-sum
   (disjoint physical-endurant arbitrary-sum))

(def-axiom axiom-feature-amount-of-matter
   (disjoint feature amount-of-matter))

(def-axiom axiom-physical-object-feature
   (disjoint physical-object feature))

(def-axiom axiom-physical-object-amount-of-matter
   (disjoint physical-object amount-of-matter))

(def-axiom axiom-temporal-quality-abstract-quality
   (disjoint temporal-quality abstract-quality))

(def-axiom axiom-temporal-quality-physical-quality
   (disjoint temporal-quality physical-quality))

(def-axiom axiom-physical-quality-abstract-quality
   (disjoint physical-quality abstract-quality))

(def-axiom axiom-physical-region-temporal-region
   (disjoint physical-region temporal-region))

(def-axiom axiom-physical-region-abstract-region
   (disjoint physical-region abstract-region))

(def-axiom axiom-arbitrary-sum-non-physical-endurant
   (disjoint arbitrary-sum non-physical-endurant))

(def-axiom axiom-abstract-region-temporal-region
   (disjoint abstract-region temporal-region))

(def-axiom Axiom-temporary-atomic-part-temporary-proper-part
(subrelation-of temporary-atomic-part temporary-proper-part))

(def-axiom Axiom-temporary-participant-temporary-participant-in
   (Inverse temporary-participant temporary-participant-in ))

(def-axiom Axiom-overlaps-overlaps
   (Inverse overlaps overlaps ))

(def-axiom Axiom-total-constant-participant-constant-participant
(subrelation-of total-constant-participant constant-participant))

(def-axiom Axiom-quale-of-has-quale
   (Inverse quale-of has-quale ))

(def-axiom identity-c
  (transitive identity-c))

(def-axiom Axiom-constant-participant-constant-participant-in
   (Inverse constant-participant constant-participant-in ))

(def-axiom Axiom-constant-participant-in-constant-participant
   (Inverse constant-participant-in constant-participant ))

(def-axiom Axiom-total-temporary-participant-temporary-participant
(subrelation-of total-temporary-participant temporary-participant))

(def-axiom Axiom-temporary-part-of-temporary-part
   (Inverse temporary-part-of temporary-part ))

(def-axiom Axiom-part-immediate-relation
(subrelation-of part immediate-relation))

(def-axiom Axiom-life-life-of
   (Inverse life life-of ))

(def-axiom Axiom-mereologically-coincides-mereologically-coincides
   (Inverse mereologically-coincides mereologically-coincides ))

(def-axiom axiom-endurant-perdurant
   (disjoint endurant perdurant))

(def-axiom axiom-physical-endurant-arbitrary-sum
   (disjoint physical-endurant arbitrary-sum))

(def-axiom Axiom-mediated-relation-i-mediated-relation
   (Inverse mediated-relation-i mediated-relation ))

(def-axiom Axiom-total-temporary-participant-in-temporary-participant-in
(subrelation-of total-temporary-participant-in temporary-participant-in))

(def-axiom Axiom-part-of-part
   (Inverse part-of part ))

(def-axiom Axiom-generically-dependent-on-generic-dependent
   (Inverse generically-dependent-on generic-dependent ))

(def-axiom Axiom-time-of-q-presence-of-mediated-relation-i
(subrelation-of time-of-q-presence-of mediated-relation-i))

(def-axiom Axiom-r-location-of-r-location
   (Inverse r-location-of r-location ))

(def-axiom Axiom-temporary-part-of-partly-compresent
(subrelation-of temporary-part-of partly-compresent))

(def-axiom Axiom-q-present-at-time-of-q-presence-of
   (Inverse q-present-at time-of-q-presence-of ))

(def-axiom Axiom-generic-constituent-generic-constituent-of
   (Inverse generic-constituent generic-constituent-of ))

(def-axiom Axiom-generic-location-of-generic-location
   (Inverse generic-location-of generic-location ))

(def-axiom Axiom-specifically-constantly-dependent-on-immediate-relation-i
(subrelation-of specifically-constantly-dependent-on immediate-relation-i))

(def-axiom Axiom-atomic-part-of-part-of
(subrelation-of atomic-part-of part-of))

(def-axiom Axiom-abstract-location-of-abstract-location
   (Inverse abstract-location-of abstract-location ))

(def-axiom Axiom-atomic-part-part
(subrelation-of atomic-part part))

(def-axiom Axiom-has-quale-quale-of
   (Inverse has-quale quale-of ))

(def-axiom Axiom-physical-location-of-exact-location-of
(subrelation-of physical-location-of exact-location-of))

(def-axiom Axiom-quale-of-q-location-of
(subrelation-of quale-of q-location-of))

(def-axiom Axiom-t-inherent-in-inherent-in
(subrelation-of t-inherent-in inherent-in))

(def-axiom Axiom-generic-constituent-immediate-relation
(subrelation-of generic-constituent immediate-relation))

(def-axiom Axiom-generic-constituent-of-immediate-relation-i
(subrelation-of generic-constituent-of immediate-relation-i))

(def-axiom part
  (transitive part))

(def-axiom Axiom-generic-location-generic-location-of
   (Inverse generic-location generic-location-of ))

(def-axiom Axiom-physical-location-of-physical-location
   (Inverse physical-location-of physical-location ))

(def-axiom Axiom-temporary-proper-part-of-temporary-proper-part
   (Inverse temporary-proper-part-of temporary-proper-part ))

(def-axiom Axiom-q-present-at-mediated-relation
(subrelation-of q-present-at mediated-relation))

(def-axiom Axiom-has-quality-inherent-in
   (Inverse has-quality inherent-in ))

(def-axiom Axiom-specifically-constantly-dependent-on-specific-constant-dependent
   (Inverse specifically-constantly-dependent-on specific-constant-dependent ))

(def-axiom Axiom-generic-dependent-immediate-relation
(subrelation-of generic-dependent immediate-relation))

(def-axiom Axiom-specific-constant-constituent-immediate-relation
(subrelation-of specific-constant-constituent immediate-relation))

(def-axiom Axiom-total-constant-participant-in-constant-participant-in
(subrelation-of total-constant-participant-in constant-participant-in))

(def-axiom Axiom-specific-constant-constituent-specific-constant-constituent-of
   (Inverse specific-constant-constituent specific-constant-constituent-of ))

(def-axiom Axiom-q-location-of-q-location
   (Inverse q-location-of q-location ))

(def-axiom Axiom-q-location-immediate-relation
(subrelation-of q-location immediate-relation))

(def-axiom Axiom-mereologically-coincides-temporary-part
(subrelation-of mereologically-coincides temporary-part))

(def-axiom Axiom-weak-connection-immediate-relation
(subrelation-of weak-connection immediate-relation))

(def-axiom Axiom-identity-n-immediate-relation
(subrelation-of identity-n immediate-relation))

(def-axiom Axiom-time-of-q-presence-of-q-present-at
   (Inverse time-of-q-presence-of q-present-at ))

(def-axiom Axiom-proper-part-proper-part-of
   (Inverse proper-part proper-part-of ))

(def-axiom Axiom-sibling-part-sibling-part
   (Inverse sibling-part sibling-part ))

(def-axiom axiom-abstract-quality
   (disjoint abstract quality))

(def-axiom Axiom-temporary-participant-participant
(subrelation-of temporary-participant participant))

(def-axiom Axiom-total-constant-participant-in-total-constant-participant
   (Inverse total-constant-participant-in total-constant-participant ))

(def-axiom Axiom-temporary-participant-in-temporary-participant
   (Inverse temporary-participant-in temporary-participant ))

(def-axiom axiom-physical-quality-abstract-quality
   (disjoint physical-quality abstract-quality))

(def-axiom Axiom-strong-connection-strong-connection
   (Inverse strong-connection strong-connection ))

(def-axiom Axiom-spatio-temporally-present-at-spatio-temporal-presence-of
   (Inverse spatio-temporally-present-at spatio-temporal-presence-of ))

(def-axiom Axiom-sibling-part-mediated-relation
(subrelation-of sibling-part mediated-relation))

(def-axiom Axiom-spatio-temporally-present-at-exact-location
(subrelation-of spatio-temporally-present-at exact-location))

(def-axiom axiom-endurant-quality
   (disjoint endurant quality))

(def-axiom Axiom-r-location-of-immediate-relation-i
(subrelation-of r-location-of immediate-relation-i))

(def-axiom Axiom-temporary-proper-part-temporary-part
(subrelation-of temporary-proper-part temporary-part))

(def-axiom Axiom-partly-compresent-mediated-relation
(subrelation-of partly-compresent mediated-relation))

(def-axiom Axiom-strong-connection-mediated-relation
(subrelation-of strong-connection mediated-relation))

(def-axiom Axiom-participant-in-immediate-relation-i
(subrelation-of participant-in immediate-relation-i))

(def-axiom Axiom-identity-c-identity-c
   (Inverse identity-c identity-c ))

(def-axiom Axiom-identity-n-identity-n
   (Inverse identity-n identity-n ))

(def-axiom Axiom-host-of-host
   (Inverse host-of host ))

(def-axiom Axiom-identity-c-immediate-relation
(subrelation-of identity-c immediate-relation))

(def-axiom Axiom-temporary-participant-in-participant-in
(subrelation-of temporary-participant-in participant-in))

(def-axiom Axiom-specific-constant-dependent-immediate-relation
(subrelation-of specific-constant-dependent immediate-relation))

(def-axiom Axiom-exact-location-generic-location
(subrelation-of exact-location generic-location))

(def-axiom Axiom-spatio-temporal-presence-of-spatio-temporally-present-at
   (Inverse spatio-temporal-presence-of spatio-temporally-present-at ))

(def-axiom Axiom-life-of-life
   (Inverse life-of life ))

(def-axiom Axiom-specific-constant-dependent-specifically-constantly-dependent-on
   (Inverse specific-constant-dependent specifically-constantly-dependent-on ))

(def-axiom proper-part
  (transitive proper-part))

(def-axiom axiom-physical-object-feature
   (disjoint physical-object feature))

(def-axiom Axiom-boundary-of-proper-part-of
(subrelation-of boundary-of proper-part-of))

(def-axiom Axiom-life-constant-participant-in
(subrelation-of life constant-participant-in))

(def-axiom Axiom-constant-participant-participant
(subrelation-of constant-participant participant))

(def-axiom Axiom-abstract-location-exact-location
(subrelation-of abstract-location exact-location))

(def-axiom axiom-temporal-quality-physical-quality
   (disjoint temporal-quality physical-quality))

(def-axiom Axiom-life-of-constant-participant
(subrelation-of life-of constant-participant))

(def-axiom Axiom-proper-part-part
(subrelation-of proper-part part))

(def-axiom Axiom-exact-location-of-exact-location
   (Inverse exact-location-of exact-location ))

(def-axiom Axiom-generic-location-mediated-relation
(subrelation-of generic-location mediated-relation))

(def-axiom Axiom-specific-constant-constituent-of-specific-constant-constituent
   (Inverse specific-constant-constituent-of specific-constant-constituent ))

(def-axiom Axiom-constant-participant-in-participant-in
(subrelation-of constant-participant-in participant-in))

(def-axiom Axiom-temporary-part-temporary-part-of
   (Inverse temporary-part temporary-part-of ))

(def-axiom Axiom-inherent-in-has-quality
   (Inverse inherent-in has-quality ))

(def-axiom axiom-temporal-quality-abstract-quality
   (disjoint temporal-quality abstract-quality))

(def-axiom axiom-feature-amount-of-matter
   (disjoint feature amount-of-matter))

(def-axiom Axiom-temporary-proper-part-proper-part
(subrelation-of temporary-proper-part proper-part))

(def-axiom Axiom-t-inherent-in-has-t-quality
   (Inverse t-inherent-in has-t-quality ))

(def-axiom Axiom-boundary-of-boundary
   (Inverse boundary-of boundary ))

(def-axiom Axiom-physical-location-exact-location
(subrelation-of physical-location exact-location))

(def-axiom Axiom-q-location-of-immediate-relation-i
(subrelation-of q-location-of immediate-relation-i))

(def-axiom Axiom-participant-participant-in
   (Inverse participant participant-in ))

(def-axiom axiom-physical-object-amount-of-matter
   (disjoint physical-object amount-of-matter))

(def-axiom Axiom-participant-in-participant
   (Inverse participant-in participant ))

(def-axiom Axiom-part-of-immediate-relation-i
(subrelation-of part-of immediate-relation-i))

(def-axiom Axiom-temporary-proper-part-temporary-proper-part-of
   (Inverse temporary-proper-part temporary-proper-part-of ))

(def-axiom Axiom-has-t-quality-t-inherent-in
   (Inverse has-t-quality t-inherent-in ))

(def-axiom Axiom-exact-location-of-generic-location-of
(subrelation-of exact-location-of generic-location-of))

(def-axiom part-of
  (transitive part-of))

(def-axiom Axiom-temporary-atomic-part-temporary-atomic-part-of
   (Inverse temporary-atomic-part temporary-atomic-part-of ))

(def-axiom Axiom-inherent-in-immediate-relation
(subrelation-of inherent-in immediate-relation))

(def-axiom proper-part-of
  (transitive proper-part-of))

(def-axiom Axiom-partly-compresent-partly-compresent
   (Inverse partly-compresent partly-compresent ))

(def-axiom axiom-physical-region-abstract-region
   (disjoint physical-region abstract-region))

(def-axiom Axiom-weak-connection-weak-connection
   (Inverse weak-connection weak-connection ))

(def-axiom Axiom-temporary-proper-part-of-temporary-part-of
(subrelation-of temporary-proper-part-of temporary-part-of))

(def-axiom Axiom-exact-location-exact-location-of
   (Inverse exact-location exact-location-of ))

(def-axiom Axiom-temporary-proper-part-of-proper-part-of
(subrelation-of temporary-proper-part-of proper-part-of))

(def-axiom Axiom-specific-constant-constituent-of-immediate-relation-i
(subrelation-of specific-constant-constituent-of immediate-relation-i))

(def-axiom Axiom-spatio-temporal-presence-of-exact-location-of
(subrelation-of spatio-temporal-presence-of exact-location-of))

(def-axiom Axiom-has-t-quality-has-quality
(subrelation-of has-t-quality has-quality))

(def-axiom Axiom-immediate-relation-immediate-relation-i
   (Inverse immediate-relation immediate-relation-i ))

(def-axiom identity-n
  (transitive identity-n))

(def-axiom Axiom-participant-immediate-relation
(subrelation-of participant immediate-relation))

(def-axiom Axiom-atomic-part-of-atomic-part
   (Inverse atomic-part-of atomic-part ))

(def-axiom axiom-physical-endurant-non-physical-endurant
   (disjoint physical-endurant non-physical-endurant))

(def-axiom Axiom-generic-dependent-generically-dependent-on
   (Inverse generic-dependent generically-dependent-on ))

(def-axiom Axiom-has-quale-q-location
(subrelation-of has-quale q-location))

(def-axiom axiom-physical-region-temporal-region
   (disjoint physical-region temporal-region))

(def-axiom Axiom-q-location-q-location-of
   (Inverse q-location q-location-of ))

(def-axiom Axiom-total-temporary-participant-total-temporary-participant-in
   (Inverse total-temporary-participant total-temporary-participant-in ))

(def-axiom Axiom-atomic-part-atomic-part-of
   (Inverse atomic-part atomic-part-of ))

(def-axiom Axiom-immediate-relation-i-immediate-relation
   (Inverse immediate-relation-i immediate-relation ))

(def-axiom Axiom-host-of-specific-constant-dependent
(subrelation-of host-of specific-constant-dependent))

(def-axiom Axiom-temporary-part-partly-compresent
(subrelation-of temporary-part partly-compresent))

(def-axiom Axiom-temporary-part-part
(subrelation-of temporary-part part))

(def-axiom axiom-perdurant-quality
   (disjoint perdurant quality))

(def-axiom Axiom-temporary-atomic-part-of-temporary-atomic-part
   (Inverse temporary-atomic-part-of temporary-atomic-part ))

(def-axiom Axiom-r-location-r-location-of
   (Inverse r-location r-location-of ))

(def-axiom Axiom-total-constant-participant-total-constant-participant-in
   (Inverse total-constant-participant total-constant-participant-in ))

(def-axiom Axiom-host-host-of
   (Inverse host host-of ))

(def-axiom axiom-abstract-perdurant
   (disjoint abstract perdurant))

(def-axiom Axiom-generically-dependent-on-immediate-relation-i
(subrelation-of generically-dependent-on immediate-relation-i))

(def-axiom Axiom-generic-constituent-of-generic-constituent
   (Inverse generic-constituent-of generic-constituent ))

(def-axiom Axiom-boundary-boundary-of
   (Inverse boundary boundary-of ))

(def-axiom Axiom-boundary-proper-part
(subrelation-of boundary proper-part))

(def-axiom Axiom-r-location-immediate-relation
(subrelation-of r-location immediate-relation))

(def-axiom Axiom-physical-location-physical-location-of
   (Inverse physical-location physical-location-of ))

(def-axiom Axiom-proper-part-of-proper-part
   (Inverse proper-part-of proper-part ))

(def-axiom axiom-endurant-abstract
   (disjoint endurant abstract))

(def-axiom Axiom-overlaps-mediated-relation
(subrelation-of overlaps mediated-relation))

(def-axiom Axiom-mediated-relation-mediated-relation-i
   (Inverse mediated-relation mediated-relation-i ))

(def-axiom Axiom-proper-part-of-part-of
(subrelation-of proper-part-of part-of))

(def-axiom axiom-abstract-region-temporal-region
   (disjoint abstract-region temporal-region))

(def-axiom Axiom-temporary-part-of-part-of
(subrelation-of temporary-part-of part-of))

(def-axiom Axiom-abstract-location-of-exact-location-of
(subrelation-of abstract-location-of exact-location-of))

(def-axiom Axiom-temporary-atomic-part-of-temporary-proper-part-of
(subrelation-of temporary-atomic-part-of temporary-proper-part-of))

(def-axiom Axiom-abstract-location-abstract-location-of
   (Inverse abstract-location abstract-location-of ))

(def-axiom axiom-arbitrary-sum-non-physical-endurant
   (disjoint arbitrary-sum non-physical-endurant))

(def-axiom Axiom-host-specifically-constantly-dependent-on
(subrelation-of host specifically-constantly-dependent-on))

(def-axiom Axiom-has-quality-immediate-relation-i
(subrelation-of has-quality immediate-relation-i))

(def-axiom Axiom-generic-location-of-mediated-relation-i
(subrelation-of generic-location-of mediated-relation-i))

(def-axiom Axiom-part-part-of
   (Inverse part part-of ))

(def-axiom Axiom-total-temporary-participant-in-total-temporary-participant
   (Inverse total-temporary-participant-in total-temporary-participant ))

