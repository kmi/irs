;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology luisa-lom2-wsml)

#|
(def-class learning-object ()
((identifier :type l-o-m-identifier :max-cardinality 1)
 (title :type lang-string :max-cardinality 1)
 (language :type human-language)
 (description :type lang-string)
 (keyword :type lang-string)
 (coverage :type lang-string)
 (coverage--spatial-location :type oc_-geographical-region)
 (coverage--temporal-period :type oc_-time-interval)
 (coverage--jurisdiction :type oc_-administrative-unit)
 (structure :type vocabulary-item :max-cardinality 1 :min-cardinality 1)
 (aggregation-level :type vocabulary-item :max-cardinality 1 :min-cardinality 1)
 (version :type lang-string :max-cardinality 1 :min-cardinality 1)
 (status :type vocabulary-item :max-cardinality 1 :min-cardinality 1)
 (contribute-entity :type v-card :min-cardinality 1)
 (contribute-date :type date-time :max-cardinality 1 :min-cardinality 1)
 (contribute-role :type vocabulary-item :max-cardinality 1 :min-cardinality 1)
 (metadata-record-identifier :type l-o-m-identifier :max-cardinality 1)
 (metadata-contribution-role :type vocabulary-item :max-cardinality 1 :min-cardinality 1)
 (metadata-contribution-entity :type v-card :min-cardinality 1)
 (metadata-contribution-date :type date-time :max-cardinality 1 :min-cardinality 1)
 (metadata-schema :type metadata-schema)
 (metadata-language :type human-language)
 (format :type lang-string)
 (format--m-i-m-e :type m-i-m-e-type)
 (size :type integer)
 (location :type url)
 (requirement :type technical-requirement)
 (installation-remarks :type lang-string :max-cardinality 1)
 (other-platform-requirement :type lang-string)
 (interactivity-type :type vocabulary-item :max-cardinality 1)
 (learning-resource-type :type vocabulary-item)
 (interactivity-level :type vocabulary-item :max-cardinality 1)
 (semantic-density :type vocabulary-item :max-cardinality 1)
 (intended-end-user-role :type vocabulary-item)
 (semantic-context :type oc_-microtheory)
 (context :type vocabulary-item)
 (typical-age-range :type lang-string)
 (tipycal-age-range-min :type integer)
 (tipycal-age-range-max :type integer)
 (difficulty :type vocabulary-item :max-cardinality 1)
 (tipical-learning-time :type duration :max-cardinality 1)
 (duration :type duration :max-cardinality 1)
 (educational-description :type lang-string)
 (user-language :type human-language)
 (cost :type boolean)
 (copyright-and-other-restrictions :type boolean))
)

(def-class l-o-m-identifier ()
((catalog :type string)
 (entry :type string))
)

(def-class lang-string ()
((has-single-lang-string :type lang-string--single))
)

(def-class lang-string--single ()
((has-human-language :type human-language :min-cardinality 1)
 (has-character-string :type string))
)

(def-class human-language ())

(def-class oc_-geographical-region ())

(def-class duration ()
((description :type lang-string :max-cardinality 1)
 (value))
)

(def-class date-time ()
((description :type lang-string :max-cardinality 1)
 (value :type list-date-and-time))
)

(def-class vocabulary-item ()
((source :type string)
 (value :type string))
)

(def-class oc_-time-interval ())

(def-class oc_-administrative-unit ())

(def-class v-card ())

(def-class metadata-schema ()
((version :type string)
 (schema :type string))
)

(def-class m-i-m-e-type ())

(def-class technical-requirement ()
((min-version :type string)
 (max-version :type string)
 (name :type vocabulary-item :max-cardinality 1 :min-cardinality 1)
 (type :type vocabulary-item :max-cardinality 1 :min-cardinality 1))
)

(def-class oc_-microtheory ())

|#

