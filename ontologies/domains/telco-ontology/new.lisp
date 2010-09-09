;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology telco-ontology)

(def-class telco-role ()
)

(Def-class resource ()
)

(def-class service-provider (telco-role)
((has-catalogue :type catalogue))
)

(def-class content-provider (telco-role))

(def-class content-broker (telco-role)
((has-distribution-capacities :type distribution-capacity :min-cardinality 1))
)

(def-class final-user (telco-role)
((has-download :type download)
 (has-device :type device :min-cardinality 1)
 (has-preferences :type final-user-preferences :max-cardinality 1)
 (has-content-for-user :type content-for-user))
)

(def-class owner (telco-role))

(def-class d-r-m-provider (telco-role)
((has-license-server :type license-server)
 (has-packager :type packager))
)

(def-class telco-role ()
((has-contract :type contract)
 (has-ressource :type ressource))
)

(def-class contract ()
((has-roles :type telco-role :max-cardinality 2 :min-cardinality 2))
)

(def-class d-r-m-c-p-contract (contract)
((drm-service-price :type decimal))
)

(def-class c-p-s-p-contract (contract)
((has-price :type price :max-cardinality 1 :min-cardinality 1)
 (has-commission :type price :max-cardinality 1)
 (has-sale-package :type sale-package :min-cardinality 1))
)

(def-class c-b-c-p-contract (contract)
((has-distribution-service-contracted :type distribution-capacity :min-cardinality 1))
)

(def-class distribution-capacity ()
((price :type decimal))
)

(def-class storage-capacity (distribution-capacity)
((has-downloading-server :type downloading-server :min-cardinality 1))
)

(def-class network-capacity (distribution-capacity))

(def-class bandwidth (network-capacity))

(def-class downloading-server ()
((has-content :type content))
)

(def-class final-user-s-p-contract (contract)
((has-provided-content :type content :min-cardinality 1))
)

(def-class content ()
((has-format :type format)
 (has-rights :type right)
 (has-state :type content-state :max-cardinality 1 :min-cardinality 1)
 (has-content-property :type content-property))
)

(def-class sale-package ()
((is-special-offer :type boolean)
 (has-content :type content :min-cardinality 1)
 (has-price :type price))
)

(def-class download ()
((has-content :type content :max-cardinality 1 :min-cardinality 1)
 (begin-date :type list-date-and-time)
 (end-date :type list-date-and-time))
)

(def-class streaming (download))

(def-class offline (download)
((is-completed :type boolean))
)

(def-class device ()
((operating-system :type string)
 (has-quality-threshold :type quality-threshold))
)

(def-class personal-computer (device))

(def-class personal-digital-assistant (device))

(def-class format ()
((has-possible-quality :type quality :min-cardinality 1))
)

(def-class quality ()
((min-value :type decimal)
 (max-value :type decimal)
 (unit :type string))
)

(def-class audio-format (format)
((has-audio-codec :type audio-codec :max-cardinality 1 :min-cardinality 1))
)

(def-class video-format (format)
((has-video-codec :type video-codec :max-cardinality 1 :min-cardinality 1)
 (bitrate :type decimal)
 (frame-rate :type decimal)
 (key-frame :type decimal))
)

(def-class codec ())

(def-class audio-codec (codec))

(def-class video-codec (codec))

(def-class right ())

(def-class audio-content (content)
((has-format :type audio-format))
)

(def-class video-content (content))

(def-class content-management-right (right))

(def-class transport-right (right))

(def-class content-modification-right (right))

(def-class license-server (resource)
((has-license :type license))
)

(def-class license ()
((begin-date :type list-date-and-time)
 (expiration-date :type list-date-and-time)
 (has-content :type content :min-cardinality 1)
 (has-right :type right :max-cardinality 1 :min-cardinality 1))
)

(def-class final-user-preferences ())

(def-class ressource ())

(def-class packager (ressource))

(def-class content-state ()
((value-state :type string))
)

(def-class right-database (ressource))

(def-class network-server (ressource))

(def-class content-for-user ()
((has-content :type content :max-cardinality 1 :min-cardinality 1)
 (has-license :type license :max-cardinality 1 :min-cardinality 1))
)

(def-class resolution (quality))

(def-class quality-threshold ()
((has-quality :type quality :max-cardinality 1 :min-cardinality 1)
 (has-maximum :type decimal))
)

(def-class catalogue ()
((has-sale-package :type sale-package))
)

(def-class product-price ()
((price-value :type decimal)
 (currency :type string))
)

(def-class content-property ()
((relevance :type decimal))
)

(def-class content-title (content-property))

(def-class content-type (content-property))

(def-class duration (content-property)
((duration-value :type decimal)
 (unit :type string))
)

(def-class author (content-property))

(def-class localization (content-property))



