;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology extended-sid-tid-ou)

(def-class location ( )
((has-name :type string))
)

(def-class country ( location ))

(def-class role ( common-business_-domain ))

(def-class t-i-d-product-offering ( product-offering )
((has-t-i-d-product-specification :type t-i-d-product-specification))
)

(def-class party-role ( role ))

(def-class t-i-d-party ( party )
((has-party-role :type t-i-d-party-role)
(has-id :type string))
)

(def-class t-i-d-product ( product ))

(def-class t-i-d-service ( service ))

(def-class t-i-d-product-specification ( product-specification )
((has-description :type string)
(is-valid-until :type string))
)

(def-class t-i-d-market-segment ( market-segment ))

(def-class t-i-d-resource ( resource ))

(def-class t-i-d-organisation ( t-i-d-party ))

(def-class d-a-m-equipment ( t-i-d-resource ))

(def-class t-i-d-individual ( t-i-d-party ))

(def-class d-a-m-physical-device ( t-i-d-resource ))

(def-class t-i-d-party-role ( party-role ))

(def-class d-a-m-service ( t-i-d-service ))

(def-class d-a-m-terminal ( t-i-d-resource ))

(def-class t-i-d-client-area ( t-i-d-market-segment )
((has-location :type location))
)

(def-class d-a-m-download-service ( d-a-m-service ))

(def-class t-i-d-customer ( t-i-d-party-role customer )
((has-name :type string))
)

(def-class d-a-m-security-control (t-i-d-resource)
)


