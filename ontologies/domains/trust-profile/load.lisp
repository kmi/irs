;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology trust-profile
    :includes (wsto european-train-travel-application
                    http-grounding rfc2616)
    :namespace-uri "http://kmi.open.ac.uk/irs/trust#"
    :namespaces (("tt" trust-profile)
                 ("hg" http-grounding))
    :type :domain
    :author "stefania"
    :allowed-editors ("john" "alessio"))