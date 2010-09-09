;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology lhdl-goals
    "Goals and services for the Living Human Digital Library."
    :type :goal
    :namespace-uri "http://kmi.open.ac.uk/projects/lhdl/ns/goals#"
    :author "dave"
    :files ("new" "community" "execution" "review2008" "lilo")
    :includes (amazon-s3 http-grounding lhdl-domain mime rfc2616 xml-rpc)
    :namespaces (("s3" amazon-s3)
                 ("domain" lhdl-domain)
                 ("lhdl" lhdl-goals)
                 ("hg" http-grounding)
                 ("mime" mime)
                 ("rfc2616" rfc2616)
                 ("xml" xml)
                 ("xmlrpc" xml-rpc)))
