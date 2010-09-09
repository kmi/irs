;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology lhdl-domain :includes (wsmo)
              :type :domain
              :author "dave"
              :namespace-uri "http://kmi.open.ac.uk/projects/lhdl/ns/domain#"
              :files ("lhdl-domain" "skyhooks")
              :namespaces (("lhdl" lhdl-domain)))
