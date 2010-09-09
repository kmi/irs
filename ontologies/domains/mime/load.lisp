;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs.

(in-package #:ocml)

(def-ontology mime
    "MIME (Multi-purpose Internet Mail Extensions)."
    :includes () :type :domain
    :author "dave" :allowed-editors ("carlos" "john")
    :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/ns/mime#"
    :namespaces (("mime" mime))
    :files ("base64"))
