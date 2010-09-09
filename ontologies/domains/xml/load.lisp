;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs.

(in-package #:ocml)

(def-ontology xml
    "XML."
    :includes () :type :domain
    :author "dave"
    :namespaces (("xml" xml))
    :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/ns/xml#"
    :files ("xml" "serialisation" "utilities"))
