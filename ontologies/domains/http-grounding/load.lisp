;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs.

(in-package #:ocml)

(def-ontology http-grounding
    "IRS specific fiddling for the HTTP protocol."
    :includes (rfc2616) :type :domain
    :author "dave" :allowed-editors ("dave" "john")
    :namespaces (("rfc2616" rfc2616)
                 ("grnd" http-grounding))
    :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/ns/http-grounding#"
    :files ("http"))
