;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs.

(in-package #:ocml)

(def-ontology rfc2616
    "Describes the HTTP protocol, as defined in RFC 2616."
  :includes () :type :domain
  :author "dave" :allowed-editors ("dave" "john")
  :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/ns/rfc2616#"
  :namespaces (("rfc2616" rfc2616))
  :files ("lisp" "rfc2616"))
