;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs

(in-package #:ocml)

(def-ontology xsd-types
    "Lisp/OCML shadows for XML types."
  :includes ()
  :type :domain
  :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/xsd-types#"
  :namespaces (("xsd" xsd-types))
  :author "dave" :allowed-editors ("dave" "carlos")
  :files ("lisp-level" "time"))
