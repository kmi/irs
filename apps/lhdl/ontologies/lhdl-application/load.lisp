;;; Copyright © 2009 The Open University.

(in-package #:ocml)

;;; The "secrets" file is just that: secret.  We don't distibute it
;;; with the IRS because it contains secret stuff (like passwords,
;;; keys, etc).  OCML will warn that it can't find the file, and lots
;;; of things won't work.  But the things that don't actually depend
;;; on the secrets should still function.
(def-ontology lhdl-application
    "Top level application ontology for the Living Human Digital Library."
    :type :application
    :namespace-uri "http://kmi.open.ac.uk/projects/lhdl/ns/application#"
    :author "dave"
    :files ("secrets")
    :includes (lhdl-domain lhdl-goals amazon-s3)
    :namespaces (("lhdl" lhdl-application)
                 ("domain" lhdl-domain)
                 ("goals" lhdl-goals)))
