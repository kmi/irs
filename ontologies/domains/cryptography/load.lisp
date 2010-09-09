;;; Copyright © 2008, 2009 The Open University

;;; File created in GNU Emacs.

(in-package #:ocml)

(def-ontology cryptography
    "Cryptographic methods as used on the net and elsewhere."
    :includes () :type :domain
    :author "dave" :allowed-editors ("dave" "john")
    :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/ns/cryptography#"
    :namespaces (("crypt" cryptography))
    :files ("encryption" "md5" "sha1"))
