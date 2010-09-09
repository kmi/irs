;;; Copyright © 2009 The Open University.

(in-package #:ocml)

;;; The "secrets" file is just that: secret.  We don't distibute it
;;; with the IRS because it contains secret stuff (like passwords,
;;; keys, etc).  OCML will warn that it can't find the file, and lots
;;; of things won't work.  But the things that don't actually depend
;;; on the secrets should still function.
(def-ontology nih-application
    "Top level application ontology for the Not Invented Here application."
    :type :application
    :namespace-uri "http://kmi.open.ac.uk/irs/apps/nih#"
    :author "dave"
    :files ("secrets")
    :includes (amazon-s3 flickr yahoo)
    :namespaces (("nih" nih-application)
		 ("flickr" flickr)
		 ("s3" amazon-s3)
		 ("yahoo" yahoo)))
