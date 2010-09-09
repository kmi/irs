;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs

(in-package #:ocml)

(def-ontology amazon-s3
    "Goals and services for accessing Amazon S3 web file storage."
    :includes (cryptography http-grounding mime rfc2616 wsmo)
    :type :goal
    :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/amazon-s3#"
    :namespaces (("s3" amazon-s3)
                 ("enc" cryptography)
                 ("hg" http-grounding)
                 ("mime" mime)
                 ("rfc2616" rfc2616))
    :author "dave" :allowed-editors ("dave" "john")
    :files ("amazon-s3" "domain"))
