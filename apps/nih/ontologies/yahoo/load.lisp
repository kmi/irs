;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs

(in-package #:ocml)

(def-ontology yahoo
    "Goals and services for accessing Yahoo."
    :includes (http-grounding mime rfc2616 wsmo)
    :type :goal
    :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/yahoo#"
    :namespaces (("yah" yahoo)
                 ("hg" http-grounding)
                 ("mime" mime)
                 ("rfc2616" rfc2616))
    :author "dave" :allowed-editors ("dave" "john")
    :files ("search"))
