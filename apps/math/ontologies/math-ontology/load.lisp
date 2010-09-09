;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs

(in-package #:ocml)

(def-ontology math-ontology
    "Goals and services for simple mathematics."
    :includes (http-grounding mime rfc2616 wsmo)
    :type :goal
    :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/math-ontology#"
    :namespaces (("math" math-ontology)
                 ("hg" http-grounding)
                 ("mime" mime)
                 ("rfc2616" rfc2616))
    :author "dave" :allowed-editors ("dave" "john")
    :files ("maths"))
