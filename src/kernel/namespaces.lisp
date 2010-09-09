;;; Copyright © 2008

(in-package #:irs)

(defvar *source-namespaces*
  '(("grnd" "http://www.kmi.open.ac.uk/projects/irs/ns/http-grounding#")
    ("rfc2616" "http://www.kmi.open.ac.uk/projects/irs/ns/rfc2616#")
    ("rio" "http://www.kmi.open.ac.uk/projects/irs/ns/rdf-in-ocml#")))

(defun register-source-namespace (prefix url)
  (push (list prefix url) *source-namespaces*)
  (setup-namespaces-for-source))

;;; In here are definitions of OCML namespace prefixes that we need at
;;; compile time.
(defun setup-namespaces-for-source ()
  "Set the current namespaces for building source."
  ;; XXX We should (but are not!) careful that we don't break the
  ;; bindings of the current ontology.
  (dolist (namespace *source-namespaces*)
    (ocml:register-namespace (first namespace) (second namespace))))

(eval-when (:load-toplevel)
  (setup-namespaces-for-source))
