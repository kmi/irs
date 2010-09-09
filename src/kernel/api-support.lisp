;;; Copyright © 2008 The Open University

(in-package #:irs)

(defun tobool (str)
  "Convert an HTTP GET true/false value to a Lisp boolean."
  (equal str "true"))

(defun find-ontology (ontology)
  "Get ontology by name or IRI."
  (if (symbolp ontology)
      (ocml::get-ontology ontology :error-if-not-found nil)
      (multiple-value-bind (guessed-ocmlname namespace-iri)
          (identify-ontology-by-wsml-iri ontology)
        (if guessed-ocmlname
            (cdr (find namespace-iri
                       ocml::*all-ontologies* :test #'string=
                       :key (lambda (pair)
                              (ocml::namespace-uri-of (cdr pair)))))
            (ocml::get-ontology (intern ontology :ocml)
                                :error-if-not-found nil)))))

(defun identify-ontology-by-wsml-iri (iri)
  "WSML.  Return the likely OCML name, and the namespace IRI."
  (let ((hash (position #\# iri)))
    (if hash
        (let ((ocml-name (intern (subseq iri (+ 1 hash)) :ocml))
              (namespace-iri (subseq iri 0 (+ 1 hash))))
          (values ocml-name namespace-iri)))))