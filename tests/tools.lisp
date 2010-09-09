;;; Copyright © 2008 The Open University

(in-package #:irs.tests)

(defun parse-xml (string)
  "Parse the XML in STRING to a DOM object."
  (cxml:parse (flexi-streams:string-to-octets string)
              (cxml-dom:make-dom-builder)))

(defun the-irs-uri (control &rest rest)
  (concatenate 'string "http://localhost:8080" (apply #'format nil control rest)))

;;; {{{ Debug tools for API-REST's ontology serialisation.
(defun dump-to-file (object file)
  (with-open-file (str file :direction :output :if-exists :supersede)
    (write-sequence object str)))

(defun roundtrip-ontology (ontology-name &optional raw)
  "Check downloading, uploading, and download again the XML of
ONTOLOGY-NAME produces the same text."
  (ocml:load-ontology-by-name ontology-name)
  (let ((clean (irs.api.rest::serialise-ontology
                (ocml::get-ontology ontology-name))))
    (irs.api.rest::send-ontology* clean)
    (let ((dirty (irs.api.rest::serialise-ontology
                  (ocml::get-ontology ontology-name))))
      (values (string= dirty clean)
              (if raw clean (length clean))
              (if raw dirty (length dirty))))))

;;; xmlindent clean.xml > cleaner.xml; \
;;; xmlindent dirty.xml > dirtier.xml; \
;;; diff -u cleaner.xml dirtier.xml > diff.xml
(defun roundtrip-ontology-and-store (ontology-name)
  (multiple-value-bind (match clean dirty)
      (roundtrip-ontology ontology-name t)
    (dump-to-file clean "clean.xml")
    (dump-to-file dirty "dirty.xml")
    match))
;;; }}}
