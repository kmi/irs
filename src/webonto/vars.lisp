;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package :webonto)

(defvar *webonto-edits-filename* "new.lisp")

(defvar *webonto-new-filename* "new.lisp")

(defvar *load-file-header*
    ";;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package \"OCML\")")

(defvar *edits-file-header* *load-file-header*)

(defvar *users* nil)

(defvar *default-port* 3000)

(defvar *default-host*
  #+:irs-use-lispweb (format nil "http://~a" (tcp::full-hostname))
  #-:irs-use-lispweb "http://localhost/")

(defvar *ocml-line-separator* #\@)

(defvar cl-user::*ocml-line-separator* *ocml-line-separator*)

(defvar *required-ontologies* '()
  "Ontologies to be loaded by SETUP-LIBRARY.  Not affected by the
  ontology-names file.")

(defun require-ontologies (ontologies)
  "Ensure ONTOLOGIES are loaded after next call to SETUP-LIBRARY."
  (dolist (o ontologies)
    (pushnew (intern (symbol-name o) :ocml) *required-ontologies*)))

(defvar *ontology-names-file*)

(defvar *ontology-home-directories-file*)

(defvar *ontologies*)

(defvar *ontologies-string*)

(defvar *ontology-home-pathnames*)

(defvar *ontology-home-pathnames-string*)

(define-constant +ellipsis+ "...")

