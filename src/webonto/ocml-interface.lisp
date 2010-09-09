;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

;;; Redefine ocml::ocml-metaclass.
(defclass ocml-metaclass (web-onto::ocml-class almost-ocml-metaclass)
  (
   ;; Templates for creating new instances and subclasses
   (subclass-template-variables :initform nil :accessor subclass-template-variables)
   (subclass-template :initform nil :accessor subclass-template)
   (instance-template-variables :initform nil :accessor instance-template-variables)
   (instance-template :initform nil :accessor instance-template)
   (patterns :initform nil :accessor class-patterns)))

(defclass ocml-ontology (almost-ocml-ontology name-mixin documentation-mixin
					      web-onto::ocml-ontology)
  ( ;; Extra slots for Web-Onto.
   (locked-p :initform nil :accessor locked-p)
   (current-editor :initform nil :accessor current-editor)

   ;; Extra slots for the IRS.
   (corba-methods :initform nil :accessor corba-methods)))

(defun web-onto::ontology-directory (ontology)
  (translate-logical-pathname (ontology-pathname ontology)))

(defun ontology-new-source-location (ontology)
  (merge-pathnames (web-onto::ontology-directory ontology)
                   web-onto::*webonto-new-filename*))

(defun ontology-load-filename (ontology)
  (merge-pathnames (web-onto::ontology-directory ontology)
                   ocml:*load-filename*))

(defun ontology-logical-load-filename (ontology)
  (concatenate 'string
               (ontology-pathname ontology)
               ocml:*load-filename*))
