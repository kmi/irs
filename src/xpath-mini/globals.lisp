(in-package :xpath-mini)

(defvar *context*)

(defvar *ocml-ctx-ontology*)

(defvar *ocml-ctx-instance*)

(defvar *xml-ctx-sequence*)

(defvar *ocml-ctx-class*)

(defvar *read-ctx-sequence*)

;;; functions which have to be implemented in order to parse a certain
;;; kind of tree.  XXX Perhaps  replace with a class and methods?
(defvar *apply-leading-/* nil)

(defvar *apply-child* nil)

(defvar *apply-attribute* nil)
