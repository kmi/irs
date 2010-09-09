;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defun initialise ()
  (setup-library)
  (setup-colours))

;;;now make sure the variables for the users and ontologies are up to date
(defun setup-library ()
  (read-in-users-string)
  (read-in-users)
  (read-in-ontology-names)
  (read-in-ontology-names-string)
  (load-all-ontologies)
  (http::read-in-tables)
  ;;(setup-ontology-home-pathnames)
  ;;(setup-load-files)

  ;; We save copies of OCML files which are changed through the Web
  ;; interface in here, so make sure it exists.
  (ocml::create-directory webonto::*old-source-directory*))
