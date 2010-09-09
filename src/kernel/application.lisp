;;; Application interface

(in-package :irs)

(defgeneric initialise-application (application)
  (:documentation "Initialise data structures for the APPLICATION.
  Should hold between images."))

(defmethod initialise-application :before (application)
  (initialise))

(defmethod initialise-application :after (application)
  (webonto:setup-library))

(defgeneric start-application (application)
  (:documentation "Run APPLICATION specific code in the IRS.  Add
  process visualisers.  Must be run after loading image."))

(defmethod start-application :after (application)
  #+:irs-lispworks
  (visualiser:refresh))

(defun use-application (application)
  (require application)
  (initialise-application application)
  (start-application application))
