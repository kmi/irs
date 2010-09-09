(in-package #:ocml)

(in-ontology mime)

;;; XXX This should be a relation (#_base64), not a function.
(def-function #_encode-base64 (octets)
  :lisp-fun #'base64:usb8-array-to-base64-string)
