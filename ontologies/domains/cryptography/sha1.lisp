;;; Copyright © 2008 The Open University

(in-package #:ocml)

(in-ontology cryptography)

(defun %hmac-sha1 (data key)
  "Compute an RFC 2104 HMAC-SHA1 digest on DATA using KEY."
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array key) :sha1)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array data))
    (ironclad:hmac-digest hmac)))

(def-function #_hmac-sha1 (data key)
  "Return the SHA-1 digest of DATA, using KEY."
  :lisp-fun #'%hmac-sha1)
