;;; Copyright © 2009 The Open University

(in-package #:ocml)

(in-ontology cryptography)

(defun %md5 (data)
  "Compute the MD5 digest on DATA using KEY."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence  :md5 (flexi-streams:string-to-octets data))))

(def-function #_md5 (data key)
  "Return the MD5 digest of DATA, using KEY."
    :lisp-fun #'%md5)
