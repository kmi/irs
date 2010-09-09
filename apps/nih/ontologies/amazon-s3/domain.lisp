(in-package #:ocml)

(in-ontology amazon-s3)

;;;; Domain ontology bits.

(def-class #_amazon-account ()
  ((#_has-amazon-access-key :type #_amazon-access-key)
   (#_has-amazon-secret-key :type #_amazon-secret-key)))

(def-class #_amazon-access-key ()
    ((#_has-value :type string)))

(def-class #_amazon-secret-key ()
    ((#_has-value :type string)))

(def-class #_amazon-bucket ()
    ((#_has-value :type string)))

(def-class #_amazon-object-key ()
    ((#_has-value :type string)))

;;; {{{ Skyhooks

(define-skyhook skyhook-amazon-account #_amazon-account
  (lambda (value)
    (irs.grounding::skyhook-by-instance-name
     '#_amazon-account (read-ocml-from-string value))))

(define-skyhook skyhook-amazon-bucket #_amazon-bucket
  (lambda (value)
    (name (new-instance '#_amazon-bucket `((#_has-value ,value))))))

(define-skyhook skyhook-amazon-key #_amazon-key
  (lambda (value)
    (name (new-instance '#_amazon-key `((#_has-value ,value))))))

(define-skyhook skyhook-string string
  (lambda (value)
    value))

;;; }}}
