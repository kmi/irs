;;; Copyright © 2008 The Open University

(in-package #:ocml)

(in-ontology cryptography)

(def-class #_CryptographicAlgorithm ())

(def-class #_DES (#_CryptographicAlgorithm))

(def-class #_3DES (#_DES))

(def-class #_AES (#_CryptographicAlgorithm))

(def-class #_RSA (#_CryptographicAlgorithm))
