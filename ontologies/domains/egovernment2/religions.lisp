;;; Mode: Lisp; Package: ocml

;;;Authors Liliana Cabral, John Domingue

;;; File created in WebOnto

(in-package "OCML")

(in-ontology egovernment2)


(def-class religious-faith ())

(def-class religious-organization (charitable-organization)
  ((belongs-to-faith :type religious-faith)))

(def-instance bahai-faith religious-faith)

(def-instance buddhism religious-faith)

(def-instance christianity religious-faith)

(def-instance confucianism religious-faith)

(def-instance hinduism religious-faith)

(def-instance islam religious-faith)

(def-instance jainism religious-faith)

(def-instance judaism religious-faith)

(def-instance shinto religious-faith)

(def-instance sikhism religious-faith)

(def-instance taoism religious-faith)

(def-instance vodun religious-faith)
