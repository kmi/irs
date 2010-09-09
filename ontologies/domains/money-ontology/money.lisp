;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")


(def-class Geographical-Region (temporal-thing))


(def-class Geopolitical-Entity (Geographical-Region Generic-Agent))

(def-class country (Geopolitical-Entity)
   ((has-currency :type currency)
    (has-government :type government)))

(def-class government (organization))

(def-class currency (information-bearing-object)
   ((issued-by :type government)))



