;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")


(def-class government (government-organization))

(def-class country (Geopolitical-Entity)
  ((has-capital :type capital-city)
   (has-currency :type currency)
   (has-government :type government)))

(def-class currency (information-bearing-object)
   ((issued-by :type government)))

(def-class european-currency (currency))

(def-class non-european-currency (currency))

(def-instance us-dollar non-european-currency)

(def-instance yen non-european-currency)

(def-instance euro european-currency)

(def-instance pound european-currency)

(def-instance kroner european-currency)
