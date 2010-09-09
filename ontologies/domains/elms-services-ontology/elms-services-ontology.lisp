;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology elms-services-ontology)


(def-class catalogue-data ()
  ((has-product-code :type string)
   (has-description :type string)
   (has-cost :type string)
   (has-max-user-weight :type integer)
   (has-charging-value :type string)
   (has-product-widtht :type string)
   (has-product-height :type string)
   (has-product-seat-height :type string)
   (has-product-depth :type string)
   (has-technician-fit :type boolean)
   (has-product-weight :type string)
   (has-narrative-detail :type string)
   (has-essex-ss-ot-dept :type string)
   (has-main-supplier :type string)
   (has-telephone-number :type string)
   (has-fax-number :type string)
   (has-category :type string)))