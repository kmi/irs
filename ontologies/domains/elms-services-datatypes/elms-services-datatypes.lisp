;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology elms-services-datatypes)


;;; 1) CatalogueEntryByWeightInterfaceOut -- ELMDB

(def-class catalogue-entry-by-weight-request-type ()
  ((has-max-client-weight :type integer)))

(def-class catalogue-entry-response-type ()
  ((has-catalogue-data :type catalogue-data)))

(def-class catalogue-data ()
  ((has-product-code :type string)
   (has-description :type string)
   (has-cost :type string)
   (has-max-user-weight :type integer)
   (has-charging-value :type string)
   (has-product-widtht :type string)
   (has-product-hight :type string)
   (has-product-seat-hight :type string)
   (has-product-depth :type string)
   (has-technician-fit :type boolean)
   (has-product-weight :type string)
   (has-narrative-detail :type string)
   (has-essex-ss-ot-dept :type string)
   (hsa-main-supplier :type string)
   (has-telephone-number :type string)
   (has-fax-number :type string)
   (has-category :type string)))

;;; 14) RedirectEquipmentToNewAdressInterfaceOut -- ELMDB

(def-class redirect-equipment-to-new-address-request-type ()
  ((has-citizen-key :type string)
   (has-new-address :type string)
   (has-new-phone-number :type string)))


(def-class redirect-equipment-to-new-address-response-type ()
  ((has-response :type string)))


;;; 15) ReturnEquipmentInterfaceOut --ELMDB

(def-class return-equipment-request-type ()
  ((has-citizen-code :type string)
   (has-item-code :type string)
   (has-return-date :type date))
  )

(def-class date ()
  ((has-date :type string))
)

(def-class return-equipment-response-type ()
  ((has-response :type string)))
