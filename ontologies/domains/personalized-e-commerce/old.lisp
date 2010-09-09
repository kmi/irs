;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology personalized-e-commerce)

(def-class shopping-item ()
  ((has-normal-price :type real-number :min-cardinality 1)
   (has-current-price :type real-number :min-cardinality 1)     
   (is-on-sale :type boolean :cardinality 1)
   (related-items :type shopping-item)
   (is-bought-by :type customer-profile)
   (short-description :type string)
   (long-description :type string)
   (item-name :type string)))

(def-class customer (affiliated-person)
  ((customer-id :type string :cardinality 1)
   (has-profile :type customer-profile)
   (has-age-range :type age-range)
   (personal-details :type person-type)))

(def-class customer-profile (information-stuff)
  ((number-of-visits :type integer :cardinality 1)
   (number-of-transactions :type integer :cardinality 1)
   (type-of-goods-bought :type list)
   (type-of-goods-most-freequently-bought :type list)
   (list-of-goods-bought :type shopping-item)))

(def-class package-deal (temporal-thing)
 ((deal-price :type real-number :min-cardinality 1)
  (in-store-price :type real-number :cardinality 1)
  (has-shopping-items :type shopping-item)
  (is-bought-by :type customer-profile)
  (short-description :type string)
  (long-description :type string)
  (deal-name :type string)))

(def-class shopping-aisle ()
  ((has-items :type shopping-item)))

(def-relation is-part-of-package-deal (?pd ?i))

(def-relation is-in-category (?x ?cat))

(def-relation has-category (?x ?cat))

(def-function all-items-in-package-deal (?pd) -> ?items
  :body (all-slot-values ?pd has-shopping-items))

(def-function all-item-names-in-deal (?pd) -> ?names
  :body 
  (setofall ?n (and (has-shopping-items ?pd ?i) 
                      (item-name ?i ?n))))

(def-rule part-of-package-deal
  ((is-part-of-package-deal ?pd ?i)
   if
   (= ?pd (setofall ?pd (has-shopping-items ?pd ?i)))))

(def-rule find-category-of-entity
  ((is-in-category ?x ?cat)
   if
   (or (direct-superclass-of ?cat ?x)
       (and (instance ?x)
            (instance-of ?x ?cat)))))

(def-rule find-subcategory-of-entity
  ((has-category ?x ?cat)
   if
   (or (direct-subclass-of ?cat ?x)
       (and (instance ?x)
            (direct-instance-of ?x ?c)
            (subclass-of ?cat ?c)))))

