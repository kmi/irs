;;; Mode: Lisp; Package: ocml

;; Units Manipulation
;; (Imports International System of Units Ontology)
;;
;; Units Manipulation brings support for the algebraic manipulation of physical quantities
;; so that units of measure can be taken into account. It therefore redefines the main
;; algebraic operations in order to take units of measure into account. It is informed by 
;; EngMath but we are here concerned about the computation of operations as opposed to 
;; EngMath which is solely a support for sharing definition and doesn't actually compute 
;; them.
;;
;; Informed by EngMath: T. R. Gruber and G. R. Olsen. An ontology for
;; engineering mathematics. In J. Doyle, P. To rasso, and
;; E. Sandewall, editors, Fourth International Conference on
;; Principles of Knowledge Representation and Reasoning, pages
;; 258-269, Bonn, Germany, 1994. Morgan Kaufmann.
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 1.0

;; Comments 
;;
;; Conversion Rates are defined generically when possible and specifically on the basis of
;; the base unit for certain cases like time. This in fact introduces a dependency between
;; scales and units but it is necessary for a simpler conversion and definition

(in-package "OCML")

(in-ontology units-manipulation)

;; Include axiom
(def-function #_magnitudeInUnit (?q ?unit) -> ?mag
  "The magnitude of a constant-quantity is a numeric value for the
quantity given in terms of some unit-of-measure.  For example, the
magnitude of the quantity 2 kilometers in the unit-of-measure meter is
the real number 2000. Units of measure are scalar quantities, and magnitude is defined in
terms of scalar multiplication.
This function uses primitive methods in order to return numbers that can then
be involved into further native operations"

  :constraint (and (#_phys-q:ConstantQuantity ?q)
                   (not (#_phys-q:DimensionlessQuantity ?q))
                   (#_phys-q:UnitOfMeasure ?unit))

  :def (and (#_phys-q:ConstantQuantity ?q)
            (#_phys-q:UnitOfMeasure ?unit)
            (#_phys-q:DimesionlessQuantity ?mag)
            (#_phys-q:compatibleQuantities ?q ?unit))

  :body (#_primitiveDivides (#_primitiveTimes (#_phys-q:getMagnitude ?q)
                                              (#_phys-q:quantityConversionRate ?q))
                            (#_phys-q:unitConversionRate ?unit)))


(def-function #_magnitudeInBaseUnit (?q ?system) -> ?mag
  "Return the magnitude of the quantity using the base unit for the dimension in the given system of units. The system of units is actually ignored when it comes to Dimensionless units but it is convenient to use the same function for homogeneity."

  :constraint (and (#_phys-q:ConstantQuantity ?q)
                   (not (#_phys-q:DimensionlessQuantity ?q))
                   (#_phys-q:SystemOfUnits ?system)
                   (#_phys-q:DimensionlessQuantity ?mag))

  :body (#_primitiveTimes (#_phys-q:getMagnitude ?q)
                          (#_phys-q:unitConversionRate (the-slot-value ?q #_phys-q:hasUnitOfMeasure))))

;;
;; Units Manipulation
;;

;;
;; BASE ARITHMETIC OPERATIONS
;; We do not use namespaces here in order to support the definition of formulas
;; in a reasonable manner. This is also necessary in order override existing definitions
;; which could be extremely confusing.
;;
;; Fix this so that the instance created has a namespace
(def-function + (?x &rest ?y) -> ?z
  "Adds physical quantities"
  :constraint (and (#_phys-q:ConstantQuantity ?x) 
                   (#_phys-q:ConstantQuantity ?y)
                   (#_phys-q:compatibleQuantities ?x ?y))

  :lisp-fun (lambda (x y)
              (if (and (holds? '#_phys-q:DimensionlessQuantity x)
                       (holds? '#_phys-q:DimensionlessQuantity y))
                  (ocml-eval-fun `(#_primitivePlus ,(ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                                   ,(ocml-eval-fun `(#_phys-q:getMagnitude ,y))))

              (name (new-instance '#_phys-q:ConstantQuantity
                            `((#_phys-q:hasMagnitude
                               ,(ocml-eval-fun 
                                 `(#_primitivePlus ,(ocml-eval-fun `(#_magnitudeInBaseUnit ,x #_si:InternationalSystemOfUnits))
                                                   ,(ocml-eval-fun `(#_magnitudeInBaseUnit ,y #_si:InternationalSystemOfUnits)))))

                              (#_phys-q:hasUnitOfMeasure
                                 ,(ocml-eval-fun 
                                   `(#_phys-q:baseUnit #_si:InternationalSystemOfUnits ,(ocml-eval-fun `(#_phys-q:quantityDimension ,x)))))))))))


(def-function #_primitivePlus (?X &rest ?y)
  "Adds numbers"
  :constraint (and (number ?x) (number ?y))
  :lisp-fun #'+)

(def-function - (?x &rest ?y) -> ?z
  "Substracts physical quantities"
  :constraint (and (#_phys-q:ConstantQuantity ?x) 
                   (#_phys-q:ConstantQuantity ?y)
                   (#_phys-q:compatibleQuantities ?x ?y))

  :lisp-fun (lambda (x y)
              (if (and (holds? '#_phys-q:DimensionlessQuantity x)
                       (holds? '#_phys-q:DimensionlessQuantity y))
                  (ocml-eval-fun `(#_primitiveMinus ,(ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                                    ,(ocml-eval-fun `(#_phys-q:getMagnitude ,y))))

              (name (new-instance '#_phys-q:ConstantQuantity
                            `((#_phys-q:hasMagnitude
                               ,(ocml-eval-fun 
                                 `(#_primitiveMinus ,(ocml-eval-fun `(#_magnitudeInBaseUnit ,x #_si:InternationalSystemOfUnits))
                                                   ,(ocml-eval-fun `(#_magnitudeInBaseUnit ,y #_si:InternationalSystemOfUnits)))))

                              (#_phys-q:hasUnitOfMeasure
                                 ,(ocml-eval-fun 
                                   `(#_phys-q:baseUnit #_si:InternationalSystemOfUnits ,(ocml-eval-fun `(#_phys-q:quantityDimension ,x)))))))))))

(def-function #_primitiveMinus (?X &rest ?y)
  "Adds numbers"
  :constraint (and (number ?x) (number ?y))
  :lisp-fun #'-)

;; NOTE: Times not implemented for inputs where both are Constant Quantities
(def-function * (?x &rest ?y) -> ?z
  "Multiplies physical quantities. We still do not contemplate the multiplication of 2 quantities with dimension since we would also need to compute the resulting dimension."
  :constraint (and (#_phys-q:ConstantQuantity ?x) 
                   (#_phys-q:ConstantQuantity ?y)
                   (or (#_phys-q:DimensionlessQuantity ?x)
                       (#_phys-q:DimensionlessQuantity ?y)))

  :lisp-fun (lambda (x y)
              (if (and (holds? '#_phys-q:DimensionlessQuantity x)
                       (holds? '#_phys-q:DimensionlessQuantity y))
                  (ocml-eval-fun `(#_primitiveTimes ,(ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                                    ,(ocml-eval-fun `(#_phys-q:getMagnitude ,y))))

                (let ((unit-of-measure (if (holds? '#_phys-q:DimensionlessQuantity x)
                                           (ocml-eval-fun `(the-slot-value ,y #_phys-q:hasUnitOfMeasure))
                                         (ocml-eval-fun `(the-slot-value ,x #_phys-q:hasUnitOfMeasure)))))

                  (name (new-instance '#_phys-q:ConstantQuantity
                                      `((#_phys-q:hasMagnitude
                                         ,(ocml-eval-fun 
                                           `(#_primitiveTimes ,(ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                                             ,(ocml-eval-fun `(#_phys-q:getMagnitude ,y)))))

                                        (#_phys-q:hasUnitOfMeasure ,unit-of-measure))))))))

(def-function #_primitiveTimes (?X &rest ?y)
  "Multiplies numbers"
  :constraint (and (number ?x) (number ?y))
  :lisp-fun #'*)

(def-function / (?x &rest ?y) -> ?z
  "Divides physical quantities. The divisor should be a dimensionless quantity."
  :constraint (and (#_phys-q:ConstantQuantity ?x) 
                   (#_phys-q:DimensionlessQuantity ?y))

  :lisp-fun (lambda (x y)
              (if (and (holds? '#_phys-q:DimensionlessQuantity x)
                       (holds? '#_phys-q:DimensionlessQuantity y))
                  (ocml-eval-fun `(#_primitiveDivides ,(ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                                      ,(ocml-eval-fun `(#_phys-q:getMagnitude ,y))))

                (let ((unit-of-measure (ocml-eval-fun `(the-slot-value ,x #_phys-q:hasUnitOfMeasure))))
                  (name (new-instance '#_phys-q:ConstantQuantity
                                      `((#_phys-q:hasMagnitude
                                         ,(ocml-eval-fun 
                                           `(#_primitiveDivides ,(ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                                             ,(ocml-eval-fun `(#_phys-q:getMagnitude ,y)))))

                                        (#_phys-q:hasUnitOfMeasure ,unit-of-measure))))))))

(def-function #_primitiveDivides (?X &rest ?y)
  "Multiplies numbers"
  :constraint (and (number ?x) (number ?y))
  :lisp-fun #'/)


;;
;; Extended set of arithmetic operations 
;;

(def-function min (?quantities) -> ?q
  "Returns the minimum of the Physical Quantities. Takes into account units."
  :constraint (and (every ?quantities #_phys-q:PhysicalQuantity)
                   (not (exists ?x ?y
                                (and (member ?x ?quantities)
                                     (member ?y ?quantities)
                                     (not (#_phys-q:compatibleQuantities ?x ?y))))))

  :body (the ?q 
             (and (member ?q ?quantities)
                  (not (exists ?x
                               (and (member ?x ?quantities)
                                    (< ?x ?q)))))))

(def-function max (?quantities) -> ?q
  "Returns the maximum of the Physical Quantities. Takes into account units."
  :constraint (and (every ?quantities #_phys-q:PhysicalQuantity)
                   (not (exists ?x ?y
                                (and (member ?x ?quantities)
                                     (member ?y ?quantities)
                                     (not (#_phys-q:compatibleQuantities ?x ?y))))))

  :body (the ?q 
             (and (member ?q ?quantities)
                  (not (exists ?x
                               (and (member ?x ?quantities)
                                    (> ?x ?q)))))))


;;
;; Physical Quantities Relations
;;

(def-rule proveLowerThan 
   "A predicate to test whether a Physical Quantity is less than another. Polymorphic relation."

   ((#_lowerThan ?x ?y)
   if 
   (#_phys-q:PhysicalQuantity ?x)
   (#_phys-q:PhysicalQuantity ?y)
   (not (#_phys-q:DimensionlessQuantity ?x))
   (not (#_phys-q:DimensionlessQuantity ?y))
   (#_phys-q:compatibleQuantities ?x ?y)
   (#_primitiveLowerThan (#_magnitudeInBaseUnit ?x #_si:InternationalSystemOfUnits)
                         (#_magnitudeInBaseUnit ?y #_si:InternationalSystemOfUnits)))

   ((#_lowerThan ?x ?y)
   if 
   (#_phys-q:DimensionlessQuantity ?x)
   (#_phys-q:DimensionlessQuantity ?y)
   (#_primitiveLowerThan (#_phys-q:getMagnitude ?x )
                         (#_phys-q:getMagnitude ?y))))
   
;; Redefine the Relation from Base-Ontology
(def-relation < (?x ?y)
  "Comparison for Physical Quantities. True if x < y. One can also use the relation #_lowerThan."
  :constraint (and (#_phys-q:PhysicalQuantity ?x)
                   (#_phys-q:PhysicalQuantity ?y))

  :iff-def (#_lowerThan ?x ?y))

(def-relation #_primitiveLowerThan (?x ?y)
   "A predicate to test whether a number is less than another"
   :constraint (and (number ?x)(number ?y))
   :lisp-fun #'(lambda (x y env)
                 (if (< (instantiate x env)
                        (instantiate y env))
		     (List env)
		     :fail)))


(def-rule proveGreaterThan 
   "A predicate to test whether a Physical Quantity is more than another. Polymorphic relation."

   ((#_greaterThan ?x ?y)
   if 
   (#_phys-q:PhysicalQuantity ?x)
   (#_phys-q:PhysicalQuantity ?y)
   (not (#_phys-q:DimensionlessQuantity ?x))
   (not (#_phys-q:DimensionlessQuantity ?y))
   (#_phys-q:compatibleQuantities ?x ?y)
   (#_primitiveGreaterThan (#_magnitudeInBaseUnit ?x #_si:InternationalSystemOfUnits)
                           (#_magnitudeInBaseUnit ?y #_si:InternationalSystemOfUnits)))

   ((#_greaterThan ?x ?y)
   if 
   (#_phys-q:DimensionlessQuantity ?x)
   (#_phys-q:DimensionlessQuantity ?y)
   (#_primitiveGreaterThan (#_phys-q:getMagnitude ?x )
                           (#_phys-q:getMagnitude ?y))))
   
;; Redefine the Relation from Base-Ontology
(def-relation > (?x ?y)
  "Comparison for Physical Quantities. True if x > y. One can also use the relation #_greaterThan."
  :constraint (and (#_phys-q:PhysicalQuantity ?x)
                   (#_phys-q:PhysicalQuantity ?y))

  :iff-def (#_greaterThan ?x ?y))

(def-relation #_primitiveGreaterThan (?x ?y)
   "A predicate to test whether a number is greater than another"
   :constraint (and (number ?x)(number ?y))
   :lisp-fun #'(lambda (x y env)
                 (if (> (instantiate x env)
                        (instantiate y env))
		     (List env)
		     :fail)))

(def-relation equal (?x ?y)
  "Checks whether two Physical Quantities are equal. This method is to be used instead of = which is true if the instances are both the same."
  :constraint  (and (#_phys-q:PhysicalQuantity ?x)
                    (#_phys-q:PhysicalQuantity ?y))

  :iff-def (or (and (Number ?x)
                    (Number ?y)
                    (= ?x ?y))
               
               (and (not (Number ?x))
                    (#_phys-q:compatibleQuantities ?x ?y)
                    (= (#_magnitudeInBaseUnit ?x #_si:InternationalSystemOfUnits)
                       (#_magnitudeInBaseUnit ?y #_si:InternationalSystemOfUnits)))))


(def-relation <= (?x ?y)
   "A predicate to test whether a Physical Quantity is less or equal to another"
   :constraint (and (#_phys-q:PhysicalQuantity ?x)
                    (#_phys-q:PhysicalQuantity ?y))

   :iff-def (or (equal ?x ?y)
                (< ?x ?y)))

(def-relation >= (?x ?y)
   "A predicate to test whether a Physical Quantity is greater or equal to another"
   :constraint (and (#_phys-q:PhysicalQuantity ?x)
                    (#_phys-q:PhysicalQuantity ?y))

   :iff-def (or (equal ?x ?y)
                (> ?x ?y)))


;;
;; Statistical Functions
;;
(def-function #_createConstantQuantityInstance (?sample ?mag) -> ?inst
  "Helper function for creating a Constant Quantity instance. It uses a sample instance for determining the kind of instance to be created as well as for finding the dimension if necessary."

  :constraint (and (or (#_phys-q:ConstantQuantity ?sample)
                       (#_phys-q:DimensionlessQuantity ?sample))
                   (Number ?mag))

  :lisp-fun #'(lambda (sample mag)
                (if (holds? '#_phys-q:DimensionlessQuantity sample)
                    (name (new-instance '#_phys-q:DimensionlessQuantity
                                  `((#_phys-q:hasMagnitude ,mag))))
                  
                  (name (new-instance '#_phys-q:ConstantQuantity
                                `((#_phys-q:hasMagnitude ,mag)
                                  (#_phys-q:hasUnitOfMeasure
                                   ,(ocml-eval-fun 
                                     `(#_phys-q:baseUnit #_si:InternationalSystemOfUnits ,(ocml-eval-fun `(#_phys-q:quantityDimension ,sample)))))))))))

(def-function mean (?quantities) -> ?avg
  "Returns the mean of the Physical Quantities. Takes into account units."

  :constraint (and (every ?quantities #_phys-q:PhysicalQuantity)
                   (not (exists ?x ?y
                                (and (member ?x ?quantities)
                                     (member ?y ?quantities)
                                     (not (#_phys-q:compatibleQuantities ?x ?y))))))

  :lisp-fun #'(lambda (quantities)
                (let ((result
                       (cl-stats:mean 
                        (mapcar #'(lambda (x)
                                    (if (holds? '#_phys-q:DimensionlessQuantity x)
                                        (ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                      (ocml-eval-fun `(#_magnitudeInBaseUnit ,x #_si:InternationalSystemOfUnits))))
                                quantities))))
                  ;; Create the instance if necessary
                  (if (holds? '#_phys-q:DimensionlessQuantity (car quantities))
                      result
                    (ocml-eval-fun `(#_createConstantQuantityInstance ,(car quantities) ,result))))))

  
(def-function sum (?quantities) -> ?sum
  "Computes the sum of all the quantities given in the list"

  :constraint (and (every ?quantities #_phys-q:PhysicalQuantity)
                   (not (exists ?x ?y
                                (and (member ?x ?quantities)
                                     (member ?y ?quantities)
                                     (not (#_phys-q:compatibleQuantities ?x ?y))))))

  :lisp-fun #'(lambda (quantities)
                (let ((result
                       (reduce #'+ 
                               (mapcar #'(lambda (x)
                                           (if (holds? '#_phys-q:DimensionlessQuantity x)
                                               (ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                             (ocml-eval-fun `(#_magnitudeInBaseUnit ,x #_si:InternationalSystemOfUnits))))
                               quantities))))
                  ;; Create the instance if necessary
                  (if (holds? '#_phys-q:DimensionlessQuantity (car quantities))
                      result
                    (ocml-eval-fun `(#_createConstantQuantityInstance ,(car quantities) ,result))))))


;; TODO: Doesn't really make sense for Unit that have a dimension
(def-function prod (?quantities) -> ?prod
  "Computes the product of all the quantities given in the list"

  :constraint (and (every ?quantities #_phys-q:PhysicalQuantity)
                   (not (exists ?x ?y
                                (and (member ?x ?quantities)
                                     (member ?y ?quantities)
                                     (not (#_phys-q:compatibleQuantities ?x ?y))))))

  :lisp-fun #'(lambda (quantities)
                (let ((result
                       (reduce #'* 
                               (mapcar #'(lambda (x)
                                           (if (holds? '#_phys-q:DimensionlessQuantity x)
                                               (ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                             (ocml-eval-fun `(#_magnitudeInBaseUnit ,x #_si:InternationalSystemOfUnits))))
                               quantities))))
                  ;; Create the instance if necessary
                  (if (holds? '#_phys-q:DimensionlessQuantity (car quantities))
                      result
                  (ocml-eval-fun `(#_createConstantQuantityInstance ,(car quantities) ,result))))))

(def-function count (?list) -> ?count
  "Function that returns the length of a list."
  :constraint (list ?list)

  :lisp-fun #'length)
              
    
(def-function variance (?quantities) -> ?variance
  "Computes the variance for all the quantities given in the list. Takes into account units and performs the appropriate transformation. The results will be based on the standard unit for the given dimension."

  :constraint (and (every ?quantities #_phys-q:PhysicalQuantity)
                   (not (exists ?x ?y
                                (and (member ?x ?quantities)
                                     (member ?y ?quantities)
                                     (not (#_phys-q:compatibleQuantities ?x ?y))))))

  :lisp-fun #'(lambda (quantities)
                (let ((list
                       (mapcar #'(lambda (x)
                                   (if (holds? '#_phys-q:DimensionlessQuantity x)
                                       (ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                     (ocml-eval-fun `(#_magnitudeInBaseUnit ,x #_si:InternationalSystemOfUnits))))
                               quantities)))

                  ;; Compute the variance
                  (cl-stats:variance list))))

(def-function standard-deviation (?quantities) -> ?sd
  "Computes the standard deviation for all the quantities given in the list. Takes into account units and performs the appropriate transformation. The results will be based on the standard unit for the given dimension."

  :constraint (and (every ?quantities #_phys-q:PhysicalQuantity)
                   (not (exists ?x ?y
                                (and (member ?x ?quantities)
                                     (member ?y ?quantities)
                                     (not (#_phys-q:compatibleQuantities ?x ?y))))))

  :lisp-fun #'(lambda (quantities)
                (let ((list
                       (mapcar #'(lambda (x)
                                   (if (holds? '#_phys-q:DimensionlessQuantity x)
                                       (ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                     (ocml-eval-fun `(#_magnitudeInBaseUnit ,x #_si:InternationalSystemOfUnits))))
                               quantities)))

                  ;; Compute the std-deviation
                  (cl-stats:standard-deviation list))))

(def-function range (?quantities) -> ?sd
  "Computes the range for all the quantities given in the list. Takes into account units and performs the appropriate transformation. The results will be based on the standard unit for the given dimension."

  :constraint (and (every ?quantities #_phys-q:PhysicalQuantity)
                   (not (exists ?x ?y
                                (and (member ?x ?quantities)
                                     (member ?y ?quantities)
                                     (not (#_phys-q:compatibleQuantities ?x ?y))))))

  :lisp-fun #'(lambda (quantities)
                (let ((list
                       (mapcar #'(lambda (x)
                                   (if (holds? '#_phys-q:DimensionlessQuantity x)
                                       (ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                     (ocml-eval-fun `(#_magnitudeInBaseUnit ,x #_si:InternationalSystemOfUnits))))
                               quantities)))

                  ;; Compute the range
                  (cl-stats:range list))))

(def-function mode (?quantities) -> ?mode
  "Computes the mode for all the quantities given in the list. Takes into account units and performs the appropriate transformation. The results will be based on the standard unit for the given dimension. This function returns a list with the most common result and the number of occurrences for this."

  :constraint (and (every ?quantities #_phys-q:PhysicalQuantity)
                   (not (exists ?x ?y
                                (and (member ?x ?quantities)
                                     (member ?y ?quantities)
                                     (not (#_phys-q:compatibleQuantities ?x ?y))))))

  :lisp-fun #'(lambda (quantities)
                (let* ((dimLess (holds? '#_phys-q:DimensionlessQuantity (car quantities)))
                       (numList
                        (mapcar #'(lambda (x)
                                    (if dimLess
                                        (ocml-eval-fun `(#_phys-q:getMagnitude ,x))
                                     (ocml-eval-fun `(#_magnitudeInBaseUnit ,x #_si:InternationalSystemOfUnits))))
                               quantities)))

                  ;; Compute the mode and return the Physical Quantity instance
                  ;; together with the occurrence
                  (multiple-value-bind (mode-values mode-count)
                      (cl-stats:mode numList)
                    (if dimLess 
                        (values (setofall '?x 
                                          `(and (member ?x ,quantities)
                                                (= ,(car mode-values) (#_phys-q:getMagnitude ?x)))) 
                                mode-count) 
                      
                      (values  (setofall '?x 
                                         `(and (member ?x ,quantities)
                                               (= ,(car mode-values) (#_magnitudeInBaseUnit ?x #_si:InternationalSystemOfUnits))))
                               mode-count)))))) 



