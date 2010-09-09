;;; Mode: Lisp; Package: ocml

;; Physical Quantities Ontology
;;
;; Ontology that includes support for the manipulation of quantities expressed in some
;; unit of measure. Brings support for dimensional analysis.
;; Based on EngMath although our conceptualisation focusses on supporting 
;; the manipulation of quantities as opposed to EngMath that is aimed at supporting
;; transferring definitions but not computing them.

;; EngMath: T. R. Gruber and G. R. Olsen. An ontology for engineering
;; mathematics. In J. Doyle, P. To rasso, and E. Sandewall, editors,
;; Fourth International Conference on Principles of Knowledge
;; Representation and Reasoning, pages 258-269, Bonn, Germany,
;; 1994. Morgan Kaufmann.
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 1.0
;;


(in-package "OCML")

(in-ontology physical-quantities)

(def-class #_PhysicalQuantity () 
  "A physical-quantity is a measure of some quantifiable aspect of the
modeled world, such as 'the earth's diameter' (a constant length) and 
'the stress in a loaded deformable solid' (a measure of stress, which 
is a function of three spatial coordinates).  The first type is called 
constant-quantity and the second type is called function-quantity. 
All physical quantities are either constant-quantities or 
function-quantities.  Although the name and definition of this concept
is inspired from physics, physical quantities need not be material.
For example, amounts of money are physical quantities. 

   Physical quantities are distinguished from purely numeric entities
like a real numbers by their physical dimensions.  A
physical-dimension is a property that distinguishes types of
quantities.  Every physical-quantity has exactly one associated
physical-dimension.  In physics, we talk about dimensions such as
length, time, and velocity; again, nonphysical dimensions such as
currency are also possible.  The dimension of purely numeric entities
is the identity-dimension."
  
  ((#_hasUnitOfMeasure :type #_UnitOfMeasure :cardinality 1)))  

(def-class #_ConstantQuantity (#_PhysicalQuantity)
  "A constant-quantity is a constant value of some physical-quantity, like
3 meters or 55 miles per hour. All real-numbers are constant quantities whose dimension is the identity-dimension.

   All constant quantites can be expressed as the product of some 
dimensionless quantity and a unit of measure.  This is what it means
to say a quantity `has a magnitude'.  For example, 2 meters can be expressed 
as (* 3 meter), where meter is defined as a unit of measure for length.
All units of measure are also constant quantities."

  ((#_hasMagnitude :type #_DimensionlessQuantity :cardinality 1)))

;; TODO: Defined these
(def-class #_FunctionQuantity (#_PhysicalQuantity)
  "A Function Quantity is a function that maps from one or more
constant-quantities to a constant-quantity.  The function must have a
fixed arity of at least 1.  All elements of the range (ie, values of the
function) have the same physical-dimension, which is the dimension of
the function-quantity itself. The height over time of a particule in free fall is a Function Quantity. 
  Kept here for consistency with EngMath and eventual future extensions We should also capture that it is a Function and additional restrictions (see EngMath).")

(def-axiom #_ConstantAndFunctionQuantitiesDisjoint
  (exhaustive-subclass-partition #_PhysicalQuantity (set-of #_ConstantQuantity #_FunctionQuantity)))

(def-class #_DimensionlessQuantity (#_ConstantQuantity)
  "Although it sounds contradictory, a dimensionless-quantity is a
quantity whose Unit of Measure is the Identity Unit.  All numeric tensors, 
including real numbers, are nondimensional quantities."
  ((#_hasUnitOfMeasure :value #_IdentityUnit)))


(def-rule #_allNumbersAreDimensionlessQuantities
   "Rules that establishes that all 'numbers' are Dimensionless Quantities. Doing so allows us to treat homogeneously everything."
   ((#_DimensionlessQuantity ?x)
   if
   (Number ?x)))

(def-class #_ZeroQuantity (#_PhysicalQuantity)
  "A zero quantity is one which, when multiplied times any
quantity, results in another zero quantity (possibly the same zero).
The class of zero quantities includes the number 0, and zero
quantities for every physical dimension and order of tensor.")

(def-axiom #_zeroQuantitiesDefinition
  (and (#_PhysicalQuantity ?x)
       (forall (?q)
               (=> (#_PhysicalQuantity ?q)
                   (#_ZeroQuantity (* ?q ?x))))))

(def-axiom #_zeroIsAZeroQuantity
  (#_ZeroQuantity 0))

(def-class #_PhysicalDimension ()
  "A physical dimension is a property we associate with physical
quantities for purposes of classification or differentiation.  Mass,
length, and force are examples of physical dimensions.  Composite
physical dimensions can be described by composing primitive dimensions.
For example, Length/Time (length over time) is a dimension that can
be associated with a velocity.")

;; TODO: Additional axiom that states that the product of identity-dimension and any other dimension is that other dimension.
(def-instance #_IdentityDimension #_PhysicalDimension
  "Identity Dimension is the Dimension for the Dimensionless Quantities")

(def-class #_UnitOfMeasure ()
  "A unit-of-measure serves as a standard of measurement for some dimension. For example, the meter is a unit-of-measure for the length-dimension, as is the inch. Square-feet is a unit for length*length quantities. Units of Measure have a scale prefix that is a multiplier wrt to the reference unit within the dimension. Supports transformation between compatible units."

  ((#_hasDimension :type #_PhysicalDimension :cardinality 1)
   (#_hasConversionRate :type #_ConversionRate :cardinality 1)
   (#_hasAbbreviation :type String :cardinality 1))
)

(def-class #_ConversionRate ()
  "A conversion rate is basically a multiplier of the quantity wrt the reference unit within a certain dimension. Kilo, Mega, are examples of Scale Prefixes. By means of scale prefixes it is possible to transform between units."

  ((#_hasRate :type Number :cardinality 1))
)

(def-instance #_IdentityConversionRate #_ConversionRate
  "Conversion Rate for reference units"
  
  ((#_hasRate 1)))

(def-instance #_IdentityUnit #_UnitOfMeasure
  "The identity unit can be combined with any other unit to produce
the same unit.  The identity unit is the real number 1.  Its dimension 
is the identity-dimension."
  ((#_hasDimension #_IdentityDimension)
   (#_hasConversionRate #_IdentityConversionRate)
   (#_hasAbbreviation ""))
)

(def-class #_SystemOfUnits () ?system
  "A system-of-units defines a standard system of measurement. The mapping from dimensions to units in the system is provided by the function called standard-unit; since
this mapping is functional and total, there is exactly one unit
in the system of units per dimension."

  ((#_hasBaseUnit :type #_UnitOfMeasure))

  ;; Every unit in the system is the standard unit for its dimension. 
  :iff-def (forall ?baseUnit
                   (=> (= (has-slot-value ?system #_hasBaseUnit) ?baseUnit)
                       (not (exists ?unit2
                                    (and (= (has-slot-value ?system #_hasBaseUnit) ?unit2)
                                         (= (the-slot-value ?unit2 #_hasDimension) (the-slot-value ?baseUnit #_hasDimension))))))))
                
;; TODO: Add constraints for the composability of units
;; Requires fixing the relation for multiplying units
(def-function #_baseUnit (?system ?dim) -> ?unit
  "The Base Unit for a given system and dimension is a unit in that 
system whose dimension is the given dimension."

  :def (and (#_SystemOfUnits ?system)
            (#_PhysicalDimension ?dim)
            (#_UnitOfMeasure ?unit)
            (= (has-slot-value ?system #_hasBaseUnit) ?unit)
            (= (the-slot-value ?unit #_hasDimension) ?dim)
            (not (exists ?unit2
                         (and (has-slot-value ?system #_hasBaseUnit ?unit2)
                              (= (the-slot-value ?unit2 #_hasDimension) ?dim)))))

  :body (first (setofall ?unit 
                  (and (has-slot-value ?system #_hasBaseUnit ?unit)
                       (= (the-slot-value ?unit #_hasDimension) ?dim))))
)

(def-function #_quantityDimension (?q) -> ?dim
  "Obtain the quantity dimension. Also deals with numbers as a special case."

  :constraint (#_PhysicalQuantity ?q)

  :body (if (Number ?q)
            #_IdentityDimension
          (the-slot-value (the-slot-value ?q #_hasUnitOfMeasure) #_hasDimension)))

(def-function #_unitConversionRate (?unit) -> ?rate
  "Obtain the conversion rate to the base unit for the given unit of measure"
  :constraint (#_UnitOfMeasure ?unit)

  :body (the-slot-value (the-slot-value ?unit #_hasConversionRate)
                        #_hasRate))

(def-function #_quantityConversionRate (?q) -> ?rate
  "Obtain the conversion rate to the base unit for the given quantity. Deals with numbers as special case." 
  :constraint (#_PhysicalQuantity ?q)

  :body (if (Number ?q)
            1
          (#_unitConversionRate (the-slot-value ?q #_hasUnitOfMeasure))))

(def-relation #_compatibleQuantities (?q1 ?q2)
  "Two Quantities are compatible if their Physical Dimensions are equal. For example 20 feets, and 21 meters are compatible for the dimension is length in both cases."
  :constraint (and (#_PhysicalQuantity ?q1)
                   (#_PhysicalQuantity ?q2))

  :iff-def (= (#_quantityDimension ?q1)
              (#_quantityDimension ?q2)))

;; TODO Implement for Function Quantities
(def-function #_getMagnitude (?q) -> ?mag
  "Obtain the magnitude (in the defined unit of measure if any) of a given quantity." 
  :constraint (#_PhysicalQuantity ?q)

  :body (if (Number ?q)
            ?q
          (if (#_ConstantQuantity ?q) 
              (the-slot-value ?q #_hasMagnitude))))

