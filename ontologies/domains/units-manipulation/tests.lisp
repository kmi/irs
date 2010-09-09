;;; Mode: Lisp; Package: ocml

;;; The Open University

;; Tests for the Units Manipulation Library

(in-package "OCML")

(in-ontology units-manipulation)

(def-instance #_5-Meters #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 5)
   (#_phys-q:hasUnitOfMeasure #_si:Meter)))

(def-instance #_7-Meters #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 7)
   (#_phys-q:hasUnitOfMeasure #_si:Meter)))

(def-instance #_17-Meters #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 17)
   (#_phys-q:hasUnitOfMeasure #_si:Meter)))

(def-instance #_107-Meters #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 107)
   (#_phys-q:hasUnitOfMeasure #_si:Meter)))

(def-instance #_70-Meters #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 70)
   (#_phys-q:hasUnitOfMeasure #_si:Meter)))

(def-instance #_1000-Meters #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 1000)
   (#_phys-q:hasUnitOfMeasure #_si:Meter)))

(def-instance #_1-Kilometer #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 1)
   (#_phys-q:hasUnitOfMeasure #_si:Kilometer)))

(def-instance #_2-Years #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 2)
   (#_phys-q:hasUnitOfMeasure #_si:Year)))

(def-instance #_5-Megabytes #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 5)
   (#_phys-q:hasUnitOfMeasure #_si:Megabyte)))

(def-instance #_8-Bits #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 8)
   (#_phys-q:hasUnitOfMeasure #_si:Bit)))

(def-instance #_22-Kilograms #_phys-q:ConstantQuantity
  ((#_phys-q:hasMagnitude 22)
   (#_phys-q:hasUnitOfMeasure #_si:Kilogram)))

;; Test basic manipulation functions
;;(ocml-eval-fun-term '#_phys-q:baseUnit '(#_si:InternationalSystemOfUnits #_si-dim:LengthDimension))

;;(ocml-eval-fun-term '#_phys-q:baseUnit '(#_si:InternationalSystemOfUnits #_si-dim:MassDimension))

;;(ocml-eval-fun-term '#_phys-q:baseUnit '(#_si:InternationalSystemOfUnits #_si-dim:AmountOfInformationDimension))

;;(ocml-eval-fun-term '#_phys-q:baseUnit '(#_si:InternationalSystemOfUnits #_si-dim:TimeDimension))

;;(ocml-eval-fun-term '#_phys-q:baseUnit '(#_si:InternationalSystemOfUnits #_si-dim:CurrencyDimension))

;;(ocml-eval-fun-term '#_phys-q:getMagnitude '(#_22-Kilograms))    
;;(ocml-eval-fun-term '#_phys-q:getMagnitude '(8))    

;;(ocml-eval-fun-term '#_magnitudeInBaseUnit '(#_22-Kilograms #_si:InternationalSystemOfUnits))

;;(ocml-eval-fun-term '#_magnitudeInBaseUnit '(#_8-Bits #_si:InternationalSystemOfUnits))

;;(ocml-eval-fun-term '#_magnitudeInBaseUnit '(#_5-Meters #_si:InternationalSystemOfUnits))

;;(ocml-eval-fun-term '#_magnitudeInBaseUnit '(#_1-Kilometer #_si:InternationalSystemOfUnits))

;;(ocml-eval-fun-term '#_primitivePlus '((#_magnitudeInBaseUnit #_5-Meters #_si:InternationalSystemOfUnits) (#_magnitudeInBaseUnit #_1-Kilometer #_si:InternationalSystemOfUnits)))

;;(ocml-eval-fun-term '#_primitivePlus '((#_phys-q:getMagnitude 8) (#_phys-q:getMagnitude 7)))

;;(holds? '#_phys-q:DimensionlessQuantity '8)
;;(holds? 'Number '8)

;;
;; Test Mathematical Operations over Numbers and Physical Quantities 
;;
;;(ocml-eval-fun-term '+ '(8 7))    
;;(ocml-eval-fun-term '+ '(8.5 7.5))    
;;(ocml-eval-fun-term '+ '(#_5-Meters #_1-Kilometer))

;;(ocml-eval-fun-term '- '(8 7))    
;;(ocml-eval-fun-term '- '(8 7.567778))    
;;(ocml-eval-fun-term '- '(#_1-Kilometer #_5-Meters))

;;(ocml-eval-fun-term '- '(7 8))    
;;(ocml-eval-fun-term '- '(#_5-Meters #_1-Kilometer))

;;     
;;(holds? '#_phys-q:compatibleQuantities '7 '8)


;;(holds? '#_phys-q:compatibleQuantities '#_1-Kilometer '#_5-Meters)
;;(holds? '#_phys-q:compatibleQuantities '#_1-Kilometer '#_22-Kilograms)


;;(ocml-eval-fun-term '#_phys-q:quantityConversionRate '(#_1-Kilometer))

;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_1-Kilometer #_si:Meter)) 
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_1-Kilometer #_si:Kilometer)) 
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_1-Kilometer #_si:Inch)) 
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_1-Kilometer #_si:Mile)) 
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_1-Kilometer #_si:Yard)) 

;; Time manipulation is a funny thing :)

;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_2-Years #_si:Month)) 
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_2-Years #_si:Day))
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_2-Years #_si:Minute))
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_2-Years #_si:Week))   

;; Amount of Information
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_5-Megabytes #_si:Byte))   
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_5-Megabytes #_si:Gigabyte))   
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_5-Megabytes #_si:Bit))   
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_5-Megabytes #_si:Kilobyte))  
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_8-Bits #_si:Byte))  

;; Mass
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_22-Kilograms #_si:Gram))    
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_22-Kilograms #_si:Milligram))    
;;(ocml-eval-fun-term '#_magnitudeInUnit '(#_22-Kilograms #_si:Stone))    

;; Relations Tests

;;(holds? '< '7 '8)
;;(holds? '< '9 '8)

;;(holds? '< '#_5-Megabytes '#_8-Bits)
;;(holds? '< '#_8-Bits '#_5-Megabytes)
;;(holds? '< '#_5-Meters '#_1-Kilometer)
;;(holds? '< '#_1-Kilometer '#_5-Meters)

;;(holds? '> '#_5-Megabytes '#_8-Bits)
;;(holds? '> '#_8-Bits '#_5-Megabytes)
;;(holds? '> '#_5-Meters '#_1-Kilometer)
;;(holds? '> '#_1-Kilometer '#_5-Meters)

;;(holds? '<= '#_5-Megabytes '#_8-Bits)
;;(holds? '<= '#_8-Bits '#_5-Megabytes)
;;(holds? '<= '#_1000-Meters '#_1-Kilometer)
;;(holds? '<= '#_1-Kilometer '#_1000-Meters)

;;(holds? '>= '#_5-Megabytes '#_8-Bits)
;;(holds? '>= '#_8-Bits '#_5-Megabytes)
;;(holds? '>= '#_1000-Meters '#_1-Kilometer)
;;(holds? '>= '#_1-Kilometer '#_1000-Meters)

;;(holds? '#_equal '#_1-Kilometer '#_5-Meters)
;;(holds? '#_equal '#_1-Kilometer '#_1000-Meters)
;;(holds? '#_equal '8 '8)


;; Arithmetic Operations Test
;;(ocml-eval-fun-term 'min '('(#_5-Meters #_7-Meters #_17-Meters #_107-Meters #_70-Meters #_1000-Meters #_1-Kilometer)))    

;;(ocml-eval-fun-term 'min '('(#_17-Meters #_7-Meters #_17-Meters #_107-Meters #_70-Meters #_1000-Meters #_1-Kilometer)))    

;;(ocml-eval-fun-term 'max '('(#_17-Meters #_7-Meters #_17-Meters #_107-Meters #_70-Meters #_1000-Meters #_1-Kilometer)))    

;;(ocml-eval-fun-term 'max '('(1 2 3 4 6 8 100)))

;;(ocml-eval-fun-term 'mean '('(1 2 3 4 6 8 100)))

;;(ocml-eval-fun-term 'mean '('(#_17-Meters #_7-Meters #_17-Meters #_107-Meters #_70-Meters #_1000-Meters #_1-Kilometer)))    

;;(ocml-eval-fun-term 'sum '('(#_17-Meters #_7-Meters #_17-Meters #_107-Meters #_70-Meters #_1000-Meters #_1-Kilometer)))    

;;(ocml-eval-fun-term 'sum '('(1 2 3 4 6 8 100)))

;;(ocml-eval-fun-term 'count '('(1 2 3 4 6 8 100)))

;;(ocml-eval-fun-term 'count '('(#_7-Meters #_17-Meters #_107-Meters #_70-Meters #_1000-Meters #_1-Kilometer)))    

;;(ocml-eval-fun-term 'variance '('(1 2 3 4 6 8 100)))

;;(ocml-eval-fun-term 'variance '('(#_7-Meters #_17-Meters #_107-Meters #_70-Meters #_1000-Meters #_1-Kilometer)))    

;;(ocml-eval-fun-term 'standard-deviation '('(1 2 3 4 6 8 100)))

;;(ocml-eval-fun-term 'standard-deviation '('(#_7-Meters #_17-Meters #_107-Meters #_70-Meters #_1000-Meters #_1-Kilometer)))    

;;(ocml-eval-fun-term 'range '('(1 2 3 4 6 8 100)))

;;(ocml-eval-fun-term 'range '('(#_7-Meters #_17-Meters #_107-Meters #_70-Meters #_1000-Meters #_1-Kilometer)))    


;;(ocml-eval-fun-term 'mode '('(1 2 3 4 6 8 100)))

;;(ocml-eval-fun-term 'mode '('(#_7-Meters #_17-Meters #_107-Meters #_70-Meters #_1000-Meters #_1-Kilometer)))    

