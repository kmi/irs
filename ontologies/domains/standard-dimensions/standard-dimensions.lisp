;;; Mode: Lisp; Package: ocml

;; Standard Dimensions Ontology
;;
;; Ontology that includes the standard dimensions that are part of the International
;; System of Units (see Internation System of Units Ontology)
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 1.0
;;

(in-package "OCML")

(in-ontology standard-dimensions)

(def-instance #_LengthDimension #_phys-q:PhysicalDimension
  "The fundamental dimension of length, as defined by the SI standard.")

(def-instance #_MassDimension #_phys-q:PhysicalDimension
  "The fundamental dimension of mass, as defined by the SI standard.")

(def-instance #_TimeDimension #_phys-q:PhysicalDimension
  "The fundamental dimension of physical, continuous time, 
as defined by the SI standard.")

(def-instance #_ElectricalCurrentDimension #_phys-q:PhysicalDimension
  "The fundamental dimension of electrical current, as defined by the
SI standard.")

(def-instance #_ThermodynamicTemperatureDimension #_phys-q:PhysicalDimension
  "The fundamental dimension of temperature, as defined by the SI standard.")

(def-instance #_AmountOfSubstanceDimension #_phys-q:PhysicalDimension
  "The fundamental dimension of amount of substance, as defined by the
SI standard.")

(def-instance #_LuminousIntensityDimension #_phys-q:PhysicalDimension
  "The fundamental dimension of luminous-intensity, as defined by the
SI standard.")

(def-instance #_CurrencyDimension #_phys-q:PhysicalDimension
  "The fundamental dimension of currency or money.
CURRENCY-DIMENSION is to currencies as US-dollar's and ECU's as the
LENGTH-DIMENSION is to units of length such as meters.")

(def-instance #_AmountOfInformationDimension #_phys-q:PhysicalDimension
  "The fundamental dimension of amount of information. Amount of Information is to amounts like KB or MB, as the length dimension is to units of length such as meters.")

(def-instance #_SignalTransmissionRateDimension #_phys-q:PhysicalDimension
  "The fundamental dimension of signal transmission rate. Signal Transmission Rate is to amounts like Bd, as the length dimension is to units of length such as meters.")


;; 
;; Derived Units 
;;

(def-instance #_ForceDimension #_phys-q:PhysicalDimension 

  "The physical dimension of force is defined as mass times length
over time squared.  In some systems FORCE-DIMENSION is fundamental and
MASS-DIMENSION is a derived dimension.  This theory goes with the SI
standard, but we include the definition of force as a non-fundamental
built-in dimension.")

(def-instance #_EnergyDimension #_phys-q:PhysicalDimension 

  "The physical dimension of energy is defined as mass times length squared
over time squared."
)

(def-instance #_AreaDimension #_phys-q:PhysicalDimension
  "The physical dimension of an area is defined as
length dimension squared."
)
  
(def-instance #_VolumeDimension #_phys-q:PhysicalDimension
  "the physical dimension of volume length * length * length"
)

(def-instance #_SpeedDimension #_phys-q:PhysicalDimension
  "the physical dimension of speed length/time"
)

(def-instance #_AccelerationDimension #_phys-q:PhysicalDimension
  "the physical dimension of acceleration, length/time^2"
)

(def-instance #_FrequencyDimension #_phys-q:PhysicalDimension
  "the physical dimension of frequency"
)

(def-instance #_ElectricPotentialDifferenceDimension #_phys-q:PhysicalDimension
  "the physical dimension of electric potential difference"
)

(def-instance #_PressureDimension #_phys-q:PhysicalDimension
  "the physical dimension of pressure is defined as 
force over area"
)
 
(def-instance #_WorkDimension #_phys-q:PhysicalDimension)



