;;; Mode: Lisp; Package: ocml

;; International System of Units Ontology
;; (Imports Standard-Units, Standard-Prefixes, Standard-Dimensions)
;;
;; This ontology captures the International System of Units based on Units of Measure
;; (see Physical Quantities). This brings support for the manipulation of quantities
;; expressed in the different units defined within SI (Systeme Internationale d'unites)
;;
;; Informed by EngMath: T. R. Gruber and G. R. Olsen. An ontology for
;; engineering mathematics.  In J. Doyle, P. To rasso, and
;; E. Sandewall, editors, Fourth International Conference on
;; Principles of Knowledge Representation and Reasoning, pages
;; 258-269, Bonn, Germany, 1994. Morgan Kaufmann.
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 1.0

(in-package "OCML")

(in-ontology international-system-units)

;;
;; Define the System Of Units

(def-instance #_InternationalSystemOfUnits #_phys-q:SystemOfUnits
  "International System Of Units defined in terms of its base units (the normative ones) plus three personal additions to support Currencies and Amount of Information."
  ((#_phys-q:hasBaseUnit #_Second
                         #_Meter
                         #_Kilogram
                         #_Ampere
                         #_Kelvin
                         #_Mole
                         #_Candela
                         #_Euro         
                         #_Byte)))
                         

;; Base Units in the Systeme Internationale

(def-instance #_Second #_phys-q:UnitOfMeasure
  "The SI standard unit of time."
  ((#_phys-q:hasAbbreviation "s")
   (#_phys-q:hasConversionRate #_phys-q:IdentityConversionRate)
   (#_phys-q:hasDimension #_si-dim:TimeDimension))
)

(def-instance #_Meter #_phys-q:UnitOfMeasure
  "The SI standard unit of length."
  ((#_phys-q:hasAbbreviation "m")
   (#_phys-q:hasConversionRate #_phys-q:IdentityConversionRate)
   (#_phys-q:hasDimension #_si-dim:LengthDimension))
)

(def-instance #_Kilogram #_phys-q:UnitOfMeasure
  "The SI standard unit of mass."
  ((#_phys-q:hasAbbreviation "kg")
   (#_phys-q:hasConversionRate #_phys-q:IdentityConversionRate)
   (#_phys-q:hasDimension #_si-dim:MassDimension))
)

(def-instance #_Ampere #_phys-q:UnitOfMeasure
  "The SI standard unit of electric current."
  ((#_phys-q:hasAbbreviation "A")
   (#_phys-q:hasConversionRate #_phys-q:IdentityConversionRate)
   (#_phys-q:hasDimension #_si-dim:ElectricCurrentDimension))
)

(def-instance #_Kelvin #_phys-q:UnitOfMeasure
  "The SI standard unit of thermodynamic temperature."
  ((#_phys-q:hasAbbreviation "K")
   (#_phys-q:hasConversionRate #_phys-q:IdentityConversionRate)
   (#_phys-q:hasDimension #_si-dim:ThermodynamicTemperatureDimension))
)

(def-instance #_Mole #_phys-q:UnitOfMeasure
  "The SI standard unit of amount of substance."
  ((#_phys-q:hasAbbreviation "mol")
   (#_phys-q:hasConversionRate #_phys-q:IdentityConversionRate)
   (#_phys-q:hasDimension #_si-dim:AmountOfSubstanceDimension))
)

(def-instance #_Candela #_phys-q:UnitOfMeasure
  "The SI standard unit of luminous intensity."
  ((#_phys-q:hasAbbreviation "cd")
   (#_phys-q:hasConversionRate #_phys-q:IdentityConversionRate)
   (#_phys-q:hasDimension #_si-dim:LuminousIntesityDimension))
)

;; Personal additions
(def-instance #_Byte #_phys-q:UnitOfMeasure
  "The SI standard unit of binary information."
  ((#_phys-q:hasAbbreviation "B")
   (#_phys-q:hasConversionRate #_phys-q:IdentityConversionRate)
   (#_phys-q:hasDimension #_si-dim:AmountOfInformationDimension))
)

(def-instance #_Euro #_phys-q:UnitOfMeasure
  "The SI standard unit of Currency :)."
  ((#_phys-q:hasAbbreviation "Eur")
   (#_phys-q:hasConversionRate #_phys-q:IdentityConversionRate)
   (#_phys-q:hasDimension #_si-dim:CurrencyDimension))
)

(def-instance #_Baud #_phys-q:UnitOfMeasure
  "The SI standard unit of Signal Transmission Rate."
  ((#_phys-q:hasAbbreviation "Bd")
   (#_phys-q:hasConversionRate #_phys-q:IdentityConversionRate)
   (#_phys-q:hasDimension #_si-dim:SignalTransmissionRateDimension))
)

;;
;; Scaled Base Units
;;

;; TIME
(def-instance #_Microsecond #_phys-q:UnitOfMeasure
  "Unit of time."
  ((#_phys-q:hasAbbreviation "us")
   (#_phys-q:hasConversionRate #_MicrosecondToSecond)
   (#_phys-q:hasDimension #_si-dim:TimeDimension))
)

(def-instance #_Millisecond #_phys-q:UnitOfMeasure
  "Unit of time."
  ((#_phys-q:hasAbbreviation "ms")
   (#_phys-q:hasConversionRate #_MillisecondToSecond)
   (#_phys-q:hasDimension #_si-dim:TimeDimension))
)

(def-instance #_Minute #_phys-q:UnitOfMeasure
  "Unit of time."
  ((#_phys-q:hasAbbreviation "min")
   (#_phys-q:hasConversionRate #_MinuteToSecond)
   (#_phys-q:hasDimension #_si-dim:TimeDimension))
)

(def-instance #_Minute #_phys-q:UnitOfMeasure
  "Unit of time."
  ((#_phys-q:hasAbbreviation "min")
   (#_phys-q:hasConversionRate #_MinuteToSecond)
   (#_phys-q:hasDimension #_si-dim:TimeDimension))
)

(def-instance #_Hour #_phys-q:UnitOfMeasure
  "Unit of time."
  ((#_phys-q:hasAbbreviation "h")
   (#_phys-q:hasConversionRate #_HourToSecond)
   (#_phys-q:hasDimension #_si-dim:TimeDimension))
)

(def-instance #_Day #_phys-q:UnitOfMeasure
  "Unit of time."
  ((#_phys-q:hasAbbreviation "d")
   (#_phys-q:hasConversionRate #_DayToSecond)
   (#_phys-q:hasDimension #_si-dim:TimeDimension))
)

(def-instance #_Week #_phys-q:UnitOfMeasure
  "Unit of time."
  ((#_phys-q:hasAbbreviation "week")
   (#_phys-q:hasConversionRate #_WeekToSecond)
   (#_phys-q:hasDimension #_si-dim:TimeDimension))
)

(def-instance #_Month #_phys-q:UnitOfMeasure
  "Unit of time."
  ((#_phys-q:hasAbbreviation "month")
   (#_phys-q:hasConversionRate #_MonthToSecond)
   (#_phys-q:hasDimension #_si-dim:TimeDimension))
)

(def-instance #_Year #_phys-q:UnitOfMeasure
  "Unit of time."
  ((#_phys-q:hasAbbreviation "year")
   (#_phys-q:hasConversionRate #_YearToSecond)
   (#_phys-q:hasDimension #_si-dim:TimeDimension))
)

;; LENGTH

(def-instance #_Kilometer #_phys-q:UnitOfMeasure
  "Unit of length."
  ((#_phys-q:hasAbbreviation "Km")
   (#_phys-q:hasConversionRate #_si-prefixes:Kilo)
   (#_phys-q:hasDimension #_si-dim:LengthDimension))
)

(def-instance #_Millimeter #_phys-q:UnitOfMeasure
  "Unit of length."
  ((#_phys-q:hasAbbreviation "mm")
   (#_phys-q:hasConversionRate #_si-prefixes:Milli)
   (#_phys-q:hasDimension #_si-dim:LengthDimension))
)

(def-instance #_Centimeter #_phys-q:UnitOfMeasure
  "Unit of length."
  ((#_phys-q:hasAbbreviation "cm")
   (#_phys-q:hasConversionRate #_si-prefixes:Centi)
   (#_phys-q:hasDimension #_si-dim:LengthDimension))
)

(def-instance #_Decimeter #_phys-q:UnitOfMeasure
  "Unit of length."
  ((#_phys-q:hasAbbreviation "dm")
   (#_phys-q:hasConversionRate #_si-prefixes:Deci)
   (#_phys-q:hasDimension #_si-dim:LengthDimension))
)

(def-instance #_Mile #_phys-q:UnitOfMeasure
  "Unit of length."
  ((#_phys-q:hasAbbreviation "mi")
   (#_phys-q:hasConversionRate #_MileToMeter)
   (#_phys-q:hasDimension #_si-dim:LengthDimension))
)

(def-instance #_Yard #_phys-q:UnitOfMeasure
  "Unit of length."
  ((#_phys-q:hasAbbreviation "yd")
   (#_phys-q:hasConversionRate #_YardToMeter)
   (#_phys-q:hasDimension #_si-dim:LengthDimension))
)

(def-instance #_Inch #_phys-q:UnitOfMeasure
  "Unit of length."
  ((#_phys-q:hasAbbreviation "in")
   (#_phys-q:hasConversionRate #_InchToMeter)
   (#_phys-q:hasDimension #_si-dim:LengthDimension))
)

(def-instance #_Foot #_phys-q:UnitOfMeasure
  "Unit of length."
  ((#_phys-q:hasAbbreviation "ft")
   (#_phys-q:hasConversionRate #_FootToMeter)
   (#_phys-q:hasDimension #_si-dim:LengthDimension))
)

;; Amount of Information
(def-instance #_Bit #_phys-q:UnitOfMeasure
  "Unit of Amount of Information."
  ((#_phys-q:hasAbbreviation "b")
   (#_phys-q:hasConversionRate #_BitToByte)
   (#_phys-q:hasDimension #_si-dim:AmountOfInformationDimension))
)

(def-instance #_Kilobyte #_phys-q:UnitOfMeasure
  "Unit of Amount of Information."
  ((#_phys-q:hasAbbreviation "KB")
   (#_phys-q:hasConversionRate #_si-prefixes:Kibi)
   (#_phys-q:hasDimension #_si-dim:AmountOfInformationDimension))
)

(def-instance #_Megabyte #_phys-q:UnitOfMeasure
  "Unit of Amount of Information."
  ((#_phys-q:hasAbbreviation "MB")
   (#_phys-q:hasConversionRate #_si-prefixes:Mebi)
   (#_phys-q:hasDimension #_si-dim:AmountOfInformationDimension))
)

(def-instance #_Gigabyte #_phys-q:UnitOfMeasure
  "Unit of Amount of Information."
  ((#_phys-q:hasAbbreviation "GB")
   (#_phys-q:hasConversionRate #_si-prefixes:Gibi)
   (#_phys-q:hasDimension #_si-dim:AmountOfInformationDimension))
)

(def-instance #_Terabyte #_phys-q:UnitOfMeasure
  "Unit of Amount of Information."
  ((#_phys-q:hasAbbreviation "TB")
   (#_phys-q:hasConversionRate #_si-prefixes:Tebi)
   (#_phys-q:hasDimension #_si-dim:AmountOfInformationDimension))
)


;; Mass
(def-instance #_Gram #_phys-q:UnitOfMeasure
  "Unit of Mass."
  ((#_phys-q:hasAbbreviation "g")
   (#_phys-q:hasConversionRate #_si-prefixes:Milli)
   (#_phys-q:hasDimension #_si-dim:Mass))
)

(def-instance #_Milligram #_phys-q:UnitOfMeasure
  "Unit of Mass."
  ((#_phys-q:hasAbbreviation "mg")
   (#_phys-q:hasConversionRate #_si-prefixes:Micro)
   (#_phys-q:hasDimension #_si-dim:Mass))
)

(def-instance #_Quintal #_phys-q:UnitOfMeasure
  "Unit of Mass."
  ((#_phys-q:hasAbbreviation "q")
   (#_phys-q:hasConversionRate #_si-prefixes:Hecto)
   (#_phys-q:hasDimension #_si-dim:Mass))
)

(def-instance #_Tonne #_phys-q:UnitOfMeasure
  "Unit of Mass."
  ((#_phys-q:hasAbbreviation "t")
   (#_phys-q:hasConversionRate #_si-prefixes:Kilo)
   (#_phys-q:hasDimension #_si-dim:Mass))
)

(def-instance #_OunceAvoirdupois #_phys-q:UnitOfMeasure
  "Unit of Mass."
  ((#_phys-q:hasAbbreviation "oz av")
   (#_phys-q:hasConversionRate #_OunceAvoirdupoisToKilogram)
   (#_phys-q:hasDimension #_si-dim:Mass))
)

(def-instance #_PoundAvoirdupois #_phys-q:UnitOfMeasure
  "Unit of Mass."
  ((#_phys-q:hasAbbreviation "lb av")
   (#_phys-q:hasConversionRate #_PoundAvoirdupoisToKilogram)
   (#_phys-q:hasDimension #_si-dim:Mass))
)

(def-instance #_Stone #_phys-q:UnitOfMeasure
  "Unit of Mass."
  ((#_phys-q:hasAbbreviation "st")
   (#_phys-q:hasConversionRate #_StoneToKilogram)
   (#_phys-q:hasDimension #_si-dim:Mass))
)

;; TODO: Support Currency Conversion using the latest data from the Web

;; Currency
;;(def-instance #_UnitedKingdomPounds #_phys-q:UnitOfMeasure
;;  "Unit of Mass."
;;  ((#_phys-q:hasAbbreviation "GBP")
;;   (#_phys-q:hasConversionRate #_StoneToKilogram)
;;   (#_phys-q:hasDimension #_si-dim:Mass))
;;)

;;
;; Special Conversion Rates
;;

;; Mass
(def-instance #_PoundAvoirdupoisToKilogram #_phys-q:ConversionRate
  "Conversion rate for Pounds av. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 0.45359237)))

(def-instance #_OunceAvoirdupoisToKilogram #_phys-q:ConversionRate
  "Conversion rate for Ounce av. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 0.028349523125)))

(def-instance #_StoneToKilogram #_phys-q:ConversionRate
  "Conversion rate for Stones. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 6.35029318)))

;; Amount of Information
(def-instance #_BitToByte #_phys-q:ConversionRate
  "Conversion rate for Bits. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate (expt 2 -3))))

;; Time
(def-instance #_MillisecondToSecond #_phys-q:ConversionRate
  "Conversion Rate for Millisecond. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 0.001)))

(def-instance #_MicrosecondToSecond #_phys-q:ConversionRate
  "Conversion Rate for Microseconds. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 0.000001)))

(def-instance #_MinuteToSecond #_phys-q:ConversionRate
  "Conversion Rate for Minutes. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 60)))

(def-instance #_HourToSecond #_phys-q:ConversionRate
  "Conversion Rate for Hours. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 3600)))

(def-instance #_DayToSecond #_phys-q:ConversionRate
  "Conversion Rate for Days. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 86400)))

(def-instance #_WeekToSecond #_phys-q:ConversionRate
  "Conversion Rate for Weeks. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 604800)))

;; Month is special for it depends on the actual month (and year for February)
;; TODO: FIX THIS
(def-instance #_MonthToSecond #_phys-q:ConversionRate
  "Conversion Rate for Month. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 2592000)))

;; Year is special for it depends on the actual year
;; TODO: Fix this
(def-instance #_YearToSecond #_phys-q:ConversionRate
  "Conversion Rate for Year. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 31536000)))


;; Length
(def-instance #_MileToMeter #_phys-q:ConversionRate
  "Conversion Rate for Miles. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 1609.344)))

(def-instance #_FootToMeter #_phys-q:ConversionRate
  "Conversion Rate for Feet. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 0.3048)))

(def-instance #_InchToMeter #_phys-q:ConversionRate
  "Conversion Rate for Inches. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 0.0254)))

(def-instance #_YardToMeter #_phys-q:ConversionRate
  "Conversion Rate for Yard. Named differently to avoid clash with the unit name."

  ((#_phys-q:hasRate 0.9144)))

