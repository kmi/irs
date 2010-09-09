;;; Mode: Lisp; Package: ocml

;; Standard Prefixes Ontology
;;
;; Ontology that includes the standard prefixes that are part of the International
;; System of Units (see Internation System of Units Ontology) as well as some
;; additional ones used in Computer Science
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 1.0
;;
;; Included Typical Prefixes as a means to support calculating and transforming

(in-package "OCML")

(in-ontology standard-prefixes)

(def-class #_MetricPrefix (#_phys-q:ConversionRate)
  "These are the typical prefixes used in the metric system")

(def-class #_BinaryPrefix (#_phys-q:ConversionRate)
  "These are the typical prefixes specified in powers of 2 as typically used in computer science.")

;; Metrics Scale Prefix
(def-instance #_Yotta #_MetricPrefix
  "Scale Prefix for 1 septillion"

  ((#_phys-q:hasRate (expt 10 24))))

(def-instance #_Zetta #_MetricPrefix
  "Scale Prefix for 1 sextillion"

  ((#_phys-q:hasRate (expt 10 21))))

(def-instance #_Exa #_MetricPrefix
  "Scale Prefix for 1 quintillion"

  ((#_phys-q:hasRate (expt 10 18))))

(def-instance #_Peta #_MetricPrefix
  "Scale Prefix for 1 quadrillion"

  ((#_phys-q:hasRate (expt 10 15))))

(def-instance #_Tera #_MetricPrefix
  "Scale Prefix for 1 trillion"

  ((#_phys-q:hasRate (expt 10 12))))


(def-instance #_Giga #_MetricPrefix
  "Scale Prefix for 1 billion"

  ((#_phys-q:hasRate (expt 10 9))))


(def-instance #_Mega #_MetricPrefix
  "Scale Prefix for 1 million"

  ((#_phys-q:hasRate (expt 10 6))))


(def-instance #_Kilo #_MetricPrefix
  "Scale Prefix for 1 thousand"

  ((#_phys-q:hasRate (expt 10 3))))


(def-instance #_Hecto #_MetricPrefix
  "Scale Prefix for 1 hundred"

  ((#_phys-q:hasRate (expt 10 2))))


(def-instance #_Deka #_MetricPrefix
  "Scale Prefix for 1 ten"

  ((#_phys-q:hasRate 10)))


(def-instance #_Deci #_MetricPrefix
  "Scale Prefix for 1 tenth"

  ((#_phys-q:hasRate 0.1)))


(def-instance #_Centi #_MetricPrefix
  "Scale Prefix for 1 hundredth"

  ((#_phys-q:hasRate 0.01)))

(def-instance #_Milli #_MetricPrefix
  "Scale Prefix for 1 thousandth"

  ((#_phys-q:hasRate (expt 10 -3))))

(def-instance #_Micro #_MetricPrefix
  "Scale Prefix for 1 millionth"

  ((#_phys-q:hasRate (expt 10 -6))))


(def-instance #_Nano #_MetricPrefix
  "Scale Prefix for 1 billionth"

  ((#_phys-q:hasRate (expt 10 -9))))

(def-instance #_Pico #_MetricPrefix
  "Scale Prefix for 1 trillionth"

  ((#_phys-q:hasRate (expt 10 -12))))

(def-instance #_Femto #_MetricPrefix
  "Scale Prefix for 1 quadrillionth"

  ((#_phys-q:hasRate (expt 10 -15))))

(def-instance #_Atto #_MetricPrefix
  "Scale Prefix for 1 quintillionth"

  ((#_phys-q:hasRate (expt 10 -18))))

(def-instance #_Zepto #_MetricPrefix
  "Scale Prefix for 1 sextillionth"

  ((#_phys-q:hasRate (expt 10 -21))))


(def-instance #_Yocto #_MetricPrefix
  "Scale Prefix for 1 septillionth"

  ((#_phys-q:hasRate (expt 10 -24))))


;; Binary Scale Prefix
;; Despite the names, these correspond to the scales used for measuring binary information
;; These are the standard naming for prefixes that have to be distinguished from the ones
;; used in the metric system (e.g., kilo as 1000, or kilo as 1024)

(def-instance #_Kibi #_BinaryPrefix
  "Scale Prefix for Kilo as in Kilobytes"

  ((#_phys-q:hasRate (expt 2 10))))

(def-instance #_Mebi #_BinaryPrefix
  "Scale Prefix for Mega as in Megabytes"

  ((#_phys-q:hasRate (expt 2 20))))


(def-instance #_Gibi #_BinaryPrefix
  "Scale Prefix for Giga as in Gigabytes"

  ((#_phys-q:hasRate (expt 2 30))))


(def-instance #_Tebi #_BinaryPrefix
  "Scale Prefix for Tera as is Terabytes"

  ((#_phys-q:hasRate (expt 2 40))))


(def-instance #_Pebi #_BinaryPrefix
  "Scale Prefix for Peta as in Petabytes"

  ((#_phys-q:hasRate (expt 2 50))))


(def-instance #_Exbi #_BinaryPrefix
  "Scale Prefix for Exa as in Exabytes"

  ((#_phys-q:hasRate (expt 2 60))))




