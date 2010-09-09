;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(def-ontology time-ontology
  :includes (units-manipulation xsd-types)
  :type :domain 
  ;;:namespace-uri "http://www.ip-super.org/ontologies/time-ontology/20080612#"
  :namespace-uri "http://ip-super.org/ontologies/process/time/v1.2.0#"
  :namespaces (("TIME" time-ontology)
               ("UNITS" units-manipulation)
               ("SI" international-system-units)
               ("PHYS-Q" physical-quantities)
               ("XSD" xsd-types))
  :files ("time-ontology" 
          "new" )
  :author "carlos" :allowed-editors ("nil"))