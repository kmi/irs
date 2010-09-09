;;; Copyright © 2008 The Open University

;;; ATTENTION!  DO NOT REFORMAT THE STRINGS IN HERE!
;;;
;;; They are sensitive to white space changes which are a pain to
;;; debug.  You break it, you fix it.

(in-package #:irs.tests)

(def-suite api-rest-suite
  :description "Tests for the REST API.")

(in-suite api-rest-suite)

(defparameter query1 (ocml:as-ocml '(setofall ?x (member ?x (a b)))))

(defparameter query2 (ocml:as-ocml '(setofall (?x ?y)
                                     (and (member ?x (a b)) (member ?y (1 2))))))

(test add-math-test
  (let* ((a (random 1000))
         (b (random 1000))
         (sum (utilities:http-request (the-irs-uri (format nil "/api-rest/achieve-goal?ontology=MATH-ONTOLOGY&goal=math:add-goal&math:hasA=~A&HAS-B=~A" a b)))))
    (is (= (+ a b) (parse-integer (read-from-string sum))))))

(test query-test
      (is (string= (utilities:http-request
                    (format nil "http://localhost:8080/api-rest/query?o=BASE-ONTOLOGY&q=~A"
                            (hunchentoot:url-encode (format nil "~A" query1))))
                   "(A B)"))

      (is (string= (utilities:http-request
                    (format nil "http://localhost:8080/api-rest/query?o=BASE-ONTOLOGY&q=~A"
                            (hunchentoot:url-encode (format nil "~A" query2))))
                   "((A 1) (A 2) (B 1) (B 2))"))

      (is (string= (utilities:http-request
                    (format nil "http://localhost:8080/api-rest/query?f=xml&o=BASE-ONTOLOGY&q=~A"
                            (hunchentoot:url-encode (format nil "~A" query1))))
                   "<?xml version='1.0' encoding='UTF-8'?>
<tns:ResultSet xmlns:tns='http://kmi.open.ac.uk/irs/query/20080215/result'
    xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'
    xsi:schemaLocation='http://kmi.open.ac.uk/irs/query/20080215/result QueryResultSet.xsd'>
    <tns:Result>
        <tns:Binding Variable='?X' Value='A'/>
    </tns:Result>
    <tns:Result>
        <tns:Binding Variable='?X' Value='B'/>
    </tns:Result>
</tns:ResultSet>"))

      (is (string= (utilities:http-request
                    (format nil "http://localhost:8080/api-rest/query?f=xml&o=BASE-ONTOLOGY&q=~A"
                            (hunchentoot:url-encode (format nil "~A" query2))))
                   "<?xml version='1.0' encoding='UTF-8'?>
<tns:ResultSet xmlns:tns='http://kmi.open.ac.uk/irs/query/20080215/result'
    xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'
    xsi:schemaLocation='http://kmi.open.ac.uk/irs/query/20080215/result QueryResultSet.xsd'>
    <tns:Result>
        <tns:Binding Variable='?X' Value='A'/>
        <tns:Binding Variable='?Y' Value='1'/>
    </tns:Result>
    <tns:Result>
        <tns:Binding Variable='?X' Value='A'/>
        <tns:Binding Variable='?Y' Value='2'/>
    </tns:Result>
    <tns:Result>
        <tns:Binding Variable='?X' Value='B'/>
        <tns:Binding Variable='?Y' Value='1'/>
    </tns:Result>
    <tns:Result>
        <tns:Binding Variable='?X' Value='B'/>
        <tns:Binding Variable='?Y' Value='2'/>
    </tns:Result>
</tns:ResultSet>")))

(test knowledge-base-api-test
  (finishes
    (parse-xml
     (let ((iri (hunchentoot:url-encode "http://kmi.open.ac.uk/ontologies/international-system-units#Second")))
       (utilities:http-request (the-irs-uri "/api-rest/retrieve-instance?iri=~A" iri)))))
  (finishes (parse-xml (utilities:http-request (the-irs-uri "/api-rest/get-ontology?ontology=BASE-ONTOLOGY"))))
  (finishes (utilities:http-request (the-irs-uri "/api-rest/get-iri-from-namespace?namespace=~A" (hunchentoot:url-encode "http://kmi.open.ac.uk/ontologies/international-system-units#"))))
  (is (string=
       (utilities:http-request
        (the-irs-uri "/api-rest/get-ontology-iri-from-namespace?namespace=~A"
                     (hunchentoot:url-encode "http://kmi.open.ac.uk/ontologies/international-system-units#")))
       "http://kmi.open.ac.uk/ontologies/international-system-units#INTERNATIONAL-SYSTEM-UNITS")))

;;; Test that the instance upload works.  We change the abbreviation
;;; used for seconds in the SI units ontology.

(defparameter *test-send-instance*
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<tns:OcmlInstance xmlns:tns=\"http://kmi.open.ac.uk/ocml/xsd/20080215/OcmlModel\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><tns:Iri>http://kmi.open.ac.uk/ontologies/international-system-units#Second</tns:Iri><tns:ClassIri>http://kmi.open.ac.uk/ontologies/physical-quantities#UnitOfMeasure</tns:ClassIri><tns:Documentation>The SI standard unit of time.</tns:Documentation><tns:SlotValue><tns:Iri>http://kmi.open.ac.uk/ontologies/physical-quantities#hasAbbreviation</tns:Iri><tns:SlotTypeIri>http://www.kmi.open.ac.uk/projects/ocml/namespaces/assumed/BASE-ONTOLOGY#STRING</tns:SlotTypeIri><tns:Value tns:ltype=\"string\">a</tns:Value></tns:SlotValue><tns:SlotValue><tns:Iri>http://kmi.open.ac.uk/ontologies/physical-quantities#hasConversionRate</tns:Iri><tns:SlotTypeIri>http://kmi.open.ac.uk/ontologies/physical-quantities#ConversionRate</tns:SlotTypeIri><tns:Value tns:ltype=\"symbol\">http://kmi.open.ac.uk/ontologies/physical-quantities#IdentityConversionRate</tns:Value></tns:SlotValue><tns:SlotValue><tns:Iri>http://kmi.open.ac.uk/ontologies/physical-quantities#hasDimension</tns:Iri><tns:SlotTypeIri>http://kmi.open.ac.uk/ontologies/physical-quantities#PhysicalDimension</tns:SlotTypeIri><tns:Value tns:ltype=\"symbol\">http://kmi.open.ac.uk/ontologies/standard-dimensions#TimeDimension</tns:Value></tns:SlotValue></tns:OcmlInstance>")

(defparameter *test-send-instance-integer-value*
"<?xml version=\'1.0\' encoding=\'UTF-8\'?>

<tns:OcmlInstance xmlns:tns=\'http://www.kmi.open.ac.uk/ocml/xsd/20080215/OcmlModel\'
		  xmlns:xsi=\'http://www.w3.org/2001/XMLSchema-instance\'>
  <tns:Iri>http://www.kmi.open.ac.uk/projects/ocml/namespaces/assumed/DINO#ALBERTOSAURUS</tns:Iri>
  <tns:ClassIri>http://www.kmi.open.ac.uk/projects/ocml/namespaces/assumed/DINO#THEROPODA</tns:ClassIri>
  <tns:SlotValue>
    <tns:Iri>http://www.kmi.open.ac.uk/projects/ocml/namespaces/assumed/DINO#MASS</tns:Iri>
    <tns:SlotTypeIri>http://www.kmi.open.ac.uk/projects/ocml/namespaces/assumed/BASE-ONTOLOGY#INTEGER</tns:SlotTypeIri>
    <tns:Value tns:ltype=\"number\">5</tns:Value>
  </tns:SlotValue>
</tns:OcmlInstance>")

(test send-instance-test
      (is (= 200
             (multiple-value-bind (content response-code)
                 (utilities:http-request
                  (the-irs-uri "/api-rest/send-instance")
                  :method :post :parameters
                  `(("instance" . ,*test-send-instance*)))
               (declare (ignore content))
               response-code)))
      (is (equal '("a")
                 (ocml::with-ontology ('ocml::international-system-units)
               (ocml::findall '?a
                              '(ocml::has-slot-value
                                'OCML::|http://kmi.open.ac.uk/ontologies/international-system-units#Second|
                                'OCML::|http://kmi.open.ac.uk/ontologies/physical-quantities#hasAbbreviation| ?a)))))
      (is (= 200
             (multiple-value-bind (content response-code)
                 (utilities:http-request
                  (the-irs-uri "/api-rest/send-instance")
                  :method :post :parameters
                  `(("instance" . ,*test-send-instance-integer-value*)))
               (declare (ignore content))
               response-code)))
      (is (equal
           '(5) (ocml::with-ontology ('ocml::dino)
                     (ocml::findall '?a
                                    '(ocml::has-slot-value
                                      'OCML::ALBERTOSAURUS 'OCML::MASS ?a))))))

;;; {{{ Test send-ontology service

(defparameter *test-send-ontology*
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<tns:OcmlOntology xmlns:tns=\"http://kmi.open.ac.uk/ocml/xsd/20080215/OcmlModel\"
                  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
                  xsi:schemaLocation=\"http://kmi.open.ac.uk/ocml/xsd/20080215/OcmlModel OcmlModel.xsd\">
    <tns:Iri>http://www.irs.net/testo#TESTO</tns:Iri>
    <tns:Import><tns:Iri>http://ip-super.org/ontologies/process/time/v1.2.0#TIME-ONTOLOGY</tns:Iri></tns:Import>
    <tns:Import><tns:Iri>http://kmi.open.ac.uk/ontologies/physical-quantities#PHYSICAL-QUANTITIES</tns:Iri></tns:Import>
</tns:OcmlOntology>")

(defparameter *test-send-ontology2*
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<tns:OcmlOntology xmlns:tns=\"http://kmi.open.ac.uk/ocml/xsd/20080215/OcmlModel\"
                  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
                  xsi:schemaLocation=\"http://kmi.open.ac.uk/ocml/xsd/20080215/OcmlModel OcmlModel.xsd\">
    <tns:Iri>http://www.irs.net/vest#VEST</tns:Iri>
    <tns:Import><tns:Iri>http://ip-super.org/ontologies/process/time/v1.2.0#TIME-ONTOLOGY</tns:Iri></tns:Import>
    <tns:Import><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#COBRA</tns:Iri></tns:Import>
    <tns:Import><tns:Iri>http://kmi.open.ac.uk/ontologies/physical-quantities#PHYSICAL-QUANTITIES</tns:Iri></tns:Import>
<tns:OcmlClass><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#BirthdayEvent</tns:Iri><tns:SuperClass>http://ip-super.org/ontologies/process/cobra/v1.1.1#MonitoringEvent</tns:SuperClass><tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#leadsToState</tns:Iri><tns:SlotTypeIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#BusinessActivityStateType</tns:SlotTypeIri><tns:MinCardinality>1</tns:MinCardinality><tns:MaxCardinality>1</tns:MaxCardinality></tns:Slot><tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#canOccurInState</tns:Iri><tns:SlotTypeIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#BusinessActivityStateType</tns:SlotTypeIri><tns:MinCardinality>1</tns:MinCardinality></tns:Slot><tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#generatedBy</tns:Iri><tns:SlotTypeIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#Agent</tns:SlotTypeIri><tns:MinCardinality>1</tns:MinCardinality><tns:MaxCardinality>1</tns:MaxCardinality></tns:Slot><tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#concernsActivityInstance</tns:Iri><tns:SlotTypeIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#ActivityInstance</tns:SlotTypeIri><tns:MaxCardinality>1</tns:MaxCardinality></tns:Slot><tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#concernsProcessInstance</tns:Iri><tns:SlotTypeIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#ProcessInstance</tns:SlotTypeIri><tns:MinCardinality>1</tns:MinCardinality><tns:MaxCardinality>1</tns:MaxCardinality></tns:Slot>

<tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#birthdayDay</tns:Iri><tns:SlotTypeIri>http://www.kmi.open.ac.uk/projects/ocml/namespaces/assumed/BASE-ONTOLOGY#STRING</tns:SlotTypeIri><tns:MinCardinality>1</tns:MinCardinality><tns:MaxCardinality>1</tns:MaxCardinality></tns:Slot>

<tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#christmasDay</tns:Iri>
<tns:SlotTypeIri>http://www.kmi.open.ac.uk/projects/ocml/namespaces/assumed/BASE-ONTOLOGY#INTEGER</tns:SlotTypeIri>
<tns:FixedValue tns:ltype=\"number\">25</tns:FixedValue>
<tns:FixedValue tns:ltype=\"number\">12</tns:FixedValue>
<tns:FixedValue tns:ltype=\"number\">1</tns:FixedValue>
<tns:MinCardinality>1</tns:MinCardinality>
<tns:MaxCardinality>1</tns:MaxCardinality>
</tns:Slot>

<tns:Documentation/></tns:OcmlClass>

<tns:OcmlInstance>
  <tns:Iri>http://www.irs.net/vest#JesusBirthday</tns:Iri>
  <tns:ClassIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#BirthdayEvent</tns:ClassIri>
  <tns:SlotValue>
    <tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#birthdayDay</tns:Iri>
    <tns:Value xsi:type='xs:string' xmlns:xs='http://www.w3.org/2001/XMLSchema'  tns:ltype=\"string\">Christmas day!</tns:Value>
  </tns:SlotValue>
</tns:OcmlInstance>

</tns:OcmlOntology>")

(test send-ontology-test
  (is (= 200
         (multiple-value-bind (content response-code)
             (utilities:http-request
              (the-irs-uri "/api-rest/send-ontology")
              :method :post :parameters
              `(("ontology" . ,*test-send-ontology*)))
           (declare (ignore content))
           response-code)))
  (is (= 200
         (multiple-value-bind (content response-code)
             (utilities:http-request
              (the-irs-uri "/api-rest/send-ontology")
              :method :post :parameters
              `(("ontology" . ,*test-send-ontology2*)))
           (declare (ignore content))
           response-code))))

;;; }}}

;;; {{{ send-class
(defparameter *test-send-class*
  "<tns:OcmlClass
  xmlns:tns=\"http://kmi.open.ac.uk/ocml/xsd/20080215/OcmlModel\"
  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
  xsi:schemaLocation=\"http://kmi.open.ac.uk/ocml/xsd/20080215/OcmlModel
  OcmlModel.xsd\"><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#BirthdayEvent</tns:Iri><tns:SuperClass>http://ip-super.org/ontologies/process/cobra/v1.1.1#MonitoringEvent</tns:SuperClass><tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#leadsToState</tns:Iri><tns:SlotTypeIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#BusinessActivityStateType</tns:SlotTypeIri><tns:MinCardinality>1</tns:MinCardinality><tns:MaxCardinality>1</tns:MaxCardinality></tns:Slot><tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#canOccurInState</tns:Iri><tns:SlotTypeIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#BusinessActivityStateType</tns:SlotTypeIri><tns:MinCardinality>1</tns:MinCardinality></tns:Slot><tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#generatedBy</tns:Iri><tns:SlotTypeIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#Agent</tns:SlotTypeIri><tns:MinCardinality>1</tns:MinCardinality><tns:MaxCardinality>1</tns:MaxCardinality></tns:Slot><tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#concernsActivityInstance</tns:Iri><tns:SlotTypeIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#ActivityInstance</tns:SlotTypeIri><tns:MaxCardinality>1</tns:MaxCardinality></tns:Slot><tns:Slot><tns:Iri>http://ip-super.org/ontologies/process/cobra/v1.1.1#concernsProcessInstance</tns:Iri><tns:SlotTypeIri>http://ip-super.org/ontologies/process/cobra/v1.1.1#ProcessInstance</tns:SlotTypeIri><tns:MinCardinality>1</tns:MinCardinality><tns:MaxCardinality>1</tns:MaxCardinality></tns:Slot><tns:Documentation/></tns:OcmlClass>")

(test send-class-test
  (is (= 200
         (multiple-value-bind (content response-code)
             (utilities:http-request
              (the-irs-uri "/api-rest/send-class")
              :method :post :parameters
              `(("ontology" . "COBRA")
                ("class" . ,*test-send-class*)))
           (declare (ignore content))
           response-code))))

;;; }}}

;;; {{{ Round trip tests for some ontologies
(test round-trip-ontology-test
  (is-true (roundtrip-ontology 'dino)))
;;; }}}
