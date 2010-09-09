;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology ontoweb-ontology)


;;;Automatically translated from RDF file #P"D:/users/jbd2/code/freaky/rdf-files/ontoweb-data/ontoweb-ontology-rc1.rdfs"
;;;at 20:27:57, on 27/10/2003

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#GENERICONTOLOGY)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_EXCEPTION_HANDLING (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_EXCEPTION_HANDLING property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_exception_handling")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWINSTANCESATTRIBUTES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWINSTANCESATTRIBUTES property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allowinstancesattributes")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#INPROCEEDINGS)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___ORGANISATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___ORGANISATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#organisation)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#description)
(Subpropertyof //www.ontoweb.org/extended\#description)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#EVENT)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#COMPANYSTAFF)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#AUTHOR (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#AUTHOR property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#author")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#EDUCATIONALRESSOURCE)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCLANGUAGE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCLANGUAGE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dclanguage")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#TRADITIONALLANGUAGE)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#RESEARCHAREA)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#LINGUISTICONTOLOGY)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#MISCRISKS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#MISCRISKS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#miscrisks")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#RESEARCHGROUP)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#MISC)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#IMPROVEECOMMERCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#IMPROVEECOMMERCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#improveecommerce")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#HOMEPAGE___PROJECT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#HOMEPAGE___PROJECT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#project)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#homepage)
(Subpropertyof //www.ontoweb.org/extended\#homepage)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#MASTERTHESIS)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCRIGHTS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCRIGHTS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dcrights")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#TITLE___PUBLICATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#TITLE___PUBLICATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#publication)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#title)
(Subpropertyof //www.ontoweb.org/extended\#title)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#APPLICATIONFOREDUCATION)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGYUSED (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGYUSED property
((Range //www.ontoweb.org/extended\#methodology)
(Domain //www.ontoweb.org/extended\#applicationforintelligentinformationintegration)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#methodologyused")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#PROJECTMANAGEMENTBOARD)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#URL (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#URL property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#url")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#OTHERTECHNICALRISKS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#OTHERTECHNICALRISKS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#othertechnicalrisks")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#APPLICATIONFORINTELLIGENTINFORMATIONINTEGRATION)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGY property
((Range //www.ontoweb.org/extended\#methodology)
(Domain //www.ontoweb.org/extended\#application)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#methodology")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NAME___EVENT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NAME___EVENT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#event)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#name)
(Subpropertyof //www.ontoweb.org/extended\#name)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NUMBEROFMETACLASSES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NUMBEROFMETACLASSES property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#numberofmetaclasses")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWSLOTDEFAULTVALUE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWSLOTDEFAULTVALUE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allowslotdefaultvalue")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DURATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DURATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#duration")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#LOCATION___EVENT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#LOCATION___EVENT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#event)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#location)
(Subpropertyof //www.ontoweb.org/extended\#location)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#PROVIDER (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#PROVIDER property
((Range //www.ontoweb.org/extended\#organisation)
(Domain //www.ontoweb.org/extended\#educationalressource)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#provider")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#HOMEPAGE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#HOMEPAGE property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#homepage")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#BRANCHINGFACTOR (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#BRANCHINGFACTOR property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#branchingfactor")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allows")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWCOMPLETE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWCOMPLETE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allowcomplete")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#SOURCEINFORMATIONAVAILABILITY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#SOURCEINFORMATIONAVAILABILITY property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#sourceinformationavailability")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#IMPORTS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#IMPORTS property
((Range //www.ontoweb.org/extended\#language)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#imports")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#APPLICATIONFORKNOWLEDGEMANAGEMENT)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#MANUAL)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DIFFICULTYLEVEL (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DIFFICULTYLEVEL property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#difficultylevel")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DATE_START_ (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DATE_START_ property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#event)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#date(start)")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#COPYRIGHT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#COPYRIGHT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#copyright")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCFORMAT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCFORMAT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dcformat")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#EDUCATIONALAIM (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#EDUCATIONALAIM property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#educationalaim")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#HEAD (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#HEAD property
((Range //www.ontoweb.org/extended\#person)
(Domain //www.ontoweb.org/extended\#project)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#head")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NAME___ORGANISATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NAME___ORGANISATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#organisation)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#name)
(Subpropertyof //www.ontoweb.org/extended\#name)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#WORKSHOP)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCSUBJECT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCSUBJECT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dcsubject")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGYFORREENGINEERING)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#SUCCESSSTORIES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#SUCCESSSTORIES property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#application)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#successstories")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_CONSTRAINT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_CONSTRAINT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_constraint")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#ORGANISATION)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCCONTRIBUTOR (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCCONTRIBUTOR property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dccontributor")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NUMBEROFCONCEPTS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NUMBEROFCONCEPTS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#numberofconcepts")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#CARRIEDOUTBY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#CARRIEDOUTBY property
((Range //www.ontoweb.org/extended\#organisation)
(Domain //www.ontoweb.org/extended\#project)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#carriedoutby")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#LANGUAGE)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#MAXIMUMARITY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#MAXIMUMARITY property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#maximumarity")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#LOCATION___ORGANISATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#LOCATION___ORGANISATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#organisation)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#location)
(Subpropertyof //www.ontoweb.org/extended\#location)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___EVENT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___EVENT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#event)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#description)
(Subpropertyof //www.ontoweb.org/extended\#description)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#MISCPUBLICATION)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCPUBLISHER (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCPUBLISHER property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dcpublisher")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#TOOL)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#URL___EDUCATIONALRESSOURCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#URL___EDUCATIONALRESSOURCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#url)
(Subpropertyof //www.ontoweb.org/extended\#url)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_PRODUTION_RULES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_PRODUTION_RULES property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_prodution_rules")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#CONSORTIUM)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ISWORKEDONBY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ISWORKEDONBY property
((Range //www.ontoweb.org/extended\#academicstaff)
(Domain //www.ontoweb.org/extended\#researcharea)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#isworkedonby")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#TITLE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#TITLE property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#title")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#MODELINGGUIDELINES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#MODELINGGUIDELINES property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#modelingguidelines")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGYFORONTOLOGYLEARNING)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#SUPPORTS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#SUPPORTS property
((Range //www.ontoweb.org/extended\#methodology)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#supports")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#STUDENT)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#IMPROVEB2B (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#IMPROVEB2B property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#improveb2b")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#IMPROVEKM (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#IMPROVEKM property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#improvekm")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#PROBLEMS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#PROBLEMS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#application)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#problems")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGYFROMTHESCRATCH)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ISSUPPORTEDBY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ISSUPPORTEDBY property
((Range //www.ontoweb.org/extended\#tool)
(Domain //www.ontoweb.org/extended\#methodology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#issupportedby")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCCREATOR (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCCREATOR property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dccreator")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#PRICINGPOLICY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#PRICINGPOLICY property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#pricingpolicy")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#ARTICLE)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#BUSINESSAREA)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#HOMEPAGE___ORGANISATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#HOMEPAGE___ORGANISATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#organisation)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#homepage)
(Subpropertyof //www.ontoweb.org/extended\#homepage)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#AUTOMATIONOFMANUALTASK (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#AUTOMATIONOFMANUALTASK property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#automationofmanualtask")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#PERSON)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#LOCATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#LOCATION property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#location")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#CATEGORY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#CATEGORY property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#category")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#OTHERCOMMERCIALRISKS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#OTHERCOMMERCIALRISKS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#othercommercialrisks")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#ACADEMICSTAFF)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#APPLICATION)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_LOCKING_LEVEL (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_LOCKING_LEVEL property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_locking_level")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___RESEARCHAREA (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___RESEARCHAREA property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#researcharea)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#description)
(Subpropertyof //www.ontoweb.org/extended\#description)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#CONFERENCE)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_ATTACHED_INFERENCE_ENGINE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_ATTACHED_INFERENCE_ENGINE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_attached_inference_engine")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_DEFAULT_VALUE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_DEFAULT_VALUE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_default_value")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#INITIALCONSTRUCTIONCOSTS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#INITIALCONSTRUCTIONCOSTS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#initialconstructioncosts")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#FAX (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#FAX property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#person)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#fax")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#LECTURE)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#AUTHOROF (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#AUTHOROF property
((Range //www.ontoweb.org/extended\#publication)
(Domain //www.ontoweb.org/extended\#person)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#authorof")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#BOOKLET)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#APPLICATIONFORINFORMATIONRETRIEVAL)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#ONTOLOGY)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#KNOWLEDGEAQUISITION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#KNOWLEDGEAQUISITION property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#knowledgeaquisition")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#SHORTDESCRIPTION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#SHORTDESCRIPTION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#news)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#shortdescription")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGYFORMERGE)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#POTENTIALCOSTCUTTING (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#POTENTIALCOSTCUTTING property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#potentialcostcutting")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#ENTERPRISE)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ISIMPORTEDFROM (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ISIMPORTEDFROM property
((Range //www.ontoweb.org/extended\#tool)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#isimportedfrom")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NOTE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NOTE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#publication)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#note")
))

;;there's a relation methodology of arity 2 so ocml
;;complains. Is this a bug in the ontoweb ontology?
;;;(def-class //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGY)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#FUNCTIONALITY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#FUNCTIONALITY property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#functionality")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DATE_END_ (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DATE_END_ property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#event)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#date(end)")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#PERFORMANCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#PERFORMANCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#performance")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#INFERENCEENGINE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#INFERENCEENGINE property
((Range //www.ontoweb.org/extended\#tool)
(Domain //www.ontoweb.org/extended\#application)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#inferenceengine")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_COLLABORATIVE_WORKING (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_COLLABORATIVE_WORKING property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_collaborative_working")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DEFAULT_ROOT_RELATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DEFAULT_ROOT_RELATION property
())

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#INBOOK)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_WORK_MANAGEMENT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_WORK_MANAGEMENT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_work_management")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#INCLUDES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#INCLUDES property
((Range //www.ontoweb.org/extended\#ontology)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#includes")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#SPECIALINTERESTGROUP)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#UNIVERSITY)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWSOUND (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWSOUND property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allowsound")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DELIVERYLANGUAGE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DELIVERYLANGUAGE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#deliverylanguage")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#URL___PUBLICATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#URL___PUBLICATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#publication)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#url)
(Subpropertyof //www.ontoweb.org/extended\#url)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ABSTRACT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ABSTRACT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#publication)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#abstract")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#EMPLOYS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#EMPLOYS property
((Range //www.ontoweb.org/extended\#person)
(Domain //www.ontoweb.org/extended\#organisation)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#employs")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ADRESS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ADRESS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#person)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#adress")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#KEYWORDS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#KEYWORDS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#publication)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#keywords")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCTITLE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCTITLE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dctitle")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#TITLE___EDUCATIONALRESSOURCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#TITLE___EDUCATIONALRESSOURCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#title)
(Subpropertyof //www.ontoweb.org/extended\#title)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#MAXIMUM_ARITY_ALLOWED (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#MAXIMUM_ARITY_ALLOWED property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#maximum_arity_allowed")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NAME___NEWS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NAME___NEWS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#news)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#name)
(Subpropertyof //www.ontoweb.org/extended\#name)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#USEDTODEVELOP (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#USEDTODEVELOP property
((Range //www.ontoweb.org/extended\#ontology)
(Domain //www.ontoweb.org/extended\#methodology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#usedtodevelop")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#USING (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#USING property
((Range //www.ontoweb.org/extended\#methodology)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#using")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#INFERENCEENGINEUSED (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#INFERENCEENGINEUSED property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#applicationforintelligentinformationintegration)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#inferenceengineused")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NAME___PERSON (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NAME___PERSON property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#person)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#name)
(Subpropertyof //www.ontoweb.org/extended\#name)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#INCOLLECTION)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#DEPARTMENT)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCCOVERAGE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCCOVERAGE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dccoverage")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#BENEFITS___APPLICATIONFORINTELLIGENTINFORMATIONINTEGRATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#BENEFITS___APPLICATIONFORINTELLIGENTINFORMATIONINTEGRATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#applicationforintelligentinformationintegration)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#benefits)
(Subpropertyof //www.ontoweb.org/extended\#benefits)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#FINANCEDBY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#FINANCEDBY property
((Range //www.ontoweb.org/extended\#organisation)
(Domain //www.ontoweb.org/extended\#project)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#financedby")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWPRODUCTIONRULES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWPRODUCTIONRULES property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allowproductionrules")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#PROJECT)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_MERGE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_MERGE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_merge")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ISEXPORTEDTO (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ISEXPORTEDTO property
((Range //www.ontoweb.org/extended\#tool)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#isexportedto")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCIDENTIFIER (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCIDENTIFIER property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dcidentifier")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___NEWS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___NEWS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#news)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#description)
(Subpropertyof //www.ontoweb.org/extended\#description)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NUMBEROFRELATIONS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NUMBEROFRELATIONS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#numberofrelations")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#APPLICATIONFORECOMMERCE)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NAME___PROJECT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NAME___PROJECT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#project)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#name)
(Subpropertyof //www.ontoweb.org/extended\#name)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_USER_CHANGE_CONTROL (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_USER_CHANGE_CONTROL property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_user_change_control")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#BUSINESSSECTOR (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#BUSINESSSECTOR property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#businesssector")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#MANAGER (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#MANAGER property
((Range //www.ontoweb.org/extended\#person)
(Domain //www.ontoweb.org/extended\#project)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#manager")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#ONTOWEBPORTAL)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCDATE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCDATE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dcdate")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#METAONTOLOGY)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#TASKONTOLOGY)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCRELATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCRELATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dcrelation")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#YEAR (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#YEAR property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#publication)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#year")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#AUDIENCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#AUDIENCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#audience")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_DOCUMENTATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_DOCUMENTATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_documentation")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCTYPE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCTYPE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dctype")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#RELATED (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#RELATED property
((Range //www.ontoweb.org/extended\#topic)
(Domain //www.ontoweb.org/extended\#application)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#related")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#NEWS)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NUMBEROFAXIOMS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NUMBEROFAXIOMS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#numberofaxioms")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#PEDAGOGICROLE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#PEDAGOGICROLE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#pedagogicrole")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#MEETING)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#THESIS)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NAME___ONTOLOGY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NAME___ONTOLOGY property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontology)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#name)
(Subpropertyof //www.ontoweb.org/extended\#name)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#PRODUCT)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_INHERITANCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_INHERITANCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_inheritance")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___EDUCATIONALRESSOURCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___EDUCATIONALRESSOURCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#educationalressource)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#description)
(Subpropertyof //www.ontoweb.org/extended\#description)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_CONFIGURATION_MANAGEMENT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_CONFIGURATION_MANAGEMENT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_configuration_management")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NAME___BUSINESSAREA (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NAME___BUSINESSAREA property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessarea)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#name)
(Subpropertyof //www.ontoweb.org/extended\#name)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#BOOK)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#LACKOFTRANSPARENTROL (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#LACKOFTRANSPARENTROL property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#lackoftransparentrol")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#IMPROVEINTRANETCOMMUNICATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#IMPROVEINTRANETCOMMUNICATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#improveintranetcommunication")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#description")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#REPRESENTATIONONTOLOGY)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#INSTITUTE)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCDESCRIPTION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCDESCRIPTION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dcdescription")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#MAINTENANCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#MAINTENANCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#maintenance")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#ASSOCIATION)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#USEDTOIMPLEMENT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#USEDTOIMPLEMENT property
((Range //www.ontoweb.org/extended\#ontology)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#usedtoimplement")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#RECOMMENDEDLIFECYCLE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#RECOMMENDEDLIFECYCLE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#methodology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#recommendedlifecycle")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#KNOWLEDGEAQUISITION___APPLICATIONFORINTELLIGENTINFORMATIONINTEGRATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#KNOWLEDGEAQUISITION___APPLICATIONFORINTELLIGENTINFORMATIONINTEGRATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#applicationforintelligentinformationintegration)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#knowledgeaquisition)
(Subpropertyof //www.ontoweb.org/extended\#knowledgeaquisition)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#GENERATESFROM (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#GENERATESFROM property
((Range //www.ontoweb.org/extended\#tool)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#generatesfrom")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ISIMPLEMENTEDIN (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ISIMPLEMENTEDIN property
((Range //www.ontoweb.org/extended\#language)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#isimplementedin")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWREASONING (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWREASONING property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allowreasoning")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#KRFORMALISM (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#KRFORMALISM property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#krformalism")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#STUDIESAT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#STUDIESAT property
((Range //www.ontoweb.org/extended\#university)
(Domain //www.ontoweb.org/extended\#student)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#studiesat")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#WEBBASEDLANGUAGE)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NAME (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NAME property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#name")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NUMBEROFINSTANCES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NUMBEROFINSTANCES property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#numberofinstances")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#DEFAULT_ROOT_CONCEPT)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ISDEALTWITHIN (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ISDEALTWITHIN property
((Range //www.ontoweb.org/extended\#project)
(Domain //www.ontoweb.org/extended\#researcharea)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#isdealtwithin")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWIMPLEMENTEDINFERENCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOWIMPLEMENTEDINFERENCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#language)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allowimplementedinference")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#TOPIC___EDUCATIONALRESSOURCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#TOPIC___EDUCATIONALRESSOURCE property
((Range //www.ontoweb.org/extended\#topic)
(Domain //www.ontoweb.org/extended\#educationalressource)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#topic)
(Subpropertyof //www.ontoweb.org/extended\#topic)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#CARRIESOUT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#CARRIESOUT property
((Range //www.ontoweb.org/extended\#project)
(Domain //www.ontoweb.org/extended\#organisation)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#carriesout")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#MEMBER (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#MEMBER property
((Range //www.ontoweb.org/extended\#person)
(Domain //www.ontoweb.org/extended\#project)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#member")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGYFORCOOPERATIVECONSTRUCTION)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#METHODOLOGYFOREVALUATION)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#BENEFITS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#BENEFITS property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#benefits")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#CARRIEDOUTBY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#CARRIEDOUTBY property
((Range //www.ontoweb.org/extended\#organisation)
(Domain //www.ontoweb.org/extended\#researcharea)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#carriedoutby")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#HOMEPAGE___APPLICATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#HOMEPAGE___APPLICATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#application)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#homepage)
(Subpropertyof //www.ontoweb.org/extended\#homepage)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#BIBLIOGRAPHYDETAILS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#BIBLIOGRAPHYDETAILS property
((Range //www.ontoweb.org/extended\#publication)
(Domain //www.ontoweb.org/extended\#educationalressource)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#bibliographydetails")
))

;;there's a relate called topic
;;;so we don't load the class
;;;(def-class //WWW.ONTOWEB.ORG/EXTENDED\#TOPIC)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ADVANTAGES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ADVANTAGES property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#advantages")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#RUNTIMEDEPLOYMENT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#RUNTIMEDEPLOYMENT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#runtimedeployment")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_VALIDATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#ALLOW_VALIDATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#allow_validation")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#BELONGSTO (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#BELONGSTO property
((Range //www.ontoweb.org/extended\#organisation)
(Domain //www.ontoweb.org/extended\#person)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#belongsto")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___BUSINESSAREA (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___BUSINESSAREA property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessarea)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#description)
(Subpropertyof //www.ontoweb.org/extended\#description)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#EMAIL (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#EMAIL property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#person)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#email")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DEVELOPS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DEVELOPS property
((Range //www.ontoweb.org/extended\#product)
(Domain //www.ontoweb.org/extended\#organisation)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#develops")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#CONTAINS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#CONTAINS property
((Range //www.ontoweb.org/extended\#ontology)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#contains")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___APPLICATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___APPLICATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#application)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#description)
(Subpropertyof //www.ontoweb.org/extended\#description)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#FINANCES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#FINANCES property
((Range //www.ontoweb.org/extended\#project)
(Domain //www.ontoweb.org/extended\#organisation)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#finances")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DCSOURCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DCSOURCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontowebportal)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#dcsource")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#DOMAINONTOLOGY)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#QUALITYASSURANCE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#QUALITYASSURANCE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#application)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#qualityassurance")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#PUBLISHES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#PUBLISHES property
((Range //www.ontoweb.org/extended\#publication)
(Domain //www.ontoweb.org/extended\#organisation)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#publishes")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#COMPETITION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#COMPETITION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#competition")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#GENERATES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#GENERATES property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#generates")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#SEMANTICPORTALSANDWEBCOMMUNITIES)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#BUSINESSSCENARIO)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#CONTACT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#CONTACT property
((Range //www.ontoweb.org/extended\#person)
(Domain //www.ontoweb.org/extended\#event)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#contact")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#PHDTHESIS)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#BENEFITS___APPLICATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#BENEFITS___APPLICATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#application)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#benefits)
(Subpropertyof //www.ontoweb.org/extended\#benefits)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#KNOWLEDGEAQUISITION___APPLICATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#KNOWLEDGEAQUISITION___APPLICATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#application)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#knowledgeaquisition)
(Subpropertyof //www.ontoweb.org/extended\#knowledgeaquisition)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#STEPS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#STEPS property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#methodology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#steps")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#DOMAINTASKONTOLOGY)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#EXHIBITION)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#EXPORTSTO (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#EXPORTSTO property
((Range //www.ontoweb.org/extended\#language)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#exportsto")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DOMAINOFONTOLOGY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DOMAINOFONTOLOGY property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#domainofontology")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#TOPIC (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#TOPIC property
((Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#topic")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#TOPIC___NEWS (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#TOPIC___NEWS property
((Range //www.ontoweb.org/extended\#topic)
(Domain //www.ontoweb.org/extended\#news)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#topic)
(Subpropertyof //www.ontoweb.org/extended\#topic)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#INTEROPERABILITY (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#INTEROPERABILITY property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#businessscenario)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#interoperability")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#APPLICATIONONTOLOGY)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#TITLE___APPLICATION (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#TITLE___APPLICATION property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#application)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#title)
(Subpropertyof //www.ontoweb.org/extended\#title)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#APPLICATIONFORNATURALLANGUAGEPROCESSING)

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#PROCEEDINGS)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#LICENCEPRICE (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#LICENCEPRICE property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#ontology)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#licenceprice")
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#HOMEPAGE___PERSON (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#HOMEPAGE___PERSON property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#person)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#homepage)
(Subpropertyof //www.ontoweb.org/extended\#homepage)
))

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___PROJECT (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#DESCRIPTION___PROJECT property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#project)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#description)
(Subpropertyof //www.ontoweb.org/extended\#description)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#TECHREPORT)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#NAME___RESEARCHAREA (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#NAME___RESEARCHAREA property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#researcharea)
(//Schema.Ontoprise.Com/Oxml/Rdf/1.0\#Is_Local_Relation_Of //www.ontoweb.org/extended\#name)
(Subpropertyof //www.ontoweb.org/extended\#name)
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#WORKGROUP)

(def-relation //WWW.ONTOWEB.ORG/EXTENDED\#HAS_LIBRARIES_OF_ONTOLOGIES (?x ?y))

(def-instance //WWW.ONTOWEB.ORG/EXTENDED\#HAS_LIBRARIES_OF_ONTOLOGIES property
((Range //www.w3.org/2001/xmlschema\#string)
(Domain //www.ontoweb.org/extended\#tool)
(Subpropertyof //www.ontoweb.org/extended\#default_root_relation)
(Label "http://www.ontoweb.org/extended#has_libraries_of_ontologies")
))

(def-class //WWW.ONTOWEB.ORG/EXTENDED\#PUBLICATION)
