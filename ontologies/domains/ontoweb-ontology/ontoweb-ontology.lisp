;;; Mode: Lisp; Package: ocml

(in-package "OCML")

(in-ontology ONTOWEB-ONTOLOGY)

;;;Automatically translated from RDF file "freaky:rdf-files;ontoweb-data;OntoWeb-Ontology-RC1.rdfs"
;;;at 14:55:54, on 5/11/2003

(def-class Default_Root_Concept ()
())

(def-class binary-relation (Default_Root_Concept))

(def-class ONTO-WEB-PORTAL (default_root_concept)
((Author 
:type string)
(Language 
:type string)
(Rights 
:type string)
(Format 
:type string)
(Subject 
:type string)
(Contributor 
:type string)
(Publisher 
:type string)
(Creator 
:type string)
(Title 
:type string)
(Coverage 
:type string)
(Identifier 
:type string)
(Date 
:type string)
(Relation 
:type string)
(Type 
:type string)
(Description 
:type string)
(Source 
:type string)
))


(def-class EVENT (onto-web-portal)
((Name___Event 
:type string)
(Location___Event 
:type string)
(Date_Start_ 
:type string)
(Description___Event 
:type string)
(Date_End_ 
:type string)
(Contact 
:type person)
))


(def-class EDUCATIONAL-RESSOURCE (onto-web-portal)
((Duration 
:type string)
(Provider 
:type organisation)
(Difficulty-Level 
:type string)
(Copyright 
:type string)
(Educational-Aim 
:type string)
(Url___Educational-Ressource 
:type string)
(Category 
:type string)
(Delivery-Language 
:type string)
(Title___Educational-Ressource 
:type string)
(Audience 
:type string)
(Pedagogic-Role 
:type string)
(Description___Educational-Ressource 
:type string)
(Topic___Educational-Ressource 
:type topic)
(Bibliography-Details 
:type publication)
))


(def-class ORGANISATION (onto-web-portal)
((Description___Organisation 
:type string)
(Name___Organisation 
:type string)
(Location___Organisation 
:type string)
(Homepage___Organisation 
:type string)
(Employs 
:type person)
(Carries-Out 
:type project)
(Develops 
:type product)
(Finances 
:type project)
(Publishes 
:type publication)
))


(def-class dc-LANGUAGE (onto-web-portal)
((Allow-Instances-Attributes 
:type string)
(Allow-Slot-Default-Value 
:type string)
(Allow-Complete 
:type string)
(Maximum-Arity 
:type string)
(Is-Imported-From 
:type tool)
(Allow-Sound 
:type string)
(Allow-Production-Rules 
:type string)
(Is-Exported-To 
:type tool)
(Used-To-Implement 
:type ontology)
(Generates-From 
:type tool)
(Allow-Reasoning 
:type string)
(K-Rformalism 
:type string)
(Allow-Implemented-Inference 
:type string)
))

(def-class PERSON (onto-web-portal)
((belongs_name :type string) ;;added by john domingue
 (belongs_url :type url) ;;added by john domingue
(Phone 
:type string)
(Photo 
:type string)
(Fax 
:type string)
(Author-Of 
:type publication)
(Adress 
:type string)
(Name___Person 
:type string)
(Belongs-To 
:type organisation)
(Email 
:type string)
(Homepage 
:type string)
))


(def-class APPLICATION (onto-web-portal)
((Has-Methodology 
:type methodology)
(Success-Stories 
:type string)
(Problems 
:type string)
(Inference-Engine 
:type tool)
(Related 
:type topic)
(Homepage___Application 
:type string)
(Description___Application 
:type string)
(Quality-Assurance 
:type string)
(Benefits___Application 
:type string)
(Knowledge-Aquisition___Application 
:type string)
(Title___Application 
:type string)
))


(def-class ONTOLOGY (onto-web-portal)
((Number-Of-Metaclasses 
:type string)
(Branching-Factor 
:type string)
(Number-Of-Concepts 
:type string)
(Includes 
:type ontology)
(Using 
:type methodology)
(Number-Of-Relations 
:type string)
(Number-Of-Axioms 
:type string)
(Name___Ontology 
:type string)
(Is-Implemented-In 
:type dc-language)
(Number-Of-Instances 
:type string)
(Domain-Of-Ontology 
:type string)
(Licence-Price 
:type string)
))


(def-class METHODOLOGY (onto-web-portal)
((Is-Supported-By 
:type tool)
(Used-To-Develop 
:type ontology)
(Recommended-Life-Cycle 
:type string)
(Steps 
:type string)
))


(def-class PROJECT (onto-web-portal)
((Homepage___Project 
:type string)
(Head 
:type person)
(Carried-Out-B-Y 
:type organisation)
(Financed-By 
:type organisation)
(Name___Project 
:type string)
(Manager 
:type person)
(Member 
:type person)
(Description___Project 
:type string)
))


(def-class NEWS (onto-web-portal)
((Short-Description 
:type string)
(Name___News 
:type string)
(Description___News 
:type string)
(Topic___News 
:type topic)
))


(def-class PRODUCT (onto-web-portal)
())


(def-class TOPIC (onto-web-portal)
())


(def-class BUSINESS-SCENARIO (onto-web-portal)
((Misc-Risks 
:type string)
(Improve-E-Commerce 
:type string)
(Other-Technical-Risks 
:type string)
(Source-Information-Availability 
:type string)
(Improve-B2-B 
:type string)
(Improve-K-M 
:type string)
(Automation-Of-Manual-Task 
:type string)
(Other-Commercial-Risks 
:type string)
(Initial-Construction-Costs 
:type string)
(Potential-Cost-Cutting 
:type string)
(Performance 
:type string)
(Business-Sector 
:type string)
(Lack-Of-Transparent-Rol 
:type string)
(Improve-Intranet-Communication 
:type string)
(Maintenance 
:type string)
(Advantages 
:type string)
(Runtime-Deployment 
:type string)
(Competition 
:type string)
(Interoperability 
:type string)
))


(def-class PUBLICATION (onto-web-portal)
((Title___Publication 
:type string)
(Note 
:type string)
(Url___Publication 
:type string)
(Abstract 
:type string)
(Keywords 
:type string)
(Year 
:type string)
))


(def-class WORKGROUP (organisation)
())


(def-class TECH-REPORT (publication)
())


(def-class PROCEEDINGS (publication)
())


(def-class APPLICATION-FOR-NATURAL-LANGUAGE-PROCESSING (application)
())


(def-class APPLICATION-ONTOLOGY (ontology)
())


(def-class EXHIBITION (event)
())


(def-class DOMAIN-TASK-ONTOLOGY (ontology)
())


(def-class SEMANTIC-PORTALS-AND-WEB-COMMUNITIES (application)
())


(def-class DOMAIN-ONTOLOGY (ontology)
())


(def-class METHODOLOGY-FOR-EVALUATION (methodology)
())


(def-class METHODOLOGY-FOR-COOPERATIVE-CONSTRUCTION (methodology)
())


(def-class WEB-BASED-LANGUAGE (dc-language)
())


(def-class ASSOCIATION (organisation)
())


(def-class INSTITUTE (organisation)
())


(def-class REPRESENTATION-ONTOLOGY (ontology)
())


(def-class BOOK (publication)
())


(def-class THESIS (publication)
())


(def-class MEETING (event)
())


(def-class TASK-ONTOLOGY (ontology)
())


(def-class META-ONTOLOGY (ontology)
())


(def-class APPLICATION-FOR-E-COMMERCE (application)
())


(def-class DEPARTMENT (organisation)
())


(def-class IN-COLLECTION (publication)
())


(def-class UNIVERSITY (organisation)
())


(def-class SPECIAL-INTEREST-GROUP (organisation)
())


(def-class IN-BOOK (publication)
())


(def-class ENTERPRISE (organisation)
())


(def-class METHODOLOGY-FOR-MERGE (methodology)
())


(def-class APPLICATION-FOR-INFORMATION-RETRIEVAL (application)
())


(def-class BOOKLET (publication)
())


(def-class LECTURE (event)
())


(def-class CONFERENCE (event)
())


(def-class ACADEMIC-STAFF (person)
())


(def-class BUSINESS-AREA (topic)
((Name___Business-Area 
:type string)
(Description___Business-Area 
:type string)
))


(def-class ARTICLE (publication)
())


(def-class METHODOLOGY-FROM-THE-SCRATCH (methodology)
())


(def-class STUDENT (person)
((Studies-At 
:type university)
))


(def-class METHODOLOGY-FOR-ONTOLOGY-LEARNING (methodology)
())


(def-class CONSORTIUM (organisation)
())


(def-class TOOL (product)
((Allow_Exception_Handling 
:type string)
(Allows 
:type string)
(Imports 
:type dc-language)
(Allow_Constraint 
:type string)
(Allow_Prodution_Rules 
:type string)
(Supports 
:type methodology)
(Pricing-Policy 
:type string)
(Allow_Locking_Level 
:type string)
(Allow_Attached_Inference_Engine 
:type string)
(Allow_Default_Value 
:type string)
(Functionality 
:type string)
(Allow_Collaborative_Working 
:type string)
(Allow_Work_Management 
:type string)
(Maximum_Arity_Allowed 
:type string)
(Allow_Merge 
:type string)
(Allow_User_Change_Control 
:type string)
(Allow_Documentation 
:type string)
(Allow_Inheritance 
:type string)
(Allow_Configuration_Management 
:type string)
(Allow_Validation 
:type string)
(Contains 
:type ontology)
(Exports-To 
:type dc-language)
(Has_Libraries_Of_Ontologies 
:type string)
))


(def-class MISC-PUBLICATION (publication)
())


(def-class METHODOLOGY-FOR-REENGINEERING (methodology)
())


(def-class WORKSHOP (event)
())


(def-class MANUAL (publication)
())


(def-class APPLICATION-FOR-KNOWLEDGE-MANAGEMENT (application)
())


(def-class APPLICATION-FOR-INTELLIGENT-INFORMATION-INTEGRATION (application)
((Methodology-Used 
:type methodology)
(Inference-Engine-Used 
:type string)
(Benefits___Application-For-Intelligent-Information-Integration 
:type string)
(Knowledge-Aquisition___Application-For-Intelligent-Information-Integration 
:type string)
))


(def-class PROJECT-MANAGEMENT-BOARD (organisation)
())


(def-class APPLICATION-FOR-EDUCATION (application)
())


(def-class MISC (event)
())


(def-class RESEARCH-GROUP (organisation)
())


(def-class LINGUISTIC-ONTOLOGY (ontology)
())


(def-class RESEARCH-AREA (topic)
((Is-Worked-On-By 
:type academic-staff)
(Description___Research-Area 
:type string)
(Is-Dealt-With-In 
:type project)
(Carried-Out-By 
:type organisation)
(Name___Research-Area 
:type string)
))


(def-class TRADITIONAL-LANGUAGE (dc-language)
())


(def-class COMPANY-STAFF (person)
())


(def-class IN-PROCEEDINGS (publication)
())


(def-class GENERIC-ONTOLOGY (ontology)
())


(def-class MASTER-THESIS (thesis)
())


(def-class PHD-THESIS (thesis)
())


(def-relation ALLOW_EXCEPTION_HANDLING (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_exception_handling"
)

(def-instance ALLOW_EXCEPTION_HANDLING binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW-INSTANCES-ATTRIBUTES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#allowInstancesAttributes"
)

(def-instance ALLOW-INSTANCES-ATTRIBUTES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DESCRIPTION___ORGANISATION (?x ?y)
)

(def-instance DESCRIPTION___ORGANISATION binary-relation
((Sub-Property-Of DESCRIPTION)
(Is_Local_Relation_Of DESCRIPTION)
))

(def-relation AUTHOR (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#author"
)

(def-instance AUTHOR binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation LANGUAGE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcLanguage"
)

(def-instance LANGUAGE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation MISC-RISKS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#miscRisks"
)

(def-instance MISC-RISKS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IMPROVE-E-COMMERCE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#improveECommerce"
)

(def-instance IMPROVE-E-COMMERCE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation HOMEPAGE___PROJECT (?x ?y)
)

(def-instance HOMEPAGE___PROJECT binary-relation
((Sub-Property-Of HOMEPAGE)
(Is_Local_Relation_Of HOMEPAGE)
))

(def-relation RIGHTS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcRights"
)

(def-instance RIGHTS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation TITLE___PUBLICATION (?x ?y)
)

(def-instance TITLE___PUBLICATION binary-relation
((Sub-Property-Of TITLE)
(Is_Local_Relation_Of TITLE)
))

(def-relation METHODOLOGY-USED (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#methodologyUsed"
)

(def-instance METHODOLOGY-USED binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation Url (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#URL"
)

(def-instance Url binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation OTHER-TECHNICAL-RISKS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#otherTechnicalRisks"
)

(def-instance OTHER-TECHNICAL-RISKS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

#|
(def-relation METHODOLOGY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#methodology"
)
|#


(def-instance METHODOLOGY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NAME___EVENT (?x ?y)
)

(def-instance NAME___EVENT binary-relation
((Sub-Property-Of NAME)
(Is_Local_Relation_Of NAME)
))

(def-relation PHONE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#phone"
)

(def-instance PHONE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NUMBER-OF-METACLASSES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#numberOfMetaclasses"
)

(def-instance NUMBER-OF-METACLASSES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW-SLOT-DEFAULT-VALUE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#allowSlotDefaultValue"
)

(def-instance ALLOW-SLOT-DEFAULT-VALUE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DURATION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#duration"
)

(def-instance DURATION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation LOCATION___EVENT (?x ?y)
)

(def-instance LOCATION___EVENT binary-relation
((Sub-Property-Of LOCATION)
(Is_Local_Relation_Of LOCATION)
))

(def-relation PROVIDER (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#provider"
)

(def-instance PROVIDER binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation HOMEPAGE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#homepage"
)

(def-instance HOMEPAGE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation BRANCHING-FACTOR (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#branchingFactor"
)

(def-instance BRANCHING-FACTOR binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOWS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#allows"
)

(def-instance ALLOWS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW-COMPLETE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#allowComplete"
)

(def-instance ALLOW-COMPLETE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation SOURCE-INFORMATION-AVAILABILITY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#sourceInformationAvailability"
)

(def-instance SOURCE-INFORMATION-AVAILABILITY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IMPORTS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#imports"
)

(def-instance IMPORTS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DIFFICULTY-LEVEL (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#difficultyLevel"
)

(def-instance DIFFICULTY-LEVEL binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DATE_START_ (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#date(start)"
)

(def-instance DATE_START_ binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation COPYRIGHT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#copyright"
)

(def-instance COPYRIGHT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation FORMAT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcFormat"
)

(def-instance FORMAT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation EDUCATIONAL-AIM (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#educationalAim"
)

(def-instance EDUCATIONAL-AIM binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation HEAD (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#head"
)

(def-instance HEAD binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NAME___ORGANISATION (?x ?y)
)

(def-instance NAME___ORGANISATION binary-relation
((Sub-Property-Of NAME)
(Is_Local_Relation_Of NAME)
))

(def-relation SUBJECT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcSubject"
)

(def-instance SUBJECT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation SUCCESS-STORIES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#successStories"
)

(def-instance SUCCESS-STORIES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW_CONSTRAINT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_constraint"
)

(def-instance ALLOW_CONSTRAINT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation CONTRIBUTOR (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcContributor"
)

(def-instance CONTRIBUTOR binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NUMBER-OF-CONCEPTS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#numberOfConcepts"
)

(def-instance NUMBER-OF-CONCEPTS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation CARRIED-OUT-B-Y (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#carriedOutBY"
)

(def-instance CARRIED-OUT-B-Y binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation MAXIMUM-ARITY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#maximumArity"
)

(def-instance MAXIMUM-ARITY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation LOCATION___ORGANISATION (?x ?y)
)

(def-instance LOCATION___ORGANISATION binary-relation
((Sub-Property-Of LOCATION)
(Is_Local_Relation_Of LOCATION)
))

(def-relation DESCRIPTION___EVENT (?x ?y)
)

(def-instance DESCRIPTION___EVENT binary-relation
((Sub-Property-Of DESCRIPTION)
(Is_Local_Relation_Of DESCRIPTION)
))

(def-relation PUBLISHER (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcPublisher"
)

(def-instance PUBLISHER binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation Url___EDUCATIONAL-RESSOURCE (?x ?y)
)

(def-instance Url___EDUCATIONAL-RESSOURCE binary-relation
((Sub-Property-Of Url)
(Is_Local_Relation_Of Url)
))

(def-relation ALLOW_PRODUTION_RULES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_prodution_rules"
)

(def-instance ALLOW_PRODUTION_RULES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IS-WORKED-ON-BY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#isWorkedOnBy"
)

(def-instance IS-WORKED-ON-BY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation TITLE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#title"
)

(def-instance TITLE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation MODELING-GUIDELINES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#ModelingGuidelines"
)

(def-instance MODELING-GUIDELINES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation SUPPORTS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#supports"
)

(def-instance SUPPORTS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IMPROVE-B2-B (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#improveB2B"
)

(def-instance IMPROVE-B2-B binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IMPROVE-K-M (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#improveKM"
)

(def-instance IMPROVE-K-M binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation PROBLEMS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#problems"
)

(def-instance PROBLEMS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation PHOTO (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#photo"
)

(def-instance PHOTO binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IS-SUPPORTED-BY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#isSupportedBy"
)

(def-instance IS-SUPPORTED-BY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation CREATOR (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcCreator"
)

(def-instance CREATOR binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation PRICING-POLICY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#PricingPolicy"
)

(def-instance PRICING-POLICY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation HOMEPAGE___ORGANISATION (?x ?y)
)

(def-instance HOMEPAGE___ORGANISATION binary-relation
((Sub-Property-Of HOMEPAGE)
(Is_Local_Relation_Of HOMEPAGE)
))

(def-relation AUTOMATION-OF-MANUAL-TASK (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#automationOfManualTask"
)

(def-instance AUTOMATION-OF-MANUAL-TASK binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation LOCATION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#location"
)

(def-instance LOCATION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation CATEGORY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#category"
)

(def-instance CATEGORY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation OTHER-COMMERCIAL-RISKS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#otherCommercialRisks"
)

(def-instance OTHER-COMMERCIAL-RISKS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW_LOCKING_LEVEL (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_locking_level"
)

(def-instance ALLOW_LOCKING_LEVEL binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DESCRIPTION___RESEARCH-AREA (?x ?y)
)

(def-instance DESCRIPTION___RESEARCH-AREA binary-relation
((Sub-Property-Of DESCRIPTION)
(Is_Local_Relation_Of DESCRIPTION)
))

(def-relation ALLOW_ATTACHED_INFERENCE_ENGINE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_attached_inference_engine"
)

(def-instance ALLOW_ATTACHED_INFERENCE_ENGINE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW_DEFAULT_VALUE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_default_value"
)

(def-instance ALLOW_DEFAULT_VALUE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation INITIAL-CONSTRUCTION-COSTS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#initialConstructionCosts"
)

(def-instance INITIAL-CONSTRUCTION-COSTS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation FAX (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#fax"
)

(def-instance FAX binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation AUTHOR-OF (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#authorOf"
)

(def-instance AUTHOR-OF binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation KNOWLEDGE-AQUISITION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#knowledgeAquisition"
)

(def-instance KNOWLEDGE-AQUISITION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation SHORT-DESCRIPTION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#shortDescription"
)

(def-instance SHORT-DESCRIPTION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation POTENTIAL-COST-CUTTING (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#potentialCostCutting"
)

(def-instance POTENTIAL-COST-CUTTING binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IS-IMPORTED-FROM (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#isImportedFrom"
)

(def-instance IS-IMPORTED-FROM binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NOTE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#note"
)

(def-instance NOTE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation FUNCTIONALITY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Functionality"
)

(def-instance FUNCTIONALITY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DATE_END_ (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#date(end)"
)

(def-instance DATE_END_ binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation PERFORMANCE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#performance"
)

(def-instance PERFORMANCE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation INFERENCE-ENGINE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#inferenceEngine"
)

(def-instance INFERENCE-ENGINE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW_COLLABORATIVE_WORKING (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_collaborative_working"
)

(def-instance ALLOW_COLLABORATIVE_WORKING binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation Default_Root_Relation (?x ?y)
)

(def-relation ALLOW_WORK_MANAGEMENT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_work_management"
)

(def-instance ALLOW_WORK_MANAGEMENT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation INCLUDES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#includes"
)

(def-instance INCLUDES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW-SOUND (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#allowSound"
)

(def-instance ALLOW-SOUND binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DELIVERY-LANGUAGE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#deliveryLanguage"
)

(def-instance DELIVERY-LANGUAGE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation Url___PUBLICATION (?x ?y)
)

(def-instance Url___PUBLICATION binary-relation
((Sub-Property-Of Url)
(Is_Local_Relation_Of Url)
))

(def-relation ABSTRACT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#abstract"
)

(def-instance ABSTRACT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation EMPLOYS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#employs"
)

(def-instance EMPLOYS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ADRESS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#adress"
)

(def-instance ADRESS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation KEYWORDS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#keywords"
)

(def-instance KEYWORDS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation TITLE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcTitle"
)

(def-instance TITLE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation TITLE___EDUCATIONAL-RESSOURCE (?x ?y)
)

(def-instance TITLE___EDUCATIONAL-RESSOURCE binary-relation
((Sub-Property-Of TITLE)
(Is_Local_Relation_Of TITLE)
))

(def-relation MAXIMUM_ARITY_ALLOWED (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Maximum_arity_allowed"
)

(def-instance MAXIMUM_ARITY_ALLOWED binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NAME___NEWS (?x ?y)
)

(def-instance NAME___NEWS binary-relation
((Sub-Property-Of NAME)
(Is_Local_Relation_Of NAME)
))

(def-relation USED-TO-DEVELOP (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#usedToDevelop"
)

(def-instance USED-TO-DEVELOP binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation USING (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#using"
)

(def-instance USING binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation INFERENCE-ENGINE-USED (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#inferenceEngineUsed"
)

(def-instance INFERENCE-ENGINE-USED binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NAME___PERSON (?x ?y)
)

(def-instance NAME___PERSON binary-relation
((Sub-Property-Of NAME)
(Is_Local_Relation_Of NAME)
))

(def-relation COVERAGE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcCoverage"
)

(def-instance COVERAGE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation BENEFITS___APPLICATION-FOR-INTELLIGENT-INFORMATION-INTEGRATION (?x ?y)
)

(def-instance BENEFITS___APPLICATION-FOR-INTELLIGENT-INFORMATION-INTEGRATION binary-relation
((Sub-Property-Of BENEFITS)
(Is_Local_Relation_Of BENEFITS)
))

(def-relation FINANCED-BY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#financedBy"
)

(def-instance FINANCED-BY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW-PRODUCTION-RULES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#allowProductionRules"
)

(def-instance ALLOW-PRODUCTION-RULES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW_MERGE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_merge"
)

(def-instance ALLOW_MERGE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IS-EXPORTED-TO (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#isExportedTo"
)

(def-instance IS-EXPORTED-TO binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IDENTIFIER (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcIdentifier"
)

(def-instance IDENTIFIER binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DESCRIPTION___NEWS (?x ?y)
)

(def-instance DESCRIPTION___NEWS binary-relation
((Sub-Property-Of DESCRIPTION)
(Is_Local_Relation_Of DESCRIPTION)
))

(def-relation NUMBER-OF-RELATIONS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#numberOfRelations"
)

(def-instance NUMBER-OF-RELATIONS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NAME___PROJECT (?x ?y)
)

(def-instance NAME___PROJECT binary-relation
((Sub-Property-Of NAME)
(Is_Local_Relation_Of NAME)
))

(def-relation ALLOW_USER_CHANGE_CONTROL (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_user_change_control"
)

(def-instance ALLOW_USER_CHANGE_CONTROL binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation BUSINESS-SECTOR (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#businessSector"
)

(def-instance BUSINESS-SECTOR binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation MANAGER (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#manager"
)

(def-instance MANAGER binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DATE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcDate"
)

(def-instance DATE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation RELATION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcRelation"
)

(def-instance RELATION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation YEAR (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#year"
)

(def-instance YEAR binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation AUDIENCE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#audience"
)

(def-instance AUDIENCE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW_DOCUMENTATION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_documentation"
)

(def-instance ALLOW_DOCUMENTATION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation TYPE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcType"
)

(def-instance TYPE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation RELATED (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#related"
)

(def-instance RELATED binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NUMBER-OF-AXIOMS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#numberOfAxioms"
)

(def-instance NUMBER-OF-AXIOMS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation PEDAGOGIC-ROLE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#pedagogicRole"
)

(def-instance PEDAGOGIC-ROLE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NAME___ONTOLOGY (?x ?y)
)

(def-instance NAME___ONTOLOGY binary-relation
((Sub-Property-Of NAME)
(Is_Local_Relation_Of NAME)
))

(def-relation ALLOW_INHERITANCE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_inheritance"
)

(def-instance ALLOW_INHERITANCE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DESCRIPTION___EDUCATIONAL-RESSOURCE (?x ?y)
)

(def-instance DESCRIPTION___EDUCATIONAL-RESSOURCE binary-relation
((Sub-Property-Of DESCRIPTION)
(Is_Local_Relation_Of DESCRIPTION)
))

(def-relation ALLOW_CONFIGURATION_MANAGEMENT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_configuration_management"
)

(def-instance ALLOW_CONFIGURATION_MANAGEMENT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NAME___BUSINESS-AREA (?x ?y)
)

(def-instance NAME___BUSINESS-AREA binary-relation
((Sub-Property-Of NAME)
(Is_Local_Relation_Of NAME)
))

(def-relation LACK-OF-TRANSPARENT-ROL (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#lackOfTransparentRol"
)

(def-instance LACK-OF-TRANSPARENT-ROL binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IMPROVE-INTRANET-COMMUNICATION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#improveIntranetCommunication"
)

(def-instance IMPROVE-INTRANET-COMMUNICATION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DESCRIPTION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#description"
)

(def-instance DESCRIPTION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DESCRIPTION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcDescription"
)

(def-instance DESCRIPTION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation MAINTENANCE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#maintenance"
)

(def-instance MAINTENANCE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation USED-TO-IMPLEMENT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#usedToImplement"
)

(def-instance USED-TO-IMPLEMENT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation RECOMMENDED-LIFE-CYCLE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#recommendedLifeCycle"
)

(def-instance RECOMMENDED-LIFE-CYCLE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation KNOWLEDGE-AQUISITION___APPLICATION-FOR-INTELLIGENT-INFORMATION-INTEGRATION (?x ?y)
)

(def-instance KNOWLEDGE-AQUISITION___APPLICATION-FOR-INTELLIGENT-INFORMATION-INTEGRATION binary-relation
((Sub-Property-Of KNOWLEDGE-AQUISITION)
(Is_Local_Relation_Of KNOWLEDGE-AQUISITION)
))

(def-relation GENERATES-FROM (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#generatesFrom"
)

(def-instance GENERATES-FROM binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IS-IMPLEMENTED-IN (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#isImplementedIn"
)

(def-instance IS-IMPLEMENTED-IN binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW-REASONING (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#allowReasoning"
)

(def-instance ALLOW-REASONING binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation K-RFORMALISM (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#KRformalism"
)

(def-instance K-RFORMALISM binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation STUDIES-AT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#studiesAt"
)

(def-instance STUDIES-AT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NAME (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#name"
)

(def-instance NAME binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation NUMBER-OF-INSTANCES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#numberOfInstances"
)

(def-instance NUMBER-OF-INSTANCES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation IS-DEALT-WITH-IN (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#isDealtWithIn"
)

(def-instance IS-DEALT-WITH-IN binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW-IMPLEMENTED-INFERENCE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#allowImplementedInference"
)

(def-instance ALLOW-IMPLEMENTED-INFERENCE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation TOPIC___EDUCATIONAL-RESSOURCE (?x ?y)
)

(def-instance TOPIC___EDUCATIONAL-RESSOURCE binary-relation
((Sub-Property-Of TOPIC)
(Is_Local_Relation_Of TOPIC)
))

(def-relation CARRIES-OUT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#carriesOut"
)

(def-instance CARRIES-OUT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation MEMBER (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#member"
)

(def-instance MEMBER binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation BENEFITS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#benefits"
)

(def-instance BENEFITS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation CARRIED-OUT-BY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#carriedOutBy"
)

(def-instance CARRIED-OUT-BY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation HOMEPAGE___APPLICATION (?x ?y)
)

(def-instance HOMEPAGE___APPLICATION binary-relation
((Sub-Property-Of HOMEPAGE)
(Is_Local_Relation_Of HOMEPAGE)
))

(def-relation BIBLIOGRAPHY-DETAILS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#bibliographyDetails"
)

(def-instance BIBLIOGRAPHY-DETAILS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ADVANTAGES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#advantages"
)

(def-instance ADVANTAGES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation RUNTIME-DEPLOYMENT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#runtimeDeployment"
)

(def-instance RUNTIME-DEPLOYMENT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation ALLOW_VALIDATION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Allow_validation"
)

(def-instance ALLOW_VALIDATION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation BELONGS-TO (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#belongsTo"
)

(def-instance BELONGS-TO binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DESCRIPTION___BUSINESS-AREA (?x ?y)
)

(def-instance DESCRIPTION___BUSINESS-AREA binary-relation
((Sub-Property-Of DESCRIPTION)
(Is_Local_Relation_Of DESCRIPTION)
))

(def-relation EMAIL (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#email"
)

(def-instance EMAIL binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DEVELOPS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#develops"
)

(def-instance DEVELOPS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation CONTAINS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#contains"
)

(def-instance CONTAINS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DESCRIPTION___APPLICATION (?x ?y)
)

(def-instance DESCRIPTION___APPLICATION binary-relation
((Sub-Property-Of DESCRIPTION)
(Is_Local_Relation_Of DESCRIPTION)
))

(def-relation FINANCES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#finances"
)

(def-instance FINANCES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation SOURCE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#dcSource"
)

(def-instance SOURCE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation QUALITY-ASSURANCE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#qualityAssurance"
)

(def-instance QUALITY-ASSURANCE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation PUBLISHES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#publishes"
)

(def-instance PUBLISHES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation COMPETITION (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#competition"
)

(def-instance COMPETITION binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation GENERATES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#generates"
)

(def-instance GENERATES binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation CONTACT (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#contact"
)

(def-instance CONTACT binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation BENEFITS___APPLICATION (?x ?y)
)

(def-instance BENEFITS___APPLICATION binary-relation
((Sub-Property-Of BENEFITS)
(Is_Local_Relation_Of BENEFITS)
))

(def-relation KNOWLEDGE-AQUISITION___APPLICATION (?x ?y)
)

(def-instance KNOWLEDGE-AQUISITION___APPLICATION binary-relation
((Sub-Property-Of KNOWLEDGE-AQUISITION)
(Is_Local_Relation_Of KNOWLEDGE-AQUISITION)
))

(def-relation STEPS (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#steps"
)

(def-instance STEPS binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation EXPORTS-TO (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#exportsTo"
)

(def-instance EXPORTS-TO binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation DOMAIN-OF-ONTOLOGY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#domainOfOntology"
)

(def-instance DOMAIN-OF-ONTOLOGY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

#|
(def-relation TOPIC (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#topic"
)
|#

(def-instance TOPIC binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation TOPIC___NEWS (?x ?y)
)

(def-instance TOPIC___NEWS binary-relation
((Sub-Property-Of TOPIC)
(Is_Local_Relation_Of TOPIC)
))

(def-relation INTEROPERABILITY (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#interoperability"
)

(def-instance INTEROPERABILITY binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation TITLE___APPLICATION (?x ?y)
)

(def-instance TITLE___APPLICATION binary-relation
((Sub-Property-Of TITLE)
(Is_Local_Relation_Of TITLE)
))

(def-relation LICENCE-PRICE (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#licencePrice"
)

(def-instance LICENCE-PRICE binary-relation
((Sub-Property-Of Default_Root_Relation)
))

(def-relation HOMEPAGE___PERSON (?x ?y)
)

(def-instance HOMEPAGE___PERSON binary-relation
((Sub-Property-Of HOMEPAGE)
(Is_Local_Relation_Of HOMEPAGE)
))

(def-relation DESCRIPTION___PROJECT (?x ?y)
)

(def-instance DESCRIPTION___PROJECT binary-relation
((Sub-Property-Of DESCRIPTION)
(Is_Local_Relation_Of DESCRIPTION)
))

(def-relation NAME___RESEARCH-AREA (?x ?y)
)

(def-instance NAME___RESEARCH-AREA binary-relation
((Sub-Property-Of NAME)
(Is_Local_Relation_Of NAME)
))

(def-relation HAS_LIBRARIES_OF_ONTOLOGIES (?x ?y)
:pretty-name "http://www.OntoWeb.org/extended#Has_libraries_of_ontologies"
)

(def-instance HAS_LIBRARIES_OF_ONTOLOGIES binary-relation
((Sub-Property-Of Default_Root_Relation)
))