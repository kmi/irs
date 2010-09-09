;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology aktive-portal-ontology2)


;;;This file  provides a very simple notion of GENERIC-AREA-OF-INTEREST,
;;;with two main subclasses: BUSINESS-AREA and RESEARCH-AREA.
;;;Areas can have sub-areas, e.g., adaptive-hypermedia is a sub-area of
;;;hypermedia.
;;;I have also done an initial list of research areas relevant to AKT.  I have
;;;used the sub-area-of slot very timidly, because I know that these are very 
;;;sensitive relations!

(def-class GENERIC-AREA-OF-INTEREST (intangible-thing)
 "A generic class to specify generic areas for research or business initiatives.
  For instance, the area in which a project is situated"
 ((sub-area-of :type generic-area-of-interest)))


(def-class RESEARCH-AREA (generic-area-of-interest)
  ((sub-area-of :type research-area)))

(def-class BUSINESS-AREA (generic-area-of-interest)
  )

(def-instance  LEARNING-RESEARCH-AREA  research-area) 

(def-instance COMPUTING-RESEARCH-AREA research-area)

(def-instance ARTIFICIAL-INTELLIGENCE-RESEARCH-AREA research-area
  )

(def-instance COGNITIVE-MODELLING-RESEARCH-AREA research-area
  )

(def-instance E-COMMERCE-RESEARCH-AREA research-area)

(def-instance TELEPRESENCE-RESEARCH-AREA research-area)

(def-instance HUMAN-COMPUTER-INTERACTION research-area
  ((sub-area-of computing-research-area)))

(def-instance SOFTWARE-VISUALIZATION  research-area
  ((sub-area-of human-computer-interaction)))

(def-instance INFORMATION-RETRIEVAL research-area)

(def-instance AGENT-BASED-COMPUTING research-area)

(def-instance HYPERMEDIA research-area)

(def-instance ADAPTIVE-HYPERMEDIA research-area
  ((sub-area-of hypermedia)))

(def-instance COLLABORATIVE-HYPERMEDIA research-area
  ((sub-area-of hypermedia)))

(def-instance MULTIMEDIA-RESEARCH-AREA  research-area
  )

(def-instance DYNAMIC-LINKING research-area
  ((sub-area-of adaptive-hypermedia)))

(def-instance ORGANIZATIONAL-LEARNING research-area
  ((sub-area-of Learning-research-area)))




(def-instance KNOWLEDGE-MANAGEMENT research-area
  )

(def-instance KNOWLEDGE-ACQUISITION research-area
  ((sub-area-of knowledge-management)))


(def-instance INCIDENTAL-KA  research-area
  ((sub-area-of knowledge-acquisition)))

(def-instance KNOWLEDGE-LIFECYCLE  research-area
  ((sub-area-of knowledge-management)))

(def-instance KNOWLEDGE-MODELLING  research-area
  ((sub-area-of knowledge-management)))

(def-instance ONTOLOGIES  research-area
  ((sub-area-of knowledge-modelling knowledge-reuse)))

(def-instance WEB-RESEARCH-AREA research-area)

(def-instance SEMANTIC-WEB-AREA  research-area
  ((sub-area-of web-research-area)))

(def-instance PROBLEM-SOLVING-METHODS  research-area
  ((sub-area-of knowledge-modelling knowledge-reuse)))

(def-instance KNOWLEDGE-MAINTENANCE  research-area
  ((sub-area-of knowledge-management)))


(def-instance KNOWLEDGE-RETRIEVAL  research-area
  ((sub-area-of knowledge-management)))

(def-instance KNOWLEDGE-REUSE  research-area
 ((sub-area-of knowledge-management)) )

(def-instance KNOWLEDGE-PUBLISHING  research-area
 ((sub-area-of knowledge-management)))

(def-instance LANGUAGE-ENGINEERING  research-area
  )

(def-instance  INFORMATION-EXTRACTION research-area
  ((sub-area-of language-engineering)))






























