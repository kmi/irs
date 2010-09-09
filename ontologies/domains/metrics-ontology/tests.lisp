;;; Mode: Lisp; Package: ocml

;; Created by Carlos Pedrinaci 
;;
;; Ontology for supporting the definition of Metrics
;; Provides general purpose constructs for defining metrics
;; which shall then be computed by the appropriate PSM

(in-package "OCML")

(in-ontology metrics-ontology)

;; Ontology Instances

(def-instance #_countAllProcessInstance |http://kmi.open.ac.uk/ontologies/metrics-ontology#Count|
	(
		(|http://ip-super.org/ontologies/process/cobra/v1.1.1#hasAnalysisResult| NIL)
	)
)

(def-instance #_countProcessInstanceRunning |http://kmi.open.ac.uk/ontologies/metrics-ontology#Count|
	(
		(|http://ip-super.org/ontologies/process/cobra/v1.1.1#hasAnalysisResult| NIL)
	)
)

(def-instance #_countProcessInstanceCompleted |http://kmi.open.ac.uk/ontologies/metrics-ontology#Count|
	(
		(|http://ip-super.org/ontologies/process/cobra/v1.1.1#hasAnalysisResult| NIL)
	)
)

(def-instance #_countProcessInstanceTerminated |http://kmi.open.ac.uk/ontologies/metrics-ontology#Count|
	(
		(|http://ip-super.org/ontologies/process/cobra/v1.1.1#hasAnalysisResult| NIL)
	)
)

(def-instance #_countAllActivityInstance |http://kmi.open.ac.uk/ontologies/metrics-ontology#Count|
	(
		(|http://ip-super.org/ontologies/process/cobra/v1.1.1#hasAnalysisResult| NIL)
	)
)

(def-instance #_countActivityInstanceRunning |http://kmi.open.ac.uk/ontologies/metrics-ontology#Count|
	(
		(|http://ip-super.org/ontologies/process/cobra/v1.1.1#hasAnalysisResult| NIL)
	)
)

(def-instance #_countActivityInstanceCompleted |http://kmi.open.ac.uk/ontologies/metrics-ontology#Count|
	(
		(|http://ip-super.org/ontologies/process/cobra/v1.1.1#hasAnalysisResult| NIL)
	)
)

(def-instance #_countActivityInstanceTerminated |http://kmi.open.ac.uk/ontologies/metrics-ontology#Count|
	(
		(|http://ip-super.org/ontologies/process/cobra/v1.1.1#hasAnalysisResult| NIL)
	)
)
