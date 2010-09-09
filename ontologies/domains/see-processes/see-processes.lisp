;;
;; Ontology Capturing the SEE Processes
;; Serves as SEE formalisation of the internal execution processes
;; and supports monitoring them
;;
;; Author: Carlos Pedrinaci
;; Creation Date: 10/7/2008
;; Last Change: 10/7/2008
;;

(in-package "OCML")
(in-ontology see-processes)

;; Ontology Classes

(def-class #_InvokeWebService ( #_COBRA:Activity )
)

(def-class #_SelectWebService ( #_COBRA:Activity )
)

(def-class #_DiscoverWebServices ( #_COBRA:Activity )
)

(def-class #_AchieveGoal ( #_COBRA:Process )
)



