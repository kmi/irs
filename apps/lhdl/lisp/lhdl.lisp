(in-package #:irs.applications.lhdl)

(defmethod initialise-application ((application (eql :lhdl)))
  (pushnew "irs:apps;lhdl;ontologies;" ocml:*ontology-path*)
  (webonto:require-ontologies '(:lhdl-application))
  (start-web-interface))

(defmethod start-application ((application (eql :lhdl))))
