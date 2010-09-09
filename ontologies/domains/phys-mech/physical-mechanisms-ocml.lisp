;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue from borst et al, IJCHS vol 46 no 2/3 feb/march 1997
;;;p. 375

;;; The Open University

(in-package "OCML")


(def-ontology phys-mech-ontology)

(in-ontology phys-mech-ontology)

(def-class mechanism ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-class one-port (mechanism))

(def-class two-port (mechanism))

(def-class multi-port (mechanism))


(def-class source (one-port))

(def-class store (one-port))

(def-class dissipator (one-port))


(def-class convertor (two-port))


(def-class distributor (multi-port))


(def-class effort-source (source))

(def-class flow-source (source))


(def-class stuff-store (store))

(def-class action-store (store))


(def-class resistor (dissipator))

(def-class friction (dissipator))


(def-class transformation (convertor))

(def-class gyration (convertor))


(def-class effort-distributor (distributor))

(def-class flow-distributor (distributor))


(def-class voltage-source (effort-source))

(def-class force-source (effort-source))


(def-class current-source (flow-source))

(def-class velocity-source (flow-source))


(def-class capacitor (stuff-store))

(def-class spring (stuff-store))


(def-class inductance (action-store))

(def-class mass (action-store))


(def-class transformer (transformation))

(def-class lever (transformation))


(def-class voice-coil (gyration))

(def-class gyrator (gyration))


(def-class parallel-connection (effort-distributor))

(def-class same-force (effort-distributor))


(def-class series-connection (flow-distributor))

(def-class same-velocity (flow-distributor))

