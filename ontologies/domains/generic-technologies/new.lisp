;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology generic-technologies)

(def-class web-based-electronic-newsletter (web-technology publishing-medium))

(def-class multimedia-technology (software-technology))

(def-class agent-technology (software-technology))

(def-class modelling-technology (software-technology))

(def-class software-visualization-technology (software-technology))

(def-class programming-language (software-technology))

(def-class genetic-algorithms  (software-technology))

(def-class operating-system (software-system))

(def-class software-status ())

(def-class statistical-software-technology (software-technology))

(def-class internet-agent-technology (internet-technology agent-technology))

(def-class modelling-language (programming-language modelling-technology))

(def-class drawing-technology (multimedia-technology))

(def-class bayesian-software-technology (statistical-software-technology))

(def-class web-agent-technology (web-technology internet-agent-technology)
  ())

(def-instance java programming-language)

(def-instance lisp programming-language)

(def-instance prolog programming-language)

(def-instance c++ programming-language)

(def-instance smalltalk programming-language)

(def-instance perl programming-language)

(def-instance hypernews computing-technology)

(def-instance cgi programming-language)

(def-instance frame-logic modelling-technology)

(def-instance alpha-status software-status)

(def-instance beta-status software-status)

(def-instance finished-status software-status)
