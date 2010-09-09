;;; Copyright © 2009 The Open University

(in-package #:ocml)

(in-ontology xml)

(def-rule #_elementByName
  ((#_elementByName ?element ?element ?name) if
   (#_tag ?element ?name))
  ((#_elementByName ?root ?element ?name) if
   (#_contents ?root ?contents)
   (member ?c ?contents)
   (#_elementByName ?c ?element ?name)))

(def-rule #_attributeNameValue
  ((#_attributeNameValue ?element ?name ?value) if
   (#_attributes ?element ?attributes)
   (member ?attribute ?attributes)
   (#_name ?attribute ?name)
   (#_value ?attribute ?value)))
