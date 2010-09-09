;;; Copyright © 2008 The Open University

(in-package #:ocml)

(in-ontology xml-rpc)

;;; {{{ Serialisation to XML

;;; Move a structure of things above into XML.  Should go in the other
;;; direction, too.

(def-rule #_mapToXml
    ((#_mapToXml ?methodcall ?xml) if
     (#_MethodCall ?methodcall)
     (#_methodName ?methodcall ?methodname)
     (= ?methodnamexml (#_xml:Element "methodName" ()
                                     (#_xml:Text ?methodname)))
     (#_params ?methodcall ?params)
     (#_mapParamsToXml ?params ?paramsxml)
     (= ?xml (#_xml:Element "methodCall" () ?methodnamexml ?paramsxml))))

(def-rule #_mapParamsToXml
    ((#_mapParamsToXml ?params ?paramsxml) if
     (= ?xmls (setofall ?xp (and (member ?p ?params)
                                 (#_value ?p ?v)
                                 (#_paramToXml ?v ?xp1)
                                 (= ?xp (#_xml:Element "param" () ?xp1)))))
     (= ?paramsxml (#_xml:ElementList "params" () (reverse ?xmls)))))

(def-rule #_paramToXml
    ((#_paramToXml ?param ?xml) if
     (#_I4 ?param)
     (#_value ?param ?value)
     (= ?xml (#_xml:Element "value" ()
                           (#_xml:Element "i4" () (#_xml:Text (#_toString ?value))))))
  ((#_paramToXml ?param ?xml) if
   (#_String ?param)
   (#_value ?param ?value)
   (= ?xml (#_xml:Element "value" ()
                         (#_xml:Element "string" ()
                                       (#_xml:Text (#_toString ?value))))))
  ((#_paramToXml ?param ?xml) if
   (#_Base64 ?param)
   (#_value ?param ?value)
   (= ?xml (#_xml:Element "value" ()
                         (#_xml:Element "string" ()
                                       (#_xml:Text (#_toString ?value))))))
  ((#_paramToXml ?param ?xml) if
   (#_Struct ?param)
   (#_value ?param ?values)
   (= ?members (setofall ?member
			 (and (member ?memb ?values)
			      (#_name ?memb ?name)
			      (#_value ?memb ?value)
			      (#_paramToXml ?value ?value-xml)
			      (= ?member (#_xml:Element "member" ()
							    (#_xml:Element "name" ()
									   (#_xml:Text ?name))
							    ?value-xml)))))
   (= ?xml (#_xml:Element "value" () (#_xml:ElementList "struct" () ?members)))))

(def-function #_toString (thing)
  :lisp-fun (lambda (thing)
              (format nil "~A" thing)))

;;; }}}
