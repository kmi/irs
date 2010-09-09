(in-package #:ocml)

(in-ontology json)

;Based on the JSON specification as described at http://www.json.org/

;{{{ Value
(def-class #_Value () ?value
"Values can be string, number, true, false, null, object, or array.
Of these, string, number, and true and false are matched to the OCML
built-in types String, Number, and Boolean respectively.
Null is treated as NIL. Finally, two new classes, Object and Array,
are introduced below as subclasses of Value."
  :sufficient
  (or
   (String ?value)
   (Number ?value)
   (Boolean ?value)
   (= ?value nil)))
;}}}

;;According to the JSON specification described at json.org, JSON is
;;built on two main types of structures: Objects and Arrays

;{{{ Object
(def-class #_Object (#_Value)
"An Object is an unordered set of key/value pairs."
  ((#_members :type #_Members)))

(def-class #_Members () ?members
  :iff-def (and (listp ?members)
		(every ?members #_Pair)))

(def-class #_Pair ()
  ((#_key :type string)
   (#_value :type #_Value)))
;}}}

;{{{ Array
(def-class #_Array (#_Value)
"An Array is an ordered collection of values."
  ((#_elements :type #_Elements)))

(def-class #_Elements () ?elements
  :iff-def (and (listp ?elements)
		(every ?elements #_Value)))
;}}}
