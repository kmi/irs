;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology luisaelearning-domain)

(def-class learning-object
((has-name :type string)
 (has-id :type integer)
 (has-competency :type competency)
 (has-competency-requirement :type competency)
 (has-language :type language)
 (has-related-learningobjects :type learning-object-list)))

(def-class competency
((has-competency-name :type string)
 (has-competency-id :type integer)))

(def-class language
((has-language-name :type string)
 (has-language-id :type integer)))

(def-class learning-object-list (list) ?x
:constraint (exists ?e (and (element-of ?e ?x)
                            (learning-object ?e))))

(def-class actor
((has-actor-name :type string)
 (has-actor-id :type integer)))

(def-class learner (actor) ?x
((has-competency :type competency)
 (has-language :type language)
 (has-competency-demand :type competency))))
;;; :constraint (exists ?e )