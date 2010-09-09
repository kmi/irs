;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology luisa-wsmo-ontology)


(def-class actor ()
((has-actor-id :type integer)
 (has-actor-name :type string))
)

(def-class learner (actor)
((has-learner-history :type learning-object-list)
 (has-competency-demand :type competency-list)
 (has-language :type language)
 (has-competencies :type competency-list)
 (has-max-cost :type cost))
)

(def-class cost ()
((has-amount :type float)
 (has-currency :type currency))
)

(def-class currency ()
((has-currency-code :type string)
 (has-currency-title :type string))
)

(def-class learning-object ()
((has-related-learning-objects :type learning-object-list)
 (has-approximate-learning-time :type integer)
 (has-language :type language)
 (has-required-competencies :type competency-list)
 (has-competency :type competency)
 (has-cost :type cost)
 (has-relation-partof :type learning-object)
 (has-relation-haspart :type learning-object)
 (has-title :type string)
 (has-id :type integer)
 (has-lom-set :type lom-learning-object))
)

(def-class competency ()
((has-required-competencies :type competency-list)
 (has-related-competencies :type competency-list)
 (has-competency-id :type integer)
 (has-competency-title :type string))
)

(def-class competency-list ()
((has-competencies :type competency :min-cardinality 1))
)

(def-class get-lo-list-response ()
((has-retrieved-learning-object-list :type learning-object-list))
)

(def-class learning-object-list ()
((has-learning-objects :type learning-object :min-cardinality 1))
)

(def-class get-lo-by-competency-request ()
((has-competency-request :type string))
)

(def-class get-lo-by-competency-response ()
((has-learning-object :type string))
)

(def-class get-lo-results-by-competency-request ()
((has-competency-request :type competency)
 (has-learner :type learner))
)

(def-class processed-lo-results ()
((has-learner :type learner)
 (has-missing-competencies :type competency-list)
 (has-user-ability :type boolean)
 (has-lo-list :type learning-object-list))
)

(def-class get-lo-results-by-competency-response ()
((has-processed-lo-results :type processed-lo-results))
)

(def-class language ()
((has-language-id :type integer)
 (has-language-title :type string))
)

(def-class get-lo-by-requester-request ()
((has-requester :type learner))
)




(def-class actor ()
((has-actor-id :type integer)
 (has-actor-name :type string))
)

(def-class learner (actor)
((has-learner-history :type learning-object-list)
 (has-competency-demand :type competency-list)
 (has-language :type language)
 (has-competencies :type competency-list)
 (has-max-cost :type cost))
)

(def-class cost ()
((has-amount :type float)
 (has-currency :type currency))
)

(def-class currency ()
((has-currency-code :type string)
 (has-currency-title :type string))
)

(def-class learning-object ()
((has-related-learning-objects :type learning-object-list)
 (has-approximate-learning-time :type integer)
 (has-language :type language)
 (has-required-competencies :type competency-list)
 (has-competency :type competency)
 (has-cost :type cost)
 (has-relation-partof :type learning-object)
 (has-relation-haspart :type learning-object)
 (has-title :type string)
 (has-id :type integer)
 (has-lom-set :type lom-learning-object))
)

(def-class competency ()
((has-required-competencies :type competency-list)
 (has-related-competencies :type competency-list)
 (has-competency-id :type integer)
 (has-competency-title :type string))
)

(def-class competency-list ()
((has-competencies :type competency :min-cardinality 1))
)

(def-class get-lo-list-response ()
((has-retrieved-learning-object-list :type learning-object-list))
)

(def-class learning-object-list ()
((has-learning-objects :type learning-object :min-cardinality 1))
)

(def-class get-lo-by-competency-request ()
((has-competency-request :type string))
)

(def-class get-lo-by-competency-response ()
((has-learning-object :type string))
)

(def-class get-lo-results-by-competency-request ()
((has-competency-request :type competency)
 (has-learner :type learner))
)

(def-class processed-lo-results ()
((has-learner :type learner)
 (has-missing-competencies :type competency-list)
 (has-user-ability :type boolean)
 (has-lo-list :type learning-object-list))
)

(def-class get-lo-results-by-competency-response ()
((has-processed-lo-results :type processed-lo-results))
)

(def-class language ()
((has-language-id :type integer)
 (has-language-title :type string))
)

(def-class get-lo-by-requester-request ()
((has-requester :type learner))
)



