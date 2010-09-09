;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology luisa-wsmo-descriptions)


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





(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-GOAL-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-GOAL
           (GOAL)
           ?GOAL
           ((HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-GOAL-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-MEDIATOR-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-MEDIATOR
           (WG-MEDIATOR)
           ?MEDIATOR
           ((HAS-SOURCE-COMPONENT
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-GOAL)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-MEDIATOR-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE
           (WEB-SERVICE)
           ?WEB-SERVICE
           ((HAS-CAPABILITY
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-W-S-CAPABILITY)
            (HAS-INTERFACE
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE)
            (USED-MEDIATOR :VALUE LUISA-GET-L-O-BY-COMPETENCY-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-W-S-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-W-S-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE LUISA-GET-L-O-BY-COMPETENCY-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-W-S-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING
             :VALUE
             ((GROUNDED-TO-LISP (NORMAL DUMMY-METHOD))))))

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
           (PROBLEM-SOLVING-PATTERN)
           NIL)

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE
           (INTERFACE)
           ?INTERFACE
           ((HAS-CHOREOGRAPHY
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
            (HAS-ORCHESTRATION
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE-ORCHESTRATION)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-PUBLISHER-INFORMATION
           (PUBLISHER-INFORMATION)
           ((HAS-ASSOCIATED-WEB-SERVICE-INTERFACE
             :VALUE
             LUISA-GET-L-O-BY-COMPETENCY-WEB-SERVICE-INTERFACE)
            (HAS-WEB-SERVICE-HOST :VALUE "137.108.25.167")
            (HAS-WEB-SERVICE-PORT :VALUE 3001)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/soap")))