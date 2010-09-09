;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology luisa-domain2)

(def-class competency ()
((has-competency-title :type string)
 (has-competency-id :type integer)
 (has-related-competencies :type competencies)
 (has-required-competencies :type competencies)))

(def-class competency-list (list) ?x
:constraint (exists ?e (and (element-of ?e ?x)
                            (competency ?e))))

(def-class competencies ()
 ((has-competencies :type competency-list)))

(def-class language ()
((has-language-title :type string)
 (has-language-id :type integer)))

(def-class currency ()
((has-currency-title :type string)
(has-currency-code :type string)))

(def-class cost ()
((has-currency :type currency)
(has-amount :type float)))

(def-class learning-object ()
((has-id :type integer)
 (has-title :type string)

;;;has to be of type learning-object-list in later versions
 (has-relation-haspart :type learning-object)
 (has-relation-partof :type learning-object)
 
 (has-url :type string)
 (has-cost :type cost)
 (has-competency :type competency)
 (has-required-competencies :type competencies)
 (has-language :type language)
 (has-approximate-learning-time :type integer)
 (has-related-learning-objects :type learning-objects)))

(def-class learning-object-list (list) ?x
:constraint (exists ?e (and (element-of ?e ?x)
                            (learning-object ?e))))

(def-class learning-objects ()
((has-learning-objects :type learning-object-list)))

;;; results from the composed and the smart service!
(def-class processed-lo-results ()
((has-lo-list :type learning-objects)
 (has-user-ability :type boolean)
 (has-missing-competencies :type competencies)
 (has-learner :type learner)))

(def-class actor ()
((has-actor-name :type string)
 (has-actor-id :type integer)))

(def-class learner (actor) 
((has-competencies :type competencies)
 (has-language :type language)
 (has-competency-demand :type competencies)
 (has-learner-history :type learning-objects)))


#|
following relations could probably be defined as ocml-defined axioms or wsml-relations using the axioms in the wsml file.
are aimed for use in the capability descriptions! correct?

(def-relation lolist-maps-competency (?lol ?c)
  :constraint
(exists ?e (and (element-of ?e ?lol)
                            (learning-object ?e)))

:sufficient
    (forall (?e) (lo-has-competency ?e ?c))) 

(def-relation lo-has-competency (?x ?c)
:constraint (and (learning-object ?x)
		  (competency ?c))

:sufficient (has-Competency ?x ?c))


then the following postcondition could be used within the WS capability:

/*
postcondition postGetLoByCompetency
     definedBy 
          ?post[hasOCMLLogicalExpression hasValue "(kappa (?goal) 
  (and
    (lolist-maps-competency ((wsmo-role-value ?goal 'lolist) (wsmo-role-value ?goal 'competency))) "] memberOf OCMLPostacondition. 
*/



|#


;;; a competency can not be part of the competencies AND and the competency-demands of a user.
;;; rule or relation, still wrong!

#|
(def-relation consistent-competencies (?l)
:constraint (and  (learner ?l)
             (has-competencies ?l ?lc)
             (has-competency-demand ?l ?lcd)
             (competency ?e))

:sufficient (not (exists ?e (and (element-of ?e ?lc)
                              (element-of ?e ?lcd)))))
|#


(def-class get-lo-by-requester-request () 
((has-requester :type learner)))

(def-class get-lo-by-competency-request ()
((has-competency-request :type competency)))

(def-class get-lo-response ()
((has-retrieved-learning-object-list :type learning-object-list)))


#|
(def-class simple-region (region) ?sr
  "A simple region is constituted of a single Monotone Polygons"
  :constraint (=(length (has-parts ?sr)) 1))

def-class location (set) ?l
  "A Location is a subset of a space"
  ((has-space :type Space)
   (has-points :type Point))
  :constraint (subset ?l (has-space ?l)))

(def-rule: suitable-items (?o)
( : constraint (and (order ?o)
                             (reason-of-order ?o ?r))
for-all ((item ?i)
             (if (list-suitable-items)
                  (= (used-for ?i) ?r))))


;;; Is this relation correct? --> check if an item fits a patient (if the user’s weight < = maximum weight supported by the item

(def-relation item-fits-user-weight (?I ?c)
 (:constraint (client ?c)
                    (item ?I)
                    (max-user-weight ?I ?mw)
                    (weight ?c ?cw)
 :iff-def (or (< ?wc ?mw)
                   (= ?wc ?mw))
))
    

;;; Is this relation correct? --> check if an item fits a patient purpose

(def-relation item-fits-order-purpose (?I ?o)
 (:constraint (order ?o)
                    (item ?I)
                    (reason-of-order ?o ?r)
                    (used-for ?i ?u)
 :iff-def (= ?r ?u)
))

(def-rule foo
(has-project ?x ?y)
then
(exec (tell (project-covered-by ?y ?x)))
(exec (output "has project ~S ~S" ?x ?y)))
    
|#


