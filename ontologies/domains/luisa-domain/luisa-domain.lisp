;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology luisa-domain)



(def-class organizzazione ()
  ((direttore-generale :type persona)))

(def-instance ibm organizzazione)

(def-instance pippo organizzazione)


(def-class autorità ())

(def-instance livello6 autorità)

(def-instance livello7 autorità)


(def-class persona ()
  ((fa_parte_di :type organizzazione)
   (ha_autorità :type autorità)))

(def-instance Antonio persona
  ((fa_parte_di ibm)
   (ha_autorità livello7)))

(def-instance Chiara persona
  ((fa_parte_di pippo)
   (ha_autorità livello7)))


(def-relation direttore-generale (?o ?p)
  :iff-def (and (persona ?p)
                (organizzazione ?o)
                (ha_autorità ?p livello7)
                (fa_parte_di ?p ?o)))

(def-rule direttore-generale-azienda 
    ((direttore-generale ?y ?x)
     if
     (and (ha_autorità ?x livello7)
          (fa_parte_di ?x ?y))))


(setofall '?x `(direttore-generale 'pippo ?x))

(ask (direttore-generale pippo ?x))



(def-class competency ()
((has-competency-name :type string)
 (has-competency-id :type integer)))

(def-class competency-list (list) ?x
:constraint (exists ?e (and (element-of ?e ?x)
                            (competency ?e))))

(def-class language ()
((has-language-name :type string)
 (has-language-id :type integer)))

(def-class currency ()
((has-currency-name :type string)
(has-currency-code :type string)))

(def-class cost ()
((has-currencye :type currency)
(has-amount :type float)))

(def-class learning-object ()
((has-name :type string)
 (has-title :type string)
 (has-Part :type learning-object)
 (has-cost :type cost) 	
 (has-id :type integer)
 (has-competency :type competency)
 (has-required-competencies :type competency-list)
 (has-language :type language)
 (has-related-learningobjects :type learning-object-list)))

(def-class learning-object-list (list) ?x
:constraint (exists ?e (and (element-of ?e ?x)
                            (learning-object ?e))))

;;; results from the composed and the smart service!
(def-class processed-lo-results ()
((has-lo-list :type learning-object-list)
 (has-user-ability :type boolean)
 (has-missing-competencies :type competency-list)
 (has-learner :type learner)))

(def-class actor ()
((has-actor-name :type string)
 (has-actor-id :type integer)))



(def-class learner (actor) 
((has-competencies :type competency-list)
 (has-language :type language)
 (has-competency-demand :type competency-list)
 (has-learner-history :type learning-object-list)))

;;; a competency can not be part of the competencies AND and the competenecy-demands of a user.
;;; rule or relation, still wrong!

#|
(def-relation competency-gap (?l) 
:constraint (learner ?l)
             (has-competencies ?l ?lc)
             (has-competency-demand ?l ?lcd)
             (competency ?e)

:iff-def (not (exists ?e (and (element-of ?e ?lc)
                              (element-of ?e ?lcd)))))
|#


(def-class getLOByRequesterRequest () 
((has-requester :type learner)))

(def-class getLOByCompetencyRequest ()
((has-competency-request :type competency)))

(def-class getLOResponse ()
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


