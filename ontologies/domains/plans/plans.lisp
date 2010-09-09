;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*- 

;;; Plans ontology.
;;; Author Aldo Gangemi

 ;;; Translated from owl with OWL2OCML Translator (Author: Baldassarre Claudio)

(in-package "OCML")

(in-ontology Plans)



;-_-_-_-_-_-_-_-_-_-_-_-_-_- DEFINITION OF RELATIONS AND RULES-_-_-_-_-_-_-_-_-_-_-_-_-_-


(def-relation exit-condition-of (?x ?y)
   :iff-def (exit-condition ?y ?x)
   :avoid-infinite-loop t)

(def-relation exit-condition (?x ?y)
   :iff-def (exit-condition-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule exit-condition-of-predecessor
 ((exit-condition-of ?x ?y)
 if (predecessor ?x ?y)))

(def-relation influences (?x ?y)
   :iff-def (influenced-by ?y ?x)
   :avoid-infinite-loop t)

(def-relation influenced-by (?x ?y)
   :iff-def (influences ?y ?x)
   :avoid-infinite-loop t)

(def-rule influences-specific-constant-dependent
 ((influences ?x ?y)
 if (specific-constant-dependent ?x ?y)))

(def-relation contributes-to-achieving-of (?x ?y)
   :iff-def (achievable-through ?y ?x)
   :avoid-infinite-loop t)

(def-relation achievable-through (?x ?y)
   :iff-def (contributes-to-achieving-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule contributes-to-achieving-of-referenced-by
 ((contributes-to-achieving-of ?x ?y)
 if (referenced-by ?x ?y)))

(def-rule disposition-to-mediated-relation
 ((disposition-to ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation precondition (?x ?y)
   :iff-def (precondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation precondition-of (?x ?y)
   :iff-def (precondition ?y ?x)
   :avoid-infinite-loop t)

(def-rule precondition-mediated-relation
 ((precondition ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation iteration-interval-of (?x ?y)
   :iff-def (iteration-interval ?y ?x)
   :avoid-infinite-loop t)

(def-relation iteration-interval (?x ?y)
   :iff-def (iteration-interval-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule iteration-interval-of-mediated-relation-i
 ((iteration-interval-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-relation postcondition-of (?x ?y)
   :iff-def (postcondition ?y ?x)
   :avoid-infinite-loop t)

(def-relation postcondition (?x ?y)
   :iff-def (postcondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule postcondition-of-mediated-relation-i
 ((postcondition-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-relation task-postcondition-of (?x ?y)
   :iff-def (task-postcondition ?y ?x)
   :avoid-infinite-loop t)

(def-relation task-postcondition (?x ?y)
   :iff-def (task-postcondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule task-postcondition-of-mediated-relation-i
 ((task-postcondition-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-relation discards (?x ?y)
   :iff-def (discarded-within ?y ?x)
   :avoid-infinite-loop t)

(def-relation discarded-within (?x ?y)
   :iff-def (discards ?y ?x)
   :avoid-infinite-loop t)

(def-rule discards-d-uses
 ((discards ?x ?y)
 if (d-uses ?x ?y)))

(def-rule adopts-goal-adopts
 ((adopts-goal ?x ?y)
 if (adopts ?x ?y)))

(def-relation sibling-task (?x ?y)
   :iff-def (sibling-task ?y ?x)
   :avoid-infinite-loop t)

(def-relation sibling-task (?x ?y)
   :iff-def (sibling-task ?y ?x)
   :avoid-infinite-loop t)

(def-rule sibling-task-mediated-relation
 ((sibling-task ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation main-goal-of (?x ?y)
   :iff-def (main-goal ?y ?x)
   :avoid-infinite-loop t)

(def-relation main-goal (?x ?y)
   :iff-def (main-goal-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule main-goal-of-proper-part-of
 ((main-goal-of ?x ?y)
 if (proper-part-of ?x ?y)))

(def-rule adopts-plan-adopts
 ((adopts-plan ?x ?y)
 if (adopts ?x ?y)))

(def-relation postcondition (?x ?y)
   :iff-def (postcondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation postcondition-of (?x ?y)
   :iff-def (postcondition ?y ?x)
   :avoid-infinite-loop t)

(def-rule postcondition-mediated-relation
 ((postcondition ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation subgoal-of (?x ?y)
   :iff-def (subgoal ?y ?x)
   :avoid-infinite-loop t)

(def-relation subgoal (?x ?y)
   :iff-def (subgoal-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule subgoal-of-proper-part-of
 ((subgoal-of ?x ?y)
 if (proper-part-of ?x ?y)))

(def-relation achievable-through (?x ?y)
   :iff-def (contributes-to-achieving-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation contributes-to-achieving-of (?x ?y)
   :iff-def (achievable-through ?y ?x)
   :avoid-infinite-loop t)

(def-rule achievable-through-references
 ((achievable-through ?x ?y)
 if (references ?x ?y)))

(def-relation task-postcondition (?x ?y)
   :iff-def (task-postcondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation task-postcondition-of (?x ?y)
   :iff-def (task-postcondition ?y ?x)
   :avoid-infinite-loop t)

(def-rule task-postcondition-mediated-relation
 ((task-postcondition ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation iteration-interval (?x ?y)
   :iff-def (iteration-interval-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation iteration-interval-of (?x ?y)
   :iff-def (iteration-interval ?y ?x)
   :avoid-infinite-loop t)

(def-rule iteration-interval-mediated-relation
 ((iteration-interval ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation subgoal (?x ?y)
   :iff-def (subgoal-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation subgoal-of (?x ?y)
   :iff-def (subgoal ?y ?x)
   :avoid-infinite-loop t)

(def-rule subgoal-proper-part
 ((subgoal ?x ?y)
 if (proper-part ?x ?y)))

(def-relation influenced-by (?x ?y)
   :iff-def (influences ?y ?x)
   :avoid-infinite-loop t)

(def-relation influences (?x ?y)
   :iff-def (influenced-by ?y ?x)
   :avoid-infinite-loop t)

(def-rule influenced-by-specifically-constantly-dependent-on
 ((influenced-by ?x ?y)
 if (specifically-constantly-dependent-on ?x ?y)))

(def-relation discarded-within (?x ?y)
   :iff-def (discards ?y ?x)
   :avoid-infinite-loop t)

(def-relation discards (?x ?y)
   :iff-def (discarded-within ?y ?x)
   :avoid-infinite-loop t)

(def-rule discarded-within-d-used-by
 ((discarded-within ?x ?y)
 if (d-used-by ?x ?y)))

(def-relation main-goal (?x ?y)
   :iff-def (main-goal-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation main-goal-of (?x ?y)
   :iff-def (main-goal ?y ?x)
   :avoid-infinite-loop t)

(def-rule main-goal-proper-part
 ((main-goal ?x ?y)
 if (proper-part ?x ?y)))

(def-relation task-precondition (?x ?y)
   :iff-def (task-precondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation task-precondition-of (?x ?y)
   :iff-def (task-precondition ?y ?x)
   :avoid-infinite-loop t)

(def-rule task-precondition-mediated-relation
 ((task-precondition ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation task-precondition-of (?x ?y)
   :iff-def (task-precondition ?y ?x)
   :avoid-infinite-loop t)

(def-relation task-precondition (?x ?y)
   :iff-def (task-precondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule task-precondition-of-mediated-relation-i
 ((task-precondition-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-relation precondition-of (?x ?y)
   :iff-def (precondition ?y ?x)
   :avoid-infinite-loop t)

(def-relation precondition (?x ?y)
   :iff-def (precondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule precondition-of-mediated-relation-i
 ((precondition-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-relation exit-condition (?x ?y)
   :iff-def (exit-condition-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation exit-condition-of (?x ?y)
   :iff-def (exit-condition ?y ?x)
   :avoid-infinite-loop t)

(def-rule exit-condition-successor
 ((exit-condition ?x ?y)
 if (successor ?x ?y)))

(def-relation influenced-by (?x ?y)
   :iff-def (influences ?y ?x)
   :avoid-infinite-loop t)

(def-relation discards (?x ?y)
   :iff-def (discarded-within ?y ?x)
   :avoid-infinite-loop t)

(def-rule postcondition-of-mediated-relation-i
 ((postcondition-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-relation exit-condition (?x ?y)
   :iff-def (exit-condition-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation main-goal-of (?x ?y)
   :iff-def (main-goal ?y ?x)
   :avoid-infinite-loop t)

(def-relation postcondition (?x ?y)
   :iff-def (postcondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule influences-specific-constant-dependent
 ((influences ?x ?y)
 if (specific-constant-dependent ?x ?y)))

(def-relation precondition (?x ?y)
   :iff-def (precondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation exit-condition-of (?x ?y)
   :iff-def (exit-condition ?y ?x)
   :avoid-infinite-loop t)

(def-relation subgoal (?x ?y)
   :iff-def (subgoal-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule contributes-to-achieving-of-referenced-by
 ((contributes-to-achieving-of ?x ?y)
 if (referenced-by ?x ?y)))

(def-relation task-precondition (?x ?y)
   :iff-def (task-precondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule exit-condition-successor
 ((exit-condition ?x ?y)
 if (successor ?x ?y)))

(def-rule iteration-interval-mediated-relation
 ((iteration-interval ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation subgoal-of (?x ?y)
   :iff-def (subgoal ?y ?x)
   :avoid-infinite-loop t)

(def-rule task-precondition-of-mediated-relation-i
 ((task-precondition-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-relation discarded-within (?x ?y)
   :iff-def (discards ?y ?x)
   :avoid-infinite-loop t)

(def-rule task-postcondition-mediated-relation
 ((task-postcondition ?x ?y)
 if (mediated-relation ?x ?y)))

(def-rule main-goal-of-proper-part-of
 ((main-goal-of ?x ?y)
 if (proper-part-of ?x ?y)))

(def-rule subgoal-proper-part
 ((subgoal ?x ?y)
 if (proper-part ?x ?y)))

(def-relation task-postcondition-of (?x ?y)
   :iff-def (task-postcondition ?y ?x)
   :avoid-infinite-loop t)

(def-relation achievable-through (?x ?y)
   :iff-def (contributes-to-achieving-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation main-goal (?x ?y)
   :iff-def (main-goal-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation task-postcondition (?x ?y)
   :iff-def (task-postcondition-of ?y ?x)
   :avoid-infinite-loop t)

(def-rule iteration-interval-of-mediated-relation-i
 ((iteration-interval-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-rule discards-d-uses
 ((discards ?x ?y)
 if (d-uses ?x ?y)))

(def-relation contributes-to-achieving-of (?x ?y)
   :iff-def (achievable-through ?y ?x)
   :avoid-infinite-loop t)

(def-rule postcondition-mediated-relation
 ((postcondition ?x ?y)
 if (mediated-relation ?x ?y)))

(def-rule sibling-task-mediated-relation
 ((sibling-task ?x ?y)
 if (mediated-relation ?x ?y)))

(def-rule disposition-to-mediated-relation
 ((disposition-to ?x ?y)
 if (mediated-relation ?x ?y)))

(def-rule influenced-by-specifically-constantly-dependent-on
 ((influenced-by ?x ?y)
 if (specifically-constantly-dependent-on ?x ?y)))

(def-rule task-postcondition-of-mediated-relation-i
 ((task-postcondition-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-rule task-precondition-mediated-relation
 ((task-precondition ?x ?y)
 if (mediated-relation ?x ?y)))

(def-rule discarded-within-d-used-by
 ((discarded-within ?x ?y)
 if (d-used-by ?x ?y)))

(def-relation precondition-of (?x ?y)
   :iff-def (precondition ?y ?x)
   :avoid-infinite-loop t)

(def-relation postcondition-of (?x ?y)
   :iff-def (postcondition ?y ?x)
   :avoid-infinite-loop t)

(def-rule exit-condition-of-predecessor
 ((exit-condition-of ?x ?y)
 if (predecessor ?x ?y)))

(def-rule achievable-through-references
 ((achievable-through ?x ?y)
 if (references ?x ?y)))

(def-rule adopts-plan-adopts
 ((adopts-plan ?x ?y)
 if (adopts ?x ?y)))

(def-rule precondition-mediated-relation
 ((precondition ?x ?y)
 if (mediated-relation ?x ?y)))

(def-relation iteration-interval-of (?x ?y)
   :iff-def (iteration-interval ?y ?x)
   :avoid-infinite-loop t)

(def-relation iteration-interval (?x ?y)
   :iff-def (iteration-interval-of ?y ?x)
   :avoid-infinite-loop t)

(def-relation task-precondition-of (?x ?y)
   :iff-def (task-precondition ?y ?x)
   :avoid-infinite-loop t)

(def-rule adopts-goal-adopts
 ((adopts-goal ?x ?y)
 if (adopts ?x ?y)))

(def-relation sibling-task (?x ?y)
   :iff-def (sibling-task ?y ?x)
   :avoid-infinite-loop t)

(def-rule precondition-of-mediated-relation-i
 ((precondition-of ?x ?y)
 if (mediated-relation-i ?x ?y)))

(def-rule subgoal-of-proper-part-of
 ((subgoal-of ?x ?y)
 if (proper-part-of ?x ?y)))

(def-rule main-goal-proper-part
 ((main-goal ?x ?y)
 if (proper-part ?x ?y)))

(def-relation influences (?x ?y)
   :iff-def (influenced-by ?y ?x)
   :avoid-infinite-loop t)



;-_-_-_-_-_-_-_-_-_-_-_-_-_- DEFINITION OF ANONYMOUS ITEMS-_-_-_-_-_-_-_-_-_-_-_-_-_-



;-_-_-_-_-_-_-_-_-_-_-_-_-_- DEFINITION OF CLASSES-_-_-_-_-_-_-_-_-_-_-_-_-_-


(def-class Thing () 
 "Comment Here"
)

(def-class saturated-plan (plan) ?e
"A saturated plan is a plan that cannot be executed twice, since it defines spatio-temporal parameters restricted to one value, e.g. one of its tasks classifies an event that is valued by a definite temporal value in a definite space region.Of course, in the case of maximal spatio-temporal regions, a saturated plan tends to approximate an abstract plan from the execution viewpoint, but these worst cases are unavoidable when dealing with maximality."
((d-uses :min-cardinality 1 :min-cardinality 1)
(valued-by :min-cardinality 1 :min-cardinality 1 :min-cardinality 1 :min-cardinality 1)
)
:iff-def   ( and ( plan ?e   )
( exists ?b
 (and (valued-by ?e ?b)
	(and ( exists ?a (and (valued-by ?e ?a)	(space-region ?e)) )( parameter ?e   )))
 )
( exists ?d
 (and (valued-by ?e ?d)
	(and ( exists ?c (and (valued-by ?e ?c)	(time-interval ?e)) )( parameter ?e   )))
 )
)

:avoid-infinite-loop t
 )

(def-class elementary-task (task) ?a
"An atomic task."
((component :cardinality 0)
)
:iff-def   ( and ( task ?a   )
)

:avoid-infinite-loop t
 )

(def-class control-task (elementary-task) ?b
"A control task is an elementary task that sequences a planning activity, e.g. an activity aimed at (cognitively or via simulation) anticipating other activities. Therefore, control tasks have usually at least one direct successor task (the controlled one), with the exception of ending tasks.The reification of control constructs allows to represent procedural knowledge into the same ontology including controlled action. Besides conceptual transparency and independency from a particular grounding system, a further advantage is enabling the representation of coordination tasks. For example, a manager that coordinates the execution of several related activities can be represented as a role with a responsibility (duty+right) towards some complex task."
:iff-def   ( and ( elementary-task ?b   )
( exists ?a
 (and (sequences ?b ?a)
	(or ( planning-activity ?a  )( decision-state ?a  )))
 )
)

:avoid-infinite-loop t
 :constraint  ( (not ( action-task ?b ))
)
)

(def-class plan-execution (situation) ?b
"Plan executions are situations that proactively satisfy a plan (cf. definition of P-SAT above). Subplan executions are proper parts of the whole plan execution."
((p-sat :min-cardinality 1)
)
:iff-def   ( and ( situation ?b   )
( exists ?a
 (and (p-sat ?b ?a)
	(plan ?b))
 )
)

:avoid-infinite-loop t
 )

(def-class decision-state (state) ?c
"A state related to planning. It is sequenced by 'deliberation task', and is preceded by a decision activity."
((follows :min-cardinality 1)
(sequenced-by :min-cardinality 1 :min-cardinality 1)
)
:iff-def   ( and ( state ?c   )
( exists ?a
 (and (sequenced-by ?c ?a)
	(member ?c(deliberation-task)))
 )
( exists ?b
 (and (follows ?c ?b)
	(decision-activity ?c))
 )
)

:avoid-infinite-loop t
 )

(def-class subplan (plan) ?a
"A proper part of a plan."
((proper-part-of :min-cardinality 1)
)
:iff-def   ( and ( plan ?a   )
( exists ?a
 (and (proper-part-of ?a ?a)
	(plan ?a))
 )
)

:avoid-infinite-loop t
 )

(def-class circumstantial-plan (plan) ?c
"A circumstantial plan has all components classifying named individuals from the ground ontology (e.g. only specific persons, specified resources, a finite number of time intervals and space regions, etc.).This condition cannot be formalized in FOL, since we would like to express a condition by which an instance of an circumstantial plan specifies both instances of plan components, and instances of situation elements, e.g. that 'manager' classifies a specified (named) person."
((classifies :min-cardinality 1)
)
:iff-def   ( and ( plan ?c   )
( exists ?b
 (and (classifies ?c ?b)
	(particular ?band ( exists ?a (and (classifies ?b ?a)	(particular ?b)) )( concept ?b   )))
 )
)

:avoid-infinite-loop t
 )

(def-class planning-as-technique (technique) 
 "The technique by which a planning process can be carried out."
)

(def-class goal-qua-main (goal) ?a
"A main goal can be defined as a goal that is part of a plan but not of one of its subplans. The characteristic axiom cannot be formalized in OWL-DL (it requires coreference)."
((main-goal-of :min-cardinality 1)
)
:iff-def   ( and ( goal ?a   )
( exists ?a
 (and (main-goal-of ?a ?a)
	(plan ?a))
 )
)

:avoid-infinite-loop t
 )

(def-class goal-situation (situation) ?b
"A goal situation is a situation that satisfies a goal.Opposite to the case of subplan executions, a goal situation is not part of a plan execution.In other words, it is not true in general that any situation satisfying a part of a description, is also part of the situation that satisfies the whole description. This helps to account for the following cases: a) Execution of plans containing abort or suspension conditions (the plan would be satisfied even if the goal has not been reached, see below), b) Incidental satisfaction, like when a situation satisfies a goal without being intentionally planned (but anyway desired)."
((satisfies :min-cardinality 1)
)
:iff-def   ( and ( situation ?b   )
( exists ?a
 (and (satisfies ?b ?a)
	(goal ?b))
 )
)

:avoid-infinite-loop t
 )

(def-class plan-assessment (technique) ?b
 "A technique to evaluate a plan execution."
((has-in-scope :min-cardinality 1)
)
:iff-def  (exists ?a
 (and (has-in-scope ?b ?a)
	(plan-execution ?a)))

)

(def-class complex-task (task) ?b
"A task that has at least two other tasks as components."
((component :min-cardinality 1 :min-cardinality 2)
)
:iff-def   ( and ( task ?b   )
( exists ?a
 (and (component ?b ?a)
	(task ?b))
 )
)

:avoid-infinite-loop t
 )

(def-class maximal-task (complex-task) 
 "A maximal task is a complex task that has all the tasks defined in a plan as components.In OWL-DL the axiom is defined as a concept axiom over plan component task."
)

(def-class information-gathering (activity) 
 "An activity aimed at gathering information for some purpose. It is typically sequenced by case tasks for taking decisions (can be part of decision activities)."
)

(def-class sequential-task (complex-task) ?c
 "A sequential task is a complex task that includes a successor relation among any two component tasks, and does not contain any control task.The first condition cannot be stated in OWL-DL, because it needs coreference."
((component :min-cardinality 1 :min-cardinality 2)
)
:iff-def (and  (exists ?a
 (and (component ?c ?a)
	(action-task ?a)))
 (exists ?b
 (and (component ?c ?b)
	(not ( control-task ?b  ))))
)
)

(def-class schedule (task) ?a
"A scheduling is a task that cannot be executed twice, since it has a temporal parameter restricted to one value, e.g. it classifies an event that is valued by a definite temporal value."
((requisite :min-cardinality 1)
(valued-by :min-cardinality 1 :min-cardinality 1)
)
:iff-def   ( and ( task ?a   )
( exists ?d
 (and (valued-by ?a ?d)
	(and ( exists ?c (and (valued-by ?a ?c)	(time-interval ?a)) )( parameter ?a   )))
 )
)

:avoid-infinite-loop t
 )

(def-class planning-activity (activity) ?b
 "The activity to generate a plan."
((expected-by :min-cardinality 1)
(product :type plan)
)
:iff-def  (exists ?a
 (and (expected-by ?b ?a)
	(planning-as-technique ?a)))

)

(def-class abstract-plan (plan) 
 "An abstract plan is a plan whose roles and tasks only specify classes of entities that can be included in a plan execution. In other words, a component from an abstract plan does not classify any named entity. This condition cannot be formalized in FOL, since we would like to express a condition by which an instance of an abstract plan specifies instances of plan components, but no instances of situation elements, e.g. that 'manager' classifies some (if any) instance of person, but not a specified (named) person."
)

(def-class decision-activity (activity) ?b
 "An activity related to planning. It is sequenced by 'case task', and can contain an information gathering activity."
((sequenced-by :min-cardinality 1 :min-cardinality 1)
)
:iff-def  (exists ?a
 (and (sequenced-by ?b ?a)
	(member ?a(case-task))))

)

(def-class plan-assessment-task (control-task) ?b
"A task defined in a plan assessment."
((defined-by :min-cardinality 1)
)
:iff-def   ( and ( control-task ?b   )
( exists ?a
 (and (defined-by ?b ?a)
	(plan-assessment ?b))
 )
)

:avoid-infinite-loop t
 )

(def-class bag-task (complex-task) ?a
 "A bag task is a complex task that does not include either a control task, or a successor relation among any two component tasks.The last condition cannot be stated in OWL-DL, because it needs a coreference."
:iff-def  (exists ?b
 (and (component ?a ?b)
	(not ( control-task ?b  ))))

)

(def-class action-task (elementary-task) ?a
 "An action task is an elementary task that sequences non-planning activities, like: moving, exercising forces, gathering information, etc. Planning activites are mental events involving some rational event."
:constraint  ( (not ( control-task ?a ))
)
)



;-_-_-_-_-_-_-_-_-_-_-_-_-_- DEFINITION OF INDIVIDUALS-_-_-_-_-_-_-_-_-_-_-_-_-_-


(def-class O2OInstanceClass-No.1 () ?x
((direct-predecessor :min-cardinality 1)
)
:iff-def (and (control-task ?x)
 (exists ?a
 (and (direct-predecessor ?x ?a)
	(member ?a(readiness-task))))
)
)

(def-instance activation-task O2OInstanceClass-No.1)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.2 () ?x
((direct-successor :min-cardinality 1)
)
:iff-def (and  (exists ?b
 (and (sequences ?x ?b)
	(decision-activity ?b)))
(control-task ?x)
 (exists ?a
 (and (direct-successor ?x ?a)
	(member ?a(deliberation-task))))
)
)

(def-instance case-task O2OInstanceClass-No.2)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.3 () ?x
((direct-predecessor :min-cardinality 1)
)
:iff-def (and (control-task ?x)
 (exists ?b
 (and (direct-predecessor ?x ?b)
	(and ( or ( action-task ?b  )( complex-task ?b  ))( exists ?a (and (direct-predecessor ?b ?a)	(member ?b(any-order-task concurrency-task))) ))))
)
)

(def-instance synchro-task O2OInstanceClass-No.3)

;-----------------------------------------------------------------------Instance

(def-instance completion-task control-task)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.5 () ?x
((direct-successor :min-cardinality 1)
)
:iff-def (and  (exists ?a
 (and (direct-successor ?x ?a)
	(and ( task ?a   )( not (nullmember ?a(case-task))))))
 (exists ?a
 (and (direct-successor ?x ?a)
	(member ?a(case-task))))
(control-task ?x)
)
)

(def-instance partly-case-task O2OInstanceClass-No.5)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.6 () ?x
((predecessor :min-cardinality 1)
)
:iff-def (and  (exists ?a
 (and (predecessor ?x ?a)
	(task ?a)))
(control-task ?x)
)
)

(def-instance ending-task O2OInstanceClass-No.6)

;-----------------------------------------------------------------------Instance

(def-instance loop-task control-task)

;-----------------------------------------------------------------------Instance

(def-instance suspension-task control-task)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.9 () ?x
(;(direct-successor :min-cardinality 1)
 (direct-successor :min-cardinality 2)
)
:iff-def (and  (exists ?a
 (and (direct-successor ?x ?a)
	(task ?a)))
 (exists ?a
 (and (sequences ?x ?a)
	(planning-activity ?a)))
(control-task ?x)
)
)

(def-instance branching-task O2OInstanceClass-No.9)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.10 () ?x
(;(direct-successor :min-cardinality 1)
 (direct-successor :cardinality 3)
)
:iff-def (and  (exists ?a
 (and (direct-successor ?x ?a)
	(member ?a(deliberation-task))))
(control-task ?x)
 (exists ?b
 (and (sequences ?x ?b)
	(decision-activity ?b)))
)
)

(def-instance alternate-task O2OInstanceClass-No.10)

;-----------------------------------------------------------------------Instance

(def-instance loop-for control-task)

;-----------------------------------------------------------------------Instance

(def-instance abortion-task control-task)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.13 () ?x
((successor :min-cardinality 1)
 (direct-successor :min-cardinality 2)
)
:iff-def (and  (exists ?a
 (and (successor ?x ?a)
	(member ?a(synchro-task))))
(control-task ?x)
)
)

(def-instance parallel-task O2OInstanceClass-No.13)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.14 () ?x
((direct-predecessor :min-cardinality 1)
)
:iff-def (and  (exists ?a
 (and (direct-predecessor ?x ?a)
	(member ?a(suspension-task))))
(control-task ?x)
)
)

(def-instance reactivation-task O2OInstanceClass-No.14)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.15 () ?x
((exit-condition :min-cardinality 1)
)
:iff-def (and  (exists ?a
 (and (exit-condition ?x ?a)
	(member ?a(deliberation-task))))
(control-task ?x)
)
)

(def-instance loop-until O2OInstanceClass-No.15)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.16 () ?x

:iff-def (and  (exists ?a
 (and (sequences ?x ?a)
	(decision-state ?a)))
(control-task ?x)
)
)

(def-instance deliberation-task O2OInstanceClass-No.16)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.17 () ?x
((successor :min-cardinality 1)
)
:iff-def (and (control-task ?x)
 (exists ?a
 (and (successor ?x ?a)
	(member ?a(synchro-task))))
)
)

(def-instance any-order-task O2OInstanceClass-No.17)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.18 () ?x
((successor :min-cardinality 1)
 (direct-successor :min-cardinality 2)
;(direct-successor :min-cardinality 1)
(temporally-overlaps :min-cardinality 1)
)
:iff-def (and (control-task ?x)
 (exists ?a
 (and (successor ?x ?a)
	(member ?a(synchro-task))))
 (exists ?c
 (and (temporally-overlaps ?x ?c)
	(and ( task ?c   )( exists ?b (and (temporally-overlaps ?c ?b)	(activity ?band ( activity ?b   )( exists ?a (and (temporally-overlaps ?b ?a)	(activity ?b)) ))) ))))
)
)

(def-instance concurrency-task O2OInstanceClass-No.18)

;-----------------------------------------------------------------------Instance

(def-instance abandonment-task control-task)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.20 () ?x
((successor :min-cardinality 1)
)
:iff-def (and (control-task ?x)
 (exists ?a
 (and (successor ?x ?a)
	(task ?a)))
)
)

(def-instance beginning-task O2OInstanceClass-No.20)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.21 () ?x
((predecessor :min-cardinality 1)
)
:iff-def (and  (exists ?a
 (and (predecessor ?x ?a)
	(member ?a(consideredness-task))))
(plan-assessment-task ?x)
)
)

(def-instance rejectedness-task O2OInstanceClass-No.21)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.22 () ?x
((direct-predecessor :min-cardinality 1)
)
:iff-def (and  (exists ?a
 (and (direct-predecessor ?x ?a)
	(member ?a(preparedness-task))))
(plan-assessment-task ?x)
)
)

(def-instance readiness-task O2OInstanceClass-No.22)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.23 () ?x
((direct-predecessor :min-cardinality 1)
)
:iff-def (and (plan-assessment-task ?x)
 (exists ?a
 (and (direct-predecessor ?x ?a)
	(member ?a(possibility-task))))
)
)

(def-instance consideredness-task O2OInstanceClass-No.23)

;-----------------------------------------------------------------------Instance

(def-instance possibility-task plan-assessment-task)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.25 () ?x
((predecessor :min-cardinality 1)
)
:iff-def (and  (exists ?a
 (and (predecessor ?x ?a)
	(member ?a(consideredness-task))))
(plan-assessment-task ?x)
)
)

(def-instance acceptation-task O2OInstanceClass-No.25)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.26 () ?x
((direct-predecessor :min-cardinality 1)
)
:iff-def (and  (exists ?a
 (and (direct-predecessor ?x ?a)
	(member ?a(acceptation-task))))
(plan-assessment-task ?x)
)
)

(def-instance decidedness-task O2OInstanceClass-No.26)

;-----------------------------------------------------------------------Instance

(def-class O2OInstanceClass-No.27 () ?x
((direct-predecessor :min-cardinality 1)
)
:iff-def (and  (exists ?a
 (and (direct-predecessor ?x ?a)
	(member ?a(decidedness-task))))
(plan-assessment-task ?x)
)
)

(def-instance preparedness-task O2OInstanceClass-No.27)

;-----------------------------------------------------------------------Instance



;-_-_-_-_-_-_-_-_-_-_-_-_-_- DEFINITION OF AXIOMS-_-_-_-_-_-_-_-_-_-_-_-_-_-


(def-axiom Axiom-exit-condition-of-exit-condition
   (Inverse exit-condition-of exit-condition ))

(def-axiom Axiom-exit-condition-of-predecessor
(subrelation-of exit-condition-of predecessor))

(def-axiom Axiom-influences-influenced-by
   (Inverse influences influenced-by ))

(def-axiom Axiom-influences-specific-constant-dependent
(subrelation-of influences specific-constant-dependent))

(def-axiom Axiom-contributes-to-achieving-of-achievable-through
   (Inverse contributes-to-achieving-of achievable-through ))

(def-axiom Axiom-contributes-to-achieving-of-referenced-by
(subrelation-of contributes-to-achieving-of referenced-by))

(def-axiom Axiom-disposition-to-mediated-relation
(subrelation-of disposition-to mediated-relation))

(def-axiom Axiom-precondition-precondition-of
   (Inverse precondition precondition-of ))

(def-axiom Axiom-precondition-mediated-relation
(subrelation-of precondition mediated-relation))

(def-axiom Axiom-iteration-interval-of-iteration-interval
   (Inverse iteration-interval-of iteration-interval ))

(def-axiom Axiom-iteration-interval-of-mediated-relation-i
(subrelation-of iteration-interval-of mediated-relation-i))

(def-axiom Axiom-postcondition-of-postcondition
   (Inverse postcondition-of postcondition ))

(def-axiom Axiom-postcondition-of-mediated-relation-i
(subrelation-of postcondition-of mediated-relation-i))

(def-axiom Axiom-task-postcondition-of-task-postcondition
   (Inverse task-postcondition-of task-postcondition ))

(def-axiom Axiom-task-postcondition-of-mediated-relation-i
(subrelation-of task-postcondition-of mediated-relation-i))

(def-axiom Axiom-discards-discarded-within
   (Inverse discards discarded-within ))

(def-axiom Axiom-discards-d-uses
(subrelation-of discards d-uses))

(def-axiom Axiom-adopts-goal-adopts
(subrelation-of adopts-goal adopts))

(def-axiom Axiom-sibling-task-sibling-task
   (Inverse sibling-task sibling-task ))

(def-axiom Axiom-sibling-task-mediated-relation
(subrelation-of sibling-task mediated-relation))

(def-axiom Axiom-main-goal-of-main-goal
   (Inverse main-goal-of main-goal ))

(def-axiom Axiom-main-goal-of-proper-part-of
(subrelation-of main-goal-of proper-part-of))

(def-axiom Axiom-adopts-plan-adopts
(subrelation-of adopts-plan adopts))

(def-axiom Axiom-postcondition-postcondition-of
   (Inverse postcondition postcondition-of ))

(def-axiom Axiom-postcondition-mediated-relation
(subrelation-of postcondition mediated-relation))

(def-axiom Axiom-subgoal-of-subgoal
   (Inverse subgoal-of subgoal ))

(def-axiom Axiom-subgoal-of-proper-part-of
(subrelation-of subgoal-of proper-part-of))

(def-axiom Axiom-achievable-through-contributes-to-achieving-of
   (Inverse achievable-through contributes-to-achieving-of ))

(def-axiom Axiom-achievable-through-references
(subrelation-of achievable-through references))

(def-axiom Axiom-task-postcondition-task-postcondition-of
   (Inverse task-postcondition task-postcondition-of ))

(def-axiom Axiom-task-postcondition-mediated-relation
(subrelation-of task-postcondition mediated-relation))

(def-axiom Axiom-iteration-interval-iteration-interval-of
   (Inverse iteration-interval iteration-interval-of ))

(def-axiom Axiom-iteration-interval-mediated-relation
(subrelation-of iteration-interval mediated-relation))

(def-axiom Axiom-subgoal-subgoal-of
   (Inverse subgoal subgoal-of ))

(def-axiom Axiom-subgoal-proper-part
(subrelation-of subgoal proper-part))

(def-axiom Axiom-influenced-by-influences
   (Inverse influenced-by influences ))

(def-axiom Axiom-influenced-by-specifically-constantly-dependent-on
(subrelation-of influenced-by specifically-constantly-dependent-on))

(def-axiom Axiom-discarded-within-discards
   (Inverse discarded-within discards ))

(def-axiom Axiom-discarded-within-d-used-by
(subrelation-of discarded-within d-used-by))

(def-axiom Axiom-main-goal-main-goal-of
   (Inverse main-goal main-goal-of ))

(def-axiom Axiom-main-goal-proper-part
(subrelation-of main-goal proper-part))

(def-axiom Axiom-task-precondition-task-precondition-of
   (Inverse task-precondition task-precondition-of ))

(def-axiom Axiom-task-precondition-mediated-relation
(subrelation-of task-precondition mediated-relation))

(def-axiom Axiom-task-precondition-of-task-precondition
   (Inverse task-precondition-of task-precondition ))

(def-axiom Axiom-task-precondition-of-mediated-relation-i
(subrelation-of task-precondition-of mediated-relation-i))

(def-axiom Axiom-precondition-of-precondition
   (Inverse precondition-of precondition ))

(def-axiom Axiom-precondition-of-mediated-relation-i
(subrelation-of precondition-of mediated-relation-i))

(def-axiom Axiom-exit-condition-exit-condition-of
   (Inverse exit-condition exit-condition-of ))

(def-axiom Axiom-exit-condition-successor
(subrelation-of exit-condition successor))

(def-axiom axiom-control-task-action-task
   (disjoint control-task action-task))

(def-axiom axiom-action-task-control-task
   (disjoint action-task control-task))

(def-axiom Axiom-iteration-interval-iteration-interval-of
   (Inverse iteration-interval iteration-interval-of ))

(def-axiom Axiom-postcondition-mediated-relation
(subrelation-of postcondition mediated-relation))

(def-axiom Axiom-precondition-precondition-of
   (Inverse precondition precondition-of ))

(def-axiom Axiom-influences-specific-constant-dependent
(subrelation-of influences specific-constant-dependent))

(def-axiom Axiom-exit-condition-exit-condition-of
   (Inverse exit-condition exit-condition-of ))

(def-axiom Axiom-task-postcondition-mediated-relation
(subrelation-of task-postcondition mediated-relation))

(def-axiom Axiom-sibling-task-sibling-task
   (Inverse sibling-task sibling-task ))

(def-axiom Axiom-iteration-interval-of-mediated-relation-i
(subrelation-of iteration-interval-of mediated-relation-i))

(def-axiom Axiom-subgoal-subgoal-of
   (Inverse subgoal subgoal-of ))

(def-axiom Axiom-postcondition-of-mediated-relation-i
(subrelation-of postcondition-of mediated-relation-i))

(def-axiom Axiom-contributes-to-achieving-of-referenced-by
(subrelation-of contributes-to-achieving-of referenced-by))

(def-axiom Axiom-precondition-of-mediated-relation-i
(subrelation-of precondition-of mediated-relation-i))

(def-axiom axiom-control-task-action-task
   (disjoint control-task action-task))

(def-axiom Axiom-task-postcondition-of-task-postcondition
   (Inverse task-postcondition-of task-postcondition ))

(def-axiom Axiom-main-goal-of-proper-part-of
(subrelation-of main-goal-of proper-part-of))

(def-axiom Axiom-iteration-interval-mediated-relation
(subrelation-of iteration-interval mediated-relation))

(def-axiom Axiom-task-precondition-of-mediated-relation-i
(subrelation-of task-precondition-of mediated-relation-i))

(def-axiom Axiom-task-precondition-of-task-precondition
   (Inverse task-precondition-of task-precondition ))

(def-axiom Axiom-subgoal-proper-part
(subrelation-of subgoal proper-part))

(def-axiom Axiom-task-precondition-task-precondition-of
   (Inverse task-precondition task-precondition-of ))

(def-axiom Axiom-exit-condition-of-predecessor
(subrelation-of exit-condition-of predecessor))

(def-axiom Axiom-influenced-by-influences
   (Inverse influenced-by influences ))

(def-axiom Axiom-discarded-within-d-used-by
(subrelation-of discarded-within d-used-by))

(def-axiom Axiom-subgoal-of-subgoal
   (Inverse subgoal-of subgoal ))

(def-axiom Axiom-task-postcondition-of-mediated-relation-i
(subrelation-of task-postcondition-of mediated-relation-i))

(def-axiom Axiom-iteration-interval-of-iteration-interval
   (Inverse iteration-interval-of iteration-interval ))

(def-axiom Axiom-achievable-through-contributes-to-achieving-of
   (Inverse achievable-through contributes-to-achieving-of ))

(def-axiom Axiom-adopts-plan-adopts
(subrelation-of adopts-plan adopts))

(def-axiom Axiom-contributes-to-achieving-of-achievable-through
   (Inverse contributes-to-achieving-of achievable-through ))

(def-axiom Axiom-discarded-within-discards
   (Inverse discarded-within discards ))

(def-axiom Axiom-sibling-task-mediated-relation
(subrelation-of sibling-task mediated-relation))

(def-axiom Axiom-adopts-goal-adopts
(subrelation-of adopts-goal adopts))

(def-axiom Axiom-postcondition-postcondition-of
   (Inverse postcondition postcondition-of ))

(def-axiom Axiom-exit-condition-successor
(subrelation-of exit-condition successor))

(def-axiom Axiom-exit-condition-of-exit-condition
   (Inverse exit-condition-of exit-condition ))

(def-axiom Axiom-influenced-by-specifically-constantly-dependent-on
(subrelation-of influenced-by specifically-constantly-dependent-on))

(def-axiom Axiom-main-goal-proper-part
(subrelation-of main-goal proper-part))

(def-axiom Axiom-achievable-through-references
(subrelation-of achievable-through references))

(def-axiom Axiom-postcondition-of-postcondition
   (Inverse postcondition-of postcondition ))

(def-axiom Axiom-discards-discarded-within
   (Inverse discards discarded-within ))

(def-axiom Axiom-subgoal-of-proper-part-of
(subrelation-of subgoal-of proper-part-of))

(def-axiom axiom-action-task-control-task
   (disjoint action-task control-task))

(def-axiom Axiom-influences-influenced-by
   (Inverse influences influenced-by ))

(def-axiom Axiom-discards-d-uses
(subrelation-of discards d-uses))

(def-axiom Axiom-disposition-to-mediated-relation
(subrelation-of disposition-to mediated-relation))

(def-axiom Axiom-task-precondition-mediated-relation
(subrelation-of task-precondition mediated-relation))

(def-axiom Axiom-task-postcondition-task-postcondition-of
   (Inverse task-postcondition task-postcondition-of ))

(def-axiom Axiom-main-goal-of-main-goal
   (Inverse main-goal-of main-goal ))

(def-axiom Axiom-main-goal-main-goal-of
   (Inverse main-goal main-goal-of ))

(def-axiom Axiom-precondition-of-precondition
   (Inverse precondition-of precondition ))

(def-axiom Axiom-precondition-mediated-relation
(subrelation-of precondition mediated-relation))

