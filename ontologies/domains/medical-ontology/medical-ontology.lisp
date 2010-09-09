;;; Mode: Lisp; Package: ocml

(in-package "OCML")

;;   File:       medical-ontology.ol
;;   Author:     Guus Schreiber (debugged and translated to ocmnl by Enrico Motta)
;;   Works With: Ontolingua V4.0
;;   History:    11/11/1996 (Created)

(in-ontology medical-ontology)

(def-class medical-ontology-object () (?object)
  "General class of all objects in the medical ontology")





(def-relation disorder-group-member (?group ?disorder)
  "Taxonomical relation between disorders"
  :constraint (and (disorder-group ?group)
                   (disorder ?disorder)))


(def-class disorder-stage (medical-ontology-object) (?stage)
  "A stage of a particular disorder. e.g. stages of a carcinoma"
  )

(def-relation has-stage (?disorder ?stage)
  "Relation connecting a disorder to potential stages of the disorder"
  :constraint (and (disorder-stage ?stage)
                   (disorder ?disorder)))
  

(def-relation stage-transition (?stage1 ?stage2)
  "Relation specifying the ordening of disorder stages. E.g. II -> III"
  :constraint (and (disorder-stage ?stage1)
                   (disorder-stage ?stage2)))

(def-class medical-variable (medical-ontology-object) (?variable)
  "Any observable or non-observable information item about a patient"
  :iff-def (or (observable ?variable)
               (medical-state ?variable)))

;;:AXIOM-CONSTRAINTS  
;;  (and (subclass-of disorder-group medical-ontology-object)
;;       (exhaustive-subclass-partition '(observable medical-state))))

(def-function variable.value-type (?variable) -> ?value-type
  "Function pointing to the value type of a variable, e.g number, string, etc."
  :constraint (medical-variable ?variable))

;;  :AXIOM-CONSTRAINTS 
;;  (domain variable.value-type variable))


(def-class medical-observable (medical-variable))


(def-function medical-observable.value (?observable) -> ?value
  "Function represetning a value of an medical-observable"
  :constraint (medical-observable ?observable))


(def-relation observation (?observed-value ?procedure ?agent ?time-stamp)
  "Relation for exprsssing a certain observation. OBSERVATION has four arguments: 
    1. the actual value observed, represented as a function
    2. the context in which the value is observed, i.e. a medical procedure such 
       as an anannemis or a certain blood test
    3. the observer: the person or machine that made the observation. See 
       See the 'organisation' ontology.
    4. a time stamp
  "
  :constraint (and (medical-observable.value ?observed-value)
                   (medical-procedure ?procedure)
                   (medical-agent ?agent)
                   (time-stamp ?time-stamp)))


(def-class medical-procedure (medical-ontology-object) (?procedure)
  "A class representing all medicallly-oriented activities."
 )

(def-class diagnostic-procedure (medical-procedure) (?procedure)
  "Procedures such as blood test, X-rays, etc."
  )

(def-class therapeutic-procedure (medical-procedure) (?procedure)
  "Procedures such as surgery and drug presriptions. 
   Some procedures may be both diagnostic and therapeutic."
  )


(def-class medical-state (medical-variable) (?state)
  )

(def-function medical-state.value (?state) -> ?value
  :constraint (medical-state ?state))


(def-relation classification (?disorder ?state)
  :constraint (and (disorder ?disorder)
                   (medical-state ?state)))


(def-relation state-abstraction (?state1 ?state2)
  :constraint (and (medical-state ?state1)
                   (medical-state ?state2)))


(def-relation medical-observable-abstraction (?state1 ?state2)
  :constraint (and (medical-observable-abstraction ?state1)
                   (medical-observable-abstraction ?state2)))
 

(def-relation dependency (?from ?to)
  "Any depenedncy between medical states on the one hand and medical states 
or medical-observables on the other hand. It is assumed that dependencies between 
medical-observables do not occur"
  :constraint (and (medical-state ?from)
                   (medical-variable ?to))
  :sufficient (or (state-dependency ?from ?to)
                  (medical-observable-dependency ?from ?to)))

(def-relation state-dependency (?from ?to)
  "A subtype of a dependency in which the dependent variable is also an 
internal state"
  :constraint (and (dependency ?from ?to)
                   (medical-state ?to)))

(def-relation medical-observable-dependency (?from ?to)
  "A subtype of a dependency in which the dependent variable is an medical-observable"
  :constraint (and (dependency ?from ?to)
                   (medical-observable ?to)))

  

