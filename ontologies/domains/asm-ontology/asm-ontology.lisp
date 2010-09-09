;;; Mode: Lisp; Package: ocml

(in-package "OCML")

(in-ontology asm-ontology)

(def-class asm ()
  "The Abstract State Machine class"
  ((has-transition-rule           :type transition-rule)
   (has-state-signature           :type state-signature           :max-cardinality 1)
   (has-non-functional-properties :type non-functional-properties :max-cardinality 1)))

; (def-class transition-rules ()
;  "A collection of transitions rules for an ASM"
;  ((has-transition-rule  :type transition-rule)))

(def-class transition-rule ()
  "A single ASM transition rule")

(def-class if-rule (transition-rule)
  "A guarded transition rule"
  ((has-condition         :type string :cardinality 1)
   (has-transition-rule   :type transition-rule)))

(def-class forall-rule (transition-rule)
   "Perform the update rules for each variable binding satisfying the condition"
  ((with-condition      :type string          :cardinality 1) 
   (has-variable-list   :type variable-list   :cardinality 1)
   (has-transition-rule :type transition-rule :min-cardinality 1)))

(def-class choose-rule (transition-rule)
   "Chooose a group of variable bindings satisfying the condition with which to run the rules"
  ((with-condition      :type string          :cardinality 1) 
   (has-variable-list   :type variable-list   :cardinality 1)
   (has-transition-rule :type transition-rule :min-cardinality 1)))

(def-class variable-list (list)
  ((has-element-type :value string)))

(def-class update-rule (transition-rule)
  "A rule updating the state"
  ((has-fact :type string :cardinality 1)))

(def-class add-modifier (update-rule))
(def-class delete-modifier (update-rule))
(def-class update-modifier (update-rule))

(def-class piped-rules (transition-rule)
  "Piped transition rules"
  ((has-transition-rule :type transition-rule)))

(def-class state-signature ()
  "The state signature of the ASM"
  ((has-static     :type mode)
   (has-in         :type mode)
   (has-out        :type mode)
   (has-shared     :type mode)
   (has-controlled :type mode)))

(def-class mode ()
  ((has-grounding  :type string)))

;; Some test instances

(def-instance mondex-asm asm 
  ((has-transition-rule mondex-choose-rule)))

(def-instance mondex-choose-rule choose-rule
  ((with-condition "authentic(from) AND authentic(to) AND from neq to AND value leq balance(from)")
   (has-variable-list ("fail?" "value" "from" "to"))
   (has-transition-rule mondex-if-rule)))

(def-instance mondex-if-rule if-rule
  ((has-condition "NOT fail?")
   (has-transition-rule mondex-update-rule1 mondex-update-rule2)))

(def-instance mondex-update-rule1 update-modifier
  ((has-fact "balance(from) := balance(from) - value")))

(def-instance mondex-update-rule2 update-modifier
  ((has-fact "balance(to) := balance(to) + value")))


