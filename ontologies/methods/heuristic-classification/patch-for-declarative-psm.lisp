;;;;;;;; A declarative method for classification task ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; put the following psm  in the file heuristic-classifier.lisp

(def-class declarative-heuristic-optimal-sol-classifier (problem-solving-method) ?psm
  "This is a declarative method for finding an admissible solution to a
   classification problem in the sence that the body of the PSM is developed based on the 
   goal expressions defined in the task which solved. The method first does abstraction 
   by evaluatingn the goal-expression in the subtask abstraction, then finds the virtual 
   slution space and finally finds solutions by evaluating the goal expression of the task
   optimal-classification-task, As a siude effect, we also check assumptions in the other
   PSM. "
  
  ((has-input-role :value has-abstractors
                   :value has-refiners
                   :value has-observables
                   :value has-candidate-solutions
                   :value has-solution-exclusion-criterion
                   )
   
   (has-output-role :value has-solution)
   (has-abstractors :type abstractors)
   (has-refiners :type refiners)
   (has-observables :type observables)
   (has-solution-exclusion-criterion :type candidate-exclusion-criterion
                            :default-value default-candidate-exclusion-criterion)
   (has-candidate-solutions :type solution-space)
   (has-solution :type solution)
   (has-assumption 
    :value assumption1
    :value assumption2
    :value assumption3
    :value assumption4
    :documentation "There are 4 assumptions associated with this method.  The first
                    two state that the abstraction and refinement hierarchies have to 
                    be free of cycles.  The third states that the psm assumes the existence 
                    of a solution in the virtual solution space - i.e., if such a solution
                    exists, the psm will find it.  The final assumption states that the 
                    method assumes that the exclusion criterion is correct.  That is, if a
                    solution is excluded, all its refinements can be excluded too.")
   
   (assumption1 
    :value (kappa (?psm)
                  (not (exists (?ob1 ?ob2)
                               (and ;(not (= ?ob1 ?ob2))
                                    (member ?ob1 (the ?observables 
                                                      (and (generalized-abstract-from 
                                                            ?observables 
                                                            (role-value ?psm has-observables) 
                                                            (role-value ?psm has-abstractors))
                                                           (forall ?ab (=> (member ?ab 
                                                                                   (role-value ?psm has-abstractors))  
                                                                           (not (abstractor-is-applicable? 
                                                                                 ?ab 
                                                                                 ?observables)))))))
                                    (member ?ob2 (the ?observables 
                                                      (and (generalized-abstract-from 
                                                            ?observables 
                                                            (role-value ?psm has-observables) 
                                                            (role-value ?psm has-abstractors))
                                                           (forall ?ab (=> (member ?ab 
                                                                                   (role-value ?psm has-abstractors))  
                                                                           (not (abstractor-is-applicable? 
                                                                                 ?ab 
                                                                                 ?observables)))))))
                                    ;(not (= ?ob1 ?ob2))
                                    (my-generalised-abstract-link  ?ob2 ?ob1)
                                    (my-generalised-abstract-link  ?ob1 ?ob2))))))
   (assumption2
    :value (kappa (?psm)
                  (not (exists (?sol1 ?sol2)
                               (and (or (member ?sol1 
                                                (role-value ?psm 'has-candidate-solutions))
                                        (exists ?sol
                                                (and (member ?sol
                                                             (role-value 
                                                              ?psm 
                                                              'has-candidate-solutions))
                                                     (generalised-refinement-of 
                                                      ?sol1 ?sol
                                                      (role-value ?psm has-refiners)))))
                                    (generalised-refinement-of 
                                     ?sol2 ?sol1
                                     (role-value ?psm has-refiners))
                                    (generalised-refinement-of 
                                     ?sol1 ?sol2
                                     (role-value ?psm has-refiners)))))))

   ;;;;;;  the theorem prover depends on the order of relations occurring in a logic expression;;;;;   
   (assumption3
    :value (kappa (?psm)
                  (exists ?sol
                          (and 
                           (member ?sol 
                                       (the-virtual-solution-space 
                                        (role-value ?psm has-candidate-solutions)
                                        (role-value ?psm has-refiners)))
                           (admissible-solution 
                                ?sol 
                                (apply-match-criterion 
                                 (role-value ?psm 'has-match-criterion)
                                 (role-value ?psm 'has-observables)
                                 ?sol)
                                (role-value 
                                 ?psm 
                                 'has-solution-admissibility-criterion))))))                          
                               
   ;(assumption3
   ; :value (kappa (?psm)
   ;               (exists ?sol
   ;                       (and (admissible-solution 
   ;                             ?sol 
   ;                             (apply-match-criterion 
   ;                              (role-value ?psm 'has-match-criterion)
   ;                              (role-value ?psm 'has-observables)
   ;                              ?sol)
   ;                             (role-value 
   ;                              ?psm 
   ;                              'has-solution-admissibility-criterion))
   ;                            (member ?sol 
   ;                                    (the-virtual-solution-space 
   ;                                     (role-value ?psm has-candidate-solutions)
   ;                                     (role-value ?psm has-refiners)))))))
   (assumption4
    :value (kappa (?psm)
                  (forall (?sol ?score)
                          (=>
                           (and 
                            (member ?sol (role-value ?psm 'has-candidate-solutions)) 
                     ;;;;;;;; the relation member used here to constraint the range of the variable ?sol ;;;;;;;;;
                            (sol-has-match-score ?sol 
                                                 (role-value ?psm 'has-observables)
                                                 ?score 
                                                 (role-value 
                                                  ?psm 'has-match-criterion))
                            (ruled-out-solution ?sol ?score
                                                (role-value 
                                                 ?psm 'has-solution-exclusion-criterion)))
                           (not (exists 
                                 ?sol2
                                 (and (generalised-refinement-of 
                                       ?sol2 ?sol (role-value 
                                                   ?psm has-refiners))
                                      (admissible-solution 
                                       ?sol2 
                                       (apply-match-criterion 
                                        (role-value ?psm 'has-match-criterion)
                                        (role-value ?psm 'has-observables)
                                        ?sol2)
                                       (role-value 
                                        ?psm 
                                        'has-solution-admissibility-criterion)))))))))

  
   (has-body :value 
    '(lambda (?psm) (in-environment 
                     ((?observables . (the ?observables 
                                           (and (generalized-abstract-from 
                                                 ?observables 
                                                 (role-value ?psm has-observables) 
                                                 (role-value ?psm has-abstractors))
                                                (forall ?ab (=> (member ?ab 
                                                                        (role-value ?psm has-abstractors))  
                                                                (not (abstractor-is-applicable? 
                                                                      ?ab 
                                                                      ?observables)))))))
                      (?candidate-solutions . (the-virtual-solution-space 
                                               (role-value ?psm 'has-candidate-solutions)
                                               (role-value ?psm 'has-refiners))))

                     (if (holds (role-value ?psm assumption1) ?psm) 
                         (output  "~2%assumption1 holds~2%")
                         (output "~2%assumption1 does not holds~2%"))

                     (if (holds (role-value ?psm assumption2) ?psm) 
                         (output "assumption2 holds~2%")
                         (output "assumption2 does not holds~2%"))

                     (if (holds (role-value ?psm assumption3) ?psm) 
                         (output "assumption3 holds~2%") 
                         (output "assumption3 does not holds~2%"))

                     (if (holds (role-value ?psm assumption4) ?psm) 
                         (output "assumption4 holds~2%")
                         (output "assumption4 does not holds~2%"))

                     (output "Observables:~%~S" ?observables)

                     (output "~2%Solution Space:~%~S~%" ?candidate-solutions) 
                     
                     
                     (setofall ?sol (and 
                                     (member ?sol ?candidate-solutions)
                                     (admissible-solution 
                                      ?sol 
                                      (apply-match-criterion 
                                       (role-value ?psm 'has-match-criterion)
                                       ?observables  ;(role-value ?psm 'has-observables)
                                       ?sol)
                                      (role-value 
                                       ?psm 
                                       'has-solution-admissibility-criterion))
                                       
                                     (best-match 
                                      ?observables   ;(role-value ?psm 'has-observables)
                                      ?sol
                                      ?candidate-solutions
                                      (role-value ?psm 'has-match-criterion))))))))
                       
 :own-slots ((tackles-task-type optimal-classification-task)))


;;;PATCHES 
;;; re-define function  the-virtual-solution-space as follows. The old definition
;;; is not operational.

;;;THE-VIRTUAL-SOLUTION-SPACE
(def-function the-virtual-solution-space (?init-space ?refs) -> ?solution-space
  "The space generated by refinement application from an initial solution space"
  :constraint (and (every ?refs refiner)
                   (solution-space ?init-space))
  :body  (setofall ?sol2 (or (member ?sol2 ?init-space)
                             (exists ?sol3
                                     (and (member ?sol3 ?init-space)
                                          (generalised-refinement-of ?sol2
                                                                     ?sol3 
                                                                     ?refs))))))

;;; re-define relation generalized-abstract-from as follows. The old definition
;;; is not operational (leading to a infinite loop).

(def-relation generalized-abstract-from (?observables-out ?observables-in ?abs)
  :constraint (and (observables ?observables-out)
                   (observables ?observables-in)
                   (abstractors ?abs))

  :iff-def (or (= ?observables-out ?observables-in)
               (generalized-abstract-from 
                ?observables-out 
                (setofall ?ob (or (member ?ob ?observables-in)
                                  (directly-abstracted-from ?ob ?observables-in ?abs)))
                ?abs)))
;;; define new relation my-generalized-abstract-link to replace generalised-abstract-link as follows. 

(def-relation my-generalised-abstract-link (?ob1 ?ob2)
  "?ob1 is in a chain of abstraction which stems from ?ob2"
  :iff-def (or (member (the ?f (has-observable-feature ?ob2 ?f) 
                            (all-features-in-observables (the ?abstract-from  (abstract-from ?ob1 ?abstract-from))))
               (exists ?ob (and (member ?ob (the ?abstract-from  (abstract-from ?ob1 ?abstract-from)))
                                        (my-generalised-abstract-link ?ob ?ob2))))))


;;; re-define relation directly-abstracted-from as follows. The old definition
;;; has a bug.

(def-relation directly-abstracted-from (?ob ?observables ?abs)
  :constraint (and (observables ?observables)
                   (observable ?ob)
                   (abstractors ?abs))
  :iff-def (exists (?ab)
                   (and (member ?ab ?abs)
                        (abstractor-is-applicable? ?ab ?observables)
                        (= ?ob (apply-abstract-operator ?ab ?observables)))))


;;; re-define relation generalised-refinement-of as follows. The old definition
;;; is too abstract that the OCML theorem prover can not deal with it.

;;;GENERALISED-REFINEMENT-OF
(def-relation generalised-refinement-of (?sol-out ?sol-in ?refs)
  :iff-def (or (exists ?ref (and (member ?ref ?refs)
                                 (member ?sol-out (apply-refiner-operator 
                                                   ?ref ?sol-in))))
               (exists (?sol-temp ?ref1)
                       (and (member ?ref1 ?refs)
                            (member ?sol-temp (apply-refiner-operator 
                                                   ?ref1 ?sol-in))
                            (generalised-refinement-of ?sol-out ?sol-temp ?refs)))))






;;;;;;  Adding the following function in the file heuristic-classify-apples.lisp for test purpose ;;;;;;;;;;;;;;;;;


(defun declarative-apple-optimal-classification (obs)
  (heuristic-classify :solution-space '(apple)
                      :observables obs                     
                      :abstractors '(sugar-abstractor)
                   ;;;:match-criterion 'abstraction-aware-match-criterion
                      :refiners '(refinement-through-subclass-of-links)
                      :domain-name 'apples2
                      :task-type 'optimal-classification-task
                      :method-type 'declarative-heuristic-optimal-sol-classifier
                      :solution-admissibility-criterion 'DEFAULT-SOLUTION-ADMISSIBILITY-criterion))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              