;;;;;;;;;;;;;;;;;;; ABSTRACTION AWARE MATCH CRITERION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(def-relation feature-abstracted-from (?feature ?feature-list)
  :constraint (and (feature ?feature)
                   (every ?feature-list feature)
                   (not (member ?feature ?feature-list))))

(def-instance abstraction-aware-match-criterion match-criterion
  ((has-scoring-mechanism  abstraction-aware-scoring-mechanism)
   (has-match-score-comparison-relation default-match-score-comparison-relation)))

(def-instance abstraction-aware-scoring-mechanism match-score-mechanism
  ((has-macro-scoring-mechanism abstraction-aware-macro-score-mechanism)
   (has-feature-scoring-mechanism  default-feature-score-mechanism)))

(def-function abstraction-aware-macro-score-mechanism (?observables ?solution
                                                                    ?feature-score-mech)
  :lisp-fun #'(lambda (observables solution feature-score-mech)
                (abstraction-aware-macro-score-mechanism observables solution
                                                         feature-score-mech)))

(defun abstraction-aware-macro-score-mechanism (observables solution feature-score-mech)
  (let ((obs-features (ocml-eval-gen `(all-features-in-observables ',observables)))
        (sol-features (ocml-eval-gen `(all-features-in-solution ',solution)))
        (score-vector (list nil nil nil nil)))
    (loop for feature in obs-features
          for result = (ocml-eval-gen 
                        `(call ',feature-score-mech ',observables ',feature 
                               ',solution ',sol-features))
          do
          (case result
            (:inconsistent (if (not (member feature (first score-vector))) 
                             (setf (first score-vector) 
                                   (cons feature (first score-vector)))))
            (:explained (setf (second score-vector)
                              (cons feature (second score-vector))))
            (:unexplained  (if (not (member feature (third score-vector)))
                             (setf (third score-vector) 
                                   (cons feature (third score-vector))))))
          finally
          (progn 
            (setf (fourth score-vector)
                  (set-difference sol-features obs-features))
            ;;; remove all explained features (direct or indirect) from unexplained features
            (setf (third score-vector) (set-difference (third score-vector) 
                                                       (ocml-eval-gen `(all-explained-features ',(second
                                                                                                  score-vector)))))                   
            (return score-vector)))))

(def-function  all-explained-features (?features)
  "We assume that the features in ?features are exlained, with  we
  want to find all featuers that are in the tree rooted at a feture in ?features"
  :constriant (every ?features feature) 
  :body (setofall ?feature (exists (?r ?d) 
                                   (and (member ?r ?features)
                                        (feature-abstracted-from ?r ?d)
                                        (member ?feature ?d)))))


;;;;;;;;;;;;;;;;;;END ABSTRACTION AWARE MATCH CRITERION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;