;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology emergency-gis-situations)


;;; top concepts

(def-class feature (role))

(def-class qualitative-feature (feature))

(def-class quantitative-feature (feature))

(def-class observable-features () ?x
:iff-def (exists ?y (and (slot-of ?y ?x)
                         (quantitative-feature ?y))))

(def-class required-features () ?x
:iff-def (exists ?y (and (slot-of ?y ?x)
                         (qualitative-feature ?y))))
 
(def-class classifiable-situation ()
((has-observables :type quantitative-feature) ;; exit features
 (has-requirements :type qualitative-feature) ;;entry features
 (has-abstractor :type abstractor)
 (has-following-situation :type classifiable-situation) ;;representing the solution space for the classification
 ))  ;; has-affordances (inherited allowed operations)


(def-class feature-level ()
"example of qualitative features. We abstract to it from the WS observables")

(def-instance high feature-level)

(def-instance medium feature-level)

(def-instance low feature-level)


;;; classification elements

(defun situation-optimal-classification (obs sol abstrcs dom &optional (match-criterion 'abstraction-aware-match-criterion))
  (heuristic-classify :solution-space sol
                      :observables obs
                      :abstractors abstrcs
                      :refiners '(refinement-through-subclass-of-links)
                      :domain-name dom
                      :task-type 'optimal-classification-task
                      :method-type 'heuristic-optimal-sol-classifier
                       :match-criterion match-criterion
                      :solution-admissibility-criterion 'DEFAULT-SOLUTION-ADMISSIBILITY-criterion))    


(def-relation-mapping solution :up
  ((solution ?x)
   if
   (or (= ?x classifiable-situation)
       (subclass-of ?x classifiable-situation))))


(def-relation-mapping observable :up
  ((observable ?x)
   if
   (or (== ?X (?f ?v ?obs)) 
       (== ?x (?f ?v)))
   (or (and (slot-of ?f ?c)
            (or (= ?c classifiable-situation)
                (subclass-of ?c classifiable-situation)))
       (= ?f sugar))))


(def-relation-mapping has-observable-feature :up
  ((has-observable-feature ?x ?f)
   if
    (or (== ?X (?f ?v ?obs)) (== ?x (?f ?v)))))


(def-relation-mapping has-observable-value :up
  ((has-observable-value ?x ?v)
   if
    (or (== ?X (?f ?v ?obs)) (== ?x (?f ?v)))))


(def-relation-mapping directly-abstracts-from :up
  ((directly-abstracts-from ?ob ?obs)
   if
   (== ?ob (?f ?v ?obs))))


;;;testing function

(defun situation-classification-from-observables (obs sol abstrcs dom)
(situation-optimal-classification obs sol abstrcs dom))


;;;support functions 

(defun get-observables-from-instance (inst)
(let ((obs nil))
     (setf obs-name (ocml::setofall '?x `(has-slot-value ,inst 'has-observables ?x)))
     (loop for o in obs-name
           do (setf obs-pair (list o))      
              (setf obs-pair (append obs-pair (list (THE-SLOT-VALUE inst o))))
	      (setf obs (append obs (list obs-pair))))
     (first (list obs))))

(defun get-observables-from-class (c)
(let ((obs nil))
     (setf obs-name (ALL-CLASS-SLOT-VALUES c 'has-observables))
     (loop for o in obs-name
           do (setf obs-pair (list o))      
              (setf obs-pair (append obs-pair (ALL-CLASS-SLOT-VALUES c o)))
	      (setf obs (append obs (list obs-pair))))
     (first (list obs))))



(defun get-solution-space-from-instance (inst)
(let ((sol nil))
     (setf sol (ocml::setofall '?x `(has-slot-value ,inst 'has-following-situation ?x)))))

(defun get-solution-space-from-class (c)
(let ((sol nil))
     (setf sol (all-class-slot-values c 'has-following-situation))))



(defun get-abstractor-from-solution-space (sol)
(let ((abst nil))
     (loop for s in sol
           do (setf abst (append abst (ocml::ALL-CLASS-SLOT-VALUES s 'has-abstractor))))
     (first (list abst))))


;;;classification function with implicit observables (within the caller class)

(defun classify-current-situation (dom c)
(let ((obs nil)
      (sol nil)
      (abstrcs nil))
      (setf obs (get-observables-from-class c))
      (print obs)
      (setf sol (get-solution-space-from-class c))
      (setf abstrcs (get-abstractor-from-solution-space sol))
      (situation-optimal-classification obs sol abstrcs dom)))


;;; classification function with explicit observables 

(defun classify-current-situation2 (dom c obs)
(let ((sol nil)
      (abstrcs nil))
      (setf sol (get-solution-space-from-class c))
      (setf abstrcs (get-abstractor-from-solution-space sol))
      (situation-optimal-classification obs sol abstrcs dom)))

;;(classify-current-situation2 'met-office-domain 'snow-storm-entry-situation '((snow-level 4)))


;;;function used within the lowering function to get the following situation

(defun get-next-situation (inst)
(let ((entry-sit (ocml::findany '?x `(ocml::has-slot-value ,inst 'ocml::has-caller-situation ?x))))
  (print entry-sit)
  (if (ocml::get-ocml-class entry-sit) 
      (let ((new-sit nil)
            (domain nil)
            (obs nil)
            (obs-pairs (ocml::findany '?x `(ocml::has-slot-value ,inst 'producing-observable ?x))))
        (print entry-sit)
        (loop for p in obs-pairs
              do (setf o (first p))
                 (setf f (second p))
                 (setf obs (append obs (list (list o (funcall f inst))))))
        (setf domain (ocml::name (ocml::home-ontology (ocml::get-ocml-class entry-sit)))) 
        (setf new-sit (first (classify-current-situation2 domain entry-sit obs)))
        (ocml::name (new-instance new-sit))))))


;(get-next-situation 'instance3332)


