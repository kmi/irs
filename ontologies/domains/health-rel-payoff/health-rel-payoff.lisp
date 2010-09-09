;;; Mode: Lisp; Package: ocml


(in-package "OCML")



;;    File:     health-rel-payoff.lisp
;;    Copyright by Laura Corti, Luisella Dazzi and Sabina Falasconi, 1996

(in-ontology health-rel-payoff)

(def-class SURVIVAL ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico")

(def-class STRATEGY ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico")

(def-class TEST ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico")

(def-class THERAPY ()
  "This is not in the pavia ontology.  maybe it should be 
  THERAPEUTIC PROCEDURE.  I define it 
   as a placeholder --Enrico")

(def-class  POPULATION-TYPE ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico")

(def-class SYMBOLIC-VALUE ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico") 

(def-class HEALTH-PAYOFF (payoff) (?hp)
  "The class of health-payoff: main outcomes and surrogate outcomes."
  )

(def-class MAIN-OUTCOME (HEALTH-PAYOFF) (?mo)
  "Main outcome of health-care: life expectancy and adjusted life years."
 )


(def-class SURROGATE-OUTCOME (HEALTH-PAYOFF) (?so)
  "Surrogate outcomes of health-care: morbidity and functionality."
  )


(def-class LIFE-EXPECTANCY (main-outcome) (?le)
  " "
  :constraint (survival ?le))


(def-class DIAGNOSTIC-STRATEGY (strategy) (?ts)
  "A diagnostic strategy is a list of tests."
  :constraint (and  (list ?ts)
                    (forall ?t (and (item ?t ?ts)
			            (test ?t)))))

(def-class CARE-STRATEGY (strategy) (?ths)
  "A care strategy is a list of therapies."
  :constraint (and 
	    (list ?ths)
	    (forall ?th (and (item ?th ?ths)
			     (therapy ?th)))))


(def-function LE-STRATEGY.DEALE (?pt ?ds ?dstr ?cs) -> ?le
  "Mapping from a population-type with some disorders to its 
   life-expectancy calculated with DEALE algorithm."
  :constraint  (and (population-type ?pt)
	            (list ?ds)
	            (forall ?d (and (item ?d ?ds)
			            (disorder ?d)))
	            (diagnostic-strategy ?dstr)
	            (care-strategy ?cs)
	            (life-expectancy ?le))
  :body  (deale ?pt (/ 1 (disorders-strategy.risks ?ds ?dstr ?cs))))

(def-function DISORDERS-STRATEGY.RISKS (?ds ?dstr ?cs) -> ?risks
  "Mapping from a list of disorders to their risks."
  :constraint (and (list ?ds)
	    (forall ?d (and (item ?d ?ds)
			    (disorder ?d)))
	    (diagnostic-strategy ?dstr)
	    (care-strategy ?cs)
	    (list ?risks)
	    (forall ?risk (and (item ?risk ?risks)
			       (disorder-strategy.risk ?ds ?dstr ?cs ?risk)))))

(def-class UTILITY-COEFF () (?uc)
  "It is a number between 0 and 1."
  :iff-def (and (real-number ?uc)
	    (>= ?uc 0)
	    (=< ?uc 1)))

(def-function POPULATION-TYPE.UTILITY-COEFF (?pt) -> ?uc
  "Mapping from a population-type to a utility coefficient."
  :constraint (and (population-type ?pt)
	           (utility-coeff ?uc)))

(def-class QALY (main-outcome) (?qaly)
  "QALY is the acronimum of Quality Adjusted Life Years."
  :constraint (real-number ?qaly))


(def-function UTILITY.QALY (?ucdus) -> ?qaly
  "Mapping from a list of utilities and durations to its
   quality adjusted expected life years."
  :constraint (and (list ?ucdus)
	    (forall  ?ucdu (and (item ?ucdu ?ucdus)
				(double ?ucdu)
				(utility-coeff (first ?ucdu))
				(duration (nth ?ucdu 1))))
 	    (qaly ?qaly))
  :body 
	   (cond ((null ?ucdus) 0)
			   ((list ?ucdus)
			    (+ (* (first (first ?ucdus))
				  (nth (first ?ucdus) 1))
			       (utility.qaly (rest ?ucdus))))))

(def-class MORBIDITY (surrogate-outcome )(?mor)
  "It's a surrogate outcome and the intangible-cost parameter."
  :constraint (symbolic-value ?mor))


(def-class FUNCTIONALITY (surrogate-outcome ) (?fun)
  "It's a surrogate outcome."
  :constraint (symbolic-value ?fun))


(def-class psychological-functionality (functionality) (?ps-fun)
  "it's a subclass of the functionality.")

(def-class PHYSICAL-FUNCTIONALITY (FUNCTIONALITY) (?ph-fun)
  "It's a subclass of the functionality.")

(def-class FUNCTIONALITY-FUNCTION (function) (?ff)
  "It's the class of the functionality function.")
  

(def-function DISORDER-STRATEGY.RISK (?d ?ds ?cs) -> ?risk
  "Mapping from a disorder to its risk."
  :constraint (and (disorder ?d)
	    (diagnostic-strategy ?ds)
	    (care-strategy ?cs)
	    (real-number ?risk))
  :body  (/ 1 (disorder-stages-strategy.disorder-survival 
                      (disorder.stages ?d) ?ds ?cs)))

(def-function BASERISK (?pt) -> ?br
  "Mapping from a population type to its baserisk."
  :constraint (and (population-type ?pt)
	    (real-number ?br))
  :body  (/ 1 (population-type.survival ?pt)))


(def-function GLOBAL-RISK (?pt ?survs) -> ?grisk
  "Mapping from a population-type and a list of disorders to its global risk
   - the sum of the risk of the disorders ."
  :constraint (and (population-type ?pt)
	           (List ?survs)
	           (forall ?sur (and (item ?sur ?survs)
			             (survival ?sur)))
	           (real-number ?grisk))
  :body (cond ((Null ?survs) (baserisk ?pt))
              ((List ?survs) (+ (/ 1 (First ?survs))
                                (global-risk ?pt (Rest ?survs))))))
									    
(def-function DEALE (?pt ?survs) -> ?surv
  "Mapping from a population and a list of survivals to its survival 
   calculated with the DEALE algorithm."
  :constraint (and (population-type ?pt)
	    (List ?survs)
	    (forall ?sur (and (item ?sur ?survs)
			    (survival ?sur)))
	    (survival ?surv))
  :body  (/ 1 (global-risk ?pt ?survs)))


(def-relation DISORDER.DIAGNOSTIC-STRATEGY (?d ?str)
  "Relation between a disorder and its diagnostic-strategy."
  :constraint (and (disorder ?d)
	           (diagnostic-strategy ?str)))

(def-class GENERIC-DIAG-STRATEGY (diagnostic-strategy) (?gds)
  "A generic strategy is a list of tests.
   What's the point of this? --enrico"
  )

(def-class GENERIC-CARE-STRATEGY (care-strategy ) (?gcs)
  "A generic strategy is a list of therapies.
   What's the point of this? --enrico")

(def-function DISORDER-STRATEGY.LE (?d ?ds ?cs) -> ?le
  "Mapping from a disorder, a diagnostic strategy and a care strategy to its survival."
  :constraint (and (disorder ?d)
	    (diagnostic-strategy ?ds)
	    (care-strategy ?cs)
	    (life-expectancy ?le)
	   )
  :body (le-strategy.deale (list ?d) ?ds ?cs))


(def-function EARLY-DIAGNOSIS.LE (?d ?ds ?cs) -> ?le
  "Mapping from a disorder, a diagnostic strategy and a care strategy
 to its survival."
  :constraint (and (disorder ?d)
	    (diagnostic-strategy ?ds)
	    (care-strategy ?cs)
	    (life-expectancy ?le)
	    
	    (not (null ?ds)))
  :body (disorder-strategy.le ?d ?ds ?cs))


(def-function LATE-DIAGNOSIS.LE (?d ?cs) -> ?le
  "Mapping from a disorder, a diagnostic strategy and a care strategy
 to its survival."
  :constraint (and (disorder ?d)
	    (care-strategy ?cs)
	    (life-expectancy ?le))
  :body  (disorder-strategy.le ?d nil ?cs))

(def-function LEAD-TIME (?d ?ds) -> ?lt
  "Mapping from a disorder and a diagnostic-strategy to its lead-time."
  :constraint (and (disorder ?d)
	    (diagnostic-strategy ?ds)
	    (duration ?lt))
  :body  (- (early-diagnosis.le ?d ?ds nil)
            (late-diagnosis.le ?d nil)))


;(def-class HEALTH-OUTCOME (?hout)
;  "It's a goal and the desired-outcome of a medical-action ."
;  :def (and (goal ?hout)
;	    (exists ?mac (and (medical-action ?mac)
;			      (medical-action.desired-outcome ?mac ?hout)))))

(def-function DISORDER-STAGE-STRATEGY.PERCENTAGE (?dst ?dstr ?cs) -> ?perc
  "Mapping from a disorder-stage to its percentage. 
The sum of stages percentages must be equal to 1, with or 
without screening"
  :constraint (and (disorder-stage ?dst)
	    (diagnostic-strategy ?dstr)
	    (care-strategy ?cs)
	    (percentage ?perc)))

(def-function DISORDER-STAGES-STRATEGY.DISORDER-SURVIVAL (?dsts ?ds ?cs) -> ?dsurv
  "Mapping from disorder stages to disorder survival."
  :constraint (and (exists ?d (and (disorder ?d)
			    (= ?dsts (disorder.stages ?d))))
	    (diagnostic-strategy ?ds)
	    (care-strategy ?cs)
	    (survival ?dsurv)
	    (exists ?du 
                    (and (duration ?du)
                         (= ?dsurv 
                            (cond ((Null ?dsts) 0)
                                  ((List ?dsts) 
                                   (+ (/ (disorder-stage-strategy.percentage 
                                          (First ?dsts) ?ds ?cs)
                                         (/ (- (log (disorder-stage.survival-rate 
                                                     (First ?dsts) ?du) 
                                                    The-Exponentiation-Constant-E)) ?du))
                                      (disorder-stages-strategy.disorder-survival 
                                       (Rest ?dsts)?ds ?cs)))))))))




			     

