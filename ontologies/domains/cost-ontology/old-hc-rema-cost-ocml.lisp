;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")


;;;3.2	Cost ontology
;;   File:    cost-ontology.lisp
;;   Copyright by Luisella Dazzi, Laura Corti, and Sabina Falasconi, 1996

(def-ontology cost-ontology
  :includes (medical-ontology))

(in-ontology cost-ontology)

(def-class PAYOFF ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico")

(def-class COST-FUNCTION ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico")

(def-class EFFECTIVENESS-FUNCTION ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico")

(def-class UTILITY-FUNCTION ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico")

(def-class DURATION ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico")

(def-class PERSON ()
  "This is not in the pavia ontology.  I define it 
   as a placeholder --Enrico")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-class ECONOMIC-PAYOFF (payoff) (?ep)
  "Economic payoff."
  )

(def-class ECONOMIC-COST (economic-payoff) (?ec)
  "Economic cost."
  )

(def-class ECONOMIC-BENEFIT (economic-payoff) (?eb)
  "Economic benefit.")


(def-class DIRECT-COST (economic-cost) (?dc)
  "Direct cost.")

(def-class INDIRECT-COST (economic-cost)  (?ic)
  "Indirect cost.")

(def-class INTANGIBLE-COST (economic-cost) (?intc)
  "Intangible cost.")

(def-class DIRECT-BENEFIT (economic-benefit) (?db)
  "Direct benefit.")

(def-class INDIRECT-BENEFIT (economic-benefit) (?ib)
  "Indirect benefit.")

(def-class INTANGIBLE-BENEFIT (economic-benefit)  (?intb)
  "Intangible benefit.")

;;;;;

(def-class RESOURCE () (?r)
  "Resources: physical and human,they have a cost."
  :iff-def (and (individual ?r)
                (exists ?cost
                        (= (resource.cost ?r)
                           ?cost))))

(def-class HUMAN-RESOURCE (person resource) (?hr)
  "Human person,his/her cost is the salary.")

(def-class PHYSICAL-RESOURCE (resource) (?phres)
  "A physical resource is some equipment or machine 
   (e. g. X-ray machinery) or consumer good."
  :constraint (not (person ?phres)))

(def-function RESOURCE.COST (?r) -> ?res.cost
  "Mapping from a resource to its cost."
  :constraint (resource ?r))

(def-instance  RESOURCE.COST cost-function)

(def-class PURCHASE-MODE () (?pur)
  "Purchase mode for a resource (e.g. leasing, cash, instalment)."
  :constraint (individual ?pur))

(def-class CAPITAL () (?cap)   
  "real-estate or
   cash-assets or
   movables." 
  :constraint (resource ?cap))

(def-class REAL-ESTATE (capital) (?realest)
  "Real estate.")

(def-class CASH  (capital)  (?cash)
  "Cash.")

(def-class MOVABLES  (capital) (?mov)
  "Movables.")
			       
(def-class CONSUMER-GOOD (physical-resource ) (?cg)
  "Class of consumer goods.")
  
;  :def (and  (physical-resource ?cg)
;	     (can-have-one ?cg storage-cost)
;	     (has-one ?cg consumer-good.cost)))

(def-function CONSUMER-GOOD.COST (?cg) -> ?cg.cost
  "Mapping from consumer goods to its cost."
  :constraint (consumer-good ?cg))

(def-instance consumer-good.cost cost-function )

(def-function STORAGE-COST (?cg) -> ?s.cost
  "Mapping from consumer goods to its storage-cost."
  :constraint (consumer-good ?cg))

(def-instance storage-cost cost-function )


(def-class USE-TIME (duration) (?ut)
  "Use time of a resource")


(def-class TIMED-PHYSICAL-RESOURCE (physical-resource ) (?tps)
  "Physical resource with use time.")


(def-function TIMED-PHYSICAL-RESOURCE.UNITARY-COST (?tps) -> ?uc
  "Unitary cost of a timed physical resource."
  :constraint (timed-physical-resource ?tps))

(def-instance timed-physical-resource.unitary-cost cost-function)


(def-class MACHINERY (timed-physical-resource) (?mach)
  "The machinery is a physical-resource,has a purchase-cost, a purchase-date,
a purchase-mode, an amortization-period, a residual-value, it uses some 
consumer-good, has an unitary-cost and performs an action.")

;  :def (and (timed-physical-resource ?mach)
;	    (has-one ?mach machinery.purchase-cost)
;	    (has-value-of-type ?mach machinery.purchase-date calendar-date)
;	    (has-value-of-type ?mach machinery.purchase-mode purchase-mode)
;	    (has-value-of-type ?mach machinery.amortization-period duration)
;	    (has-one ?mach machinery.residual-value)
;	    (has-some ?mach machinery.consumer-goods) 
;	    (has-one ?mach machinery.unitary-cost)    
;	    (has-some ?mach machinery.action)))

(def-function MACHINERY.PURCHASE-DATE (?mach) -> ?pd
  "Mapping from a machinery to its purchase date."
  :constraint (and (machinery ?mach)
	           (calendar-date ?pd)))

(def-function MACHINERY.AMORTIZATION-PERIOD (?mach) -> ?ap
  "Mapping from a machinery to  its amortization period."
  :constraint (and (machinery ?mach)
	    (duration ?ap)))

(def-function MACHINERY.PURCHASE-MODE (?mach) -> ?pm
  "Mapping from a machinery to  its purchase mode."
  :constraint (and (machinery ?mach)
	    (purchase-mode ?pm)))

(def-function MACHINERY.RESIDUAL-VALUE (?mach) -> ?rv
  "Mapping from a machinery to  its residual-value."
  :constraint (machinery ?mach))

(def-instance machinery.residual-value cost-function )

(def-function MACHINERY.CONSUMER-GOODS (?mach) -> ?cgs
  "Mapping from a machinery to the list of consumer goods that it uses."
  :constraint (and (machinery ?mach)
	           (list ?cgs)
                   (not (exists ?cg (and (member ?cg ?cgs)
			                 (not (consumer-good ?cg)))))))
  
 

(def-function MACHINERY.PURCHASE-COST (?mach) -> ?pc
  "Mapping from a machinery to its purchase cost."
  :constraint (machinery ?mach))

(def-instance machinery.purchase-cost cost-function )


(def-function MACHINERY.ACTION (?mach) -> ?ac
  "Mapping from machinery to its action."
  :constraint (and (machinery ?mach)
	           (action ?ac)))


(def-function MACHINERY.UNITARY-COST (?mach) -> ?muc
  "Unitary cost of a machinery."
  :constraint (and (machinery ?mach)
	           (timed-physical-resource.unitary-cost ?mach ?muc)))

(def-instance machinery.unitary-cost cost-function )


(def-function MACHINERY.COST (?mach ?ut) -> ?mach.cost
  "Mapping from a machinery to its cost given its use-time."
  :constraint (and (use-time ?ut)
	    (machinery ?mach)
	    (= ?mach.cost
	       (* ?ut (machinery.unitary-cost ?mach)))))

(def-instance machinery.cost cost-function)


(def-class ROOM (timed-physical-resource) (?room)
  "Physical space (e.g. a building room).")
  

(def-function ROOM.UNITARY-COST (?room) -> ?ruc
  "Unitary cost of a room."
  :constraint (room ?room)
  :body (timed-physical-resource.unitary-cost ?room))

(def-instance room.unitary-cost cost-function)

(def-function ROOM.COST (?room ?ut) -> ?room.cost
  "Mapping from a room to its cost given its use-time."
  :constraint (and (use-time ?ut)
	           (room ?room))
  :body (* ?ut (room.unitary-cost ?room)))


(def-instance room.cost cost-function)
  

(def-function COST-ANALYSIS (?str) -> ?str.cost
  "The cost-analysis makes the sum of the action-costs of a strategy."
  :constraint (strategy ?str)
  :body (cond ((Null ?str) 0)
              ((List ?str) 
               (+ (action.cost (first ?str)) 
                  (cost-analysis (Rest ?str))))))
  

(def-function EFFECTIVENESS-ANALYSIS (?str) -> ?str.ef
  "The effectiveness of a strategy."
  :constraint (strategy ?str))

(def-instance effectiveness-analysis effectiveness-function)


(def-function BENEFIT-ANALYSIS (?str) -> ?str.bf
  "The benefit-analysis makes the sum of the action-benefits of a strategy."
  :constraint (strategy ?str)
  :body (cond ((Null ?str) 0)
              ((List ?str)
               (+ (action.benefit (first ?str))
                  (benefit-analysis (Rest ?str))))))
  

(def-function UTILITY-ANALYSIS (?str) -> ?str.ut
  "The utility of a strategy."
  :constraint (strategy ?str))

(def-instance utility-analysis utility-function)
  


(def-function COST-BENEFIT-ANALYSIS (?str) -> ?str.cb
  "It makes the cost-benefit analysis of a strategy by ratio."
  :constraint (strategy ?str)
  :body (/ (cost-analysis ?str) (benefit-analysis ?str)))
		
		
 (def-function DIFF-COST-BENEFIT-ANALYSIS (?str) -> ?str.cb
  "It makes the cost-benefit analysis of a strategy by difference."
  :constraint (strategy ?str) 
  :body (- (benefit-analysis ?str) (cost-analysis ?str)))


(def-function COST-EFFECTIVENESS-ANALYSIS (?str) -> ?str.ce
  "It makes the cost-effectiveness analysis of a strategy."
  :constraint (strategy ?str)
  :body (/ (cost-analysis ?str) (effectiveness-analysis ?str)))

  

(def-function COST-UTILITY-ANALYSIS (?str) -> ?str.cu
  "It makes the cost-utility analysis of a strategy."
  :constraint (strategy ?str)
  :body (/ (cost-analysis ?str) (utility-analysis ?str)))

