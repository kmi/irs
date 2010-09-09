(in-package "OCML")

(in-ontology elms-ontology)

#|
;;;john liliana
(def-class purpose () ?x
  ()
  ;;change this to a proper meta class later
  :iff-def (member ?x (adult-general-care 
				adult-mobility-impairment-care 
				adult-hearing-impairment-care
				adult-hearing-impairment-care
				adult-visual-impairment-care
				adult-speech-impairment-care
				baby-general-care
				baby-impairment-care)))
|#

;;;use a care-descriptor to link an impairment to a care type
(def-class care-descriptor ()
  ((related-impairment :type impairment)))

(def-instance adult-general-care care-descriptor
  ((related-impairment impairment)))

(def-instance adult-mobility-impairment-care care-descriptor
  ((related-impairment mobility-impairment)))

(def-instance adult-hearing-impairment-care care-descriptor
  ((related-impairment hearing-impairment)))

(def-instance adult-visual-impairment-care care-descriptor
  ((related-impairment visual-impairment)))

(def-instance adult-speech-impairment-care care-descriptor
  ((related-impairment speech-impairment)))

(def-instance baby-general-care care-descriptor
  ((related-impairment impairment)))

(def-instance baby-impairment-care care-descriptor
  ((related-impairment impairment)))

;;; this section describes the general properties of an item of the ELMS catalogue

(def-class care-item (tangible-thing)
  ((code :type string)
   (used-for :type care-descriptor)
   (cost :type number)
   (currency :type string :default-value GBP)
   (max-user-weight :type number) 
   (max-user-weight-measure :type string :default-value kilogram)
   (item-width :type number)
   (item-width-measure :type string :default-value meter)
   (item-height :type number)
   (item-height-measure :type string :default-value meter)
   (item-seat-height :type number)
   (item-seat-height-measure :type string :default-value meter)
   (item-depth :type  amount-of-length)
   (item-depth-measure :type string :default-value meter)
   (item-weight :type number)
   (item-weight-measure :type string :default-value kilogram)
   (narrative-detail :type string)
   (to-be-approved-by :type case-worker-category)
   (picture :type string :min-cardinality 0)
   (main-supplier :type supplier :cardinality 1)
   (other-suppliers :type supplier :min-cardinality 0)
   (needs-technician-fit :type boolean)) 
)


;;; Item classification 

(def-class adaptation-item (care-item) 
((used-for :value adult-mobility-impairment-care))
)

(def-class bathing-item (care-item)
)

(def-class adult-bathing-item (bathing-item) 
((used-for :value adult-mobility-impairment-care))
)

(def-class baby-bathing-item (bathing-item) 
((used-for :value baby-impairment-care))
)

(def-class bed-accessories-item (care-item) 
((used-for :value adult-mobility-impairment-care))
)

(def-class foster-care-item (care-item) 
)

(def-class baby-foster-care-item (foster-care-item) 
((used-for :value baby-general-care))
)

(def-class adult-foster-care-item (foster-care-item) 
((used-for :value adult-general-care))
)

(def-class kitchen-item (care-item) 
((used-for :value adult-mobility-impairment-care))
)

(def-class manual-handling-item (care-item) 
((used-for :value adult-mobility-impairment-care))
)

(def-class mobility-item (care-item) 
((used-for :value adult-mobility-impairment-care))
)

(def-class seating-item (care-item) 
((used-for :value adult-general-care))
)

(def-class sensory-item (care-item) 
)

(def-class sensory-for-hearing-impairment-item (sensory-item) 
((used-for :value adult-mobility-hearing-impairment-care))
)

(def-class sensory-for-visual-impairment-item (sensory-item) 
((used-for :value adult-mobility-visual-impairment))
)

(def-class general-sensory-item (sensory-item) 
  ((used-for :value adult-general-care)))

(def-class toileting-item (care-item) 
((used-for :value adult-mobility-impairment-care))
)

;;;contact details

(def-class contact-details (intangible-thing) 
  ((street :type string) 
   (street-number :type number)
   (post-code :type string))
   (city :type string)
   (county :type string)
   (country :type string)
   (phone-number :type string)
   (e-mail :type string))

(def-class gender (intangible-thing) ?x
  :iff-def (element-of  ?x (set-of male-gender female-gender)))


;;;class person


(def-class person ()
  ((has-name :type string :cardinality 1) 
   (middle-name :type string :min-cardinality 0)
   (surname :type string :cardinality 1)
   (has-gender :type gender) ;;;optional field
   (has-contact-details :type contact-details :cardinality 1)
   (height :type float :min-cardinality 0 :max-cardinality 1) ;;;optional field
   (weight :type float :min-cardinality 0 :max-cardinality 1) ;;;optional field
   (age :type integer :min-cardinality 0 :max-cardinality 1))) ;;;optional field 


;;;class client

(def-class client (person) 
  ((has-impairment :type impairment))
)


;;;class impairment

(def-class impairment (intangible-thing))

(def-class hearing-impairment (impairment)) ;;; stored in SWIFT DB 

(def-class speech-impairment (impairment)) ;;; Stored in SWIFT DB

(def-class visual-impairment (impairment)) ;;; Stored in SWIFT DB

(def-class mobility-impairment (impairment)) ;;; Has to be introduced by the case worker


;;;class case worker

(def-class case-worker (person) 
  ((worker-code :type string)
   (understands-sign-language :type boolean)
   (budget :type number)
   (has-category :type case-worker-category))
)

;;; this section defines different kinds of case worker


(def-class case-worker-category (intangible-thing)  ?x
  ()
  ;;change this to a proper meta class later
  :iff-def (member ?x (assistant-social-worker social-worker ot-team-manager ota ot ot-service-manager manager)))


;;;class referral

(def-class referral-role-descriptor () ?x
  ()
  :iff-def (member ?x (person doctor-in-hospital gp relative friend patient other)))



;;; this section defines the case: the case worker opens a case when he goes to visit the patient


(def-class assessment-of-care-equipment (county-council-care-service) 
 ((initiated-by :type referral-role-descriptor :cardinality 1))
  (deliver-to :type client :cardinality 1)
  (order-of-equipment :type order :min-cardinality 1)
  (case-worker-in-charge :type case-worker) 
  )


(def-class order (intangible-thing)
  ((reason-of-order :type purpose) 
   (ordered-item :type care-item :cardinality 1)
   (case-worker-required-for-approval :type case-worker-category)
   (due-to-equipment-failure :type Boolean)
   (needs-minor-adaptations :type minor-adaptations :cardinality 1)
   (level-of-order :type level-of-delivery-descriptor :cardinality 1)
   (date-ordered :type calendar-date :cardinality 1)
   (time-ordered :type time-of-day :min-cardinality 0 :max-cardinality 1) 
   (date-delivery :type calendar-date :cardinality 1)
   (date-returned :type calendar-date :min-cardinality 0 :max-cardinality 1))
)


(def-class minor-adaptations (intangible-thing)
((needed :type Boolean)
 (text :type string))
)


(def-class level-of-delivery-descriptor () ?x
  ()
  :iff-def (member ?x (p1 p2 emergency)))



;;; I don’t think this is correct. I’d like to list all the suitable items for a certain reason of order (checking if the used-for field in item = reason-of-order in order

#|
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
    
|#



(def-class supplier (intangible-thing) 
  ((has-name :type string :cardinality 1) 
   (has-email-address :type string :min-cardinality 0 :max-cardinality 1)
   (web-site :type string :min-cardinality 0 :max-cardinality 1)
   (telephone-number :type string)
   (fax-number :type string)))

;;EXAMPLE INSTANCES

(def-instance wall-mounted-shower-seat adaptation-item
  ((code B58Z)
   (used-for mobility-impairment)
   (cost 52.00)
   (max-user-weight 159.00)
   (item-width 0.33)
   (item-height 0.00)
   (item-seat-height 0.00)
   (item-depth 0.00)
   (item-weight 0.00)
   (narrative-detail  "Solo Compact Plus shower seat has the benefit of an adjustable leg for increased weight capacity.  It is designed for areas where space is limited.   Fold up when not in use.  Padded seat.")
   (to-be-approved-by ot-team-manager)
   ;;(category adaptations) 
   (main-supplier supplier1)
   (needs-technician-fit false)))



(def-instance small-wall-mounted-shower-seat adaptation-item
  ((code B58Z)
   (used-for mobility-impairment)
   (cost 52.00)
   (max-user-weight 100.00)
   (item-width 0.33)
   (item-height 0.00)
   (item-seat-height 0.00)
   (item-depth 0.00)
   (item-weight 0.00)
   (narrative-detail  "Solo Compact Plus shower seat has the benefit of an adjustable leg for increased weight capacity.  It is designed for areas where space is limited.   Fold up when not in use.  Padded seat.")
   (to-be-approved-by ot-team-manager)
   ;;(category adaptations) 
   (main-supplier supplier1)
   (needs-technician-fit false)))

(def-instance large-wall-mounted-shower-seat adaptation-item
  ((code B58Z)
   (used-for mobility-impairment)
   (cost 52.00)
   (max-user-weight 220.00)
   (item-width 0.33)
   (item-height 0.00)
   (item-seat-height 0.00)
   (item-depth 0.00)
   (item-weight 0.00)
   (narrative-detail  "Solo Compact Plus shower seat has the benefit of an adjustable leg for increased weight capacity.  It is designed for areas where space is limited.   Fold up when not in use.  Padded seat.")
   (to-be-approved-by ot-team-manager)
   ;;(category adaptations) 
   (main-supplier supplier1)
   (needs-technician-fit false)))

(def-instance supplier1 supplier
  ((has-name "Shower's 'R Us") 
   (has-email-address "supp1@email.com")
   (web-site "www.supplier1co.co.uk")
   (telephone-number 0208777777)
   (fax-number 02087999999)))



(def-instance john client
  ((has-impairment hearing-impairment)
   (weight 185)))

(def-instance bill client
  ((has-impairment visual-impairment)
   (weight 170)))

(def-instance fred client
  ((has-impairment speech-impairment)
   (weight 150)))

(def-instance clive client
  ((has-impairment mobility-impairment)
   (weight 180)))

(def-instance jessica client
  ((has-impairment mobility-impairment)
   (weight 110)))