;;; FILE: arthritis-ontology.lisp
;;; **************************************************************************
;;; Based on: DM864, Plowman, Complete Family Health Encyclopedia
;;; and CYC ontology
;;; 
;;; Author: Arthur Stutt
;;; Date: 2.2002

(in-package ocml)

(in-ontology arthritis)

(def-domain-class disease-or-disorder () 
  ((disease-name :type dname)
   (disease-short-name :type dname)
   (disease-alternative-name :type dname)
   (susceptible-groups :type group)  
   (caused-by :type disease-causing-agent) ;; virus/bacterium etc.
   (reported-symptoms :type symptom)
   (site :type site)
   (onset :type onset-index) ;;e.g. gradual
   (periodicity :type periodicity-index)
   (severity :type severity-index)
   (rarity :type rarity-index) ;;rare-common
   (physical-examination-result :type examination-result)
   (test-result :type test-result)
   (treatment :type treatment)
   (sequelae :type disease-or-disorder)
   (sequela-of :type disease-or-disorder)
   (associated-with :type disease-or-disorder)
   (part-of-syndrome :type syndrome)
   (possible-confusibles :type disease-or-disorder)
   (discriminators :type symptom)
   (prognosis :value prognosis)
   )
  )

(def-domain-class disorder (disease-or-disorder) 
  ((caused-by :type disorder-causing-agent) ;; injury/wear-and-tear etc.
   )
  )

(def-domain-class disease (disease-or-disorder) 
  ((caused-by :type disease-causing-agent) ;; virus/bacterium etc.
   (spread-by :type transmission-medium) ;;aerosol, touch, etc.
   (incubation-period :type integer) ;;; days
   )
  )

(def-domain-class autoimmune-disorder (disease-or-disorder) 
  ((caused-by :type disorder-causing-agent) ;; injury/wear-and-tear etc.
   )
  )

(def-domain-class heart-disease (disease) 
  ((site :value heart)
   )
  )

(def-domain-class skin-disease (disease) 
  ((site :value skin) ;; 
   )
  )
;;;;;;;;; Arthritis ;;;;;;;;;;
(def-domain-class joint-disease (disease)
  )

(def-domain-class joint-disorder (disorder)
  )

(def-domain-class rheumatic-fever (joint-disease heart-disease)
  )

(def-domain-class arthritis (joint-disease)
  )


;;;;;;;;; osteoarthritis ;;;;;;
(def-domain-class osteoarthritis (arthritis) 
  ((disease-alternative-name :value degenerative-arthritis
                             :value osteoarthrosis)
   (caused-by :value joint-wear-and-tear
              :value joint-injury
              :value joint-deformity
              :value inflammation-from-disease)
   (susceptible-groups :value older-people 
                       :value sufferers-joint-injury)
   (reported-symptoms :value joint-pain 
                      :value joint-swelling 
                      :value joint-stiffness 
                      :value joint-creaking)
   (site :value hips
         :value knees
         :value spine)
   (physical-examination-result :value joint-tenderness
                                :value swelling
                                :value pain-on-movement)
   (test-result :value (x-ray loss-of-cartilage  positive)
                :value (x-ray formation-osteophytes  positive))   
   (treatment :value (analgesic-drugs pain) 
              :value (nonsteroidal-anti-inflammatories  inflammation) 
              :value (corticosteroid  painful-joint)
              :value (physiotherapy  general-symptoms)
              )
   (discriminators :value none)
   )
  )

(def-domain-class severe-osteoarthritis (osteoarthritis) 
  ((reported-symptoms :value severe-joint-pain 
                      :value severe-swelling 
                      :value severe-joint-stiffness)
   (test-result :value (x-ray extensive-loss-of-cartilage  positive)
                :value (x-ray extensive-formation-osteophytes  positive)) ;;outgrowths new bone  
   (treatment :value (arthroplasty relieve-symptoms) ;; joint replacement *********
              :value (arthrodesis relieve-symptoms) ;; joint immobilization
              )
   )
  )

;;;;;;;;; Rheumatoid arthritis ;;;;;;
(def-domain-class rheumatoid-arthritis (arthritis autoimmune-disorder) 
  ((disease-alternative-name :value none)
   (caused-by :value joint-wear-and-tear
              :value joint-injury
              :value joint-deformity
              :value inflammation-from-disease)
   (susceptible-groups :value young-adults
                       :value middle-aged
                       :value older-people)
   (reported-symptoms :value joint-pain 
                      :value joint-stiffness
                      :value joint-swelling
                      :value joint-reddening 
                      :value joint-deformity
                      :value mild-fever
                      :value generalised-aches-and-pains)
   (site :value fingers
         :value wrists
         :value toes
         :value joints)
   (onset gradual) ;;???
   (periodicity recurrent-attacks)
   (severity moderate)
   (rarity 2) ;;1-2% population
   (physical-examination-result :value weakness-tendons
                                :value weakness-ligaments
                                :value weakness-muscles
                                :value weak-grip
                                :value raynauds-phenomenon
                                :value carpal-tunnel-syndrome
                                :value tenosynovitis
                                :value soft-nodules-beneath-skin
                                :value bursitis
                                :value bakers-cyst-behind-knee
                                :value fatigue
                                )
   (test-result :value (x-ray joint-deformities positive)
                :value (blood-test anaemia positive)
                :value (blood-test rheumatoid-factor positive)
                )   
   (treatment :value (antirheumatic-drugs slow-progress) 
              :value (nonsteroidal-anti-inflammatories relieve-joint-pain-stiffness) 
              :value (corticosteroid  relieve-painful-joint)
              :value (immuno-suppressant-drugs  suppress-immune-system)
              :value (physiotherapy  general-symptoms)
              :value (splints  relieve-hand-wrist-pain)
              :value (occupational-therapy  general-symptoms)
              :value (diet  relieve-general-symptoms)
              :value (acupuncture  relieve-pain)
              )
   (discriminators :value none)
   (sequelae :value pericarditis
             :value poor-circulation
             :value foot-ulcers
             :value hand-ulcers
             :value pleural-effusion
             :value pulmonary-fibrosis
             :value sjogrens-syndrome
             :value enlarged-lymph-nodes
             :value hypersplenism-feltys-syndrome
             )
   )
  )

(def-domain-class severe-rheumatoid-arthritis (arthritis) 
  ((disease-name :value severe-rheumatoid-arthritis)
   (reported-symptoms :value severe-joint-pain 
                      :value severe-joint-stiffness
                      :value severe-joint-swelling
                      :value severe-joint-deformity)
   (site :value hip
         :value knee)
   (periodicity frequent-attacks)
   (severity severe)
   (test-result :value (x-ray severe-joint-deformities positive)
                )   
   (treatment :value (arthroplasty relieve-symptoms) ;; **************
              )
   )
  )

(def-domain-class juvenile-rheumatoid-arthritis (arthritis) 
  ((susceptible-groups :value children-2-4-years
                       :value children-around-puberty)
   (rarity rare)
   (required-symptom-period 3) ;;months ??  
   (possible-confusibles :value viral-bacterial-infection
                         :value rheumatic-fever
                         :value crohns-disease
                         :value ulcerative-colitis
                         :value haemophilia
                         :value sickle-cell-anaemia
                         :value leukaemia)
   (prognosis :value condition-disappears-after-several-years
              :value may-leave-deformity)
   )
  )

(def-domain-class stills-disease (juvenile-rheumatoid-arthritis) 
  ((disease-alternative-name :value systemic-onset-juvenile-arthritis)
   (reported-symptoms :value fever 
                      :value rash
                      :value enlarged-lymph-nodes 
                      :value abdominal-pain
                      :value weight-loss)
   (onset illness-followed-by-joint-symptoms)
   )
  )

(def-domain-class polyarticular-juvenile-arthritis (juvenile-rheumatoid-arthritis) 
  ((site :value many-joints)
   )
  )

(def-domain-class pauciarticular-juvenile-artritis (juvenile-rheumatoid-arthritis) 
  ((site :value four-or-fewer-joints)
   )
  )

;;; no rheumatoid factor
(def-domain-class seronegative-rheumatoid-arthritis (arthritis) 
  ((test-result :value (blood-test rheumatoid-factor negative)
                )
   )
  )

;;;;;;;;; Ankylosing Spondylitis ;;;;;;
(def-domain-class ankylosing-spondylitis (arthritis) 
  ((disease-alternative-name :value none)
   (caused-by :value unknown)
   (susceptible-groups :value 20-40-year-olds) 
   (reported-symptoms :value pain-lower-back-and-hips 
                      :value stiffness-lower-back-and-hips
                      :value worse-in-morning 
                      :value chest-pain 
                      :value loss-appetite
                      :value tiredness) ;; 20-40 = range / worse-in-morning=qualification
   (site :value joints-between-spine-and-pelvis
         :value hips
         :value knees
         :value ankles
         :value tissues-heel)
   (onset :value preceded-by-colitis
          :value preceded-by-psoriasis) 
   (rarity 1) ;;1% population
   (physical-examination-result :value iritis
                                :value inflammation-tissues-heel
                                )
   (test-result :value (blood-test HLA-B27 positive);; histocompatibility complex found 'more' often
                :value (x-ray increase-bone-density-around-sacriliac-joints positive)
                :value (x-ray loss-of-spacebetween-joints-and-bony-outgrowths positive)
                )   
   (treatment :value (heat relieve-general-symptoms) 
              :value (massage relieve-general-symptoms) 
              :value (exercise relieve-general-symptoms)
              :value (immuno-suppressant-drugs  suppress-immune-system)
              :value (anti-inflammatory-drugs  reduce-pain-and-stiffness)
              )
   (discriminators :value none)
   (sequelae :value ankylosis 
             :value kyphosis
             ) ;;ankylosis = permanent stiffness loss of movement / kyphosis = curvature- of spine
   )
  )

;;;;;;;;; Gout ;;;;;;
(def-domain-class gout (arthritis) 
  ((disease-alternative-name :value none)
   (caused-by :value hyperuricaemia-leads-to-uric-acid-crystals-in-joints)
   (susceptible-groups :value men) ;; 10:1 men:women
   (reported-symptoms :value severe-pain
                      :value affects-single-joint)
   (site :value joints-base-big-toe
         :value wrist
         :value knees
         :value ankles
         :value foot
         :value small-joints-of-hand)
   (periodicity :value acute-attacks
                :value peaking-at 24-to-36-hours 
                :value last-few-days
                :value affects-single-joint
                :value subsequent-attack-6-24-months
                :value subsequent-attack-more-joints
                :value subsequent-attack-constant-pain)
   (physical-examination-result :value swollen-joint
                                :value red-joint
                                :value swelling-spreads
                                :value redness-spreads
                                :value tender-joint
                                )
   (test-result :value (blood-test high-level-uric-acid positive)
                :value (aspirated-fluid-joint uric-acid-crystals positive)
                )   
   (treatment :value (nonsteroidal-anti-inflammatories relieve-joint-pain-inflammation)
              :value (colchicine relieve-joint-pain-inflammation)
              :value (corticosteriod-drugs relieve-joint-pain-inflammation)
              :value (allopurinal inhibit-formation-uric-acid) 
              :value (uricosuric-drugs increase-kidney-excretion-uric-acid)
              )
   (possible-confusibles :value cellulitis);; redness/swelling
   (discriminators :value affects-single-joint)
   (associated-with :value kidney-stones) ;;? sequela?
   (sequelae :value hypertension
             ) 
   )
  )

;;;;;;;;; Infective Arthritis ;;;;;;
(def-domain-class infective-arthritis (arthritis) 
  ((disease-alternative-name :value septic-arthritis
                             :value pyogenic-arthritis)
   (caused-by :value bacterial-invasion-of-joint-from-bacteraemia
              :value bacterial-invasion-of-joint-from-infected-wound
              :value tubercle-bacilli-invasion-of-joint)
   (reported-symptoms :value joint-pain
                      :value joint-swelling)
   (site :value joints)
   (physical-examination-result :value swollen-joint
                                :value red-joint
                                :value swelling-spreads
                                :value redness-spreads
                                :value tender-joint
                                )
   (sequela-of :value chickenpox
                :value rubella
                :value mumps
                :value rheumatic-fever
                :value non-specific-urethritis
                )
   (part-of-syndrome reiters-sundrome with-NSU)
   )
  )

;;;;;;;;; Lupus ;;;;;;
(def-domain-class lupus (skin-disease joint-disorder autoimmune-disorder) 
  (comment "causes inflammation of connective tissue - CHEP p. 650")
  (caused-by :value autoimmune-disorder
             :value inherited
             :value hormonal-factors)
  (triggered-by :value viral-infection
                :value sunlight
                :value drugs)
  (susceptible-groups :value women-child-bearing-age
                      :value certain-ethnic-groups) ;; 9:1 women:men / Chinese People of colour
  (rarity :value 0.4);; 1:250
  (periodicity :value symptoms-subside-and-recur)
  (severity :value variable)
  (test-result :value (blood-test antibodies-attack-bodys-tissues positive)
               :value (biopsy antibodies-attack-bodys-tissues positive)
               )
  (treatment :value (anti-malarial-drugs skin-rash) 
             :value (nonsteroidal-anti-inflammatories relieve-joint-pain-stiffness) 
             :value (corticosteroid  relieve-fever)
             :value (corticosteroid  relieve-pleurisy)
             :value (corticosteroid  relieve-neurological-symptoms)
             :value (immuno-suppressant-drugs relieve-severe-neurological-symptoms)
             :value (immuno-suppressant-drugs relieve-kidney-damage)
             )
  )


(def-domain-class discoid-lupus-erythematosus (lupus) 
  ((disease-alternative-name :value DLE) 
   (reported-symptoms :value rash 
                      :value red-circular-areas-skin
                      :value scarring
                      :value hair-loss)
   (site :value face
         :value behind-ears
         :value scalp)
   (physical-examination-result :value red-circular-areas-skin
                                :value scarring
                                :value hair-loss
                                )
   )
  )

(def-domain-class systemic-lupus-erythematosus (lupus) 
  ((disease-alternative-name :value SLE)
   (prognosis :value possibly-fatal
              :value prologed-survival-if-diagnosed-early
              :value prologed-survival-if-kidney-problems-treated) 
   (reported-symptoms :value rash
                      :value illness
                      :value fatigue
                      :value loss-of-appetite
                      :value nausea
                      :value joint-pain
                      :value weight-loss)
   (site :value skin
         :value joints
         :value kidneys)
   (physical-examination-result :value red-blotchy-butterfly-shaped-rash-nose-cheeks
                                :value no-scarring
                                )
   (test-result :value (blood-test anaemia positive)
                :value (neuro-test neurological-problems positive)
                :value (psych-test psychological-problems positive)
                :value (test kidney-failure positive)
                :value (test pleurisy positive)
                value (test arthritis positive)
                value (test pericarditis positive)
                )
   (treatment :value (nonsteroidal-anti-inflammatories relieve-joint-pain-stiffness) 
              :value (corticosteroid  relieve-fever)
              :value (corticosteroid  relieve-pleurisy)
              :value (corticosteroid  relieve-neurological-symptoms)
              :value (immuno-suppressant-drugs relieve-severe-neurological-symptoms)
              :value (immuno-suppressant-drugs relieve-kidney-damage)
              :value (physiotherapy  general-symptoms)
              :value (splints  relieve-hand-wrist-pain)
              :value (occupational-therapy  general-symptoms)
              :value (diet  relieve-general-symptoms)
              :value (acupuncture  relieve-pain)
              )
   (discriminators :value red-blotchy-butterfly-shaped-rash)
   )
  )


;;;;;;;;; END Arthritis ;;;;;

;;;;;;;;; RTDs ;;;;;;;;;;;;;;
(def-domain-class respiratory-tract-disease (disease) 
  ((disease-short-name :default-value "RTD")
   )
  )

(def-domain-class upper-respiratory-tract-disease (respiratory-tract-disease) 
  ((disease-short-name :default-value "URTD")
   )
  )

(def-domain-class lower-respiratory-tract-disease (respiratory-tract-disease) 
  ((disease-short-name :default-value "LRTD")
   )
  )

;;;;;;;;; URTDs ;;;;;;;;;;;
(def-domain-class influenza (upper-respiratory-tract-disease) 
  ((disease-short-name :value flu)
   (caused-by :value virus)
   (spread-by :value droplet-infection)
   (incubation-period :value 2) ;;; days
   (reported-symptoms :value fever11 
                      :value shivering 
                      :value headache 
                      :value aches-and-pains 
                      :value nasal-congestion 
                      :value sore-throat 
                      :value cough)
   (physical-examination-result :value high-temperature)
   (test-result :value none)
   (treatment :value (salicylates discomfort) 
              :value (codeine-linctus  cough) 
              :value (warm-environment  rest)
              :value (steam-inhalers  laryngitis)
              )
   (discriminators :value sore-throat-with-persistent-dry-cough)
   )
  )

;;;;;;;;; LRTDs ;;;;;;;;;;;
(def-domain-class pneumonia (lower-respiratory-tract-disease) 
  )

(def-domain-class bacterial-pneumonia (pneumonia) 
  ((reported-symptoms :value productive-cough 
                      :value systemic-upset 
                      :value purulent-sputum 
                      :value mucoid-sputum)
   (susceptible-groups :value ageing 
                       :value sufferers-chronic-respiratory-disease 
                       :value sufferers-heart-failure 
                       :value patients-prone-to-aspiration)
   (environmental-factors :value winter 
                          :value polluted-air)
   )
  )

(def-domain-class pneumococcal-pneumonia (bacterial-pneumonia) 
  ((disease-alternative-name :value acute-lobar-pneumonia)
   (spread-by :value droplet-infection)
   (reported-symptoms :value sudden-onset-fever 
                      :value shivering 
                      :value tachypnoea 
                      :value cold-sores 
                      :value generalized-pain 
                      :value pleuritic-chest-pain
                      :value painful-dry-cough-becoming-productive-of-blood)
   (physical-examination-result :value high-temperature
                                :value diminished-chest-movements
                                :value signs-of-consolidation
                                :value pleural-rub
                                :value crepitations)
   (test-result :value (peripheral-blood-film polymorphonuclear-leucocytosis)
                :value (blood-culture pneumococcus-growth)
                :value (sputum-culture pneumococcus-growth)
                :value (gram-staining diplococci-presence))
   (treatment :value (benzyl-penicillin 600);; mg
              :value (amoxycillin 250)
              :value (erythromycin)
              )
   (sequelae :value pulmonary-fibrosis 
             :value bronchiectasis 
             :value abscess 
             :value empyema 
             :value meningism 
             :value toxic-circulatory-collapse) ;; artefact of disease/ secondary result
   (possible-confusibles :value pulmonary-infarction 
                         :value haemoptysis)
   (discriminators :value sore-throat-with-persistent-dry-cough)
   )
  )

(def-domain-class staphylococcal-pneumonia (bacterial-pneumonia) 
  )

(def-domain-class gram-negative-pneumonia (bacterial-pneumonia) 
  )


;;; test instances
(def-domain-instance influenza1 influenza 
  )

(def-domain-instance pneum1 pneumococcal-pneumonia 
  )

(def-domain-instance arthritis1 stills-disease 
  )

(def-domain-instance arthritis1 stills-disease 
  )


(def-domain-instance seveer1 severe-rheumatoid-arthritis 
  )

;;;;;; Treatments ;;;;;;;;;
(def-domain-class treatment ()
  )

(def-domain-class drug-treatment (treatment)
  )

(def-domain-class surgical-treatment (treatment)
  )

(def-domain-class body-part-replacement (surgical-treatment)
  )

(def-domain-class body-part-fusion (surgical-treatment)
  )

(def-domain-class arthroplasty (body-part-replacement)
  ((comment :value "replacement joint/part by metal or plastic components")
   (site :value 
         :value hip 
         :value knees 
         :value finger 
         :value shoulder 
         :value elbow
         )
   )
  )

(def-domain-class arthrodesis (body-part-fusion)
  ((comment :value "two boness in diseased joint fused to prevent joint movement")
   (needed-if :value all-other-options-fail)
   (prognosis :value fusion-within-6-months
              :value no-need-for-maintenance
         )
   )
  )

;;1. Classes for symptoms, tests, treatments, onset? etc. prognosis?
;;2. Patients
;;3. Diagnoses
;;4. Rules patient history, tests, symptoms etc --> disorders

;;;;;;;;;;;;
;; Patient class + rel from patient to possible transportedted patients
;; e.g. general health, age, fear of flying etc.

;(def-class topic-type () ?x
; :iff-def (or (= ?x topic) (subclass-of ?x topic)))

(def-relation query1 (?x ?disease)
  :sufficient (and 
            (susceptible-groups ?x ?disease)
            (susceptible-groups ?x ?disease)))

(def-relation query2 (?x ?disease)
  :sufficient (susceptible-groups ?x ?disease))

(def-relation query3 (?x ?c)
  :sufficient (and 
               (treatment ?x (arthroplasty ?rest))
             (disease-name ?x ?c)))


;(= ?x topic) (subclass-of ?x topic)))