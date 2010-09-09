;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")


(in-ontology apples2)



;;;CLASS FRUIT
;;;just for the sake of it
(def-class fruit)


(def-class apple (fruit)
  ((has_foreground :type yes-or-no)
   (background :type colour)))


(def-class granny  (apple)
  "Granny Smith"
  (( has_foreground :value no)
   (background :value green)
   (rusty :value no)))

(def-class grouwe_reinet (apple) 
  "Grauwe Reinette"
  (( has_foreground :value no)
   (background :value green)
   (rusty :value yes)))

(def-class landsberger (apple) 
  "Landsberger Reinette" 
  (( has_foreground :value no)
   (background :value yellow)))

(def-class present (apple) 
  "Present of England"
  (( has_foreground :value no)
   (background :value yellow_green)
   (greasy :value yes)))

(def-class lombart (apple) 
  "Lombarts Claville"
  (( has_foreground :value no)
   (background :value yellow_green)
   (greasy :value no)
   (form :value flat)))

(def-class delicious (apple) 
  "Golden Delicious"
  (( has_foreground :value no)
   (background :value yellow_green)
   (greasy :value no)
   (form :value high)))

(def-class caroline (apple) 
  "Sweet Caroline"
  (( has_foreground :value yes)
   (yellow :value yes)
   (is_striped :value no)
   (kelk_leaves :value yes)))

(def-class estivale (apple) 
  "Delbare Estivale"
  (( has_foreground :value yes)
   (yellow :value yes)
   (is_striped :value no)
   (kelk_leaves :value no)))

(def-class noble (apple) 
  "Princesse Noble"
  (( has_foreground :value yes)
   (yellow :value yes)
   (is_striped :value yes)
   (bobbels :value yes)))

(def-class elstar (apple) 
  "Elstar"
  (( has_foreground :value yes)
   (yellow :value yes)
   (is_striped :value yes)
   (bobbels :value no)
   ( lenticells :value yes)	;;;;;;added BJW
   (striping_type :value vague)))

(def-class mubo (apple)
  "Mubo"
  (( has_foreground :value yes)
   (yellow :value yes)
   (is_striped :value yes)
   (bobbels :value no)
   (striping_type :value clear)))

(def-class bramley (apple) 
  "Bramley"
  (( has_foreground :value yes)
   (yellow :value no)
   (redness :value small)
   (steel :value stump)))

(def-class notaris (apple)
  "Notarisappel"
  (( has_foreground :value yes)
   (yellow :value no)
   (steel :value short)
   (redness :value small)))

(def-class goudreinet (apple) 
  "Goudreinet"
  (( has_foreground :value yes)
   (yellow :value no)
   (redness :value medium)
   (rusty :value yes)
   (very_rusty :value yes)))

(def-class karmijn (apple) 
  "Karmijn"
  (( has_foreground :value yes)
   (yellow :value no)
   (redness :value medium)
   (rusty :value yes)
   (very_rusty :value no)
   (lenticells :value yes)))

(def-class cox (apple) 
  "Cox Orange Pippin"
  (( has_foreground :value yes)
   (yellow :value no)
   (redness :value medium)
   (rusty :value yes)
   (very_rusty :value no)
   (lenticells :value no)))

(def-class jonagold (apple) 
  "Jonagold"
  (( has_foreground :value yes)
   (yellow :value no)
   (redness :value medium)
   (rusty :value no)
   (long_steel_plus_lenticells :value yes)))

;(def-class cox (apple) Cox Orange Pippin"
;(( has_foreground :value yes)
;yellow :value no
;(redness :value medium)
;rusty :value no
;long_steel_plus_lenticells :value no
;schuin_kelkblad_plus_closed :value yes)))

(def-class alkema (apple) 
  "Alkema"
  (( has_foreground :value yes)
   (yellow :value no)
   (redness :value medium)
   (rusty :value no)
   (long_steel_plus_lenticells :value no)
   (schuin_kelkblad_plus_closed :value no)))

(def-class star (apple) 
  "Star Apple"
  (( has_foreground :value yes)
   (yellow :value no)
   (redness :value high)
   (vruchtvlees :value red)))

(def-class jonathan (apple) 
  "Jonathan"
  (( has_foreground :value yes)
   (yellow :value no)
   (redness :value high)
   (ribbed :value slightly)	;;;;added BJW
   (vruchtvlees :value white)))

(def-class brown-apple (apple) 
  "brown-apple"
  ((background :value brown)))

(def-class brown-apple1 (brown-apple) 
  "brown-apple"
  ((ribbed :value slightly)))

(def-class brown-apple2 (brown-apple) 
  "brown-apple"
  ((ribbed :value a-lot)))



;;;;;;;;;; let's introduce some hierarchy
(def-class chinese-granny  (granny)
  "Granny Smith"
  (( has_foreground :value no)
   (background :value green)
   (rusty :value no)
   (area :value china)))

(def-class German-granny  (granny)
  "Granny Smith"
  (( has_foreground :value no)
   (background :value green)
   (rusty :value no)
   (area :value Germany)))



;;;;;;;;;;;;; Abstraction

(def-class high-sweet-granny  (chinese-granny)
  "Granny Smith"
  (( has_foreground :value no)
   (background :value green)
   (rusty :value no)
   (sweet-level :value high)))          

(def-class middle-sweet-granny  (chinese-granny)
  "Granny Smith"
  (( has_foreground :value no)
   (background :value green)
   (rusty :value no)
   (sweet-level :value middle)))

(def-class low-sweet-granny  (chinese-granny)
  "Granny Smith"
  (( has_foreground :value no)
   (background :value green)
   (rusty :value no)
   (sweet-level :value low)))


