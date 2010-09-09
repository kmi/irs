;;; FILE: dino2a.lisp
;;; **************************************************************************
;;; Based on: www.nhm.ac.uk/education/schools_and_colleges/DINOFILE.XLS
;;; and personal2.stthomas.edu/jstweet/stegosauroidea.htm
;;; and www.enchantedlearning.com/subjects/dinosaurs//donos/Stegosaurus.shtml 
;;; Author: Arthur Stutt
;;; Date: 2.2001

(in-package ocml)
(in-ontology dino)

(def-domain-class dinosaur () ;; SUPERORDER
  ((dname :type dname)
   (name-meaning :type string)
   (discovery-site :type continent)
   (length :type float) ;; metres
   (height :type float) ;; metres
   (mass :type integer) ;; kilos
   (pedality :type pedality)
   (teeth-type :type teeth-type :max-cardinality 2)
   (vority :type vority)
   (food :type food :max-cardinality 6)
   (geo-period :type geo-period)
   (years-bp-start :type integer) ;; millions of years before present
   (years-bp-end :type integer) ;; millions of years before present
   (hippedness :type hippedness)
   (weapon :type symbol)
   (dinogroup :type dinogroup-type)))

(def-class dinogroup-type ())

(def-domain-class saurischia (dinosaur) ;; ORDER Lizard-hipped
  ())

(def-domain-class ornithischia (dinosaur) ;; ORDER Bird-hipped
  ()) 

(def-domain-class sauropodomorpha (saurischia) ;; SUBORDER Lizard-hipped herbivorous
  ())

(def-domain-class theropoda (saurischia) ;; SUBORDER Lizard-hipped carnivorous
  ())  
  
(def-domain-class cerapoda (ornithischia) ;; SUBORDER Bird-hipped bipedal
  ())

(def-domain-class thyreophora (ornithischia) ;; SUBORDER Bird-hipped plated and armoured
  ())
  
(def-domain-class ornithopoda (cerapoda) ;; INFRAORDER 
  ())    
 
(def-domain-class pachycephalosauria (cerapoda) ;; INFRAORDER 
  ())
   
(def-domain-class ceratopsia (cerapoda) ;; INFRAORDER 
  ())
  
(def-domain-class stegosauria (thyreophora) ;; INFRAORDER [**same as stegosauroidea?**]
  ())  

;;; personal2.stthomas.edu/jstweet/stegosauroidea.htm
;;; as far as I can see stegosauria and stegosauroidea are the same
(def-domain-class stegosauroidea (dinosaur)
  "Most stegosauroids have spines from the mid-back to the tail, and small plates 
   over the neck; Stegosaurus is unique in its large plates and not having either 
   spines graded from plates or shoulder spikes - personal2.stthomas.edu"
  ((plate-rows :type integer) ;;may be 0
   (plate-number :type integer) ;;may be 0
   (plate-location :type dinosaur-body-part)
   (spine-rows :type integer) ;;may be 0
   (spine-number :type integer) ;;may be 0
   (spine-location :type dinosaur-body-part) 
   (tail-armour :type tail-armour)))  

;;; inherit from both for now 
(def-domain-class stegosauridae (stegosauria stegosauroidea) ;; FAMILY 
  ())
 
(def-domain-class ankylosauria (thyreophora) ;; INFRAORDER 
  ()) 
       
(def-domain-class prosauropoda (sauropodomorpha) ;; INFRAORDER 
  ())
        
(def-domain-class sauropoda (sauropodomorpha) ;; INFRAORDER 
  ())    
 
(def-domain-class ceratosauria (theropoda) ;; INFRAORDER 
  ())
   
(def-domain-class tetanurae (theropoda) ;; INFRAORDER 
  ()) 
  
(def-domain-class ceratopsidae (ceratopsia) ;; FAMILY 
  ())
            
 
;;; INSTANCES
(def-domain-instance albertosaurus theropoda
  ((dname  albertosaurus)
   (name-meaning   "alberta lizard")
   (discovery-site   north-america)
   (length   9) 
   (height   3.5) 
   (mass   1500) 
   (pedality   bipedal)
   (teeth-type   saw-edged   flesh-slicing)
   (vority   carnivore)
   (food   plants)
   (geo-period   cretaceous)
   (years-bp-start   76) 
   (years-bp-end   74)
   (hippedness   lizard-hipped)
   (dinogroup   theropod)
   )
  )
   
(def-domain-instance allosaurus theropoda
  ((dname   allosaurus)
   (name-meaning   "different reptile")
   (discovery-site   africa   north-america   australia)
   (length   12) 
   (height   5) 
   (mass   2000) 
   (pedality   bipedal)
   (teeth-type   serrated-edge   dagger-like)
   (vority   carnivore)
   (food   stegosaurus   diplodocus   dinosaur)
   (geo-period   jurassic)
   (years-bp-start   153) 
   (years-bp-end   135)
   (hippedness   lizard-hipped)
   (dinogroup   theropod)
   )
  )

(def-domain-instance baryonyx theropoda
  ((dname   baryonyx)
   (name-meaning   "heavy claw")
   (discovery-site   europe)
   (length   10) 
   (height   4) 
   (mass   2000) 
   (pedality   bipedal)
   (teeth-type   finely-serrated-edge   sharp)
   (vority   carnivore)
   (food   fish   iguanodon)
   (geo-period   cretaceous)
   (years-bp-start   125) 
   (years-bp-end   125)
   (hippedness   lizard-hipped)
   (dinogroup   theropod)
   )
  )

(def-domain-instance camarasaurus sauropoda
  ((dname   camarasaurus)
   (name-meaning   "chambered lizard")
   (discovery-site   north-america)
   (length   18) 
   (height   9) 
   (mass   20000) 
   (pedality   quadripedal)
   (teeth-type   chisel-like   sharp-snipping)
   (vority   herbivore)
   (food   tough-plant-material)
   (geo-period   jurassic)
   (years-bp-start   150) 
   (years-bp-end   140)
   (hippedness   lizard-hipped)
   (dinogroup   sauropod)
   )
  )

 (def-domain-instance centrosaurus ceratopsidae
  ((dname centrosaurus)
   (name-meaning "horned lizard")
   (discovery-site north-america)
   (length 6) 
   (height 2) 
   (mass 1000) 
   (pedality quadripedal)
   (teeth-type horny-beak)
   (vority herbivore)
   (food tough-plant-material)
   (geo-period cretaceous)
   (years-bp-start 76) 
   (years-bp-end 74)
   (hippedness bird-hipped)
   (dinogroup ceratopian)
   )
  )

 (def-domain-instance chasmosaurus ceratopsidae
  ((dname chasmosaurus)
   (name-meaning "ravine reptile")
   (discovery-site north-america)
   (length 5.2) 
   (height 3.6) 
   (mass 2500) 
   (pedality quadripedal)
   (teeth-type horny-beak)
   (vority herbivore)
   (food plant-material)
   (geo-period cretaceous)
   (years-bp-start 80) 
   (years-bp-end 65)
   (hippedness bird-hipped)
   (dinogroup ceratopian)
   )
  )

(def-domain-instance coelophysis theropoda
  ((dname coelophysis)
   (name-meaning "hollow form")
   (discovery-site north-america)
   (length 3) 
   (height 2) 
   (mass 27) 
   (pedality bipedal)
   (teeth-type small-sharp)
   (vority carnivore)
   (food vertebrates)
   (geo-period triassic)
   (years-bp-start 225) 
   (years-bp-end 220)
   (hippedness lizard-hipped)
   (dinogroup theropod)
   )
  )

(def-domain-instance compsognathus theropoda
  ((dname compsognathus)
   (name-meaning "pretty jaw")
   (discovery-site europe)
   (length 1) 
   (height 0.7) 
   (mass 3.6) 
   (pedality bipedal)
   (teeth-type sharp)
   (vority carnivore)
   (food vertebrates)
   (geo-period jurassic)
   (years-bp-start 145) 
   (years-bp-end 140)
   (hippedness lizard-hipped)
   (dinogroup theropod)
   )
  )

(def-domain-instance deinonychus theropoda
  ((dname deinonychus)
   (name-meaning "pretty jaw")
   (discovery-site north-america)
   (length 3) 
   (height 1.5) 
   (mass 75) 
   (pedality bipedal)
   (teeth-type backward-curving)
   (vority carnivore)
   (food vertebrates plant-eating-dinosaurs)
   (geo-period cretaceous)
   (years-bp-start 110) 
   (years-bp-end 100)
   (hippedness lizard-hipped)
   (dinogroup theropod)
   )
  )

(def-domain-instance diplodocus sauropoda
  ((dname diplodocus)
   (name-meaning "double beam")
   (discovery-site north-america)
   (length 26) 
   (height 8) 
   (mass 10000) 
   (pedality quadripedal)
   (teeth-type comb-like-rows)
   (vority herbivore)
   (food tree-leaves soft-plant-leaves)
   (geo-period jurassic)
   (years-bp-start 155) 
   (years-bp-end 145)
   (hippedness lizard-hipped)
   (dinogroup sauropod)
   )
  )

(def-domain-instance dromaeosaurus dinosaur
  ((dname dromaeosaurus)
   (name-meaning "running lizard")
   (discovery-site north-america)
   (length 1.8) 
   (height 0.8) 
   (mass 15) 
   (pedality bipedal)
   (teeth-type small-razorsharp-backwardcurving)
   (vority carnivore)
   (food vertebrates)
   (geo-period cretaceous)
   (years-bp-start 76) 
   (years-bp-end 74)
   (hippedness lizard-hipped)
   (dinogroup theropod)
   )
  )

(def-domain-instance edmontosaurus ornithopoda
  ((dname edmontosaurus)
   (name-meaning "edmonton lizard")
   (discovery-site north-america)
   (length 13) 
   (height 3.5) 
   (mass 3400) 
   (pedality bipedal-or-quadripedal)
   (teeth-type horny-beak 1000-grinding-cheek-teeth)
   (vority herbivore)
   (food tough-pine-needles cones twigs)
   (geo-period cretaceous)
   (years-bp-start 71) 
   (years-bp-end 65)
   (hippedness bird-hipped)
   (dinogroup ornithopod)
   )
  )

(def-domain-instance euoplocephalus ankylosauria
  ((dname euoplocephalus)
   (name-meaning "well-armoured head")
   (discovery-site north-america)
   (length 6) 
   (height 1.8) 
   (mass 2000) 
   (pedality quadripedal)
   (teeth-type horny-beak chewing-teeth)
   (vority herbivore)
   (food plant-material)
   (geo-period cretaceous)
   (years-bp-start 76) 
   (years-bp-end 70)
   (hippedness bird-hipped)
   (dinogroup ankylosaur)
   )
  )

(def-domain-instance gallimimus theropoda
  ((dname gallimimus)
   (name-meaning "fowl mimic")
   (discovery-site asia)
   (length 5.6) 
   (height 3) 
   (mass 200) 
   (pedality bipedal)
   (teeth-type horny-beak no-teeth)
   (vority omnivore)
   (food plants insects lizards)
   (geo-period cretaceous)
   (years-bp-start 74) 
   (years-bp-end 70)
   (hippedness lizard-hipped)
   (dinogroup theropod)
   )
  )

(def-domain-instance hypsilophodon ornithopoda
  ((dname hypsilophodon)
   (name-meaning "high-ridge tooth")
   (discovery-site north-america europe)
   (length 2) 
   (height 0.8) 
   (mass 50) 
   (pedality bipedal)
   (teeth-type horny-beak broad-chisel-like-teeth)
   (vority herbivore)
   (food plant-material)
   (geo-period cretaceous)
   (years-bp-start 125) 
   (years-bp-end 125)
   (hippedness bird-hipped)
   (dinogroup ornithopod)
   )
  )


(def-domain-instance iguanodon ornithopoda
  ((dname iguanodon)
   (name-meaning "iguana tooth")
   (discovery-site africa north-america europe asia)
   (length 10) 
   (height 5) 
   (mass 4500) 
   (pedality bipedal-or-quadripedal)
   (teeth-type horny-beak chewing-cheek-teeth)
   (vority herbivore)
   (food plant-material)
   (geo-period cretaceous)
   (years-bp-start 135) 
   (years-bp-end 125)
   (hippedness bird-hipped)
   (dinogroup ornithopod)
   )
  )

(def-domain-instance maiasaura ornithopoda
  ((dname maiasaura)
   (name-meaning "good mother lizard")
   (discovery-site north-america)
   (length 9) 
   (height 2.3) 
   (mass 2500) 
   (pedality bipedal-or-quadripedal)
   (teeth-type horny-beak chewing-teeth)
   (vority herbivore)
   (food plant-material)
   (geo-period cretaceous)
   (years-bp-start 80) 
   (years-bp-end 75)
   (hippedness bird-hipped)
   (dinogroup ornithopod)
   )
  )

(def-domain-instance massospondylus prosauropoda
  ((dname massospondylus)
   (name-meaning "massive vertebrae")
   (discovery-site north-america)
   (length 4) 
   (height 1) 
   (mass 70) 
   (pedality bipedal-or-quadripedal)
   (teeth-type rounded-front-teeth flat-back-teeth)
   (vority herbivore)
   (food tough-plant-material)
   (geo-period triassic)
   (years-bp-start 208) 
   (years-bp-end 204)
   (hippedness lizard-hipped)
   (dinogroup prosauropod)
   )
  )

(def-domain-instance orodromeus ornithopoda
  ((dname orodromeus)
   (name-meaning "mountain runner")
   (discovery-site north-america)
   (length 2.5) 
   (height 1) 
   (mass 50) 
   (pedality bipedal)
   (teeth-type horny-beak grinding-cheek-teeth)
   (vority herbivore)
   (food tough-plant-material)
   (geo-period cretaceous)
   (years-bp-start 74) 
   (years-bp-end 74)
   (hippedness bird-hipped)
   (dinogroup ornithopod)
   )
  )

(def-domain-instance oviraptor theropoda
  ((dname oviraptor)
   (name-meaning "egg thief")
   (discovery-site asia)
   (length 1.8) 
   (height 0.8) 
   (mass 20) 
   (pedality bipedal)
   (teeth-type beak no-teeth)
   (vority omnivore)
   (food hard-fruits eggs shellfish?)
   (geo-period cretaceous)
   (years-bp-start 70) 
   (years-bp-end 65)
   (hippedness lizard-hipped)
   (dinogroup theropod)
   )
  )

(def-domain-instance pachycephalosaurus ornithopoda
  ((dname pachycephalosaurus)
   (name-meaning "thick-headed lizard")
   (discovery-site north-america)
   (length 8) 
   (height 6) 
   (mass 3000) 
   (pedality bipedal)
   (teeth-type flat-curved serrated)
   (vority herbivore)
   (food plant-material)
   (geo-period cretaceous)
   (years-bp-start 76) 
   (years-bp-end 65)
   (hippedness bird-hipped)
   (dinogroup ornithopod)
   )
  )

(def-domain-instance parasaurolophus ornithopoda
  ((dname parasaurolophus)
   (name-meaning "like saurolophus - the ridge lizard")
   (discovery-site north-america)
   (length 10) 
   (height 5.2) 
   (mass 3500) 
   (pedality bipedal-or-quadripedal)
   (teeth-type beak-jaw toothed-jaw)
   (vority herbivore)
   (food pine-needles tree-leaves)
   (geo-period cretaceous)
   (years-bp-start 76) 
   (years-bp-end 65)
   (hippedness bird-hipped)
   (dinogroup ornithopod)
   )
  )

(def-domain-instance psittacosaurus ornithopoda
  ((dname psittacosaurus)
   (name-meaning "parrot lizard")
   (discovery-site asia)
   (length 2.5) 
   (height 1) 
   (mass 50) 
   (pedality bipedal-or-quadripedal)
   (teeth-type beak)
   (vority herbivore)
   (food plant-material)
   (geo-period cretaceous)
   (years-bp-start 95) 
   (years-bp-end 90)
   (hippedness bird-hipped)
   (dinogroup ornithopod)
   )
  )

(def-domain-instance tenontosaurus ornithopoda
  ((dname tenontosaurus)
   (name-meaning "sinew lizard")
   (discovery-site north-america)
   (length 7.3) 
   (height 2.5) 
   (mass 900) 
   (pedality quadripedal)
   (teeth-type beak chopping-teeth)
   (vority herbivore)
   (food plant-material)
   (geo-period cretaceous)
   (years-bp-start 110) 
   (years-bp-end 100)
   (hippedness bird-hipped)
   (dinogroup ornithopod)
   )
  )
 
(def-domain-instance triceratops ceratopsidae
  ((dname triceratops)
   (name-meaning "three-horned face")
   (discovery-site north-america)
   (length 9) 
   (height 3) 
   (mass 5500) 
   (pedality quadripedal)
   (teeth-type horny-beak shearing-teeth)
   (vority herbivore)
   (food tough-palm-fronds)
   (geo-period cretaceous)
   (years-bp-start 72) 
   (years-bp-end 67)
   (hippedness bird-hipped)
   (dinogroup ceratopian) ;; = ceratopsidae?
   )
  )
   
(def-domain-instance troodon theropoda
  ((dname troodon)
   (name-meaning "wounding tooth")
   (discovery-site north-america)
   (length 2) 
   (height 1.3) 
   (mass 40) 
   (pedality bipedal)
   (teeth-type curved-flattened-coarselyserrated-teeth)
   (vority carnivore)
   (food small-vertebrates lizards mammals)
   (geo-period cretaceous)
   (years-bp-start 76) 
   (years-bp-end 74)
   (hippedness lizard-hipped)
   (dinogroup theropod)
   )
  )


   
(def-domain-instance tyrannosaurus theropoda
  ((dname tyrannosaurus)
   (name-meaning "tyrant lizard")
   (discovery-site north-america)
   (length 12) 
   (height 5.6) 
   (mass 7000) 
   (pedality bipedal)
   (teeth-type 60-sawedged-bonecrushing-pointed-teeth)
   (vority carnivore)
   (food vertebrates)
   (geo-period cretaceous)
   (years-bp-start 67) 
   (years-bp-end 65)
   (hippedness lizard-hipped)
   (dinogroup theropod)
   )
  )  
   
;;;; ARCHAEOPTERYX is defined from
;;;; Collins Gem Dinosaurs by David Lambert 2000
   
(def-domain-instance archaeopteryx dinosaur
  ((dname archaeopteryx)
   (name-meaning "ancient wing")
   (discovery-site europe)
   (length 0.5) 
   (height 0.5) ;;?? 
   ;;(mass unknown) 
   (pedality bipedal)
   (teeth-type small-sharp-teeth)
   (vority carnivore)
   (food small-animals)
   (geo-period jurassic)
   (years-bp-start 155) ;;??  
   (years-bp-end 155) ;;?? 
   (hippedness lizard-hipped)
   (dinogroup theropod)
   )
  )  

;;;; VELOCIRAPTOR is defined from
;;;; Collins Gem Dinosaurs by David Lambert 2000
   
(def-domain-instance velociraptor dinosaur
  ((dname velociraptor)
   (name-meaning "quick robber")
   (discovery-site asia)
   (length 1.8) 
   (height 0.8) ;;?? 
   ;;(mass unknown) 
   (pedality bipedal)
   (teeth-type sharp-teeth)
   (vority carnivore)
   (food animals)
   (geo-period cretaceous)
   (years-bp-start 80) ;;??  
   (years-bp-end 80) ;;?? 
   (hippedness lizard-hipped)
   (dinogroup theropod)
   )
  )  
;;; + switchblade on 2nd toe  


(def-domain-class dromaeosauridae (dinosaur)
  ((weapon switchblade-2nd-toe)))

;;;; STEGOSAURUS is defined a bit more carefully as part of a
;;;; class hierarchy with species as instances

;(def-domain-class stegosauroidea (dinosaur)
;  "Most stegosauroids have spines from the mid-back to the tail, and small plates 
;   over the neck                        ; Stegosaurus is unique in its large plates and not having either 
;   spines graded from plates or shoulder spikes - personal2.stthomas.edu"
;  ((plate-rows :type integer)           ;;may be 0
;   (plate-number :type integer)         ;;may be 0
;   (plate-location :type dinosaur-body-part)
;   (spine-rows :type integer)           ;;may be 0
;   (spine-number :type integer)         ;;may be 0
;   (spine-location :type dinosaur-body-part) 
;   (tail-armour :type tail-armour)))

;(def-domain-class stegosauridae (stegosauroidea)
;  ())

(def-domain-class stegosaurus (stegosauridae)
  ((dname :value stegosaurus)
   (name-meaning :value "covered/roof lizard")
   (discovery-site :value north-america)
   (length :default-value 9) 
   (height :default-value 2.7) 
   (mass :default-value 3100) 
   (pedality :value quadripedal)
   (teeth-type :value beak :value small-cheek-teeth)
   (vority :value herbivore)
   (food :value ferns :value smaller-club-mosses :value cycads :value horsetails :value bushy-conifers)
   (geo-period :value jurassic)
   (years-bp-start :default-value 156) 
   (years-bp-end :default-value 140)
   (hippedness :value bird-hipped)
   (dinogroup :value thyreophora)
   (plate-rows :value 2)
   (plate-number :value 17)
   (plate-location :value back)
   (tail-spikes-number :type integer)
   )
  )

(def-domain-instance stegosaurus-ungulatis stegosaurus
  ((tail-spikes-number 8)
   )
  )

(def-domain-instance stegosaurus-stenops stegosaurus
  ((tail-spikes-number 4)
   )
  )

(def-domain-instance tuojiangosaurus stegosaurus
  ((dname tuojiangosaurus)
   (name-meaning "tuijiang lizard")
   (discovery-site asia)
   (length 7) 
   (height 2) 
   (mass 1500) 
   (pedality quadripedal)
   (teeth-type small-snipping-teeth)
   (vority herbivore)
   (food soft-shoots ferns)
   (geo-period jurassic)
   (years-bp-start 157) 
   (years-bp-end 154)
   (hippedness bird-hipped)
   (dinogroup stegosauridae)
   (spine-location shoulder) 
   )
  )


;;; Example input for identification


(def-class description ()
  ((has-dino-observables :type list)))

(def-domain-instance dino1 description
  ((has-dino-observables
    '
    ((discovery-site gobi-desert) 
     (length 6.2) 
     (height 2.2) 
     (mass 1200)
     (back-legs 1.4)
     (front-legs 0.2)
     (teeth-type small-snipping-teeth)))))

(def-domain-instance dino2 description
  ((has-dino-observables
    '
       ((name-meaning "massive vertebrae")
   (discovery-site north-america)
   (length 4) 
   (height 1) 
   (mass 70) 
   (pedality bipedal-or-quadripedal)))))

(def-domain-instance dino3 description
  ((has-dino-observables
    '
      ((vority herbivore)
   (food plant-material)
   (geo-period cretaceous)
   (years-bp-start 135) 
   (years-bp-end 125)
   (hippedness bird-hipped)
   (dinogroup ornithopod)))))


;;; rule location -> continent
;;; rules dimensions -> canonical dimensions
;;; rule size-of-limbs to pedality; teeth -> vority
(def-rule length-ok
  ((length-ok ?len)
   IF
   (dinosaur ?d)
   (length ?d ?can-len)
   (> (/ ?can-len 10) (abs (- ?can-len ?len))) ;;; diff < canonical length / 10 ??absolute
   ))

;;; will this work - for length, height, mass - all should be within a tenth of canonical
;;; e.g. (ask (dimensions-match length 6.2))
(def-rule dimensions-match
  ((dimensions-match ?dim ?val) 
   IF
   (dinosaur ?d)
   (?dim ?d ?can-val)
   (> (/ ?can-val 10) (abs (- ?can-val ?val))) ;;; diff < canonical length / 10 ??absolute
   )) 


;;;; Needed since some dinosaurs eat generic dinosaurs
(tell (dino-syn PLANT-EATING-DINOSAURS))
(tell (dino-syn DINOSAUR))

;;;;;;; RELATIONS
(def-relation feeds-on (?predator ?prey)
  "Dinosaurs eat all sorts of things"
  :sufficient 
             (food ?predator ?prey))

(def-relation is-food-for (?prey ?predator)
  "Things are eaten by dinosaurs"
  :sufficient 
             (food ?predator ?prey))

(def-relation cannibal-dinosaurs (?predator ?prey)
  "Some dinosaurs eat other dinosaurs"
  :constraint (and (dinosaur ?predator)
                   (dinosaur ?prey))
  :sufficient 
  (or
   ((and 
             (dinosaur ?predator) 
             (dinosaur ?prey) 
             (food ?predator ?prey))
   (and
    (dinosaur ?predator)
    (food ?predator ?prey)
    (dino-syn ?prey)))))

(def-relation is-bigger-than (?x ?m1 ?y ?m2)
  "Some dinosaurs are bigger than others"
  :sufficient 
  (and
   (dinosaur ?x)
   (dinosaur ?y)
   (not (= ?x ?y))
   (mass ?x ?m1)
   (mass ?y ?m2)
   (> ?m1 ?m2)))

(def-relation is-smaller-than (?x ?m1 ?y ?m2)
  "Some dinosaurs are bigger than others"
  :sufficient 
  (and
   (dinosaur ?x)
   (dinosaur ?y)
   (not (= ?x ?y))
   (mass ?x ?m1)
   (mass ?y ?m2)
   (< ?m1 ?m2)))

;;;; RULES
(def-rule d-cans
  ((d-cans ?predator ?prey)
   IF
   (dinosaur ?predator) 
   (dinosaur ?prey) 
   (food ?predator ?prey)))


;;; FILE: dino2a.lisp PART 2
;;; **************************************************************************
;;; Formal fossil descriptions
;;; For matching algorithms e.g. Heuristic Classsification

;;; A dinosaur is made up of areas, each of which can have its
;;; description e.g., skull, leg, arm, tail etc.
(def-domain-class dinosaur-body-area ()
  ((bname :type bname)
   )
  )

;;; Amniote = animla with membrane around embryo
;;; www.kheper.auz.com/gaia/biosphere/vertebrates/osteology/skull.html 
(def-domain-class amniote-skull ()
  ((has-braincase :type skull-element)
   (has-epipterygoid :type skull-element)
   (has-ectopterygoid :type skull-element)
   (has-frontal :type skull-element)
   (has-jugal-bone :type skull-element)
   (has-lacrymal :type skull-element)  ;; or lacrimal
   (has-maxilla :type skull-element)
   (has-nasal :type skull-element)   
   (has-parietal :type skull-element)
   (has-palatine :type skull-element)
   (has-premaxilla :type skull-element)
   (has-postparietal :type skull-element)
   (has-postorbital :type skull-element)
   (has-postfrontal :type skull-element)
   (has-prefrontal :type skull-element)
   (has-parasphenoid :type skull-element)
   (has-pterygoid :type skull-element)
   (has-quadrate :type skull-element)
   (has-quatratojugal :type skull-element)
   (has-stapes :type skull-element)
   (has-squamosal :type skull-element)
   (has-supratemporal :type skull-element)
   (has-tabular :type skull-element)
   (has-vomer :type skull-element) 
   )
  )
 
 
;;; a partial set of skull elements
;;; types could be integers for areas or other dimensions
;;; From: www.talkorigins.org/faqs/archaeopteryx/info.html
;;; This class contains the elements mentioned in the above but not
;;; included in the definition of an Amniote skull
(def-domain-class dinosaur-skull (amniote-skull)
  ((has-nares :type skull-element)
   (has-preorbital-fossa :type skull-element)
   (has-preorbital-bar :type skull-element)
   (has-postorbital-fossa :type skull-element)
   (has-antorbital-fossa :type skull-element)
   (has-orbit :type skull-element)
   (has-jugal-bone :type skull-element)
   (has-dentary :type skull-element)
   (has-retroarticular-process :type skull-element)
   (has-premaxilla :type skull-element)
   (has-ectopterygoid-hook :type skull-element) 
   )
  )
  
(def-domain-class archaeopteryx-skull (dinosaur-skull)
  (
  ))
    
(def-domain-instance archaeopteryx-skull-1 archaeopteryx-skull
  ((has-maxilla maxilla-arch-1)
   (has-nares nares-arch-1)
   (has-preorbital-fossa preorbital-fossa-arch-1)
   (has-preorbital-bar preorbital-bar-arch-1)
   (has-postorbital-fossa postorbital-fossa-arch-1)
   (has-antorbital-fossa antorbital-fossa-arch-1)
   (has-orbit orbit-arch-1)
   (has-jugal-bone jugal-bone-arch-1)
   (has-quadrate quadrate-arch-1)
   (has-dentary dentary-arch-1)
   (has-retroarticular-process retroarticular-process-arch-1)
   (has-premaxilla premaxilla-arch-1)
   (has-lacrymal lacrymal-arch-1)  
   (has-vomer vomer-arch-1) 
   (has-ectopterygoid-hook ectopterygoid-hook-arch-1) 
   )
  ) 

(def-domain-instance archaeopteryx-skull-2 archaeopteryx-skull
  ((has-maxilla maxilla-arch-1)
   (has-nares nares-arch-1)
   (has-preorbital-fossa preorbital-fossa-arch-1)
   (has-preorbital-bar preorbital-bar-arch-1)
   (has-postorbital-fossa postorbital-fossa-arch-1)
   (has-antorbital-fossa antorbital-fossa-arch-1)
   (has-orbit orbit-arch-1)
   (has-jugal-bone jugal-bone-arch-1)
   (has-quadrate quadrate-arch-2)
   (has-dentary dentary-arch-1)
   (has-retroarticular-process retroarticular-process-arch-1)
   (has-premaxilla premaxilla-arch-1)
   (has-lacrymal lacrymal-arch-1)  
   (has-vomer vomer-arch-1) 
   (has-ectopterygoid-hook ectopterygoid-hook-arch-1) 
   )
  )   
  
(def-domain-class skull-element (dinosaur-body-area)
  ((has-area :type integer)
   (has-length :type integer)
   (has-width :type integer)
   (has-qualitative-desc :type descriptor)
   (is-connected-to :type skull-element) ;;???
   )
  )  
  
(def-domain-class archaeopteryx-skull-element (skull-element)
  (
  ))

(def-domain-class archaeopteryx-nares (archaeopteryx-skull-element)
  ((is-bounded-by-top :type skull-element)
   (is-bounded-by-bottom :type skull-element)
   )
  )  
  
(def-domain-class archaeopteryx-preorbital-bar (archaeopteryx-skull-element)
  ((separates1 :type skull-element)
   (separates2 :type skull-element)
   )
  )  
     
(def-domain-instance maxilla-arch-1 archaeopteryx-skull-element
  ((bname maxilla-arch-1)
   (has-qualitative-desc sharply-tapered)))

(def-domain-instance nares-arch-1 archaeopteryx-nares
  ((bname nares-arch-1)
   (has-qualitative-desc long-elliptical-external)
   (is-bounded-by-top premaxilla-arch-1)
   (is-bounded-by-bottom nasal-arch-1)))

(def-domain-instance preorbital-fossa-arch-1 archaeopteryx-skull-element
  ((bname preorbital-fossa-arch-1)
   (has-qualitative-desc large)))

(def-domain-instance preorbital-bar-arch-1 archaeopteryx-preorbital-bar
  ((bname preorbital-bar-arch-1)
   (has-qualitative-desc slender-vertical)
   (separates1 antorbital-fossa-arch-1)
   (separates2 orbit-arch-1)))

(def-domain-instance postorbital-fossa-arch-1 archaeopteryx-skull-element
  ((bname postorbital-fossa-arch-1)
   (has-qualitative-desc large-triangular)))
   
(def-domain-instance orbit-arch-1 archaeopteryx-skull-element
  ((bname orbit-arch-1)
   (has-qualitative-desc large-circular)))

(def-domain-instance jugal-bone-arch-1 archaeopteryx-skull-element
  ((bname jugal-bone-arch-1)
   (has-qualitative-desc thin-straight separate)))
   
(def-domain-instance quadrate-arch-1 archaeopteryx-skull-element
  ((bname quadrate-arch-1)
   (has-qualitative-desc stout-moderatelylong-inclinedforward)))

(def-domain-instance quadrate-arch-2 archaeopteryx-preorbital-bar
  ((bname quadrate-arch-2)
   (separates1 antorbital-fossa-arch-1)
   (separates2 orbit-arch-1)
   (has-qualitative-desc stout-moderatelylong-inclinedforward)))

(def-domain-instance dentary-arch-1 archaeopteryx-skull-element
  ((bname dentary-arch-1)
   (has-qualitative-desc shallow-with-bend-behind-toothrow)))
   
(def-domain-instance retroarticular-process-arch-1 archaeopteryx-skull-element
  ((bname retroarticular-process-arch-1)
   (has-qualitative-desc long)))
 
(def-domain-instance premaxilla-arch-1 archaeopteryx-skull-element
  ((bname premaxilla-arch-1)
   (has-qualitative-desc separate)))
   
(def-domain-instance lacrymal-arch-1 archaeopteryx-skull-element
  ((bname lacrymal-arch-1)
   (has-qualitative-desc separate)))

(def-domain-instance vomer-arch-1 archaeopteryx-skull-element
  ((bname vomer-arch-1)
   (has-qualitative-desc single)))
   
(def-domain-instance ectopterygoid-hook-arch-1 archaeopteryx-skull-element
  ((bname ectopterygoid-hook-arch-1)
   (has-qualitative-desc strongly-curved)))
   
   
;;; -----------------------------------------------------------------------   
   

(def-relation is-bounded-by (?ds ?bone ?sep1 ?sep2)
  "Boundaries of regions"
  :sufficient 
  (and
   (dinosaur-skull ?ds)
   (or 
    (has-maxilla ?ds ?bone)
    (has-nares ?ds ?bone)
    (has-preorbital-fossa ?ds ?bone)
    (has-preorbital-bar ?ds ?bone)
    (has-postorbital-fossa ?ds ?bone)
    (has-antorbital-fossa ?ds ?bone)
    (has-orbit ?ds ?bone)
    (has-jugal-bone ?ds ?bone)
    (has-quadrate ?ds ?bone)
    (has-dentary ?ds ?bone)
    (has-retroarticular-process ?ds ?bone)
    (has-premaxilla ?ds ?bone)
    (has-lacrymal ?ds ?bone)  
    (has-vomer ?ds ?bone) 
    (has-ectopterygoid-hook ?ds ?bone))
   (separates1 ?bone ?sep1)
   (separates2 ?bone ?sep2)))  
 
;;; change!!   
(def-relation separates (?ds ?x ?s1 ?s2)
  "Boundaries of regions"
  :sufficient 
  (and
   (dinosaur-skull ?ds)
   (bone-area ?x)
   (bone-area ?s1)
   (bone-area ?s2)
   ))   
  

;;; rules qualitative description as above from quantitative?