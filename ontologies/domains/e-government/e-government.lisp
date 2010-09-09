;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology e-government)

(def-class e-government-subject ()
  ((pretty-name :type string)))


(def-class arts-crafts-media (e-government-subject))
(def-class beliefs-and-faiths (e-government-subject))
(def-class business-and-employment (e-government-subject))
(def-class children-and-families (e-government-subject))
(def-class disability (e-government-subject))
(def-class education-careers-and-training (e-government-subject))
(def-class environment-and-transport (e-government-subject))
(def-class ethnic-origin-and-language (e-government-subject))
(def-class government-and-politics (e-government-subject))
(def-class health (e-government-subject))
(def-class law (e-government-subject))
(def-class leisure (e-government-subject))
(def-class science-and-technology (e-government-subject))
(def-class social-issues-advice-and-support (e-government-subject))
(def-class sport (e-government-subject))

 
(def-class Access-for-Disabled-People (disability))
(def-class Disabled-People (disability))
(def-class Equipment-for-Disabled-People (disability))
(def-class Related-terms (disability))
(def-class Employment (disability)) 

(def-class Children (children-and-families)) 
(def-class Families (children-and-families)) 
(def-class Young-People (children-and-families)) 
(def-class Related-terms (children-and-families))
(def-class Family-Support-Services (children-and-families)) 
(def-class Parenting-Courses (children-and-families)) 

 
  
 

 
  


 
 