;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology aktive-portal-ontology2)

(def-relation ADDRESSES-GENERIC-AREA-OF-INTEREST (?X ?area)
  :constraint (generic-area-of-interest ?area)
  :sufficient (and (sub-area-of ?sub-area ?area)
                   (addresses-generic-area-of-interest ?x ?sub-area)))
                   
(def-class TECHNOLOGY (thing) 
  "By technology we mean engineered applications of science.
   I guess we are probably confining ourselves to tangible things
   but as I am not sure I will use thing as the direct superclass - 
   e.g., an algorithm is an intangible thing, but it could be seen as 
   a technology, if we give a broad interpretation of the term"
  ((has-author :type generic-agent)
   (owned-by :type legal-agent)
   (technology-builds-on :type technology)
   (supports-method :type method)
   (addresses-generic-area-of-interest :type generic-area-of-interest)))


(def-class METHOD (intangible-thing) 
  "Merrian-Webster has a good set of definitions for a method.
   They say it is 'a systematic procedure,
   technique, or mode of inquiry employed by or proper to a particular 
   discipline or art;  a systematic plan followed in presenting 
   material for instruction; a way, technique, or
   process of or for doing something; a body of skills or techniques'.
   This is very much also what we mean by method."
  ((has-author :type person)
   (owned-by :type organization)
   (method-builds-on :type method)
   (addresses-generic-area-of-interest 
    :type generic-area-of-interest)))

(def-class COMPUTING-TECHNOLOGY (technology))
 
(def-class SOFTWARE-TECHNOLOGY (computing-technology))

(def-class IMPLEMENTED-SYSTEM (software-technology)
  ((requires-hardware-platform :type hardware-platform) 
   (runs-on-operating-system :type operating-system) 
   (requires-software-technology :type software-technology)
   (has-status :type software-status 
               :documentation "Whether the software is released, alpha or beta")))

(def-class WEB-SITE (implemented-system Information-bearing-object)
  ((has-url :type url)))

(def-class PROGRAMMING-ENVIRONMENT (implemented-system)
  ((supports-language :type programming-language)))

(def-class WEB-BASED-SYSTEM (implemented-system)
  "A system which is accessible through the web"
  ((requires-hardware-platform-on-server-side :type hardware-platform) 
   (runs-on-operating-system-on-server-side :type operating-system) 
   (requires-software-technology-on-server-side :type software-technology)
   (requires-hardware-platform-on-client-side :type hardware-platform) 
   (runs-on-operating-system-on-client-side :type operating-system) 
   (requires-software-technology-on-client-side :type software-technology))
  :slot-renaming ((requires-software-technology-on-server-side requires-software-technology)
                  (requires-hardware-platform-on-server-side requires-hardware-platform)
                  (runs-on-operating-system-on-server-side runs-on-operating-system)))

(def-class WEB-BROWSER (implemented-system)
  "A web browser is not a web-based system!")

  

(def-class HARDWARE-TECHNOLOGY (computing-technology))

(def-class OPERATING-SYSTEM (implemented-system))

(def-class EDITOR (implemented-system)
  ((supports-language :type specification-or-computing-language)))

(def-class WEB-BASED-EDITOR (editor web-based-system))


(def-class HARDWARE-PLATFORM (hardware-technology))

(def-class SPECIFICATION-OR-COMPUTING-LANGUAGE (software-technology))

(def-class PROGRAMMING-LANGUAGE (specification-or-computing-language))

(def-class SPECIFICATION-LANGUAGE (specification-or-computing-language))

(def-class SCRIPTING-LANGUAGE (programming-language))


(def-class SOFTWARE-STATUS (intangible-thing))

(def-instance ALPHA-VERSION software-status)

(def-instance BETA-VERSION software-status)

(def-instance RELEASED-VERSION software-status)

(def-instance BROKEN-VERSION software-status)

(def-instance EXPERIMENTAL-VERSION software-status)


(def-class WEB-TECHNOLOGY (computing-technology)
  ((addresses-generic-area-of-interest :value web-research-area)))

(def-class SERVER-TECHNOLOGY (software-technology web-technology))

(def-class MULTIMEDIA-TECHNOLOGY (software-technology)
  ((addresses-generic-area-of-interest :value multimedia-research-area))) 

(def-class LANGUAGE-ENGINEERING-TECHNOLOGY (software-technology)
  ((addresses-generic-area-of-interest :value  language-engineering)))

(def-class INFORMATION-EXTRACTION-TECHNOLOGY (language-engineering-technology)
  ((addresses-generic-area-of-interest :value  INFORMATION-EXTRACTION )))

(def-class AGENT-TECHNOLOGY (software-technology)
  ((addresses-generic-area-of-interest :value AGENT-BASED-COMPUTING )))

(def-class SOFTWARE-VISUALIZATION-TECHNOLOGY (software-technology)
  ((addresses-generic-area-of-interest  SOFTWARE-VISUALIZATION)))

(def-class KNOWLEDGE-MODELLING-TECHNOLOGY (software-technology)
  ((addresses-generic-area-of-interest :value knowledge-modelling )))

(def-class ONTOLOGY-SPECIFICATION-LANGUAGE (specification-language 
                                            knowledge-modelling-technology)
  ((addresses-generic-area-of-interest :value ontologies)))



(def-class KNOWLEDGE-PROGRAMMING-LANGUAGE (programming-language 
                                           knowledge-modelling-technology)
  "These are knowledge modelling languages which have an interpreter and that
   can be used to build systems")

(def-class OPERATIONAL-ONTOLOGY-SPECIFICATION-LANGUAGE (ontology-specification-language 
                                                        knowledge-programming-language))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Below I define some instances to give examples of 
;;;the above categories


(def-instance CLIPS knowledge-programming-language)

(def-instance ONTOLINGUA ontology-specification-language)

(def-instance DAML+OIL operational-ontology-specification-language)

(def-instance OIL operational-ontology-specification-language)

(def-instance OCML operational-ontology-specification-language)

(def-instance JAVA programming-language)

(def-instance LISP programming-language)

(def-instance PROLOG programming-language)

(def-instance C++ programming-language)

(def-instance SMALLTALK programming-language)

(def-instance PERL programming-language)

(def-instance CGI scripting-language)

(def-instance JAVASCRIPT scripting-language)



(def-instance KMI-WEB-LISP-SERVER server-technology)

(def-instance HARLEQUIN-COMMON-LISP programming-environment
  ((supports-language lisp)
   (has-status released-version)
   (owned-by Xanalys)
   (RUNS-ON-OPERATING-SYSTEM windows2000 unix)
   ))

(def-instance GLOBAL-GRAPHICS-SOFTWARE public-company
  "About 600 employees - I guess we consider it a medium-sized organization"
  ((has-size medium-size)))

(def-instance XANALYS company
  ((subsidiary-of GLOBAL-GRAPHICS-SOFTWARE)
   (has-size small-size)))


(def-instance WINDOWS2000 operating-system)

(def-instance MAC-OS operating-system)

(def-instance UNIX operating-system)

(def-instance NETSCAPE-COMMUNICATOR-BROWSER web-browser)


(def-instance WEB-ONTO web-based-editor
  ((supports-language ocml)
   (has-status released-version)
   (has-author john-domingue)
   (owned-by The-Open-University)
   (RUNS-ON-OPERATING-SYSTEM-ON-CLIENT-SIDE windows2000 unix)
   (RUNS-ON-OPERATING-SYSTEM-ON-SERVER-SIDE windows2000  unix)
   (requires-software-technology-on-server-side  kmi-web-lisp-server harlequin-common-lisp)
   (requires-software-technology-on-client-side netscape-communicator-browser java)))
