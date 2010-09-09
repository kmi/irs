;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology aktive-portal-ontology2)


(def-class ACTIVITY (temporal-thing)
  ((has-sub-activity :type activity)
   (has-status :type activity-status)
   (uses-resource :type thing)
   (produces-output :type thing)))

(def-class activity-status (string))


(def-class project (activity) ?x
  ((has-leading-organization :type organization)
   (produces-output :type (or event technology method organization
                              information-bearing-object))
   (involves-organization :type organization :min-cardinality 1)
   (has-goals :type string)
   (has-project-leader :type person)
   (has-project-member :type person :min-cardinality 1)
   (funding-source :type organization)
   (has-web-address :type URL)
   (addresses-generic-area-of-interest 
    :type generic-area-of-interest)
   )
  :constraint (and (forall ?y
                      (=> (has-leading-organization ?x ?y)
                          (involves-organization ?x ?y)))
                   (forall ?y
                      (=> (has-project-leader ?x ?y)
                          (has-project-member ?x ?y)))))


(def-relation PROJECT-INVOLVES-ORGANIZATION-UNIT (?p ?u)
  "It is sufficient that somebody in unit ?u works in project ?p"
  :constraint (and (project ?p)(organization-unit ?u))
  :sufficient (and (project ?p)(organization-unit ?u)
                   (has-project-member ?p ?x)
                   (works-in-unit ?x ?u)))
                

;;;A random example....
(def-instance  AKT-PROJECT project
  ((has-leading-organization university-of-southampton)
   (involves-organization the-open-university
                          university-of-edinburgh
                          university-of-sheffield
                          university-of-aberdeen
                          university-of-southampton)
   (has-project-leader nigel-shadbolt)
   (has-project-member john-domingue
                       nigel-shadbolt)
   (funding-source epsrc)
   (has-web-address "http://www.aktors.org")
   (addresses-generic-area-of-interest knowledge-management
                                       knowledge-acquisition
                                       incidental-ka
                                       semantic-web-area 
                                       knowledge-retrieval
                                       knowledge-reuse 
                                       information-extraction
                                       knowledge-maintenance 
                                       knowledge-publishing
                                       dynamic-linking
                                       ontologies)))





