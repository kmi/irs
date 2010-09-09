;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology profile)








(DEF-CLASS GET-FORM-DEFINITION-PROFILE
           (GOAL)
           ?GOAL
           ((HAS-INPUT-ROLE
             :VALUE
             TRANSACTION-ID
             :VALUE
             TYPE
             :VALUE
             ADMG)
            (HAS-INPUT-SOAP-BINDING
             :VALUE
             (TRANSACTION-ID "string")
             :VALUE
             (TYPE "string")
             :VALUE
             (ADMG "string"))
            (HAS-OUTPUT-ROLE :VALUE HAS-FORMDEF-RESULT)
            (HAS-OUTPUT-SOAP-BINDING :VALUE (HAS-FORMDEF-RESULT "xml"))
            (TRANSACTION-ID :TYPE INTEGER)
            (TYPE :TYPE STRING)
            (ADMG :TYPE STRING)
            (HAS-FORMDEF-RESULT :TYPE STRING)))

(DEF-CLASS GET-FORM-DEFINITION-PROFILE-M
           (WG-MEDIATOR)
           ?MEDIATOR
           ((HAS-SOURCE-COMPONENT :VALUE GET-FORM-DEFINITION-PROFILE)))



(DEF-CLASS GET-FORM-DEFINITION-PROFILE-W
           (WEB-SERVICE)
           ?WEB-SERVICE
           ((HAS-CAPABILITY
             :VALUE
             GET-FORM-DEFINITION-PROFILE-W-CAPABILITY)
            (HAS-INTERFACE
             :VALUE
             GET-FORM-DEFINITION-PROFILE-W-INTERFACE)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             GET-FORM-DEFINITION-PROFILE-W-NON-FUNC-PROPS)))

(DEF-CLASS GET-FORM-DEFINITION-PROFILE-W-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE GET-FORM-DEFINITION-PROFILE-M)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             GET-FORM-DEFINITION-PROFILE-W-NON-FUNC-PROPS)))

(def-class get-form-definition-profile-w-interface (interface)
  ((has-orchestration :value get-form-definition-profile-w-orchestration)
   (has-choreography :value get-form-definition-profile-w-choreography)))

(DEF-CLASS GET-FORM-DEFINITION-PROFILE-W-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-WEB-SERVICE-HOST :VALUE "137.108.25.26")
            (HAS-WEB-SERVICE-PORT :VALUE 3001)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/soap")
            (HAS-LISP-FUNCTION
             :VALUE
             XS-CFORM-DEF-FOR-ADM-GOAL-PROFILE)))

(DEF-CLASS LIFE-EVENT-CHANGE-ADDRESS-PROFILE
           (GOAL)
           ?GOAL
           ((HAS-INPUT-ROLE
             :VALUE
             TRANSACTION-ID
             :VALUE
             ADMG
             :VALUE
             HAS-ADDRESS-TYPE
             :VALUE
             HAS-STREET-NUMBER
             :VALUE
             HAS-STREET
             :VALUE
             HAS-LOCALITY
             :VALUE
             HAS-POSTAL-CODE)
            (HAS-INPUT-SOAP-BINDING
             :VALUE
             (TRANSACTION-ID "sexpr")
             :VALUE
             (ADMG "sexpr")
             :VALUE
             (HAS-ADDRESS-TYPE "sexpr")
             :VALUE
             (HAS-STREET-NUMBER "sexpr")
             :VALUE
             (HAS-STREET "sexpr")
             :VALUE
             (HAS-LOCALITY "sexpr")
             :VALUE
             (HAS-POSTAL-CODE "sexpr"))
            (HAS-OUTPUT-ROLE :VALUE HAS-CHANGE-ADDRESS-RESULT)
            (HAS-OUTPUT-SOAP-BINDING :VALUE (HAS-CHANGE-ADDRESS-RESULT "sexpr"))
            (TRANSACTION-ID :TYPE INTEGER)
            (ADMG :TYPE GOAL)
            (HAS-ADDRESS-TYPE :TYPE DOMICILE)
            (HAS-STREET-NUMBER :TYPE INTEGER)
            (HAS-STREET :TYPE STRING)
            (HAS-LOCALITY :TYPE STRING)
            (HAS-POSTAL-CODE :TYPE STRING)
            (HAS-CHANGE-ADDRESS-RESULT :TYPE STRING)))

(DEF-CLASS LIFE-EVENT-CHANGE-ADDRESS-PROFILE-M
           (WG-MEDIATOR)
           ?MEDIATOR
           ((HAS-SOURCE-COMPONENT
             :VALUE
             LIFE-EVENT-CHANGE-ADDRESS-PROFILE)))

(DEF-CLASS LIFE-EVENT-CHANGE-ADDRESS-PROFILE-W
           (WEB-SERVICE)
           ?WEB-SERVICE
           ((HAS-CAPABILITY
             :VALUE
             LIFE-EVENT-CHANGE-ADDRESS-PROFILE-W-CAPABILITY)
            (HAS-INTERFACE
             :VALUE
             LIFE-EVENT-CHANGE-ADDRESS-PROFILE-W-INTERFACE)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             LIFE-EVENT-CHANGE-ADDRESS-PROFILE-W-NON-FUNC-PROPS)))

(DEF-CLASS LIFE-EVENT-CHANGE-ADDRESS-PROFILE-W-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE LIFE-EVENT-CHANGE-ADDRESS-PROFILE-M)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             LIFE-EVENT-CHANGE-ADDRESS-PROFILE-W-NON-FUNC-PROPS)))

(def-class life-event-change-address-profile-w-interface (interface)
  ((has-orchestration :value life-event-change-address-profile-w-orchestration)
   (has-choreography :value life-event-change-address-profile-w-choreography)))

(DEF-CLASS LIFE-EVENT-CHANGE-ADDRESS-PROFILE-W-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-WEB-SERVICE-HOST :VALUE "137.108.25.26")
            (HAS-WEB-SERVICE-PORT :VALUE 3001)
            (HAS-WEB-SERVICE-LOCATION :VALUE "/soap")
            (HAS-LISP-FUNCTION :VALUE ADMG-SOLVE-GENERAL)))