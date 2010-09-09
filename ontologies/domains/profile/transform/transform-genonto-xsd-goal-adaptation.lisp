

(in-package "OCML")

(setf (logical-pathname-translations "transform")
       '(("ROOT;*"        "C:\\users\\jbd2\\code\\ocml\\library\\v5-0\\domains\\profile\\transform\\")))

(load "transform:root;transform-genonto-xsd.lisp")

(in-ontology wsmo-exchange-rate)

(DEF-CLASS EXCHANGE-RATE-GOAL
           (GOAL)
           ?GOAL
           ((HAS-INPUT-ROLE
             :VALUE
             HAS_SOURCE_CURRENCY
             :VALUE
             HAS_TARGET_CURRENCY
             ;:VALUE
             ;HAS_PREFERRED_SERVICE
             :VALUE
             HAS_AMOUNT)
            (HAS-INPUT-SOAP-BINDING
             :VALUE
             (HAS_SOURCE_CURRENCY "sexpr")
             :VALUE
             (HAS_TARGET_CURRENCY "sexpr"))
            (HAS-OUTPUT-ROLE :VALUE HAS_EXCHANGE_RATE)
            (HAS-OUTPUT-SOAP-BINDING
             :VALUE
             (HAS_EXCHANGE_RATE "float"))
            (HAS_SOURCE_CURRENCY :TYPE CURRENCY)
            (HAS_TARGET_CURRENCY :TYPE CURRENCY)
            (HAS_AMOUNT :TYPE FLOAT)
            ;(HAS_PREFERRED_SERVICE :TYPE STRING)
            (HAS_EXCHANGE_RATE :TYPE POSITIVE-NUMBER)))

(defparameter *md-db*
  (second `'((transform 
              ((ocml-xsd 
                ((struc 
                  ((classes 
                    ((EXCHANGE-RATE-GOAL
                      ((has_source_currency 
                        ((enumeration( ,(ocml-eval (all-instances currency))))))
                       (has_target_currency 
                        ((enumeration( ,(ocml-eval (all-instances currency))))))
                       ;(has_preferred_service 
                       ; ((maxLength ( (128)  ))))
                       (has_amount 
                        ((maxInclusive ( (1000000) ))))))))
                   (types 
                    ((float (("xs:decimal")))
                     ;(string (("xs:string")))
                     (currency(("xs:string"))))))))))))))




(defun xs-schema-for-goal (goal)
  (setf *current-class* goal)
  (concatenate 'string
                (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
                (format nil "<!-- Generated from OCML class ~A -->~%" goal)
                (xml-with "xs" "schema" 0 
                          (list (m-a "elementFormDefault" "qualified")
                                (m-a "attributeFormDefault" "unqualified")
                                (m-a "xmlns:xs" "http://www.w3.org/2001/XMLSchema"))
                          
                          (xml-with "xs" "complexType" 0
                                    (list (m-a "name" goal))
                                    (xs-sequence goal 2
                                                 (let ((o_roles (eval `(ocml-eval (ALL-CLASS-SLOT-LOCAL-VALUES ,goal has-output-role)))))
                                                   (util-filter #'(lambda (x) (not (member x o_roles)))
                                                                (get-local-slots goal)))
                                                 )))))



;(princ (xs-schema-for-goal 'EXCHANGE-RATE-GOAL))
