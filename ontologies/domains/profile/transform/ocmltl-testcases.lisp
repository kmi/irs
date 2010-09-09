
(defparameter *toolsdb*
  `((getters ((ocml (
                    (slots ((c) (apply get-slots (list c))))
                    ))
             (goal (
                    (used-classes-in-iroles ((g) (apply get-used-classes-in-iroles (list g))))
                    (iroles ((g) (apply get-iroles (list g))))
                    ))
             ))
   (conditions ((ocml (
                       (simple-type ((c s) (apply is-slot-stype (c s))))
                       ))
                ))
   (actions ((xml (
                   
   (actions ((xsd (
                   (simple-type 
   ))

(defparameter *mddb*
  `((transform ((struc ((classes ((EXCHANGE-RATE-GOAL
                                                       ((has_target_currency 
                                                         ((enumeration( ,(ocml-eval (all-instances currency))))))
                                                        (has_source_currency 
                                                         ((enumeration( ,(ocml-eval (all-instances currency))))))))))
                                  (types ((string (("xs:string")))
                                          (integer (("xs:integer")))
                                          (currency(("xs:string")))
                                          (positive-number (("xs:integer")))))))))))
  

(ocmltl :mddb *mddb*
          
  ;function to distinguish between simple types (string, float)
  ;and complex ones (classes)
  (def treat-types (s)
    (t-cond ((? simple-type s) (> simple-type s))
            (t (> complex-type s))))

  ;fucntion to generate all elements for a type
  (def treat-class (c)
    (inhole xs-cmpl-type c
      (inhole xs-sequ c
        (t-for s (! slots c)
          (t-apply treat-types s)))))
  
  ;generate xml header and comment
  (> xml)
  (> gen-about goal)
  
  ;generate xml schema tag, with a hole
  (t-let ts (> xs-schema))
  
  ;inside xs-schema hole generate all types
  ;used by the input roles (with indentation 2)
  (inhole ts :it 2
    (for c (! used-classes-in-iroles goal)
      (t-apply treat-class c)))
  
  ;generate but only for input roles
  (inhole ts :it 2
    (inhole xs-cmpl-type goal :it 2
      (inhole xs-sequ goal :it 2
        (for s (! iroles goal)
          (t-apply treat-types s)))))
)
 
