
(in-package "OCML")

(setf (logical-pathname-translations "transform")
       '(("ROOT;*"        "C:\\users\\jbd2\\code\\ocml\\library\\v5-0\\domains\\profile\\transform\\")))

(load "transform:root;transform-genonto-xsd.lisp")

(in-ontology profile-fam)

(defparameter *md-db*
  '((transform ((ocml-xsd ((struc ((classes ((profile-fam
                                              ((has-first-name 
                                                ((maxLength ((120)))
                                                 (minLength ((10)))))
                                               (has-sex 
                                                ((enumeration (((F |Female|)(M |Male|))))))))))
                                   (types ((string (("xs:string")))
                                           (integer (("xs:integer")))))))))))))

(princ (xs-schema 'profile-fam))

'((transform ((ocml-xsd ((struc ((classes ((profile-fam
                                              ((Firstname 
                                                ((maxLength ((120)))
                                                 (minLength ((10))))))))))))))))
