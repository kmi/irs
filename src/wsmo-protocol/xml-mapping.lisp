(in-package wsmo-protocol)

(def-relation lift (?class ?web-service ?xml-string ?ontological-representation)
  (or (and (person ?class)
           (german-buy-train-ticket-service ?web-service)
           (def-instance 
      (and (person ?class)
           (english-buy-train-ticket-service ?web-service)
           ....)))

(def-lift ((?class person) (?web-service german-buy-train-ticket-service) ?xml-string ?instance)
   (make-instance....))

(def-lift ((?class person) (?web-service english-buy-train-ticket-service) ?xml-string ?instance)
   (make-instance....))

(def-lift ((?class person) (?web-service french-buy-train-ticket-service) ?xml-string ?instance)
   (make-instance....))

(def-lift ((?class date) (?web-service french-buy-train-ticket-service) ?xml-string ?instance)
   (make-instance....))

(def-lift ((?class date) (?web-service german-buy-train-ticket-service) ?xml-string ?instance)
   (make-instance....))

(def-relation lower (?instance ?web-service ?xml-string))

(def-lower ((?instance person) (?web-service german-buy-train-ticket-service) ?xml-string)
 "<person></person>")

(def-web-service-xml-mapping person-xml-mapping (?person person) (?web-service web-service) ?xml
  ("<xml>")
  ((has-address ?person ?address) "<address>" ?address "</address>")
  ((has-age ?person ?age) "<age>" ?age "</age>")
  ("</xml>"))

(def-web-service-xml-mapping person-xml-mapping (?person person) (?web-service vta-web-service) ?xml
  "<xml>"
  ((has-address ?person ?address) (concatenate "<address>" ?address "</address>"))
  ((has-religion ?person ?religion) (concatenate "<religion>" ?religion "</religion>"))
  "</xml>")



(def-rule country-is-in-continent-rule 
  ((country-is-in-continent ?country europe) if
   (european-country ?country))
  ((country-is-in-continent ?country asian) if
   (asian-country ?country))

(defmacro ocml::def-web-service xml-mapping (name (class-variable ocml-class) (web-service-variable web-service-class) result-var
                                            &rest body)
  `(ocml::def-rule-internal 
    ',name
    ((,ocml-class class-variable)
     (,web-service-class web-service-variable)
     ,(mapcar #'(lambda (mapping-part)
                 (if (listp mapping-part)))))))