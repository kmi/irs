;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology uk-location-ontology)

(def-class uk-address (postal-address)
  ((address-country :value united-kingdom)
   (address-county :type uk-county)
   (address-city-or-village :type (or uk-town uk-village uk-city))))


(def-class uk-county (geographical-region))

(def-instance aberdeen uk-county)

(def-instance avon uk-county)

(def-instance bedfordshire uk-county)

(def-instance cambridgeshire uk-county)

(def-instance carlisle uk-county)

(def-instance cheshire uk-county)

(def-instance clwyd uk-county)

(def-instance cornwall uk-county)

(def-instance cumbria uk-county)

(def-instance derbyshire uk-county)

(def-instance devon uk-county)

(def-instance dorset uk-county)

(def-instance dumfries uk-county)

(def-instance dundee uk-county)

(def-instance dunfermline uk-county)

(def-instance durham uk-county)

(def-instance dyfed uk-county)

(def-instance eastsussex uk-county)

(def-instance edinburgh uk-county)

(def-instance falkirk uk-county)

(def-instance glasgow uk-county)

(def-instance gloucestershire uk-county)

(def-instance gloucestershire uk-county)

(def-instance greenock uk-county)

(def-instance gwent uk-county)

(def-instance gwynedd uk-county)

(def-instance hampshire uk-county)

(def-instance herefordshire uk-county)

(def-instance humberside uk-county)

(def-instance inverness uk-county)

(def-instance kent uk-county)

(def-instance kilmarock uk-county)

(def-instance kirkaldy uk-county)

(def-instance lancashire uk-county)

(def-instance leicestershire uk-county)

(def-instance lincolnshire uk-county)

(def-instance london uk-county)

(def-instance manchester uk-county)

(def-instance merseyside uk-county)

(def-instance midglamorgan uk-county)

(def-instance norfolk uk-county)

(def-instance northants uk-county)

(def-instance northyorks uk-county)

(def-instance notts uk-county)

(def-instance oxford uk-county)

(def-instance paisley uk-county)

(def-instance perth uk-county)

(def-instance powys uk-county)

(def-instance shropshire uk-county)

(def-instance somerset uk-county)

(def-instance southglamorgan uk-county)

(def-instance staffs uk-county)

(def-instance suffolk uk-county)

(def-instance surrey uk-county)

(def-instance tynenwear uk-county)

(def-instance warwicks uk-county)

(def-instance westglamorgan uk-county)

(def-instance westmidlands uk-county)

(def-instance westsussex uk-county)

(def-instance westyorks uk-county)

(def-instance wiltshire uk-county)

(def-instance worcester uk-county)

