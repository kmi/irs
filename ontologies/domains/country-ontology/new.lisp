;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology country-ontology)

(def-class social-class ())

(def-class person ()
  ((has-address :type address)
   (has-employer :type organisation)
   (has-age :type age)
   (has-ethnic-origin :type ethnic-origin)
   (has-religion :type religion)
   (has-social-class :type social-class)
   (has-disability :type disability)
   (receives-benefits :type benefit)))

#|
(def-web-service xml-mapping person-xml-mapping (?person person) (?web-service web-service) ?xml
  ("<xml>")
  ((has-address ?person ?address) "<address>" ?address "</address>")
  ((has-age ?person ?age) "<age>" ?age "</age>")
  ("</xml>"))

(def-web-service xml-mapping person-xml-mapping (?person person) (?web-service vta-web-service) ?xml
  "<xml>"
  ((has-address ?person ?address) "<address>" ?address "</address>")
  ((has-religion ?person ?religion) "<religion>" ?religion "</religion>")
  "</xml>")
|#


(def-class inanimate-physical-entity ()
  ((has-location :type location)
   (has-owner :type agent)))

(def-class co-ordinate ()
  ((has-x :type integer :cardinality 1)
   (has-y :type integer :cardinality 1)
   (has-z :type integer :cardinality 1)))

(def-class region-descriptor ())

(def-class african-country (country))

(def-class oceanian-country (country))

(def-class south-american-country (country))

(def-class north-american-country (country))

(def-class asian-country (country))

(def-class european-country (country))

(def-class coordinate-region (region-descriptor)
  ((has-coordinate :type co-ordinate)))

(def-class radial-region-descriptor (region-descriptor)
  ((has-centre :type co-ordinate)
   (has-radius :type integer)))

(def-class building (inanimate-physical-entity))

(def-class site (inanimate-physical-entity)
  ((contains-entitites :type inanimate-physical-entity)))

(def-class runway (inanimate-physical-entity ))

(def-class road (inanimate-physical-entity ))

(def-class citizen (person))

(def-class public-road (road))

(def-class private-road (road))

(def-class airport (site)
  ((has-runway :type runway)
   (has-terminal :type terminal)
   (has-tower :type tower)
   (has-car-park :type car-park)
   (has-road :type private-road)))

(def-class quadrilateral-region (coordinate-region))

(def-instance england european-country
  ((has-name "England")
   (has-capital london)))

(def-instance wales european-country
  ((has-name "Wales")
   (has-capital cardiff)))

(def-instance scotland european-country
  ((has-name "Scotland")
   (has-capital Edinburgh)))

(def-instance ireland european-country
  ((has-name "Ireland")
   (has-capital dublin)))

(def-instance angola african-country
  ((has-name "Angola")
   (has-capital luanda)))

(def-instance afghanistan asian-country
  ((has-name "Afghanistan")
   (has-capital kabul)))

(def-instance kabul capital-city
  ((has-name "Kabul")
   (is-capital-of afghanistan)))

(def-instance albania european-country
  ((has-name "Albania")
   (has-capital tirane)))

(def-instance tirane capital-city
  ((has-name "Tirane")
   (is-capital-of albania)))

(def-instance algeria african-country
  ((has-name "Algeria")
   (has-capital algers)))

(def-instance algers capital-city
  ((has-name "Algers")
   (is-capital-of algeria)))

(def-instance andorra european-country
  ((has-name "Andorra")
   (has-capital andorra-la-vella)))

(def-instance andorra-la-vella capital-city
  ((has-name "Andorra la Vella")
   (is-capital-of andorra)))

(def-instance angola african-country
  ((has-name "Angola")
   (has-capital luanda)))

(def-instance luanda capital-city
  ((has-name "Luanda")
   (is-capital-of angola)))

(def-instance antigua-and-barbuda north-american-country
  ((has-name "Antigua and Barbuda")
   (has-capital st-johns)))

(def-instance st-johns capital-city
  ((has-name "St. John's")
   (is-capital-of antigua-and-barbuda)))

(def-instance argentina south-american-country
  ((has-name "Argentina")
   (has-capital buenos-aires)))

(def-instance buenos-aires capital-city
  ((has-name "Buenos Aires")
   (is-capital-of argentina)))

(def-instance armenia european-country
  ((has-name "Armenia")
   (has-capital yerevan)))

(def-instance yerevan capital-city
  ((has-name "Yerevan")
   (is-capital-of armenia)))

(def-instance australia oceanian-country
  ((has-name "Australia")
   (has-capital canberra)))

(def-instance canberra capital-city
  ((has-name "Canberra")
   (is-capital-of australia)))

(def-instance austria european-country
  ((has-name "Austria")
   (has-capital vienna)))

(def-instance vienna capital-city
  ((has-name "Vienna")
   (is-capital-of austria)))

(def-instance azerbaijan asian-country
  ((has-name "Azerbaijan")
   (has-capital baku)))

(def-instance baku capital-city
  ((has-name "Baku")
   (is-capital-of azerbaijan)))

(def-instance bahamas north-american-country
  ((has-name "Bahamas")
   (has-capital nassau)))

(def-instance nassau capital-city
  ((has-name "Nassau")
   (is-capital-of bahamas)))

(def-instance bahrain asian-country
  ((has-name "Bahrain")
   (has-capital manama)))

(def-instance manama capital-city
  ((has-name "Manama")
   (is-capital-of bahrain)))

(def-instance bangladesh asian-country
  ((has-name "Bangladesh")
   (has-capital dhaka)))

(def-instance dhaka capital-city
  ((has-name "Dhaka")
   (is-capital-of bangladesh)))

(def-instance barbados north-american-country
  ((has-name "Barbados")
   (has-capital bridgetown)))

(def-instance bridgetown capital-city
  ((has-name "Bridgetown")
   (is-capital-of barbados)))

(def-instance belarus european-country
  ((has-name "Belarus")
   (has-capital minsk)))

(def-instance minsk capital-city
  ((has-name "Minsk")
   (is-capital-of belarus)))

(def-instance belgium european-country
  ((has-name "Belgium")
   (has-capital brussels)))

(def-instance brussels capital-city
  ((has-name "Brussels")
   (is-capital-of belgium)))

(def-instance belize north-american-country
  ((has-name "Belize")
   (has-capital belmopan)))

(def-instance belmopan capital-city
  ((has-name "Belmopan")
   (is-capital-of belize)))

(def-instance benin african-country
  ((has-name "Benin")
   (has-capital port-novo)))

(def-instance port-novo capital-city
  ((has-name "Port-Novo")
   (is-capital-of benin)))

(def-instance bhutan asian-country
  ((has-name "Bhutan")
   (has-capital thimphu)))

(def-instance thimphu capital-city
  ((has-name "Thimphu")
   (is-capital-of bhutan)))

(def-instance bolivia south-american-country
  ((has-name "Bolivia")
   (has-capital sucre)))

(def-instance sucre capital-city
  ((has-name "Sucre")
   (is-capital-of bolivia)))

(def-instance bosnia-and-herzegovina european-country
  ((has-name "Bosnia and Herzegovina")
   (has-capital sarajevo)))

(def-instance sarajevo capital-city
  ((has-name "Sarajevo")
   (is-capital-of bosnia-and-herzegovina)))

(def-instance botswana african-country
  ((has-name "Botswana")
   (has-capital gaborone)))

(def-instance gaborone capital-city
  ((has-name "Gaborone")
   (is-capital-of botswana)))

(def-instance brazil south-american-country
  ((has-name "Brazil")
   (has-capital brasilia)))

(def-instance brasilia capital-city
  ((has-name "Brasilia")
   (is-capital-of brazil)))

(def-instance brunei asian-country
  ((has-name "Brunei")
   (has-capital bander-seri-begawan)))

(def-instance bander-seri-begawan capital-city
  ((has-name "Bander Seri Begawan")
   (is-capital-of brunei)))

(def-instance bulgaria european-country
  ((has-name "Bulgaria")
   (has-capital sofia)))

(def-instance sofia capital-city
  ((has-name "Sofia")
   (is-capital-of bulgaria)))

(def-instance burkina-faso african-country
  ((has-name "Burkina Faso")
   (has-capital ouagadougou)))

(def-instance ouagadougou capital-city
  ((has-name "Ouagadougou")
   (is-capital-of burkina-faso)))

(def-instance burma asian-country
  ((has-name "Burma")
   (has-capital yangon)))

(def-instance yangon capital-city
  ((has-name "Yangon")
   (is-capital-of burma)))

(def-instance burundi african-country
  ((has-name "Burundi")
   (has-capital bujumbura)))

(def-instance bujumbura capital-city
  ((has-name "Bujumbura")
   (is-capital-of burundi)))

(def-instance cambodia asian-country
  ((has-name "Cambodia")
   (has-capital phnom-penh)))

(def-instance phnom-penh capital-city
  ((has-name "Phnom Penh")
   (is-capital-of cambodia)))

(def-instance cameroon south-american-country
  ((has-name "Cameroon")
   (has-capital yaounde)))

(def-instance yaounde capital-city
  ((has-name "Yaounde")
   (is-capital-of cameroon)))

(def-instance canada north-american-country
  ((has-name "Canada")
   (has-capital ottawa)))

(def-instance ottawa capital-city
  ((has-name "Ottawa")
   (is-capital-of canada)))

(def-instance cape-verde european-country
  ((has-name "Cape Verde")
   (has-capital praia)))

(def-instance praia capital-city
  ((has-name "Praia")
   (is-capital-of cape-verde)))

(def-instance central-african-republic african-country
  ((has-name "Central African Republic")
   (has-capital bangui)))

(def-instance bangui capital-city
  ((has-name "Bangui")
   (is-capital-of central-african-republic)))

(def-instance chad african-country
  ((has-name "Chad")
   (has-capital ndjamena)))

(def-instance ndjamena capital-city
  ((has-name "N'Djamena")
   (is-capital-of chad)))

(def-instance chile south-american-country
  ((has-name "Chile")
   (has-capital santiago)))

(def-instance santiago capital-city
  ((has-name "Santiago")
   (is-capital-of chile)))

(def-instance china asian-country
  ((has-name "China")
   (has-capital beijing)))

(def-instance beijing capital-city
  ((has-name "Beijing")
   (is-capital-of china)))

(def-instance colombia south-american-country
  ((has-name "Colombia")
   (has-capital bogota)))

(def-instance bogota capital-city
  ((has-name "Bogota")
   (is-capital-of colombia)))

(def-instance comoros african-country
  ((has-name "Comoros")
   (has-capital moroni)))

(def-instance moroni capital-city
  ((has-name "Moroni")
   (is-capital-of comoros)))

(def-instance congo african-country
  ((has-name "Congo")
   (has-capital brazzaville)))

(def-instance brazzaville capital-city
  ((has-name "Brazzaville")
   (is-capital-of congo)))

(def-instance democratic-republic-of-congo african-country
  ((has-name "Democratic Republic of Congo")
   (has-capital kinshasa)))

(def-instance kinshasa capital-city
  ((has-name "Kinshasa")
   (is-capital-of democratic-republic-of-congo)))

(def-instance costa-rica north-american-country
  ((has-name "Costa Rica")
   (has-capital san-jose)))

(def-instance san-jose capital-city
  ((has-name "San Jose")
   (is-capital-of costa-rica)))

(def-instance ivory-coast african-country
  ((has-name "Ivory Coast")
   (has-capital yamoussoukro)))

(def-instance yamoussoukro capital-city
  ((has-name "Yamoussoukro")
   (is-capital-of ivory-coast)))

(def-instance croatia european-country
  ((has-name "Croatia")
   (has-capital zagreb)))

(def-instance zagreb capital-city
  ((has-name "Zagreb")
   (is-capital-of croatia)))

(def-instance cuba north-american-country
  ((has-name "Cuba")
   (has-capital havana)))

(def-instance havana capital-city
  ((has-name "Havana")
   (is-capital-of cuba)))

(def-instance cyprus european-country
  ((has-name "Cyprus")
   (has-capital nicosia)))

(def-instance nicosia capital-city
  ((has-name "Nicosia")
   (is-capital-of cyprus)))

(def-instance czech-republic european-country
  ((has-name "Czech Republic")
   (has-capital prague)))

(def-instance prague capital-city
  ((has-name "Prague")
   (is-capital-of czech-republic)))

(def-instance denmark european-country
  ((has-name "Denmark")
   (has-capital copenhagen)))

(def-instance copenhagen capital-city
  ((has-name "Copenhagen")
   (is-capital-of denmark)))

(def-instance djibouti african-country
  ((has-name "Djibouti")
   (has-capital djibouti)))

(def-instance djibouti capital-city
  ((has-name "Djibouti")
   (is-capital-of djibouti)))

(def-instance dominica north-american-country
  ((has-name "Dominica")
   (has-capital roseau)))

(def-instance roseau capital-city
  ((has-name "Roseau")
   (is-capital-of dominica)))

(def-instance dominican-republic north-american-country
  ((has-name "Dominican Republic")
   (has-capital santo-domingo)))

(def-instance santo-domingo capital-city
  ((has-name "Santo Domingo")
   (is-capital-of dominican-republic)))

(def-instance ecuador south-american-country
  ((has-name "Ecuador")
   (has-capital quito)))

(def-instance quito capital-city
  ((has-name "Quito")
   (is-capital-of ecuador)))

(def-instance egypt african-country
  ((has-name "Egypt")
   (has-capital cairo)))

(def-instance cairo capital-city
  ((has-name "Cairo")
   (is-capital-of egypt)))

(def-instance el-salvador north-american-country
  ((has-name "El Salvador")
   (has-capital san-salvador)))

(def-instance san-salvador capital-city
  ((has-name "San Salvador")
   (is-capital-of el-salvador)))

(def-instance equatorial-guinea african-country
  ((has-name "Equatorial Guinea")
   (has-capital malabo)))

(def-instance malabo capital-city
  ((has-name "Malabo")
   (is-capital-of equatorial-guinea)))

(def-instance eritrea african-country
  ((has-name "Eritrea")
   (has-capital asmara)))

(def-instance asmara capital-city
  ((has-name "Asmara")
   (is-capital-of eritrea)))

(def-instance estonia european-country
  ((has-name "Estonia")
   (has-capital tallinn)))

(def-instance tallinn capital-city
  ((has-name "Tallinn")
   (is-capital-of estonia)))

(def-instance ethiopia african-country
  ((has-name "Ethiopia")
   (has-capital addis-ababa)))

(def-instance addis-ababa capital-city
  ((has-name "Addis Ababa")
   (is-capital-of ethiopia)))

(def-instance fiji oceanian-country
  ((has-name "Fiji")
   (has-capital suva)))

(def-instance suva capital-city
  ((has-name "Suva")
   (is-capital-of fiji)))

(def-instance finland european-country
  ((has-name "Finland")
   (has-capital helsinki)))

(def-instance helsinki capital-city
  ((has-name "Helsinki")
   (is-capital-of finland)))

(def-instance france european-country
  ((has-name "France")
   (has-capital paris)))

(def-instance paris capital-city
  ((has-name "Paris")
   (is-capital-of france)))

(def-instance gabon african-country
  ((has-name "Gabon")
   (has-capital liberville)))

(def-instance liberville capital-city
  ((has-name "Liberville")
   (is-capital-of gabon)))

(def-instance gambia african-country
  ((has-name "Gambia")
   (has-capital banjul)))

(def-instance banjul capital-city
  ((has-name "Banjul")
   (is-capital-of gambia)))

(def-instance georgia european-country
  ((has-name "Georgia")
   (has-capital tbilisi)))

(def-instance tbilisi capital-city
  ((has-name "Tbilisi")
   (is-capital-of georgia)))

(def-instance germany european-country
  ((has-name "Germany")
   (has-capital berlin)))

(def-instance berlin capital-city
  ((has-name "Berlin")
   (is-capital-of germany)))

(def-instance ghana european-country
  ((has-name "Ghana")
   (has-capital accra)))

(def-instance accra capital-city
  ((has-name "Accra")
   (is-capital-of ghana)))

(def-instance greece european-country
  ((has-name "Greece")
   (has-capital athens)))

(def-instance athens capital-city
  ((has-name "Athens")
   (is-capital-of greece)))

(def-instance grenada north-american-country
  ((has-name "Grenada")
   (has-capital st-georges)))

(def-instance st-georges capital-city
  ((has-name "St. George's")
   (is-capital-of grenada)))

(def-instance guatemala north-american-country
  ((has-name "Guatemala")
   (has-capital guatemala-city)))

(def-instance guatemala-city capital-city
  ((has-name "Guatemala City")
   (is-capital-of guatemala)))

(def-instance guinea african-country
  ((has-name "Guinea")
   (has-capital conakry)))

(def-instance conakry capital-city
  ((has-name "Conakry")
   (is-capital-of guinea)))

(def-instance guinea-bissau african-country
  ((has-name "Guinea-Bissau")
   (has-capital bissau)))

(def-instance bissau capital-city
  ((has-name "Bissau")
   (is-capital-of guinea-bissau)))

(def-instance guyana south-american-country
  ((has-name "Guyana")
   (has-capital georgetown)))

(def-instance georgetown capital-city
  ((has-name "Georgetown")
   (is-capital-of guyana)))

(def-instance haiti north-american-country
  ((has-name "Haiti")
   (has-capital port-au-prince)))

(def-instance port-au-prince capital-city
  ((has-name "Port-au-Prince")
   (is-capital-of haiti)))

(def-instance honduras north-american-country
  ((has-name "Honduras")
   (has-capital tegucigalpa)))

(def-instance tegucigalpa capital-city
  ((has-name "Tegucigalpa")
   (is-capital-of honduras)))

(def-instance hungary european-country
  ((has-name "Hungary")
   (has-capital budapest)))

(def-instance budapest capital-city
  ((has-name "Budapest")
   (is-capital-of hungary)))

(def-instance iceland european-country
  ((has-name "Iceland")
   (has-capital reykjavik)))

(def-instance reykjavik capital-city
  ((has-name "Reykjavik")
   (is-capital-of iceland)))

(def-instance india asian-country
  ((has-name "India")
   (has-capital new-delhi)))

(def-instance new-delhi capital-city
  ((has-name "New Delhi")
   (is-capital-of india)))

(def-instance indonesia asian-country
  ((has-name "Indonesia")
   (has-capital jakarta)))

(def-instance jakarta capital-city
  ((has-name "Jakarta")
   (is-capital-of indonesia)))

(def-instance iran asian-country
  ((has-name "Iran")
   (has-capital tehran)))

(def-instance tehran capital-city
  ((has-name "Tehran")
   (is-capital-of iran)))

(def-instance iraq asian-country
  ((has-name "Iraq")
   (has-capital baghdad)))

(def-instance baghdad capital-city
  ((has-name "Baghdad")
   (is-capital-of iraq)))

(def-instance ireland european-country
  ((has-name "Ireland")
   (has-capital dublin)))

(def-instance dublin capital-city
  ((has-name "Dublin")
   (is-capital-of ireland)))

(def-instance israel asian-country
  ((has-name "Israel")
   (has-capital jerusalem)))

(def-instance jerusalem capital-city
  ((has-name "Jerusalem")
   (is-capital-of israel)))

(def-instance italy european-country
  ((has-name "Italy")
   (has-capital rome)))

(def-instance rome capital-city
  ((has-name "Rome")
   (is-capital-of italy)))

(def-instance jamaica north-american-country
  ((has-name "Jamaica")
   (has-capital kingston)))

(def-instance kingston capital-city
  ((has-name "Kingston")
   (is-capital-of jamaica)))

(def-instance japan asian-country
  ((has-name "Japan")
   (has-capital tokyo)))

(def-instance tokyo capital-city
  ((has-name "Tokyo")
   (is-capital-of japan)))

(def-instance jordan asian-country
  ((has-name "Jordan")
   (has-capital amman)))

(def-instance amman capital-city
  ((has-name "Amman")
   (is-capital-of jordan)))

(def-instance kazakstan asian-country
  ((has-name "Kazakstan")
   (has-capital astana)))

(def-instance astana capital-city
  ((has-name "Astana")
   (is-capital-of kazakstan)))

(def-instance kenya african-country
  ((has-name "Kenya")
   (has-capital nairobi)))

(def-instance nairobi capital-city
  ((has-name "Nairobi")
   (is-capital-of kenya)))

(def-instance kiribati oceanian-country
  ((has-name "Kiribati")
   (has-capital bairiki)))

(def-instance bairiki capital-city
  ((has-name "Bairiki")
   (is-capital-of kiribati)))

(def-instance north-korea asian-country
  ((has-name "North Korea")
   (has-capital pyongyang)))

(def-instance pyongyang capital-city
  ((has-name "Pyongyang")
   (is-capital-of north-korea)))

(def-instance south-korea asian-country
  ((has-name "South Korea")
   (has-capital seoul)))

(def-instance seoul capital-city
  ((has-name "Seoul")
   (is-capital-of south-korea)))

(def-instance kuwait asian-country
  ((has-name "Kuwait")
   (has-capital kuwait-city)))

(def-instance kuwait-city capital-city
  ((has-name "Kuwait City")
   (is-capital-of kuwait)))

(def-instance kyrgyzstan asian-country
  ((has-name "Kyrgyzstan")
   (has-capital bishkek)))

(def-instance bishkek capital-city
  ((has-name "Bishkek")
   (is-capital-of kyrgyzstan)))

(def-instance laos asian-country
  ((has-name "Laos")
   (has-capital vientiane)))

(def-instance vientiane capital-city
  ((has-name "Vientiane")
   (is-capital-of laos)))

(def-instance latvia european-country
  ((has-name "Latvia")
   (has-capital riga)))

(def-instance riga capital-city
  ((has-name "Riga")
   (is-capital-of latvia)))

(def-instance lebanon asian-country
  ((has-name "Lebanon")
   (has-capital beirut)))

(def-instance beirut capital-city
  ((has-name "Beirut")
   (is-capital-of lebanon)))

(def-instance lesotho african-country
  ((has-name "Lesotho")
   (has-capital maseru)))

(def-instance maseru capital-city
  ((has-name "Maseru")
   (is-capital-of lesotho)))

(def-instance liberia african-country
  ((has-name "Liberia")
   (has-capital monrovia)))

(def-instance monrovia capital-city
  ((has-name "Monrovia")
   (is-capital-of liberia)))

(def-instance libya african-country
  ((has-name "Libya")
   (has-capital tripoli)))

(def-instance tripoli capital-city
  ((has-name "Tripoli")
   (is-capital-of libya)))

(def-instance liechtenstein european-country
  ((has-name "Liechtenstein")
   (has-capital vaduz)))

(def-instance vaduz capital-city
  ((has-name "Vaduz")
   (is-capital-of liechtenstein)))

(def-instance lithuania european-country
  ((has-name "Lithuania")
   (has-capital vilnius)))

(def-instance vilnius capital-city
  ((has-name "Vilnius")
   (is-capital-of lithuania)))

(def-instance luxembourg european-country
  ((has-name "Luxembourg")
   (has-capital luxembourg)))

(def-instance luxembourg capital-city
  ((has-name "Luxembourg")
   (is-capital-of luxembourg)))

(def-instance macedonia european-country
  ((has-name "Macedonia")
   (has-capital skopje)))

(def-instance skopje capital-city
  ((has-name "Skopje")
   (is-capital-of macedonia)))

(def-instance madagascar african-country
  ((has-name "Madagascar")
   (has-capital antananarivo)))

(def-instance antananarivo capital-city
  ((has-name "Antananarivo")
   (is-capital-of madagascar)))

(def-instance malawi african-country
  ((has-name "Malawi")
   (has-capital lilongwe)))

(def-instance lilongwe capital-city
  ((has-name "Lilongwe")
   (is-capital-of malawi)))

(def-instance malaysia asian-country
  ((has-name "Malaysia")
   (has-capital kuala-lumpur)))

(def-instance kuala-lumpur capital-city
  ((has-name "Kuala Lumpur")
   (is-capital-of malaysia)))

(def-instance maldives asian-country
  ((has-name "Maldives")
   (has-capital male)))

(def-instance male capital-city
  ((has-name "Male")
   (is-capital-of maldives)))

(def-instance mali african-country
  ((has-name "Mali")
   (has-capital bamako)))

(def-instance bamako capital-city
  ((has-name "Bamako")
   (is-capital-of mali)))

(def-instance malta european-country
  ((has-name "Malta")
   (has-capital valletta)))

(def-instance valletta capital-city
  ((has-name "Valletta")
   (is-capital-of malta)))

(def-instance marshall-islands oceanian-country
  ((has-name "Marshall Islands")
   (has-capital majuro)))

(def-instance majuro capital-city
  ((has-name "Majuro")
   (is-capital-of marshall-islands)))

(def-instance mauritania african-country
  ((has-name "Mauritania")
   (has-capital nouakchott)))

(def-instance nouakchott capital-city
  ((has-name "Nouakchott")
   (is-capital-of mauritania)))

(def-instance mauritius african-country
  ((has-name "Mauritius")
   (has-capital port-louis)))

(def-instance port-louis capital-city
  ((has-name "Port Louis")
   (is-capital-of mauritius)))

(def-instance mexico north-american-country
  ((has-name "Mexico")
   (has-capital mexico-city)))

(def-instance mexico-city capital-city
  ((has-name "Mexico City")
   (is-capital-of mexico)))

(def-instance micronesia oceanian-country
  ((has-name "Micronesia")
   (has-capital palikir)))

(def-instance palikir capital-city
  ((has-name "Palikir")
   (is-capital-of micronesia)))

(def-instance moldova european-country
  ((has-name "Moldova")
   (has-capital chisinau)))

(def-instance chisinau capital-city
  ((has-name "Chisinau")
   (is-capital-of moldova)))

(def-instance monaco european-country
  ((has-name "Monaco")
   (has-capital monaco)))

(def-instance monaco capital-city
  ((has-name "Monaco")
   (is-capital-of monaco)))

(def-instance mongolia asian-country
  ((has-name "Mongolia")
   (has-capital ulan-bator)))

(def-instance ulan-bator capital-city
  ((has-name "Ulan Bator")
   (is-capital-of mongolia)))

(def-instance morocco african-country
  ((has-name "Morocco")
   (has-capital rabat)))

(def-instance rabat capital-city
  ((has-name "Rabat")
   (is-capital-of morocco)))

(def-instance mozambique african-country
  ((has-name "Mozambique")
   (has-capital maputo)))

(def-instance maputo capital-city
  ((has-name "Maputo")
   (is-capital-of mozambique)))

(def-instance namibia african-country
  ((has-name "Namibia")
   (has-capital windhoek)))

(def-instance windhoek capital-city
  ((has-name "Windhoek")
   (is-capital-of namibia)))

(def-instance nauru oceanian-country
  ((has-name "Nauru")
   (has-capital no-official-capital)))

(def-instance no-official-capital capital-city
  ((has-name "no official capital")
   (is-capital-of nauru)))

(def-instance nepal asian-country
  ((has-name "Nepal")
   (has-capital kathmandu)))

(def-instance kathmandu capital-city
  ((has-name "Kathmandu")
   (is-capital-of nepal)))

(def-instance netherlands european-country
  ((has-name "Netherlands")
   (has-capital amsterdam)))

(def-instance amsterdam capital-city
  ((has-name "Amsterdam")
   (is-capital-of netherlands)))

(def-instance new-zealand oceanian-country
  ((has-name "New Zealand")
   (has-capital wellington)))

(def-instance wellington capital-city
  ((has-name "Wellington")
   (is-capital-of new-zealand)))

(def-instance nicaragua north-american-country
  ((has-name "Nicaragua")
   (has-capital managua)))

(def-instance managua capital-city
  ((has-name "Managua")
   (is-capital-of nicaragua)))

(def-instance niger african-country
  ((has-name "Niger")
   (has-capital niamey)))

(def-instance niamey capital-city
  ((has-name "Niamey")
   (is-capital-of niger)))

(def-instance nigeria african-country
  ((has-name "Nigeria")
   (has-capital abuja)))

(def-instance abuja capital-city
  ((has-name "Abuja")
   (is-capital-of nigeria)))

(def-instance norway european-country
  ((has-name "Norway")
   (has-capital oslo)))

(def-instance oslo capital-city
  ((has-name "Oslo")
   (is-capital-of norway)))

(def-instance oman asian-country
  ((has-name "Oman")
   (has-capital muscat)))

(def-instance muscat capital-city
  ((has-name "Muscat")
   (is-capital-of oman)))

(def-instance pakistan asian-country
  ((has-name "Pakistan")
   (has-capital islamabad)))

(def-instance islamabad capital-city
  ((has-name "Islamabad")
   (is-capital-of pakistan)))

(def-instance palau oceanian-country
  ((has-name "Palau")
   (has-capital koror)))

(def-instance koror capital-city
  ((has-name "Koror")
   (is-capital-of palau)))

(def-instance panama north-american-country
  ((has-name "Panama")
   (has-capital panama-city)))

(def-instance panama-city capital-city
  ((has-name "Panama City")
   (is-capital-of panama)))

(def-instance papua-new-guinea oceanian-country
  ((has-name "Papua New Guinea")
   (has-capital port-moresby)))

(def-instance port-moresby capital-city
  ((has-name "Port Moresby")
   (is-capital-of papua-new-guinea)))

(def-instance paraguay south-american-country
  ((has-name "Paraguay")
   (has-capital asuncion)))

(def-instance asuncion capital-city
  ((has-name "Asuncion")
   (is-capital-of paraguay)))

(def-instance peru south-american-country
  ((has-name "Peru")
   (has-capital lima)))

(def-instance lima capital-city
  ((has-name "Lima")
   (is-capital-of peru)))

(def-instance philippines asian-country
  ((has-name "Philippines")
   (has-capital manila)))

(def-instance manila capital-city
  ((has-name "Manila")
   (is-capital-of philippines)))

(def-instance poland european-country
  ((has-name "Poland")
   (has-capital warsaw)))

(def-instance warsaw capital-city
  ((has-name "Warsaw")
   (is-capital-of poland)))

(def-instance portugal european-country
  ((has-name "Portugal")
   (has-capital lisbon)))

(def-instance lisbon capital-city
  ((has-name "Lisbon")
   (is-capital-of portugal)))

(def-instance qatar asian-country
  ((has-name "Qatar")
   (has-capital doha)))

(def-instance doha capital-city
  ((has-name "Doha")
   (is-capital-of qatar)))

(def-instance romania european-country
  ((has-name "Romania")
   (has-capital bucharest)))

(def-instance bucharest capital-city
  ((has-name "Bucharest")
   (is-capital-of romania)))

(def-instance russian-federation asian-country
  ((has-name "Russian Federation")
   (has-capital moscow)))

(def-instance moscow capital-city
  ((has-name "Moscow")
   (is-capital-of russian-federation)))

(def-instance rwanda african-country
  ((has-name "Rwanda")
   (has-capital kigali)))

(def-instance kigali capital-city
  ((has-name "Kigali")
   (is-capital-of rwanda)))

(def-instance saint-kitts-and-nevis north-american-country
  ((has-name "Saint Kitts and Nevis")
   (has-capital basseterre)))

(def-instance basseterre capital-city
  ((has-name "Basseterre")
   (is-capital-of saint-kitts-and-nevis)))

(def-instance saint-lucia north-american-country
  ((has-name "Saint Lucia")
   (has-capital castries)))

(def-instance castries capital-city
  ((has-name "Castries")
   (is-capital-of saint-lucia)))

(def-instance saint-vincent-and-the-grenadines north-american-country
  ((has-name "Saint Vincent and the Grenadines")
   (has-capital kingstown)))

(def-instance kingstown capital-city
  ((has-name "Kingstown")
   (is-capital-of saint-vincent-and-the-grenadines)))

(def-instance san-marino european-country
  ((has-name "San Marino")
   (has-capital san-marino)))

(def-instance san-marino capital-city
  ((has-name "San Marino")
   (is-capital-of san-marino)))

(def-instance sao-tome-and-principe african-country
  ((has-name "Sao Tome and Principe")
   (has-capital sao-tome)))

(def-instance sao-tome capital-city
  ((has-name "Sao Tome")
   (is-capital-of sao-tome-and-principe)))

(def-instance saudi-arabia asian-country
  ((has-name "Saudi Arabia")
   (has-capital riyadh)))

(def-instance riyadh capital-city
  ((has-name "Riyadh")
   (is-capital-of saudi-arabia)))

(def-instance senegal african-country
  ((has-name "Senegal")
   (has-capital dakar)))

(def-instance dakar capital-city
  ((has-name "Dakar")
   (is-capital-of senegal)))

(def-instance seychelles african-country
  ((has-name "Seychelles")
   (has-capital victoria)))

(def-instance victoria capital-city
  ((has-name "Victoria")
   (is-capital-of seychelles)))

(def-instance sierra-leone african-country
  ((has-name "Sierra Leone")
   (has-capital freetown)))

(def-instance freetown capital-city
  ((has-name "Freetown")
   (is-capital-of sierra-leone)))

(def-instance singapore asian-country
  ((has-name "Singapore")
   (has-capital singapore-city)))

(def-instance singapore-city capital-city
  ((has-name "Singapore City")
   (is-capital-of singapore)))

(def-instance slovakia european-country
  ((has-name "Slovakia")
   (has-capital bratislava)))

(def-instance bratislava capital-city
  ((has-name "Bratislava")
   (is-capital-of slovakia)))

(def-instance slovenia european-country
  ((has-name "Slovenia")
   (has-capital ljubljana)))

(def-instance ljubljana capital-city
  ((has-name "Ljubljana")
   (is-capital-of slovenia)))

(def-instance solomon-islands oceanian-country
  ((has-name "Solomon Islands")
   (has-capital honiara)))

(def-instance honiara capital-city
  ((has-name "Honiara")
   (is-capital-of solomon-islands)))

(def-instance somalia african-country
  ((has-name "Somalia")
   (has-capital mogadishu)))

(def-instance mogadishu capital-city
  ((has-name "Mogadishu")
   (is-capital-of somalia)))

(def-instance south-africa african-country
  ((has-name "South Africa")
   (has-capital pretoria)))

(def-instance pretoria capital-city
  ((has-name "Pretoria")
   (is-capital-of south-africa)))

(def-instance spain european-country
  ((has-name "Spain")
   (has-capital madrid)))

(def-instance madrid capital-city
  ((has-name "Madrid")
   (is-capital-of spain)))

(def-instance sri-lanka asian-country
  ((has-name "Sri Lanka")
   (has-capital colombo)))

(def-instance colombo capital-city
  ((has-name "Colombo")
   (is-capital-of sri-lanka)))

(def-instance sudan african-country
  ((has-name "Sudan")
   (has-capital khartoum)))

(def-instance khartoum capital-city
  ((has-name "Khartoum")
   (is-capital-of sudan)))

(def-instance suriname south-american-country
  ((has-name "Suriname")
   (has-capital paramaribo)))

(def-instance paramaribo capital-city
  ((has-name "Paramaribo")
   (is-capital-of suriname)))

(def-instance swaziland african-country
  ((has-name "Swaziland")
   (has-capital mbabane)))

(def-instance mbabane capital-city
  ((has-name "Mbabane")
   (is-capital-of swaziland)))

(def-instance sweden european-country
  ((has-name "Sweden")
   (has-capital stockholm)))

(def-instance stockholm capital-city
  ((has-name "Stockholm")
   (is-capital-of sweden)))

(def-instance switzerland european-country
  ((has-name "Switzerland")
   (has-capital bern)))

(def-instance bern capital-city
  ((has-name "Bern")
   (is-capital-of switzerland)))

(def-instance syria asian-country
  ((has-name "Syria")
   (has-capital damascus)))

(def-instance damascus capital-city
  ((has-name "Damascus")
   (is-capital-of syria)))

(def-instance taiwan asian-country
  ((has-name "Taiwan")
   (has-capital taipei)))

(def-instance taipei capital-city
  ((has-name "Taipei")
   (is-capital-of taiwan)))

(def-instance tajikistan asian-country
  ((has-name "Tajikistan")
   (has-capital dushanbe)))

(def-instance dushanbe capital-city
  ((has-name "Dushanbe")
   (is-capital-of tajikistan)))

(def-instance tanzania african-country
  ((has-name "Tanzania")
   (has-capital dodoma)))

(def-instance dodoma capital-city
  ((has-name "Dodoma")
   (is-capital-of tanzania)))

(def-instance thailand asian-country
  ((has-name "Thailand")
   (has-capital bangkok)))

(def-instance bangkok capital-city
  ((has-name "Bangkok")
   (is-capital-of thailand)))

(def-instance togo oceanian-country
  ((has-name "Togo")
   (has-capital lome)))

(def-instance lome capital-city
  ((has-name "Lome")
   (is-capital-of togo)))

(def-instance tonga oceanian-country
  ((has-name "Tonga")
   (has-capital nukualofa)))

(def-instance nukualofa capital-city
  ((has-name "Nuku'alofa")
   (is-capital-of tonga)))

(def-instance trinidad-and-tobago north-american-country
  ((has-name "Trinidad and Tobago")
   (has-capital port-of-spain)))

(def-instance port-of-spain capital-city
  ((has-name "Port-of-Spain")
   (is-capital-of trinidad-and-tobago)))

(def-instance tunisia african-country
  ((has-name "Tunisia")
   (has-capital tunis)))

(def-instance tunis capital-city
  ((has-name "Tunis")
   (is-capital-of tunisia)))

(def-instance turkey asian-country
  ((has-name "Turkey")
   (has-capital ankara)))

(def-instance ankara capital-city
  ((has-name "Ankara")
   (is-capital-of turkey)))

(def-instance turkmenistan asian-country
  ((has-name "Turkmenistan")
   (has-capital ashgabat)))

(def-instance ashgabat capital-city
  ((has-name "Ashgabat")
   (is-capital-of turkmenistan)))

(def-instance tuvalu oceanian-country
  ((has-name "Tuvalu")
   (has-capital funafuti)))

(def-instance funafuti capital-city
  ((has-name "Funafuti")
   (is-capital-of tuvalu)))

(def-instance uganda african-country
  ((has-name "Uganda")
   (has-capital kampala)))

(def-instance kampala capital-city
  ((has-name "Kampala")
   (is-capital-of uganda)))

(def-instance ukraine european-country
  ((has-name "Ukraine")
   (has-capital kiev)))

(def-instance kiev capital-city
  ((has-name "Kiev")
   (is-capital-of ukraine)))

(def-instance united-arab-emirates asian-country
  ((has-name "United Arab Emirates")
   (has-capital abu-dhabi)))

(def-instance abu-dhabi capital-city
  ((has-name "Abu Dhabi")
   (is-capital-of united-arab-emirates)))

(def-instance united-kingdom european-country
  ((has-name "United Kingdom")
   (has-capital london)))

(def-instance london capital-city
  ((has-name "London")
   (is-capital-of united-kingdom)))

(def-instance united-states north-american-country
  ((has-name "United States")
   (has-capital washington-dc)))

(def-instance washington-dc capital-city
  ((has-name "Washington D.C.")
   (is-capital-of united-states)))

(def-instance uruguay south-american-country
  ((has-name "Uruguay")
   (has-capital montevideo)))

(def-instance montevideo capital-city
  ((has-name "Montevideo")
   (is-capital-of uruguay)))

(def-instance uzbekistan asian-country
  ((has-name "Uzbekistan")
   (has-capital tashkent)))

(def-instance tashkent capital-city
  ((has-name "Tashkent")
   (is-capital-of uzbekistan)))

(def-instance vanuatu oceanian-country
  ((has-name "Vanuatu")
   (has-capital port-vila)))

(def-instance port-vila capital-city
  ((has-name "Port-Vila")
   (is-capital-of vanuatu)))

(def-instance vatican-city european-country
  ((has-name "Vatican City")
   (has-capital vatican-city)))

(def-instance vatican-city capital-city
  ((has-name "Vatican City")
   (is-capital-of vatican-city)))

(def-instance venezuela south-american-country
  ((has-name "Venezuela")
   (has-capital caracas)))

(def-instance caracas capital-city
  ((has-name "Caracas")
   (is-capital-of venezuela)))

(def-instance vietnam asian-country
  ((has-name "Vietnam")
   (has-capital hanoi)))

(def-instance hanoi capital-city
  ((has-name "Hanoi")
   (is-capital-of vietnam)))

(def-instance western-samoa oceanian-country
  ((has-name "Western Samoa")
   (has-capital apia)))

(def-instance apia capital-city
  ((has-name "Apia")
   (is-capital-of western-samoa)))

(def-instance yemen asian-country
  ((has-name "Yemen")
   (has-capital sana)))

(def-instance sana capital-city
  ((has-name "Sana")
   (is-capital-of yemen)))

(def-instance serbia-and-montenegro european-country
  ((has-name "Serbia and Montenegro")
   (has-capital belgrade)))

(def-instance belgrade capital-city
  ((has-name "Belgrade")
   (is-capital-of serbia-and-montenegro)))

(def-instance zambia african-country
  ((has-name "Zambia")
   (has-capital lusaka)))

(def-instance lusaka capital-city
  ((has-name "Lusaka")
   (is-capital-of zambia)))

(def-instance zimbabwe african-country
  ((has-name "Zimbabwe")
   (has-capital harare)))

(def-instance harare capital-city
  ((has-name "Harare")
   (is-capital-of zimbabwe)))

(def-instance scotland european-country
  ((has-name "Scotland")
   (has-capital Edinburgh)))

(def-instance edinburgh capital-city
  ((has-name "Edinburgh")
   (is-capital-of scotland)))

(def-instance wales european-country
  ((has-name "Wales")
   (has-capital cardiff)))

(def-instance cardiff capital-city
  ((has-name "Cardiff")
   (is-capital-of wales)))

(def-instance northern-ireland european-country
  ((has-name "northern ireland")
   (has-capital  belfast)))

(def-instance belfast capital-city
  ((has-name "Belfast")
   (is-capital-of northern-ireland)))

(def-instance Professional-occupations social-class)

(def-instance Managerial-and-Technical-occupations social-class)

(def-instance Skilled-occupations-non-manual  social-class)

(def-instance Skilled-occupations-manual  social-class)

(def-instance Partly-skilled-occupations  social-class)

(def-instance Unskilled-occupations  social-class)

