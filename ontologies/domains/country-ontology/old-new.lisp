;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology country-ontology)

(def-class geo-political-region ()
  ((has-name :type string)))

(def-class country (geo-political-region )
  ((has-capital :type capital-city)))



(def-class european-country (country))

(def-class asian-country (country))

(def-class north-american-country (country))

(def-class south-american-country (country))

(def-class oceanian-country (country))

(def-class african-country (country))

(def-instance afghanistan asian-country
  ((has-name "Afghanistan")
   (has-capital kabul)))



(def-instance albania european-country
  ((has-name "Albania")
   (has-capital tirane)))



(def-instance algeria african-country
  ((has-name "Algeria")
   (has-capital algers)))



(def-instance andorra european-country
  ((has-name "Andorra")
   (has-capital andorra-la-vella)))



(def-instance angola african-country
  ((has-name "Angola")
   (has-capital luanda)))



(def-instance antigua-and-barbuda north-american-country
  ((has-name "Antigua and Barbuda")
   (has-capital st-johns)))



(def-instance argentina south-american-country
  ((has-name "Argentina")
   (has-capital buenos-aires)))



(def-instance armenia european-country
  ((has-name "Armenia")
   (has-capital yerevan)))



(def-instance australia oceanian-country
  ((has-name "Australia")
   (has-capital canberra)))



(def-instance austria european-country
  ((has-name "Austria")
   (has-capital vienna)))



(def-instance azerbaijan asian-country
  ((has-name "Azerbaijan")
   (has-capital baku)))



(def-instance bahamas north-american-country
  ((has-name "Bahamas")
   (has-capital nassau)))



(def-instance bahrain asian-country
  ((has-name "Bahrain")
   (has-capital manama)))



(def-instance bangladesh asian-country
  ((has-name "Bangladesh")
   (has-capital dhaka)))



(def-instance barbados north-american-country
  ((has-name "Barbados")
   (has-capital bridgetown)))



(def-instance belarus european-country
  ((has-name "Belarus")
   (has-capital minsk)))



(def-instance belgium european-country
  ((has-name "Belgium")
   (has-capital brussels)))



(def-instance belize north-american-country
  ((has-name "Belize")
   (has-capital belmopan)))



(def-instance benin african-country
  ((has-name "Benin")
   (has-capital port-novo)))



(def-instance bhutan asian-country
  ((has-name "Bhutan")
   (has-capital thimphu)))



(def-instance bolivia south-american-country
  ((has-name "Bolivia")
   (has-capital sucre)))



(def-instance bosnia-and-herzegovina european-country
  ((has-name "Bosnia and Herzegovina")
   (has-capital sarajevo)))



(def-instance botswana african-country
  ((has-name "Botswana")
   (has-capital gaborone)))



(def-instance brazil south-american-country
  ((has-name "Brazil")
   (has-capital brasilia)))



(def-instance brunei asian-country
  ((has-name "Brunei")
   (has-capital bander-seri-begawan)))



(def-instance bulgaria european-country
  ((has-name "Bulgaria")
   (has-capital sofia)))



(def-instance burkina-faso african-country
  ((has-name "Burkina Faso")
   (has-capital ouagadougou)))



(def-instance burma asian-country
  ((has-name "Burma")
   (has-capital yangon)))



(def-instance burundi african-country
  ((has-name "Burundi")
   (has-capital bujumbura)))



(def-instance cambodia asian-country
  ((has-name "Cambodia")
   (has-capital phnom-penh)))



(def-instance cameroon south-american-country
  ((has-name "Cameroon")
   (has-capital yaounde)))



(def-instance canada north-american-country
  ((has-name "Canada")
   (has-capital ottawa)))



(def-instance cape-verde european-country
  ((has-name "Cape Verde")
   (has-capital praia)))



(def-instance central-african-republic african-country
  ((has-name "Central African Republic")
   (has-capital bangui)))



(def-instance chad african-country
  ((has-name "Chad")
   (has-capital ndjamena)))



(def-instance chile south-american-country
  ((has-name "Chile")
   (has-capital santiago)))



(def-instance china asian-country
  ((has-name "China")
   (has-capital beijing)))



(def-instance colombia south-american-country
  ((has-name "Colombia")
   (has-capital bogota)))



(def-instance comoros african-country
  ((has-name "Comoros")
   (has-capital moroni)))



(def-instance congo african-country
  ((has-name "Congo")
   (has-capital brazzaville)))



(def-instance democratic-republic-of-congo african-country
  ((has-name "Democratic Republic of Congo")
   (has-capital kinshasa)))



(def-instance costa-rica north-american-country
  ((has-name "Costa Rica")
   (has-capital san-jose)))



(def-instance ivory-coast african-country
  ((has-name "Ivory Coast")
   (has-capital yamoussoukro)))



(def-instance croatia european-country
  ((has-name "Croatia")
   (has-capital zagreb)))



(def-instance cuba north-american-country
  ((has-name "Cuba")
   (has-capital havana)))



(def-instance cyprus european-country
  ((has-name "Cyprus")
   (has-capital nicosia)))



(def-instance czech-republic european-country
  ((has-name "Czech Republic")
   (has-capital prague)))



(def-instance denmark european-country
  ((has-name "Denmark")
   (has-capital copenhagen)))



(def-instance djibouti african-country
  ((has-name "Djibouti")
   (has-capital djibouti)))



(def-instance dominica north-american-country
  ((has-name "Dominica")
   (has-capital roseau)))



(def-instance dominican-republic north-american-country
  ((has-name "Dominican Republic")
   (has-capital santo-domingo)))



(def-instance ecuador south-american-country
  ((has-name "Ecuador")
   (has-capital quito)))



(def-instance egypt african-country
  ((has-name "Egypt")
   (has-capital cairo)))



(def-instance el-salvador north-american-country
  ((has-name "El Salvador")
   (has-capital san-salvador)))



(def-instance equatorial-guinea african-country
  ((has-name "Equatorial Guinea")
   (has-capital malabo)))



(def-instance eritrea african-country
  ((has-name "Eritrea")
   (has-capital asmara)))



(def-instance estonia european-country
  ((has-name "Estonia")
   (has-capital tallinn)))



(def-instance ethiopia african-country
  ((has-name "Ethiopia")
   (has-capital addis-ababa)))



(def-instance fiji oceanian-country
  ((has-name "Fiji")
   (has-capital suva)))



(def-instance finland european-country
  ((has-name "Finland")
   (has-capital helsinki)))



(def-instance france european-country
  ((has-name "France")
   (has-capital paris)))



(def-instance gabon african-country
  ((has-name "Gabon")
   (has-capital liberville)))



(def-instance gambia african-country
  ((has-name "Gambia")
   (has-capital banjul)))



(def-instance georgia european-country
  ((has-name "Georgia")
   (has-capital tbilisi)))



(def-instance germany european-country
  ((has-name "Germany")
   (has-capital berlin)))



(def-instance ghana european-country
  ((has-name "Ghana")
   (has-capital accra)))



(def-instance greece european-country
  ((has-name "Greece")
   (has-capital athens)))



(def-instance grenada north-american-country
  ((has-name "Grenada")
   (has-capital st-georges)))



(def-instance guatemala north-american-country
  ((has-name "Guatemala")
   (has-capital guatemala-city)))



(def-instance guinea african-country
  ((has-name "Guinea")
   (has-capital conakry)))



(def-instance guinea-bissau african-country
  ((has-name "Guinea-Bissau")
   (has-capital bissau)))



(def-instance guyana south-american-country
  ((has-name "Guyana")
   (has-capital georgetown)))



(def-instance haiti north-american-country
  ((has-name "Haiti")
   (has-capital port-au-prince)))



(def-instance honduras north-american-country
  ((has-name "Honduras")
   (has-capital tegucigalpa)))



(def-instance hungary european-country
  ((has-name "Hungary")
   (has-capital budapest)))



(def-instance iceland european-country
  ((has-name "Iceland")
   (has-capital reykjavik)))



(def-instance india asian-country
  ((has-name "India")
   (has-capital new-delhi)))



(def-instance indonesia asian-country
  ((has-name "Indonesia")
   (has-capital jakarta)))



(def-instance iran asian-country
  ((has-name "Iran")
   (has-capital tehran)))



(def-instance iraq asian-country
  ((has-name "Iraq")
   (has-capital baghdad)))



(def-instance ireland european-country
  ((has-name "Ireland")
   (has-capital dublin)))



(def-instance israel asian-country
  ((has-name "Israel")
   (has-capital jerusalem)))



(def-instance italy european-country
  ((has-name "Italy")
   (has-capital rome)))



(def-instance jamaica north-american-country
  ((has-name "Jamaica")
   (has-capital kingston)))



(def-instance japan asian-country
  ((has-name "Japan")
   (has-capital tokyo)))



(def-instance jordan asian-country
  ((has-name "Jordan")
   (has-capital amman)))



(def-instance kazakstan asian-country
  ((has-name "Kazakstan")
   (has-capital astana)))



(def-instance kenya african-country
  ((has-name "Kenya")
   (has-capital nairobi)))



(def-instance kiribati oceanian-country
  ((has-name "Kiribati")
   (has-capital bairiki)))



(def-instance north-korea asian-country
  ((has-name "North Korea")
   (has-capital pyongyang)))



(def-instance south-korea asian-country
  ((has-name "South Korea")
   (has-capital seoul)))



(def-instance kuwait asian-country
  ((has-name "Kuwait")
   (has-capital kuwait-city)))



(def-instance kyrgyzstan asian-country
  ((has-name "Kyrgyzstan")
   (has-capital bishkek)))



(def-instance laos asian-country
  ((has-name "Laos")
   (has-capital vientiane)))



(def-instance latvia european-country
  ((has-name "Latvia")
   (has-capital riga)))



(def-instance lebanon asian-country
  ((has-name "Lebanon")
   (has-capital beirut)))



(def-instance lesotho african-country
  ((has-name "Lesotho")
   (has-capital maseru)))



(def-instance liberia african-country
  ((has-name "Liberia")
   (has-capital monrovia)))



(def-instance libya african-country
  ((has-name "Libya")
   (has-capital tripoli)))



(def-instance liechtenstein european-country
  ((has-name "Liechtenstein")
   (has-capital vaduz)))



(def-instance lithuania european-country
  ((has-name "Lithuania")
   (has-capital vilnius)))



(def-instance luxembourg european-country
  ((has-name "Luxembourg")
   (has-capital luxembourg)))



(def-instance macedonia european-country
  ((has-name "Macedonia")
   (has-capital skopje)))



(def-instance madagascar african-country
  ((has-name "Madagascar")
   (has-capital antananarivo)))



(def-instance malawi african-country
  ((has-name "Malawi")
   (has-capital lilongwe)))



(def-instance malaysia asian-country
  ((has-name "Malaysia")
   (has-capital kuala-lumpur)))



(def-instance maldives asian-country
  ((has-name "Maldives")
   (has-capital male)))



(def-instance mali african-country
  ((has-name "Mali")
   (has-capital bamako)))



(def-instance malta european-country
  ((has-name "Malta")
   (has-capital valletta)))



(def-instance marshall-islands oceanian-country
  ((has-name "Marshall Islands")
   (has-capital majuro)))



(def-instance mauritania african-country
  ((has-name "Mauritania")
   (has-capital nouakchott)))



(def-instance mauritius african-country
  ((has-name "Mauritius")
   (has-capital port-louis)))



(def-instance mexico north-american-country
  ((has-name "Mexico")
   (has-capital mexico-city)))



(def-instance micronesia oceanian-country
  ((has-name "Micronesia")
   (has-capital palikir)))



(def-instance moldova european-country
  ((has-name "Moldova")
   (has-capital chisinau)))



(def-instance monaco european-country
  ((has-name "Monaco")
   (has-capital monaco)))



(def-instance mongolia asian-country
  ((has-name "Mongolia")
   (has-capital ulan-bator)))



(def-instance morocco african-country
  ((has-name "Morocco")
   (has-capital rabat)))



(def-instance mozambique african-country
  ((has-name "Mozambique")
   (has-capital maputo)))



(def-instance namibia african-country
  ((has-name "Namibia")
   (has-capital windhoek)))



(def-instance nauru oceanian-country
  ((has-name "Nauru")
   (has-capital no-official-capital)))



(def-instance nepal asian-country
  ((has-name "Nepal")
   (has-capital kathmandu)))



(def-instance netherlands european-country
  ((has-name "Netherlands")
   (has-capital amsterdam)))



(def-instance new-zealand oceanian-country
  ((has-name "New Zealand")
   (has-capital wellington)))



(def-instance nicaragua north-american-country
  ((has-name "Nicaragua")
   (has-capital managua)))



(def-instance niger african-country
  ((has-name "Niger")
   (has-capital niamey)))



(def-instance nigeria african-country
  ((has-name "Nigeria")
   (has-capital abuja)))



(def-instance norway european-country
  ((has-name "Norway")
   (has-capital oslo)))



(def-instance oman asian-country
  ((has-name "Oman")
   (has-capital muscat)))



(def-instance pakistan asian-country
  ((has-name "Pakistan")
   (has-capital islamabad)))



(def-instance palau oceanian-country
  ((has-name "Palau")
   (has-capital koror)))



(def-instance panama north-american-country
  ((has-name "Panama")
   (has-capital panama-city)))



(def-instance papua-new-guinea oceanian-country
  ((has-name "Papua New Guinea")
   (has-capital port-moresby)))



(def-instance paraguay south-american-country
  ((has-name "Paraguay")
   (has-capital asuncion)))



(def-instance peru south-american-country
  ((has-name "Peru")
   (has-capital lima)))



(def-instance philippines asian-country
  ((has-name "Philippines")
   (has-capital manila)))



(def-instance poland european-country
  ((has-name "Poland")
   (has-capital warsaw)))



(def-instance portugal european-country
  ((has-name "Portugal")
   (has-capital lisbon)))



(def-instance qatar asian-country
  ((has-name "Qatar")
   (has-capital doha)))



(def-instance romania european-country
  ((has-name "Romania")
   (has-capital bucharest)))



(def-instance russian-federation asian-country
  ((has-name "Russian Federation")
   (has-capital moscow)))



(def-instance rwanda african-country
  ((has-name "Rwanda")
   (has-capital kigali)))



(def-instance saint-kitts-and-nevis north-american-country
  ((has-name "Saint Kitts and Nevis")
   (has-capital basseterre)))



(def-instance saint-lucia north-american-country
  ((has-name "Saint Lucia")
   (has-capital castries)))



(def-instance saint-vincent-and-the-grenadines north-american-country
  ((has-name "Saint Vincent and the Grenadines")
   (has-capital kingstown)))



(def-instance san-marino european-country
  ((has-name "San Marino")
   (has-capital san-marino)))



(def-instance sao-tome-and-principe african-country
  ((has-name "Sao Tome and Principe")
   (has-capital sao-tome)))



(def-instance saudi-arabia asian-country
  ((has-name "Saudi Arabia")
   (has-capital riyadh)))



(def-instance senegal african-country
  ((has-name "Senegal")
   (has-capital dakar)))



(def-instance seychelles african-country
  ((has-name "Seychelles")
   (has-capital victoria)))



(def-instance sierra-leone african-country
  ((has-name "Sierra Leone")
   (has-capital freetown)))



(def-instance singapore asian-country
  ((has-name "Singapore")
   (has-capital singapore-city)))



(def-instance slovakia european-country
  ((has-name "Slovakia")
   (has-capital bratislava)))



(def-instance slovenia european-country
  ((has-name "Slovenia")
   (has-capital ljubljana)))



(def-instance solomon-islands oceanian-country
  ((has-name "Solomon Islands")
   (has-capital honiara)))



(def-instance somalia african-country
  ((has-name "Somalia")
   (has-capital mogadishu)))



(def-instance south-africa african-country
  ((has-name "South Africa")
   (has-capital pretoria)))



(def-instance spain european-country
  ((has-name "Spain")
   (has-capital madrid)))



(def-instance sri-lanka asian-country
  ((has-name "Sri Lanka")
   (has-capital colombo)))



(def-instance sudan african-country
  ((has-name "Sudan")
   (has-capital khartoum)))



(def-instance suriname south-american-country
  ((has-name "Suriname")
   (has-capital paramaribo)))



(def-instance swaziland african-country
  ((has-name "Swaziland")
   (has-capital mbabane)))



(def-instance sweden european-country
  ((has-name "Sweden")
   (has-capital stockholm)))



(def-instance switzerland european-country
  ((has-name "Switzerland")
   (has-capital bern)))



(def-instance syria asian-country
  ((has-name "Syria")
   (has-capital damascus)))



(def-instance taiwan asian-country
  ((has-name "Taiwan")
   (has-capital taipei)))



(def-instance tajikistan asian-country
  ((has-name "Tajikistan")
   (has-capital dushanbe)))



(def-instance tanzania african-country
  ((has-name "Tanzania")
   (has-capital dodoma)))



(def-instance thailand asian-country
  ((has-name "Thailand")
   (has-capital bangkok)))



(def-instance togo oceanian-country
  ((has-name "Togo")
   (has-capital lome)))



(def-instance tonga oceanian-country
  ((has-name "Tonga")
   (has-capital nukualofa)))



(def-instance trinidad-and-tobago north-american-country
  ((has-name "Trinidad and Tobago")
   (has-capital port-of-spain)))



(def-instance tunisia african-country
  ((has-name "Tunisia")
   (has-capital tunis)))



(def-instance turkey asian-country
  ((has-name "Turkey")
   (has-capital ankara)))



(def-instance turkmenistan asian-country
  ((has-name "Turkmenistan")
   (has-capital ashgabat)))



(def-instance tuvalu oceanian-country
  ((has-name "Tuvalu")
   (has-capital funafuti)))



(def-instance uganda african-country
  ((has-name "Uganda")
   (has-capital kampala)))



(def-instance ukraine european-country
  ((has-name "Ukraine")
   (has-capital kiev)))



(def-instance united-arab-emirates asian-country
  ((has-name "United Arab Emirates")
   (has-capital abu-dhabi)))



(def-instance united-kingdom european-country
  ((has-name "United Kingdom")
   (has-capital london)))



(def-instance united-states north-american-country
  ((has-name "United States")
   (has-capital washington-dc)))



(def-instance uruguay south-american-country
  ((has-name "Uruguay")
   (has-capital montevideo)))



(def-instance uzbekistan asian-country
  ((has-name "Uzbekistan")
   (has-capital tashkent)))



(def-instance vanuatu oceanian-country
  ((has-name "Vanuatu")
   (has-capital port-vila)))



(def-instance vatican-city european-country
  ((has-name "Vatican City")
   (has-capital vatican-city)))



(def-instance venezuela south-american-country
  ((has-name "Venezuela")
   (has-capital caracas)))



(def-instance vietnam asian-country
  ((has-name "Vietnam")
   (has-capital hanoi)))



(def-instance western-samoa oceanian-country
  ((has-name "Western Samoa")
   (has-capital apia)))



(def-instance yemen asian-country
  ((has-name "Yemen")
   (has-capital sana)))



(def-instance serbia-and-montenegro european-country
  ((has-name "Serbia and Montenegro")
   (has-capital belgrade)))



(def-instance zambia african-country
  ((has-name "Zambia")
   (has-capital lusaka)))



(def-instance zimbabwe african-country
  ((has-name "Zimbabwe")
   (has-capital harare)))



