;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology uk-location-ontology)

(def-class uk-town (town))

(def-class uk-city (city))

(def-class uk-village (village))

(def-class english-city (uk-city))

(def-class scotish-city (uk-city))

(def-class welsh-city (uk-city))

(def-class irish-city (uk-city))

(def-instance London english-city)

(def-instance Andover english-city)

(def-instance Axminster english-city)

(def-instance Banbury english-city)

(def-instance Barnsley english-city)

(def-instance Basingstoke english-city)

(def-instance Bath english-city)

(def-instance Bedford english-city)

(def-instance Berwick_upon_Tweed english-city)

(def-instance Birmingham english-city)

(def-instance Bishops_Stortford english-city)

(def-instance Blackburn english-city)

(def-instance Blackpool english-city)

(def-instance Bolton english-city)

(def-instance Bournemouth english-city)

(def-instance Bradford english-city)

(def-instance Brighton english-city)

(def-instance Bristol english-city)

(def-instance Burnley english-city)

(def-instance Bury_St_Edmonds english-city)

(def-instance Cambridge english-city)

(def-instance Carlisle english-city)

(def-instance Chelmsford english-city)

(def-instance Cheltenham english-city)

(def-instance Chester english-city)

(def-instance Chichester english-city)

(def-instance Colchester english-city)

(def-instance Coventry english-city)

(def-instance Crawley english-city)

(def-instance Croydon english-city)

(def-instance Darlington english-city)

(def-instance Dartford english-city)

(def-instance Derby english-city)

(def-instance Doncaster english-city)

(def-instance Douglas_IoM english-city)

(def-instance Dover english-city)

(def-instance Enfield english-city)

(def-instance Epsom english-city)

(def-instance Exeter english-city)

(def-instance Gloucester english-city)

(def-instance Grantham english-city)

(def-instance Guildford english-city)

(def-instance Halifax english-city)

(def-instance Harrogate english-city)

(def-instance Hastings english-city)

(def-instance Hawes english-city)

(def-instance Hereford english-city)

(def-instance High_Wycombe english-city)

(def-instance Huddersfield english-city)

(def-instance Hull english-city)

(def-instance Ipswich english-city)

(def-instance Kendal english-city)

(def-instance Kings_Lynn english-city)

(def-instance Lancaster english-city)

(def-instance Leeds english-city)

(def-instance Leicester english-city)

(def-instance Lincoln english-city)

(def-instance Liverpool english-city)

(def-instance London english-city)

(def-instance Lowestoft english-city)

(def-instance Ludlow english-city)

(def-instance Luton english-city)

(def-instance Maidstone english-city)

(def-instance Manchester english-city)

(def-instance Middlesbrough english-city)

(def-instance Milton_Keynes english-city)

(def-instance Newcastle english-city)

(def-instance Newmarket english-city)

(def-instance Newport english-city)

(def-instance Northampton english-city)

(def-instance Norwich english-city)

(def-instance Nottingham english-city)

(def-instance Okehampton english-city)

(def-instance Oxford english-city)

(def-instance Penzance english-city)

(def-instance Peterborough english-city)

(def-instance Plymouth english-city)

(def-instance Portsmouth english-city)

(def-instance Preston english-city)

(def-instance Reading english-city)

(def-instance Rochdale english-city)

(def-instance Rugby english-city)

(def-instance Salisbury english-city)

(def-instance Scunthorpe english-city)

(def-instance Sheffield english-city)

(def-instance Shrewsbury english-city)

(def-instance Skegness english-city)

(def-instance Slough english-city)

(def-instance Southampton english-city)

(def-instance Southend_on_Sea english-city)

(def-instance Staines english-city)

(def-instance Stanhope english-city)

(def-instance St_Helier english-city)

(def-instance St_Marys english-city)

(def-instance Stoke_on_Trent english-city)

(def-instance Stratford_upon_Avon english-city)

(def-instance Sunderland english-city)

(def-instance Swindon english-city)

(def-instance Taunton english-city)

(def-instance Thirsk english-city)

(def-instance Torquay english-city)

(def-instance Tunbridge_Wells english-city)

(def-instance Warminster english-city)

(def-instance Warrington english-city)

(def-instance Warwick english-city)

(def-instance Watford english-city)

(def-instance Western_Super_Mare english-city)

(def-instance Weymouth english-city)

(def-instance Whitby english-city)

(def-instance Winchester english-city)

(def-instance Wolverhampton english-city)

(def-instance Worcester english-city)

(def-instance Yeovil english-city)

(def-instance York english-city)

(def-instance Edinburgh scotish-city)

(def-instance Aberdeen scotish-city)

(def-instance Aviemore scotish-city)

(def-instance Ayr scotish-city)

(def-instance Bathgate scotish-city)

(def-instance Braemar scotish-city)

(def-instance Campbeltown scotish-city)

(def-instance Cumbernauld scotish-city)

(def-instance Dornoch scotish-city)

(def-instance Dumfries scotish-city)

(def-instance Dunbar scotish-city)

(def-instance Dundee scotish-city)

(def-instance Edinburgh scotish-city)

(def-instance Elgin scotish-city)

(def-instance Falkirk scotish-city)

(def-instance Fort_Augutus scotish-city)

(def-instance Fort_William scotish-city)

(def-instance Fraserburgh scotish-city)

(def-instance Galashiels scotish-city)

(def-instance Glasgow scotish-city)

(def-instance Gleneagles scotish-city)

(def-instance Hawick scotish-city)

(def-instance Huntly scotish-city)

(def-instance Inverness scotish-city)

(def-instance Kelso scotish-city)

(def-instance Killin scotish-city)

(def-instance Kilmarnock scotish-city)

(def-instance Kings_House scotish-city)

(def-instance Kirkcaldy scotish-city)

(def-instance Kirkcudbright scotish-city)

(def-instance Kirkwall scotish-city)

(def-instance Lanark scotish-city)

(def-instance Lerwick scotish-city)

(def-instance Lochboisedale scotish-city)

(def-instance Montrose scotish-city)

(def-instance Motherwell scotish-city)

(def-instance Oban scotish-city)

(def-instance Paisley scotish-city)

(def-instance Peebles scotish-city)

(def-instance Perth scotish-city)

(def-instance Peterhead scotish-city)

(def-instance Pitlochry scotish-city)

(def-instance Port_Askaig scotish-city)

(def-instance Portree scotish-city)

(def-instance Sanquar scotish-city)

(def-instance St_Andrews scotish-city)

(def-instance Stirling scotish-city)

(def-instance Stranraer scotish-city)

(def-instance Stornoway scotish-city)

(def-instance Tiree scotish-city)

(def-instance Tobermory scotish-city)

(def-instance Ullapool scotish-city)

(def-instance Cardiff welsh-city)

(def-instance Aberystwyth welsh-city)

(def-instance Bangor welsh-city)

(def-instance Brecon welsh-city)

(def-instance Bridgend welsh-city)

(def-instance Cardigan welsh-city)

(def-instance Carmarthen welsh-city)

(def-instance Cardiff welsh-city)

(def-instance Colwyn-bay welsh-city)

(def-instance Holyhead welsh-city)

(def-instance Llandrindod_Wells welsh-city)

(def-instance Merthye_Tydfil welsh-city)

(def-instance Milford_Haven welsh-city)

(def-instance Portmadoc welsh-city)

(def-instance Swansea welsh-city)

(def-instance Welshpool welsh-city)

(def-instance WrexhamN welsh-city)

(def-instance Armagh irish-city)

(def-instance Ballymena irish-city)

(def-instance Belfast irish-city)

(def-instance Coleraine irish-city)

(def-instance Enniskillen irish-city)

(def-instance Larne irish-city)

(def-instance Londonderry irish-city)

(def-instance Newry irish-city)

(def-instance Omagh irish-city)

