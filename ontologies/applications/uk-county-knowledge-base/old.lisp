;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology uk-county-knowledge-base)

(def-class uk-location ())

(def-class uk-county (uk-location)
  ((has-name :type string)
   (has-alternative-name :type string)
   (has-town :type uk-town)))

(def-class uk-town (uk-location)
  ((has-name :type string)
   (has-county :type uk-county)))

(def-class irish-town (uk-town))

(def-class scottish-town (uk-town))

(def-class welsh-town (uk-town))

(def-class english-town (uk-town))

(def-class irish-county (uk-county))

(def-class scottish-county (uk-county))

(def-class welsh-county (uk-county))

(def-class english-county (uk-county))

(def-rule close-to-rule
    ((close-to ?x ?y)
     if
     (has-county ?x ?county)
     (has-county ?y ?county)
     (exec (output "~%The towns ~(~a~) and ~(~a~) are close to each other because they are both in ~(~a~).~%" ?x ?y ?county))))

(def-instance bedfordshire english-county
  ((has-name "bedfordshire")
   (has-alternative-name "")
   (has-town arlesey bedford biggleswade dunstable henlow leighton-buzzard luton sandy shefford)))

(def-instance dunstable english-town
  ((has-county  bedfordshire)))

(def-instance aberdeen scottish-town
  ((has-name "Aberdeen")
   (has-county aberdeenshire)))

(def-instance aboyne scottish-town
  ((has-name "Aboyne")
   (has-county aberdeenshire)))

(def-instance alford scottish-town
  ((has-name "Alford")
   (has-county aberdeenshire)))

(def-instance ballater scottish-town
  ((has-name "Ballater")
   (has-county aberdeenshire)))

(def-instance ellon scottish-town
  ((has-name "Ellon")
   (has-county aberdeenshire)))

(def-instance fraserburgh scottish-town
  ((has-name "Fraserburgh")
   (has-county aberdeenshire)))

(def-instance huntly scottish-town
  ((has-name "Huntly")
   (has-county aberdeenshire)))

(def-instance insch scottish-town
  ((has-name "Insch")
   (has-county aberdeenshire)))

(def-instance inverurie scottish-town
  ((has-name "Inverurie")
   (has-county aberdeenshire)))

(def-instance milltimber scottish-town
  ((has-name "Milltimber")
   (has-county aberdeenshire)))

(def-instance peterculter scottish-town
  ((has-name "Peterculter")
   (has-county aberdeenshire)))

(def-instance peterhead scottish-town
  ((has-name "Peterhead")
   (has-county aberdeenshire)))

(def-instance strathdon scottish-town
  ((has-name "Strathdon")
   (has-county aberdeenshire)))

(def-instance turriff scottish-town
  ((has-name "Turriff")
   (has-county aberdeenshire)))

(def-instance westhill scottish-town
  ((has-name "Westhill")
   (has-county aberdeenshire)))

(def-instance amlwch welsh-town
  ((has-name "Amlwch")
   (has-county anglesey)))

(def-instance beaumaris welsh-town
  ((has-name "Beaumaris")
   (has-county anglesey)))

(def-instance bodorgan welsh-town
  ((has-name "Bodorgan")
   (has-county anglesey)))

(def-instance brynteg welsh-town
  ((has-name "Brynteg")
   (has-county anglesey)))

(def-instance cemaes-bay welsh-town
  ((has-name "Cemaes Bay")
   (has-county anglesey)))

(def-instance dulas welsh-town
  ((has-name "Dulas")
   (has-county anglesey)))

(def-instance gaerwen welsh-town
  ((has-name "Gaerwen")
   (has-county anglesey)))

(def-instance holyhead welsh-town
  ((has-name "Holyhead")
   (has-county anglesey)))

(def-instance llanbedrgoch welsh-town
  ((has-name "Llanbedrgoch")
   (has-county anglesey)))

(def-instance llanerchymedd welsh-town
  ((has-name "Llanerchymedd")
   (has-county anglesey)))

(def-instance llanfairpwllgwyngyll welsh-town
  ((has-name "Llanfairpwllgwyngyll")
   (has-county anglesey)))

(def-instance llangefni welsh-town
  ((has-name "Llangefni")
   (has-county anglesey)))

(def-instance marianglas welsh-town
  ((has-name "Marianglas")
   (has-county anglesey)))

(def-instance menai-bridge welsh-town
  ((has-name "Menai Bridge")
   (has-county anglesey)))

(def-instance moelfre welsh-town
  ((has-name "Moelfre")
   (has-county anglesey)))

(def-instance pentraeth welsh-town
  ((has-name "Pentraeth")
   (has-county anglesey)))

(def-instance penysarn welsh-town
  ((has-name "Penysarn")
   (has-county anglesey)))

(def-instance rhosgoch welsh-town
  ((has-name "Rhosgoch")
   (has-county anglesey)))

(def-instance rhosneigr welsh-town
  ((has-name "Rhosneigr")
   (has-county anglesey)))

(def-instance ty-croes welsh-town
  ((has-name "Ty Croes")
   (has-county anglesey)))

(def-instance tyn-y-gongl welsh-town
  ((has-name "Tyn-Y-Gongl")
   (has-county anglesey)))

(def-instance arbroath scottish-town
  ((has-name "Arbroath")
   (has-county angus)))

(def-instance brechin scottish-town
  ((has-name "Brechin")
   (has-county angus)))

(def-instance carnoustie scottish-town
  ((has-name "Carnoustie")
   (has-county angus)))

(def-instance dundee scottish-town
  ((has-name "Dundee")
   (has-county angus)))

(def-instance forfar scottish-town
  ((has-name "Forfar")
   (has-county angus)))

(def-instance kirriemuir scottish-town
  ((has-name "Kirriemuir")
   (has-county angus)))

(def-instance montrose scottish-town
  ((has-name "Montrose")
   (has-county angus)))

(def-instance acharacle scottish-town
  ((has-name "Acharacle")
   (has-county argyllshire)))

(def-instance appin scottish-town
  ((has-name "Appin")
   (has-county argyllshire)))

(def-instance ballachulish scottish-town
  ((has-name "Ballachulish")
   (has-county argyllshire)))

(def-instance bridge-of-orchy scottish-town
  ((has-name "Bridge Of Orchy")
   (has-county argyllshire)))

(def-instance cairndow scottish-town
  ((has-name "Cairndow")
   (has-county argyllshire)))

(def-instance campbeltown scottish-town
  ((has-name "Campbeltown")
   (has-county argyllshire)))

(def-instance colintraive scottish-town
  ((has-name "Colintraive")
   (has-county argyllshire)))

(def-instance dalmally scottish-town
  ((has-name "Dalmally")
   (has-county argyllshire)))

(def-instance dunoon scottish-town
  ((has-name "Dunoon")
   (has-county argyllshire)))

(def-instance inveraray scottish-town
  ((has-name "Inveraray")
   (has-county argyllshire)))

(def-instance isle-of-coll scottish-town
  ((has-name "Isle Of Coll")
   (has-county argyllshire)))

(def-instance isle-of-colonsay scottish-town
  ((has-name "Isle Of Colonsay")
   (has-county argyllshire)))

(def-instance isle-of-gigha scottish-town
  ((has-name "Isle Of Gigha")
   (has-county argyllshire)))

(def-instance isle-of-iona scottish-town
  ((has-name "Isle Of Iona")
   (has-county argyllshire)))

(def-instance isle-of-mull scottish-town
  ((has-name "Isle Of Mull")
   (has-county argyllshire)))

(def-instance kinlochleven scottish-town
  ((has-name "Kinlochleven")
   (has-county argyllshire)))

(def-instance lochgilphead scottish-town
  ((has-name "Lochgilphead")
   (has-county argyllshire)))

(def-instance oban scottish-town
  ((has-name "Oban")
   (has-county argyllshire)))

(def-instance tarbert scottish-town
  ((has-name "Tarbert")
   (has-county argyllshire)))

(def-instance taynuilt scottish-town
  ((has-name "Taynuilt")
   (has-county argyllshire)))

(def-instance tighnabruaich scottish-town
  ((has-name "Tighnabruaich")
   (has-county argyllshire)))

(def-instance ardrossan scottish-town
  ((has-name "Ardrossan")
   (has-county ayrshire)))

(def-instance ayr scottish-town
  ((has-name "Ayr")
   (has-county ayrshire)))

(def-instance beith scottish-town
  ((has-name "Beith")
   (has-county ayrshire)))

(def-instance cumnock scottish-town
  ((has-name "Cumnock")
   (has-county ayrshire)))

(def-instance dalry scottish-town
  ((has-name "Dalry")
   (has-county ayrshire)))

(def-instance darvel scottish-town
  ((has-name "Darvel")
   (has-county ayrshire)))

(def-instance galston scottish-town
  ((has-name "Galston")
   (has-county ayrshire)))

(def-instance girvan scottish-town
  ((has-name "Girvan")
   (has-county ayrshire)))

(def-instance irvine scottish-town
  ((has-name "Irvine")
   (has-county ayrshire)))

(def-instance kilbirnie scottish-town
  ((has-name "Kilbirnie")
   (has-county ayrshire)))

(def-instance kilmarnock scottish-town
  ((has-name "Kilmarnock")
   (has-county ayrshire)))

(def-instance kilwinning scottish-town
  ((has-name "Kilwinning")
   (has-county ayrshire)))

(def-instance largs scottish-town
  ((has-name "Largs")
   (has-county ayrshire)))

(def-instance mauchline scottish-town
  ((has-name "Mauchline")
   (has-county ayrshire)))

(def-instance maybole scottish-town
  ((has-name "Maybole")
   (has-county ayrshire)))

(def-instance newmilns scottish-town
  ((has-name "Newmilns")
   (has-county ayrshire)))

(def-instance prestwick scottish-town
  ((has-name "Prestwick")
   (has-county ayrshire)))

(def-instance saltcoats scottish-town
  ((has-name "Saltcoats")
   (has-county ayrshire)))

(def-instance skelmorlie scottish-town
  ((has-name "Skelmorlie")
   (has-county ayrshire)))

(def-instance stevenston scottish-town
  ((has-name "Stevenston")
   (has-county ayrshire)))

(def-instance troon scottish-town
  ((has-name "Troon")
   (has-county ayrshire)))

(def-instance west-kilbride scottish-town
  ((has-name "West Kilbride")
   (has-county ayrshire)))

(def-instance aberlour scottish-town
  ((has-name "Aberlour")
   (has-county banffshire)))

(def-instance ballindalloch scottish-town
  ((has-name "Ballindalloch")
   (has-county banffshire)))

(def-instance banff scottish-town
  ((has-name "Banff")
   (has-county banffshire)))

(def-instance buckie scottish-town
  ((has-name "Buckie")
   (has-county banffshire)))

(def-instance keith scottish-town
  ((has-name "Keith")
   (has-county banffshire)))

(def-instance macduff scottish-town
  ((has-name "Macduff")
   (has-county banffshire)))

(def-instance arlesey english-town
  ((has-name "Arlesey")
   (has-county bedfordshire)))

(def-instance bedford english-town
  ((has-name "Bedford")
   (has-county bedfordshire)))

(def-instance biggleswade english-town
  ((has-name "Biggleswade")
   (has-county bedfordshire)))

(def-instance dunstable english-town
  ((has-county  bedfordshire)))

(def-instance henlow english-town
  ((has-name "Henlow")
   (has-county bedfordshire)))

(def-instance leighton-buzzard english-town
  ((has-name "Leighton Buzzard")
   (has-county bedfordshire)))

(def-instance luton english-town
  ((has-name "Luton")
   (has-county bedfordshire)))

(def-instance sandy english-town
  ((has-name "Sandy")
   (has-county bedfordshire)))

(def-instance shefford english-town
  ((has-name "Shefford")
   (has-county bedfordshire)))

(def-instance abingdon english-town
  ((has-name "Abingdon")
   (has-county berkshire)))

(def-instance ascot english-town
  ((has-name "Ascot")
   (has-county berkshire)))

(def-instance bracknell english-town
  ((has-name "Bracknell")
   (has-county berkshire)))

(def-instance crowthorne english-town
  ((has-name "Crowthorne")
   (has-county berkshire)))

(def-instance didcot english-town
  ((has-name "Didcot")
   (has-county berkshire)))

(def-instance faringdon english-town
  ((has-name "Faringdon")
   (has-county berkshire)))

(def-instance hungerford english-town
  ((has-name "Hungerford")
   (has-county berkshire)))

(def-instance maidenhead english-town
  ((has-name "Maidenhead")
   (has-county berkshire)))

(def-instance newbury english-town
  ((has-name "Newbury")
   (has-county berkshire)))

(def-instance reading english-town
  ((has-name "Reading")
   (has-county berkshire)))

(def-instance sandhurst english-town
  ((has-name "Sandhurst")
   (has-county berkshire)))

(def-instance thatcham english-town
  ((has-name "Thatcham")
   (has-county berkshire)))

(def-instance wallingford english-town
  ((has-name "Wallingford")
   (has-county berkshire)))

(def-instance wantage english-town
  ((has-name "Wantage")
   (has-county berkshire)))

(def-instance windsor english-town
  ((has-name "Windsor")
   (has-county berkshire)))

(def-instance wokingham english-town
  ((has-name "Wokingham")
   (has-county berkshire)))

(def-instance cockburnspath scottish-town
  ((has-name "Cockburnspath")
   (has-county berwickshire)))

(def-instance coldstream scottish-town
  ((has-name "Coldstream")
   (has-county berwickshire)))

(def-instance duns scottish-town
  ((has-name "Duns")
   (has-county berwickshire)))

(def-instance earlston scottish-town
  ((has-name "Earlston")
   (has-county berwickshire)))

(def-instance eyemouth scottish-town
  ((has-name "Eyemouth")
   (has-county berwickshire)))

(def-instance gordon scottish-town
  ((has-name "Gordon")
   (has-county berwickshire)))

(def-instance lauder scottish-town
  ((has-name "Lauder")
   (has-county berwickshire)))

(def-instance brecon welsh-town
  ((has-name "Brecon")
   (has-county brecknockshire)))

(def-instance crickhowell welsh-town
  ((has-name "Crickhowell")
   (has-county brecknockshire)))

(def-instance llangammarch-wells welsh-town
  ((has-name "Llangammarch Wells")
   (has-county brecknockshire)))

(def-instance llanwrtyd-wells welsh-town
  ((has-name "Llanwrtyd Wells")
   (has-county brecknockshire)))

(def-instance amersham english-town
  ((has-name "Amersham")
   (has-county buckinghamshire)))

(def-instance aylesbury english-town
  ((has-name "Aylesbury")
   (has-county buckinghamshire)))

(def-instance beaconsfield english-town
  ((has-name "Beaconsfield")
   (has-county buckinghamshire)))

(def-instance bourne-end english-town
  ((has-name "Bourne End")
   (has-county buckinghamshire)))

(def-instance buckingham english-town
  ((has-name "Buckingham")
   (has-county buckinghamshire)))

(def-instance chalfont-st-giles english-town
  ((has-name "Chalfont St. Giles")
   (has-county buckinghamshire)))

(def-instance chesham english-town
  ((has-name "Chesham")
   (has-county buckinghamshire)))

(def-instance gerrards-cross english-town
  ((has-name "Gerrards Cross")
   (has-county buckinghamshire)))

(def-instance great-missenden english-town
  ((has-name "Great Missenden")
   (has-county buckinghamshire)))

(def-instance high-wycombe english-town
  ((has-name "High Wycombe")
   (has-county buckinghamshire)))

(def-instance iver english-town
  ((has-name "Iver")
   (has-county buckinghamshire)))

(def-instance marlow english-town
  ((has-name "Marlow")
   (has-county buckinghamshire)))

(def-instance milton-keynes english-town
  ((has-name "Milton Keynes")
   (has-county buckinghamshire)))

(def-instance newport-pagnell english-town
  ((has-name "Newport Pagnell")
   (has-county buckinghamshire)))

(def-instance olney english-town
  ((has-name "Olney")
   (has-county buckinghamshire)))

(def-instance princes-risborough english-town
  ((has-name "Princes Risborough")
   (has-county buckinghamshire)))

(def-instance slough english-town
  ((has-name "Slough")
   (has-county buckinghamshire)))

(def-instance bangor-ll welsh-town
  ((has-name "Bangor Ll")
   (has-county caernarfonshire)))

(def-instance betws-y-coed welsh-town
  ((has-name "Betws-Y-Coed")
   (has-county caernarfonshire)))

(def-instance caernarfon welsh-town
  ((has-name "Caernarfon")
   (has-county caernarfonshire)))

(def-instance conwy welsh-town
  ((has-name "Conwy")
   (has-county caernarfonshire)))

(def-instance criccieth welsh-town
  ((has-name "Criccieth")
   (has-county caernarfonshire)))

(def-instance dolwyddelan welsh-town
  ((has-name "Dolwyddelan")
   (has-county caernarfonshire)))

(def-instance garndolbenmaen welsh-town
  ((has-name "Garndolbenmaen")
   (has-county caernarfonshire)))

(def-instance llandudno welsh-town
  ((has-name "Llandudno")
   (has-county caernarfonshire)))

(def-instance llandudno-junction welsh-town
  ((has-name "Llandudno Junction")
   (has-county caernarfonshire)))

(def-instance llanfairfechan welsh-town
  ((has-name "Llanfairfechan")
   (has-county caernarfonshire)))

(def-instance penmaenmawr welsh-town
  ((has-name "Penmaenmawr")
   (has-county caernarfonshire)))

(def-instance porthmadog welsh-town
  ((has-name "Porthmadog")
   (has-county caernarfonshire)))

(def-instance pwllheli welsh-town
  ((has-name "Pwllheli")
   (has-county caernarfonshire)))

(def-instance trefriw welsh-town
  ((has-name "Trefriw")
   (has-county caernarfonshire)))

(def-instance y-felinheli welsh-town
  ((has-name "Y Felinheli")
   (has-county caernarfonshire)))

(def-instance berriedale scottish-town
  ((has-name "Berriedale")
   (has-county caithness)))

(def-instance dunbeath scottish-town
  ((has-name "Dunbeath")
   (has-county caithness)))

(def-instance halkirk scottish-town
  ((has-name "Halkirk")
   (has-county caithness)))

(def-instance latheron scottish-town
  ((has-name "Latheron")
   (has-county caithness)))

(def-instance lybster scottish-town
  ((has-name "Lybster")
   (has-county caithness)))

(def-instance thurso scottish-town
  ((has-name "Thurso")
   (has-county caithness)))

(def-instance wick scottish-town
  ((has-name "Wick")
   (has-county caithness)))

(def-instance cambridge english-town
  ((has-name "Cambridge")
   (has-county cambridgeshire)))

(def-instance chatteris english-town
  ((has-name "Chatteris")
   (has-county cambridgeshire)))

(def-instance ely english-town
  ((has-name "Ely")
   (has-county cambridgeshire)))

(def-instance march english-town
  ((has-name "March")
   (has-county cambridgeshire)))

(def-instance wisbech english-town
  ((has-name "Wisbech")
   (has-county cambridgeshire)))

(def-instance aberaeron welsh-town
  ((has-name "Aberaeron")
   (has-county cardiganshire)))

(def-instance aberystwyth welsh-town
  ((has-name "Aberystwyth")
   (has-county cardiganshire)))

(def-instance borth welsh-town
  ((has-name "Borth")
   (has-county cardiganshire)))

(def-instance bow-street welsh-town
  ((has-name "Bow Street")
   (has-county cardiganshire)))

(def-instance cardigan welsh-town
  ((has-name "Cardigan")
   (has-county cardiganshire)))

(def-instance lampeter welsh-town
  ((has-name "Lampeter")
   (has-county cardiganshire)))

(def-instance llanarth welsh-town
  ((has-name "Llanarth")
   (has-county cardiganshire)))

(def-instance llandysul welsh-town
  ((has-name "Llandysul")
   (has-county cardiganshire)))

(def-instance llanon welsh-town
  ((has-name "Llanon")
   (has-county cardiganshire)))

(def-instance llanrhystud welsh-town
  ((has-name "Llanrhystud")
   (has-county cardiganshire)))

(def-instance new-quay welsh-town
  ((has-name "New Quay")
   (has-county cardiganshire)))

(def-instance talybont-sy welsh-town
  ((has-name "Talybont Sy")
   (has-county cardiganshire)))

(def-instance tregaron welsh-town
  ((has-name "Tregaron")
   (has-county cardiganshire)))

(def-instance ystrad-meurig welsh-town
  ((has-name "Ystrad Meurig")
   (has-county cardiganshire)))

(def-instance ammanford welsh-town
  ((has-name "Ammanford")
   (has-county carmarthenshire)))

(def-instance burry-port welsh-town
  ((has-name "Burry Port")
   (has-county carmarthenshire)))

(def-instance carmarthen welsh-town
  ((has-name "Carmarthen")
   (has-county carmarthenshire)))

(def-instance clynderwen welsh-town
  ((has-name "Clynderwen")
   (has-county carmarthenshire)))

(def-instance ferryside welsh-town
  ((has-name "Ferryside")
   (has-county carmarthenshire)))

(def-instance kidwelly welsh-town
  ((has-name "Kidwelly")
   (has-county carmarthenshire)))

(def-instance llandeilo welsh-town
  ((has-name "Llandeilo")
   (has-county carmarthenshire)))

(def-instance llandovery welsh-town
  ((has-name "Llandovery")
   (has-county carmarthenshire)))

(def-instance llanelli welsh-town
  ((has-name "Llanelli")
   (has-county carmarthenshire)))

(def-instance llangadog welsh-town
  ((has-name "Llangadog")
   (has-county carmarthenshire)))

(def-instance llanwrda welsh-town
  ((has-name "Llanwrda")
   (has-county carmarthenshire)))

(def-instance llanybydder welsh-town
  ((has-name "Llanybydder")
   (has-county carmarthenshire)))

(def-instance newcastle-emlyn welsh-town
  ((has-name "Newcastle Emlyn")
   (has-county carmarthenshire)))

(def-instance pencader welsh-town
  ((has-name "Pencader")
   (has-county carmarthenshire)))

(def-instance whitland welsh-town
  ((has-name "Whitland")
   (has-county carmarthenshire)))

(def-instance guernsey english-town
  ((has-name "Guernsey")
   (has-county channel-isles)))

(def-instance jersey english-town
  ((has-name "Jersey")
   (has-county channel-isles)))

(def-instance alderley-edge english-town
  ((has-name "Alderley Edge")
   (has-county cheshire)))

(def-instance altrincham english-town
  ((has-name "Altrincham")
   (has-county cheshire)))

(def-instance birkenhead english-town
  ((has-name "Birkenhead")
   (has-county cheshire)))

(def-instance cheadle english-town
  ((has-name "Cheadle")
   (has-county cheshire)))

(def-instance chester english-town
  ((has-name "Chester")
   (has-county cheshire)))

(def-instance congleton english-town
  ((has-name "Congleton")
   (has-county cheshire)))

(def-instance crewe english-town
  ((has-name "Crewe")
   (has-county cheshire)))

(def-instance dukinfield english-town
  ((has-name "Dukinfield")
   (has-county cheshire)))

(def-instance ellesmere-port english-town
  ((has-name "Ellesmere Port")
   (has-county cheshire)))

(def-instance frodsham english-town
  ((has-name "Frodsham")
   (has-county cheshire)))

(def-instance hyde english-town
  ((has-name "Hyde")
   (has-county cheshire)))

(def-instance knutsford english-town
  ((has-name "Knutsford")
   (has-county cheshire)))

(def-instance lymm english-town
  ((has-name "Lymm")
   (has-county cheshire)))

(def-instance macclesfield english-town
  ((has-name "Macclesfield")
   (has-county cheshire)))

(def-instance malpas english-town
  ((has-name "Malpas")
   (has-county cheshire)))

(def-instance middlewich english-town
  ((has-name "Middlewich")
   (has-county cheshire)))

(def-instance nantwich english-town
  ((has-name "Nantwich")
   (has-county cheshire)))

(def-instance neston english-town
  ((has-name "Neston")
   (has-county cheshire)))

(def-instance northwich english-town
  ((has-name "Northwich")
   (has-county cheshire)))

(def-instance prenton english-town
  ((has-name "Prenton")
   (has-county cheshire)))

(def-instance runcorn english-town
  ((has-name "Runcorn")
   (has-county cheshire)))

(def-instance sale english-town
  ((has-name "Sale")
   (has-county cheshire)))

(def-instance sandbach english-town
  ((has-name "Sandbach")
   (has-county cheshire)))

(def-instance stalybridge english-town
  ((has-name "Stalybridge")
   (has-county cheshire)))

(def-instance stockport english-town
  ((has-name "Stockport")
   (has-county cheshire)))

(def-instance tarporley english-town
  ((has-name "Tarporley")
   (has-county cheshire)))

(def-instance wallasey english-town
  ((has-name "Wallasey")
   (has-county cheshire)))

(def-instance wilmslow english-town
  ((has-name "Wilmslow")
   (has-county cheshire)))

(def-instance winsford english-town
  ((has-name "Winsford")
   (has-county cheshire)))

(def-instance wirral english-town
  ((has-name "Wirral")
   (has-county cheshire)))

(def-instance alloa scottish-town
  ((has-name "Alloa")
   (has-county clackmannanshire)))

(def-instance alva scottish-town
  ((has-name "Alva")
   (has-county clackmannanshire)))

(def-instance clackmannan scottish-town
  ((has-name "Clackmannan")
   (has-county clackmannanshire)))

(def-instance dollar scottish-town
  ((has-name "Dollar")
   (has-county clackmannanshire)))

(def-instance menstrie scottish-town
  ((has-name "Menstrie")
   (has-county clackmannanshire)))

(def-instance tillicoultry scottish-town
  ((has-name "Tillicoultry")
   (has-county clackmannanshire)))

(def-instance bodmin english-town
  ((has-name "Bodmin")
   (has-county cornwall)))

(def-instance boscastle english-town
  ((has-name "Boscastle")
   (has-county cornwall)))

(def-instance bude english-town
  ((has-name "Bude")
   (has-county cornwall)))

(def-instance callington english-town
  ((has-name "Callington")
   (has-county cornwall)))

(def-instance calstock english-town
  ((has-name "Calstock")
   (has-county cornwall)))

(def-instance camborne english-town
  ((has-name "Camborne")
   (has-county cornwall)))

(def-instance camelford english-town
  ((has-name "Camelford")
   (has-county cornwall)))

(def-instance delabole english-town
  ((has-name "Delabole")
   (has-county cornwall)))

(def-instance falmouth english-town
  ((has-name "Falmouth")
   (has-county cornwall)))

(def-instance fowey english-town
  ((has-name "Fowey")
   (has-county cornwall)))

(def-instance gunnislake english-town
  ((has-name "Gunnislake")
   (has-county cornwall)))

(def-instance hayle english-town
  ((has-name "Hayle")
   (has-county cornwall)))

(def-instance helston english-town
  ((has-name "Helston")
   (has-county cornwall)))

(def-instance launceston english-town
  ((has-name "Launceston")
   (has-county cornwall)))

(def-instance liskeard english-town
  ((has-name "Liskeard")
   (has-county cornwall)))

(def-instance looe english-town
  ((has-name "Looe")
   (has-county cornwall)))

(def-instance lostwithiel english-town
  ((has-name "Lostwithiel")
   (has-county cornwall)))

(def-instance marazion english-town
  ((has-name "Marazion")
   (has-county cornwall)))

(def-instance newquay english-town
  ((has-name "Newquay")
   (has-county cornwall)))

(def-instance padstow english-town
  ((has-name "Padstow")
   (has-county cornwall)))

(def-instance par english-town
  ((has-name "Par")
   (has-county cornwall)))

(def-instance penryn english-town
  ((has-name "Penryn")
   (has-county cornwall)))

(def-instance penzance english-town
  ((has-name "Penzance")
   (has-county cornwall)))

(def-instance perranporth english-town
  ((has-name "Perranporth")
   (has-county cornwall)))

(def-instance port-isaac english-town
  ((has-name "Port Isaac")
   (has-county cornwall)))

(def-instance redruth english-town
  ((has-name "Redruth")
   (has-county cornwall)))

(def-instance saltash english-town
  ((has-name "Saltash")
   (has-county cornwall)))

(def-instance st-agnes-tr5 english-town
  ((has-name "St. Agnes Tr5")
   (has-county cornwall)))

(def-instance st-austell english-town
  ((has-name "St. Austell")
   (has-county cornwall)))

(def-instance st-columb english-town
  ((has-name "St. Columb")
   (has-county cornwall)))

(def-instance st-ives english-town
  ((has-name "St. Ives")
   (has-county cornwall)))

(def-instance tintagel english-town
  ((has-name "Tintagel")
   (has-county cornwall)))

(def-instance torpoint english-town
  ((has-name "Torpoint")
   (has-county cornwall)))

(def-instance truro english-town
  ((has-name "Truro")
   (has-county cornwall)))

(def-instance wadebridge english-town
  ((has-name "Wadebridge")
   (has-county cornwall)))

(def-instance antrim irish-town
  ((has-name "Antrim")
   (has-county county-antrim)))

(def-instance ballycastle irish-town
  ((has-name "Ballycastle")
   (has-county county-antrim)))

(def-instance ballyclare irish-town
  ((has-name "Ballyclare")
   (has-county county-antrim)))

(def-instance ballymena irish-town
  ((has-name "Ballymena")
   (has-county county-antrim)))

(def-instance ballymoney irish-town
  ((has-name "Ballymoney")
   (has-county county-antrim)))

(def-instance belfast irish-town
  ((has-name "Belfast")
   (has-county county-antrim)))

(def-instance bushmills irish-town
  ((has-name "Bushmills")
   (has-county county-antrim)))

(def-instance carrickfergus irish-town
  ((has-name "Carrickfergus")
   (has-county county-antrim)))

(def-instance crumlin irish-town
  ((has-name "Crumlin")
   (has-county county-antrim)))

(def-instance larne irish-town
  ((has-name "Larne")
   (has-county county-antrim)))

(def-instance lisburn irish-town
  ((has-name "Lisburn")
   (has-county county-antrim)))

(def-instance newtownabbey irish-town
  ((has-name "Newtownabbey")
   (has-county county-antrim)))

(def-instance portrush irish-town
  ((has-name "Portrush")
   (has-county county-antrim)))

(def-instance armagh irish-town
  ((has-name "Armagh")
   (has-county county-armagh)))

(def-instance craigavon irish-town
  ((has-name "Craigavon")
   (has-county county-armagh)))

(def-instance ballynahinch irish-town
  ((has-name "Ballynahinch")
   (has-county county-down)))

(def-instance banbridge irish-town
  ((has-name "Banbridge")
   (has-county county-down)))

(def-instance bangor-bt irish-town
  ((has-name "Bangor Bt")
   (has-county county-down)))

(def-instance castlewellan irish-town
  ((has-name "Castlewellan")
   (has-county county-down)))

(def-instance donaghadee irish-town
  ((has-name "Donaghadee")
   (has-county county-down)))

(def-instance downpatrick irish-town
  ((has-name "Downpatrick")
   (has-county county-down)))

(def-instance dromore irish-town
  ((has-name "Dromore")
   (has-county county-down)))

(def-instance hillsborough irish-town
  ((has-name "Hillsborough")
   (has-county county-down)))

(def-instance holywood irish-town
  ((has-name "Holywood")
   (has-county county-down)))

(def-instance newcastle-bt irish-town
  ((has-name "Newcastle Bt")
   (has-county county-down)))

(def-instance newry irish-town
  ((has-name "Newry")
   (has-county county-down)))

(def-instance newtownards irish-town
  ((has-name "Newtownards")
   (has-county county-down)))

(def-instance barnard-castle english-town
  ((has-name "Barnard Castle")
   (has-county county-durham)))

(def-instance billingham english-town
  ((has-name "Billingham")
   (has-county county-durham)))

(def-instance bishop-auckland english-town
  ((has-name "Bishop Auckland")
   (has-county county-durham)))

(def-instance blaydon-on-tyne english-town
  ((has-name "Blaydon-On-Tyne")
   (has-county county-durham)))

(def-instance boldon-colliery english-town
  ((has-name "Boldon Colliery")
   (has-county county-durham)))

(def-instance chester-le-street english-town
  ((has-name "Chester Le Street")
   (has-county county-durham)))

(def-instance consett english-town
  ((has-name "Consett")
   (has-county county-durham)))

(def-instance crook english-town
  ((has-name "Crook")
   (has-county county-durham)))

(def-instance darlington english-town
  ((has-name "Darlington")
   (has-county county-durham)))

(def-instance durham english-town
  ((has-name "Durham")
   (has-county county-durham)))

(def-instance east-boldon english-town
  ((has-name "East Boldon")
   (has-county county-durham)))

(def-instance ferryhill english-town
  ((has-name "Ferryhill")
   (has-county county-durham)))

(def-instance gateshead english-town
  ((has-name "Gateshead")
   (has-county county-durham)))

(def-instance hartlepool english-town
  ((has-name "Hartlepool")
   (has-county county-durham)))

(def-instance hebburn english-town
  ((has-name "Hebburn")
   (has-county county-durham)))

(def-instance houghton-le-spring english-town
  ((has-name "Houghton Le Spring")
   (has-county county-durham)))

(def-instance jarrow english-town
  ((has-name "Jarrow")
   (has-county county-durham)))

(def-instance newton-aycliffe english-town
  ((has-name "Newton Aycliffe")
   (has-county county-durham)))

(def-instance peterlee english-town
  ((has-name "Peterlee")
   (has-county county-durham)))

(def-instance rowlands-gill english-town
  ((has-name "Rowlands Gill")
   (has-county county-durham)))

(def-instance ryton english-town
  ((has-name "Ryton")
   (has-county county-durham)))

(def-instance seaham english-town
  ((has-name "Seaham")
   (has-county county-durham)))

(def-instance shildon english-town
  ((has-name "Shildon")
   (has-county county-durham)))

(def-instance south-shields english-town
  ((has-name "South Shields")
   (has-county county-durham)))

(def-instance spennymoor english-town
  ((has-name "Spennymoor")
   (has-county county-durham)))

(def-instance stanley english-town
  ((has-name "Stanley")
   (has-county county-durham)))

(def-instance stockton-on-tees english-town
  ((has-name "Stockton-On-Tees")
   (has-county county-durham)))

(def-instance sunderland english-town
  ((has-name "Sunderland")
   (has-county county-durham)))

(def-instance trimdon-station english-town
  ((has-name "Trimdon Station")
   (has-county county-durham)))

(def-instance washington english-town
  ((has-name "Washington")
   (has-county county-durham)))

(def-instance wingate english-town
  ((has-name "Wingate")
   (has-county county-durham)))

(def-instance enniskillen irish-town
  ((has-name "Enniskillen")
   (has-county county-fermanagh)))

(def-instance coleraine irish-town
  ((has-name "Coleraine")
   (has-county county-londonderry)))

(def-instance limavady irish-town
  ((has-name "Limavady")
   (has-county county-londonderry)))

(def-instance londonderry irish-town
  ((has-name "Londonderry")
   (has-county county-londonderry)))

(def-instance maghera irish-town
  ((has-name "Maghera")
   (has-county county-londonderry)))

(def-instance magherafelt irish-town
  ((has-name "Magherafelt")
   (has-county county-londonderry)))

(def-instance portstewart irish-town
  ((has-name "Portstewart")
   (has-county county-londonderry)))

(def-instance augher irish-town
  ((has-name "Augher")
   (has-county county-tyrone)))

(def-instance aughnacloy irish-town
  ((has-name "Aughnacloy")
   (has-county county-tyrone)))

(def-instance caledon irish-town
  ((has-name "Caledon")
   (has-county county-tyrone)))

(def-instance castlederg irish-town
  ((has-name "Castlederg")
   (has-county county-tyrone)))

(def-instance clogher irish-town
  ((has-name "Clogher")
   (has-county county-tyrone)))

(def-instance cookstown irish-town
  ((has-name "Cookstown")
   (has-county county-tyrone)))

(def-instance dungannon irish-town
  ((has-name "Dungannon")
   (has-county county-tyrone)))

(def-instance fivemiletown irish-town
  ((has-name "Fivemiletown")
   (has-county county-tyrone)))

(def-instance omagh irish-town
  ((has-name "Omagh")
   (has-county county-tyrone)))

(def-instance strabane irish-town
  ((has-name "Strabane")
   (has-county county-tyrone)))

(def-instance alston english-town
  ((has-name "Alston")
   (has-county cumberland)))

(def-instance beckermet english-town
  ((has-name "Beckermet")
   (has-county cumberland)))

(def-instance brampton english-town
  ((has-name "Brampton")
   (has-county cumberland)))

(def-instance carlisle english-town
  ((has-name "Carlisle")
   (has-county cumberland)))

(def-instance cleator english-town
  ((has-name "Cleator")
   (has-county cumberland)))

(def-instance cleator-moor english-town
  ((has-name "Cleator Moor")
   (has-county cumberland)))

(def-instance cockermouth english-town
  ((has-name "Cockermouth")
   (has-county cumberland)))

(def-instance egremont english-town
  ((has-name "Egremont")
   (has-county cumberland)))

(def-instance frizington english-town
  ((has-name "Frizington")
   (has-county cumberland)))

(def-instance holmrook english-town
  ((has-name "Holmrook")
   (has-county cumberland)))

(def-instance keswick english-town
  ((has-name "Keswick")
   (has-county cumberland)))

(def-instance maryport english-town
  ((has-name "Maryport")
   (has-county cumberland)))

(def-instance millom english-town
  ((has-name "Millom")
   (has-county cumberland)))

(def-instance moor-row english-town
  ((has-name "Moor Row")
   (has-county cumberland)))

(def-instance penrith english-town
  ((has-name "Penrith")
   (has-county cumberland)))

(def-instance ravenglass english-town
  ((has-name "Ravenglass")
   (has-county cumberland)))

(def-instance seascale english-town
  ((has-name "Seascale")
   (has-county cumberland)))

(def-instance st-bees english-town
  ((has-name "St. Bees")
   (has-county cumberland)))

(def-instance whitehaven english-town
  ((has-name "Whitehaven")
   (has-county cumberland)))

(def-instance wigton english-town
  ((has-name "Wigton")
   (has-county cumberland)))

(def-instance workington english-town
  ((has-name "Workington")
   (has-county cumberland)))

(def-instance abergele welsh-town
  ((has-name "Abergele")
   (has-county denbighshire)))

(def-instance colwyn-bay welsh-town
  ((has-name "Colwyn Bay")
   (has-county denbighshire)))

(def-instance denbigh welsh-town
  ((has-name "Denbigh")
   (has-county denbighshire)))

(def-instance llangollen welsh-town
  ((has-name "Llangollen")
   (has-county denbighshire)))

(def-instance llanrwst welsh-town
  ((has-name "Llanrwst")
   (has-county denbighshire)))

(def-instance ruthin welsh-town
  ((has-name "Ruthin")
   (has-county denbighshire)))

(def-instance wrexham welsh-town
  ((has-name "Wrexham")
   (has-county denbighshire)))

(def-instance alfreton english-town
  ((has-name "Alfreton")
   (has-county derbyshire)))

(def-instance ashbourne english-town
  ((has-name "Ashbourne")
   (has-county derbyshire)))

(def-instance bakewell english-town
  ((has-name "Bakewell")
   (has-county derbyshire)))

(def-instance belper english-town
  ((has-name "Belper")
   (has-county derbyshire)))

(def-instance buxton english-town
  ((has-name "Buxton")
   (has-county derbyshire)))

(def-instance chesterfield english-town
  ((has-name "Chesterfield")
   (has-county derbyshire)))

(def-instance derby english-town
  ((has-name "Derby")
   (has-county derbyshire)))

(def-instance dronfield english-town
  ((has-name "Dronfield")
   (has-county derbyshire)))

(def-instance glossop english-town
  ((has-name "Glossop")
   (has-county derbyshire)))

(def-instance heanor english-town
  ((has-name "Heanor")
   (has-county derbyshire)))

(def-instance high-peak english-town
  ((has-name "High Peak")
   (has-county derbyshire)))

(def-instance hope-valley english-town
  ((has-name "Hope Valley")
   (has-county derbyshire)))

(def-instance ilkeston english-town
  ((has-name "Ilkeston")
   (has-county derbyshire)))

(def-instance matlock english-town
  ((has-name "Matlock")
   (has-county derbyshire)))

(def-instance ripley english-town
  ((has-name "Ripley")
   (has-county derbyshire)))

(def-instance swadlincote english-town
  ((has-name "Swadlincote")
   (has-county derbyshire)))

(def-instance axminster english-town
  ((has-name "Axminster")
   (has-county devon)))

(def-instance barnstaple english-town
  ((has-name "Barnstaple")
   (has-county devon)))

(def-instance beaworthy english-town
  ((has-name "Beaworthy")
   (has-county devon)))

(def-instance bideford english-town
  ((has-name "Bideford")
   (has-county devon)))

(def-instance braunton english-town
  ((has-name "Braunton")
   (has-county devon)))

(def-instance brixham english-town
  ((has-name "Brixham")
   (has-county devon)))

(def-instance buckfastleigh english-town
  ((has-name "Buckfastleigh")
   (has-county devon)))

(def-instance budleigh-salterton english-town
  ((has-name "Budleigh Salterton")
   (has-county devon)))

(def-instance chulmleigh english-town
  ((has-name "Chulmleigh")
   (has-county devon)))

(def-instance colyton english-town
  ((has-name "Colyton")
   (has-county devon)))

(def-instance crediton english-town
  ((has-name "Crediton")
   (has-county devon)))

(def-instance cullompton english-town
  ((has-name "Cullompton")
   (has-county devon)))

(def-instance dartmouth english-town
  ((has-name "Dartmouth")
   (has-county devon)))

(def-instance dawlish english-town
  ((has-name "Dawlish")
   (has-county devon)))

(def-instance exeter english-town
  ((has-name "Exeter")
   (has-county devon)))

(def-instance exmouth english-town
  ((has-name "Exmouth")
   (has-county devon)))

(def-instance holsworthy english-town
  ((has-name "Holsworthy")
   (has-county devon)))

(def-instance honiton english-town
  ((has-name "Honiton")
   (has-county devon)))

(def-instance ilfracombe english-town
  ((has-name "Ilfracombe")
   (has-county devon)))

(def-instance ivybridge english-town
  ((has-name "Ivybridge")
   (has-county devon)))

(def-instance kingsbridge english-town
  ((has-name "Kingsbridge")
   (has-county devon)))

(def-instance lifton english-town
  ((has-name "Lifton")
   (has-county devon)))

(def-instance lynmouth english-town
  ((has-name "Lynmouth")
   (has-county devon)))

(def-instance lynton english-town
  ((has-name "Lynton")
   (has-county devon)))

(def-instance newton-abbot english-town
  ((has-name "Newton Abbot")
   (has-county devon)))

(def-instance north-tawton english-town
  ((has-name "North Tawton")
   (has-county devon)))

(def-instance okehampton english-town
  ((has-name "Okehampton")
   (has-county devon)))

(def-instance ottery-st-mary english-town
  ((has-name "Ottery St. Mary")
   (has-county devon)))

(def-instance paignton english-town
  ((has-name "Paignton")
   (has-county devon)))

(def-instance plymouth english-town
  ((has-name "Plymouth")
   (has-county devon)))

(def-instance salcombe english-town
  ((has-name "Salcombe")
   (has-county devon)))

(def-instance seaton english-town
  ((has-name "Seaton")
   (has-county devon)))

(def-instance sidmouth english-town
  ((has-name "Sidmouth")
   (has-county devon)))

(def-instance south-brent english-town
  ((has-name "South Brent")
   (has-county devon)))

(def-instance south-molton english-town
  ((has-name "South Molton")
   (has-county devon)))

(def-instance tavistock english-town
  ((has-name "Tavistock")
   (has-county devon)))

(def-instance teignmouth english-town
  ((has-name "Teignmouth")
   (has-county devon)))

(def-instance tiverton english-town
  ((has-name "Tiverton")
   (has-county devon)))

(def-instance torquay english-town
  ((has-name "Torquay")
   (has-county devon)))

(def-instance torrington english-town
  ((has-name "Torrington")
   (has-county devon)))

(def-instance totnes english-town
  ((has-name "Totnes")
   (has-county devon)))

(def-instance umberleigh english-town
  ((has-name "Umberleigh")
   (has-county devon)))

(def-instance winkleigh english-town
  ((has-name "Winkleigh")
   (has-county devon)))

(def-instance woolacombe english-town
  ((has-name "Woolacombe")
   (has-county devon)))

(def-instance yelverton english-town
  ((has-name "Yelverton")
   (has-county devon)))

(def-instance beaminster english-town
  ((has-name "Beaminster")
   (has-county dorset)))

(def-instance blandford-forum english-town
  ((has-name "Blandford Forum")
   (has-county dorset)))

(def-instance bridport english-town
  ((has-name "Bridport")
   (has-county dorset)))

(def-instance broadstone english-town
  ((has-name "Broadstone")
   (has-county dorset)))

(def-instance dorchester english-town
  ((has-name "Dorchester")
   (has-county dorset)))

(def-instance ferndown english-town
  ((has-name "Ferndown")
   (has-county dorset)))

(def-instance gillingham-sp english-town
  ((has-name "Gillingham Sp")
   (has-county dorset)))

(def-instance lyme-regis english-town
  ((has-name "Lyme Regis")
   (has-county dorset)))

(def-instance poole english-town
  ((has-name "Poole")
   (has-county dorset)))

(def-instance portland english-town
  ((has-name "Portland")
   (has-county dorset)))

(def-instance shaftesbury english-town
  ((has-name "Shaftesbury")
   (has-county dorset)))

(def-instance sherborne english-town
  ((has-name "Sherborne")
   (has-county dorset)))

(def-instance sturminster-newton english-town
  ((has-name "Sturminster Newton")
   (has-county dorset)))

(def-instance swanage english-town
  ((has-name "Swanage")
   (has-county dorset)))

(def-instance verwood english-town
  ((has-name "Verwood")
   (has-county dorset)))

(def-instance wareham english-town
  ((has-name "Wareham")
   (has-county dorset)))

(def-instance weymouth english-town
  ((has-name "Weymouth")
   (has-county dorset)))

(def-instance wimborne english-town
  ((has-name "Wimborne")
   (has-county dorset)))

(def-instance annan scottish-town
  ((has-name "Annan")
   (has-county dumfriesshire)))

(def-instance canonbie scottish-town
  ((has-name "Canonbie")
   (has-county dumfriesshire)))

(def-instance dumfries scottish-town
  ((has-name "Dumfries")
   (has-county dumfriesshire)))

(def-instance gretna scottish-town
  ((has-name "Gretna")
   (has-county dumfriesshire)))

(def-instance langholm scottish-town
  ((has-name "Langholm")
   (has-county dumfriesshire)))

(def-instance lockerbie scottish-town
  ((has-name "Lockerbie")
   (has-county dumfriesshire)))

(def-instance moffat scottish-town
  ((has-name "Moffat")
   (has-county dumfriesshire)))

(def-instance sanquhar scottish-town
  ((has-name "Sanquhar")
   (has-county dumfriesshire)))

(def-instance thornhill scottish-town
  ((has-name "Thornhill")
   (has-county dumfriesshire)))

(def-instance alexandria scottish-town
  ((has-name "Alexandria")
   (has-county dunbartonshire)))

(def-instance arrochar scottish-town
  ((has-name "Arrochar")
   (has-county dunbartonshire)))

(def-instance clydebank scottish-town
  ((has-name "Clydebank")
   (has-county dunbartonshire)))

(def-instance dumbarton scottish-town
  ((has-name "Dumbarton")
   (has-county dunbartonshire)))

(def-instance helensburgh scottish-town
  ((has-name "Helensburgh")
   (has-county dunbartonshire)))

(def-instance dunbar scottish-town
  ((has-name "Dunbar")
   (has-county east-lothian)))

(def-instance east-linton scottish-town
  ((has-name "East Linton")
   (has-county east-lothian)))

(def-instance gullane scottish-town
  ((has-name "Gullane")
   (has-county east-lothian)))

(def-instance haddington scottish-town
  ((has-name "Haddington")
   (has-county east-lothian)))

(def-instance humbie scottish-town
  ((has-name "Humbie")
   (has-county east-lothian)))

(def-instance longniddry scottish-town
  ((has-name "Longniddry")
   (has-county east-lothian)))

(def-instance north-berwick scottish-town
  ((has-name "North Berwick")
   (has-county east-lothian)))

(def-instance prestonpans scottish-town
  ((has-name "Prestonpans")
   (has-county east-lothian)))

(def-instance tranent scottish-town
  ((has-name "Tranent")
   (has-county east-lothian)))

(def-instance barking english-town
  ((has-name "Barking")
   (has-county essex)))

(def-instance basildon english-town
  ((has-name "Basildon")
   (has-county essex)))

(def-instance benfleet english-town
  ((has-name "Benfleet")
   (has-county essex)))

(def-instance billericay english-town
  ((has-name "Billericay")
   (has-county essex)))

(def-instance braintree english-town
  ((has-name "Braintree")
   (has-county essex)))

(def-instance brentwood english-town
  ((has-name "Brentwood")
   (has-county essex)))

(def-instance buckhurst-hill english-town
  ((has-name "Buckhurst Hill")
   (has-county essex)))

(def-instance burnham-on-crouch english-town
  ((has-name "Burnham-On-Crouch")
   (has-county essex)))

(def-instance canvey-island english-town
  ((has-name "Canvey Island")
   (has-county essex)))

(def-instance chelmsford english-town
  ((has-name "Chelmsford")
   (has-county essex)))

(def-instance chigwell english-town
  ((has-name "Chigwell")
   (has-county essex)))

(def-instance clacton-on-sea english-town
  ((has-name "Clacton-On-Sea")
   (has-county essex)))

(def-instance colchester english-town
  ((has-name "Colchester")
   (has-county essex)))

(def-instance dagenham english-town
  ((has-name "Dagenham")
   (has-county essex)))

(def-instance dunmow english-town
  ((has-name "Dunmow")
   (has-county essex)))

(def-instance epping english-town
  ((has-name "Epping")
   (has-county essex)))

(def-instance frinton-on-sea english-town
  ((has-name "Frinton-On-Sea")
   (has-county essex)))

(def-instance grays english-town
  ((has-name "Grays")
   (has-county essex)))

(def-instance halstead english-town
  ((has-name "Halstead")
   (has-county essex)))

(def-instance harlow english-town
  ((has-name "Harlow")
   (has-county essex)))

(def-instance harwich english-town
  ((has-name "Harwich")
   (has-county essex)))

(def-instance hockley english-town
  ((has-name "Hockley")
   (has-county essex)))

(def-instance hornchurch english-town
  ((has-name "Hornchurch")
   (has-county essex)))

(def-instance ilford english-town
  ((has-name "Ilford")
   (has-county essex)))

(def-instance ingatestone english-town
  ((has-name "Ingatestone")
   (has-county essex)))

(def-instance leigh-on-sea english-town
  ((has-name "Leigh-On-Sea")
   (has-county essex)))

(def-instance loughton english-town
  ((has-name "Loughton")
   (has-county essex)))

(def-instance maldon english-town
  ((has-name "Maldon")
   (has-county essex)))

(def-instance manningtree english-town
  ((has-name "Manningtree")
   (has-county essex)))

(def-instance ongar english-town
  ((has-name "Ongar")
   (has-county essex)))

(def-instance purfleet english-town
  ((has-name "Purfleet")
   (has-county essex)))

(def-instance rainham english-town
  ((has-name "Rainham")
   (has-county essex)))

(def-instance rayleigh english-town
  ((has-name "Rayleigh")
   (has-county essex)))

(def-instance rochford english-town
  ((has-name "Rochford")
   (has-county essex)))

(def-instance romford english-town
  ((has-name "Romford")
   (has-county essex)))

(def-instance saffron-walden english-town
  ((has-name "Saffron Walden")
   (has-county essex)))

(def-instance south-ockendon english-town
  ((has-name "South Ockendon")
   (has-county essex)))

(def-instance southend-on-sea english-town
  ((has-name "Southend-On-Sea")
   (has-county essex)))

(def-instance southminster english-town
  ((has-name "Southminster")
   (has-county essex)))

(def-instance stanford-le-hope english-town
  ((has-name "Stanford-Le-Hope")
   (has-county essex)))

(def-instance stansted english-town
  ((has-name "Stansted")
   (has-county essex)))

(def-instance tilbury english-town
  ((has-name "Tilbury")
   (has-county essex)))

(def-instance upminster english-town
  ((has-name "Upminster")
   (has-county essex)))

(def-instance waltham-abbey english-town
  ((has-name "Waltham Abbey")
   (has-county essex)))

(def-instance walton-on-the-naze english-town
  ((has-name "Walton On The Naze")
   (has-county essex)))

(def-instance westcliff-on-sea english-town
  ((has-name "Westcliff-On-Sea")
   (has-county essex)))

(def-instance wickford english-town
  ((has-name "Wickford")
   (has-county essex)))

(def-instance witham english-town
  ((has-name "Witham")
   (has-county essex)))

(def-instance woodford-green english-town
  ((has-name "Woodford Green")
   (has-county essex)))

(def-instance anstruther scottish-town
  ((has-name "Anstruther")
   (has-county fife)))

(def-instance burntisland scottish-town
  ((has-name "Burntisland")
   (has-county fife)))

(def-instance cowdenbeath scottish-town
  ((has-name "Cowdenbeath")
   (has-county fife)))

(def-instance cupar scottish-town
  ((has-name "Cupar")
   (has-county fife)))

(def-instance dunfermline scottish-town
  ((has-name "Dunfermline")
   (has-county fife)))

(def-instance glenrothes scottish-town
  ((has-name "Glenrothes")
   (has-county fife)))

(def-instance inverkeithing scottish-town
  ((has-name "Inverkeithing")
   (has-county fife)))

(def-instance kelty scottish-town
  ((has-name "Kelty")
   (has-county fife)))

(def-instance kirkcaldy scottish-town
  ((has-name "Kirkcaldy")
   (has-county fife)))

(def-instance leven scottish-town
  ((has-name "Leven")
   (has-county fife)))

(def-instance lochgelly scottish-town
  ((has-name "Lochgelly")
   (has-county fife)))

(def-instance newport-on-tay scottish-town
  ((has-name "Newport-On-Tay")
   (has-county fife)))

(def-instance st-andrews scottish-town
  ((has-name "St. Andrews")
   (has-county fife)))

(def-instance tayport scottish-town
  ((has-name "Tayport")
   (has-county fife)))

(def-instance bagillt welsh-town
  ((has-name "Bagillt")
   (has-county flintshire)))

(def-instance buckley welsh-town
  ((has-name "Buckley")
   (has-county flintshire)))

(def-instance deeside welsh-town
  ((has-name "Deeside")
   (has-county flintshire)))

(def-instance flint welsh-town
  ((has-name "Flint")
   (has-county flintshire)))

(def-instance holywell welsh-town
  ((has-name "Holywell")
   (has-county flintshire)))

(def-instance mold welsh-town
  ((has-name "Mold")
   (has-county flintshire)))

(def-instance prestatyn welsh-town
  ((has-name "Prestatyn")
   (has-county flintshire)))

(def-instance rhyl welsh-town
  ((has-name "Rhyl")
   (has-county flintshire)))

(def-instance st-asaph welsh-town
  ((has-name "St. Asaph")
   (has-county flintshire)))

(def-instance aberdare welsh-town
  ((has-name "Aberdare")
   (has-county glamorgan)))

(def-instance bargoed welsh-town
  ((has-name "Bargoed")
   (has-county glamorgan)))

(def-instance barry welsh-town
  ((has-name "Barry")
   (has-county glamorgan)))

(def-instance bridgend-cf welsh-town
  ((has-name "Bridgend Cf")
   (has-county glamorgan)))

(def-instance caerphilly welsh-town
  ((has-name "Caerphilly")
   (has-county glamorgan)))

(def-instance cardiff welsh-town
  ((has-name "Cardiff")
   (has-county glamorgan)))

(def-instance cowbridge welsh-town
  ((has-name "Cowbridge")
   (has-county glamorgan)))

(def-instance dinas-powys welsh-town
  ((has-name "Dinas Powys")
   (has-county glamorgan)))

(def-instance ferndale welsh-town
  ((has-name "Ferndale")
   (has-county glamorgan)))

(def-instance hengoed welsh-town
  ((has-name "Hengoed")
   (has-county glamorgan)))

(def-instance llantwit-major welsh-town
  ((has-name "Llantwit Major")
   (has-county glamorgan)))

(def-instance maesteg welsh-town
  ((has-name "Maesteg")
   (has-county glamorgan)))

(def-instance merthyr-tydfil welsh-town
  ((has-name "Merthyr Tydfil")
   (has-county glamorgan)))

(def-instance mountain-ash welsh-town
  ((has-name "Mountain Ash")
   (has-county glamorgan)))

(def-instance neath welsh-town
  ((has-name "Neath")
   (has-county glamorgan)))

(def-instance penarth welsh-town
  ((has-name "Penarth")
   (has-county glamorgan)))

(def-instance pentre welsh-town
  ((has-name "Pentre")
   (has-county glamorgan)))

(def-instance pontyclun welsh-town
  ((has-name "Pontyclun")
   (has-county glamorgan)))

(def-instance pontypridd welsh-town
  ((has-name "Pontypridd")
   (has-county glamorgan)))

(def-instance port-talbot welsh-town
  ((has-name "Port Talbot")
   (has-county glamorgan)))

(def-instance porth welsh-town
  ((has-name "Porth")
   (has-county glamorgan)))

(def-instance porthcawl welsh-town
  ((has-name "Porthcawl")
   (has-county glamorgan)))

(def-instance swansea welsh-town
  ((has-name "Swansea")
   (has-county glamorgan)))

(def-instance tonypandy welsh-town
  ((has-name "Tonypandy")
   (has-county glamorgan)))

(def-instance treharris welsh-town
  ((has-name "Treharris")
   (has-county glamorgan)))

(def-instance treorchy welsh-town
  ((has-name "Treorchy")
   (has-county glamorgan)))

(def-instance badminton english-town
  ((has-name "Badminton")
   (has-county gloucestershire)))

(def-instance berkeley english-town
  ((has-name "Berkeley")
   (has-county gloucestershire)))

(def-instance blakeney english-town
  ((has-name "Blakeney")
   (has-county gloucestershire)))

(def-instance bristol english-town
  ((has-name "Bristol")
   (has-county gloucestershire)))

(def-instance cheltenham english-town
  ((has-name "Cheltenham")
   (has-county gloucestershire)))

(def-instance chipping-campden english-town
  ((has-name "Chipping Campden")
   (has-county gloucestershire)))

(def-instance cinderford english-town
  ((has-name "Cinderford")
   (has-county gloucestershire)))

(def-instance cirencester english-town
  ((has-name "Cirencester")
   (has-county gloucestershire)))

(def-instance coleford english-town
  ((has-name "Coleford")
   (has-county gloucestershire)))

(def-instance drybrook english-town
  ((has-name "Drybrook")
   (has-county gloucestershire)))

(def-instance dursley english-town
  ((has-name "Dursley")
   (has-county gloucestershire)))

(def-instance dymock english-town
  ((has-name "Dymock")
   (has-county gloucestershire)))

(def-instance fairford english-town
  ((has-name "Fairford")
   (has-county gloucestershire)))

(def-instance gloucester english-town
  ((has-name "Gloucester")
   (has-county gloucestershire)))

(def-instance lechlade english-town
  ((has-name "Lechlade")
   (has-county gloucestershire)))

(def-instance longhope english-town
  ((has-name "Longhope")
   (has-county gloucestershire)))

(def-instance lydbrook english-town
  ((has-name "Lydbrook")
   (has-county gloucestershire)))

(def-instance lydney english-town
  ((has-name "Lydney")
   (has-county gloucestershire)))

(def-instance mitcheldean english-town
  ((has-name "Mitcheldean")
   (has-county gloucestershire)))

(def-instance moreton-in-marsh english-town
  ((has-name "Moreton-In-Marsh")
   (has-county gloucestershire)))

(def-instance newent english-town
  ((has-name "Newent")
   (has-county gloucestershire)))

(def-instance newnham english-town
  ((has-name "Newnham")
   (has-county gloucestershire)))

(def-instance ruardean english-town
  ((has-name "Ruardean")
   (has-county gloucestershire)))

(def-instance stonehouse english-town
  ((has-name "Stonehouse")
   (has-county gloucestershire)))

(def-instance stroud english-town
  ((has-name "Stroud")
   (has-county gloucestershire)))

(def-instance tetbury english-town
  ((has-name "Tetbury")
   (has-county gloucestershire)))

(def-instance tewkesbury english-town
  ((has-name "Tewkesbury")
   (has-county gloucestershire)))

(def-instance westbury-on-severn english-town
  ((has-name "Westbury-On-Severn")
   (has-county gloucestershire)))

(def-instance wotton-under-edge english-town
  ((has-name "Wotton-Under-Edge")
   (has-county gloucestershire)))

(def-instance aldershot english-town
  ((has-name "Aldershot")
   (has-county hampshire)))

(def-instance alresford english-town
  ((has-name "Alresford")
   (has-county hampshire)))

(def-instance alton english-town
  ((has-name "Alton")
   (has-county hampshire)))

(def-instance andover english-town
  ((has-name "Andover")
   (has-county hampshire)))

(def-instance basingstoke english-town
  ((has-name "Basingstoke")
   (has-county hampshire)))

(def-instance bordon english-town
  ((has-name "Bordon")
   (has-county hampshire)))

(def-instance bournemouth english-town
  ((has-name "Bournemouth")
   (has-county hampshire)))

(def-instance brockenhurst english-town
  ((has-name "Brockenhurst")
   (has-county hampshire)))

(def-instance christchurch english-town
  ((has-name "Christchurch")
   (has-county hampshire)))

(def-instance eastleigh english-town
  ((has-name "Eastleigh")
   (has-county hampshire)))

(def-instance emsworth english-town
  ((has-name "Emsworth")
   (has-county hampshire)))

(def-instance fareham english-town
  ((has-name "Fareham")
   (has-county hampshire)))

(def-instance farnborough english-town
  ((has-name "Farnborough")
   (has-county hampshire)))

(def-instance fleet english-town
  ((has-name "Fleet")
   (has-county hampshire)))

(def-instance fordingbridge english-town
  ((has-name "Fordingbridge")
   (has-county hampshire)))

(def-instance gosport english-town
  ((has-name "Gosport")
   (has-county hampshire)))

(def-instance havant english-town
  ((has-name "Havant")
   (has-county hampshire)))

(def-instance hayling-island english-town
  ((has-name "Hayling Island")
   (has-county hampshire)))

(def-instance hook english-town
  ((has-name "Hook")
   (has-county hampshire)))

(def-instance lee-on-the-solent english-town
  ((has-name "Lee-On-The-Solent")
   (has-county hampshire)))

(def-instance liphook english-town
  ((has-name "Liphook")
   (has-county hampshire)))

(def-instance liss english-town
  ((has-name "Liss")
   (has-county hampshire)))

(def-instance lymington english-town
  ((has-name "Lymington")
   (has-county hampshire)))

(def-instance lyndhurst english-town
  ((has-name "Lyndhurst")
   (has-county hampshire)))

(def-instance new-milton english-town
  ((has-name "New Milton")
   (has-county hampshire)))

(def-instance petersfield english-town
  ((has-name "Petersfield")
   (has-county hampshire)))

(def-instance portsmouth english-town
  ((has-name "Portsmouth")
   (has-county hampshire)))

(def-instance ringwood english-town
  ((has-name "Ringwood")
   (has-county hampshire)))

(def-instance romsey english-town
  ((has-name "Romsey")
   (has-county hampshire)))

(def-instance rowlands-castle english-town
  ((has-name "Rowland's Castle")
   (has-county hampshire)))

(def-instance southampton english-town
  ((has-name "Southampton")
   (has-county hampshire)))

(def-instance southsea english-town
  ((has-name "Southsea")
   (has-county hampshire)))

(def-instance stockbridge english-town
  ((has-name "Stockbridge")
   (has-county hampshire)))

(def-instance tadley english-town
  ((has-name "Tadley")
   (has-county hampshire)))

(def-instance tidworth english-town
  ((has-name "Tidworth")
   (has-county hampshire)))

(def-instance waterlooville english-town
  ((has-name "Waterlooville")
   (has-county hampshire)))

(def-instance whitchurch-rg english-town
  ((has-name "Whitchurch Rg")
   (has-county hampshire)))

(def-instance winchester english-town
  ((has-name "Winchester")
   (has-county hampshire)))

(def-instance yateley english-town
  ((has-name "Yateley")
   (has-county hampshire)))

(def-instance bromyard english-town
  ((has-name "Bromyard")
   (has-county herefordshire)))

(def-instance hereford english-town
  ((has-name "Hereford")
   (has-county herefordshire)))

(def-instance kington english-town
  ((has-name "Kington")
   (has-county herefordshire)))

(def-instance ledbury english-town
  ((has-name "Ledbury")
   (has-county herefordshire)))

(def-instance leominster english-town
  ((has-name "Leominster")
   (has-county herefordshire)))

(def-instance ross-on-wye english-town
  ((has-name "Ross-On-Wye")
   (has-county herefordshire)))

(def-instance abbots-langley english-town
  ((has-name "Abbots Langley")
   (has-county hertfordshire)))

(def-instance baldock english-town
  ((has-name "Baldock")
   (has-county hertfordshire)))

(def-instance barnet english-town
  ((has-name "Barnet")
   (has-county hertfordshire)))

(def-instance berkhamsted english-town
  ((has-name "Berkhamsted")
   (has-county hertfordshire)))

(def-instance bishops-stortford english-town
  ((has-name "Bishop's Stortford")
   (has-county hertfordshire)))

(def-instance borehamwood english-town
  ((has-name "Borehamwood")
   (has-county hertfordshire)))

(def-instance broxbourne english-town
  ((has-name "Broxbourne")
   (has-county hertfordshire)))

(def-instance buntingford english-town
  ((has-name "Buntingford")
   (has-county hertfordshire)))

(def-instance harpenden english-town
  ((has-name "Harpenden")
   (has-county hertfordshire)))

(def-instance hatfield english-town
  ((has-name "Hatfield")
   (has-county hertfordshire)))

(def-instance hemel-hempstead english-town
  ((has-name "Hemel Hempstead")
   (has-county hertfordshire)))

(def-instance hertford english-town
  ((has-name "Hertford")
   (has-county hertfordshire)))

(def-instance hitchin english-town
  ((has-name "Hitchin")
   (has-county hertfordshire)))

(def-instance hoddesdon english-town
  ((has-name "Hoddesdon")
   (has-county hertfordshire)))

(def-instance kings-langley english-town
  ((has-name "Kings Langley")
   (has-county hertfordshire)))

(def-instance knebworth english-town
  ((has-name "Knebworth")
   (has-county hertfordshire)))

(def-instance letchworth english-town
  ((has-name "Letchworth")
   (has-county hertfordshire)))

(def-instance much-hadham english-town
  ((has-name "Much Hadham")
   (has-county hertfordshire)))

(def-instance radlett english-town
  ((has-name "Radlett")
   (has-county hertfordshire)))

(def-instance rickmansworth english-town
  ((has-name "Rickmansworth")
   (has-county hertfordshire)))

(def-instance royston english-town
  ((has-name "Royston")
   (has-county hertfordshire)))

(def-instance sawbridgeworth english-town
  ((has-name "Sawbridgeworth")
   (has-county hertfordshire)))

(def-instance st-albans english-town
  ((has-name "St. Albans")
   (has-county hertfordshire)))

(def-instance stevenage english-town
  ((has-name "Stevenage")
   (has-county hertfordshire)))

(def-instance tring english-town
  ((has-name "Tring")
   (has-county hertfordshire)))

(def-instance waltham-cross english-town
  ((has-name "Waltham Cross")
   (has-county hertfordshire)))

(def-instance ware english-town
  ((has-name "Ware")
   (has-county hertfordshire)))

(def-instance watford english-town
  ((has-name "Watford")
   (has-county hertfordshire)))

(def-instance welwyn english-town
  ((has-name "Welwyn")
   (has-county hertfordshire)))

(def-instance welwyn-garden-city english-town
  ((has-name "Welwyn Garden City")
   (has-county hertfordshire)))

(def-instance huntingdon english-town
  ((has-name "Huntingdon")
   (has-county huntingdonshire)))

(def-instance arisaig scottish-town
  ((has-name "Arisaig")
   (has-county inverness-shire)))

(def-instance aviemore scottish-town
  ((has-name "Aviemore")
   (has-county inverness-shire)))

(def-instance beauly scottish-town
  ((has-name "Beauly")
   (has-county inverness-shire)))

(def-instance boat-of-garten scottish-town
  ((has-name "Boat Of Garten")
   (has-county inverness-shire)))

(def-instance carrbridge scottish-town
  ((has-name "Carrbridge")
   (has-county inverness-shire)))

(def-instance corrour scottish-town
  ((has-name "Corrour")
   (has-county inverness-shire)))

(def-instance dalwhinnie scottish-town
  ((has-name "Dalwhinnie")
   (has-county inverness-shire)))

(def-instance fort-augustus scottish-town
  ((has-name "Fort Augustus")
   (has-county inverness-shire)))

(def-instance fort-william scottish-town
  ((has-name "Fort William")
   (has-county inverness-shire)))

(def-instance glenfinnan scottish-town
  ((has-name "Glenfinnan")
   (has-county inverness-shire)))

(def-instance invergarry scottish-town
  ((has-name "Invergarry")
   (has-county inverness-shire)))

(def-instance inverness scottish-town
  ((has-name "Inverness")
   (has-county inverness-shire)))

(def-instance isle-of-benbecula scottish-town
  ((has-name "Isle Of Benbecula")
   (has-county inverness-shire)))

(def-instance isle-of-canna scottish-town
  ((has-name "Isle Of Canna")
   (has-county inverness-shire)))

(def-instance isle-of-eigg scottish-town
  ((has-name "Isle Of Eigg")
   (has-county inverness-shire)))

(def-instance isle-of-rhum scottish-town
  ((has-name "Isle Of Rhum")
   (has-county inverness-shire)))

(def-instance isle-of-scalpay scottish-town
  ((has-name "Isle Of Scalpay")
   (has-county inverness-shire)))

(def-instance isle-of-skye scottish-town
  ((has-name "Isle Of Skye")
   (has-county inverness-shire)))

(def-instance kingussie scottish-town
  ((has-name "Kingussie")
   (has-county inverness-shire)))

(def-instance lochailort scottish-town
  ((has-name "Lochailort")
   (has-county inverness-shire)))

(def-instance mallaig scottish-town
  ((has-name "Mallaig")
   (has-county inverness-shire)))

(def-instance nethy-bridge scottish-town
  ((has-name "Nethy Bridge")
   (has-county inverness-shire)))

(def-instance newtonmore scottish-town
  ((has-name "Newtonmore")
   (has-county inverness-shire)))

(def-instance roy-bridge scottish-town
  ((has-name "Roy Bridge")
   (has-county inverness-shire)))

(def-instance spean-bridge scottish-town
  ((has-name "Spean Bridge")
   (has-county inverness-shire)))

(def-instance brodick scottish-town
  ((has-name "Brodick")
   (has-county isle-of-arran)))

(def-instance castlebay scottish-town
  ((has-name "Castlebay")
   (has-county isle-of-barra)))

(def-instance rothesay scottish-town
  ((has-name "Rothesay")
   (has-county isle-of-bute)))

(def-instance millport scottish-town
  ((has-name "Millport")
   (has-county isle-of-cumbrae)))

(def-instance harris scottish-town
  ((has-name "Harris")
   (has-county isle-of-harris)))

(def-instance leverburgh scottish-town
  ((has-name "Leverburgh")
   (has-county isle-of-harris)))

(def-instance ballygrant scottish-town
  ((has-name "Ballygrant")
   (has-county isle-of-islay)))

(def-instance bowmore scottish-town
  ((has-name "Bowmore")
   (has-county isle-of-islay)))

(def-instance bridgend-pa scottish-town
  ((has-name "Bridgend Pa")
   (has-county isle-of-islay)))

(def-instance bruichladdich scottish-town
  ((has-name "Bruichladdich")
   (has-county isle-of-islay)))

(def-instance port-askaig scottish-town
  ((has-name "Port Askaig")
   (has-county isle-of-islay)))

(def-instance port-charlotte scottish-town
  ((has-name "Port Charlotte")
   (has-county isle-of-islay)))

(def-instance port-ellen scottish-town
  ((has-name "Port Ellen")
   (has-county isle-of-islay)))

(def-instance portnahaven scottish-town
  ((has-name "Portnahaven")
   (has-county isle-of-islay)))

(def-instance craighouse scottish-town
  ((has-name "Craighouse")
   (has-county isle-of-jura)))

(def-instance stornoway scottish-town
  ((has-name "Stornoway")
   (has-county isle-of-lewis)))

(def-instance ballasalla english-town
  ((has-name "Ballasalla")
   (has-county isle-of-man)))

(def-instance castletown english-town
  ((has-name "Castletown")
   (has-county isle-of-man)))

(def-instance douglas english-town
  ((has-name "Douglas")
   (has-county isle-of-man)))

(def-instance isle-of-man english-town
  ((has-name "Isle Of Man")
   (has-county isle-of-man)))

(def-instance kirk-michael english-town
  ((has-name "Kirk Michael")
   (has-county isle-of-man)))

(def-instance laxey english-town
  ((has-name "Laxey")
   (has-county isle-of-man)))

(def-instance peel english-town
  ((has-name "Peel")
   (has-county isle-of-man)))

(def-instance port-erin english-town
  ((has-name "Port Erin")
   (has-county isle-of-man)))

(def-instance port-st-mary english-town
  ((has-name "Port St. Mary")
   (has-county isle-of-man)))

(def-instance ramsey english-town
  ((has-name "Ramsey")
   (has-county isle-of-man)))

(def-instance lochmaddy scottish-town
  ((has-name "Lochmaddy")
   (has-county isle-of-north-uist)))

(def-instance portree scottish-town
  ((has-name "Portree")
   (has-county isle-of-skye)))

(def-instance lochboisdale scottish-town
  ((has-name "Lochboisdale")
   (has-county isle-of-south-uist)))

(def-instance scarinish scottish-town
  ((has-name "Scarinish")
   (has-county isle-of-tiree)))

(def-instance bembridge english-town
  ((has-name "Bembridge")
   (has-county isle-of-wight)))

(def-instance cowes english-town
  ((has-name "Cowes")
   (has-county isle-of-wight)))

(def-instance east-cowes english-town
  ((has-name "East Cowes")
   (has-county isle-of-wight)))

(def-instance freshwater english-town
  ((has-name "Freshwater")
   (has-county isle-of-wight)))

(def-instance newport-po english-town
  ((has-name "Newport Po")
   (has-county isle-of-wight)))

(def-instance ryde english-town
  ((has-name "Ryde")
   (has-county isle-of-wight)))

(def-instance sandown english-town
  ((has-name "Sandown")
   (has-county isle-of-wight)))

(def-instance seaview english-town
  ((has-name "Seaview")
   (has-county isle-of-wight)))

(def-instance shanklin english-town
  ((has-name "Shanklin")
   (has-county isle-of-wight)))

(def-instance totland-bay english-town
  ((has-name "Totland Bay")
   (has-county isle-of-wight)))

(def-instance ventnor english-town
  ((has-name "Ventnor")
   (has-county isle-of-wight)))

(def-instance yarmouth english-town
  ((has-name "Yarmouth")
   (has-county isle-of-wight)))

(def-instance bryher english-town
  ((has-name "Bryher")
   (has-county isles-of-scilly)))

(def-instance st-agnes-tr22 english-town
  ((has-name "St. Agnes Tr22")
   (has-county isles-of-scilly)))

(def-instance st-martins english-town
  ((has-name "St. Martins")
   (has-county isles-of-scilly)))

(def-instance st-marys english-town
  ((has-name "St. Marys")
   (has-county isles-of-scilly)))

(def-instance tresco english-town
  ((has-name "Tresco")
   (has-county isles-of-scilly)))

(def-instance ashford-tn english-town
  ((has-name "Ashford Tn")
   (has-county kent)))

(def-instance aylesford english-town
  ((has-name "Aylesford")
   (has-county kent)))

(def-instance beckenham english-town
  ((has-name "Beckenham")
   (has-county kent)))

(def-instance belvedere english-town
  ((has-name "Belvedere")
   (has-county kent)))

(def-instance bexley english-town
  ((has-name "Bexley")
   (has-county kent)))

(def-instance bexleyheath english-town
  ((has-name "Bexleyheath")
   (has-county kent)))

(def-instance birchington english-town
  ((has-name "Birchington")
   (has-county kent)))

(def-instance broadstairs english-town
  ((has-name "Broadstairs")
   (has-county kent)))

(def-instance bromley english-town
  ((has-name "Bromley")
   (has-county kent)))

(def-instance canterbury english-town
  ((has-name "Canterbury")
   (has-county kent)))

(def-instance chatham english-town
  ((has-name "Chatham")
   (has-county kent)))

(def-instance chislehurst english-town
  ((has-name "Chislehurst")
   (has-county kent)))

(def-instance cranbrook english-town
  ((has-name "Cranbrook")
   (has-county kent)))

(def-instance dartford english-town
  ((has-name "Dartford")
   (has-county kent)))

(def-instance deal english-town
  ((has-name "Deal")
   (has-county kent)))

(def-instance dover english-town
  ((has-name "Dover")
   (has-county kent)))

(def-instance edenbridge english-town
  ((has-name "Edenbridge")
   (has-county kent)))

(def-instance erith english-town
  ((has-name "Erith")
   (has-county kent)))

(def-instance faversham english-town
  ((has-name "Faversham")
   (has-county kent)))

(def-instance folkestone english-town
  ((has-name "Folkestone")
   (has-county kent)))

(def-instance gillingham-me english-town
  ((has-name "Gillingham Me")
   (has-county kent)))

(def-instance gravesend english-town
  ((has-name "Gravesend")
   (has-county kent)))

(def-instance greenhithe english-town
  ((has-name "Greenhithe")
   (has-county kent)))

(def-instance herne-bay english-town
  ((has-name "Herne Bay")
   (has-county kent)))

(def-instance hythe english-town
  ((has-name "Hythe")
   (has-county kent)))

(def-instance keston english-town
  ((has-name "Keston")
   (has-county kent)))

(def-instance longfield english-town
  ((has-name "Longfield")
   (has-county kent)))

(def-instance maidstone english-town
  ((has-name "Maidstone")
   (has-county kent)))

(def-instance margate english-town
  ((has-name "Margate")
   (has-county kent)))

(def-instance new-romney english-town
  ((has-name "New Romney")
   (has-county kent)))

(def-instance orpington english-town
  ((has-name "Orpington")
   (has-county kent)))

(def-instance queenborough english-town
  ((has-name "Queenborough")
   (has-county kent)))

(def-instance ramsgate english-town
  ((has-name "Ramsgate")
   (has-county kent)))

(def-instance rochester english-town
  ((has-name "Rochester")
   (has-county kent)))

(def-instance romney-marsh english-town
  ((has-name "Romney Marsh")
   (has-county kent)))

(def-instance sandwich english-town
  ((has-name "Sandwich")
   (has-county kent)))

(def-instance sevenoaks english-town
  ((has-name "Sevenoaks")
   (has-county kent)))

(def-instance sheerness english-town
  ((has-name "Sheerness")
   (has-county kent)))

(def-instance sidcup english-town
  ((has-name "Sidcup")
   (has-county kent)))

(def-instance sittingbourne english-town
  ((has-name "Sittingbourne")
   (has-county kent)))

(def-instance snodland english-town
  ((has-name "Snodland")
   (has-county kent)))

(def-instance swanley english-town
  ((has-name "Swanley")
   (has-county kent)))

(def-instance swanscombe english-town
  ((has-name "Swanscombe")
   (has-county kent)))

(def-instance tenterden english-town
  ((has-name "Tenterden")
   (has-county kent)))

(def-instance tonbridge english-town
  ((has-name "Tonbridge")
   (has-county kent)))

(def-instance tunbridge-wells english-town
  ((has-name "Tunbridge Wells")
   (has-county kent)))

(def-instance welling english-town
  ((has-name "Welling")
   (has-county kent)))

(def-instance west-malling english-town
  ((has-name "West Malling")
   (has-county kent)))

(def-instance west-wickham english-town
  ((has-name "West Wickham")
   (has-county kent)))

(def-instance westerham english-town
  ((has-name "Westerham")
   (has-county kent)))

(def-instance westgate-on-sea english-town
  ((has-name "Westgate-On-Sea")
   (has-county kent)))

(def-instance whitstable english-town
  ((has-name "Whitstable")
   (has-county kent)))

(def-instance banchory scottish-town
  ((has-name "Banchory")
   (has-county kincardineshire)))

(def-instance laurencekirk scottish-town
  ((has-name "Laurencekirk")
   (has-county kincardineshire)))

(def-instance stonehaven scottish-town
  ((has-name "Stonehaven")
   (has-county kincardineshire)))

(def-instance kinross scottish-town
  ((has-name "Kinross")
   (has-county kinross-shire)))

(def-instance castle-douglas scottish-town
  ((has-name "Castle Douglas")
   (has-county kirkcudbrightshire)))

(def-instance dalbeattie scottish-town
  ((has-name "Dalbeattie")
   (has-county kirkcudbrightshire)))

(def-instance kirkcudbright scottish-town
  ((has-name "Kirkcudbright")
   (has-county kirkcudbrightshire)))

(def-instance airdrie scottish-town
  ((has-name "Airdrie")
   (has-county lanarkshire)))

(def-instance bellshill scottish-town
  ((has-name "Bellshill")
   (has-county lanarkshire)))

(def-instance biggar scottish-town
  ((has-name "Biggar")
   (has-county lanarkshire)))

(def-instance carluke scottish-town
  ((has-name "Carluke")
   (has-county lanarkshire)))

(def-instance coatbridge scottish-town
  ((has-name "Coatbridge")
   (has-county lanarkshire)))

(def-instance glasgow scottish-town
  ((has-name "Glasgow")
   (has-county lanarkshire)))

(def-instance hamilton scottish-town
  ((has-name "Hamilton")
   (has-county lanarkshire)))

(def-instance lanark scottish-town
  ((has-name "Lanark")
   (has-county lanarkshire)))

(def-instance larkhall scottish-town
  ((has-name "Larkhall")
   (has-county lanarkshire)))

(def-instance motherwell scottish-town
  ((has-name "Motherwell")
   (has-county lanarkshire)))

(def-instance shotts scottish-town
  ((has-name "Shotts")
   (has-county lanarkshire)))

(def-instance strathaven scottish-town
  ((has-name "Strathaven")
   (has-county lanarkshire)))

(def-instance wishaw scottish-town
  ((has-name "Wishaw")
   (has-county lanarkshire)))

(def-instance accrington english-town
  ((has-name "Accrington")
   (has-county lancashire)))

(def-instance ashton-under-lyne english-town
  ((has-name "Ashton-Under-Lyne")
   (has-county lancashire)))

(def-instance askam-in-furness english-town
  ((has-name "Askam-In-Furness")
   (has-county lancashire)))

(def-instance bacup english-town
  ((has-name "Bacup")
   (has-county lancashire)))

(def-instance barrow-in-furness english-town
  ((has-name "Barrow-In-Furness")
   (has-county lancashire)))

(def-instance blackburn english-town
  ((has-name "Blackburn")
   (has-county lancashire)))

(def-instance blackpool english-town
  ((has-name "Blackpool")
   (has-county lancashire)))

(def-instance bolton english-town
  ((has-name "Bolton")
   (has-county lancashire)))

(def-instance bootle english-town
  ((has-name "Bootle")
   (has-county lancashire)))

(def-instance broughton-in-furness english-town
  ((has-name "Broughton-In-Furness")
   (has-county lancashire)))

(def-instance burnley english-town
  ((has-name "Burnley")
   (has-county lancashire)))

(def-instance bury english-town
  ((has-name "Bury")
   (has-county lancashire)))

(def-instance carnforth english-town
  ((has-name "Carnforth")
   (has-county lancashire)))

(def-instance chorley english-town
  ((has-name "Chorley")
   (has-county lancashire)))

(def-instance clitheroe english-town
  ((has-name "Clitheroe")
   (has-county lancashire)))

(def-instance colne english-town
  ((has-name "Colne")
   (has-county lancashire)))

(def-instance coniston english-town
  ((has-name "Coniston")
   (has-county lancashire)))

(def-instance dalton-in-furness english-town
  ((has-name "Dalton-In-Furness")
   (has-county lancashire)))

(def-instance darwen english-town
  ((has-name "Darwen")
   (has-county lancashire)))

(def-instance fleetwood english-town
  ((has-name "Fleetwood")
   (has-county lancashire)))

(def-instance grange-over-sands english-town
  ((has-name "Grange-Over-Sands")
   (has-county lancashire)))

(def-instance heywood english-town
  ((has-name "Heywood")
   (has-county lancashire)))

(def-instance kirkby-in-furness english-town
  ((has-name "Kirkby-In-Furness")
   (has-county lancashire)))

(def-instance lancaster english-town
  ((has-name "Lancaster")
   (has-county lancashire)))

(def-instance leigh english-town
  ((has-name "Leigh")
   (has-county lancashire)))

(def-instance littleborough english-town
  ((has-name "Littleborough")
   (has-county lancashire)))

(def-instance liverpool english-town
  ((has-name "Liverpool")
   (has-county lancashire)))

(def-instance lytham-st-annes english-town
  ((has-name "Lytham St. Annes")
   (has-county lancashire)))

(def-instance manchester english-town
  ((has-name "Manchester")
   (has-county lancashire)))

(def-instance morecambe english-town
  ((has-name "Morecambe")
   (has-county lancashire)))

(def-instance nelson english-town
  ((has-name "Nelson")
   (has-county lancashire)))

(def-instance newton-le-willows english-town
  ((has-name "Newton-Le-Willows")
   (has-county lancashire)))

(def-instance oldham english-town
  ((has-name "Oldham")
   (has-county lancashire)))

(def-instance ormskirk english-town
  ((has-name "Ormskirk")
   (has-county lancashire)))

(def-instance poulton-le-fylde english-town
  ((has-name "Poulton-Le-Fylde")
   (has-county lancashire)))

(def-instance prescot english-town
  ((has-name "Prescot")
   (has-county lancashire)))

(def-instance preston english-town
  ((has-name "Preston")
   (has-county lancashire)))

(def-instance rochdale english-town
  ((has-name "Rochdale")
   (has-county lancashire)))

(def-instance rossendale english-town
  ((has-name "Rossendale")
   (has-county lancashire)))

(def-instance salford english-town
  ((has-name "Salford")
   (has-county lancashire)))

(def-instance skelmersdale english-town
  ((has-name "Skelmersdale")
   (has-county lancashire)))

(def-instance southport english-town
  ((has-name "Southport")
   (has-county lancashire)))

(def-instance st-helens english-town
  ((has-name "St. Helens")
   (has-county lancashire)))

(def-instance thornton-cleveleys english-town
  ((has-name "Thornton-Cleveleys")
   (has-county lancashire)))

(def-instance todmorden english-town
  ((has-name "Todmorden")
   (has-county lancashire)))

(def-instance ulverston english-town
  ((has-name "Ulverston")
   (has-county lancashire)))

(def-instance warrington english-town
  ((has-name "Warrington")
   (has-county lancashire)))

(def-instance widnes english-town
  ((has-name "Widnes")
   (has-county lancashire)))

(def-instance wigan english-town
  ((has-name "Wigan")
   (has-county lancashire)))

(def-instance ashby-de-la-zouch english-town
  ((has-name "Ashby-De-La-Zouch")
   (has-county leicestershire)))

(def-instance coalville english-town
  ((has-name "Coalville")
   (has-county leicestershire)))

(def-instance hinckley english-town
  ((has-name "Hinckley")
   (has-county leicestershire)))

(def-instance ibstock english-town
  ((has-name "Ibstock")
   (has-county leicestershire)))

(def-instance leicester english-town
  ((has-name "Leicester")
   (has-county leicestershire)))

(def-instance loughborough english-town
  ((has-name "Loughborough")
   (has-county leicestershire)))

(def-instance lutterworth english-town
  ((has-name "Lutterworth")
   (has-county leicestershire)))

(def-instance market-harborough english-town
  ((has-name "Market Harborough")
   (has-county leicestershire)))

(def-instance markfield english-town
  ((has-name "Markfield")
   (has-county leicestershire)))

(def-instance melton-mowbray english-town
  ((has-name "Melton Mowbray")
   (has-county leicestershire)))

(def-instance wigston english-town
  ((has-name "Wigston")
   (has-county leicestershire)))

(def-instance barnetby english-town
  ((has-name "Barnetby")
   (has-county lincolnshire)))

(def-instance barrow-upon-humber english-town
  ((has-name "Barrow-Upon-Humber")
   (has-county lincolnshire)))

(def-instance barton-upon-humber english-town
  ((has-name "Barton-Upon-Humber")
   (has-county lincolnshire)))

(def-instance boston english-town
  ((has-name "Boston")
   (has-county lincolnshire)))

(def-instance bourne english-town
  ((has-name "Bourne")
   (has-county lincolnshire)))

(def-instance brigg english-town
  ((has-name "Brigg")
   (has-county lincolnshire)))

(def-instance cleethorpes english-town
  ((has-name "Cleethorpes")
   (has-county lincolnshire)))

(def-instance gainsborough english-town
  ((has-name "Gainsborough")
   (has-county lincolnshire)))

(def-instance grantham english-town
  ((has-name "Grantham")
   (has-county lincolnshire)))

(def-instance grimsby english-town
  ((has-name "Grimsby")
   (has-county lincolnshire)))

(def-instance horncastle english-town
  ((has-name "Horncastle")
   (has-county lincolnshire)))

(def-instance immingham english-town
  ((has-name "Immingham")
   (has-county lincolnshire)))

(def-instance lincoln english-town
  ((has-name "Lincoln")
   (has-county lincolnshire)))

(def-instance louth english-town
  ((has-name "Louth")
   (has-county lincolnshire)))

(def-instance mablethorpe english-town
  ((has-name "Mablethorpe")
   (has-county lincolnshire)))

(def-instance market-rasen english-town
  ((has-name "Market Rasen")
   (has-county lincolnshire)))

(def-instance scunthorpe english-town
  ((has-name "Scunthorpe")
   (has-county lincolnshire)))

(def-instance skegness english-town
  ((has-name "Skegness")
   (has-county lincolnshire)))

(def-instance sleaford english-town
  ((has-name "Sleaford")
   (has-county lincolnshire)))

(def-instance spalding english-town
  ((has-name "Spalding")
   (has-county lincolnshire)))

(def-instance spilsby english-town
  ((has-name "Spilsby")
   (has-county lincolnshire)))

(def-instance stamford english-town
  ((has-name "Stamford")
   (has-county lincolnshire)))

(def-instance ulceby english-town
  ((has-name "Ulceby")
   (has-county lincolnshire)))

(def-instance woodhall-spa english-town
  ((has-name "Woodhall Spa")
   (has-county lincolnshire)))

(def-instance aberdovey english-town
  ((has-name "Aberdovey")
   (has-county merioneth)))

(def-instance arthog welsh-town
  ((has-name "Arthog")
   (has-county merioneth)))

(def-instance bala welsh-town
  ((has-name "Bala")
   (has-county merioneth)))

(def-instance barmouth welsh-town
  ((has-name "Barmouth")
   (has-county merioneth)))

(def-instance blaenau-ffestiniog welsh-town
  ((has-name "Blaenau Ffestiniog")
   (has-county merioneth)))

(def-instance corwen welsh-town
  ((has-name "Corwen")
   (has-county merioneth)))

(def-instance dolgellau welsh-town
  ((has-name "Dolgellau")
   (has-county merioneth)))

(def-instance dyffryn-ardudwy welsh-town
  ((has-name "Dyffryn Ardudwy")
   (has-county merioneth)))

(def-instance fairbourne welsh-town
  ((has-name "Fairbourne")
   (has-county merioneth)))

(def-instance harlech welsh-town
  ((has-name "Harlech")
   (has-county merioneth)))

(def-instance llanbedr welsh-town
  ((has-name "Llanbedr")
   (has-county merioneth)))

(def-instance llwyngwril welsh-town
  ((has-name "Llwyngwril")
   (has-county merioneth)))

(def-instance penrhyndeudraeth welsh-town
  ((has-name "Penrhyndeudraeth")
   (has-county merioneth)))

(def-instance talsarnau welsh-town
  ((has-name "Talsarnau")
   (has-county merioneth)))

(def-instance talybont-ll welsh-town
  ((has-name "Talybont Ll")
   (has-county merioneth)))

(def-instance tywyn welsh-town
  ((has-name "Tywyn")
   (has-county merioneth)))

(def-instance ashford-tw welsh-town
  ((has-name "Ashford Tw")
   (has-county middlesex)))

(def-instance brentford english-town
  ((has-name "Brentford")
   (has-county middlesex)))

(def-instance edgware english-town
  ((has-name "Edgware")
   (has-county middlesex)))

(def-instance enfield english-town
  ((has-name "Enfield")
   (has-county middlesex)))

(def-instance feltham english-town
  ((has-name "Feltham")
   (has-county middlesex)))

(def-instance greenford english-town
  ((has-name "Greenford")
   (has-county middlesex)))

(def-instance hampton english-town
  ((has-name "Hampton")
   (has-county middlesex)))

(def-instance harrow english-town
  ((has-name "Harrow")
   (has-county middlesex)))

(def-instance hayes english-town
  ((has-name "Hayes")
   (has-county middlesex)))

(def-instance hounslow english-town
  ((has-name "Hounslow")
   (has-county middlesex)))

(def-instance isleworth english-town
  ((has-name "Isleworth")
   (has-county middlesex)))

(def-instance london english-town
  ((has-name "London")
   (has-county london)))

(def-instance northolt english-town
  ((has-name "Northolt")
   (has-county middlesex)))

(def-instance northwood english-town
  ((has-name "Northwood")
   (has-county middlesex)))

(def-instance pinner english-town
  ((has-name "Pinner")
   (has-county middlesex)))

(def-instance potters-bar english-town
  ((has-name "Potters Bar")
   (has-county middlesex)))

(def-instance ruislip english-town
  ((has-name "Ruislip")
   (has-county middlesex)))

(def-instance shepperton english-town
  ((has-name "Shepperton")
   (has-county middlesex)))

(def-instance southall english-town
  ((has-name "Southall")
   (has-county middlesex)))

(def-instance staines english-town
  ((has-name "Staines")
   (has-county middlesex)))

(def-instance stanmore english-town
  ((has-name "Stanmore")
   (has-county middlesex)))

(def-instance sunbury-on-thames english-town
  ((has-name "Sunbury-On-Thames")
   (has-county middlesex)))

(def-instance teddington english-town
  ((has-name "Teddington")
   (has-county middlesex)))

(def-instance twickenham english-town
  ((has-name "Twickenham")
   (has-county middlesex)))

(def-instance uxbridge english-town
  ((has-name "Uxbridge")
   (has-county middlesex)))

(def-instance wembley english-town
  ((has-name "Wembley")
   (has-county middlesex)))

(def-instance west-drayton english-town
  ((has-name "West Drayton")
   (has-county middlesex)))

(def-instance balerno scottish-town
  ((has-name "Balerno")
   (has-county midlothian)))

(def-instance bonnyrigg scottish-town
  ((has-name "Bonnyrigg")
   (has-county midlothian)))

(def-instance currie scottish-town
  ((has-name "Currie")
   (has-county midlothian)))

(def-instance dalkeith scottish-town
  ((has-name "Dalkeith")
   (has-county midlothian)))

(def-instance edinburgh scottish-town
  ((has-name "Edinburgh")
   (has-county midlothian)))

(def-instance gorebridge scottish-town
  ((has-name "Gorebridge")
   (has-county midlothian)))

(def-instance heriot scottish-town
  ((has-name "Heriot")
   (has-county midlothian)))

(def-instance juniper-green scottish-town
  ((has-name "Juniper Green")
   (has-county midlothian)))

(def-instance kirknewton scottish-town
  ((has-name "Kirknewton")
   (has-county midlothian)))

(def-instance lasswade scottish-town
  ((has-name "Lasswade")
   (has-county midlothian)))

(def-instance loanhead scottish-town
  ((has-name "Loanhead")
   (has-county midlothian)))

(def-instance musselburgh scottish-town
  ((has-name "Musselburgh")
   (has-county midlothian)))

(def-instance newbridge scottish-town
  ((has-name "Newbridge")
   (has-county midlothian)))

(def-instance pathhead scottish-town
  ((has-name "Pathhead")
   (has-county midlothian)))

(def-instance penicuik scottish-town
  ((has-name "Penicuik")
   (has-county midlothian)))

(def-instance rosewell scottish-town
  ((has-name "Rosewell")
   (has-county midlothian)))

(def-instance roslin scottish-town
  ((has-name "Roslin")
   (has-county midlothian)))

(def-instance west-calder scottish-town
  ((has-name "West Calder")
   (has-county midlothian)))

(def-instance abergavenny welsh-town
  ((has-name "Abergavenny")
   (has-county monmouthshire)))

(def-instance abertillery welsh-town
  ((has-name "Abertillery")
   (has-county monmouthshire)))

(def-instance blackwood welsh-town
  ((has-name "Blackwood")
   (has-county monmouthshire)))

(def-instance caldicot welsh-town
  ((has-name "Caldicot")
   (has-county monmouthshire)))

(def-instance chepstow welsh-town
  ((has-name "Chepstow")
   (has-county monmouthshire)))

(def-instance cwmbran welsh-town
  ((has-name "Cwmbran")
   (has-county monmouthshire)))

(def-instance ebbw-vale welsh-town
  ((has-name "Ebbw Vale")
   (has-county monmouthshire)))

(def-instance monmouth welsh-town
  ((has-name "Monmouth")
   (has-county monmouthshire)))

(def-instance new-tredegar welsh-town
  ((has-name "New Tredegar")
   (has-county monmouthshire)))

(def-instance newport-np welsh-town
  ((has-name "Newport Np")
   (has-county monmouthshire)))

(def-instance pontypool welsh-town
  ((has-name "Pontypool")
   (has-county monmouthshire)))

(def-instance tredegar welsh-town
  ((has-name "Tredegar")
   (has-county monmouthshire)))

(def-instance usk welsh-town
  ((has-name "Usk")
   (has-county monmouthshire)))

(def-instance caersws welsh-town
  ((has-name "Caersws")
   (has-county montgomeryshire)))

(def-instance llanbrynmair welsh-town
  ((has-name "Llanbrynmair")
   (has-county montgomeryshire)))

(def-instance llandinam welsh-town
  ((has-name "Llandinam")
   (has-county montgomeryshire)))

(def-instance llanfechain welsh-town
  ((has-name "Llanfechain")
   (has-county montgomeryshire)))

(def-instance llanfyllin welsh-town
  ((has-name "Llanfyllin")
   (has-county montgomeryshire)))

(def-instance llanidloes welsh-town
  ((has-name "Llanidloes")
   (has-county montgomeryshire)))

(def-instance llansantffraid welsh-town
  ((has-name "Llansantffraid")
   (has-county montgomeryshire)))

(def-instance llanymynech welsh-town
  ((has-name "Llanymynech")
   (has-county montgomeryshire)))

(def-instance machynlleth welsh-town
  ((has-name "Machynlleth")
   (has-county montgomeryshire)))

(def-instance meifod welsh-town
  ((has-name "Meifod")
   (has-county montgomeryshire)))

(def-instance montgomery welsh-town
  ((has-name "Montgomery")
   (has-county montgomeryshire)))

(def-instance newtown welsh-town
  ((has-name "Newtown")
   (has-county montgomeryshire)))

(def-instance welshpool welsh-town
  ((has-name "Welshpool")
   (has-county montgomeryshire)))

(def-instance elgin scottish-town
  ((has-name "Elgin")
   (has-county morayshire)))

(def-instance fochabers scottish-town
  ((has-name "Fochabers")
   (has-county morayshire)))

(def-instance forres scottish-town
  ((has-name "Forres")
   (has-county morayshire)))

(def-instance grantown-on-spey scottish-town
  ((has-name "Grantown-On-Spey")
   (has-county morayshire)))

(def-instance lossiemouth scottish-town
  ((has-name "Lossiemouth")
   (has-county morayshire)))

(def-instance nairn scottish-town
  ((has-name "Nairn")
   (has-county nairnshire)))

(def-instance attleborough english-town
  ((has-name "Attleborough")
   (has-county norfolk)))

(def-instance cromer english-town
  ((has-name "Cromer")
   (has-county norfolk)))

(def-instance dereham english-town
  ((has-name "Dereham")
   (has-county norfolk)))

(def-instance diss english-town
  ((has-name "Diss")
   (has-county norfolk)))

(def-instance downham-market english-town
  ((has-name "Downham Market")
   (has-county norfolk)))

(def-instance fakenham english-town
  ((has-name "Fakenham")
   (has-county norfolk)))

(def-instance great-yarmouth english-town
  ((has-name "Great Yarmouth")
   (has-county norfolk)))

(def-instance harleston english-town
  ((has-name "Harleston")
   (has-county norfolk)))

(def-instance holt english-town
  ((has-name "Holt")
   (has-county norfolk)))

(def-instance hunstanton english-town
  ((has-name "Hunstanton")
   (has-county norfolk)))

(def-instance kings-lynn english-town
  ((has-name "King's Lynn")
   (has-county norfolk)))

(def-instance melton-constable english-town
  ((has-name "Melton Constable")
   (has-county norfolk)))

(def-instance north-walsham english-town
  ((has-name "North Walsham")
   (has-county norfolk)))

(def-instance norwich english-town
  ((has-name "Norwich")
   (has-county norfolk)))

(def-instance sandringham english-town
  ((has-name "Sandringham")
   (has-county norfolk)))

(def-instance sheringham english-town
  ((has-name "Sheringham")
   (has-county norfolk)))

(def-instance swaffham english-town
  ((has-name "Swaffham")
   (has-county norfolk)))

(def-instance thetford english-town
  ((has-name "Thetford")
   (has-county norfolk)))

(def-instance walsingham english-town
  ((has-name "Walsingham")
   (has-county norfolk)))

(def-instance wells-next-the-sea english-town
  ((has-name "Wells-Next-The-Sea")
   (has-county norfolk)))

(def-instance wymondham english-town
  ((has-name "Wymondham")
   (has-county norfolk)))

(def-instance brackley english-town
  ((has-name "Brackley")
   (has-county northamptonshire)))

(def-instance corby english-town
  ((has-name "Corby")
   (has-county northamptonshire)))

(def-instance daventry english-town
  ((has-name "Daventry")
   (has-county northamptonshire)))

(def-instance kettering english-town
  ((has-name "Kettering")
   (has-county northamptonshire)))

(def-instance northampton english-town
  ((has-name "Northampton")
   (has-county northamptonshire)))

(def-instance peterborough english-town
  ((has-name "Peterborough")
   (has-county northamptonshire)))

(def-instance rushden english-town
  ((has-name "Rushden")
   (has-county northamptonshire)))

(def-instance towcester english-town
  ((has-name "Towcester")
   (has-county northamptonshire)))

(def-instance wellingborough english-town
  ((has-name "Wellingborough")
   (has-county northamptonshire)))

(def-instance alnwick english-town
  ((has-name "Alnwick")
   (has-county northumberland)))

(def-instance ashington english-town
  ((has-name "Ashington")
   (has-county northumberland)))

(def-instance bamburgh english-town
  ((has-name "Bamburgh")
   (has-county northumberland)))

(def-instance bedlington english-town
  ((has-name "Bedlington")
   (has-county northumberland)))

(def-instance belford english-town
  ((has-name "Belford")
   (has-county northumberland)))

(def-instance berwick-upon-tweed english-town
  ((has-name "Berwick-Upon-Tweed")
   (has-county northumberland)))

(def-instance blyth english-town
  ((has-name "Blyth")
   (has-county northumberland)))

(def-instance chathill english-town
  ((has-name "Chathill")
   (has-county northumberland)))

(def-instance choppington english-town
  ((has-name "Choppington")
   (has-county northumberland)))

(def-instance corbridge english-town
  ((has-name "Corbridge")
   (has-county northumberland)))

(def-instance cornhill-on-tweed english-town
  ((has-name "Cornhill-On-Tweed")
   (has-county northumberland)))

(def-instance cramlington english-town
  ((has-name "Cramlington")
   (has-county northumberland)))

(def-instance haltwhistle english-town
  ((has-name "Haltwhistle")
   (has-county northumberland)))

(def-instance hexham english-town
  ((has-name "Hexham")
   (has-county northumberland)))

(def-instance mindrum english-town
  ((has-name "Mindrum")
   (has-county northumberland)))

(def-instance morpeth english-town
  ((has-name "Morpeth")
   (has-county northumberland)))

(def-instance newbiggin-by-the-sea english-town
  ((has-name "Newbiggin-By-The-Sea")
   (has-county northumberland)))

(def-instance newcastle-upon-tyne english-town
  ((has-name "Newcastle Upon Tyne")
   (has-county northumberland)))

(def-instance north-shields english-town
  ((has-name "North Shields")
   (has-county northumberland)))

(def-instance prudhoe english-town
  ((has-name "Prudhoe")
   (has-county northumberland)))

(def-instance riding-mill english-town
  ((has-name "Riding Mill")
   (has-county northumberland)))

(def-instance seahouses english-town
  ((has-name "Seahouses")
   (has-county northumberland)))

(def-instance stocksfield english-town
  ((has-name "Stocksfield")
   (has-county northumberland)))

(def-instance wallsend english-town
  ((has-name "Wallsend")
   (has-county northumberland)))

(def-instance whitley-bay english-town
  ((has-name "Whitley Bay")
   (has-county northumberland)))

(def-instance wooler english-town
  ((has-name "Wooler")
   (has-county northumberland)))

(def-instance wylam english-town
  ((has-name "Wylam")
   (has-county northumberland)))

(def-instance mansfield english-town
  ((has-name "Mansfield")
   (has-county nottinghamshire)))

(def-instance newark english-town
  ((has-name "Newark")
   (has-county nottinghamshire)))

(def-instance nottingham english-town
  ((has-name "Nottingham")
   (has-county nottinghamshire)))

(def-instance retford english-town
  ((has-name "Retford")
   (has-county nottinghamshire)))

(def-instance southwell english-town
  ((has-name "Southwell")
   (has-county nottinghamshire)))

(def-instance sutton-in-ashfield english-town
  ((has-name "Sutton-In-Ashfield")
   (has-county nottinghamshire)))

(def-instance worksop english-town
  ((has-name "Worksop")
   (has-county nottinghamshire)))

(def-instance kirkwall scottish-town
  ((has-name "Kirkwall")
   (has-county orkney)))

(def-instance orkney scottish-town
  ((has-name "Orkney")
   (has-county orkney)))

(def-instance stromness scottish-town
  ((has-name "Stromness")
   (has-county orkney)))

(def-instance bampton english-town
  ((has-name "Bampton")
   (has-county oxfordshire)))

(def-instance banbury english-town
  ((has-name "Banbury")
   (has-county oxfordshire)))

(def-instance bicester english-town
  ((has-name "Bicester")
   (has-county oxfordshire)))

(def-instance burford english-town
  ((has-name "Burford")
   (has-county oxfordshire)))

(def-instance carterton english-town
  ((has-name "Carterton")
   (has-county oxfordshire)))

(def-instance chinnor english-town
  ((has-name "Chinnor")
   (has-county oxfordshire)))

(def-instance chipping-norton english-town
  ((has-name "Chipping Norton")
   (has-county oxfordshire)))

(def-instance henley-on-thames english-town
  ((has-name "Henley-On-Thames")
   (has-county oxfordshire)))

(def-instance kidlington english-town
  ((has-name "Kidlington")
   (has-county oxfordshire)))

(def-instance oxford english-town
  ((has-name "Oxford")
   (has-county oxfordshire)))

(def-instance thame english-town
  ((has-name "Thame")
   (has-county oxfordshire)))

(def-instance watlington english-town
  ((has-name "Watlington")
   (has-county oxfordshire)))

(def-instance witney english-town
  ((has-name "Witney")
   (has-county oxfordshire)))

(def-instance woodstock english-town
  ((has-name "Woodstock")
   (has-county oxfordshire)))

(def-instance innerleithen scottish-town
  ((has-name "Innerleithen")
   (has-county peeblesshire)))

(def-instance peebles scottish-town
  ((has-name "Peebles")
   (has-county peeblesshire)))

(def-instance walkerburn scottish-town
  ((has-name "Walkerburn")
   (has-county peeblesshire)))

(def-instance west-linton scottish-town
  ((has-name "West Linton")
   (has-county peeblesshire)))

(def-instance boncath welsh-town
  ((has-name "Boncath")
   (has-county pembrokeshire)))

(def-instance clarbeston-road welsh-town
  ((has-name "Clarbeston Road")
   (has-county pembrokeshire)))

(def-instance crymych welsh-town
  ((has-name "Crymych")
   (has-county pembrokeshire)))

(def-instance fishguard welsh-town
  ((has-name "Fishguard")
   (has-county pembrokeshire)))

(def-instance glogue welsh-town
  ((has-name "Glogue")
   (has-county pembrokeshire)))

(def-instance goodwick welsh-town
  ((has-name "Goodwick")
   (has-county pembrokeshire)))

(def-instance haverfordwest welsh-town
  ((has-name "Haverfordwest")
   (has-county pembrokeshire)))

(def-instance kilgetty welsh-town
  ((has-name "Kilgetty")
   (has-county pembrokeshire)))

(def-instance llanfyrnach welsh-town
  ((has-name "Llanfyrnach")
   (has-county pembrokeshire)))

(def-instance milford-haven welsh-town
  ((has-name "Milford Haven")
   (has-county pembrokeshire)))

(def-instance narberth welsh-town
  ((has-name "Narberth")
   (has-county pembrokeshire)))

(def-instance newport-sa welsh-town
  ((has-name "Newport Sa")
   (has-county pembrokeshire)))

(def-instance pembroke welsh-town
  ((has-name "Pembroke")
   (has-county pembrokeshire)))

(def-instance pembroke-dock welsh-town
  ((has-name "Pembroke Dock")
   (has-county pembrokeshire)))

(def-instance saundersfoot welsh-town
  ((has-name "Saundersfoot")
   (has-county pembrokeshire)))

(def-instance tenby welsh-town
  ((has-name "Tenby")
   (has-county pembrokeshire)))

(def-instance aberfeldy scottish-town
  ((has-name "Aberfeldy")
   (has-county perthshire)))

(def-instance auchterarder scottish-town
  ((has-name "Auchterarder")
   (has-county perthshire)))

(def-instance blairgowrie scottish-town
  ((has-name "Blairgowrie")
   (has-county perthshire)))

(def-instance callander scottish-town
  ((has-name "Callander")
   (has-county perthshire)))

(def-instance crianlarich scottish-town
  ((has-name "Crianlarich")
   (has-county perthshire)))

(def-instance crieff scottish-town
  ((has-name "Crieff")
   (has-county perthshire)))

(def-instance doune scottish-town
  ((has-name "Doune")
   (has-county perthshire)))

(def-instance dunblane scottish-town
  ((has-name "Dunblane")
   (has-county perthshire)))

(def-instance dunkeld scottish-town
  ((has-name "Dunkeld")
   (has-county perthshire)))

(def-instance killin scottish-town
  ((has-name "Killin")
   (has-county perthshire)))

(def-instance lochearnhead scottish-town
  ((has-name "Lochearnhead")
   (has-county perthshire)))

(def-instance perth scottish-town
  ((has-name "Perth")
   (has-county perthshire)))

(def-instance pitlochry scottish-town
  ((has-name "Pitlochry")
   (has-county perthshire)))

(def-instance builth-wells welsh-town
  ((has-name "Builth Wells")
   (has-county radnorshire)))

(def-instance knighton welsh-town
  ((has-name "Knighton")
   (has-county radnorshire)))

(def-instance llandrindod-wells welsh-town
  ((has-name "Llandrindod Wells")
   (has-county radnorshire)))

(def-instance presteigne welsh-town
  ((has-name "Presteigne")
   (has-county radnorshire)))

(def-instance rhayader welsh-town
  ((has-name "Rhayader")
   (has-county radnorshire)))

(def-instance bishopton scottish-town
  ((has-name "Bishopton")
   (has-county renfrewshire)))

(def-instance bridge-of-weir scottish-town
  ((has-name "Bridge Of Weir")
   (has-county renfrewshire)))

(def-instance erskine scottish-town
  ((has-name "Erskine")
   (has-county renfrewshire)))

(def-instance gourock scottish-town
  ((has-name "Gourock")
   (has-county renfrewshire)))

(def-instance greenock scottish-town
  ((has-name "Greenock")
   (has-county renfrewshire)))

(def-instance johnstone scottish-town
  ((has-name "Johnstone")
   (has-county renfrewshire)))

(def-instance kilmacolm scottish-town
  ((has-name "Kilmacolm")
   (has-county renfrewshire)))

(def-instance lochwinnoch scottish-town
  ((has-name "Lochwinnoch")
   (has-county renfrewshire)))

(def-instance paisley scottish-town
  ((has-name "Paisley")
   (has-county renfrewshire)))

(def-instance port-glasgow scottish-town
  ((has-name "Port Glasgow")
   (has-county renfrewshire)))

(def-instance renfrew scottish-town
  ((has-name "Renfrew")
   (has-county renfrewshire)))

(def-instance wemyss-bay scottish-town
  ((has-name "Wemyss Bay")
   (has-county renfrewshire)))

(def-instance achnasheen scottish-town
  ((has-name "Achnasheen")
   (has-county ross-shire)))

(def-instance alness scottish-town
  ((has-name "Alness")
   (has-county ross-shire)))

(def-instance ardgay scottish-town
  ((has-name "Ardgay")
   (has-county ross-shire)))

(def-instance avoch scottish-town
  ((has-name "Avoch")
   (has-county ross-shire)))

(def-instance cromarty scottish-town
  ((has-name "Cromarty")
   (has-county ross-shire)))

(def-instance dingwall scottish-town
  ((has-name "Dingwall")
   (has-county ross-shire)))

(def-instance fortrose scottish-town
  ((has-name "Fortrose")
   (has-county ross-shire)))

(def-instance gairloch scottish-town
  ((has-name "Gairloch")
   (has-county ross-shire)))

(def-instance garve scottish-town
  ((has-name "Garve")
   (has-county ross-shire)))

(def-instance invergordon scottish-town
  ((has-name "Invergordon")
   (has-county ross-shire)))

(def-instance isle-of-lewis scottish-town
  ((has-name "Isle Of Lewis")
   (has-county ross-shire)))

(def-instance kyle scottish-town
  ((has-name "Kyle")
   (has-county ross-shire)))

(def-instance muir-of-ord scottish-town
  ((has-name "Muir Of Ord")
   (has-county ross-shire)))

(def-instance munlochy scottish-town
  ((has-name "Munlochy")
   (has-county ross-shire)))

(def-instance plockton scottish-town
  ((has-name "Plockton")
   (has-county ross-shire)))

(def-instance strathcarron scottish-town
  ((has-name "Strathcarron")
   (has-county ross-shire)))

(def-instance strathpeffer scottish-town
  ((has-name "Strathpeffer")
   (has-county ross-shire)))

(def-instance strome-ferry scottish-town
  ((has-name "Strome Ferry")
   (has-county ross-shire)))

(def-instance tain scottish-town
  ((has-name "Tain")
   (has-county ross-shire)))

(def-instance ullapool scottish-town
  ((has-name "Ullapool")
   (has-county ross-shire)))

(def-instance hawick scottish-town
  ((has-name "Hawick")
   (has-county roxburghshire)))

(def-instance jedburgh scottish-town
  ((has-name "Jedburgh")
   (has-county roxburghshire)))

(def-instance kelso scottish-town
  ((has-name "Kelso")
   (has-county roxburghshire)))

(def-instance melrose scottish-town
  ((has-name "Melrose")
   (has-county roxburghshire)))

(def-instance newcastleton scottish-town
  ((has-name "Newcastleton")
   (has-county roxburghshire)))

(def-instance oakham english-town
  ((has-name "Oakham")
   (has-county rutland)))

(def-instance galashiels scottish-town
  ((has-name "Galashiels")
   (has-county selkirkshire)))

(def-instance selkirk scottish-town
  ((has-name "Selkirk")
   (has-county selkirkshire)))

(def-instance lerwick scottish-town
  ((has-name "Lerwick")
   (has-county shetland)))

(def-instance shetland scottish-town
  ((has-name "Shetland")
   (has-county shetland)))

(def-instance bishops-castle english-town
  ((has-name "Bishops Castle")
   (has-county shropshire)))

(def-instance bridgnorth english-town
  ((has-name "Bridgnorth")
   (has-county shropshire)))

(def-instance broseley english-town
  ((has-name "Broseley")
   (has-county shropshire)))

(def-instance bucknell english-town
  ((has-name "Bucknell")
   (has-county shropshire)))

(def-instance church-stretton english-town
  ((has-name "Church Stretton")
   (has-county shropshire)))

(def-instance craven-arms english-town
  ((has-name "Craven Arms")
   (has-county shropshire)))

(def-instance ellesmere english-town
  ((has-name "Ellesmere")
   (has-county shropshire)))

(def-instance ludlow english-town
  ((has-name "Ludlow")
   (has-county shropshire)))

(def-instance lydbury-north english-town
  ((has-name "Lydbury North")
   (has-county shropshire)))

(def-instance market-drayton english-town
  ((has-name "Market Drayton")
   (has-county shropshire)))

(def-instance much-wenlock english-town
  ((has-name "Much Wenlock")
   (has-county shropshire)))

(def-instance newport-tf english-town
  ((has-name "Newport Tf")
   (has-county shropshire)))

(def-instance oswestry english-town
  ((has-name "Oswestry")
   (has-county shropshire)))

(def-instance shifnal english-town
  ((has-name "Shifnal")
   (has-county shropshire)))

(def-instance shrewsbury english-town
  ((has-name "Shrewsbury")
   (has-county shropshire)))

(def-instance telford english-town
  ((has-name "Telford")
   (has-county shropshire)))

(def-instance whitchurch-sy english-town
  ((has-name "Whitchurch Sy")
   (has-county shropshire)))

(def-instance axbridge english-town
  ((has-name "Axbridge")
   (has-county somerset)))

(def-instance banwell english-town
  ((has-name "Banwell")
   (has-county somerset)))

(def-instance bath english-town
  ((has-name "Bath")
   (has-county somerset)))

(def-instance bridgwater english-town
  ((has-name "Bridgwater")
   (has-county somerset)))

(def-instance bruton english-town
  ((has-name "Bruton")
   (has-county somerset)))

(def-instance burnham-on-sea english-town
  ((has-name "Burnham-On-Sea")
   (has-county somerset)))

(def-instance castle-cary english-town
  ((has-name "Castle Cary")
   (has-county somerset)))

(def-instance chard english-town
  ((has-name "Chard")
   (has-county somerset)))

(def-instance cheddar english-town
  ((has-name "Cheddar")
   (has-county somerset)))

(def-instance clevedon english-town
  ((has-name "Clevedon")
   (has-county somerset)))

(def-instance crewkerne english-town
  ((has-name "Crewkerne")
   (has-county somerset)))

(def-instance dulverton english-town
  ((has-name "Dulverton")
   (has-county somerset)))

(def-instance frome english-town
  ((has-name "Frome")
   (has-county somerset)))

(def-instance glastonbury english-town
  ((has-name "Glastonbury")
   (has-county somerset)))

(def-instance highbridge english-town
  ((has-name "Highbridge")
   (has-county somerset)))

(def-instance hinton-st-george english-town
  ((has-name "Hinton St. George")
   (has-county somerset)))

(def-instance ilminster english-town
  ((has-name "Ilminster")
   (has-county somerset)))

(def-instance langport english-town
  ((has-name "Langport")
   (has-county somerset)))

(def-instance martock english-town
  ((has-name "Martock")
   (has-county somerset)))

(def-instance merriott english-town
  ((has-name "Merriott")
   (has-county somerset)))

(def-instance minehead english-town
  ((has-name "Minehead")
   (has-county somerset)))

(def-instance montacute english-town
  ((has-name "Montacute")
   (has-county somerset)))

(def-instance shepton-mallet english-town
  ((has-name "Shepton Mallet")
   (has-county somerset)))

(def-instance somerton english-town
  ((has-name "Somerton")
   (has-county somerset)))

(def-instance south-petherton english-town
  ((has-name "South Petherton")
   (has-county somerset)))

(def-instance stoke-sub-hamdon english-town
  ((has-name "Stoke-Sub-Hamdon")
   (has-county somerset)))

(def-instance street english-town
  ((has-name "Street")
   (has-county somerset)))

(def-instance taunton english-town
  ((has-name "Taunton")
   (has-county somerset)))

(def-instance templecombe english-town
  ((has-name "Templecombe")
   (has-county somerset)))

(def-instance watchet english-town
  ((has-name "Watchet")
   (has-county somerset)))

(def-instance wedmore english-town
  ((has-name "Wedmore")
   (has-county somerset)))

(def-instance wellington english-town
  ((has-name "Wellington")
   (has-county somerset)))

(def-instance wells english-town
  ((has-name "Wells")
   (has-county somerset)))

(def-instance weston-super-mare english-town
  ((has-name "Weston-Super-Mare")
   (has-county somerset)))

(def-instance wincanton english-town
  ((has-name "Wincanton")
   (has-county somerset)))

(def-instance winscombe english-town
  ((has-name "Winscombe")
   (has-county somerset)))

(def-instance yeovil english-town
  ((has-name "Yeovil")
   (has-county somerset)))

(def-instance bilston english-town
  ((has-name "Bilston")
   (has-county staffordshire)))

(def-instance brierley-hill english-town
  ((has-name "Brierley Hill")
   (has-county staffordshire)))

(def-instance burntwood english-town
  ((has-name "Burntwood")
   (has-county staffordshire)))

(def-instance burton-on-trent english-town
  ((has-name "Burton-On-Trent")
   (has-county staffordshire)))

(def-instance cannock english-town
  ((has-name "Cannock")
   (has-county staffordshire)))

(def-instance cradley-heath english-town
  ((has-name "Cradley Heath")
   (has-county staffordshire)))

(def-instance kingswinford english-town
  ((has-name "Kingswinford")
   (has-county staffordshire)))

(def-instance leek english-town
  ((has-name "Leek")
   (has-county staffordshire)))

(def-instance lichfield english-town
  ((has-name "Lichfield")
   (has-county staffordshire)))

(def-instance newcastle-st english-town
  ((has-name "Newcastle St")
   (has-county staffordshire)))

(def-instance rowley-regis english-town
  ((has-name "Rowley Regis")
   (has-county staffordshire)))

(def-instance rugeley english-town
  ((has-name "Rugeley")
   (has-county staffordshire)))

(def-instance smethwick english-town
  ((has-name "Smethwick")
   (has-county staffordshire)))

(def-instance stafford english-town
  ((has-name "Stafford")
   (has-county staffordshire)))

(def-instance stoke-on-trent english-town
  ((has-name "Stoke-On-Trent")
   (has-county staffordshire)))

(def-instance stone english-town
  ((has-name "Stone")
   (has-county staffordshire)))

(def-instance tamworth english-town
  ((has-name "Tamworth")
   (has-county staffordshire)))

(def-instance tipton english-town
  ((has-name "Tipton")
   (has-county staffordshire)))

(def-instance uttoxeter english-town
  ((has-name "Uttoxeter")
   (has-county staffordshire)))

(def-instance walsall english-town
  ((has-name "Walsall")
   (has-county staffordshire)))

(def-instance wednesbury english-town
  ((has-name "Wednesbury")
   (has-county staffordshire)))

(def-instance west-bromwich english-town
  ((has-name "West Bromwich")
   (has-county staffordshire)))

(def-instance willenhall english-town
  ((has-name "Willenhall")
   (has-county staffordshire)))

(def-instance wolverhampton english-town
  ((has-name "Wolverhampton")
   (has-county staffordshire)))

(def-instance bonnybridge scottish-town
  ((has-name "Bonnybridge")
   (has-county stirlingshire)))

(def-instance denny scottish-town
  ((has-name "Denny")
   (has-county stirlingshire)))

(def-instance falkirk scottish-town
  ((has-name "Falkirk")
   (has-county stirlingshire)))

(def-instance grangemouth scottish-town
  ((has-name "Grangemouth")
   (has-county stirlingshire)))

(def-instance larbert scottish-town
  ((has-name "Larbert")
   (has-county stirlingshire)))

(def-instance stirling scottish-town
  ((has-name "Stirling")
   (has-county stirlingshire)))

(def-instance aldeburgh english-town
  ((has-name "Aldeburgh")
   (has-county suffolk)))

(def-instance beccles english-town
  ((has-name "Beccles")
   (has-county suffolk)))

(def-instance brandon english-town
  ((has-name "Brandon")
   (has-county suffolk)))

(def-instance bungay english-town
  ((has-name "Bungay")
   (has-county suffolk)))

(def-instance bures english-town
  ((has-name "Bures")
   (has-county suffolk)))

(def-instance bury-st-edmunds english-town
  ((has-name "Bury St. Edmunds")
   (has-county suffolk)))

(def-instance eye english-town
  ((has-name "Eye")
   (has-county suffolk)))

(def-instance felixstowe english-town
  ((has-name "Felixstowe")
   (has-county suffolk)))

(def-instance halesworth english-town
  ((has-name "Halesworth")
   (has-county suffolk)))

(def-instance haverhill english-town
  ((has-name "Haverhill")
   (has-county suffolk)))

(def-instance ipswich english-town
  ((has-name "Ipswich")
   (has-county suffolk)))

(def-instance leiston english-town
  ((has-name "Leiston")
   (has-county suffolk)))

(def-instance lowestoft english-town
  ((has-name "Lowestoft")
   (has-county suffolk)))

(def-instance newmarket english-town
  ((has-name "Newmarket")
   (has-county suffolk)))

(def-instance saxmundham english-town
  ((has-name "Saxmundham")
   (has-county suffolk)))

(def-instance southwold english-town
  ((has-name "Southwold")
   (has-county suffolk)))

(def-instance stowmarket english-town
  ((has-name "Stowmarket")
   (has-county suffolk)))

(def-instance sudbury english-town
  ((has-name "Sudbury")
   (has-county suffolk)))

(def-instance woodbridge english-town
  ((has-name "Woodbridge")
   (has-county suffolk)))

(def-instance addlestone english-town
  ((has-name "Addlestone")
   (has-county surrey)))

(def-instance ashtead english-town
  ((has-name "Ashtead")
   (has-county surrey)))

(def-instance bagshot english-town
  ((has-name "Bagshot")
   (has-county surrey)))

(def-instance banstead english-town
  ((has-name "Banstead")
   (has-county surrey)))

(def-instance betchworth english-town
  ((has-name "Betchworth")
   (has-county surrey)))

(def-instance camberley english-town
  ((has-name "Camberley")
   (has-county surrey)))

(def-instance carshalton english-town
  ((has-name "Carshalton")
   (has-county surrey)))

(def-instance caterham english-town
  ((has-name "Caterham")
   (has-county surrey)))

(def-instance chertsey english-town
  ((has-name "Chertsey")
   (has-county surrey)))

(def-instance chessington english-town
  ((has-name "Chessington")
   (has-county surrey)))

(def-instance cobham english-town
  ((has-name "Cobham")
   (has-county surrey)))

(def-instance coulsdon english-town
  ((has-name "Coulsdon")
   (has-county surrey)))

(def-instance cranleigh english-town
  ((has-name "Cranleigh")
   (has-county surrey)))

(def-instance croydon english-town
  ((has-name "Croydon")
   (has-county surrey)))

(def-instance dorking english-town
  ((has-name "Dorking")
   (has-county surrey)))

(def-instance east-molesey english-town
  ((has-name "East Molesey")
   (has-county surrey)))

(def-instance egham english-town
  ((has-name "Egham")
   (has-county surrey)))

(def-instance epsom english-town
  ((has-name "Epsom")
   (has-county surrey)))

(def-instance esher english-town
  ((has-name "Esher")
   (has-county surrey)))

(def-instance farnham english-town
  ((has-name "Farnham")
   (has-county surrey)))

(def-instance gatwick english-town
  ((has-name "Gatwick")
   (has-county surrey)))

(def-instance godalming english-town
  ((has-name "Godalming")
   (has-county surrey)))

(def-instance godstone english-town
  ((has-name "Godstone")
   (has-county surrey)))

(def-instance guildford english-town
  ((has-name "Guildford")
   (has-county surrey)))

(def-instance haslemere english-town
  ((has-name "Haslemere")
   (has-county surrey)))

(def-instance hindhead english-town
  ((has-name "Hindhead")
   (has-county surrey)))

(def-instance horley english-town
  ((has-name "Horley")
   (has-county surrey)))

(def-instance kenley english-town
  ((has-name "Kenley")
   (has-county surrey)))

(def-instance kingston-upon-thames english-town
  ((has-name "Kingston Upon Thames")
   (has-county surrey)))

(def-instance leatherhead english-town
  ((has-name "Leatherhead")
   (has-county surrey)))

(def-instance lightwater english-town
  ((has-name "Lightwater")
   (has-county surrey)))

(def-instance lingfield english-town
  ((has-name "Lingfield")
   (has-county surrey)))

(def-instance mitcham english-town
  ((has-name "Mitcham")
   (has-county surrey)))

(def-instance morden english-town
  ((has-name "Morden")
   (has-county surrey)))

(def-instance new-malden english-town
  ((has-name "New Malden")
   (has-county surrey)))

(def-instance oxted english-town
  ((has-name "Oxted")
   (has-county surrey)))

(def-instance purley english-town
  ((has-name "Purley")
   (has-county surrey)))

(def-instance redhill english-town
  ((has-name "Redhill")
   (has-county surrey)))

(def-instance reigate english-town
  ((has-name "Reigate")
   (has-county surrey)))

(def-instance richmond-tw english-town
  ((has-name "Richmond Tw")
   (has-county surrey)))

(def-instance south-croydon english-town
  ((has-name "South Croydon")
   (has-county surrey)))

(def-instance surbiton english-town
  ((has-name "Surbiton")
   (has-county surrey)))

(def-instance sutton english-town
  ((has-name "Sutton")
   (has-county surrey)))

(def-instance tadworth english-town
  ((has-name "Tadworth")
   (has-county surrey)))

(def-instance thames-ditton english-town
  ((has-name "Thames Ditton")
   (has-county surrey)))

(def-instance thornton-heath english-town
  ((has-name "Thornton Heath")
   (has-county surrey)))

(def-instance virginia-water english-town
  ((has-name "Virginia Water")
   (has-county surrey)))

(def-instance wallington english-town
  ((has-name "Wallington")
   (has-county surrey)))

(def-instance walton-on-thames english-town
  ((has-name "Walton-On-Thames")
   (has-county surrey)))

(def-instance warlingham english-town
  ((has-name "Warlingham")
   (has-county surrey)))

(def-instance west-byfleet english-town
  ((has-name "West Byfleet")
   (has-county surrey)))

(def-instance west-molesey english-town
  ((has-name "West Molesey")
   (has-county surrey)))

(def-instance weybridge english-town
  ((has-name "Weybridge")
   (has-county surrey)))

(def-instance whyteleafe english-town
  ((has-name "Whyteleafe")
   (has-county surrey)))

(def-instance windlesham english-town
  ((has-name "Windlesham")
   (has-county surrey)))

(def-instance woking english-town
  ((has-name "Woking")
   (has-county surrey)))

(def-instance worcester-park english-town
  ((has-name "Worcester Park")
   (has-county surrey)))

(def-instance arundel english-town
  ((has-name "Arundel")
   (has-county sussex)))

(def-instance battle english-town
  ((has-name "Battle")
   (has-county sussex)))

(def-instance bexhill-on-sea english-town
  ((has-name "Bexhill-On-Sea")
   (has-county sussex)))

(def-instance billingshurst english-town
  ((has-name "Billingshurst")
   (has-county sussex)))

(def-instance bognor-regis english-town
  ((has-name "Bognor Regis")
   (has-county sussex)))

(def-instance brighton english-town
  ((has-name "Brighton")
   (has-county sussex)))

(def-instance burgess-hill english-town
  ((has-name "Burgess Hill")
   (has-county sussex)))

(def-instance chichester english-town
  ((has-name "Chichester")
   (has-county sussex)))

(def-instance crawley english-town
  ((has-name "Crawley")
   (has-county sussex)))

(def-instance crowborough english-town
  ((has-name "Crowborough")
   (has-county sussex)))

(def-instance east-grinstead english-town
  ((has-name "East Grinstead")
   (has-county sussex)))

(def-instance eastbourne english-town
  ((has-name "Eastbourne")
   (has-county sussex)))

(def-instance etchingham english-town
  ((has-name "Etchingham")
   (has-county sussex)))

(def-instance forest-row english-town
  ((has-name "Forest Row")
   (has-county sussex)))

(def-instance hailsham english-town
  ((has-name "Hailsham")
   (has-county sussex)))

(def-instance hartfield english-town
  ((has-name "Hartfield")
   (has-county sussex)))

(def-instance hassocks english-town
  ((has-name "Hassocks")
   (has-county sussex)))

(def-instance hastings english-town
  ((has-name "Hastings")
   (has-county sussex)))

(def-instance haywards-heath english-town
  ((has-name "Haywards Heath")
   (has-county sussex)))

(def-instance heathfield english-town
  ((has-name "Heathfield")
   (has-county sussex)))

(def-instance henfield english-town
  ((has-name "Henfield")
   (has-county sussex)))

(def-instance horsham english-town
  ((has-name "Horsham")
   (has-county sussex)))

(def-instance hove english-town
  ((has-name "Hove")
   (has-county sussex)))

(def-instance lancing english-town
  ((has-name "Lancing")
   (has-county sussex)))

(def-instance lewes english-town
  ((has-name "Lewes")
   (has-county sussex)))

(def-instance littlehampton english-town
  ((has-name "Littlehampton")
   (has-county sussex)))

(def-instance mayfield english-town
  ((has-name "Mayfield")
   (has-county sussex)))

(def-instance midhurst english-town
  ((has-name "Midhurst")
   (has-county sussex)))

(def-instance newhaven english-town
  ((has-name "Newhaven")
   (has-county sussex)))

(def-instance peacehaven english-town
  ((has-name "Peacehaven")
   (has-county sussex)))

(def-instance petworth english-town
  ((has-name "Petworth")
   (has-county sussex)))

(def-instance pevensey english-town
  ((has-name "Pevensey")
   (has-county sussex)))

(def-instance polegate english-town
  ((has-name "Polegate")
   (has-county sussex)))

(def-instance pulborough english-town
  ((has-name "Pulborough")
   (has-county sussex)))

(def-instance robertsbridge english-town
  ((has-name "Robertsbridge")
   (has-county sussex)))

(def-instance rye english-town
  ((has-name "Rye")
   (has-county sussex)))

(def-instance seaford english-town
  ((has-name "Seaford")
   (has-county sussex)))

(def-instance shoreham-by-sea english-town
  ((has-name "Shoreham-By-Sea")
   (has-county sussex)))

(def-instance st-leonards-on-sea english-town
  ((has-name "St. Leonards-On-Sea")
   (has-county sussex)))

(def-instance steyning english-town
  ((has-name "Steyning")
   (has-county sussex)))

(def-instance uckfield english-town
  ((has-name "Uckfield")
   (has-county sussex)))

(def-instance wadhurst english-town
  ((has-name "Wadhurst")
   (has-county sussex)))

(def-instance winchelsea english-town
  ((has-name "Winchelsea")
   (has-county sussex)))

(def-instance worthing english-town
  ((has-name "Worthing")
   (has-county sussex)))

(def-instance brora scottish-town
  ((has-name "Brora")
   (has-county sutherland)))

(def-instance dornoch scottish-town
  ((has-name "Dornoch")
   (has-county sutherland)))

(def-instance forsinard scottish-town
  ((has-name "Forsinard")
   (has-county sutherland)))

(def-instance golspie scottish-town
  ((has-name "Golspie")
   (has-county sutherland)))

(def-instance helmsdale scottish-town
  ((has-name "Helmsdale")
   (has-county sutherland)))

(def-instance kinbrace scottish-town
  ((has-name "Kinbrace")
   (has-county sutherland)))

(def-instance lairg scottish-town
  ((has-name "Lairg")
   (has-county sutherland)))

(def-instance rogart scottish-town
  ((has-name "Rogart")
   (has-county sutherland)))

(def-instance alcester english-town
  ((has-name "Alcester")
   (has-county warwickshire)))

(def-instance atherstone english-town
  ((has-name "Atherstone")
   (has-county warwickshire)))

(def-instance bedworth english-town
  ((has-name "Bedworth")
   (has-county warwickshire)))

(def-instance birmingham english-town
  ((has-name "Birmingham")
   (has-county warwickshire)))

(def-instance coventry english-town
  ((has-name "Coventry")
   (has-county warwickshire)))

(def-instance kenilworth english-town
  ((has-name "Kenilworth")
   (has-county warwickshire)))

(def-instance leamington-spa english-town
  ((has-name "Leamington Spa")
   (has-county warwickshire)))

(def-instance nuneaton english-town
  ((has-name "Nuneaton")
   (has-county warwickshire)))

(def-instance rugby english-town
  ((has-name "Rugby")
   (has-county warwickshire)))

(def-instance shipston-on-stour english-town
  ((has-name "Shipston-On-Stour")
   (has-county warwickshire)))

(def-instance solihull english-town
  ((has-name "Solihull")
   (has-county warwickshire)))

(def-instance southam english-town
  ((has-name "Southam")
   (has-county warwickshire)))

(def-instance stratford-upon-avon english-town
  ((has-name "Stratford-Upon-Avon")
   (has-county warwickshire)))

(def-instance studley english-town
  ((has-name "Studley")
   (has-county warwickshire)))

(def-instance sutton-coldfield english-town
  ((has-name "Sutton Coldfield")
   (has-county warwickshire)))

(def-instance warwick english-town
  ((has-name "Warwick")
   (has-county warwickshire)))

(def-instance bathgate scottish-town
  ((has-name "Bathgate")
   (has-county west-lothian)))

(def-instance boness scottish-town
  ((has-name "Bo'ness")
   (has-county west-lothian)))

(def-instance broxburn scottish-town
  ((has-name "Broxburn")
   (has-county west-lothian)))

(def-instance kirkliston scottish-town
  ((has-name "Kirkliston")
   (has-county west-lothian)))

(def-instance linlithgow scottish-town
  ((has-name "Linlithgow")
   (has-county west-lothian)))

(def-instance livingston scottish-town
  ((has-name "Livingston")
   (has-county west-lothian)))

(def-instance south-queensferry scottish-town
  ((has-name "South Queensferry")
   (has-county west-lothian)))

(def-instance ambleside english-town
  ((has-name "Ambleside")
   (has-county westmorland)))

(def-instance appleby-in-westmorland english-town
  ((has-name "Appleby-In-Westmorland")
   (has-county westmorland)))

(def-instance kendal english-town
  ((has-name "Kendal")
   (has-county westmorland)))

(def-instance kirkby-stephen english-town
  ((has-name "Kirkby Stephen")
   (has-county westmorland)))

(def-instance milnthorpe english-town
  ((has-name "Milnthorpe")
   (has-county westmorland)))

(def-instance windermere english-town
  ((has-name "Windermere")
   (has-county westmorland)))

(def-instance newton-stewart scottish-town
  ((has-name "Newton Stewart")
   (has-county wigtownshire)))

(def-instance stranraer scottish-town
  ((has-name "Stranraer")
   (has-county wigtownshire)))

(def-instance bradford-on-avon english-town
  ((has-name "Bradford-On-Avon")
   (has-county wiltshire)))

(def-instance calne english-town
  ((has-name "Calne")
   (has-county wiltshire)))

(def-instance chippenham english-town
  ((has-name "Chippenham")
   (has-county wiltshire)))

(def-instance corsham english-town
  ((has-name "Corsham")
   (has-county wiltshire)))

(def-instance devizes english-town
  ((has-name "Devizes")
   (has-county wiltshire)))

(def-instance malmesbury english-town
  ((has-name "Malmesbury")
   (has-county wiltshire)))

(def-instance marlborough english-town
  ((has-name "Marlborough")
   (has-county wiltshire)))

(def-instance melksham english-town
  ((has-name "Melksham")
   (has-county wiltshire)))

(def-instance pewsey english-town
  ((has-name "Pewsey")
   (has-county wiltshire)))

(def-instance salisbury english-town
  ((has-name "Salisbury")
   (has-county wiltshire)))

(def-instance swindon english-town
  ((has-name "Swindon")
   (has-county wiltshire)))

(def-instance trowbridge english-town
  ((has-name "Trowbridge")
   (has-county wiltshire)))

(def-instance warminster english-town
  ((has-name "Warminster")
   (has-county wiltshire)))

(def-instance westbury english-town
  ((has-name "Westbury")
   (has-county wiltshire)))

(def-instance bewdley english-town
  ((has-name "Bewdley")
   (has-county worcestershire)))

(def-instance broadway english-town
  ((has-name "Broadway")
   (has-county worcestershire)))

(def-instance bromsgrove english-town
  ((has-name "Bromsgrove")
   (has-county worcestershire)))

(def-instance droitwich english-town
  ((has-name "Droitwich")
   (has-county worcestershire)))

(def-instance dudley english-town
  ((has-name "Dudley")
   (has-county worcestershire)))

(def-instance evesham english-town
  ((has-name "Evesham")
   (has-county worcestershire)))

(def-instance halesowen english-town
  ((has-name "Halesowen")
   (has-county worcestershire)))

(def-instance kidderminster english-town
  ((has-name "Kidderminster")
   (has-county worcestershire)))

(def-instance malvern english-town
  ((has-name "Malvern")
   (has-county worcestershire)))

(def-instance oldbury english-town
  ((has-name "Oldbury")
   (has-county worcestershire)))

(def-instance pershore english-town
  ((has-name "Pershore")
   (has-county worcestershire)))

(def-instance redditch english-town
  ((has-name "Redditch")
   (has-county worcestershire)))

(def-instance stourbridge english-town
  ((has-name "Stourbridge")
   (has-county worcestershire)))

(def-instance stourport-on-severn english-town
  ((has-name "Stourport-On-Severn")
   (has-county worcestershire)))

(def-instance tenbury-wells english-town
  ((has-name "Tenbury Wells")
   (has-county worcestershire)))

(def-instance worcester english-town
  ((has-name "Worcester")
   (has-county worcestershire)))

(def-instance barnoldswick english-town
  ((has-name "Barnoldswick")
   (has-county yorkshire)))

(def-instance barnsley english-town
  ((has-name "Barnsley")
   (has-county yorkshire)))

(def-instance batley english-town
  ((has-name "Batley")
   (has-county yorkshire)))

(def-instance bedale english-town
  ((has-name "Bedale")
   (has-county yorkshire)))

(def-instance beverley english-town
  ((has-name "Beverley")
   (has-county yorkshire)))

(def-instance bingley english-town
  ((has-name "Bingley")
   (has-county yorkshire)))

(def-instance bradford english-town
  ((has-name "Bradford")
   (has-county yorkshire)))

(def-instance bridlington english-town
  ((has-name "Bridlington")
   (has-county yorkshire)))

(def-instance brighouse english-town
  ((has-name "Brighouse")
   (has-county yorkshire)))

(def-instance brough english-town
  ((has-name "Brough")
   (has-county yorkshire)))

(def-instance castleford english-town
  ((has-name "Castleford")
   (has-county yorkshire)))

(def-instance catterick-garrison english-town
  ((has-name "Catterick Garrison")
   (has-county yorkshire)))

(def-instance cleckheaton english-town
  ((has-name "Cleckheaton")
   (has-county yorkshire)))

(def-instance cottingham english-town
  ((has-name "Cottingham")
   (has-county yorkshire)))

(def-instance dewsbury english-town
  ((has-name "Dewsbury")
   (has-county yorkshire)))

(def-instance doncaster english-town
  ((has-name "Doncaster")
   (has-county yorkshire)))

(def-instance driffield english-town
  ((has-name "Driffield")
   (has-county yorkshire)))

(def-instance elland english-town
  ((has-name "Elland")
   (has-county yorkshire)))

(def-instance filey english-town
  ((has-name "Filey")
   (has-county yorkshire)))

(def-instance goole english-town
  ((has-name "Goole")
   (has-county yorkshire)))

(def-instance guisborough english-town
  ((has-name "Guisborough")
   (has-county yorkshire)))

(def-instance halifax english-town
  ((has-name "Halifax")
   (has-county yorkshire)))

(def-instance harrogate english-town
  ((has-name "Harrogate")
   (has-county yorkshire)))

(def-instance hawes english-town
  ((has-name "Hawes")
   (has-county yorkshire)))

(def-instance hebden-bridge english-town
  ((has-name "Hebden Bridge")
   (has-county yorkshire)))

(def-instance heckmondwike english-town
  ((has-name "Heckmondwike")
   (has-county yorkshire)))

(def-instance hessle english-town
  ((has-name "Hessle")
   (has-county yorkshire)))

(def-instance hornsea english-town
  ((has-name "Hornsea")
   (has-county yorkshire)))

(def-instance huddersfield english-town
  ((has-name "Huddersfield")
   (has-county yorkshire)))

(def-instance hull english-town
  ((has-name "Hull")
   (has-county yorkshire)))

(def-instance ilkley english-town
  ((has-name "Ilkley")
   (has-county yorkshire)))

(def-instance keighley english-town
  ((has-name "Keighley")
   (has-county yorkshire)))

(def-instance knaresborough english-town
  ((has-name "Knaresborough")
   (has-county yorkshire)))

(def-instance knottingley english-town
  ((has-name "Knottingley")
   (has-county yorkshire)))

(def-instance leeds english-town
  ((has-name "Leeds")
   (has-county yorkshire)))

(def-instance leyburn english-town
  ((has-name "Leyburn")
   (has-county yorkshire)))

(def-instance liversedge english-town
  ((has-name "Liversedge")
   (has-county yorkshire)))

(def-instance malton english-town
  ((has-name "Malton")
   (has-county yorkshire)))

(def-instance mexborough english-town
  ((has-name "Mexborough")
   (has-county yorkshire)))

(def-instance middlesbrough english-town
  ((has-name "Middlesbrough")
   (has-county yorkshire)))

(def-instance mirfield english-town
  ((has-name "Mirfield")
   (has-county yorkshire)))

(def-instance normanton english-town
  ((has-name "Normanton")
   (has-county yorkshire)))

(def-instance north-ferriby english-town
  ((has-name "North Ferriby")
   (has-county yorkshire)))

(def-instance northallerton english-town
  ((has-name "Northallerton")
   (has-county yorkshire)))

(def-instance ossett english-town
  ((has-name "Ossett")
   (has-county yorkshire)))

(def-instance otley english-town
  ((has-name "Otley")
   (has-county yorkshire)))

(def-instance pickering english-town
  ((has-name "Pickering")
   (has-county yorkshire)))

(def-instance pontefract english-town
  ((has-name "Pontefract")
   (has-county yorkshire)))

(def-instance pudsey english-town
  ((has-name "Pudsey")
   (has-county yorkshire)))

(def-instance redcar english-town
  ((has-name "Redcar")
   (has-county yorkshire)))

(def-instance richmond-dl english-town
  ((has-name "Richmond Dl")
   (has-county yorkshire)))

(def-instance ripon english-town
  ((has-name "Ripon")
   (has-county yorkshire)))

(def-instance rotherham english-town
  ((has-name "Rotherham")
   (has-county yorkshire)))

(def-instance saltburn-by-the-sea english-town
  ((has-name "Saltburn-By-The-Sea")
   (has-county yorkshire)))

(def-instance scarborough english-town
  ((has-name "Scarborough")
   (has-county yorkshire)))

(def-instance sedbergh english-town
  ((has-name "Sedbergh")
   (has-county yorkshire)))

(def-instance selby english-town
  ((has-name "Selby")
   (has-county yorkshire)))

(def-instance settle english-town
  ((has-name "Settle")
   (has-county yorkshire)))

(def-instance sheffield english-town
  ((has-name "Sheffield")
   (has-county yorkshire)))

(def-instance shipley english-town
  ((has-name "Shipley")
   (has-county yorkshire)))

(def-instance skipton english-town
  ((has-name "Skipton")
   (has-county yorkshire)))

(def-instance sowerby-bridge english-town
  ((has-name "Sowerby Bridge")
   (has-county yorkshire)))

(def-instance tadcaster english-town
  ((has-name "Tadcaster")
   (has-county yorkshire)))

(def-instance thirsk english-town
  ((has-name "Thirsk")
   (has-county yorkshire)))

(def-instance wakefield english-town
  ((has-name "Wakefield")
   (has-county yorkshire)))

(def-instance wetherby english-town
  ((has-name "Wetherby")
   (has-county yorkshire)))

(def-instance whitby english-town
  ((has-name "Whitby")
   (has-county yorkshire)))

(def-instance withernsea english-town
  ((has-name "Withernsea")
   (has-county yorkshire)))

(def-instance yarm english-town
  ((has-name "Yarm")
   (has-county yorkshire)))

(def-instance york english-town
  ((has-name "York")
   (has-county yorkshire)))

(def-instance aberdeenshire scottish-county
  ((has-name "Aberdeenshire")
   (has-alternative-name "")
   (has-town aberdeen aboyne alford ballater ellon fraserburgh huntly insch inverurie milltimber peterculter peterhead strathdon turriff westhill)))

(def-instance anglesey welsh-county
  ((has-name "Anglesey")
   (has-alternative-name "Sir Fon")
   (has-town amlwch beaumaris bodorgan brynteg cemaes-bay dulas gaerwen holyhead llanbedrgoch llanerchymedd llanfairpwllgwyngyll llangefni marianglas menai-bridge moelfre pentraeth penysarn rhosgoch rhosneigr ty-croes tyn-y-gongl)))

(def-instance angus scottish-county
  ((has-name "Angus")
   (has-alternative-name "")
   (has-town arbroath brechin carnoustie dundee forfar kirriemuir montrose)))

(def-instance argyllshire scottish-county
  ((has-name "Argyllshire")
   (has-alternative-name "")
   (has-town acharacle appin ballachulish bridge-of-orchy cairndow campbeltown colintraive dalmally dunoon inveraray isle-of-coll isle-of-colonsay isle-of-gigha isle-of-iona isle-of-mull kinlochleven lochgilphead oban tarbert taynuilt tighnabruaich)))

(def-instance ayrshire scottish-county
  ((has-name "Ayrshire")
   (has-alternative-name "")
   (has-town ardrossan ayr beith cumnock dalry darvel galston girvan irvine kilbirnie kilmarnock kilwinning largs mauchline maybole newmilns prestwick saltcoats skelmorlie stevenston troon west-kilbride)))

(def-instance banffshire scottish-county
  ((has-name "Banffshire")
   (has-alternative-name "")
   (has-town aberlour ballindalloch banff buckie keith macduff)))

(def-instance bedfordshire english-county
  ((has-name "bedfordshire")
   (has-alternative-name "")
   (has-town arlesey bedford biggleswade dunstable henlow leighton-buzzard luton sandy shefford)))

(def-instance berkshire english-county
  ((has-name "Berkshire")
   (has-alternative-name "")
   (has-town abingdon ascot bracknell crowthorne didcot faringdon hungerford maidenhead newbury reading sandhurst thatcham wallingford wantage windsor wokingham)))

(def-instance berwickshire scottish-county
  ((has-name "Berwickshire")
   (has-alternative-name "")
   (has-town cockburnspath coldstream duns earlston eyemouth gordon lauder)))

(def-instance brecknockshire welsh-county
  ((has-name "Brecknockshire")
   (has-alternative-name "Sir Frycheiniog")
   (has-town brecon crickhowell llangammarch-wells llanwrtyd-wells)))

(def-instance buckinghamshire english-county
  ((has-name "Buckinghamshire")
   (has-alternative-name "")
   (has-town amersham aylesbury beaconsfield bourne-end buckingham chalfont-st-giles chesham gerrards-cross great-missenden high-wycombe iver marlow milton-keynes newport-pagnell olney princes-risborough slough)))

(def-instance caernarfonshire welsh-county
  ((has-name "Caernarfonshire")
   (has-alternative-name "Sir Gaernarfon")
   (has-town bangor-ll betws-y-coed caernarfon conwy criccieth dolwyddelan garndolbenmaen llandudno llandudno-junction llanfairfechan penmaenmawr porthmadog pwllheli trefriw y-felinheli)))

(def-instance caithness scottish-county
  ((has-name "Caithness")
   (has-alternative-name "")
   (has-town berriedale dunbeath halkirk latheron lybster thurso wick)))

(def-instance cambridgeshire english-county
  ((has-name "Cambridgeshire")
   (has-alternative-name "")
   (has-town cambridge chatteris ely march wisbech)))

(def-instance cardiganshire welsh-county
  ((has-name "Cardiganshire")
   (has-alternative-name "Ceredigion")
   (has-town aberaeron aberystwyth borth bow-street cardigan lampeter llanarth llandysul llanon llanrhystud new-quay talybont-sy tregaron ystrad-meurig)))

(def-instance carmarthenshire welsh-county
  ((has-name "Carmarthenshire")
   (has-alternative-name "Sir Gaerfyrddin")
   (has-town ammanford burry-port carmarthen clynderwen ferryside kidwelly llandeilo llandovery llanelli llangadog llanwrda llanybydder newcastle-emlyn pencader whitland)))

(def-instance channel-isles english-county
  ((has-name "Channel Isles")
   (has-alternative-name "")
   (has-town guernsey jersey)))

(def-instance cheshire english-county
  ((has-name "Cheshire")
   (has-alternative-name "")
   (has-town alderley-edge altrincham birkenhead cheadle chester congleton crewe dukinfield ellesmere-port frodsham hyde knutsford lymm macclesfield malpas middlewich nantwich neston northwich prenton runcorn sale sandbach stalybridge stockport tarporley wallasey wilmslow winsford wirral)))

(def-instance clackmannanshire scottish-county
  ((has-name "Clackmannanshire")
   (has-alternative-name "")
   (has-town alloa alva clackmannan dollar menstrie tillicoultry)))

(def-instance cornwall english-county
  ((has-name "Cornwall")
   (has-alternative-name "")
   (has-town bodmin boscastle bude callington calstock camborne camelford delabole falmouth fowey gunnislake hayle helston launceston liskeard looe lostwithiel marazion newquay padstow par penryn penzance perranporth port-isaac redruth saltash st-agnes-tr5 st-austell st-columb st-ives tintagel torpoint truro wadebridge)))

(def-instance county-antrim irish-county
  ((has-name "County Antrim")
   (has-alternative-name "")
   (has-town antrim ballycastle ballyclare ballymena ballymoney belfast bushmills carrickfergus crumlin larne lisburn newtownabbey portrush)))

(def-instance county-armagh irish-county
  ((has-name "County Armagh")
   (has-alternative-name "")
   (has-town armagh craigavon)))

(def-instance county-down irish-county
  ((has-name "County Down")
   (has-alternative-name "")
   (has-town ballynahinch banbridge bangor-bt castlewellan donaghadee downpatrick dromore hillsborough holywood newcastle-bt newry newtownards)))

(def-instance county-durham english-county
  ((has-name "County Durham")
   (has-alternative-name "")
   (has-town barnard-castle billingham bishop-auckland blaydon-on-tyne boldon-colliery chester-le-street consett crook darlington durham east-boldon ferryhill gateshead hartlepool hebburn houghton-le-spring jarrow newton-aycliffe peterlee rowlands-gill ryton seaham shildon south-shields spennymoor stanley stockton-on-tees sunderland trimdon-station washington wingate)))

(def-instance county-fermanagh irish-county
  ((has-name "County Fermanagh")
   (has-alternative-name "")
   (has-town enniskillen)))

(def-instance county-londonderry irish-county
  ((has-name "County Londonderry")
   (has-alternative-name "")
   (has-town coleraine limavady londonderry maghera magherafelt portstewart)))

(def-instance county-tyrone irish-county
  ((has-name "County Tyrone")
   (has-alternative-name "")
   (has-town augher aughnacloy caledon castlederg clogher cookstown dungannon fivemiletown omagh strabane)))

(def-instance cumberland english-county
  ((has-name "Cumberland")
   (has-alternative-name "")
   (has-town alston beckermet brampton carlisle cleator cleator-moor cockermouth egremont frizington holmrook keswick maryport millom moor-row penrith ravenglass seascale st-bees whitehaven wigton workington)))

(def-instance denbighshire welsh-county
  ((has-name "Denbighshire")
   (has-alternative-name "Sir Ddinbych")
   (has-town abergele colwyn-bay denbigh llangollen llanrwst ruthin wrexham)))

(def-instance derbyshire english-county
  ((has-name "Derbyshire")
   (has-alternative-name "")
   (has-town alfreton ashbourne bakewell belper buxton chesterfield derby dronfield glossop heanor high-peak hope-valley ilkeston matlock ripley swadlincote)))

(def-instance devon english-county
  ((has-name "Devon")
   (has-alternative-name "")
   (has-town axminster barnstaple beaworthy bideford braunton brixham buckfastleigh budleigh-salterton chulmleigh colyton crediton cullompton dartmouth dawlish exeter exmouth holsworthy honiton ilfracombe ivybridge kingsbridge lifton lynmouth lynton newton-abbot north-tawton okehampton ottery-st-mary paignton plymouth salcombe seaton sidmouth south-brent south-molton tavistock teignmouth tiverton torquay torrington totnes umberleigh winkleigh woolacombe yelverton)))

(def-instance dorset english-county
  ((has-name "Dorset")
   (has-alternative-name "")
   (has-town beaminster blandford-forum bridport broadstone dorchester ferndown gillingham-sp lyme-regis poole portland shaftesbury sherborne sturminster-newton swanage verwood wareham weymouth wimborne)))

(def-instance dumfriesshire scottish-county
  ((has-name "Dumfriesshire")
   (has-alternative-name "")
   (has-town annan canonbie dumfries gretna langholm lockerbie moffat sanquhar thornhill)))

(def-instance dunbartonshire scottish-county
  ((has-name "Dunbartonshire")
   (has-alternative-name "")
   (has-town alexandria arrochar clydebank dumbarton helensburgh)))

(def-instance east-lothian scottish-county
  ((has-name "East Lothian")
   (has-alternative-name "")
   (has-town dunbar east-linton gullane haddington humbie longniddry north-berwick prestonpans tranent)))

(def-instance essex english-county
  ((has-name "Essex")
   (has-alternative-name "")
   (has-town barking basildon benfleet billericay braintree brentwood buckhurst-hill burnham-on-crouch canvey-island chelmsford chigwell clacton-on-sea colchester dagenham dunmow epping frinton-on-sea grays halstead harlow harwich hockley hornchurch ilford ingatestone leigh-on-sea loughton maldon manningtree ongar purfleet rainham rayleigh rochford romford saffron-walden south-ockendon southend-on-sea southminster stanford-le-hope stansted tilbury upminster waltham-abbey walton-on-the-naze westcliff-on-sea wickford witham woodford-green)))

(def-instance fife scottish-county
  ((has-name "Fife")
   (has-alternative-name "")
   (has-town anstruther burntisland cowdenbeath cupar dunfermline glenrothes inverkeithing kelty kirkcaldy leven lochgelly newport-on-tay st-andrews tayport)))

(def-instance flintshire welsh-county
  ((has-name "Flintshire")
   (has-alternative-name "Sir Y Fflint")
   (has-town bagillt buckley deeside flint holywell mold prestatyn rhyl st-asaph)))

(def-instance glamorgan welsh-county
  ((has-name "Glamorgan")
   (has-alternative-name "Morgannwg")
   (has-town aberdare bargoed barry bridgend-cf caerphilly cardiff cowbridge dinas-powys ferndale hengoed llantwit-major maesteg merthyr-tydfil mountain-ash neath penarth pentre pontyclun pontypridd port-talbot porth porthcawl swansea tonypandy treharris treorchy)))

(def-instance gloucestershire english-county
  ((has-name "Gloucestershire")
   (has-alternative-name "")
   (has-town badminton berkeley blakeney bristol cheltenham chipping-campden cinderford cirencester coleford drybrook dursley dymock fairford gloucester lechlade longhope lydbrook lydney mitcheldean moreton-in-marsh newent newnham ruardean stonehouse stroud tetbury tewkesbury westbury-on-severn wotton-under-edge)))

(def-instance hampshire english-county
  ((has-name "Hampshire")
   (has-alternative-name "")
   (has-town aldershot alresford alton andover basingstoke bordon bournemouth brockenhurst christchurch eastleigh emsworth fareham farnborough fleet fordingbridge gosport havant hayling-island hook lee-on-the-solent liphook liss lymington lyndhurst new-milton petersfield portsmouth ringwood romsey rowlands-castle southampton southsea stockbridge tadley tidworth waterlooville whitchurch-rg winchester yateley)))

(def-instance herefordshire english-county
  ((has-name "Herefordshire")
   (has-alternative-name "")
   (has-town bromyard hereford kington ledbury leominster ross-on-wye)))

(def-instance hertfordshire english-county
  ((has-name "Hertfordshire")
   (has-alternative-name "")
   (has-town abbots-langley baldock barnet berkhamsted bishops-stortford borehamwood broxbourne buntingford harpenden hatfield hemel-hempstead hertford hitchin hoddesdon kings-langley knebworth letchworth much-hadham radlett rickmansworth royston sawbridgeworth st-albans stevenage tring waltham-cross ware watford welwyn welwyn-garden-city)))

(def-instance huntingdonshire english-county
  ((has-name "Huntingdonshire")
   (has-alternative-name "")
   (has-town huntingdon)))

(def-instance inverness-shire scottish-county
  ((has-name "Inverness-Shire")
   (has-alternative-name "")
   (has-town arisaig aviemore beauly boat-of-garten carrbridge corrour dalwhinnie fort-augustus fort-william glenfinnan invergarry inverness isle-of-benbecula isle-of-canna isle-of-eigg isle-of-rhum isle-of-scalpay isle-of-skye kingussie lochailort mallaig nethy-bridge newtonmore roy-bridge spean-bridge)))

(def-instance isle-of-arran scottish-county
  ((has-name "Isle Of Arran")
   (has-alternative-name "Buteshire")
   (has-town brodick)))

(def-instance isle-of-barra scottish-county
  ((has-name "Isle Of Barra")
   (has-alternative-name "Inverness-Shire")
   (has-town castlebay)))

(def-instance isle-of-bute scottish-county
  ((has-name "Isle Of Bute")
   (has-alternative-name "Buteshire")
   (has-town rothesay)))

(def-instance isle-of-cumbrae scottish-county
  ((has-name "Isle Of Cumbrae")
   (has-alternative-name "Buteshire")
   (has-town millport)))

(def-instance isle-of-harris scottish-county
  ((has-name "Isle Of Harris")
   (has-alternative-name "Inverness-Shire")
   (has-town harris leverburgh)))

(def-instance isle-of-islay scottish-county
  ((has-name "Isle Of Islay")
   (has-alternative-name "Arygyllshire")
   (has-town ballygrant bowmore bridgend-pa bruichladdich port-askaig port-charlotte port-ellen portnahaven)))

(def-instance isle-of-jura scottish-county
  ((has-name "Isle Of Jura")
   (has-alternative-name "Argyllshire")
   (has-town craighouse)))

(def-instance isle-of-lewis scottish-county
  ((has-name "Isle Of Lewis")
   (has-alternative-name "Ross-Shire")
   (has-town stornoway)))

(def-instance isle-of-man english-county
  ((has-name "Isle Of Man")
   (has-alternative-name "")
   (has-town ballasalla castletown douglas isle-of-man kirk-michael laxey peel port-erin port-st-mary ramsey)))

(def-instance isle-of-north-uist scottish-county
  ((has-name "Isle Of North Uist")
   (has-alternative-name "Inverness-Shire")
   (has-town lochmaddy)))

(def-instance isle-of-skye scottish-county
  ((has-name "Isle Of Skye")
   (has-alternative-name "Inverness-Shire")
   (has-town portree)))

(def-instance isle-of-south-uist scottish-county
  ((has-name "Isle Of South Uist")
   (has-alternative-name "Inverness-Shire")
   (has-town lochboisdale)))

(def-instance isle-of-tiree scottish-county
  ((has-name "Isle Of Tiree")
   (has-alternative-name "Argyllshire")
   (has-town scarinish)))

(def-instance isle-of-wight english-county
  ((has-name "Isle Of Wight")
   (has-alternative-name "Hampshire")
   (has-town bembridge cowes east-cowes freshwater newport-po ryde sandown seaview shanklin totland-bay ventnor yarmouth)))

(def-instance isles-of-scilly english-county
  ((has-name "Isles Of Scilly")
   (has-alternative-name "")
   (has-town bryher st-agnes-tr22 st-martins st-marys tresco)))

(def-instance kent english-county
  ((has-name "Kent")
   (has-alternative-name "")
   (has-town ashford-tn aylesford beckenham belvedere bexley bexleyheath birchington broadstairs bromley canterbury chatham chislehurst cranbrook dartford deal dover edenbridge erith faversham folkestone gillingham-me gravesend greenhithe herne-bay hythe keston longfield maidstone margate new-romney orpington queenborough ramsgate rochester romney-marsh sandwich sevenoaks sheerness sidcup sittingbourne snodland swanley swanscombe tenterden tonbridge tunbridge-wells welling west-malling west-wickham westerham westgate-on-sea whitstable)))

(def-instance kincardineshire scottish-county
  ((has-name "Kincardineshire")
   (has-alternative-name "")
   (has-town banchory laurencekirk stonehaven)))

(def-instance kinross-shire scottish-county
  ((has-name "Kinross-Shire")
   (has-alternative-name "")
   (has-town kinross)))

(def-instance kirkcudbrightshire scottish-county
  ((has-name "Kirkcudbrightshire")
   (has-alternative-name "")
   (has-town castle-douglas dalbeattie kirkcudbright)))

(def-instance lanarkshire scottish-county
  ((has-name "Lanarkshire")
   (has-alternative-name "")
   (has-town airdrie bellshill biggar carluke coatbridge glasgow hamilton lanark larkhall motherwell shotts strathaven wishaw)))

(def-instance lancashire english-county
  ((has-name "Lancashire")
   (has-alternative-name "")
   (has-town accrington ashton-under-lyne askam-in-furness bacup barrow-in-furness blackburn blackpool bolton bootle broughton-in-furness burnley bury carnforth chorley clitheroe colne coniston dalton-in-furness darwen fleetwood grange-over-sands heywood kirkby-in-furness lancaster leigh littleborough liverpool lytham-st-annes manchester morecambe nelson newton-le-willows oldham ormskirk poulton-le-fylde prescot preston rochdale rossendale salford skelmersdale southport st-helens thornton-cleveleys todmorden ulverston warrington widnes wigan)))

(def-instance leicestershire english-county
  ((has-name "Leicestershire")
   (has-alternative-name "")
   (has-town ashby-de-la-zouch coalville hinckley ibstock leicester loughborough lutterworth market-harborough markfield melton-mowbray wigston)))

(def-instance lincolnshire english-county
  ((has-name "Lincolnshire")
   (has-alternative-name "")
   (has-town barnetby barrow-upon-humber barton-upon-humber boston bourne brigg cleethorpes gainsborough grantham grimsby horncastle immingham lincoln louth mablethorpe market-rasen scunthorpe skegness sleaford spalding spilsby stamford ulceby woodhall-spa)))

(def-instance merioneth english-county
  ((has-name "Merioneth")
   (has-alternative-name "")
   (has-town aberdovey arthog bala barmouth blaenau-ffestiniog corwen dolgellau dyffryn-ardudwy fairbourne harlech llanbedr llwyngwril penrhyndeudraeth talsarnau talybont-ll tywyn)))

(def-instance middlesex welsh-county
  ((has-name "Middlesex")
   (has-alternative-name "Meirionnydd")
   (has-town ashford-tw brentford edgware enfield feltham greenford hampton harrow hayes hounslow isleworth)))

(def-instance london english-county
  ((has-name "London")
   (has-alternative-name "")
   (has-town london)))

(def-instance middlesex english-county
  ((has-name "Middlesex")
   (has-alternative-name "")
   (has-town northolt northwood pinner potters-bar ruislip shepperton southall staines stanmore sunbury-on-thames teddington twickenham uxbridge wembley west-drayton)))

(def-instance midlothian scottish-county
  ((has-name "Midlothian")
   (has-alternative-name "")
   (has-town balerno bonnyrigg currie dalkeith edinburgh gorebridge heriot juniper-green kirknewton lasswade loanhead musselburgh newbridge pathhead penicuik rosewell roslin west-calder)))

(def-instance monmouthshire welsh-county
  ((has-name "Monmouthshire")
   (has-alternative-name "Sir Fynwy")
   (has-town abergavenny abertillery blackwood caldicot chepstow cwmbran ebbw-vale monmouth new-tredegar newport-np pontypool tredegar usk)))

(def-instance montgomeryshire welsh-county
  ((has-name "Montgomeryshire")
   (has-alternative-name "Sir Drefaldwyn")
   (has-town caersws llanbrynmair llandinam llanfechain llanfyllin llanidloes llansantffraid llanymynech machynlleth meifod montgomery newtown welshpool)))

(def-instance morayshire scottish-county
  ((has-name "Morayshire")
   (has-alternative-name "")
   (has-town elgin fochabers forres grantown-on-spey lossiemouth)))

(def-instance nairnshire scottish-county
  ((has-name "Nairnshire")
   (has-alternative-name "")
   (has-town nairn)))

(def-instance norfolk english-county
  ((has-name "Norfolk")
   (has-alternative-name "")
   (has-town attleborough cromer dereham diss downham-market fakenham great-yarmouth harleston holt hunstanton kings-lynn melton-constable north-walsham norwich sandringham sheringham swaffham thetford walsingham wells-next-the-sea wymondham)))

(def-instance northamptonshire english-county
  ((has-name "Northamptonshire")
   (has-alternative-name "")
   (has-town brackley corby daventry kettering northampton peterborough rushden towcester wellingborough)))

(def-instance northumberland english-county
  ((has-name "Northumberland")
   (has-alternative-name "")
   (has-town alnwick ashington bamburgh bedlington belford berwick-upon-tweed blyth chathill choppington corbridge cornhill-on-tweed cramlington haltwhistle hexham mindrum morpeth newbiggin-by-the-sea newcastle-upon-tyne north-shields prudhoe riding-mill seahouses stocksfield wallsend whitley-bay wooler wylam)))

(def-instance nottinghamshire english-county
  ((has-name "Nottinghamshire")
   (has-alternative-name "")
   (has-town mansfield newark nottingham retford southwell sutton-in-ashfield worksop)))

(def-instance orkney scottish-county
  ((has-name "Orkney")
   (has-alternative-name "")
   (has-town kirkwall orkney stromness)))

(def-instance oxfordshire english-county
  ((has-name "Oxfordshire")
   (has-alternative-name "")
   (has-town bampton banbury bicester burford carterton chinnor chipping-norton henley-on-thames kidlington oxford thame watlington witney woodstock)))

(def-instance peeblesshire scottish-county
  ((has-name "Peeblesshire")
   (has-alternative-name "")
   (has-town innerleithen peebles walkerburn west-linton)))

(def-instance pembrokeshire welsh-county
  ((has-name "Pembrokeshire")
   (has-alternative-name "Sir Benfro")
   (has-town boncath clarbeston-road crymych fishguard glogue goodwick haverfordwest kilgetty llanfyrnach milford-haven narberth newport-sa pembroke pembroke-dock saundersfoot tenby)))

(def-instance perthshire scottish-county
  ((has-name "Perthshire")
   (has-alternative-name "")
   (has-town aberfeldy auchterarder blairgowrie callander crianlarich crieff doune dunblane dunkeld killin lochearnhead perth pitlochry)))

(def-instance radnorshire welsh-county
  ((has-name "Radnorshire")
   (has-alternative-name "Sir Faesyfed")
   (has-town builth-wells knighton llandrindod-wells presteigne rhayader)))

(def-instance renfrewshire scottish-county
  ((has-name "Renfrewshire")
   (has-alternative-name "")
   (has-town bishopton bridge-of-weir erskine gourock greenock johnstone kilmacolm lochwinnoch paisley port-glasgow renfrew wemyss-bay)))

(def-instance ross-shire scottish-county
  ((has-name "Ross-Shire")
   (has-alternative-name "")
   (has-town achnasheen alness ardgay avoch cromarty dingwall fortrose gairloch garve invergordon isle-of-lewis kyle muir-of-ord munlochy plockton strathcarron strathpeffer strome-ferry tain ullapool)))

(def-instance roxburghshire scottish-county
  ((has-name "Roxburghshire")
   (has-alternative-name "")
   (has-town hawick jedburgh kelso melrose newcastleton)))

(def-instance rutland english-county
  ((has-name "Rutland")
   (has-alternative-name "")
   (has-town oakham)))

(def-instance selkirkshire scottish-county
  ((has-name "Selkirkshire")
   (has-alternative-name "")
   (has-town galashiels selkirk)))

(def-instance shetland scottish-county
  ((has-name "Shetland")
   (has-alternative-name "")
   (has-town lerwick shetland)))

(def-instance shropshire english-county
  ((has-name "Shropshire")
   (has-alternative-name "")
   (has-town bishops-castle bridgnorth broseley bucknell church-stretton craven-arms ellesmere ludlow lydbury-north market-drayton much-wenlock newport-tf oswestry shifnal shrewsbury telford whitchurch-sy)))

(def-instance somerset english-county
  ((has-name "Somerset")
   (has-alternative-name "")
   (has-town axbridge banwell bath bridgwater bruton burnham-on-sea castle-cary chard cheddar clevedon crewkerne dulverton frome glastonbury highbridge hinton-st-george ilminster langport martock merriott minehead montacute shepton-mallet somerton south-petherton stoke-sub-hamdon street taunton templecombe watchet wedmore wellington wells weston-super-mare wincanton winscombe yeovil)))

(def-instance staffordshire english-county
  ((has-name "Staffordshire")
   (has-alternative-name "")
   (has-town bilston brierley-hill burntwood burton-on-trent cannock cradley-heath kingswinford leek lichfield newcastle-st rowley-regis rugeley smethwick stafford stoke-on-trent stone tamworth tipton uttoxeter walsall wednesbury west-bromwich willenhall wolverhampton)))

(def-instance stirlingshire scottish-county
  ((has-name "Stirlingshire")
   (has-alternative-name "")
   (has-town bonnybridge denny falkirk grangemouth larbert stirling)))

(def-instance suffolk english-county
  ((has-name "Suffolk")
   (has-alternative-name "")
   (has-town aldeburgh beccles brandon bungay bures bury-st-edmunds eye felixstowe halesworth haverhill ipswich leiston lowestoft newmarket saxmundham southwold stowmarket sudbury woodbridge)))

(def-instance surrey english-county
  ((has-name "Surrey")
   (has-alternative-name "")
   (has-town addlestone ashtead bagshot banstead betchworth camberley carshalton caterham chertsey chessington cobham coulsdon cranleigh croydon dorking east-molesey egham epsom esher farnham gatwick godalming godstone guildford haslemere hindhead horley kenley kingston-upon-thames leatherhead lightwater lingfield mitcham morden new-malden oxted purley redhill reigate richmond-tw south-croydon surbiton sutton tadworth thames-ditton thornton-heath virginia-water wallington walton-on-thames warlingham west-byfleet west-molesey weybridge whyteleafe windlesham woking worcester-park)))

(def-instance sussex english-county
  ((has-name "Sussex")
   (has-alternative-name "")
   (has-town arundel battle bexhill-on-sea billingshurst bognor-regis brighton burgess-hill chichester crawley crowborough east-grinstead eastbourne etchingham forest-row hailsham hartfield hassocks hastings haywards-heath heathfield henfield horsham hove lancing lewes littlehampton mayfield midhurst newhaven peacehaven petworth pevensey polegate pulborough robertsbridge rye seaford shoreham-by-sea st-leonards-on-sea steyning uckfield wadhurst winchelsea worthing)))

(def-instance sutherland scottish-county
  ((has-name "Sutherland")
   (has-alternative-name "")
   (has-town brora dornoch forsinard golspie helmsdale kinbrace lairg rogart)))

(def-instance warwickshire english-county
  ((has-name "Warwickshire")
   (has-alternative-name "")
   (has-town alcester atherstone bedworth birmingham coventry kenilworth leamington-spa nuneaton rugby shipston-on-stour solihull southam stratford-upon-avon studley sutton-coldfield warwick)))

(def-instance west-lothian scottish-county
  ((has-name "West Lothian")
   (has-alternative-name "")
   (has-town bathgate boness broxburn kirkliston linlithgow livingston south-queensferry)))

(def-instance westmorland english-county
  ((has-name "Westmorland")
   (has-alternative-name "")
   (has-town ambleside appleby-in-westmorland kendal kirkby-stephen milnthorpe windermere)))

(def-instance wigtownshire scottish-county
  ((has-name "Wigtownshire")
   (has-alternative-name "")
   (has-town newton-stewart stranraer)))

(def-instance wiltshire english-county
  ((has-name "Wiltshire")
   (has-alternative-name "")
   (has-town bradford-on-avon calne chippenham corsham devizes malmesbury marlborough melksham pewsey salisbury swindon trowbridge warminster westbury)))

(def-instance worcestershire english-county
  ((has-name "Worcestershire")
   (has-alternative-name "")
   (has-town bewdley broadway bromsgrove droitwich dudley evesham halesowen kidderminster malvern oldbury pershore redditch stourbridge stourport-on-severn tenbury-wells worcester)))

(def-instance yorkshire english-county
  ((has-name "Yorkshire")
   (has-alternative-name "")
   (has-town barnoldswick barnsley batley bedale beverley bingley bradford bridlington brighouse brough castleford catterick-garrison cleckheaton cottingham dewsbury doncaster driffield elland filey goole guisborough halifax harrogate hawes hebden-bridge heckmondwike hessle hornsea huddersfield hull ilkley keighley knaresborough knottingley leeds leyburn liversedge malton mexborough middlesbrough mirfield normanton north-ferriby northallerton ossett otley pickering pontefract pudsey redcar richmond-dl ripon rotherham saltburn-by-the-sea scarborough sedbergh selby settle sheffield shipley skipton sowerby-bridge tadcaster thirsk wakefield wetherby whitby withernsea yarm york)))



(def-class uk-address (postal-address)
  ((address-country :value united-kingdom)
   (address-county :type uk-county)
   (address-city-or-village :type uk-town)))
