;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology luisa-first-prototype-ontology)

;;; map to OCI-object not to english-provider-object!
;;; create more instances
;;; create actor instances and develop login service

(def-class method ())

(def-class language ())

(def-class lang-string ()
((has-lang :type language)
 (has-string :type string))
)

(def-class repository ()
((has-content-language :type language)
 (has-domain :type string))
 (has-content-keywords :type string)
)



(def-class geturl-request ()
((has-request :type string)))

(def-class competency-spec ()
((title :type string))
)

(def-class competency-list (list) ?x
:constraint (exists ?e (and (element-of ?e ?x)
                            (competency-spec ?e))))

(def-class competencies ()
 ((has-competencies :type competency-list)))

(def-class competency-requirements ()
 ((has-competencies :type competency-list)))

(def-class learning-object ()
((lom-title :type lang-string :max-cardinality 1 :min-cardinality 1)
 (lom-version :type lang-string :max-cardinality 1 :min-cardinality 1)
 (lom-competency :type competency-spec :max-cardinality 1 :min-cardinality 1)
 (lom-url :type string))
)

(def-class learning-object-multiref ()
((lom-title :type lang-string :max-cardinality 1 :min-cardinality 1)
 (lom-version :type lang-string :max-cardinality 1 :min-cardinality 1)
 (lom-competency :type competency-spec :max-cardinality 1 :min-cardinality 1)
 (lom-url :type string))
)

(def-class learning-object-ref ()
((lo-ref-title :type lang-string :max-cardinality 1 :min-cardinality 1)
 (lo-ref-version :type lang-string :max-cardinality 1 :min-cardinality 1)
 (lo-ref-competency :type competency-spec :max-cardinality 1 :min-cardinality 1)
 (lo-ref-url :type string))
)

(def-class get-lo-by-competency-request ()
((has-competency-request :type string))
)

(def-class get-lo-by-field-request ()
((has-field-request :type string))
)

(def-class get-lo-by-uhp-field-request ()
((has-uhp-field-request :type string))
)

(def-class get-lo-by-competency-request2 ()
((has-competency-request :type string))
)

(def-class get-lo-by-field-request2 ()
((has-field-request :type string))
)

(def-class get-lo-by-lang-request ()
((has-lang-request :type string))
)

(def-class get-lo-by-interact-request ()
((has-interact-request :type string))
)

(def-class get-lo-by-lang-request ()
((has-lang-request :type string))
)

(def-class get-lo-by-competency-response ()
((has-learning-object :type string))
)

#|
(deflower lower-competency-request get-lo-by-competency-request 
  (lambda (instance)
    (findany '?x `(has-competency-request ,instance ?x))))
(describe-instance 'p)

|#

(deflower lower-competency-request get-lo-by-competency-request 
  (lambda (string)
    (iu:replace-all string " " "%20")))

(deflower lower-field-request get-lo-by-field-request 
  (lambda (string)
    (iu:replace-all string " " "%20")))

(deflower lower-scorm-objective scorm-objective
(lambda (string) 
  (let ((inst nil))
     (setf inst (findany '?x `(and (instance-of ?x 'lpmo-objective)
                                   (has-scorm-objective ?x ,string))))
     (setf out (the-slot-value inst 'has-objective-title)))))

(deflower lower-imsld-objective imsld-objective
(lambda (string) 
  (let ((inst nil))
     (setf inst (findany '?z `(and (instance-of ?z 'lpmo-objective)
                                   (has-imsld-objective ?z ,string))))
     (setf out (the-slot-value inst 'has-objective-title)))))
#|
(deflower lower-scorm-objective scorm-objective
(lambda (string) 
  (let ((inst nil))
     (setf inst (findany '?x `(and (instance-of ?x 'scorm-objective)
                                   (has-scorm-objective-title ?x ,string))))
     (setf out (the-slot-value inst 'has-lpmo-objective)))))
|#

(defun scorm-objective-mapping (string)
(let ((inst nil))
     (setf inst (findany '?x `(and (instance-of ?x 'scorm-objective)
                                   (has-scorm-objective-title ?x ,string))))
     (setf out (the-slot-value inst 'has-lpmo-objective))))

(def-class objective ())

(def-class scorm-objective ()
((has-scorm-objective-id :type integer)
 (has-scorm-objective-title :type string)
 (has-lpmo-objective :type string))
)

(def-class imsld-objective ()
((has-imsld-objective-id :type integer)
 (has-imsld-objective-title :type string)
 (has-lpmo-objective :type string))
)

(def-class lpmo-language ()
((has-language-title :type string))
)

(deflift lift-lpmo-language lpmo-language ()
  (lambda (string)
(ocml::name (new-instance 'lpmo-language `((has-language-title ,string))))))

;;(lift-lpmo-language "toto")
;;(describe-instance 'instance2734)
   

(def-class english-provider-object ()
((has-lpmo-id :type string)
 (has-provider-url :type string)
 (has-cost :type string)
 (has-language :type string))
)

(def-class oci-content-object ()
((has-lpmo-id :type string)
(has-oci-url :type string)
(has-language :type string)
(has-cost :type string)))

(def-class lpmo-objective ()
((has-objective-title :type string)
 (has-objective-id :type integer)
 (has-scorm-objective :type string)
(has-imsld-objective :type string))
)

(deflower lower-lpmo-objective lpmo-objective
(lambda (string) 
  (let ((inst nil))
     (setf inst (findany '?x `(and (instance-of ?x 'lpmo-objective)
                                   (has-scorm-objective ?x ,string))))
     (setf out (the-slot-value inst 'has-objective-title)))))

#|
(def-instance scorm-objective-java scorm-objective
((has-scorm-objective-id 1)
 (has-scorm-objective-title "Learn Java")
 (has-lpmo-objective  "Competency spec 1")))

(lower-scorm-objective "Learn Java")
(findany '?x '(has-scorm-objective-title ?x "Learn Java"))
(ocml-eval '(all-slot-values (findany '?x '(has-scorm-objective-title ?x "Learn Java")) 'has-lpmo-objective))
(let ((values
       (findany '?x `(= ?x (all-slot-values 'scorm-object-java 'has-lpmo-objective))))))
|#


;;(select-ontology 'luisa-first-prototype-services)
;;(describe-instance 'instance4099)

;;(ip::internal-solve-goal 'ocml::luisa-first-prototype-ontology 'ocml::get-lpmo-id-goal '((ocml::has-method "getURL")(ocml::has-scorm-objective "English4Runaways"))) 


(def-class lpmo-id-object ()
((has-lpmo-id :type integer))
)

(deflift lift-lpmo-id-object lpmo-id-object ()
(("has-lpmo-id" "getURLResponse/getURLReturn/text()")
 ))

(deflift lift-learning-object learning-object ()
(("lom-url" "getURLResponse/getURLReturn/text()")
 ))

(deflift lift-learning-object-multiref learning-object-multiref ()
(("lom-url" "multiRef/multiRef/text()")
 ))

(deflower lower-lom-instance learning-object 
  '(("LearningObject" 
     (("Title" "lom-title")
      ("Version" "lom-version")
      ("URL" "lom-url")
      ("Competency" "lom-competency")))))

(deflower lower-lo-multiref-inst learning-object-multiref 
  '(("LearningObject" 
     (("Title" "lom-title")
      ("Version" "lom-version")
      ("URL" "lom-url")
      ("Competency" "lom-competency")))))

#|
(deflift lift-learning-object-ref learning-object-ref ()
(("lo-ref-url" "getURLResponse/getURLReturn/text()")
 ))
|#


(deflower lower-lo-ref-instance learning-object-ref 
(lambda (string) 
  (let ((inst nil))
     (setf inst (findany '?x `(and (instance-of ?x 'english-provider-object)
                                   (has-lpmo-id ?x ,string))))
     (setf out (the-slot-value inst 'has-provider-url)))))


#|
(deflower lower-scorm-objective scorm-objective
(lambda (string) 
  (let ((inst nil))
     (setf inst (findany '?x `(and (instance-of ?x 'lpmo-objective)
                                   (has-scorm-objective ?x ,string))))
     (setf out (the-slot-value inst 'has-objective-title)))))

(def-instance english-provider-object1 english-provider-object
((has-lpmo-id "&quot;instance3 URL&quot;")
 (has-provider-url "http://www.ego4u.de/de/cram-up/tests/mix-3")))
|#

(deflift lift-competencies competencies ()
  (("has-competencies" (lift-competency-spec '("getListResponse/getListReturn")))))

(deflift lift-competency-spec competency-spec ()
("title" "getListReturn/text()")
 )



#|
(def-class SESSION-ID ()
  ((has-id-string :type string)
   (has-session-number :type integer)
   (has-instance-name :type string))) ;vlad 26/06/06 it's a symbol-name therefore string more than a symbol

(def-class buddyspace-SESSION-ID (SESSION-ID))

(deflower lower-buddyspace-SESSION-ID SESSION-ID
  (lambda (instance)
    (findany '?x `(has-id-string ,instance ?x))))

(deflift lift-competencies competencies ()
  (("has-competencies" (lift-competency-spec '("getListResponse/getListReturn")))))

(deflift lift-competency-spec competency-spec ()
("title" "getListReturn/multiRef/text()"))

<soapenv:Envelope>
<soapenv:Body>

<getListResponse soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
<getListReturn href="#id0"/>
</getListResponse>
<multiRef id="id0" soapenc:root="0" soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" soapenc:arrayType="xsd:anyType[3]" xsi:type="soapenc:Array">
<multiRef xsi:type="soapenc:string">http://www.luisa.org/BasicLOM#CompetencySpec1</multiRef>
<multiRef xsi:type="soapenc:string">http://www.luisa.org/BasicLOM#CompetencySpec2</multiRef>
<multiRef xsi:type="soapenc:string">http://www.luisa.org/BasicLOM#CompetencySpec3</multiRef>
</multiRef>
</soapenv:Body>
</soapenv:Envelope>

;;; soap message does not use multirefs in a proper way, but can we handle this kind of list too?
|#

(deflower lower-competencies competencies
  '(("Competencies"
     (("Specifications"
       ((lower-competency-spec "has-competencies")))))))

(deflower lower-competency-spec competency-spec
'(("Competency-Spec" 
     (("Title" "title")
      ))))



#|
(def-class inns-results ()
  ((has-inns :type accommodations-list)))

(def-class accommodation (spatial-object house-archetype)
((has-latitude :type float)
 (has-longitude :type float)
 (has-address1 :type string)
 (has-address2 :type string)
 (has-postcode :type string)
 (has-rooms :type string)
 (has-telephone :type string)))

(def-class inn (accommodation))

(deflift lift-inns-results inns-results ()
  (("has-inns" (lift-inn '("getInnsInRadiusResponse/getInnsInRadiusReturn/getInnsInRadiusReturn")))))

(deflift lift-inn inn ()
(("has-latitude" "getInnsInRadiusReturn/latitude/text()")
 ("has-longitude" "getInnsInRadiusReturn/longitude/text()")
 ("has-address1" "getInnsInRadiusReturn/aAddress1/text()")
 ("has-address2" "getInnsInRadiusReturn/aAddress2/text()")
 ;;("has-name" "getInnsInRadiusReturn/aName/text()")
 ("has-postcode" "getInnsInRadiusReturn/aPostCode/text()")
 ("has-rooms" "getInnsInRadiusReturn/aRooms/text()")
 ("has-telephone" "getInnsInRadiusReturn/aTelephone/text()")
 ("has-location" (eval (sgis-lift-point-location 
                          :lat-xpath "getInnsInRadiusReturn/latitude/text()"
                          :long-xpath "getInnsInRadiusReturn/longitude/text()")))))


(deflower lower-inns-results inns-results
  '(("sgis"
     (("objects"
       ((lower-inn "has-inns")))))))

(deflower lower-inn inn
  '(("object" (eval (sgis-lower-spatial-object)))))

 
|#

#|
(describe-instance 'instance2469)

*current-ontology*

(defparameter *learning-string*
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soapenv:Body><getURLResponse soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><getURLReturn xsi:type=\"xsd:string\">&quot;instance3 URL&quot;</getURLReturn></getURLResponse></soapenv:Body></soapenv:Envelope>")

(def-instance instance1 learning-object
((lom-title "Title1")
 (lom-version "Version1")
 (lom-competency "competency-spec1")
 (lom-url "instance1 URL")))

(deflower lower-test learning-object
  '(("object" 
     (("toto" "lom-title")
      ("toto2" "lom-url")
      ("Version" "lom-version")
      ("Competency" "lom-competency")))))

(deflower lower-lom-instance learning-object 
  '(("LearningObject" 
     (("Title" "lom-title")
      ("Version" "lom-version")
      ("URL" "lom-url")
      ("Competency" "lom-competency")))))

(lower-test 'instance2469)
(lower-lom-instance 'instance2469)
(lift-learning-object *learning-string*)

(lower-lom-instance 'instance1)
(lower-my-lom-instance 'instance1)

(xpm::o-xp "lom-url" 'instance1)
|#

(def-class lpmo-actor-simple ()
((has-actor-id :type integer)
 (has-actor-name :type string)
 (has-password :type string))
)

(def-class lpmo-learner-simple (lpmo-actor-simple)
((has-max-cost :type string)
 (has-language :type string))
)

(def-instance klaus lpmo-learner-simple
((has-actor-id 1)
 (has-actor-name "Klaus")
 (has-password "pwd")
 (has-language "#German$")
 (has-max-cost "250")))

(def-instance stefan lpmo-learner-simple
((has-actor-id 1)
 (has-actor-name "Stefan")
 (has-password "pwd")
 (has-language "#German$")
 (has-max-cost "500")))

(def-instance carlos lpmo-learner-simple
((has-actor-id 1)
 (has-actor-name "Carlos")
 (has-password "pwd")
 (has-language "#Spanish$")
 (has-max-cost "500")))

(def-instance john lpmo-learner-simple
((has-actor-id 1)
 (has-actor-name "John")
 (has-password "pwd")
 (has-language "#English$")
 (has-max-cost "500")))

(def-instance vlad lpmo-learner-simple
((has-actor-id 1)
 (has-actor-name "Vlad")
 (has-password "pwd")
 (has-language "#French$")
 (has-max-cost "500")))

(def-instance liana lpmo-learner-simple
((has-actor-id 1)
 (has-actor-name "Liana")
 (has-password "pwd")
 (has-language "#French$")
 (has-max-cost "500")))


(def-instance lpmo-objective-english lpmo-objective
((has-objective-title "Competency")
 (has-objective-id 1)
 (has-scorm-objective "English4Runaways")
 (has-imsld-objective "English4Runaways")))

(def-instance lpmo-objective-german lpmo-objective
((has-objective-title "Competency%20spec%201")
 (has-objective-id 2)
 (has-scorm-objective "LearnGerman")
 (has-imsld-objective "LearnGerman")))

;;;maybe the objective should be "Competency%20spec%201" or i have to call the replace function...
(def-instance lpmo-objective-english2 lpmo-objective
((has-objective-title "Competency")
 (has-objective-id 2)
 (has-scorm-objective "LearningEnglish")
 (has-imsld-objective "LearningEnglish")))

(def-instance lpmo-objective-french lpmo-objective
((has-objective-title "Competency%20spec%202")
 (has-objective-id 2)
 (has-scorm-objective "LearnItalian")
 (has-imsld-objective "LearnItalian")))

(def-instance scorm-objective-java scorm-objective
((has-scorm-objective-id 1)
 (has-scorm-objective-title "LearnJava")
 (has-lpmo-objective  "Competency")))

(def-instance scorm-objective-german scorm-objective
((has-scorm-objective-id 1)
 (has-scorm-objective-title "LearnGerman")
 (has-lpmo-objective  "Competency")))

#|
(def-instance english-provider-object1 english-provider-object
((has-lpmo-id "\"instance3 URL\"")
 (has-provider-url "#http://www.ego4u.de/de/cram-up/tests/mix-3$")))
|#

(def-instance provider-object-english-german english-provider-object
((has-lpmo-id "instance1 URL")
 (has-provider-url "#http://openlearn.open.ac.uk/course/view.php?id=1600$")
 (has-language "English")))

(def-instance provider-object-spanish-german english-provider-object
((has-lpmo-id "instance1 URL")
 (has-provider-url "#http://www.solocursos.net/peluqueria_y_estetica_canina_internet_-slccurso1816424.htm$")
 (has-cost "950")
 (has-language "Spanish")))

(def-instance provider-object-english-english english-provider-object
((has-lpmo-id "instance3 URL")
 (has-provider-url "#http://openlearn.open.ac.uk/course/view.php?id=1569$")
 (has-language "English")))

(def-instance provider-object-spanish-english english-provider-object
((has-lpmo-id "instance3 URL")
 (has-provider-url "#http://www.aulafacil.com/CursoIngles/CursoIngles.htm$")
 (has-language "Spanish")))

(def-instance provider-object-german-english english-provider-object
((has-lpmo-id "instance3 URL")
 (has-provider-url "#http://www.ego4u.de/de/cram-up/tests/mix-3$")
 (has-language "German")))

#|
(def-instance provider-object-english-french english-provider-object
((has-lpmo-id "instance2 URL")
 (has-provider-url "#http://openlearn.open.ac.uk/course/view.php?id=1659$")
 (has-language "English")))
|#

(def-instance provider-object-english-italian english-provider-object
((has-lpmo-id "instance2 URL")
 (has-provider-url "#http://www.bbc.co.uk/languages/italian/$")
 (has-language "English")))

(def-instance provider-object-german-italian english-provider-object
((has-lpmo-id "instance2 URL")
 (has-provider-url "#http://languages4everyone.com/de/italienisch/kurse/01_01_1.html$")
 (has-language "German")))

(def-instance provider-object-spanish-italian english-provider-object
((has-lpmo-id "instance2 URL")
 (has-provider-url "#http://www.solocursos.net/busq.cfm?pal_clave=Italiano&pfichas=246&gclid=CL3s8qv234gCFRSkXgodVnYD1g$")
 (has-language "Spanish")))

(def-instance provider-object-french-italian english-provider-object
((has-lpmo-id "instance2 URL")
 (has-provider-url "#http://selva.univ-paris1.fr/5coursenligne.htm$")
 (has-language "French")))

(def-instance provider-object-french-german english-provider-object
((has-lpmo-id "instance1 URL")
 (has-provider-url "#http://perso.orange.fr/cchamorand/$")
 (has-language "French")))

(def-instance provider-object-french-english english-provider-object
((has-lpmo-id "instance3 URL")
 (has-provider-url "#http://www.anglaisfacile.com/debutants.php$")
 (has-language "French")))

#|
(def-instance english-provider-object2 english-provider-object
((has-lpmo-id "instance3URL")
 (has-provider-url "#http://www.ego4u.de/de/cram-up/tests/mix-3$")))
|#

#|
(def-instance provider-object-english-german-english2 english-provider-object
((has-lpmo-id "\"instance1 URL\"")
 (has-provider-url "#http://www.ego4u.de/de/cram-up/tests/mix-3$")))
|#

(def-instance p get-lo-by-competency-request
((has-competency-request "Competency spec 1")))


(def-instance competency-spec1 competency-spec
((title "Competency spec 1")))

(def-instance competency-spec2 competency-spec
((title "Competency spec 2")))

(def-instance competency-spec3 competency-spec
((title "Competency spec 3")))


(def-instance empty-comp get-lo-by-competency-request
((has-competency-request "")))

(def-instance empty-field get-lo-by-field-request
((has-field-request "")))

(def-instance empty-lang get-lo-by-lang-request
((has-lang-request "")))

(def-instance empty-interact get-lo-by-interact-request
((has-interact-request "")))

(def-instance instance1 learning-object
((lom-title "Title1")
 (lom-version "Version1")
 (lom-competency "competency-spec1")
 (lom-url "instance1 URL")))

(def-instance instance2 learning-object
((lom-title "Title2")
 (lom-version "Version2")
 (lom-competency "competency-spec2")
 (lom-url "instance2 URL")))

(def-instance instance3 learning-object
((lom-title "Title2")
 (lom-version "Version2")
 (lom-competency "competency-spec3")
 (lom-url "instance3 URL")))

(def-instance en language)

(def-instance es language)

(def-instance undefined language)

(def-instance none language)

