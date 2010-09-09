
(in-package "OCML")

(in-ontology buddyspace)

(defvar *session-number* 0)

(defun current-session-number ()
  (incf *session-number*))

(defun element-to-string (el)
  (let ((sres (make-string-output-stream)))))

(deflift lift-SESSION-INFO SESSION-INFO ()
  (("has-objects" (lift-buddyspace-SESSION-ID '("buddyxml") :AS-VALUES))))

(deflift lift-buddyspace-SESSION-ID buddyspace-SESSION-ID ()
  (("has-session-id" "buddyxml/result/sessionid/text()")
   ("has-session-number" (eval (current-session-number)))
   ("has-instance-name" (eval (lift-instance-name)))))

(deflift lift-PRESENCE-INFO PRESENCE-INFO ()
  (("has-objects" (lift-BUDDYSPACE-USER '("buddyxml/presences/user") :AS-VALUES))))

(defun lift-BUDDYSPACE-USER (xml)
  (let ((res (sgis-lift-point-location-from-string (xpm::x-xp "user/connections/connection[1]/@id/text()" xml))))
    (when res ;;if there is a location
      (cond ((string-equal "online" (xpm::x-xp "user/connections/connection[1]/text()" xml))
             (lift-BUDDYSPACE-USER-ONLINE xml))
            ((string-equal "away" (xpm::x-xp "user/connections/connection[1]/text()" xml))
             (lift-BUDDYSPACE-USER-AWAY xml))))))

(deflift lift-BUDDYSPACE-USER-AWAY BUDDYSPACE-USER-AWAY ()
  (("has-pretty-name" "user/id/text()")
   ("has-user-id" "user/id/text()")
   ("has-status" "user/connections/connection[1]/text()")
   ("has-contact-url" (eval (let ((id (xpm::x-xp "user/id/text()")))
                           (concatenate 'string 
                                        "<a target='_blank' href='http://uptonpark.open.ac.uk/bswebclient?command=chat&amp;jid="
                                        (xpath-mini::url-encode id)
                                        "'>go</a>"))))
   ("has-location" (eval (sgis-lift-point-location-from-string (xpm::x-xp "user/connections/connection[1]/@id/text()"))))))

(deflift lift-BUDDYSPACE-USER-ONLINE BUDDYSPACE-USER-ONLINE ()
  (("has-pretty-name" "user/id/text()")
   ("has-user-id" "user/id/text()")
   ("has-status" "user/connections/connection[1]/text()")
   ("has-contact-url" (eval (let ((id (xpm::x-xp "user/id/text()")))
                           (concatenate 'string 
                                        "<a target='_blank' href='http://uptonpark.open.ac.uk/bswebclient?command=chat&amp;jid="
                                        (xpath-mini::url-encode id)
                                        "'>go</a>"))))
   ("has-location" (eval (sgis-lift-point-location-from-string (xpm::x-xp "user/connections/connection[1]/@id/text()"))))))

#|LIFTING TESTS

(setf *presences-xml* "<?xml version=\"1.0\" encoding=\"UTF-8\"?><buddyxml version=\"1.1\"><presences><user id=\"a.gugliotta%open.ac.uk@buddyspace.org\"><id>a.gugliotta%open.ac.uk@buddyspace.org</id><connections><connection id=\"BuddySpace2.6\" status=\"online\">online</connection></connections></user><user id=\"v.tanasescu%open.ac.uk@buddyspace.org\"><id>v.tanasescu%open.ac.uk@buddyspace.org</id><connections><connection id=\"BuddySpace2.6\" status=\"online\">online</connection></connections></user><user id=\"c.thomas%open.ac.uk@buddyspace.org\"><id>c.thomas%open.ac.uk@buddyspace.org</id><connections><connection id=\"BuddySpace2.5.2pro\" status=\"online\">online</connection></connections></user><user id=\"b.hawkridge%open.ac.uk@buddyspace.org\"><id>b.hawkridge%open.ac.uk@buddyspace.org</id><connections><connection id=\"Ben's G4/500\" status=\"online\">online</connection></connections></user><user id=\"m.b.gaved%open.ac.uk@buddyspace.org\"><id>m.b.gaved%open.ac.uk@buddyspace.org</id><connections><connection id=\"BuddySpace2.6\" status=\"away\">away</connection></connections></user><user id=\"c.pedrinaci%open.ac.uk@buddyspace.org\"><id>c.pedrinaci%open.ac.uk@buddyspace.org</id><connections><connection id=\"lat=51.876&amp;long=0.137\" status=\"online\">online</connection></connections></user><user id=\"m.s.bachler%open.ac.uk@buddyspace.org\"><id>m.s.bachler%open.ac.uk@buddyspace.org</id><connections><connection id=\"BuddySpace2.6\" status=\"away\">away</connection></connections></user><user id=\"p.alexander%open.ac.uk@buddyspace.org\"><id>p.alexander%open.ac.uk@buddyspace.org</id><connections><connection id=\"TipicIM\" status=\"xa\">xa</connection></connections></user><user id=\"buddyfinder@buddyspace.org\"><id>buddyfinder@buddyspace.org</id><connections><connection id=\"javabot\" status=\"chat\">chat</connection></connections></user><user id=\"j.b.domingue%open.ac.uk@buddyspace.org\"><id>j.b.domingue%open.ac.uk@buddyspace.org</id><connections><connection id=\"lat=51.957&amp;long=0.084\" status=\"online\">online</connection></connections></user><user id=\"a.nikolov%open.ac.uk@buddyspace.org\"><id>a.nikolov%open.ac.uk@buddyspace.org</id><connections><connection id=\"BuddySpace2.5.2pro\" status=\"online\">online</connection></connections></user><user id=\"t.heath%open.ac.uk@buddyspace.org\"><id>t.heath%open.ac.uk@buddyspace.org</id><connections><connection id=\"lat=51.935&amp;long=0.349\" status=\"away\">away</connection></connections></user><user id=\"l.s.cabral%open.ac.uk@buddyspace.org\"><id>l.s.cabral%open.ac.uk@buddyspace.org</id><connections><connection id=\"lat=51.967&amp;long=0.178\" status=\"online\">online</connection></connections></user><user id=\"b.j.norton%open.ac.uk@buddyspace.org\"><id>b.j.norton%open.ac.uk@buddyspace.org</id><connections><connection id=\"BuddySpace2.5.2pro\" status=\"online\">online</connection></connections></user><user id=\"c.m.denham%open.ac.uk@buddyspace.org\"><id>c.m.denham%open.ac.uk@buddyspace.org</id><connections><connection id=\"BuddySpace2.7\" status=\"away\">away</connection></connections></user><user id=\"k.a.quick%open.ac.uk@buddyspace.org\"><id>k.a.quick%open.ac.uk@buddyspace.org</id><connections><connection id=\"BuddySpace2.6\" status=\"online\">online</connection></connections></user><user id=\"s.galizia%open.ac.uk@buddyspace.org\"><id>s.galizia%open.ac.uk@buddyspace.org</id><connections><connection id=\"lat=51.996&amp;long=0.32\" status=\"online\">online</connection></connections></user><user id=\"l.d.d.h.mccann%open.ac.uk@buddyspace.org\"><id>l.d.d.h.mccann%open.ac.uk@buddyspace.org</id><connections><connection id=\"Work\" status=\"online\">online</connection></connections></user></presences></buddyxml>")

(sgis-lift-point-location-from-string (xpm::x-xp "user/connections/connection[1]/@id/text()" "<user id=\"c.pedrinaci%open.ac.uk@buddyspace.org\"><id>c.pedrinaci%open.ac.uk@buddyspace.org</id><connections><connection id=\"lat=51.876&amp;long=0.137\" status=\"online\">online</connection></connections></user>"))

(sgis-lift-point-location-from-string (xpm::x-xp "user/connections/connection[1]/@id/text()" "<user id=\"c.pedrinaci%open.ac.uk@buddyspace.org\"><id>c.pedrinaci%open.ac.uk@buddyspace.org</id><connections><connection id=\"blah\" status=\"online\">online</connection></connections></user>"))


(describe-instance 'INSTANCE10271)
(describe-instance 'INSTANCE10341)
(describe-instance 'INSTANCE2702)
(describe-instance 'INSTANCE3109)
(describe-instance 'INSTANCE3031)
(car (xpm::o-xp "has-latitude" 'INSTANCE2706))
(lower-longitudinal-point 'INSTANCE3109)
(describe-instance 'INSTANCE2628)
(lower-spatial-object 'INSTANCE5935)
(lift-presence-info *presences-xml*)

(lower-object-field 'INSTANCE10407)

|#



#|


(deflower lower-user user
  '(("object"
     (("id" (eval (lower-instance-name)))
      ("type" (eval (lower-instance-class-name)))
      ("archetypes" (eval (lower-instance-archetype-list)))
      ("attributes"
       (("attribute"
         (("name" (eval (string "Id")))
          ("value" "has-id")))
        ("attribute"
         (("name" (eval (string "Status")))
          ("value" "has-connections[1]/has-status")))
        ("attribute"
         (("name" (eval (string "Chat!")))
          ("value" (eval (let ((id (car (xpm::o-xp "has-id"))))
                           (concatenate 'string 
                                        "<a target='_blank' href='http://uptonpark.open.ac.uk/bswebclient?command=chat&amp;jid="
                                        (xpath-mini::url-encode id)
                                        "'>go</a>"))))))))
      (lower-connection "has-connections")))))


(deflift lift-temp-centres-results temp-centres-results ()
  (("has-objects" (lift-rest-centre '("getRestCentresInRadiusResponse/getRestCentresInRadiusReturn/getRestCentresInRadiusReturn") :AS-VALUES))))

(deflift lift-rest-centre rest-centre ()
(("has-latitude" "getRestCentresInRadiusReturn/latitude/text()")
 ("has-longitude" "getRestCentresInRadiusReturn/longitude/text()")
 ("has-address" "getRestCentresInRadiusReturn/rcAddress/text()")
 ("has-capacity" "getRestCentresInRadiusReturn/rcCapacity/text()")
 ("has-cooking" "getRestCentresInRadiusReturn/rcCooking/text()")
 ("has-GMS" "getRestCentresInRadiusReturn/rcGMS/text()")
 ("has-heating" "getRestCentresInRadiusReturn/rcHeating/text()")
 ("has-key-holder" "getRestCentresInRadiusReturn/rcKeyHolder/text()")
 ("has-key-holder2" "getRestCentresInRadiusReturn/rcKeyHolder2/text()")
 ("has-key-holder2-telephone" "getRestCentresInRadiusReturn/rcKeyHolder2Telephone/text()")
 ("has-key-holder-telephone" "getRestCentresInRadiusReturn/rcKeyHolderTelephone/text()")
 ("has-meals" "getRestCentresInRadiusReturn/rcMeals/text()")
 ("has-name" "getRestCentresInRadiusReturn/rcName/text()")
 ("has-provision" "getRestCentresInRadiusReturn/rcProvision/text()")
 ("has-remarks" "getRestCentresInRadiusReturn/rcRemarks/text()")
 ("has-telephone" "getRestCentresInRadiusReturn/rcTelephone/text()")
 ("has-location" (eval (sgis-lift-point-location 
                          :lat-xpath "getRestCentresInRadiusReturn/latitude/text()"
                          :long-xpath "getRestCentresInRadiusReturn/longitude/text()")))))

|#

#|

(setf conn-str "<?xml version=\"1.0\" encoding=\"UTF-8\"?><buddyxml version=\"1.1\"><result sessionid=\"D725EB976C868B94A57E7A8434C21A36\" requesturl=\"http://msg.open.ac.uk/buddyxml\" message=\"Login succeeded\"><sessionid>D725EB976C868B94A57E7A8434C21A36</sessionid><message>Login succeeded</message></result></buddyxml>")

(lift-SESSION-INFO conn-str)

(describe-instance 'INSTANCE10205)
(describe-instance 'INSTANCE10207)

(lower-object-field 'INSTANCE25818)

|#