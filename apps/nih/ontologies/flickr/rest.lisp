;;; Copyright © 2009,2010 The Open University

(in-package #:ocml)

(in-ontology flickr)

;;; In several cases, we have signed calls that the Flickr API
;;; documentation claims do not need to be signed.  As best I can
;;; tell, if the information you're requesting is public, you don't
;;; need to signed, if it's private, you do.  And there's no way to
;;; tell ahead of time which it is.  Most lowerings sign the requests.

;;; {{{ Goal definitions
(def-class #_authGetFrobRestGoal (goal) ?goal
    ((has-input-role :value #_hasAccount)
     (has-output-role :value #_hasFrob)
     (#_hasAccount :type #_Account)
     (#_hasFrob :type #_Frob)))

(def-class #_authGetTokenRestGoal (goal) ?goal
    ((has-input-role :value #_hasAccount :value #_hasFrob)
     (has-output-role :value #_hasToken)
     (#_hasAccount :type #_Account)
     (#_hasFrob :type #_Frob)
     (#_hasToken :type #_Token)))
;;; }}}

;;; {{{ #_authGetFrobRestService

(def-rule #_lower-for-authGetFrobRestService
    ((#_grnd:lower #_authGetFrobRestService ?invocation ?http-request)
     if
     (#_rfc2616:set-method ?http-request "GET")
     (= ?base-url "http://api.flickr.com/services/rest/?")
     (= ?account (wsmo-role-value ?invocation #_hasAccount))
     (#_hasKey ?account ?apikey)
     (#_hasValue ?apikey ?apikey-string)
     (= ?args (#_new-instance #_Arguments))
     (#_addArgument ?args "method" "flickr.auth.getFrob")
     (#_addArgument ?args "api_key" ?apikey-string)
     (#_signArguments #_rest ?args ?account)
     (#_asQuery ?args ?query)
     (= ?url (make-string "~A~A" ?base-url ?query))
     (#_rfc2616:set-url ?http-request ?url)))

(def-rule #_lift-for-authGetFrobRestService
    ((#_grnd:lift #_authGetFrobRestService ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (#_xml:serialiseXml ?xml ?http-content)
     (#_extractFrobFromXml ?xml ?frob)
     (set-goal-slot-value ?invocation #_hasFrob ?frob)))

(def-rule #_extractFrobFromXml
    ((#_extractFrobFromXml ?xml ?frob) if
     (#_xml:rootElement ?xml ?rootel)
     (#_xml:contents ?rootel ?contents)
     (member ?frobel ?contents)
     (#_xml:tag ?frobel "frob")
     (#_xml:contents ?frobel (?c))
     (#_xml:value ?c ?frobstring)
     (= ?frob (#_new-instance #_Frob))
     (exec (set-slot-value ?frob #_hasValue ?frobstring))
     cut))

(def-rule #_extractTokenFromXml
    ((#_extractTokenFromXml ?xml ?token) if
     (#_xml:rootElement ?xml ?rootel)
     (#_xml:contents ?rootel ?rootcontents)
     (member ?authel ?rootcontents)
     (#_xml:tag ?authel "auth")
     (#_xml:contents ?authel ?authcontents)
     (member ?tokenel ?authcontents)
     (#_xml:tag ?tokenel "token")
     (#_xml:contents ?tokenel ?tokencontents)
     (member ?t ?tokencontents)
     (#_xml:value ?t ?tokenstring)
     (= ?token (#_new-instance #_Token))
     (exec (set-slot-value ?token #_hasValue ?tokenstring))
     cut))

(def-class #_authGetFrobRest-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class #_authGetFrobRest-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_authGetFrobRestGoal)
     (has-non-functional-properties
      :value #_authGetFrobRest-mediator-non-functional-properties)))

(def-class #_authGetFrobRestService (web-service) ?web-service
    ((has-capability :value #_authGetFrobRestService-capability)
     (has-interface :value #_authGetFrobRestService-interface)
     (has-non-functional-properties
      :value #_authGetFrobRestService-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class #_authGetFrobRestService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_authGetFrobRestService-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_authGetFrobRestService-capability
           (capability)
           ?capability
           ((used-mediator :value #_authGetFrobRest-mediator)
            (has-non-functional-properties
             :value
             #_authGetFrobRestService-capability-non-functional-properties)))

(def-class #_authGetFrobRestService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_authGetFrobRestService-interface-choreography
           (choreography)
           ((has-earthing :value #_authGetFrobRestService-grounding)))

(def-instance #_authGetFrobRestService-grounding rest-grounding
  ())

(def-class #_authGetFrobRestService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_authGetFrobRestService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_authGetFrobRestService-interface-orchestration-problem-solving-pattern)))

(def-class #_authGetFrobRestService-interface (interface) ?interface
    ((has-choreography :value #_authGetFrobRestService-interface-choreography)
     (has-orchestration :value #_authGetFrobRestService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_authGetFrobRestService-interface-non-functional-properties)))

;;; }}}

;;; {{{ #_authGetTokenRestService

(def-rule #_lower-for-authGetTokenRestService
    ((#_grnd:lower #_authGetTokenRestService ?invocation ?http-request) if
     (#_rfc2616:set-method ?http-request "GET")
     (= ?base-url "http://api.flickr.com/services/rest/?")
     (= ?account (wsmo-role-value ?invocation #_hasAccount))
     (= ?frob (wsmo-role-value ?invocation #_hasFrob))
     (#_hasKey ?account ?apikey)
     (#_hasValue ?apikey ?apikey-string)
     (#_hasValue ?frob ?frob-string)
     (= ?args (#_new-instance #_Arguments))
     (#_addArgument ?args "method" "flickr.auth.getToken")
     (#_addArgument ?args "api_key" ?apikey-string)
     (#_addArgument ?args "frob" ?frob-string)
     (#_signArguments #_rest ?args ?account)
     (#_asQuery ?args ?query)
     (= ?url (make-string "~A~A" ?base-url ?query))
     (#_rfc2616:set-url ?http-request ?url)))

(def-rule #_lift-for-authGetTokenRestService
    ((#_grnd:lift #_authGetTokenRestService ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (#_xml:serialiseXml ?xml ?http-content)
     (#_extractTokenFromXml ?xml ?token)
     (set-goal-slot-value ?invocation #_hasToken ?token)))

(def-rule #_extractFrobFromXml
    ((#_extractFrobFromXml ?xml ?frob) if
     (#_xml:rootElement ?xml ?rootel)
     (#_xml:contents ?rootel ?contents)
     (member ?frobel ?contents)
     (#_xml:tag ?frobel "frob")
     (#_xml:contents ?frobel (?c))
     (#_xml:value ?c ?frobstring)
     (= ?frob (#_new-instance #_Frob))
     (exec (set-slot-value ?frob #_hasValue ?frobstring))))

(def-class #_authGetTokenRest-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class #_authGetTokenRest-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_authGetTokenRestGoal)
     (has-non-functional-properties
      :value #_authGetTokenRest-mediator-non-functional-properties)))

(def-class #_authGetTokenRestService (web-service) ?web-service
    ((has-capability :value #_authGetTokenRestService-capability)
     (has-interface :value #_authGetTokenRestService-interface)
     (has-non-functional-properties
      :value #_authGetTokenRestService-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class #_authGetTokenRestService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_authGetTokenRestService-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_authGetTokenRestService-capability
           (capability)
           ?capability
           ((used-mediator :value #_authGetTokenRest-mediator)
            (has-non-functional-properties
             :value
             #_authGetTokenRestService-capability-non-functional-properties)))

(def-class #_authGetTokenRestService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_authGetTokenRestService-interface-choreography
           (choreography)
           ((has-earthing :value #_authGetTokenRestService-grounding)))

(def-instance #_authGetTokenRestService-grounding rest-grounding
  ())

(def-class #_authGetTokenRestService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_authGetTokenRestService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_authGetTokenRestService-interface-orchestration-problem-solving-pattern)))

(def-class #_authGetTokenRestService-interface (interface) ?interface
    ((has-choreography :value #_authGetTokenRestService-interface-choreography)
     (has-orchestration :value #_authGetTokenRestService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_authGetTokenRestService-interface-non-functional-properties)))

;;; }}}

;;; {{{ #_photosRecentlyUpdatedRestService

;; Return a list of your photos that have been recently created or which
;; have been recently modified.

;; Recently modified may mean that the photo's metadata (title,
;; description, tags) may have been changed or a comment has been
;; added (or just modified somehow :-) Authentication

;; This method requires authentication with 'read' permission.
;; Arguments


;; api_key (Required)
;;     Your API application key. See here for more details.
;; min_date (Required)
;;     A Unix timestamp indicating the date from which modifications should be compared.
;; extras (Optional)
;;     A comma-delimited list of extra information to fetch for each returned record. Currently supported fields are: license, date_upload, date_taken, owner_name, icon_server, original_format, last_update, geo, tags, machine_tags, o_dims, views, media, path_alias, url_sq, url_t, url_s, url_m, url_o
;; per_page (Optional)
;;     Number of photos to return per page. If this argument is omitted, it defaults to 100. The maximum allowed value is 500.
;; page (Optional)
;;     The page of results to return. If this argument is omitted, it defaults to 1. 

(def-class #_photosRecentlyUpdatedRestGoal (goal) ?goal
    ((has-input-role :value #_hasAccount
		     :value #_hasToken
		     :value #_hasMinimumDate)
     (has-output-role :value #_hasPhotoList)
     (#_hasAccount :type #_Account)
     (#_hasToken :type #_Token)
     ;; XXX hasMinimumDate is the Unix time.  It must be > 0.
     (#_hasMinimumDate :type integer)
     (#_hasPhotoList :type #_PhotoList)))

(def-class #_photosRecentlyUpdatedRest-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class #_photosRecentlyUpdatedRest-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_photosRecentlyUpdatedRestGoal)
     (has-non-functional-properties
      :value #_photosRecentlyUpdatedRest-mediator-non-functional-properties)))

(def-class #_photosRecentlyUpdatedRestService (web-service) ?web-service
    ((has-capability :value #_photosRecentlyUpdatedRestService-capability)
     (has-interface :value #_photosRecentlyUpdatedRestService-interface)
     (has-non-functional-properties
      :value #_photosRecentlyUpdatedRestService-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class #_photosRecentlyUpdatedRestService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_photosRecentlyUpdatedRestService-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_photosRecentlyUpdatedRestService-capability
           (capability)
           ?capability
           ((used-mediator :value #_photosRecentlyUpdatedRest-mediator)
            (has-non-functional-properties
             :value
             #_photosRecentlyUpdatedRestService-capability-non-functional-properties)))

(def-class #_photosRecentlyUpdatedRestService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_photosRecentlyUpdatedRestService-interface-choreography
           (choreography)
           ((has-earthing :value #_photosRecentlyUpdatedRestService-grounding)))

(def-instance #_photosRecentlyUpdatedRestService-grounding rest-grounding
  ())

(def-class #_photosRecentlyUpdatedRestService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_photosRecentlyUpdatedRestService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_photosRecentlyUpdatedRestService-interface-orchestration-problem-solving-pattern)))

(def-class #_photosRecentlyUpdatedRestService-interface (interface) ?interface
    ((has-choreography :value #_photosRecentlyUpdatedRestService-interface-choreography)
     (has-orchestration :value #_photosRecentlyUpdatedRestService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_photosRecentlyUpdatedRestService-interface-non-functional-properties)))

;;; {{{ Lifting and lowering
(def-rule #_lower-for-photosRecentlyUpdatedRestService
    ((#_grnd:lower #_photosRecentlyUpdatedRestService ?invocation ?http-request) if
     (#_rfc2616:set-method ?http-request "GET")
     (= ?base-url "http://api.flickr.com/services/rest/?")
     (= ?account (wsmo-role-value ?invocation #_hasAccount))
     (= ?token (wsmo-role-value ?invocation #_hasToken))
     (#_hasKey ?account ?apikey)
     (#_hasValue ?apikey ?apikey-string)
     (#_hasValue ?token ?token-string)
     (= ?min-date (make-string "~A" (wsmo-role-value ?invocation #_hasMinimumDate)))
     (= ?args (#_new-instance #_Arguments))
     (#_addArgument ?args "method" "flickr.photos.recentlyUpdated")
     (#_addArgument ?args "api_key" ?apikey-string)
     (#_addArgument ?args "auth_token" ?token-string)
     (#_addArgument ?args "min_date" ?min-date)
     (#_signArguments #_rest ?args ?account)
     (#_asQuery ?args ?query)
     (= ?url (make-string "~A~A" ?base-url ?query))
     (#_rfc2616:set-url ?http-request ?url)))

(def-rule #_lift-for-photosRecentlyUpdatedRestService
    ((#_grnd:lift #_photosRecentlyUpdatedRestService ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (#_xml:serialiseXml ?xml ?http-content)
     (#_xml:rootElement ?xml ?root-element)
     (#_xml:elementByName ?root-element ?photos-element "photos")
     (#_xmllyPhotoList ?photos-element ?photolist)
     (set-goal-slot-value ?invocation #_hasPhotoList ?photolist)))

;;; }}}

;;; }}}

;;; {{{ #_photosRecentlyUpdatedRestServiceJson

;;Same as #_photosRecentlyUpdatedRestServiceJson but grounds to the
;;version of the service that returns JSON output

(def-class #_photosRecentlyUpdatedRestGoalJson (goal) ?goal
  ((has-input-role :value #_hasAccount
		   :value #_hasToken
		   :value #_hasMinimumDate)
   (has-output-role :value #_hasPhotoList)
   (#_hasAccount :type #_Account)
   (#_hasToken :type #_Token)
   ;; XXX hasMinimumDate is the Unix time.  It must be > 0.
   (#_hasMinimumDate :type integer)
   (#_hasPhotoList :type #_PhotoList)))

(def-class
  #_photosRecentlyUpdatedRestJson-mediator-non-functional-properties
  (non-functional-properties)
    nil)

(def-class #_photosRecentlyUpdatedRestJson-mediator (wg-mediator)
  ?mediator
  ((has-source-component :value #_photosRecentlyUpdatedRestGoalJson)
   (has-non-functional-properties :value
  #_photosRecentlyUpdatedRestJson-mediator-non-functional-properties)))


(def-class #_photosRecentlyUpdatedRestServiceJson (web-service)
  ?web-service
  ((has-capability :value
  #_photosRecentlyUpdatedRestServiceJson-capability)
   (has-interface :value
  #_photosRecentlyUpdatedRestServiceJson-interface)
   (has-non-functional-properties :value
  #_photosRecentlyUpdatedRestServiceJson-non-functional-properties)
   (has-output-role :value #_hasContent :value #_hasContentType)
   (#_hasContent :cardinality 1)
   (#_hasContentType :cardinality 1)))

(def-class
  #_photosRecentlyUpdatedRestServiceJson-non-functional-properties
  (non-functional-properties)
  nil)

(def-class
  #_photosRecentlyUpdatedRestServiceJson-capability-non-functional-properties
  (non-functional-properties)
  nil)

(def-class #_photosRecentlyUpdatedRestServiceJson-capability
  (capability)
  ?capability
  ((used-mediator :value #_photosRecentlyUpdatedRestJson-mediator)
   (has-non-functional-properties :value
    #_photosRecentlyUpdatedRestServiceJson-capability-non-functional-properties)))

(def-class
  #_photosRecentlyUpdatedRestServiceJson-interface-non-functional-properties
  (non-functional-properties)
  nil)

(def-class
  #_photosRecentlyUpdatedRestServiceJson-interface-choreography
  (choreography)
  ((has-earthing :value
  #_photosRecentlyUpdatedRestServiceJson-grounding)))

(def-instance #_photosRecentlyUpdatedRestServiceJson-grounding
  rest-grounding
  ())

(def-class
  #_photosRecentlyUpdatedRestServiceJson-interface-orchestration-problem-solving-pattern
  (problem-solving-pattern)
  ((has-body :value nil)))

(def-class
  #_photosRecentlyUpdatedRestServiceJson-interface-orchestration
  (orchestration)
  ((has-problem-solving-pattern
    :value
    #_photosRecentlyUpdatedRestServiceJson-interface-orchestration-problem-solving-pattern)))

(def-class #_photosRecentlyUpdatedRestServiceJson-interface
  (interface) ?interface
  ((has-choreography :value
  #_photosRecentlyUpdatedRestServiceJson-interface-choreography)
   (has-orchestration :value
  #_photosRecentlyUpdatedRestServiceJson-interface-orchestration)
   (has-non-functional-properties :value
    #_photosRecentlyUpdatedRestServiceJson-interface-non-functional-properties)))

;;; {{{ Lifting and lowering

;;The following lifting and lowering rules are meant for parsing
;;JSON output such as the following:
#|
{"photos":
   {"page":1, 
    "pages":2, 
    "perpage":100, 
    "total":"197", 
    "photo":
        [
         {"id":"263263790", "owner":"17186009@N00",
   "secret":"..........", "server":"114", "farm":1, "title":"men on a
   mission - barbados 2006", "ispublic":1, "isfriend":0,
   "isfamily":0}, 
         {"id":"435276838", "owner":"17186009@N00",
   "secret":"..........", "server":"181", "farm":1, "title":"Cambridge
   2007 - the original RedMobile crew (minus Nysha)", "ispublic":1,
   "isfriend":0, "isfamily":0}, 
         {"id":"623101066", "owner":"17186009@N00",
   "secret":"..........", "server":"1138", "farm":2, "title":"2007 -
   london - busspepper", "ispublic":1, "isfriend":0, "isfamily":0},
         {"id":"391467379", "owner":"17186009@N00",
   "secret":"..........", "server":"162", "farm":1, "title":"MK Winter
   2007 - Winter Garden", "ispublic":1, "isfriend":0, "isfamily":0}
         ...
        ]
   }, 
 "stat":"ok"
}
|#

(def-rule #_lower-for-photosRecentlyUpdatedRestServiceJson
  ((#_grnd:lower #_photosRecentlyUpdatedRestServiceJson ?invocation ?http-request) if
     (#_rfc2616:set-method ?http-request "GET")
     (= ?base-url "http://api.flickr.com/services/rest/?")
     (= ?account (wsmo-role-value ?invocation #_hasAccount))
     (= ?token (wsmo-role-value ?invocation #_hasToken))
     (#_hasKey ?account ?apikey)
     (#_hasValue ?apikey ?apikey-string)
     (#_hasValue ?token ?token-string)
     (= ?min-date 
	(make-string "~A" 
		     (wsmo-role-value ?invocation #_hasMinimumDate)))
     (= ?args (#_new-instance #_Arguments))
     (#_addArgument ?args "method" "flickr.photos.recentlyUpdated")
     (#_addArgument ?args "api_key" ?apikey-string)
     (#_addArgument ?args "auth_token" ?token-string)
     (#_addArgument ?args "min_date" ?min-date)
     (#_addArgument ?args "format" "json")
     (#_addArgument ?args "nojsoncallback" "1")
     (#_signArguments #_rest ?args ?account)
     (#_asQuery ?args ?query)
     (= ?url (make-string "~A~A" ?base-url ?query))
     (#_rfc2616:set-url ?http-request ?url)))

(def-rule #_lift-for-photosRecentlyUpdatedRestServiceJson
    ((#_grnd:lift #_photosRecentlyUpdatedRestServiceJson ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (#_json:serialiseJson ?json ?http-content)
     (#_json:Object ?json)
     (#_json:members ?json ?objMembers)
     (member ?photosKeyValuePair ?objMembers)
     (#_json:key ?photosKeyValuePair "photos")
     (#_json:value ?photosKeyValuePair ?innerPhotosObject)
     (#_extractPhotoListFromJson ?innerPhotosObject ?photolist)
     (set-goal-slot-value ?invocation #_hasPhotoList ?photolist)))

(def-rule #_extractPhotoListFromJson
    ((#_extractPhotoListFromJson ?innerPhotosObject ?photolist) if
     (= ?photolist
	(setofall ?photo
		  (and (#_json:Object ?innerPhotosObject)
                       (#_json:members ?innerPhotosObject ?innerPhotosObjectMembers)
                       (member ?photoKeyValuePair ?innerPhotosObjectMembers)
                       (#_json:key ?photoKeyValuePair "photo")
                       (#_json:value ?photoKeyValuePair ?photoArray)
                       (#_json:Array ?photoArray)
                       (#_json:elements ?photoArray ?photoArrayElements)
                       (member ?photoObject ?photoArrayElements)
		       (#_extractPhotoFromJson ?photoObject ?photo))))))

(def-rule #_extractPhotoFromJson
    ((#_extractPhotoFromJson ?photoObject ?photo) if
     (#_json:Object ?photoObject)
     (#_json:members ?photoObject ?photoObjectMembers)
     (member ?idKeyValuePair ?photoObjectMembers)
     (#_json:key ?idKeyValuePair "id")
     (#_json:value ?idKeyValuePair ?id)
     (member ?titleKeyValuePair ?photoObjectMembers)
     (#_json:key ?titleKeyValuePair "title")
     (#_json:value ?titleKeyValuePair ?title)
     (member ?farmKeyValuePair ?photoObjectMembers)
     (#_json:key ?farmKeyValuePair "farm")
     (#_json:value ?farmKeyValuePair ?farm)
     (member ?serverKeyValuePair ?photoObjectMembers)
     (#_json:key ?serverKeyValuePair "server")
     (#_json:value ?serverKeyValuePair ?server)
     (member ?secretKeyValuePair ?photoObjectMembers)
     (#_json:key ?secretKeyValuePair "secret")
     (#_json:value ?secretKeyValuePair ?secret)
     (= ?slots ((#_id ?id)
                (#_title ?title)
                (#_farm ?farm)
                (#_server ?server)
                (#_secret ?secret)))
     (#_new ?photo #_Photo ?slots)))

;;; }}}

;;; }}}


;;; {{{ #_photosGetSizesRestService

;; Return a list sizes a photo is available in.

;; api_key (Required)
;;     Your API application key. See here for more details.
;; photo_id (Required)

(def-class #_photosGetSizesRestGoal (goal) ?goal
    ((has-input-role :value #_hasAccount
		     :value #_hasPhoto
		     :value #_hasToken)
     (has-output-role :value #_hasPhotoSizes)
     (#_hasAccount :type #_Account)
     (#_hasToken :type #_Token)
     (#_hasPhoto :type #_Photo)
     (#_hasPhotoSizes :type #_PhotoSizeList)))

(def-class #_photosGetSizesRest-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class #_photosGetSizesRest-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_photosGetSizesRestGoal)
     (has-non-functional-properties
      :value #_photosGetSizesRest-mediator-non-functional-properties)))

(def-class #_photosGetSizesRestService (web-service) ?web-service
    ((has-capability :value #_photosGetSizesRestService-capability)
     (has-interface :value #_photosGetSizesRestService-interface)
     (has-non-functional-properties
      :value #_photosGetSizesRestService-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class #_photosGetSizesRestService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_photosGetSizesRestService-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_photosGetSizesRestService-capability
           (capability)
           ?capability
           ((used-mediator :value #_photosGetSizesRest-mediator)
            (has-non-functional-properties
             :value
             #_photosGetSizesRestService-capability-non-functional-properties)))

(def-class #_photosGetSizesRestService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_photosGetSizesRestService-interface-choreography
           (choreography)
           ((has-earthing :value #_photosGetSizesRestService-grounding)))

(def-instance #_photosGetSizesRestService-grounding rest-grounding
  ())

(def-class #_photosGetSizesRestService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_photosGetSizesRestService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_photosGetSizesRestService-interface-orchestration-problem-solving-pattern)))

(def-class #_photosGetSizesRestService-interface (interface) ?interface
    ((has-choreography :value #_photosGetSizesRestService-interface-choreography)
     (has-orchestration :value #_photosGetSizesRestService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_photosGetSizesRestService-interface-non-functional-properties)))

;;; {{{ Lifting and lowering
(def-rule #_lower-for-photosGetSizesRestService
    ((#_grnd:lower #_photosGetSizesRestService ?invocation ?http-request) if
     (#_rfc2616:set-method ?http-request "GET")
     (= ?base-url "http://api.flickr.com/services/rest/?")
     (#_argsForPhotosGetSizes ?invocation ?args)
     (#_signArguments #_rest ?args ?account)
     (#_asQuery ?args ?query)
     (= ?url (make-string "~A~A" ?base-url ?query))
     (#_rfc2616:set-url ?http-request ?url)))

(def-rule #_lift-for-photosGetSizesRestService
    ((#_grnd:lift #_photosGetSizesRestService ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (#_xml:serialiseXml ?xml ?http-content)
     (#_xml:rootElement ?xml ?root-element)
     (#_xml:elementByName ?root-element ?sizes-element "sizes")
     (#_xmllyPhotoSizeList ?sizes-element ?photosizelist)
     (set-goal-slot-value ?invocation #_hasPhotoSizes ?photosizelist)))

;;; }}}

;;; }}}

;;; {{{ #_auth_checkToken

;; Returns the credentials attached to an authentication token. This
;; call must be signed as specified in the authentication API spec.

;; This method does not require authentication.

;; Arguments

;; api_key (Required)
;;     Your API application key. See here for more details.
;; auth_token (Required)
;;     The authentication token to check. 

(def-class #_authCheckTokenRestGoal (goal) ?goal
    ((has-input-role :value #_hasAccount :value #_hasToken)
     (has-output-role :value #_hasResult)
     (#_hasAccount :type #_Account)
     (#_hasToken :type #_Token)
     (#_hasResult :type #_xml:Document)))

(def-rule #_lower-for-authCheckTokenRestService
    ((#_grnd:lower #_authCheckTokenRestService ?invocation ?http-request) if
     (#_rfc2616:set-method ?http-request "GET")
     (= ?base-url "http://api.flickr.com/services/rest/?")
     (= ?account (wsmo-role-value ?invocation #_hasAccount))
     (= ?token (wsmo-role-value ?invocation #_hasToken))
     (#_hasKey ?account ?apikey)
     (#_hasValue ?apikey ?apikey-string)
     (#_hasValue ?token ?token-string)
     (= ?args (#_new-instance #_Arguments))
     (#_addArgument ?args "method" "flickr.auth.checkToken")
     (#_addArgument ?args "api_key" ?apikey-string)
     (#_addArgument ?args "auth_token" ?token-string)
     (#_signArguments #_rest ?args ?account)
     (#_asQuery ?args ?query)
     (= ?url (make-string "~A~A" ?base-url ?query))
     (#_rfc2616:set-url ?http-request ?url)))

(def-rule #_lift-for-authCheckTokenRestService
    ((#_grnd:lift #_authCheckTokenRestService ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (#_xml:serialiseXml ?xml ?http-content)
     ;; XXX We don't decode this yet.
     (set-goal-slot-value ?invocation #_hasResult ?xml)))

(def-class #_authCheckTokenRest-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class #_authCheckTokenRest-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_authCheckTokenRestGoal)
     (has-non-functional-properties
      :value #_authCheckTokenRest-mediator-non-functional-properties)))

(def-class #_authCheckTokenRestService (web-service) ?web-service
    ((has-capability :value #_authCheckTokenRestService-capability)
     (has-interface :value #_authCheckTokenRestService-interface)
     (has-non-functional-properties
      :value #_authCheckTokenRestService-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class #_authCheckTokenRestService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_authCheckTokenRestService-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_authCheckTokenRestService-capability
           (capability)
           ?capability
           ((used-mediator :value #_authCheckTokenRest-mediator)
            (has-non-functional-properties
             :value
             #_authCheckTokenRestService-capability-non-functional-properties)))

(def-class #_authCheckTokenRestService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_authCheckTokenRestService-interface-choreography
           (choreography)
           ((has-earthing :value #_authCheckTokenRestService-grounding)))

(def-instance #_authCheckTokenRestService-grounding rest-grounding
  ())

(def-class #_authCheckTokenRestService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_authCheckTokenRestService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_authCheckTokenRestService-interface-orchestration-problem-solving-pattern)))

(def-class #_authCheckTokenRestService-interface (interface) ?interface
    ((has-choreography :value #_authCheckTokenRestService-interface-choreography)
     (has-orchestration :value #_authCheckTokenRestService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_authCheckTokenRestService-interface-non-functional-properties)))

;;; }}}

;;; {{{ #_test_login

(def-class #_testLoginRestGoal (goal) ?goal
    ((has-input-role :value #_hasAccount :value #_hasToken)
     (has-output-role :value #_hasResult)
     (#_hasAccount :type #_Account)
     (#_hasToken :type #_Token)
     (#_hasResult :type #_xml:Document)))

(def-rule #_lower-for-testLoginRestService
    ((#_grnd:lower #_testLoginRestService ?invocation ?http-request) if
     (#_rfc2616:set-method ?http-request "GET")
     (= ?base-url "http://api.flickr.com/services/rest/?")
     (= ?account (wsmo-role-value ?invocation #_hasAccount))
     (= ?token (wsmo-role-value ?invocation #_hasToken))
     (#_hasKey ?account ?apikey)
     (#_hasValue ?apikey ?apikey-string)
     (#_hasValue ?token ?token-string)
     (= ?args (#_new-instance #_Arguments))
     (#_addArgument ?args "method" "flickr.test.login")
     (#_addArgument ?args "api_key" ?apikey-string)
     (#_addArgument ?args "auth_token" ?token-string)
     (#_signArguments #_rest ?args ?account)
     (#_asQuery ?args ?query)
     (= ?url (make-string "~A~A" ?base-url ?query))
     (#_rfc2616:set-url ?http-request ?url)))

(def-rule #_lift-for-testLoginRestService
    ((#_grnd:lift #_testLoginRestService ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (#_xml:serialiseXml ?xml ?http-content)
     ;; XXX We don't decode this yet.
     (set-goal-slot-value ?invocation #_hasResult ?xml)))

(def-class #_testLoginRest-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class #_testLoginRest-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_testLoginRestGoal)
     (has-non-functional-properties
      :value #_testLoginRest-mediator-non-functional-properties)))

(def-class #_testLoginRestService (web-service) ?web-service
    ((has-capability :value #_testLoginRestService-capability)
     (has-interface :value #_testLoginRestService-interface)
     (has-non-functional-properties
      :value #_testLoginRestService-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class #_testLoginRestService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_testLoginRestService-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_testLoginRestService-capability
           (capability)
           ?capability
           ((used-mediator :value #_testLoginRest-mediator)
            (has-non-functional-properties
             :value
             #_testLoginRestService-capability-non-functional-properties)))

(def-class #_testLoginRestService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_testLoginRestService-interface-choreography
           (choreography)
           ((has-earthing :value #_testLoginRestService-grounding)))

(def-instance #_testLoginRestService-grounding rest-grounding
  ())

(def-class #_testLoginRestService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_testLoginRestService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_testLoginRestService-interface-orchestration-problem-solving-pattern)))

(def-class #_testLoginRestService-interface (interface) ?interface
    ((has-choreography :value #_testLoginRestService-interface-choreography)
     (has-orchestration :value #_testLoginRestService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_testLoginRestService-interface-non-functional-properties)))

;;; }}}

;;; {{{ #_test_echo

(def-class #_testEchoRestGoal (goal) ?goal
    ((has-input-role :value #_hasAccount)
     (has-output-role :value #_hasResult)
     (#_hasAccount :type #_Account)
     (#_hasResult :type #_xml:Document)))

(def-rule #_lower-for-testEchoRestService
    ((#_grnd:lower #_testEchoRestService ?invocation ?http-request) if
     (#_rfc2616:set-method ?http-request "GET")
     (= ?base-url "http://api.flickr.com/services/rest/?")
     (= ?account (wsmo-role-value ?invocation #_hasAccount))
     (#_hasKey ?account ?apikey)
     (#_hasValue ?apikey ?apikey-string)
     (= ?args (#_new-instance #_Arguments))
     (#_addArgument ?args "method" "flickr.test.echo")
     (#_addArgument ?args "api_key" ?apikey-string)
     (#_signArguments #_rest ?args ?account)
     (#_asQuery ?args ?query)
     (= ?url (make-string "~A~A" ?base-url ?query))
     (#_rfc2616:set-url ?http-request ?url)))

(def-rule #_lift-for-testEchoRestService
    ((#_grnd:lift #_testEchoRestService ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (#_xml:serialiseXml ?xml ?http-content)
     ;; XXX We don't decode this yet.
     (set-goal-slot-value ?invocation #_hasResult ?xml)))

(def-class #_testEchoRest-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class #_testEchoRest-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_testEchoRestGoal)
     (has-non-functional-properties
      :value #_testEchoRest-mediator-non-functional-properties)))

(def-class #_testEchoRestService (web-service) ?web-service
    ((has-capability :value #_testEchoRestService-capability)
     (has-interface :value #_testEchoRestService-interface)
     (has-non-functional-properties
      :value #_testEchoRestService-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class #_testEchoRestService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_testEchoRestService-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_testEchoRestService-capability
           (capability)
           ?capability
           ((used-mediator :value #_testEchoRest-mediator)
            (has-non-functional-properties
             :value
             #_testEchoRestService-capability-non-functional-properties)))

(def-class #_testEchoRestService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_testEchoRestService-interface-choreography
           (choreography)
           ((has-earthing :value #_testEchoRestService-grounding)))

(def-instance #_testEchoRestService-grounding rest-grounding
  ())

(def-class #_testEchoRestService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_testEchoRestService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_testEchoRestService-interface-orchestration-problem-solving-pattern)))

(def-class #_testEchoRestService-interface (interface) ?interface
    ((has-choreography :value #_testEchoRestService-interface-choreography)
     (has-orchestration :value #_testEchoRestService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_testEchoRestService-interface-non-functional-properties)))

;;; }}}
;;; {{{ #_getImageRestService

(def-class #_getImageRestGoal (goal) ?goal
    ((has-input-role :value #_photo)
     (has-output-role :value #_image)
     (#_photo :type #_Photo)
     (#_image :type string)))

(def-rule #_lower-for-getImageRestService
    ((#_grnd:lower #_getImageRestService ?invocation ?http-request)
     if
     (= ?photo (wsmo-role-value ?invocation #_photo))
     (#_farm ?photo ?farm)
     (#_server ?photo ?server)
     (#_id ?photo ?id)
     (#_secret ?photo ?secret)
     (= ?url (make-string "http://farm~a.static.flickr.com/~a/~a_~a.jpg"
			  ?farm ?server ?id ?secret))
     (#_rfc2616:set-url ?http-request ?url)
     (#_rfc2616:set-method ?http-request "GET")))

(def-rule #_lift-for-getImageRestService
    ((#_grnd:lift #_getImageRestService ?http-response ?invocation) if
     (#_rfc2616:get-content ?http-response ?http-content)
     (set-goal-slot-value ?invocation #_image ?http-content)))

(def-class #_getImageRest-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class #_getImageRest-mediator (wg-mediator) ?mediator
    ((has-source-component :value #_getImageRestGoal)
     (has-non-functional-properties
      :value #_getImageRest-mediator-non-functional-properties)))

(def-class #_getImageRestService (web-service) ?web-service
    ((has-capability :value #_getImageRestService-capability)
     (has-interface :value #_getImageRestService-interface)
     (has-non-functional-properties
      :value #_getImageRestService-non-functional-properties)
     (has-output-role :value #_hasContent :value #_hasContentType)
     (#_hasContent :cardinality 1)
     (#_hasContentType :cardinality 1)))

(def-class #_getImageRestService-non-functional-properties
    (non-functional-properties)
    nil)

(def-class #_getImageRestService-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_getImageRestService-capability
           (capability)
           ?capability
           ((used-mediator :value #_getImageRest-mediator)
            (has-non-functional-properties
             :value
             #_getImageRestService-capability-non-functional-properties)))

(def-class #_getImageRestService-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class #_getImageRestService-interface-choreography
           (choreography)
           ((has-earthing :value #_getImageRestService-grounding)))

(def-instance #_getImageRestService-grounding rest-grounding
  ())

(def-class #_getImageRestService-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value nil)))

(def-class #_getImageRestService-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             #_getImageRestService-interface-orchestration-problem-solving-pattern)))

(def-class #_getImageRestService-interface (interface) ?interface
    ((has-choreography :value #_getImageRestService-interface-choreography)
     (has-orchestration :value #_getImageRestService-interface-orchestration)
     (has-non-functional-properties
      :value
      #_getImageRestService-interface-non-functional-properties)))
;;; }}}
