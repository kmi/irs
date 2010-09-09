;;; Copyright © 2009 The Open University

;;; Stuff in here is generic for all flavours (REST,XML-RPC,SOAP)

(in-package #:ocml)

(in-ontology flickr)

(def-class #_Account ()
    ((#_hasKey :type #_APIKey :cardinality 1)
     (#_hasSecret :type #_APISecret :cardinality 1)))

(def-class #_APISecret ()
    ((#_hasValue :type string)))

(def-class #_APIKey ()
    ((#_hasValue :type string)))

(def-class #_Frob ()
    ((#_hasValue :type string)))

(def-class #_Token ()
    ((#_hasValue :type string)))

(def-class #_Photo ()
    ;; Where a string type means, as usual, that we haven't bothered
    ;; to properly model it...
    ((#_id :type string)
     (#_server :type string)
     (#_farm :type string)
     (#_title :type string)
     (#_secret :type string)

     (#_owner :type XXX)
     (#_ispublic :type XXX)
     (#_isfriend :type XXX)
     (#_isfamily :type XXX)))

(def-class #_PhotoList () ?photolist
    :iff-def (and (listp ?photolist)
		  (every ?photolist #_Photo)))

(def-class #_PhotoSize ()
    ((#_label :type string)
     (#_width :type integer)
     (#_height :type integer)
     (#_source :type string)))

(def-class #_PhotoSizeList () ?photosizelist
    :iff-def (and (listp ?photosizelist)
		  (every ?photosizelist #_PhotoSize)))

;;; {{{ Argument construction

;;; The abstract Flickr API is described as a set of parameter/value
;;; pairs.
(def-class #_Argument ()
    ((#_hasName :type string :cardinality 1)
     (#_hasValue :type string :cardinality 1)))

(def-class #_Arguments ()
    ((#_hasArgument :type #_Argument)))

(def-rule #_addArgument
    ((#_addArgument ?arguments ?name ?value)
     if
     ;; Precondition
     (#_Arguments ?arguments)
     (not (#_getArgument ?arguments ?name ?))
     ;; Side-effects
     (= ?arg (#_new-instance #_Argument))
     (exec (set-slot-value ?arg #_hasName ?name))
     (exec (set-slot-value ?arg #_hasValue ?value))
     (exec (add-slot-value ?arguments #_hasArgument ?arg))))

(def-rule #_getArgument
    ((#_getArgument ?arguments ?name ?value)
     if
     ;; Precondition
     (#_Arguments ?arguments)
     ;;
     (#_hasArgument ?arguments ?arg)
     (#_hasName ?arg ?name)
     (#_hasValue ?arg ?value)))

;;; }}}

;;; XXX This should be in a more appropriate ontology, like RFC2616...

(def-function url-encoding (?raw-string)
  :lisp-fun #'hunchentoot:url-encode)

(def-relation url-encoding (?raw ?encoded)
   :prove-by (= ?encoded (url-encoding ?raw)))

;;; Constructs the string to be signed.
(def-rule #_canonicalArgumentsString
    ((#_canonicalArgumentsString #_rest ?arguments ?canonical-string) if
     (= ?args (setofall ?pairs
			(and (#_hasArgument ?arguments ?argument)
			     (#_hasName ?argument ?name)
			     (#_hasValue ?argument ?value)
			     (= ?pairs (?name ?value)))))
     (= ?sortedargs (sort ?args #_sortArgumentsPredicate))
     (#_flattenToString ?sortedargs ?canonical-string))
  ((#_canonicalArgumentsString #_xmlrpc ?arguments ?canonical-string) if
   (= ?args (setofall ?pairs
		      (and (#_hasArgument ?arguments ?argument)
			   (#_hasName ?argument ?name)
			   (not (= ?name "method"))
			   (#_hasValue ?argument ?value)
			   (= ?pairs (?name ?value)))))
   (= ?sortedargs (sort ?args #_sortArgumentsPredicate))
   (#_flattenToString ?sortedargs ?canonical-string)))

(def-rule #_signArguments
    ((#_signArguments ?flavour ?arguments ?account) if
     (#_hasSecret ?account ?secret)
     (#_hasValue ?secret ?secret-string)
     (#_canonicalArgumentsString ?flavour ?arguments ?canonical-args)
     (= ?to-be-signed (make-string "~A~A" ?secret-string ?canonical-args))
     (= ?signature (#_crypt:md5 ?to-be-signed))
     (#_addArgument ?args "api_sig" ?signature)))

(def-function string< (a b)
    :lisp-fun #'string<)

(def-relation #_sortArgumentsPredicate ((?a1 ?a2) (?b1 ?b2))
    :iff-def (not (= nil (string< ?a1 ?b1))))

(def-rule #_flattenToString
    ""
  ((#_flattenToString () ""))
  ((#_flattenToString ((?a ?b) . ?rest) ?string) if
   (#_flattenToString ?rest ?str)
   (= ?string (make-string "~A~A~A" ?a ?b ?str))))

(def-rule #_asQuery
    ""
  ((#_asQuery ?arguments ?string) if
   (= ?args (setofall ?pairs
		      (and (#_hasArgument ?arguments ?argument)
			   (#_hasName ?argument ?name)
			   (#_hasValue ?argument ?value)
			   (= ?pairs (?name ?value)))))
   (#_listAsQuery ?args ?string)))

(def-rule #_listAsQuery
    ""
  ((#_listAsQuery () ""))
  ((#_listAsQuery ((?name ?value)) ?str) if
   (= ?str (make-string "~A=~A" (url-encoding ?name) (url-encoding ?value)))
   cut)
  ((#_listAsQuery ((?name ?value) . ?rest) ?str) if
   (#_listAsQuery ?rest ?reststr)
   (= ?str (make-string "~A=~A&~A" (url-encoding ?name) (url-encoding ?value)
			?reststr))))

(define-skyhook skyhook-account #_Account
  (lambda (value)
    (irs.grounding::skyhook-by-instance-name
     '#_Account (read-ocml-from-string value))))

(define-skyhook skyhook-frob #_Frob
  (lambda (value)
    (irs.grounding::skyhook-by-instance-name
     '#_Frob (read-ocml-from-string value))))

(define-skyhook skyhook-token #_Token
  (lambda (value)
    (irs.grounding::skyhook-by-instance-name
     '#_Token (read-ocml-from-string value))))

(def-function #_new-instance (?class)
  :lisp-fun (lambda (class)
              (let ((i (new-instance class)))
                (name i))))

(def-rule #_argsForPhotosGetSizes
    ((#_argsForPhotosGetSizes ?invocation ?args) if
     (= ?account (wsmo-role-value ?invocation #_hasAccount))
     (= ?token (wsmo-role-value ?invocation #_hasToken))
     (#_hasValue ?token ?token-string)
     (#_hasKey ?account ?apikey)
     (#_hasValue ?apikey ?apikey-string)
     (#_id (wsmo-role-value ?invocation #_hasPhoto) ?id)
     (= ?args (#_new-instance #_Arguments))
     (#_addArgument ?args "method" "flickr.photos.getSizes")
     (#_addArgument ?args "api_key" ?apikey-string)
     (#_addArgument ?args "auth_token" ?token-string)
     (#_addArgument ?args "photo_id" ?id)))

(def-rule #_authenticate
    ((#_authenticate ?account ?frob ?url) if
     (= ?base-url "http://flickr.com/services/auth/?")
     (#_hasKey ?account ?apikey)
     (#_hasValue ?apikey ?apikey-string)
     (= ?args (#_new-instance #_Arguments))
     (#_addArgument ?args "api_key" ?apikey-string)
     (#_hasValue ?frob ?frob-string)
     (#_addArgument ?args "frob" ?frob-string)
     (#_addArgument ?args "perms" "delete")
     (#_signArguments #_rest ?args ?account)
     (#_asQuery ?args ?query)
     (= ?url (make-string "~A~A" ?base-url ?query))))

(def-class #_ImageSize ()
    ())

;; s	small square 75x75
;; t	thumbnail, 100 on longest side
;; m	small, 240 on longest side
;; -	medium, 500 on longest side
;; b	large, 1024 on longest side (only exists for very large original images)
;; o	original image, either a jpg, gif or png, depending on source format

(def-instance #_SmallSquareSize #_ImageSize
  ())

(def-instance #_ThumbnailSize #_ImageSize)

(def-instance #_SmallSize #_ImageSize)

(def-instance #_MediumSize #_ImageSize)

(def-instance #_LargeSize #_ImageSize)

(def-instance #_OriginalSize #_ImageSize)

(def-rule #_ImageSizeUrlSymbol
    ""
    ((#_ImageSizeUrlSymbol #_SmallSquareSize "s"))
    ((#_ImageSizeUrlSymbol #_ThumbnailSize "t"))
    ((#_ImageSizeUrlSymbol #_SmallSize "m"))
    ((#_ImageSizeUrlSymbol #_MediumSize "-"))
    ((#_ImageSizeUrlSymbol #_LargeSize "b"))
    ((#_ImageSizeUrlSymbol #_OriginalSize "o")))
